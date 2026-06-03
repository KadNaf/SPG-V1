# server_combined.R
# Serveur unifié pour :
#   - Null Allele Frequency Estimation (EM algorithm)
#   - FST-ENA (null allele correction for Fst)
#   - DCSE-INA (corrected Cavalli-Sforza & Edwards distance)
#
# UI associée : ui_null_alleles.R (6 onglets)

server_null_alleles <- function(id, rv) {
  moduleServer(id, function(input, output, session) {

    # ─────────────────────────────────────────────────────────────────────
    # 1. HELPERS & DB PLUMBING (communs)
    # ─────────────────────────────────────────────────────────────────────
    `%||%` <- function(a, b) if (!is.null(a)) a else b

    safe_choice <- function(x, default = "all") {
      if (is.null(x) || length(x) == 0L || identical(x, "") || all(is.na(x))) default
      else as.character(x[[1]])
    }

    sql_id  <- function(con, x) as.character(DBI::dbQuoteIdentifier(con, x))
    sql_str <- function(con, x) as.character(DBI::dbQuoteString(con, x))
    na_val  <- function(x) is.null(x) || length(x) == 0L || is.na(x) || is.nan(x) ||
                            (!is.na(x) && x >= 20000)

    # DB plumbing
    db_tick    <- reactive({ rv$db_tick })
    con_r      <- reactive({ req(rv$con); rv$con })
    tbl_meta_r <- reactive({ rv$tbl_meta %||% "meta" })

    tbl_hf_r <- reactive({
      con <- con_r()
      if (exists("duck_tbl_exists", mode = "function", inherits = TRUE) &&
          exists(".duckdb_get_params", mode = "function", inherits = TRUE) &&
          duck_tbl_exists(con, "params")) {
        p <- .duckdb_get_params(con)
        return(as.character(p$tbl_hf %||% "hf"))
      }
      "hf"
    })

    db_ready <- reactive({
      db_tick(); con <- con_r()
      shiny::req(isTRUE(rv$db_ready))
      shiny::validate(
        shiny::need(DBI::dbExistsTable(con, tbl_meta_r()), "DuckDB meta table missing."),
        shiny::need(DBI::dbExistsTable(con, tbl_hf_r()),   "DuckDB hf table missing.")
      )
      TRUE
    })

    base_r <- reactive({
      db_ready()
      b <- rv$base_af %||% rv$base %||% rv$base_r %||% rv$genotype_base
      b <- suppressWarnings(as.integer(b))
      if (length(b) == 1L && is.finite(b) && b > 1L) return(as.integer(b))
      con <- con_r()
      if (DBI::dbExistsTable(con, "params") &&
          exists(".duckdb_get_params", mode = "function", inherits = TRUE)) {
        p <- .duckdb_get_params(con)
        b <- suppressWarnings(as.integer(
          p$base %||% p$base_scalar_full %||% p$base_scalar_preview))
        if (length(b) == 1L && is.finite(b) && b > 1L) return(as.integer(b))
      }
      1000L
    })

    hf_schema_r <- reactive({
      db_ready(); con <- con_r()
      info <- DBI::dbGetQuery(con,
        sprintf("PRAGMA table_info(%s)", DBI::dbQuoteIdentifier(con, tbl_hf_r())))
      cols <- info$name
      if (all(c("individual","locus","g") %in% cols))
        return(list(ind_col="individual", locus_col="locus",    gt_col="g"))
      if (all(c("indiv_id","locus_id","gt") %in% cols))
        return(list(ind_col="indiv_id",   locus_col="locus_id", gt_col="gt"))
      shiny::validate(shiny::need(FALSE,
        "hf must contain (individual,locus,g) or (indiv_id,locus_id,gt)."))
    })

    meta_schema_r <- reactive({
      db_ready(); con <- con_r()
      info <- DBI::dbGetQuery(con,
        sprintf("PRAGMA table_info(%s)", DBI::dbQuoteIdentifier(con, tbl_meta_r())))
      cols    <- info$name
      ind_col <- if ("individual" %in% cols) "individual"
                 else if ("indiv_id" %in% cols) "indiv_id"
                 else shiny::validate(shiny::need(FALSE, "No individual column in meta."))
      pop_col <- c("Population","population","pop","pop_code")[
        c("Population","population","pop","pop_code") %in% cols][1]
      shiny::validate(shiny::need(!is.na(pop_col), "No population column in meta."))
      list(ind_col=ind_col, pop_col=pop_col)
    })

    locus_order_cte <- function(con, hf_tbl_q, hl_q)
      sprintf("locus_order AS (
  SELECT CAST(%s AS VARCHAR) AS _lo_marker, MIN(rowid) AS _lo_rank
  FROM %s GROUP BY CAST(%s AS VARCHAR))", hl_q, hf_tbl_q, hl_q)

    # ── Marker / population lists ──────────────────────────────────────────
    pops_r <- reactive({
      db_ready(); con <- con_r(); ms <- meta_schema_r()
      as.character(DBI::dbGetQuery(con, sprintf(
        "SELECT DISTINCT CAST(%s AS VARCHAR) AS p FROM %s
         WHERE %s IS NOT NULL ORDER BY p",
        sql_id(con,ms$pop_col), sql_id(con,tbl_meta_r()),
        sql_id(con,ms$pop_col)))$p)
    })

    markers_r <- reactive({
      db_ready(); con <- con_r(); hs <- hf_schema_r()
      hf_q <- sql_id(con,tbl_hf_r()); hl_q <- sql_id(con,hs$locus_col)
      as.character(DBI::dbGetQuery(con, sprintf("
        WITH %s
        SELECT DISTINCT CAST(%s AS VARCHAR) AS Marker, lo._lo_rank
        FROM %s h
        LEFT JOIN locus_order lo ON CAST(%s AS VARCHAR) = lo._lo_marker
        ORDER BY lo._lo_rank ASC",
        locus_order_cte(con,hf_q,hl_q), hl_q, hf_q, hl_q))$Marker)
    })

    # ─────────────────────────────────────────────────────────────────────
    # 2. PER-LOCUS TREATMENT SELECTOR (null alleles)
    # ─────────────────────────────────────────────────────────────────────
    treat_id <- function(loc) paste0("treat_", gsub("[^A-Za-z0-9]", "_", loc))

    output$locus_treatment_ui <- renderUI({
      ns_fn   <- session$ns
      markers <- markers_r()
      if (length(markers) == 0L) return(tags$p("No markers loaded yet."))

      items <- lapply(markers, function(loc) {
        tags$div(class = "na-treat-item",
          tags$div(class = "na-treat-lbl", loc),
          selectInput(
            inputId  = ns_fn(treat_id(loc)),
            label    = NULL,
            choices  = c(
              "999999 \u2014 null homozygote"          = "null_homo",
              "000000 \u2014 absent / PCR failure"     = "absent"
            ),
            selected = "null_homo",
            width    = "100%"
          )
        )
      })
      tags$div(class = "na-treat-grid", items)
    })

    locus_treatments_r <- reactive({
      markers <- markers_r()
      treats  <- sapply(markers, function(loc) {
        val <- input[[treat_id(loc)]]
        if (is.null(val) || !val %in% c("null_homo","absent")) "null_homo" else val
      })
      stats::setNames(treats, markers)
    })

    # ─────────────────────────────────────────────────────────────────────
    # 3. EM ALGORITHMS (null allele frequency)
    # ─────────────────────────────────────────────────────────────────────

    # EM — null homozygote model (999999 coding)
    em_null_homo <- function(gt_vec, base) {
      efpop     <- length(gt_vec)
      null_mask <- is.na(gt_vec) | gt_vec <= 0L
      H_00      <- sum(null_mask)
      N         <- efpop

      if (N == 0L) return(list(rd=0.0, n=efpop))
      valid_gt <- gt_vec[!null_mask]
      if (length(valid_gt) == 0L)
        return(list(rd=ifelse(N>0L, H_00/N, 0.0), n=efpop))

      a1 <- floor(valid_gt / base)
      a2 <- valid_gt %% base
      all_alleles <- sort(unique(c(a1, a2)))
      all_alleles <- all_alleles[all_alleles >= 0L]
      if (length(all_alleles) == 0L) return(list(rd=0.0, n=efpop))

      n_valid  <- N - H_00
      genefreq <- sapply(all_alleles, function(a)
        (sum(a1==a) + sum(a2==a)) / (2L * n_valid))

      r    <- if (H_00 > 0L) sqrt(H_00 / N) else sqrt(1.0 / (N + 1.0))
      H_ii <- sapply(all_alleles, function(a) sum(a1==a & a2==a))
      H_iX <- sapply(all_alleles, function(a) sum((a1==a & a2!=a)|(a2==a & a1!=a)))
      hotot <- sum(H_ii)

      p <- numeric(length(all_alleles))
      for (ai in seq_along(all_alleles)) {
        if (genefreq[ai] <= 0) { p[ai] <- 0.0; next }
        ii <- H_ii[ai]; jj <- H_iX[ai]
        if (H_00 > 0L) {
          X <- H_00 + hotot - ii + (N - H_00 - hotot) - jj; Y <- N
        } else {
          X <- 1.0 + hotot - ii + (N - hotot) - jj;         Y <- N + 1.0
        }
        p[ai] <- 1.0 - sqrt(max(0.0, X / Y))
      }

      for (iter in seq_len(5000L)) {
        new_p <- numeric(length(all_alleles))
        ri <- 0.0; re <- 0L
        for (ai in seq_along(all_alleles)) {
          if (genefreq[ai] <= 0) { new_p[ai] <- 0.0; next }
          pa <- p[ai]; denom <- pa + 2.0 * r
          if (denom <= 0) { new_p[ai] <- 0.0; next }
          p_new     <- (pa + r)/denom * (H_ii[ai]/N) + H_iX[ai]/(2.0*N)
          ri        <- ri + r/denom * (H_ii[ai]/N)
          new_p[ai] <- p_new
          if (abs(p_new - pa) > 1e-6) re <- re + 1L
        }
        r_new <- ri + H_00 / N
        if (abs(r_new - r) > 1e-6) re <- re + 1L
        p <- new_p; r <- max(0.0, r_new)
        if (re == 0L) break
      }
      list(rd=r, n=efpop)
    }

    # EM — absent model (000000 coding)
    em_absent <- function(gt_vec, base) {
      efpop     <- length(gt_vec)
      null_mask <- is.na(gt_vec) | gt_vec <= 0L
      n_absent  <- sum(null_mask)
      N         <- efpop - n_absent

      if (N == 0L) return(list(rd=0.0, n=efpop))
      valid_gt <- gt_vec[!null_mask]
      if (length(valid_gt) == 0L) return(list(rd=0.0, n=efpop))

      a1 <- floor(valid_gt / base)
      a2 <- valid_gt %% base
      all_alleles <- sort(unique(c(a1, a2)))
      all_alleles <- all_alleles[all_alleles >= 0L]
      if (length(all_alleles) == 0L) return(list(rd=0.0, n=efpop))

      genefreq <- sapply(all_alleles, function(a)
        (sum(a1==a) + sum(a2==a)) / (2L * N))

      r    <- sqrt(1.0 / (N + 1.0))
      H_ii <- sapply(all_alleles, function(a) sum(a1==a & a2==a))
      H_iX <- sapply(all_alleles, function(a) sum((a1==a & a2!=a)|(a2==a & a1!=a)))
      hotot <- sum(H_ii)

      p <- numeric(length(all_alleles))
      for (ai in seq_along(all_alleles)) {
        if (genefreq[ai] <= 0) { p[ai] <- 0.0; next }
        ii <- H_ii[ai]; jj <- H_iX[ai]
        X <- 1.0 + hotot - ii + (N - hotot) - jj; Y <- N + 1.0
        p[ai] <- 1.0 - sqrt(max(0.0, X / Y))
      }

      for (iter in seq_len(5000L)) {
        new_p <- numeric(length(all_alleles))
        ri <- 0.0; re <- 0L
        for (ai in seq_along(all_alleles)) {
          if (genefreq[ai] <= 0) { new_p[ai] <- 0.0; next }
          pa <- p[ai]; denom <- pa + 2.0 * r
          if (denom <= 0) { new_p[ai] <- 0.0; next }
          p_new     <- (pa + r)/denom * (H_ii[ai]/N) + H_iX[ai]/(2.0*N)
          ri        <- ri + r/denom * (H_ii[ai]/N)
          new_p[ai] <- p_new
          if (abs(p_new - pa) > 1e-6) re <- re + 1L
        }
        r_new <- ri
        if (abs(r_new - r) > 1e-6) re <- re + 1L
        p <- new_p; r <- max(0.0, r_new)
        if (re == 0L) break
      }
      list(rd=r, n=efpop)
    }

    # EM FreeNA complète (pour FST-ENA / DCSE-INA)
    em_freena <- function(gt_vec, base) {
      efpop     <- length(gt_vec)
      absent_mask <- is.na(gt_vec) | gt_vec <= 0L
      n_absent  <- sum(absent_mask)
      valid_gt  <- gt_vec[!absent_mask]

      if (length(valid_gt) == 0L)
        return(list(rd=0.0, pfreq=numeric(0), efpop=efpop,
                    absent=n_absent, nnullhomo=0L, alleles=integer(0)))

      a1_all <- floor(valid_gt / base)
      a2_all <- valid_gt %% base

      null_code <- if (base >= 1000L) 999L else 99L
      null_homo_mask <- (a1_all == null_code) & (a2_all == null_code)
      n_null_homo    <- sum(null_homo_mask)

      valid_a1 <- a1_all[!null_homo_mask]
      valid_a2 <- a2_all[!null_homo_mask]
      all_alleles <- sort(unique(c(valid_a1, valid_a2)))
      all_alleles <- all_alleles[all_alleles >= 0L & all_alleles != null_code]

      N <- efpop - n_absent

      if (N == 0L || length(all_alleles) == 0L)
        return(list(rd=0.0, pfreq=numeric(0), efpop=efpop,
                    absent=n_absent, nnullhomo=n_null_homo, alleles=integer(0)))

      n_valid_geno <- N - n_null_homo
      genefreq <- sapply(all_alleles, function(a)
        (sum(valid_a1==a) + sum(valid_a2==a)) / (2L * n_valid_geno))

      rd <- if (n_null_homo > 0L) sqrt(n_null_homo / N) else sqrt(1.0 / (N + 1.0))

      H_ii <- sapply(all_alleles, function(a) sum(valid_a1==a & valid_a2==a))
      H_iX <- sapply(all_alleles, function(a) sum((valid_a1==a & valid_a2!=a) | (valid_a2==a & valid_a1!=a)))
      hotot <- sum(H_ii)

      p <- numeric(length(all_alleles))
      for (ai in seq_along(all_alleles)) {
        if (genefreq[ai] <= 0) { p[ai] <- 0.0; next }
        ii <- H_ii[ai]; jj <- H_iX[ai]
        if (n_null_homo > 0L) {
          X <- n_null_homo + hotot - ii + ((N - n_null_homo) - hotot) - jj; Y <- N
        } else {
          X <- 1.0 + hotot - ii + (N - hotot) - jj; Y <- N + 1.0
        }
        p[ai] <- 1.0 - sqrt(max(0.0, X / Y))
      }

      for (iter in seq_len(5000L)) {
        new_p <- numeric(length(all_alleles))
        rdi   <- 0.0; re <- 0L
        for (ai in seq_along(all_alleles)) {
          if (genefreq[ai] <= 0) { new_p[ai] <- 0.0; next }
          pa    <- p[ai]; denom <- pa + 2.0 * rd
          if (denom <= 0) { new_p[ai] <- 0.0; next }
          p_new     <- (pa + rd) / denom * (H_ii[ai] / N) + H_iX[ai] / (2.0 * N)
          rdi       <- rdi + rd / denom * (H_ii[ai] / N)
          new_p[ai] <- p_new
          if (abs(p_new - pa) > 1e-6) re <- re + 1L
        }
        rd_new <- rdi + (2.0 * n_null_homo) / (2.0 * N)
        if (abs(rd_new - rd) > 1e-6) re <- re + 1L
        p <- new_p; rd <- max(0.0, rd_new)
        if (re == 0L) break
      }

      pfreq <- stats::setNames(p, as.character(all_alleles))
      list(rd=rd, pfreq=pfreq, efpop=efpop,
           absent=n_absent, nnullhomo=n_null_homo, alleles=all_alleles,
           genefreq_obs=stats::setNames(genefreq, as.character(all_alleles)),
           H_ii=stats::setNames(H_ii, as.character(all_alleles)),
           H_iX=stats::setNames(H_iX, as.character(all_alleles)),
           N=N, n_valid_geno=n_valid_geno)
    }

    # ─────────────────────────────────────────────────────────────────────
    # 4. FETCH DATA & RUN EM (pour null alleles)
    # ─────────────────────────────────────────────────────────────────────
    fetch_and_run_em <- function(sel_locus="all", sel_pop="all") {
      db_ready()
      con   <- con_r(); hs <- hf_schema_r(); ms <- meta_schema_r()
      base  <- as.integer(base_r())
      hf_q  <- sql_id(con,tbl_hf_r());  meta_q <- sql_id(con,tbl_meta_r())
      hi_q  <- sql_id(con,hs$ind_col);  hl_q   <- sql_id(con,hs$locus_col)
      hg_q  <- sql_id(con,hs$gt_col);   mi_q   <- sql_id(con,ms$ind_col)
      pop_q <- sql_id(con,ms$pop_col)

      filters <- character(0)
      if (!identical(sel_locus,"all"))
        filters <- c(filters,
          sprintf("CAST(h.%s AS VARCHAR)=%s", hl_q, sql_str(con,sel_locus)))
      if (!identical(sel_pop,"all"))
        filters <- c(filters,
          sprintf("CAST(m.%s AS VARCHAR)=%s", pop_q, sql_str(con,sel_pop)))
      w_extra <- if (length(filters))
        paste0(" AND ", paste(filters, collapse=" AND ")) else ""

      sql <- sprintf("
        WITH %s
        SELECT
          CAST(m.%s AS VARCHAR) AS Population,
          CAST(h.%s AS VARCHAR) AS Marker,
          h.%s                  AS gt
        FROM %s h
        INNER JOIN %s m
          ON CAST(h.%s AS VARCHAR) = CAST(m.%s AS VARCHAR)
        LEFT JOIN locus_order lo
          ON CAST(h.%s AS VARCHAR) = lo._lo_marker
        WHERE m.%s IS NOT NULL%s
        ORDER BY lo._lo_rank ASC, Population",
        locus_order_cte(con,hf_q,hl_q),
        pop_q, hl_q, hg_q,
        hf_q,
        meta_q, hi_q, mi_q,
        hl_q,
        pop_q, w_extra)

      raw <- DBI::dbGetQuery(con, sql)
      if (nrow(raw) == 0L) return(data.frame())

      treatments   <- locus_treatments_r()
      locus_levels <- markers_r()
      combos       <- unique(raw[,c("Population","Marker"),drop=FALSE])

      results <- vector("list", nrow(combos))
      for (i in seq_len(nrow(combos))) {
        pop_i  <- combos$Population[i]
        mark_i <- combos$Marker[i]
        gts    <- raw$gt[raw$Population==pop_i & raw$Marker==mark_i]

        treat <- treatments[mark_i]
        if (is.na(treat) || length(treat)==0L) treat <- "null_homo"

        em <- if (identical(as.character(treat),"absent"))
          em_absent(gts, base)
        else
          em_null_homo(gts, base)

        n_exp <- em$n * (em$rd^2)
        results[[i]] <- data.frame(
          Locus        = mark_i,
          Population   = pop_i,
          p_nulls      = round(em$rd,        5),
          N            = as.integer(em$n),
          N_exp_blanks = round(n_exp,         9),
          p_nulls_x_N  = round(em$rd * em$n, 5),
          stringsAsFactors = FALSE
        )
      }

      out <- do.call(rbind, results)
      if (!is.null(locus_levels) && length(locus_levels)) {
        out$Locus <- factor(out$Locus, levels=locus_levels)
        out <- out[order(out$Locus, out$Population),]
        out$Locus <- as.character(out$Locus)
      }
      out
    }

    # ─────────────────────────────────────────────────────────────────────
    # 5. FETCH EM RESULTS COMPLETS (pour FST-ENA / DCSE-INA)
    # ─────────────────────────────────────────────────────────────────────
    fetch_em_results <- reactive({
      db_ready()
      con   <- con_r(); hs <- hf_schema_r(); ms <- meta_schema_r()
      base  <- as.integer(base_r())
      hf_q  <- sql_id(con, tbl_hf_r());  meta_q <- sql_id(con, tbl_meta_r())
      hi_q  <- sql_id(con, hs$ind_col);  hl_q   <- sql_id(con, hs$locus_col)
      hg_q  <- sql_id(con, hs$gt_col);   mi_q   <- sql_id(con, ms$ind_col)
      pop_q <- sql_id(con, ms$pop_col)

      sql <- sprintf("
        WITH %s
        SELECT
          CAST(m.%s AS VARCHAR) AS Population,
          CAST(h.%s AS VARCHAR) AS Marker,
          h.%s                  AS gt,
          lo._lo_rank
        FROM %s h
        INNER JOIN %s m
          ON CAST(h.%s AS VARCHAR) = CAST(m.%s AS VARCHAR)
        LEFT JOIN locus_order lo
          ON CAST(h.%s AS VARCHAR) = lo._lo_marker
        WHERE m.%s IS NOT NULL
        ORDER BY lo._lo_rank ASC, Population",
        locus_order_cte(con, hf_q, hl_q),
        pop_q, hl_q, hg_q,
        hf_q, meta_q, hi_q, mi_q,
        hl_q, pop_q)

      raw <- DBI::dbGetQuery(con, sql)
      if (nrow(raw) == 0L) return(list())

      markers <- markers_r()
      pops    <- pops_r()

      em_res <- list()
      for (loc in markers) {
        em_res[[loc]] <- list()
        for (pop in pops) {
          gts <- raw$gt[raw$Marker == loc & raw$Population == pop]
          if (length(gts) == 0L) {
            em_res[[loc]][[pop]] <- list(rd=0.0, pfreq=numeric(0),
              efpop=0L, absent=0L, nnullhomo=0L, alleles=integer(0),
              genefreq_obs=numeric(0), H_ii=numeric(0), H_iX=numeric(0),
              N=0L, n_valid_geno=0L)
          } else {
            em_res[[loc]][[pop]] <- em_freena(gts, base)
          }
        }
      }
      em_res
    })

    # ─────────────────────────────────────────────────────────────────────
    # 6. FST WEIR (1996) — fonctions partagées
    # ─────────────────────────────────────────────────────────────────────
    weir_fst_allele <- function(pop_data, use_corr = FALSE) {
      r      <- length(pop_data)
      N_tot  <- sum(sapply(pop_data, `[[`, "ni"))
      N_tot2 <- sum(sapply(pop_data, function(p) p$ni^2))
      if (N_tot == 0L || r < 2L) return(list(s1=0.0, s3=0.0))

      nc <- (N_tot - N_tot2 / N_tot) / (r - 1)
      if (nc <= 0 || N_tot - r <= 0) return(list(s1=0.0, s3=0.0))

      snA <- sum(sapply(pop_data, `[[`, "nA"))
      s2A <- sum(sapply(pop_data, function(p) if (p$ni > 0) p$nA^2 / (2 * p$ni) else 0.0))
      sAA <- if (use_corr) sum(sapply(pop_data, `[[`, "AA_corr"))
             else           sum(sapply(pop_data, `[[`, "AA"))

      MSG  <- (0.5 * snA - sAA) / N_tot
      dMSI <- N_tot - r
      MSI  <- if (dMSI > 0) (0.5 * snA + sAA - s2A) / dMSI else 0.0
      MSP  <- (s2A - 0.5 * snA^2 / N_tot) / (r - 1)
      s2G  <- MSG
      s2I  <- 0.5 * (MSI - MSG)
      s2P  <- (MSP - MSI) / (2 * nc)
      list(s1 = s2P, s3 = s2P + s2I + s2G)
    }

    # ─────────────────────────────────────────────────────────────────────
    # 7. CAVALLI-SFORZA & EDWARDS DISTANCE
    # ─────────────────────────────────────────────────────────────────────
    cs_distance <- function(freq_i, freq_j) {
      alleles <- union(names(freq_i), names(freq_j))
      csprod  <- 0.0
      for (a in alleles) {
        pi <- freq_i[a] %||% 0.0
        pj <- freq_j[a] %||% 0.0
        if (!is.na(pi) && !is.na(pj) && pi > 0 && pj > 0)
          csprod <- csprod + sqrt(pi * pj)
      }
      if (csprod > 1.0) return(NA_real_)
      (2.0 / pi) * sqrt(2.0 * (1.0 - csprod))
    }

    # ─────────────────────────────────────────────────────────────────────
    # 8. CALCULS FST GLOBAL, PAIRWISE, DCSE, PER LOCUS
    # ─────────────────────────────────────────────────────────────────────

    compute_fst_global <- function(em_res) {
      markers <- names(em_res)
      pops    <- names(em_res[[markers[1]]])
      s1 <- s3 <- s1c <- s3c <- 0.0
      rows <- vector("list", length(markers))

      for (li in seq_along(markers)) {
        loc     <- markers[li]
        em_loc  <- em_res[[loc]]
        alleles_obs  <- sort(unique(unlist(lapply(em_loc, function(e) e$alleles))))

        ni_raw  <- sapply(pops, function(p) {
          e <- em_loc[[p]]; max(0L, e$efpop - e$absent - e$nnullhomo)
        })
        ni_corr <- sapply(pops, function(p) {
          e <- em_loc[[p]]; max(0L, e$efpop - e$absent)
        })

        r_raw  <- sum(ni_raw  > 0L); r_corr <- sum(ni_corr > 0L)
        N_raw   <- sum(ni_raw);   N2_raw  <- sum(ni_raw^2)
        N_corr  <- sum(ni_corr);  N2_corr <- sum(ni_corr^2)
        nc_raw  <- if (N_raw  > 0 && r_raw  > 1) (N_raw  - N2_raw  / N_raw)  / (r_raw  - 1) else 0.0
        nc_corr <- if (N_corr > 0 && r_corr > 1) (N_corr - N2_corr / N_corr) / (r_corr - 1) else 0.0

        s1l <- s3l <- s1lc <- s3lc <- 0.0

        for (a in alleles_obs) {
          a_chr <- as.character(a)
          pop_data <- lapply(pops, function(p) {
            e  <- em_loc[[p]]
            ni <- max(0L, e$efpop - e$absent - e$nnullhomo)
            pf <- if (!is.null(e$genefreq_obs) && a_chr %in% names(e$genefreq_obs))
                    e$genefreq_obs[a_chr] else 0.0
            nA <- pf * 2L * ni
            AA <- if (!is.null(e$H_ii) && a_chr %in% names(e$H_ii)) e$H_ii[a_chr] else 0L
            list(ni=ni, nA=nA, AA=AA, AA_corr=AA)
          })
          cmp <- weir_fst_allele(pop_data, use_corr = FALSE)
          s1l <- s1l + cmp$s1; s3l <- s3l + cmp$s3

          pop_data_c <- lapply(pops, function(p) {
            e  <- em_loc[[p]]
            ni <- max(0L, e$efpop - e$absent)
            pf <- if (!is.null(e$pfreq) && a_chr %in% names(e$pfreq)) e$pfreq[a_chr] else 0.0
            rd <- e$rd
            nA <- pf * 2L * ni
            AA <- if (!is.null(e$H_ii) && a_chr %in% names(e$H_ii)) e$H_ii[a_chr] else 0L
            denom <- pf + 2.0 * rd
            AA_c  <- if (AA > 0 && denom > 0) AA * (pf / denom) else 0.0
            list(ni=ni, nA=nA, AA=AA, AA_corr=AA_c)
          })
          cmp_c <- weir_fst_allele(pop_data_c, use_corr = TRUE)
          s1lc <- s1lc + cmp_c$s1; s3lc <- s3lc + cmp_c$s3
        }

        fst_loc  <- if (s3l  != 0) s1l  / s3l  else NA_real_
        fst_locc <- if (s3lc != 0) s1lc / s3lc else NA_real_

        if (!is.na(fst_loc)  && nc_raw  > 0) { s1 <- s1 + s1l * nc_raw;   s3 <- s3 + s3l * nc_raw }
        if (!is.na(fst_locc) && nc_corr > 0) { s1c <- s1c + s1lc * nc_corr; s3c <- s3c + s3lc * nc_corr }

        rows[[li]] <- data.frame(
          Locus          = loc,
          FST_raw        = round(fst_loc,  6),
          FST_ENA        = round(fst_locc, 6),
          Delta_FST      = round(fst_locc - fst_loc, 6),
          N_pops_eff_raw  = r_raw,
          N_pops_eff_ENA   = r_corr,
          stringsAsFactors = FALSE
        )
      }

      out_loci   <- do.call(rbind, rows)
      fst_global <- if (s3  > 0) s1  / s3  else NA_real_
      fst_ena    <- if (s3c > 0) s1c / s3c else NA_real_

      list(global_raw = fst_global, global_ena = fst_ena, per_locus = out_loci)
    }

    compute_fst_pairwise <- function(em_res) {
      markers <- names(em_res)
      pops    <- names(em_res[[markers[1]]])
      n_pops  <- length(pops)
      if (n_pops < 2L) return(list(matrix_raw=NULL, matrix_ena=NULL, long=data.frame()))

      s12p  <- matrix(0.0, n_pops, n_pops, dimnames=list(pops,pops))
      s32p  <- matrix(0.0, n_pops, n_pops, dimnames=list(pops,pops))
      s12pc <- matrix(0.0, n_pops, n_pops, dimnames=list(pops,pops))
      s32pc <- matrix(0.0, n_pops, n_pops, dimnames=list(pops,pops))

      for (loc in markers) {
        em_loc <- em_res[[loc]]
        alleles_obs <- sort(unique(unlist(lapply(em_loc, function(e) e$alleles))))

        for (ii in seq_len(n_pops - 1L)) {
          for (jj in seq(ii + 1L, n_pops)) {
            pi_name <- pops[ii]; pj_name <- pops[jj]
            ei <- em_loc[[pi_name]]; ej <- em_loc[[pj_name]]

            ni_raw_i <- max(0L, ei$efpop - ei$absent - ei$nnullhomo)
            ni_raw_j <- max(0L, ej$efpop - ej$absent - ej$nnullhomo)
            ni_c_i   <- max(0L, ei$efpop - ei$absent)
            ni_c_j   <- max(0L, ej$efpop - ej$absent)

            if (ni_raw_i > 0L && ni_raw_j > 0L) {
              for (a in alleles_obs) {
                a_chr <- as.character(a)
                pop_d <- list(
                  list(ni=ni_raw_i,
                       nA=(if (!is.null(ei$genefreq_obs) && a_chr %in% names(ei$genefreq_obs))
                             ei$genefreq_obs[a_chr] else 0.0) * 2L * ni_raw_i,
                       AA=(if (!is.null(ei$H_ii) && a_chr %in% names(ei$H_ii)) ei$H_ii[a_chr] else 0L),
                       AA_corr=0.0),
                  list(ni=ni_raw_j,
                       nA=(if (!is.null(ej$genefreq_obs) && a_chr %in% names(ej$genefreq_obs))
                             ej$genefreq_obs[a_chr] else 0.0) * 2L * ni_raw_j,
                       AA=(if (!is.null(ej$H_ii) && a_chr %in% names(ej$H_ii)) ej$H_ii[a_chr] else 0L),
                       AA_corr=0.0)
                )
                cmp <- weir_fst_allele(pop_d, use_corr=FALSE)
                N2p <- ni_raw_i + ni_raw_j; N22p <- ni_raw_i^2 + ni_raw_j^2
                nc  <- if (N2p > 0) (N2p - N22p/N2p) / 1.0 else 0.0
                s12p[ii, jj]  <- s12p[ii, jj]  + cmp$s1 * nc
                s32p[ii, jj]  <- s32p[ii, jj]  + cmp$s3 * nc
              }
            }

            if (ni_c_i > 0L && ni_c_j > 0L) {
              for (a in alleles_obs) {
                a_chr <- as.character(a)
                pf_i <- if (!is.null(ei$pfreq) && a_chr %in% names(ei$pfreq)) ei$pfreq[a_chr] else 0.0
                pf_j <- if (!is.null(ej$pfreq) && a_chr %in% names(ej$pfreq)) ej$pfreq[a_chr] else 0.0
                AA_i <- if (!is.null(ei$H_ii) && a_chr %in% names(ei$H_ii)) ei$H_ii[a_chr] else 0L
                AA_j <- if (!is.null(ej$H_ii) && a_chr %in% names(ej$H_ii)) ej$H_ii[a_chr] else 0L
                denom_i <- pf_i + 2.0 * ei$rd; AAc_i <- if (AA_i > 0 && denom_i > 0) AA_i*(pf_i/denom_i) else 0.0
                denom_j <- pf_j + 2.0 * ej$rd; AAc_j <- if (AA_j > 0 && denom_j > 0) AA_j*(pf_j/denom_j) else 0.0
                pop_dc <- list(
                  list(ni=ni_c_i, nA=pf_i*2L*ni_c_i, AA=AA_i, AA_corr=AAc_i),
                  list(ni=ni_c_j, nA=pf_j*2L*ni_c_j, AA=AA_j, AA_corr=AAc_j)
                )
                cmp_c <- weir_fst_allele(pop_dc, use_corr=TRUE)
                N2p_c  <- ni_c_i + ni_c_j; N22p_c <- ni_c_i^2 + ni_c_j^2
                nc_c   <- if (N2p_c > 0) (N2p_c - N22p_c/N2p_c) / 1.0 else 0.0
                s12pc[ii, jj] <- s12pc[ii, jj] + cmp_c$s1 * nc_c
                s32pc[ii, jj] <- s32pc[ii, jj] + cmp_c$s3 * nc_c
              }
            }
          }
        }
      }

      mat_raw <- matrix(NA_real_, n_pops, n_pops, dimnames=list(pops,pops))
      mat_ena <- matrix(NA_real_, n_pops, n_pops, dimnames=list(pops,pops))
      for (ii in seq_len(n_pops - 1L)) {
        for (jj in seq(ii + 1L, n_pops)) {
          mat_raw[jj, ii] <- if (s32p[ii,jj]  > 0) s12p[ii,jj]  / s32p[ii,jj]  else NA_real_
          mat_ena[jj, ii] <- if (s32pc[ii,jj] > 0) s12pc[ii,jj] / s32pc[ii,jj] else NA_real_
        }
      }

      long_rows <- list()
      for (ii in seq_len(n_pops - 1L)) {
        for (jj in seq(ii + 1L, n_pops)) {
          long_rows[[length(long_rows)+1]] <- data.frame(
            Pop1 = pops[ii], Pop2 = pops[jj],
            FST_raw = round(mat_raw[jj,ii], 6),
            FST_ENA = round(mat_ena[jj,ii], 6),
            Delta_FST = round(mat_ena[jj,ii] - mat_raw[jj,ii], 6),
            stringsAsFactors = FALSE
          )
        }
      }

      list(matrix_raw = mat_raw, matrix_ena = mat_ena,
           long = do.call(rbind, long_rows))
    }

    compute_dc_pairwise <- function(em_res) {
      markers <- names(em_res)
      pops    <- names(em_res[[markers[1]]])
      n_pops  <- length(pops)
      if (n_pops < 2L) return(list(matrix_raw=NULL, matrix_ina=NULL, long=data.frame()))

      dc_sum_raw  <- matrix(0.0, n_pops, n_pops, dimnames=list(pops,pops))
      dc_sum_ina  <- matrix(0.0, n_pops, n_pops, dimnames=list(pops,pops))
      nloc_eff_raw <- matrix(length(markers), n_pops, n_pops, dimnames=list(pops,pops))
      nloc_eff_ina <- matrix(length(markers), n_pops, n_pops, dimnames=list(pops,pops))

      for (loc in markers) {
        em_loc <- em_res[[loc]]

        for (ii in seq_len(n_pops - 1L)) {
          for (jj in seq(ii + 1L, n_pops)) {
            pi_n <- pops[ii]; pj_n <- pops[jj]
            ei <- em_loc[[pi_n]]; ej <- em_loc[[pj_n]]

            ni_raw_i <- ei$efpop - ei$absent - ei$nnullhomo
            ni_raw_j <- ej$efpop - ej$absent - ej$nnullhomo

            if (ni_raw_i > 0L && ni_raw_j > 0L &&
                !is.null(ei$genefreq_obs) && !is.null(ej$genefreq_obs)) {
              d_raw <- cs_distance(ei$genefreq_obs, ej$genefreq_obs)
              if (!is.na(d_raw)) dc_sum_raw[jj, ii] <- dc_sum_raw[jj, ii] + d_raw
              else               nloc_eff_raw[jj, ii] <- nloc_eff_raw[jj, ii] - 1L
            } else {
              nloc_eff_raw[jj, ii] <- nloc_eff_raw[jj, ii] - 1L
            }

            ni_c_i <- ei$efpop - ei$absent
            ni_c_j <- ej$efpop - ej$absent
            if (ni_c_i > 0L && ni_c_j > 0L &&
                !is.null(ei$pfreq) && !is.null(ej$pfreq)) {
              freq_ina_i <- c(ei$pfreq, `null`=ei$rd)
              freq_ina_j <- c(ej$pfreq, `null`=ej$rd)
              d_ina <- cs_distance(freq_ina_i, freq_ina_j)
              if (!is.na(d_ina)) dc_sum_ina[jj, ii] <- dc_sum_ina[jj, ii] + d_ina
              else               nloc_eff_ina[jj, ii] <- nloc_eff_ina[jj, ii] - 1L
            } else {
              nloc_eff_ina[jj, ii] <- nloc_eff_ina[jj, ii] - 1L
            }
          }
        }
      }

      mat_raw <- matrix(NA_real_, n_pops, n_pops, dimnames=list(pops,pops))
      mat_ina <- matrix(NA_real_, n_pops, n_pops, dimnames=list(pops,pops))
      for (ii in seq_len(n_pops - 1L)) {
        for (jj in seq(ii + 1L, n_pops)) {
          if (nloc_eff_raw[jj,ii] > 0L)
            mat_raw[jj,ii] <- dc_sum_raw[jj,ii] / nloc_eff_raw[jj,ii]
          if (nloc_eff_ina[jj,ii] > 0L)
            mat_ina[jj,ii] <- dc_sum_ina[jj,ii] / nloc_eff_ina[jj,ii]
        }
      }

      long_rows <- list()
      for (ii in seq_len(n_pops - 1L)) {
        for (jj in seq(ii + 1L, n_pops)) {
          long_rows[[length(long_rows)+1]] <- data.frame(
            Pop1 = pops[ii], Pop2 = pops[jj],
            DCSE_raw = round(mat_raw[jj,ii], 6),
            DCSE_INA = round(mat_ina[jj,ii], 6),
            Delta_DCSE = round(mat_ina[jj,ii] - mat_raw[jj,ii], 6),
            stringsAsFactors = FALSE
          )
        }
      }

      list(matrix_raw=mat_raw, matrix_ina=mat_ina,
           long=do.call(rbind, long_rows))
    }

    compute_fst_per_locus_pair <- function(em_res, sel_locus="all",
                                           sel_pop1="all", sel_pop2="all") {
      markers <- names(em_res)
      pops    <- names(em_res[[markers[1]]])

      if (!identical(sel_locus,"all")) markers <- markers[markers == sel_locus]
      pairs <- if (!identical(sel_pop1,"all") && !identical(sel_pop2,"all"))
                 list(c(sel_pop1, sel_pop2))
               else {
                 pp <- combn(pops, 2, simplify=FALSE)
                 if (!identical(sel_pop1,"all"))
                   pp <- pp[sapply(pp, function(x) sel_pop1 %in% x)]
                 else if (!identical(sel_pop2,"all"))
                   pp <- pp[sapply(pp, function(x) sel_pop2 %in% x)]
                 pp
               }

      rows <- list()
      for (loc in markers) {
        em_loc <- em_res[[loc]]
        alleles_obs <- sort(unique(unlist(lapply(em_loc, function(e) e$alleles))))

        for (pair in pairs) {
          pi_n <- pair[1]; pj_n <- pair[2]
          if (!pi_n %in% pops || !pj_n %in% pops) next
          ei <- em_loc[[pi_n]]; ej <- em_loc[[pj_n]]

          ni_raw_i <- max(0L, ei$efpop - ei$absent - ei$nnullhomo)
          ni_raw_j <- max(0L, ej$efpop - ej$absent - ej$nnullhomo)
          ni_c_i   <- max(0L, ei$efpop - ei$absent)
          ni_c_j   <- max(0L, ej$efpop - ej$absent)

          s1_r <- s3_r <- s1_c <- s3_c <- 0.0

          for (a in alleles_obs) {
            a_chr <- as.character(a)
            pf_i_obs <- if (!is.null(ei$genefreq_obs) && a_chr %in% names(ei$genefreq_obs)) ei$genefreq_obs[a_chr] else 0.0
            pf_j_obs <- if (!is.null(ej$genefreq_obs) && a_chr %in% names(ej$genefreq_obs)) ej$genefreq_obs[a_chr] else 0.0
            AA_i <- if (!is.null(ei$H_ii) && a_chr %in% names(ei$H_ii)) ei$H_ii[a_chr] else 0L
            AA_j <- if (!is.null(ej$H_ii) && a_chr %in% names(ej$H_ii)) ej$H_ii[a_chr] else 0L

            if (ni_raw_i > 0L && ni_raw_j > 0L) {
              pd <- list(
                list(ni=ni_raw_i, nA=pf_i_obs*2L*ni_raw_i, AA=AA_i, AA_corr=AA_i),
                list(ni=ni_raw_j, nA=pf_j_obs*2L*ni_raw_j, AA=AA_j, AA_corr=AA_j))
              cmp <- weir_fst_allele(pd, use_corr=FALSE)
              N2p <- ni_raw_i+ni_raw_j; nc <- if (N2p>0) (N2p-(ni_raw_i^2+ni_raw_j^2)/N2p)/1.0 else 0.0
              s1_r <- s1_r + cmp$s1*nc; s3_r <- s3_r + cmp$s3*nc
            }

            pf_i <- if (!is.null(ei$pfreq) && a_chr %in% names(ei$pfreq)) ei$pfreq[a_chr] else 0.0
            pf_j <- if (!is.null(ej$pfreq) && a_chr %in% names(ej$pfreq)) ej$pfreq[a_chr] else 0.0
            denom_i <- pf_i + 2.0*ei$rd; AAc_i <- if (AA_i>0&&denom_i>0) AA_i*(pf_i/denom_i) else 0.0
            denom_j <- pf_j + 2.0*ej$rd; AAc_j <- if (AA_j>0&&denom_j>0) AA_j*(pf_j/denom_j) else 0.0

            if (ni_c_i > 0L && ni_c_j > 0L) {
              pdc <- list(
                list(ni=ni_c_i, nA=pf_i*2L*ni_c_i, AA=AA_i, AA_corr=AAc_i),
                list(ni=ni_c_j, nA=pf_j*2L*ni_c_j, AA=AA_j, AA_corr=AAc_j))
              cmp_c <- weir_fst_allele(pdc, use_corr=TRUE)
              N2pc <- ni_c_i+ni_c_j; nc_c <- if (N2pc>0) (N2pc-(ni_c_i^2+ni_c_j^2)/N2pc)/1.0 else 0.0
              s1_c <- s1_c + cmp_c$s1*nc_c; s3_c <- s3_c + cmp_c$s3*nc_c
            }
          }

          fst_r <- if (s3_r != 0) round(s1_r/s3_r, 6) else NA_real_
          fst_c <- if (s3_c != 0) round(s1_c/s3_c, 6) else NA_real_

          rows[[length(rows)+1]] <- data.frame(
            Locus = loc, Pop1 = pi_n, Pop2 = pj_n,
            FST_raw = fst_r, FST_ENA = fst_c,
            Delta = round(fst_c - fst_r, 6),
            N_i_raw = ni_raw_i, N_j_raw = ni_raw_j,
            N_i_ENA = ni_c_i,   N_j_ENA = ni_c_j,
            stringsAsFactors = FALSE
          )
        }
      }
      if (length(rows) == 0L) return(data.frame())
      do.call(rbind, rows)
    }

    # ─────────────────────────────────────────────────────────────────────
    # 9. UPDATE DES SELECTINPUTS
    # ─────────────────────────────────────────────────────────────────────
    observe({
      markers <- markers_r(); pops <- pops_r()
      updateSelectInput(session, "t1_locus",
        choices = c("All loci"="all", stats::setNames(markers,markers)), selected = "all")
      updateSelectInput(session, "t1_pop",
        choices = c("All populations"="all", stats::setNames(pops,pops)), selected = "all")
      updateSelectInput(session, "t2_locus",
        choices = c("All loci"="all", stats::setNames(markers,markers)), selected = "all")
      updateSelectInput(session, "fl_locus",
        choices = c("All loci"="all", stats::setNames(markers,markers)), selected = "all")
      updateSelectInput(session, "fl_pop1",
        choices = c("All pairs"="all", stats::setNames(pops,pops)), selected = "all")
      updateSelectInput(session, "fl_pop2",
        choices = c("All pairs"="all", stats::setNames(pops,pops)), selected = "all")
    })

    # ─────────────────────────────────────────────────────────────────────
    # 10. REACTIVES PRINCIPAUX
    # ─────────────────────────────────────────────────────────────────────
    t1_ready_r <- reactive({ req(input$run_t1 > 0L); db_ready(); TRUE })
    t2_ready_r <- reactive({ req(input$run_t2 > 0L); db_ready(); TRUE })

    t1_data_r <- reactive({
      t1_ready_r()
      withProgress(message="Running EM algorithm (FreeNA)...", value=0.2, {
        d <- fetch_and_run_em(
          sel_locus = safe_choice(input$t1_locus,"all"),
          sel_pop   = safe_choice(input$t1_pop,  "all"))
        setProgress(1); d
      })
    })

    t2_data_r <- reactive({
      t2_ready_r()
      withProgress(message="Computing global summary...", value=0.2, {
        sel_loc <- safe_choice(input$t2_locus,"all")
        long <- fetch_and_run_em(sel_locus=sel_loc, sel_pop="all")
        if (nrow(long)==0L) return(data.frame())

        con   <- con_r(); hs <- hf_schema_r(); ms <- meta_schema_r()
        hf_q  <- sql_id(con,tbl_hf_r());  meta_q <- sql_id(con,tbl_meta_r())
        hi_q  <- sql_id(con,hs$ind_col);  hl_q   <- sql_id(con,hs$locus_col)
        hg_q  <- sql_id(con,hs$gt_col);   mi_q   <- sql_id(con,ms$ind_col)
        pop_q <- sql_id(con,ms$pop_col)

        lf_extra <- if (!identical(sel_loc,"all"))
          sprintf(" AND CAST(h.%s AS VARCHAR)=%s", hl_q, sql_str(con,sel_loc)) else ""

        obs <- DBI::dbGetQuery(con, sprintf("
          WITH %s
          SELECT
            CAST(h.%s AS VARCHAR) AS Marker,
            COUNT(*) AS N_tot,
            SUM(CASE WHEN h.%s IS NULL OR h.%s <= 0 THEN 1 ELSE 0 END) AS N_blanks,
            MIN(lo._lo_rank) AS _lo_rank
          FROM %s h
          INNER JOIN %s m
            ON CAST(h.%s AS VARCHAR) = CAST(m.%s AS VARCHAR)
          LEFT JOIN locus_order lo
            ON CAST(h.%s AS VARCHAR) = lo._lo_marker
          WHERE m.%s IS NOT NULL%s
          GROUP BY CAST(h.%s AS VARCHAR)
          ORDER BY _lo_rank ASC",
          locus_order_cte(con,hf_q,hl_q),
          hl_q, hg_q, hg_q,
          hf_q, meta_q, hi_q, mi_q,
          hl_q, pop_q, lf_extra, hl_q))

        locus_levels <- markers_r()
        loci_in_long <- if (!is.null(locus_levels) && length(locus_levels))
          locus_levels[locus_levels %in% unique(long$Locus)]
        else unique(long$Locus)

        rows <- lapply(loci_in_long, function(loc) {
          sub <- long[long$Locus==loc,,drop=FALSE]
          if (nrow(sub)==0L) return(NULL)

          obs_row  <- obs[obs$Marker==loc,,drop=FALSE]
          n_tot    <- if (nrow(obs_row)) as.integer(obs_row$N_tot[1]) else sum(sub$N)
          n_blanks <- if (nrow(obs_row)) as.integer(obs_row$N_blanks[1]) else NA_integer_

          av_n_exp <- sum(sub$N * (sub$p_nulls^2), na.rm=TRUE)
          vidx  <- !is.na(sub$p_nulls)
          av_p  <- if (any(vidx) && sum(sub$N[vidx])>0)
            sum(sub$p_nulls[vidx] * sub$N[vidx]) / sum(sub$N[vidx]) else NA_real_
          f_exp <- if (!is.na(av_n_exp) && n_tot>0) av_n_exp / n_tot else NA_real_

          data.frame(
            Locus = loc,
            Av_N_exp = round(av_n_exp, 9),
            Av_p_nulls = round(av_p, 9),
            N_tot = n_tot,
            N_blanks = n_blanks,
            f_expBlanks = round(f_exp, 9),
            p_nulls = round(av_p, 9),
            stringsAsFactors = FALSE
          )
        })

        setProgress(1)
        do.call(rbind, Filter(Negate(is.null), rows))
      })
    })

    em_r <- reactive({
      db_ready()
      withProgress(message = "EM FreeNA — calculating null allele frequencies...", value=0.1, {
        res <- fetch_em_results()
        setProgress(1); res
      })
    })

    fst_global_r <- eventReactive(input$run_fst_global, {
      req(length(em_r()) > 0)
      withProgress(message = "Calculating global FST (ENA)...", value=0.2, {
        res <- compute_fst_global(em_r())
        setProgress(1); res
      })
    })

    fst_pair_r <- eventReactive(input$run_fst_pair, {
      req(length(em_r()) > 0)
      withProgress(message = "Calculating pairwise FST (ENA)...", value=0.2, {
        res <- compute_fst_pairwise(em_r())
        setProgress(1); res
      })
    })

    dc_r <- eventReactive(input$run_dc, {
      req(length(em_r()) > 0)
      withProgress(message = "Calculating pairwise DCSE (INA)...", value=0.2, {
        res <- compute_dc_pairwise(em_r())
        setProgress(1); res
      })
    })

    fst_locus_r <- eventReactive(input$run_fst_locus, {
      req(length(em_r()) > 0)
      withProgress(message = "Calculating FST per locus × pair...", value=0.2, {
        res <- compute_fst_per_locus_pair(em_r(),
          sel_locus = safe_choice(input$fl_locus, "all"),
          sel_pop1  = safe_choice(input$fl_pop1,  "all"),
          sel_pop2  = safe_choice(input$fl_pop2,  "all"))
        setProgress(1); res
      })
    })

    # ─────────────────────────────────────────────────────────────────────
    # 11. VALUE BOXES (unifiés)
    # ─────────────────────────────────────────────────────────────────────
    output$vb_loci <- renderUI({
      tryCatch(tags$span(length(markers_r())),
               error=function(e) tags$span("\u2014"))
    })
    output$vb_pops <- renderUI({
      tryCatch(tags$span(length(pops_r())),
               error=function(e) tags$span("\u2014"))
    })
    output$vb_n <- renderUI({
      tryCatch({
        db_ready(); con <- con_r(); ms <- meta_schema_r()
        n <- DBI::dbGetQuery(con, sprintf(
          "SELECT COUNT(DISTINCT CAST(%s AS VARCHAR)) AS n FROM %s WHERE %s IS NOT NULL",
          sql_id(con,ms$ind_col), sql_id(con,tbl_meta_r()),
          sql_id(con,ms$ind_col)))$n[[1]]
        tags$span(n)
      }, error=function(e) tags$span("\u2014"))
    })
    output$vb_avg_null <- renderUI({
      tryCatch({
        d <- t1_data_r()
        if (nrow(d)==0||all(is.na(d$p_nulls))) return(tags$span("\u2014"))
        v   <- round(mean(d$p_nulls, na.rm=TRUE), 4)
        col <- if(v>.20)"#9d174d" else if(v>.10)"#854d0e" else "#166534"
        tags$span(style=paste0("color:",col,";"), v)
      }, error=function(e) tags$span("\u2014"))
    })
    output$vb_fst_ena <- renderUI({
      tryCatch({
        r <- fst_global_r()
        v <- round(r$global_ena, 4)
        col <- if (!is.na(v) && v > 0.15) "#9d174d" else if (!is.na(v) && v > 0.05) "#854d0e" else "#166534"
        tags$span(style=paste0("color:",col,";"), if (is.na(v)) "—" else v)
      }, error=function(e) tags$span("\u2014"))
    })

    # ─────────────────────────────────────────────────────────────────────
    # 12. RENDER MATRIX HTML (helper)
    # ─────────────────────────────────────────────────────────────────────
    render_matrix_html <- function(mat, fmt=6, color_thresh=c(0.05,0.15,0.25),
                                   colors=c("#f0fdf4","#dcfce7","#fefce8","#fef2f2")) {
      pops <- rownames(mat)
      n    <- length(pops)
      cells <- function(i,j) {
        v <- mat[i,j]
        if (i == j) return('<td class="diag">—</td>')
        if (i < j || is.na(v)) return('<td style="color:#cbd5e1;">·</td>')
        bg <- colors[findInterval(v, color_thresh) + 1L]
        sprintf('<td style="background:%s;">%s%s', bg, round(v, fmt), '</td>')
      }
      thead <- paste0('<tr><th></th>',
        paste(sprintf('<th>%s</th>', pops[-n]), collapse=""), '</tr>')
      tbody <- paste(sapply(seq_len(n), function(i) {
        if (i == 1L) return("")
        paste0('<tr><td class="pop-label">', pops[i], '</td>',
               paste(sapply(seq_len(n), function(j) cells(i,j)), collapse=""),
               '</tr>')
      }), collapse="")
      HTML(sprintf('<div class="na-matrix-wrap"><table class="na-matrix"><thead>%s</thead><tbody>%s</tbody></table></div>',
                   thead, tbody))
    }

    # ─────────────────────────────────────────────────────────────────────
    # 13. RENDER DES TABLES (onglets 1 à 6)
    # ─────────────────────────────────────────────────────────────────────

    # Onglet 1 — Per locus × population
    output$dt_t1 <- DT::renderDT({
      d <- t1_data_r()
      shiny::validate(shiny::need(nrow(d)>0, "No data. Select parameters and click Compute."))
      disp <- d; names(disp) <- c("Locus names","Farm","p_nulls","N","N_exp_blanks","p_nulls×N")
      DT::datatable(disp, rownames=FALSE,
        options=list(pageLength=20, scrollX=TRUE, dom="lftip",
          columnDefs=list(list(className="dt-right", targets=2:5))),
        class="compact hover stripe") |>
        DT::formatRound("p_nulls", 5) |>
        DT::formatRound("N_exp_blanks", 9) |>
        DT::formatRound("p_nulls×N", 5) |>
        DT::formatStyle("p_nulls",
          backgroundColor=DT::styleInterval(c(0.05,0.10,0.20,0.30),
            c("#f0fdf4","#dcfce7","#fefce8","#fff7ed","#fef2f2"))) |>
        DT::formatStyle("Locus names", fontWeight="600", color="#0f172a")
    }, server=TRUE)

    # Onglet 2 — Global summary per locus
    output$dt_t2 <- DT::renderDT({
      d <- t2_data_r()
      shiny::validate(shiny::need(nrow(d)>0, "No data. Select parameters and click Compute."))
      disp <- d; names(disp) <- c("Locus names","Av(N_exp_blanks)","Av(p_nulls)","N_tot","N_blanks","f(expBlanks)","p_nulls")
      DT::datatable(disp, rownames=FALSE,
        options=list(pageLength=20, scrollX=TRUE, dom="lftip",
          columnDefs=list(list(className="dt-right", targets=1:6))),
        class="compact hover stripe") |>
        DT::formatRound("Av(N_exp_blanks)", 9) |>
        DT::formatRound("Av(p_nulls)", 9) |>
        DT::formatRound("f(expBlanks)", 9) |>
        DT::formatRound("p_nulls", 9) |>
        DT::formatStyle("p_nulls",
          backgroundColor=DT::styleInterval(c(0.05,0.10,0.20),
            c("#f0fdf4","#dcfce7","#fefce8","#fef2f2"))) |>
        DT::formatStyle("Locus names", fontWeight="600", color="#0f172a")
    }, server=TRUE)

    # Onglet 3 — Global FST
    output$dt_fst_global <- DT::renderDT({
      r <- fst_global_r()
      d <- r$per_locus
      shiny::validate(shiny::need(nrow(d) > 0, "No data. Click Calculate."))
      summary_row <- data.frame(
        Locus = paste0("[Multilocus FST_raw=", round(r$global_raw,6),
                       " | FST-ENA=", round(r$global_ena,6), "]"),
        FST_raw = r$global_raw, FST_ENA = r$global_ena,
        Delta_FST = r$global_ena - r$global_raw,
        N_pops_eff_raw = NA_integer_, N_pops_eff_ENA = NA_integer_,
        stringsAsFactors = FALSE
      )
      disp <- rbind(summary_row, d)
      names(disp) <- c("Locus","FST raw","FST-ENA","ΔFST (ENA−raw)","N eff. pops (raw)","N eff. pops (ENA)")
      DT::datatable(disp, rownames=FALSE,
        options=list(pageLength=25, scrollX=TRUE, dom="lftip",
          columnDefs=list(list(className="dt-right", targets=1:5))),
        class="compact hover stripe") |>
        DT::formatRound("FST raw", 6) |>
        DT::formatRound("FST-ENA", 6) |>
        DT::formatRound("ΔFST (ENA−raw)", 6) |>
        DT::formatStyle("FST-ENA",
          backgroundColor=DT::styleInterval(c(0.05,0.15,0.25),
            c("#f0fdf4","#dcfce7","#fefce8","#fef2f2"))) |>
        DT::formatStyle("Locus", fontWeight="600", color="#0f172a")
    }, server=TRUE)

    # Onglet 4 — Pairwise FST
    output$ui_fst_pair_matrix <- renderUI({
      r <- fst_pair_r(); typ <- input$fst_pair_type
      shiny::validate(shiny::need(!is.null(r$matrix_raw), "Click Calculate."))
      if (identical(typ, "both")) {
        tags$div(
          tags$strong("Raw FST"), render_matrix_html(r$matrix_raw),
          tags$br(), tags$strong("FST-ENA"), render_matrix_html(r$matrix_ena)
        )
      } else if (identical(typ, "raw")) {
        render_matrix_html(r$matrix_raw)
      } else {
        render_matrix_html(r$matrix_ena)
      }
    })

    output$dt_fst_pair <- DT::renderDT({
      r <- fst_pair_r()
      d <- r$long
      shiny::validate(shiny::need(nrow(d) > 0, "No data."))
      names(d) <- c("Pop 1","Pop 2","FST raw","FST-ENA","ΔFST (ENA−raw)")
      DT::datatable(d, rownames=FALSE,
        options=list(pageLength=20, scrollX=TRUE, dom="lftip",
          columnDefs=list(list(className="dt-right", targets=2:4))),
        class="compact hover stripe") |>
        DT::formatRound("FST raw", 6) |>
        DT::formatRound("FST-ENA", 6) |>
        DT::formatRound("ΔFST (ENA−raw)", 6) |>
        DT::formatStyle("FST-ENA",
          backgroundColor=DT::styleInterval(c(0.05,0.15,0.25),
            c("#f0fdf4","#dcfce7","#fefce8","#fef2f2")))
    }, server=TRUE)

    # Onglet 5 — Pairwise DCSE
    output$ui_dc_matrix <- renderUI({
      r <- dc_r(); typ <- input$dc_type
      shiny::validate(shiny::need(!is.null(r$matrix_raw), "Click Calculate."))
      clr <- c("#eff6ff","#dbeafe","#fef9c3","#fef2f2"); thr <- c(0.1, 0.25, 0.4)
      if (identical(typ, "both")) {
        tags$div(
          tags$strong("Raw DCSE"), render_matrix_html(r$matrix_raw, color_thresh=thr, colors=clr),
          tags$br(), tags$strong("DCSE-INA"), render_matrix_html(r$matrix_ina, color_thresh=thr, colors=clr)
        )
      } else if (identical(typ, "raw")) {
        render_matrix_html(r$matrix_raw, color_thresh=thr, colors=clr)
      } else {
        render_matrix_html(r$matrix_ina, color_thresh=thr, colors=clr)
      }
    })

    output$dt_dc <- DT::renderDT({
      r <- dc_r(); d <- r$long
      shiny::validate(shiny::need(nrow(d) > 0, "No data."))
      names(d) <- c("Pop 1","Pop 2","DCSE raw","DCSE-INA","ΔDCSE (INA−raw)")
      DT::datatable(d, rownames=FALSE,
        options=list(pageLength=20, scrollX=TRUE, dom="lftip",
          columnDefs=list(list(className="dt-right", targets=2:4))),
        class="compact hover stripe") |>
        DT::formatRound("DCSE raw", 6) |>
        DT::formatRound("DCSE-INA", 6) |>
        DT::formatRound("ΔDCSE (INA−raw)", 6)
    }, server=TRUE)

    # Onglet 6 — FST per locus × pair
    output$dt_fst_locus <- DT::renderDT({
      d <- fst_locus_r()
      shiny::validate(shiny::need(nrow(d) > 0, "No data."))
      names(d) <- c("Locus","Pop 1","Pop 2","FST raw","FST-ENA","ΔFST","N_i raw","N_j raw","N_i ENA","N_j ENA")
      DT::datatable(d, rownames=FALSE,
        options=list(pageLength=25, scrollX=TRUE, dom="lftip",
          columnDefs=list(list(className="dt-right", targets=3:9))),
        class="compact hover stripe") |>
        DT::formatRound("FST raw", 6) |>
        DT::formatRound("FST-ENA", 6) |>
        DT::formatRound("ΔFST", 6) |>
        DT::formatStyle("FST-ENA",
          backgroundColor=DT::styleInterval(c(0.05,0.15,0.25),
            c("#f0fdf4","#dcfce7","#fefce8","#fef2f2"))) |>
        DT::formatStyle("Locus", fontWeight="600", color="#0f172a")
    }, server=TRUE)

    # ─────────────────────────────────────────────────────────────────────
    # 14. DOWNLOAD HANDLERS
    # ─────────────────────────────────────────────────────────────────────
    dl_helper <- function(data_fn, filename_base, col_names=NULL) {
      list(
        csv = downloadHandler(
          filename = function() paste0(filename_base, "_", Sys.Date(), ".csv"),
          content  = function(file) {
            d <- data_fn(); if (is.null(d) || nrow(d)==0) return(invisible(NULL))
            if (!is.null(col_names)) names(d) <- col_names
            write.csv(d, file, row.names=FALSE)
          }
        ),
        txt = downloadHandler(
          filename = function() paste0(filename_base, "_", Sys.Date(), ".txt"),
          content  = function(file) {
            d <- data_fn(); if (is.null(d) || nrow(d)==0) return(invisible(NULL))
            if (!is.null(col_names)) names(d) <- col_names
            write.table(d, file, sep="\t", row.names=FALSE, quote=FALSE)
          }
        )
      )
    }

    # Onglets 1 et 2
    output$dl_t1_csv <- dl_helper(t1_data_r, "null_allele_per_pop_locus",
      c("Locus_names","Farm","p_nulls","N","N_exp_blanks","p_nulls_x_N"))$csv
    output$dl_t1_txt <- dl_helper(t1_data_r, "null_allele_per_pop_locus",
      c("Locus_names","Farm","p_nulls","N","N_exp_blanks","p_nulls_x_N"))$txt
    output$dl_t2_csv <- dl_helper(t2_data_r, "null_allele_global",
      c("Locus_names","Av_N_exp_blanks","Av_p_nulls","N_tot","N_blanks","f_expBlanks","p_nulls"))$csv
    output$dl_t2_txt <- dl_helper(t2_data_r, "null_allele_global",
      c("Locus_names","Av_N_exp_blanks","Av_p_nulls","N_tot","N_blanks","f_expBlanks","p_nulls"))$txt

    # Onglet 3
    dl_fg <- dl_helper(function() fst_global_r()$per_locus, "fst_global_ena",
      c("Locus","FST_raw","FST_ENA","Delta_FST","N_pops_eff_raw","N_pops_eff_ENA"))
    output$dl_fst_global_csv <- dl_fg$csv
    output$dl_fst_global_txt <- dl_fg$txt

    # Onglet 4
    output$dl_fst_pair_csv <- downloadHandler(
      filename = function() paste0("fst_pairwise_ena_", Sys.Date(), ".csv"),
      content  = function(file) {
        r <- fst_pair_r(); mat <- r$matrix_ena
        d <- as.data.frame(round(mat, 6)); d <- cbind(Population=rownames(d), d)
        write.csv(d, file, row.names=FALSE)
      }
    )
    output$dl_fst_pair_txt <- downloadHandler(
      filename = function() paste0("fst_pairwise_ena_", Sys.Date(), ".txt"),
      content  = function(file) {
        r <- fst_pair_r(); mat <- r$matrix_ena
        d <- as.data.frame(round(mat, 6)); d <- cbind(Population=rownames(d), d)
        write.table(d, file, sep="\t", row.names=FALSE, quote=FALSE)
      }
    )
    dl_fp <- dl_helper(function() fst_pair_r()$long, "fst_pairwise_long_ena",
      c("Pop1","Pop2","FST_raw","FST_ENA","Delta_FST"))
    output$dl_fst_pair_long_csv <- dl_fp$csv
    output$dl_fst_pair_long_txt <- dl_fp$txt

    # Onglet 5
    output$dl_dc_csv <- downloadHandler(
      filename = function() paste0("dcse_ina_", Sys.Date(), ".csv"),
      content  = function(file) {
        r <- dc_r(); mat <- r$matrix_ina
        d <- as.data.frame(round(mat, 6)); d <- cbind(Population=rownames(d), d)
        write.csv(d, file, row.names=FALSE)
      }
    )
    output$dl_dc_txt <- downloadHandler(
      filename = function() paste0("dcse_ina_", Sys.Date(), ".txt"),
      content  = function(file) {
        r <- dc_r(); mat <- r$matrix_ina
        d <- as.data.frame(round(mat, 6)); d <- cbind(Population=rownames(d), d)
        write.table(d, file, sep="\t", row.names=FALSE, quote=FALSE)
      }
    )
    dl_dc <- dl_helper(function() dc_r()$long, "dcse_pairwise_long_ina",
      c("Pop1","Pop2","DCSE_raw","DCSE_INA","Delta_DCSE"))
    output$dl_dc_long_csv <- dl_dc$csv
    output$dl_dc_long_txt <- dl_dc$txt

    # Onglet 6
    dl_fl <- dl_helper(fst_locus_r, "fst_per_locus_pair_ena",
      c("Locus","Pop1","Pop2","FST_raw","FST_ENA","Delta_FST",
        "N_i_raw","N_j_raw","N_i_ENA","N_j_ENA"))
    output$dl_fst_locus_csv <- dl_fl$csv
    output$dl_fst_locus_txt <- dl_fl$txt

  })
}