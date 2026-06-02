# module/server_null_alleles.R
# Null allele frequency estimation (EM), FST-ENA correction, and DCSE-INA genetic distance
#
# References:
#   Dempster, Laird & Rubin (1977)  — EM algorithm
#   Chapuis & Estoup (2007)         — FreeNA: ENA and INA corrections
#   Weir (1996)                     — FST following Genepop method
#   Cavalli-Sforza & Edwards (1967) — Chord genetic distance (DCSE)
#
# All algorithms are exact R translations of the Pascal source code of FreeNA.

server_null_alleles <- function(id, rv) {
  moduleServer(id, function(input, output, session) {

    # ── Helpers ────────────────────────────────────────────────────────────────
    `%||%` <- function(a, b) if (!is.null(a)) a else b

    safe_choice <- function(x, default = "all") {
      if (is.null(x) || length(x) == 0L || identical(x, "") || all(is.na(x))) default
      else as.character(x[[1]])
    }

    sql_id  <- function(con, x) as.character(DBI::dbQuoteIdentifier(con, x))
    sql_str <- function(con, x) as.character(DBI::dbQuoteString(con, x))
    treat_id <- function(loc) paste0("treat_", gsub("[^A-Za-z0-9]", "_", loc))

    # Pascal FreeNA error sentinel value
    NOTAPPL <- 20000

    # ── DB plumbing ────────────────────────────────────────────────────────────
    db_tick    <- reactive({ rv$db_tick })
    con_r      <- reactive({ req(rv$con); rv$con })
    tbl_meta_r <- reactive({ rv$tbl_meta %||% "meta" })

    tbl_hf_r <- reactive({
      con <- con_r()
      if (exists("duck_tbl_exists",    mode = "function", inherits = TRUE) &&
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
                 else shiny::validate(shiny::need(FALSE,
                   "No individual column found in meta."))
      pop_col <- c("Population","population","pop","pop_code")[
        c("Population","population","pop","pop_code") %in% cols][1]
      shiny::validate(shiny::need(!is.na(pop_col),
        "No population column found in meta."))
      list(ind_col = ind_col, pop_col = pop_col)
    })

    locus_order_cte <- function(con, hf_tbl_q, hl_q)
      sprintf("locus_order AS (
  SELECT CAST(%s AS VARCHAR) AS _lo_marker, MIN(rowid) AS _lo_rank
  FROM %s GROUP BY CAST(%s AS VARCHAR))", hl_q, hf_tbl_q, hl_q)

    # ── Marker / population lists ──────────────────────────────────────────────
    pops_r <- reactive({
      db_ready(); con <- con_r(); ms <- meta_schema_r()
      as.character(DBI::dbGetQuery(con, sprintf(
        "SELECT DISTINCT CAST(%s AS VARCHAR) AS p FROM %s
         WHERE %s IS NOT NULL ORDER BY p",
        sql_id(con, ms$pop_col), sql_id(con, tbl_meta_r()),
        sql_id(con, ms$pop_col)))$p)
    })

    markers_r <- reactive({
      db_ready(); con <- con_r(); hs <- hf_schema_r()
      hf_q <- sql_id(con, tbl_hf_r()); hl_q <- sql_id(con, hs$locus_col)
      as.character(DBI::dbGetQuery(con, sprintf("
        WITH %s
        SELECT DISTINCT CAST(%s AS VARCHAR) AS Marker, lo._lo_rank
        FROM %s h
        LEFT JOIN locus_order lo ON CAST(%s AS VARCHAR) = lo._lo_marker
        ORDER BY lo._lo_rank ASC",
        locus_order_cte(con, hf_q, hl_q), hl_q, hf_q, hl_q))$Marker)
    })

    observe({
      markers <- markers_r(); pops <- pops_r()
      updateSelectInput(session, "t1_locus",
        choices  = c("All loci" = "all", stats::setNames(markers, markers)),
        selected = "all")
      updateSelectInput(session, "t1_pop",
        choices  = c("All populations" = "all", stats::setNames(pops, pops)),
        selected = "all")
      updateSelectInput(session, "t2_locus",
        choices  = c("All loci" = "all", stats::setNames(markers, markers)),
        selected = "all")
      updateSelectInput(session, "fl_locus",
        choices  = c("All loci" = "all", stats::setNames(markers, markers)),
        selected = "all")
      updateSelectInput(session, "fl_pop1",
        choices  = c("All pairs" = "all", stats::setNames(pops, pops)),
        selected = "all")
      updateSelectInput(session, "fl_pop2",
        choices  = c("All pairs" = "all", stats::setNames(pops, pops)),
        selected = "all")
    })

    # ── Per-locus treatment selector UI ───────────────────────────────────────
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
              "999999 \u2014 null homozygote"      = "null_homo",
              "000000 \u2014 absent / PCR failure" = "absent"
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
        if (is.null(val) || !val %in% c("null_homo", "absent")) "null_homo" else val
      })
      stats::setNames(treats, markers)
    })

    # ══════════════════════════════════════════════════════════════════════════
    #  EM ALGORITHM  rDempster_per_locus — exact translation from Pascal FreeNA
    #
    #  null_homo model (999999 coding):
    #    N = efpop - absentgeno  (null homos counted in denominator)
    #    rd init: sqrt(nnullhomo/N)  or  sqrt(1/(N+1)) if no null homos
    #    rd update: rdi + 2*nnullhomo / (2*N)
    #
    #  absent model (000000 coding):
    #    N = efpop - absentgeno  (same denominator — absents excluded upstream)
    #    rd init: sqrt(1/(N+1))
    #    rd update: rdi  (no null homo contribution)
    #
    #  Returns list:
    #    $rd           numeric — null allele frequency
    #    $pfreq        named numeric — corrected allele frequencies (corrdgenefreq)
    #    $genefreq_obs named numeric — observed allele frequencies (genefreq)
    #    $H_ii         named integer — homozygote counts per allele
    #    $H_iX         named integer — heterozygote counts per allele
    #    $N            integer — effective sample size (denominator)
    #    $efpop        integer — total individuals
    #    $n_absent     integer — absent genotype count
    #    $n_null_homo  integer — null homozygote count (nnullhomo)
    #    $alleles      integer — allele indices
    # ══════════════════════════════════════════════════════════════════════════
    em_freena <- function(gt_vec, base, treat = "null_homo") {
      efpop      <- length(gt_vec)
      absent_msk <- is.na(gt_vec) | gt_vec <= 0L
      n_absent   <- sum(absent_msk)
      valid_gt   <- gt_vec[!absent_msk]

      empty <- list(rd = 0.0, pfreq = numeric(0), genefreq_obs = numeric(0),
                    H_ii = numeric(0), H_iX = numeric(0),
                    N = 0L, efpop = efpop, n_absent = n_absent,
                    n_null_homo = 0L, alleles = integer(0),
                    n_valid_geno = 0L)

      if (length(valid_gt) == 0L) return(empty)

      a1_all <- floor(valid_gt / base)
      a2_all <- valid_gt %% base

      # Null allele code (99 for 2-digit, 999 for 3-digit)
      null_code <- if (base >= 1000L) 999L else 99L

      # Null homozygotes: both alleles == null_code
      null_homo_msk <- (a1_all == null_code) & (a2_all == null_code)
      n_null_homo   <- sum(null_homo_msk)

      # Valid genotypes: neither absent nor null homozygotes
      valid_a1 <- a1_all[!null_homo_msk]
      valid_a2 <- a2_all[!null_homo_msk]
      alleles  <- sort(unique(c(valid_a1, valid_a2)))
      alleles  <- alleles[alleles >= 0L & alleles != null_code]

      # N = efpop - absentgeno  (Pascal: same denominator for both models)
      N <- efpop - n_absent

      if (N == 0L || length(alleles) == 0L) {
        empty$N <- N; empty$n_null_homo <- n_null_homo; return(empty)
      }

      # Observed allele frequencies — denominator: 2*(N - n_null_homo)
      n_valid_geno <- N - n_null_homo
      genefreq_obs <- sapply(alleles, function(a)
        (sum(valid_a1 == a) + sum(valid_a2 == a)) / (2L * n_valid_geno))

      # H_ii: homozygote counts; H_iX: heterozygote counts
      H_ii <- sapply(alleles, function(a) sum(valid_a1 == a & valid_a2 == a))
      H_iX <- sapply(alleles, function(a)
        sum((valid_a1 == a & valid_a2 != a) | (valid_a2 == a & valid_a1 != a)))
      hotot <- sum(H_ii)

      # ── rd initialization (Pascal: lines 860-863) ──────────────────────────
      rd <- if (treat == "null_homo" && n_null_homo > 0L)
              sqrt(n_null_homo / N)
            else
              sqrt(1.0 / (N + 1.0))

      # ── corrdgenefreq initialization — cpt=0 (Pascal: lines 900-910) ───────
      p <- numeric(length(alleles))
      for (ai in seq_along(alleles)) {
        if (genefreq_obs[ai] <= 0) { p[ai] <- 0.0; next }
        ii <- H_ii[ai]; jj <- H_iX[ai]
        if (treat == "null_homo" && n_null_homo > 0L) {
          # Pascal: nnullhomo > 0 branch
          X <- n_null_homo + hotot - ii +
               (N - n_null_homo - hotot) - jj
          Y <- N
        } else {
          # Pascal: nnullhomo = 0 branch (also used for absent model)
          X <- 1.0 + hotot - ii + (N - hotot) - jj
          Y <- N + 1.0
        }
        p[ai] <- 1.0 - sqrt(max(0.0, X / Y))
      }

      # ── EM iteration loop (Pascal: Repeat … Until re=0) ───────────────────
      for (iter in seq_len(5000L)) {
        new_p <- numeric(length(alleles))
        rdi   <- 0.0
        re    <- 0L

        for (ai in seq_along(alleles)) {
          if (genefreq_obs[ai] <= 0) { new_p[ai] <- 0.0; next }
          pa    <- p[ai]
          denom <- pa + 2.0 * rd
          if (denom <= 0) { new_p[ai] <- 0.0; next }

          # Pascal: corrdgenefreq update
          p_new     <- (pa + rd) / denom * (H_ii[ai] / N) +
                       H_iX[ai] / (2.0 * N)
          rdi       <- rdi + rd / denom * (H_ii[ai] / N)
          new_p[ai] <- p_new

          if (abs(p_new - pa) > 1e-6) re <- re + 1L
        }

        # Pascal line 929: rd update with null homo contribution
        rd_new <- if (treat == "null_homo")
                    rdi + (2.0 * n_null_homo) / (2.0 * N)
                  else
                    rdi   # absent model: no null homo contribution

        if (abs(rd_new - rd) > 1e-6) re <- re + 1L
        p  <- new_p
        rd <- max(0.0, rd_new)
        if (re == 0L) break
      }

      a_chr <- as.character(alleles)
      list(
        rd           = rd,
        pfreq        = stats::setNames(p,            a_chr),
        genefreq_obs = stats::setNames(genefreq_obs, a_chr),
        H_ii         = stats::setNames(H_ii,         a_chr),
        H_iX         = stats::setNames(H_iX,         a_chr),
        N            = N,
        efpop        = efpop,
        n_absent     = n_absent,
        n_null_homo  = n_null_homo,
        alleles      = alleles,
        n_valid_geno = n_valid_geno
      )
    }

    # ── Simple EM wrappers used by Tab 1 / Tab 2 (original module) ────────────
    em_null_homo_simple <- function(gt_vec, base)
      em_freena(gt_vec, base, treat = "null_homo")

    em_absent_simple <- function(gt_vec, base)
      em_freena(gt_vec, base, treat = "absent")

    # ══════════════════════════════════════════════════════════════════════════
    #  FST WEIR (1996) — GENEPOP METHOD
    #  Exact translation of loc_gFst_Genepop / loc_pFst_Genepop
    #  and loc_gFst_Genepop_correction / loc_pFst_Genepop_correction
    #
    #  For each allele:
    #    nA  = freq * 2*ni          (allele copy count)
    #    AA  = observed homozygote count
    #    cAA = AA * p/(p+2r)        (ENA-corrected AA, Pascal: cAA)
    #    MSG = (0.5*snA - sAA) / N_tot
    #    MSI = (0.5*snA + sAA - s2A) / (N_tot - r)
    #    MSP = (s2A - 0.5*snA^2/N_tot) / (r-1)
    #    s2G = MSG ; s2I = (MSI-MSG)/2 ; s2P = (MSP-MSI)/(2*nc)
    #    nc  = (N_tot - sum(ni^2)/N_tot) / (r-1)
    #  FST_locus = s1l / s3l = sum(s2P) / sum(s2P+s2I+s2G)
    #
    #  Raw:  ni = efpop - absent - nnullhomo  ;  uses genefreq_obs, AA
    #  ENA:  ni = efpop - absent              ;  uses pfreq, cAA
    # ══════════════════════════════════════════════════════════════════════════

    # Compute Weir (1996) s2P, s2I, s2G for one allele across a set of pops
    # pop_list = list of list(ni, nA, AA, AA_corr)
    # use_corr = TRUE for ENA, FALSE for raw
    weir_components_allele <- function(pop_list, use_corr = FALSE) {
      r     <- length(pop_list)
      N_tot <- sum(sapply(pop_list, `[[`, "ni"))
      N2    <- sum(sapply(pop_list, function(p) p$ni ^ 2))
      if (N_tot == 0L || r < 2L) return(list(s2P = 0.0, s2I = 0.0, s2G = 0.0))

      nc    <- (N_tot - N2 / N_tot) / (r - 1)
      if (nc <= 0 || N_tot - r <= 0) return(list(s2P = 0.0, s2I = 0.0, s2G = 0.0))

      snA  <- sum(sapply(pop_list, `[[`, "nA"))
      s2A  <- sum(sapply(pop_list, function(p)
                    if (p$ni > 0) p$nA ^ 2 / (2 * p$ni) else 0.0))
      sAA  <- if (use_corr)
                sum(sapply(pop_list, `[[`, "AA_corr"))
              else
                sum(sapply(pop_list, `[[`, "AA"))

      MSG  <- (0.5 * snA - sAA) / N_tot
      MSI  <- (0.5 * snA + sAA - s2A) / (N_tot - r)
      MSP  <- (s2A - 0.5 * snA ^ 2 / N_tot) / (r - 1)
      s2G  <- MSG
      s2I  <- 0.5 * (MSI - MSG)
      s2P  <- (MSP - MSI) / (2 * nc)
      list(s2P = s2P, s2I = s2I, s2G = s2G)
    }

    # ══════════════════════════════════════════════════════════════════════════
    #  CAVALLI-SFORZA & EDWARDS (1967) CHORD DISTANCE — prod_CS / prod_CS_correction
    #
    #  CSprod(i,j) = sum_k sqrt(p_ik * p_jk)
    #  DCSE(i,j)   = 2/pi * sqrt(2*(1 - CSprod))   if CSprod <= 1
    #  INA: k includes null allele state with freq = rd[pop]
    #       (Pascal: ajustement_r — corrdgenefreq[iloc,ipop,nallc-1] = rd[iloc,ipop])
    # ══════════════════════════════════════════════════════════════════════════
    cs_distance <- function(freq_i, freq_j) {
      alleles <- union(names(freq_i), names(freq_j))
      csprod  <- 0.0
      for (a in alleles) {
        pi <- freq_i[a]; pj <- freq_j[a]
        pi <- if (is.na(pi)) 0.0 else pi
        pj <- if (is.na(pj)) 0.0 else pj
        if (pi > 0 && pj > 0) csprod <- csprod + sqrt(pi * pj)
      }
      if (csprod > 1.0) return(NA_real_)   # dcnotapploc (Pascal sentinel)
      (2.0 / pi) * sqrt(2.0 * (1.0 - csprod))
    }

    # ══════════════════════════════════════════════════════════════════════════
    #  FETCH ALL GENOTYPES FROM DUCKDB — build em_res[[locus]][[pop]]
    # ══════════════════════════════════════════════════════════════════════════
    em_results_r <- reactive({
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
          h.%s                  AS gt
        FROM %s h
        INNER JOIN %s m
          ON CAST(h.%s AS VARCHAR) = CAST(m.%s AS VARCHAR)
        LEFT JOIN locus_order lo
          ON CAST(h.%s AS VARCHAR) = lo._lo_marker
        WHERE m.%s IS NOT NULL
        ORDER BY lo._lo_rank ASC, Population",
        locus_order_cte(con, hf_q, hl_q),
        pop_q, hl_q, hg_q,
        hf_q, meta_q, hi_q, mi_q, hl_q, pop_q)

      raw <- DBI::dbGetQuery(con, sql)
      if (nrow(raw) == 0L) return(list())

      markers    <- markers_r()
      pops       <- pops_r()
      treatments <- locus_treatments_r()

      em_res <- list()
      for (loc in markers) {
        em_res[[loc]] <- list()
        treat <- as.character(treatments[loc] %||% "null_homo")
        for (pop in pops) {
          gts <- raw$gt[raw$Marker == loc & raw$Population == pop]
          em_res[[loc]][[pop]] <-
            if (length(gts) == 0L)
              list(rd=0.0, pfreq=numeric(0), genefreq_obs=numeric(0),
                   H_ii=numeric(0), H_iX=numeric(0), N=0L,
                   efpop=0L, n_absent=0L, n_null_homo=0L,
                   alleles=integer(0), n_valid_geno=0L)
            else
              em_freena(gts, base, treat)
        }
      }
      em_res
    })

    # ══════════════════════════════════════════════════════════════════════════
    #  TABS 1 & 2  — original p_nulls per locus x population
    #  (same logic as original module, kept intact)
    # ══════════════════════════════════════════════════════════════════════════
    fetch_and_run_em_simple <- function(sel_locus = "all", sel_pop = "all") {
      db_ready()
      con   <- con_r(); hs <- hf_schema_r(); ms <- meta_schema_r()
      base  <- as.integer(base_r())
      hf_q  <- sql_id(con, tbl_hf_r());  meta_q <- sql_id(con, tbl_meta_r())
      hi_q  <- sql_id(con, hs$ind_col);  hl_q   <- sql_id(con, hs$locus_col)
      hg_q  <- sql_id(con, hs$gt_col);   mi_q   <- sql_id(con, ms$ind_col)
      pop_q <- sql_id(con, ms$pop_col)

      filters <- character(0)
      if (!identical(sel_locus, "all"))
        filters <- c(filters, sprintf("CAST(h.%s AS VARCHAR)=%s",
                                      hl_q, sql_str(con, sel_locus)))
      if (!identical(sel_pop, "all"))
        filters <- c(filters, sprintf("CAST(m.%s AS VARCHAR)=%s",
                                      pop_q, sql_str(con, sel_pop)))
      w_extra <- if (length(filters))
        paste0(" AND ", paste(filters, collapse = " AND ")) else ""

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
        locus_order_cte(con, hf_q, hl_q),
        pop_q, hl_q, hg_q,
        hf_q, meta_q, hi_q, mi_q, hl_q, pop_q, w_extra)

      raw <- DBI::dbGetQuery(con, sql)
      if (nrow(raw) == 0L) return(data.frame())

      treatments   <- locus_treatments_r()
      locus_levels <- markers_r()
      combos       <- unique(raw[, c("Population", "Marker"), drop = FALSE])
      results      <- vector("list", nrow(combos))

      for (i in seq_len(nrow(combos))) {
        pop_i  <- combos$Population[i]
        mark_i <- combos$Marker[i]
        gts    <- raw$gt[raw$Population == pop_i & raw$Marker == mark_i]
        treat  <- as.character(treatments[mark_i] %||% "null_homo")
        em     <- em_freena(gts, base, treat)
        n_exp  <- em$N * (em$rd ^ 2)
        results[[i]] <- data.frame(
          Locus        = mark_i,
          Population   = pop_i,
          p_nulls      = round(em$rd,        5),
          N            = as.integer(em$N),
          N_exp_blanks = round(n_exp,         9),
          p_nulls_x_N  = round(em$rd * em$N, 5),
          stringsAsFactors = FALSE
        )
      }

      out <- do.call(rbind, results)
      if (!is.null(locus_levels) && length(locus_levels)) {
        out$Locus <- factor(out$Locus, levels = locus_levels)
        out <- out[order(out$Locus, out$Population), ]
        out$Locus <- as.character(out$Locus)
      }
      out
    }

    # ── Ready guards ───────────────────────────────────────────────────────────
    t1_ready_r <- reactive({ req(input$run_t1 > 0L); db_ready(); TRUE })
    t2_ready_r <- reactive({ req(input$run_t2 > 0L); db_ready(); TRUE })

    t1_data_r <- reactive({
      t1_ready_r()
      withProgress(message = "Running EM algorithm (FreeNA)...", value = 0.2, {
        d <- fetch_and_run_em_simple(
          sel_locus = safe_choice(input$t1_locus, "all"),
          sel_pop   = safe_choice(input$t1_pop,   "all"))
        setProgress(1); d
      })
    })

    t2_data_r <- reactive({
      t2_ready_r()
      withProgress(message = "Computing global summary...", value = 0.2, {
        sel_loc <- safe_choice(input$t2_locus, "all")
        long    <- fetch_and_run_em_simple(sel_locus = sel_loc, sel_pop = "all")
        if (nrow(long) == 0L) return(data.frame())

        # Observed missing counts per locus
        db_ready()
        con   <- con_r(); hs <- hf_schema_r(); ms <- meta_schema_r()
        hf_q  <- sql_id(con, tbl_hf_r());  meta_q <- sql_id(con, tbl_meta_r())
        hi_q  <- sql_id(con, hs$ind_col);  hl_q   <- sql_id(con, hs$locus_col)
        hg_q  <- sql_id(con, hs$gt_col);   mi_q   <- sql_id(con, ms$ind_col)
        pop_q <- sql_id(con, ms$pop_col)

        lf_extra <- if (!identical(sel_loc, "all"))
          sprintf(" AND CAST(h.%s AS VARCHAR)=%s", hl_q, sql_str(con, sel_loc)) else ""

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
          locus_order_cte(con, hf_q, hl_q),
          hl_q, hg_q, hg_q,
          hf_q, meta_q, hi_q, mi_q, hl_q, pop_q, lf_extra, hl_q))

        locus_levels <- markers_r()
        loci_in_long <- if (!is.null(locus_levels) && length(locus_levels))
          locus_levels[locus_levels %in% unique(long$Locus)]
        else unique(long$Locus)

        rows <- lapply(loci_in_long, function(loc) {
          sub     <- long[long$Locus == loc, , drop = FALSE]
          if (nrow(sub) == 0L) return(NULL)
          obs_row  <- obs[obs$Marker == loc, , drop = FALSE]
          n_tot    <- if (nrow(obs_row)) as.integer(obs_row$N_tot[1])    else sum(sub$N)
          n_blanks <- if (nrow(obs_row)) as.integer(obs_row$N_blanks[1]) else NA_integer_
          av_n_exp <- sum(sub$N * (sub$p_nulls ^ 2), na.rm = TRUE)
          vidx     <- !is.na(sub$p_nulls)
          av_p     <- if (any(vidx) && sum(sub$N[vidx]) > 0)
            sum(sub$p_nulls[vidx] * sub$N[vidx]) / sum(sub$N[vidx]) else NA_real_
          f_exp    <- if (!is.na(av_n_exp) && n_tot > 0) av_n_exp / n_tot else NA_real_
          data.frame(
            Locus       = loc,
            Av_N_exp    = round(av_n_exp, 9),
            Av_p_nulls  = round(av_p,     9),
            N_tot       = n_tot,
            N_blanks    = n_blanks,
            f_expBlanks = round(f_exp,    9),
            p_nulls     = round(av_p,     9),
            stringsAsFactors = FALSE
          )
        })

        setProgress(1)
        do.call(rbind, Filter(Negate(is.null), rows))
      })
    })

    # ══════════════════════════════════════════════════════════════════════════
    #  TAB 3 — GLOBAL FST (raw + ENA-corrected, multilocus)
    #  Translation of sum_stats + sum_stats_correction + final_stats
    #  + loc_gFst_Genepop + loc_gFst_Genepop_correction
    # ══════════════════════════════════════════════════════════════════════════
    compute_fst_global <- function(em_res) {
      markers <- names(em_res)
      pops    <- names(em_res[[markers[1]]])
      n_pops  <- length(pops)

      s1 <- s3 <- s1c <- s3c <- 0.0
      rows <- vector("list", length(markers))

      for (li in seq_along(markers)) {
        loc    <- markers[li]
        em_loc <- em_res[[loc]]

        # All observed alleles at this locus
        alleles_obs <- sort(unique(unlist(lapply(em_loc, function(e) e$alleles))))

        # ── Effective sample sizes ──────────────────────────────────────────
        # Raw:  ni = efpop - absent - nnullhomo  (Pascal: loc_gFst_Genepop)
        # ENA:  ni = efpop - absent              (Pascal: loc_gFst_Genepop_correction)
        ni_raw  <- sapply(pops, function(p) {
          e <- em_loc[[p]]
          max(0L, e$efpop - e$n_absent - e$n_null_homo)
        })
        ni_corr <- sapply(pops, function(p) {
          e <- em_loc[[p]]
          max(0L, e$efpop - e$n_absent)
        })

        r_raw  <- sum(ni_raw  > 0L)
        r_corr <- sum(ni_corr > 0L)

        # Pascal: ntoteff, ntoteff2, npopeff
        N_raw    <- sum(ni_raw);   N2_raw  <- sum(ni_raw  ^ 2)
        N_corr   <- sum(ni_corr);  N2_corr <- sum(ni_corr ^ 2)

        nc_raw  <- if (N_raw  > 0 && r_raw  > 1)
                     (N_raw  - N2_raw  / N_raw)  / (r_raw  - 1) else 0.0
        nc_corr <- if (N_corr > 0 && r_corr > 1)
                     (N_corr - N2_corr / N_corr) / (r_corr - 1) else 0.0

        s1l <- s3l <- s1lc <- s3lc <- 0.0

        for (a in alleles_obs) {
          a_chr <- as.character(a)

          # ── Raw (loc_gFst_Genepop) ──────────────────────────────────────
          pop_raw <- lapply(pops, function(p) {
            e  <- em_loc[[p]]
            ni <- max(0L, e$efpop - e$n_absent - e$n_null_homo)
            pf <- if (!is.null(e$genefreq_obs) && a_chr %in% names(e$genefreq_obs))
                    e$genefreq_obs[[a_chr]] else 0.0
            AA <- if (!is.null(e$H_ii) && a_chr %in% names(e$H_ii))
                    e$H_ii[[a_chr]] else 0L
            list(ni = ni, nA = pf * 2L * ni, AA = AA, AA_corr = AA)
          })
          cmp  <- weir_components_allele(pop_raw, use_corr = FALSE)
          s1l  <- s1l  + cmp$s2P
          s3l  <- s3l  + cmp$s2P + cmp$s2I + cmp$s2G

          # ── ENA (loc_gFst_Genepop_correction) ──────────────────────────
          pop_ena <- lapply(pops, function(p) {
            e  <- em_loc[[p]]
            ni <- max(0L, e$efpop - e$n_absent)
            pf <- if (!is.null(e$pfreq) && a_chr %in% names(e$pfreq))
                    e$pfreq[[a_chr]] else 0.0
            rd <- e$rd
            AA <- if (!is.null(e$H_ii) && a_chr %in% names(e$H_ii))
                    e$H_ii[[a_chr]] else 0L
            # Pascal: cAA = AA * p / (p + 2*r)
            denom <- pf + 2.0 * rd
            AA_c  <- if (AA > 0 && denom > 0) AA * (pf / denom) else 0.0
            list(ni = ni, nA = pf * 2L * ni, AA = AA, AA_corr = AA_c)
          })
          cmpc <- weir_components_allele(pop_ena, use_corr = TRUE)
          s1lc <- s1lc + cmpc$s2P
          s3lc <- s3lc + cmpc$s2P + cmpc$s2I + cmpc$s2G
        }

        # Pascal: fst_gen[iloc] = s1l/s3l (or 20000 if s3l=0)
        fst_loc  <- if (s3l  != 0) s1l  / s3l  else NA_real_
        fst_locc <- if (s3lc != 0) s1lc / s3lc else NA_real_

        # Pascal: sum_stats — weighted accumulation by nc
        if (!is.na(fst_loc)  && nc_raw  > 0)
          { s1 <- s1 + s1l * nc_raw;   s3 <- s3 + s3l * nc_raw }
        if (!is.na(fst_locc) && nc_corr > 0)
          { s1c <- s1c + s1lc * nc_corr; s3c <- s3c + s3lc * nc_corr }

        rows[[li]] <- data.frame(
          Locus           = loc,
          FST_raw         = round(fst_loc,  6),
          FST_ENA         = round(fst_locc, 6),
          Delta_FST       = round(fst_locc - fst_loc, 6),
          N_pops_eff_raw  = r_raw,
          N_pops_eff_ENA  = r_corr,
          stringsAsFactors = FALSE
        )
      }

      list(
        global_raw = if (s3  > 0) s1  / s3  else NA_real_,
        global_ena = if (s3c > 0) s1c / s3c else NA_real_,
        per_locus  = do.call(rbind, rows)
      )
    }

    # ══════════════════════════════════════════════════════════════════════════
    #  TAB 4 — PAIRWISE FST (raw + ENA)
    #  Translation of loc_pFst_Genepop + loc_pFst_Genepop_correction
    #  + sum_stats pairwise section + final_stats pairwise section
    # ══════════════════════════════════════════════════════════════════════════
    compute_fst_pairwise <- function(em_res) {
      markers <- names(em_res)
      pops    <- names(em_res[[markers[1]]])
      n_pops  <- length(pops)
      if (n_pops < 2L) return(list(matrix_raw = NULL, matrix_ena = NULL,
                                   long = data.frame()))

      # Pascal: s12p, s32p, s12p_corr, s32p_corr — accumulators per pair
      s12p  <- matrix(0.0, n_pops, n_pops, dimnames = list(pops, pops))
      s32p  <- matrix(0.0, n_pops, n_pops, dimnames = list(pops, pops))
      s12pc <- matrix(0.0, n_pops, n_pops, dimnames = list(pops, pops))
      s32pc <- matrix(0.0, n_pops, n_pops, dimnames = list(pops, pops))

      for (loc in markers) {
        em_loc      <- em_res[[loc]]
        alleles_obs <- sort(unique(unlist(lapply(em_loc, function(e) e$alleles))))

        for (ii in seq_len(n_pops - 1L)) {
          for (jj in seq(ii + 1L, n_pops)) {
            pi_n <- pops[ii]; pj_n <- pops[jj]
            ei <- em_loc[[pi_n]]; ej <- em_loc[[pj_n]]

            # Pascal: ntoteff_2p = (efpop - absent - nnullhomo) for raw
            ni_raw_i <- max(0L, ei$efpop - ei$n_absent - ei$n_null_homo)
            ni_raw_j <- max(0L, ej$efpop - ej$n_absent - ej$n_null_homo)
            # Pascal: ntoteff_2p_corr = (efpop - absent) for ENA
            ni_c_i   <- max(0L, ei$efpop - ei$n_absent)
            ni_c_j   <- max(0L, ej$efpop - ej$n_absent)

            N_raw  <- ni_raw_i + ni_raw_j
            N2_raw <- ni_raw_i ^ 2 + ni_raw_j ^ 2
            N_c    <- ni_c_i   + ni_c_j
            N2_c   <- ni_c_i   ^ 2 + ni_c_j ^ 2

            # Pascal: pair = 2 if both pops have individuals, 1 if only one, 0 if none
            # nc = (N_tot - N2/N_tot) / (pair-1) with pair=2 -> denominator=1
            nc_raw <- if (N_raw > 0 && ni_raw_i > 0 && ni_raw_j > 0)
                        (N_raw - N2_raw / N_raw) else 0.0
            nc_c   <- if (N_c   > 0 && ni_c_i   > 0 && ni_c_j   > 0)
                        (N_c   - N2_c   / N_c)   else 0.0

            for (a in alleles_obs) {
              a_chr <- as.character(a)

              # ── Raw ────────────────────────────────────────────────────
              if (nc_raw > 0) {
                pd <- list(
                  list(ni  = ni_raw_i,
                       nA  = (if (a_chr %in% names(ei$genefreq_obs)) ei$genefreq_obs[[a_chr]] else 0.0) * 2L * ni_raw_i,
                       AA  = if (a_chr %in% names(ei$H_ii)) ei$H_ii[[a_chr]] else 0L,
                       AA_corr = 0.0),
                  list(ni  = ni_raw_j,
                       nA  = (if (a_chr %in% names(ej$genefreq_obs)) ej$genefreq_obs[[a_chr]] else 0.0) * 2L * ni_raw_j,
                       AA  = if (a_chr %in% names(ej$H_ii)) ej$H_ii[[a_chr]] else 0L,
                       AA_corr = 0.0)
                )
                cmp <- weir_components_allele(pd, use_corr = FALSE)
                s12p[ii, jj] <- s12p[ii, jj] + cmp$s2P * nc_raw
                s32p[ii, jj] <- s32p[ii, jj] +
                  (cmp$s2P + cmp$s2I + cmp$s2G) * nc_raw
              }

              # ── ENA ────────────────────────────────────────────────────
              if (nc_c > 0) {
                pf_i <- if (a_chr %in% names(ei$pfreq)) ei$pfreq[[a_chr]] else 0.0
                pf_j <- if (a_chr %in% names(ej$pfreq)) ej$pfreq[[a_chr]] else 0.0
                AA_i <- if (a_chr %in% names(ei$H_ii)) ei$H_ii[[a_chr]] else 0L
                AA_j <- if (a_chr %in% names(ej$H_ii)) ej$H_ii[[a_chr]] else 0L
                di <- pf_i + 2.0 * ei$rd
                dj <- pf_j + 2.0 * ej$rd
                AAc_i <- if (AA_i > 0 && di > 0) AA_i * (pf_i / di) else 0.0
                AAc_j <- if (AA_j > 0 && dj > 0) AA_j * (pf_j / dj) else 0.0
                pdc <- list(
                  list(ni = ni_c_i, nA = pf_i * 2L * ni_c_i,
                       AA = AA_i, AA_corr = AAc_i),
                  list(ni = ni_c_j, nA = pf_j * 2L * ni_c_j,
                       AA = AA_j, AA_corr = AAc_j)
                )
                cmpc <- weir_components_allele(pdc, use_corr = TRUE)
                s12pc[ii, jj] <- s12pc[ii, jj] + cmpc$s2P * nc_c
                s32pc[ii, jj] <- s32pc[ii, jj] +
                  (cmpc$s2P + cmpc$s2I + cmpc$s2G) * nc_c
              }
            }
          }
        }
      }

      # Pascal: final_stats — Fst_2p = s12p/s32p
      mat_raw <- matrix(NA_real_, n_pops, n_pops, dimnames = list(pops, pops))
      mat_ena <- matrix(NA_real_, n_pops, n_pops, dimnames = list(pops, pops))
      for (ii in seq_len(n_pops - 1L)) {
        for (jj in seq(ii + 1L, n_pops)) {
          mat_raw[jj, ii] <- if (s32p[ii,jj]  > 0) s12p[ii,jj]  / s32p[ii,jj]  else NA_real_
          mat_ena[jj, ii] <- if (s32pc[ii,jj] > 0) s12pc[ii,jj] / s32pc[ii,jj] else NA_real_
        }
      }

      long_rows <- list()
      for (ii in seq_len(n_pops - 1L))
        for (jj in seq(ii + 1L, n_pops))
          long_rows[[length(long_rows) + 1L]] <- data.frame(
            Pop1      = pops[ii], Pop2 = pops[jj],
            FST_raw   = round(mat_raw[jj, ii], 6),
            FST_ENA   = round(mat_ena[jj, ii], 6),
            Delta_FST = round(mat_ena[jj, ii] - mat_raw[jj, ii], 6),
            stringsAsFactors = FALSE
          )

      list(matrix_raw = mat_raw, matrix_ena = mat_ena,
           long = do.call(rbind, long_rows))
    }

    # ══════════════════════════════════════════════════════════════════════════
    #  TAB 5 — PAIRWISE DCSE (raw + INA-corrected)
    #  Translation of prod_CS + prod_CS_correction
    #  + sum_stats DCSE section + final_stats DCSE section
    # ══════════════════════════════════════════════════════════════════════════
    compute_dc_pairwise <- function(em_res) {
      markers <- names(em_res)
      pops    <- names(em_res[[markers[1]]])
      n_pops  <- length(pops)
      if (n_pops < 2L) return(list(matrix_raw = NULL, matrix_ina = NULL,
                                   long = data.frame()))

      # Pascal: Dc_2p, Dc_2p_corr — accumulated sum of per-locus distances
      # Pascal: nloceff, nloceff_corr — valid locus counter per pair
      dc_sum_raw  <- matrix(0.0, n_pops, n_pops, dimnames = list(pops, pops))
      dc_sum_ina  <- matrix(0.0, n_pops, n_pops, dimnames = list(pops, pops))
      nloc_eff    <- matrix(length(markers), n_pops, n_pops, dimnames = list(pops, pops))
      nloc_eff_c  <- matrix(length(markers), n_pops, n_pops, dimnames = list(pops, pops))

      for (loc in markers) {
        em_loc <- em_res[[loc]]

        for (ii in seq_len(n_pops - 1L)) {
          for (jj in seq(ii + 1L, n_pops)) {
            pi_n <- pops[ii]; pj_n <- pops[jj]
            ei <- em_loc[[pi_n]]; ej <- em_loc[[pj_n]]

            ni_raw_i <- ei$efpop - ei$n_absent - ei$n_null_homo
            ni_raw_j <- ej$efpop - ej$n_absent - ej$n_null_homo
            ni_c_i   <- ei$efpop - ei$n_absent
            ni_c_j   <- ej$efpop - ej$n_absent

            # ── Raw DCSE — prod_CS (Pascal lines 1090-1106) ─────────────
            # dcnotapploc if CSprod > 1 or either pop has no valid individuals
            if (ni_raw_i > 0L && ni_raw_j > 0L &&
                length(ei$genefreq_obs) > 0 && length(ej$genefreq_obs) > 0) {
              d_raw <- cs_distance(ei$genefreq_obs, ej$genefreq_obs)
              if (!is.na(d_raw)) {
                dc_sum_raw[jj, ii] <- dc_sum_raw[jj, ii] + d_raw
              } else {
                nloc_eff[jj, ii] <- nloc_eff[jj, ii] - 1L
              }
            } else {
              nloc_eff[jj, ii] <- nloc_eff[jj, ii] - 1L
            }

            # ── INA DCSE — prod_CS_correction (Pascal lines 1252-1283) ──
            # ajustement_r: add null allele state freq = rd[iloc,ipop]
            if (ni_c_i > 0L && ni_c_j > 0L &&
                length(ei$pfreq) > 0 && length(ej$pfreq) > 0) {
              freq_ina_i <- c(ei$pfreq, `null` = ei$rd)
              freq_ina_j <- c(ej$pfreq, `null` = ej$rd)
              d_ina <- cs_distance(freq_ina_i, freq_ina_j)
              if (!is.na(d_ina)) {
                dc_sum_ina[jj, ii] <- dc_sum_ina[jj, ii] + d_ina
              } else {
                nloc_eff_c[jj, ii] <- nloc_eff_c[jj, ii] - 1L
              }
            } else {
              nloc_eff_c[jj, ii] <- nloc_eff_c[jj, ii] - 1L
            }
          }
        }
      }

      # Pascal: final Dc_2p = Dc_2p / nloceff
      mat_raw <- matrix(NA_real_, n_pops, n_pops, dimnames = list(pops, pops))
      mat_ina <- matrix(NA_real_, n_pops, n_pops, dimnames = list(pops, pops))
      for (ii in seq_len(n_pops - 1L)) {
        for (jj in seq(ii + 1L, n_pops)) {
          mat_raw[jj, ii] <- if (nloc_eff[jj,ii]   > 0L)
            dc_sum_raw[jj,ii] / nloc_eff[jj,ii]   else NA_real_
          mat_ina[jj, ii] <- if (nloc_eff_c[jj,ii] > 0L)
            dc_sum_ina[jj,ii] / nloc_eff_c[jj,ii] else NA_real_
        }
      }

      long_rows <- list()
      for (ii in seq_len(n_pops - 1L))
        for (jj in seq(ii + 1L, n_pops))
          long_rows[[length(long_rows) + 1L]] <- data.frame(
            Pop1       = pops[ii], Pop2 = pops[jj],
            DCSE_raw   = round(mat_raw[jj, ii], 6),
            DCSE_INA   = round(mat_ina[jj, ii], 6),
            Delta_DCSE = round(mat_ina[jj, ii] - mat_raw[jj, ii], 6),
            stringsAsFactors = FALSE
          )

      list(matrix_raw = mat_raw, matrix_ina = mat_ina,
           long = do.call(rbind, long_rows))
    }

    # ══════════════════════════════════════════════════════════════════════════
    #  TAB 6 — PER-LOCUS FST FOR EACH PAIR (raw + ENA)
    # ══════════════════════════════════════════════════════════════════════════
    compute_fst_per_locus_pair <- function(em_res,
                                           sel_locus = "all",
                                           sel_pop1  = "all",
                                           sel_pop2  = "all") {
      markers <- names(em_res)
      pops    <- names(em_res[[markers[1]]])
      if (!identical(sel_locus, "all")) markers <- markers[markers == sel_locus]

      all_pairs <- combn(pops, 2, simplify = FALSE)
      pairs <- if (!identical(sel_pop1, "all") && !identical(sel_pop2, "all"))
                 list(c(sel_pop1, sel_pop2))
               else if (!identical(sel_pop1, "all"))
                 Filter(function(x) sel_pop1 %in% x, all_pairs)
               else if (!identical(sel_pop2, "all"))
                 Filter(function(x) sel_pop2 %in% x, all_pairs)
               else
                 all_pairs

      rows <- list()
      for (loc in markers) {
        em_loc      <- em_res[[loc]]
        alleles_obs <- sort(unique(unlist(lapply(em_loc, function(e) e$alleles))))

        for (pair in pairs) {
          pi_n <- pair[1]; pj_n <- pair[2]
          if (!pi_n %in% pops || !pj_n %in% pops) next
          ei <- em_loc[[pi_n]]; ej <- em_loc[[pj_n]]

          ni_raw_i <- max(0L, ei$efpop - ei$n_absent - ei$n_null_homo)
          ni_raw_j <- max(0L, ej$efpop - ej$n_absent - ej$n_null_homo)
          ni_c_i   <- max(0L, ei$efpop - ei$n_absent)
          ni_c_j   <- max(0L, ej$efpop - ej$n_absent)

          N_raw <- ni_raw_i + ni_raw_j
          N_c   <- ni_c_i   + ni_c_j
          nc_raw <- if (N_raw > 0 && ni_raw_i > 0 && ni_raw_j > 0)
                      (N_raw - (ni_raw_i^2 + ni_raw_j^2) / N_raw) else 0.0
          nc_c   <- if (N_c   > 0 && ni_c_i   > 0 && ni_c_j   > 0)
                      (N_c   - (ni_c_i^2   + ni_c_j^2)   / N_c)   else 0.0

          s1_r <- s3_r <- s1_c <- s3_c <- 0.0

          for (a in alleles_obs) {
            a_chr <- as.character(a)

            if (nc_raw > 0) {
              pd <- list(
                list(ni = ni_raw_i,
                     nA = (if (a_chr %in% names(ei$genefreq_obs)) ei$genefreq_obs[[a_chr]] else 0.0) * 2L * ni_raw_i,
                     AA = if (a_chr %in% names(ei$H_ii)) ei$H_ii[[a_chr]] else 0L,
                     AA_corr = 0.0),
                list(ni = ni_raw_j,
                     nA = (if (a_chr %in% names(ej$genefreq_obs)) ej$genefreq_obs[[a_chr]] else 0.0) * 2L * ni_raw_j,
                     AA = if (a_chr %in% names(ej$H_ii)) ej$H_ii[[a_chr]] else 0L,
                     AA_corr = 0.0)
              )
              cmp  <- weir_components_allele(pd, use_corr = FALSE)
              s1_r <- s1_r + cmp$s2P * nc_raw
              s3_r <- s3_r + (cmp$s2P + cmp$s2I + cmp$s2G) * nc_raw
            }

            if (nc_c > 0) {
              pf_i <- if (a_chr %in% names(ei$pfreq)) ei$pfreq[[a_chr]] else 0.0
              pf_j <- if (a_chr %in% names(ej$pfreq)) ej$pfreq[[a_chr]] else 0.0
              AA_i <- if (a_chr %in% names(ei$H_ii)) ei$H_ii[[a_chr]] else 0L
              AA_j <- if (a_chr %in% names(ej$H_ii)) ej$H_ii[[a_chr]] else 0L
              di   <- pf_i + 2.0 * ei$rd; dj <- pf_j + 2.0 * ej$rd
              pdc  <- list(
                list(ni = ni_c_i, nA = pf_i * 2L * ni_c_i, AA = AA_i,
                     AA_corr = if (AA_i > 0 && di > 0) AA_i*(pf_i/di) else 0.0),
                list(ni = ni_c_j, nA = pf_j * 2L * ni_c_j, AA = AA_j,
                     AA_corr = if (AA_j > 0 && dj > 0) AA_j*(pf_j/dj) else 0.0)
              )
              cmpc <- weir_components_allele(pdc, use_corr = TRUE)
              s1_c <- s1_c + cmpc$s2P * nc_c
              s3_c <- s3_c + (cmpc$s2P + cmpc$s2I + cmpc$s2G) * nc_c
            }
          }

          rows[[length(rows) + 1L]] <- data.frame(
            Locus    = loc, Pop1 = pi_n, Pop2 = pj_n,
            FST_raw  = round(if (s3_r != 0) s1_r / s3_r else NA_real_, 6),
            FST_ENA  = round(if (s3_c != 0) s1_c / s3_c else NA_real_, 6),
            Delta    = round((if (s3_c != 0) s1_c/s3_c else NA_real_) -
                             (if (s3_r != 0) s1_r/s3_r else NA_real_), 6),
            N_i_raw  = ni_raw_i, N_j_raw = ni_raw_j,
            N_i_ENA  = ni_c_i,   N_j_ENA = ni_c_j,
            stringsAsFactors = FALSE
          )
        }
      }
      if (length(rows) == 0L) return(data.frame())
      do.call(rbind, rows)
    }

    # ── Event-triggered reactives for tabs 3-6 ────────────────────────────────
    em_r <- reactive({
      db_ready()
      withProgress(message = "EM FreeNA — computing null allele frequencies...",
                   value = 0.1, { res <- em_results_r(); setProgress(1); res })
    })

    fst_global_r <- eventReactive(input$run_fst_global, {
      req(length(em_r()) > 0)
      withProgress(message = "Computing global FST (ENA)...", value = 0.2,
        { res <- compute_fst_global(em_r()); setProgress(1); res })
    })

    fst_pair_r <- eventReactive(input$run_fst_pair, {
      req(length(em_r()) > 0)
      withProgress(message = "Computing pairwise FST (ENA)...", value = 0.2,
        { res <- compute_fst_pairwise(em_r()); setProgress(1); res })
    })

    dc_r <- eventReactive(input$run_dc, {
      req(length(em_r()) > 0)
      withProgress(message = "Computing pairwise DCSE (INA)...", value = 0.2,
        { res <- compute_dc_pairwise(em_r()); setProgress(1); res })
    })

    fst_locus_r <- eventReactive(input$run_fst_locus, {
      req(length(em_r()) > 0)
      withProgress(message = "Computing per-locus FST per pair...", value = 0.2, {
        res <- compute_fst_per_locus_pair(em_r(),
          sel_locus = safe_choice(input$fl_locus, "all"),
          sel_pop1  = safe_choice(input$fl_pop1,  "all"),
          sel_pop2  = safe_choice(input$fl_pop2,  "all"))
        setProgress(1); res
      })
    })

    # ── Value boxes ────────────────────────────────────────────────────────────
    output$vb_loci <- renderUI({
      tryCatch(tags$span(length(markers_r())), error = function(e) tags$span("\u2014"))
    })
    output$vb_pops <- renderUI({
      tryCatch(tags$span(length(pops_r())), error = function(e) tags$span("\u2014"))
    })
    output$vb_n <- renderUI({
      tryCatch({
        db_ready(); con <- con_r(); ms <- meta_schema_r()
        n <- DBI::dbGetQuery(con, sprintf(
          "SELECT COUNT(DISTINCT CAST(%s AS VARCHAR)) AS n FROM %s WHERE %s IS NOT NULL",
          sql_id(con, ms$ind_col), sql_id(con, tbl_meta_r()),
          sql_id(con, ms$ind_col)))$n[[1]]
        tags$span(n)
      }, error = function(e) tags$span("\u2014"))
    })
    output$vb_avg_null <- renderUI({
      tryCatch({
        d <- t1_data_r()
        if (nrow(d) == 0 || all(is.na(d$p_nulls))) return(tags$span("\u2014"))
        v   <- round(mean(d$p_nulls, na.rm = TRUE), 4)
        col <- if (v > .20) "#9d174d" else if (v > .10) "#854d0e" else "#166534"
        tags$span(style = paste0("color:", col, ";"), v)
      }, error = function(e) tags$span("\u2014"))
    })
    output$vb_max_null <- renderUI({
      tryCatch({
        d <- t1_data_r()
        if (nrow(d) == 0 || all(is.na(d$p_nulls))) return(tags$span("\u2014"))
        v   <- round(max(d$p_nulls, na.rm = TRUE), 4)
        col <- if (v > .30) "#9d174d" else if (v > .15) "#854d0e" else "#166534"
        tags$span(style = paste0("color:", col, ";"), v)
      }, error = function(e) tags$span("\u2014"))
    })
    output$vb_fst_ena <- renderUI({
      tryCatch({
        r <- fst_global_r()
        v <- round(r$global_ena, 4)
        col <- if (!is.na(v) && v > .15) "#9d174d" else
               if (!is.na(v) && v > .05) "#854d0e" else "#166534"
        tags$span(style = paste0("color:", col, ";"),
                  if (is.na(v)) "\u2014" else v)
      }, error = function(e) tags$span("\u2014"))
    })

    # ── Tab 1 DT ───────────────────────────────────────────────────────────────
    output$dt_t1 <- DT::renderDT({
      d <- t1_data_r()
      shiny::validate(shiny::need(nrow(d) > 0,
        "No data. Select parameters and click Compute."))
      disp        <- d
      names(disp) <- c("Locus names", "Farm", "p_nulls", "N",
                        "N_exp_blanks", "p_nulls\u00d7N")
      DT::datatable(disp, rownames = FALSE,
        options = list(pageLength = 20, scrollX = TRUE, dom = "lftip",
          columnDefs = list(list(className = "dt-right", targets = 2:5))),
        class = "compact hover stripe") |>
        DT::formatRound("p_nulls",        5) |>
        DT::formatRound("N_exp_blanks",   9) |>
        DT::formatRound("p_nulls\u00d7N", 5) |>
        DT::formatStyle("p_nulls",
          backgroundColor = DT::styleInterval(c(0.05, 0.10, 0.20, 0.30),
            c("#f0fdf4","#dcfce7","#fefce8","#fff7ed","#fef2f2"))) |>
        DT::formatStyle("Locus names", fontWeight = "600", color = "#0f172a") |>
        DT::formatStyle("Farm", color = "#475569")
    }, server = TRUE)

    # ── Tab 2 DT ───────────────────────────────────────────────────────────────
    output$dt_t2 <- DT::renderDT({
      d <- t2_data_r()
      shiny::validate(shiny::need(nrow(d) > 0,
        "No data. Select parameters and click Compute."))
      disp        <- d
      names(disp) <- c("Locus names", "Av(N_exp_blanks)", "Av(p_nulls)",
                        "N_tot", "N_blanks", "f(expBlanks)", "p_nulls")
      DT::datatable(disp, rownames = FALSE,
        options = list(pageLength = 20, scrollX = TRUE, dom = "lftip",
          columnDefs = list(list(className = "dt-right", targets = 1:6))),
        class = "compact hover stripe") |>
        DT::formatRound("Av(N_exp_blanks)", 9) |>
        DT::formatRound("Av(p_nulls)",      9) |>
        DT::formatRound("f(expBlanks)",     9) |>
        DT::formatRound("p_nulls",          9) |>
        DT::formatStyle("p_nulls",
          backgroundColor = DT::styleInterval(c(0.05, 0.10, 0.20),
            c("#f0fdf4","#dcfce7","#fefce8","#fef2f2"))) |>
        DT::formatStyle("Locus names", fontWeight = "600", color = "#0f172a")
    }, server = TRUE)

    # ── Tab 3 DT ───────────────────────────────────────────────────────────────
    output$dt_fst_global <- DT::renderDT({
      r <- fst_global_r(); d <- r$per_locus
      shiny::validate(shiny::need(nrow(d) > 0, "No data. Click Compute."))
      summary_row <- data.frame(
        Locus          = paste0("[Multilocus  Raw=",
                                round(r$global_raw, 6),
                                "  |  ENA=",
                                round(r$global_ena, 6), "]"),
        FST_raw        = r$global_raw,
        FST_ENA        = r$global_ena,
        Delta_FST      = r$global_ena - r$global_raw,
        N_pops_eff_raw = NA_integer_,
        N_pops_eff_ENA = NA_integer_,
        stringsAsFactors = FALSE
      )
      disp <- rbind(summary_row, d)
      names(disp) <- c("Locus", "Raw FST", "FST-ENA",
                        "\u0394FST (ENA\u2212raw)",
                        "N eff. pops (raw)", "N eff. pops (ENA)")
      DT::datatable(disp, rownames = FALSE,
        options = list(pageLength = 25, scrollX = TRUE, dom = "lftip",
          columnDefs = list(list(className = "dt-right", targets = 1:5))),
        class = "compact hover stripe") |>
        DT::formatRound("Raw FST",               6) |>
        DT::formatRound("FST-ENA",               6) |>
        DT::formatRound("\u0394FST (ENA\u2212raw)", 6) |>
        DT::formatStyle("FST-ENA",
          backgroundColor = DT::styleInterval(c(0.05, 0.15, 0.25),
            c("#f0fdf4","#dcfce7","#fefce8","#fef2f2"))) |>
        DT::formatStyle("Locus", fontWeight = "600", color = "#0f172a")
    }, server = TRUE)

    # ── Pairwise matrix renderer (shared by tabs 4 & 5) ───────────────────────
    render_matrix_html <- function(mat, fmt = 6,
                                   thr   = c(0.05, 0.15, 0.25),
                                   clrs  = c("#f0fdf4","#dcfce7","#fefce8","#fef2f2")) {
      pops <- rownames(mat); n <- length(pops)
      cell <- function(i, j) {
        if (i == j) return('<td class="diag">\u2014</td>')
        if (i < j)  return('<td class="upper">\u00b7</td>')
        v <- mat[i, j]
        if (is.na(v)) return('<td style="color:#94a3b8;">NA</td>')
        bg <- clrs[findInterval(v, thr) + 1L]
        sprintf('<td style="background:%s;">%s</td>', bg, round(v, fmt))
      }
      thead <- paste0('<tr><th></th>',
        paste(sprintf('<th>%s</th>', pops[-n]), collapse = ""), '</tr>')
      tbody <- paste(sapply(seq_len(n), function(i) {
        if (i == 1L) return("")
        paste0('<tr><td class="pop-label">', pops[i], '</td>',
               paste(sapply(seq_len(n), function(j) cell(i, j)), collapse = ""),
               '</tr>')
      }), collapse = "")
      HTML(sprintf(
        '<div class="na-matrix-wrap"><table class="na-matrix">
         <thead>%s</thead><tbody>%s</tbody></table></div>',
        thead, tbody))
    }

    # ── Tab 4 pairwise FST ─────────────────────────────────────────────────────
    output$ui_fst_pair_matrix <- renderUI({
      r <- fst_pair_r(); typ <- input$fst_pair_type
      shiny::validate(shiny::need(!is.null(r$matrix_raw), "Click Compute."))
      if (identical(typ, "both")) {
        tags$div(
          tags$p(tags$strong("Raw FST")),
          render_matrix_html(r$matrix_raw),
          tags$br(),
          tags$p(tags$strong("FST-ENA")),
          render_matrix_html(r$matrix_ena)
        )
      } else if (identical(typ, "raw")) {
        render_matrix_html(r$matrix_raw)
      } else {
        render_matrix_html(r$matrix_ena)
      }
    })

    output$dt_fst_pair <- DT::renderDT({
      r <- fst_pair_r(); d <- r$long
      shiny::validate(shiny::need(nrow(d) > 0, "No data. Click Compute."))
      names(d) <- c("Pop 1","Pop 2","Raw FST","FST-ENA",
                     "\u0394FST (ENA\u2212raw)")
      DT::datatable(d, rownames = FALSE,
        options = list(pageLength = 20, scrollX = TRUE, dom = "lftip",
          columnDefs = list(list(className = "dt-right", targets = 2:4))),
        class = "compact hover stripe") |>
        DT::formatRound("Raw FST",               6) |>
        DT::formatRound("FST-ENA",               6) |>
        DT::formatRound("\u0394FST (ENA\u2212raw)", 6) |>
        DT::formatStyle("FST-ENA",
          backgroundColor = DT::styleInterval(c(0.05, 0.15, 0.25),
            c("#f0fdf4","#dcfce7","#fefce8","#fef2f2")))
    }, server = TRUE)

    # ── Tab 5 pairwise DCSE ────────────────────────────────────────────────────
    output$ui_dc_matrix <- renderUI({
      r <- dc_r(); typ <- input$dc_type
      shiny::validate(shiny::need(!is.null(r$matrix_raw), "Click Compute."))
      thr  <- c(0.1, 0.25, 0.4)
      clrs <- c("#eff6ff","#dbeafe","#fef9c3","#fef2f2")
      if (identical(typ, "both")) {
        tags$div(
          tags$p(tags$strong("Raw DCSE")),
          render_matrix_html(r$matrix_raw, thr = thr, clrs = clrs),
          tags$br(),
          tags$p(tags$strong("DCSE-INA")),
          render_matrix_html(r$matrix_ina, thr = thr, clrs = clrs)
        )
      } else if (identical(typ, "raw")) {
        render_matrix_html(r$matrix_raw, thr = thr, clrs = clrs)
      } else {
        render_matrix_html(r$matrix_ina, thr = thr, clrs = clrs)
      }
    })

    output$dt_dc <- DT::renderDT({
      r <- dc_r(); d <- r$long
      shiny::validate(shiny::need(nrow(d) > 0, "No data. Click Compute."))
      names(d) <- c("Pop 1","Pop 2","Raw DCSE","DCSE-INA",
                     "\u0394DCSE (INA\u2212raw)")
      DT::datatable(d, rownames = FALSE,
        options = list(pageLength = 20, scrollX = TRUE, dom = "lftip",
          columnDefs = list(list(className = "dt-right", targets = 2:4))),
        class = "compact hover stripe") |>
        DT::formatRound("Raw DCSE",                6) |>
        DT::formatRound("DCSE-INA",                6) |>
        DT::formatRound("\u0394DCSE (INA\u2212raw)", 6)
    }, server = TRUE)

    # ── Tab 6 per-locus FST ────────────────────────────────────────────────────
    output$dt_fst_locus <- DT::renderDT({
      d <- fst_locus_r()
      shiny::validate(shiny::need(nrow(d) > 0, "No data. Click Compute."))
      names(d) <- c("Locus","Pop 1","Pop 2","Raw FST","FST-ENA",
                     "\u0394FST","N_i raw","N_j raw","N_i ENA","N_j ENA")
      DT::datatable(d, rownames = FALSE,
        options = list(pageLength = 25, scrollX = TRUE, dom = "lftip",
          columnDefs = list(list(className = "dt-right", targets = 3:9))),
        class = "compact hover stripe") |>
        DT::formatRound("Raw FST", 6) |>
        DT::formatRound("FST-ENA", 6) |>
        DT::formatRound("\u0394FST",   6) |>
        DT::formatStyle("FST-ENA",
          backgroundColor = DT::styleInterval(c(0.05, 0.15, 0.25),
            c("#f0fdf4","#dcfce7","#fefce8","#fef2f2"))) |>
        DT::formatStyle("Locus", fontWeight = "600", color = "#0f172a")
    }, server = TRUE)

    # ── Download handlers — helper ─────────────────────────────────────────────
    make_dl <- function(data_fn, base_name, col_nms = NULL) {
      mk <- function(ext, write_fn)
        downloadHandler(
          filename = function() paste0(base_name, "_", Sys.Date(), ".", ext),
          content  = function(file) {
            d <- data_fn()
            if (is.null(d) || nrow(d) == 0L) return(invisible(NULL))
            if (!is.null(col_nms)) names(d) <- col_nms
            write_fn(d, file)
          }
        )
      list(csv = mk("csv", function(d, f) write.csv(d, f, row.names = FALSE)),
           txt = mk("txt", function(d, f) write.table(d, f, sep = "\t",
                                                       row.names = FALSE, quote = FALSE)))
    }

    # Tab 1
    dl1 <- make_dl(function() t1_data_r(), "null_allele_per_pop_locus",
      c("Locus_names","Farm","p_nulls","N","N_exp_blanks","p_nulls_x_N"))
    output$dl_t1_csv <- dl1$csv; output$dl_t1_txt <- dl1$txt

    # Tab 2
    dl2 <- make_dl(function() t2_data_r(), "null_allele_global",
      c("Locus_names","Av_N_exp_blanks","Av_p_nulls",
        "N_tot","N_blanks","f_expBlanks","p_nulls"))
    output$dl_t2_csv <- dl2$csv; output$dl_t2_txt <- dl2$txt

    # Tab 3
    dl3 <- make_dl(function() fst_global_r()$per_locus, "fst_global_ENA",
      c("Locus","FST_raw","FST_ENA","Delta_FST",
        "N_pops_eff_raw","N_pops_eff_ENA"))
    output$dl_fst_global_csv <- dl3$csv; output$dl_fst_global_txt <- dl3$txt

    # Tab 4 — matrix (ENA)
    output$dl_fst_pair_csv <- downloadHandler(
      filename = function() paste0("fst_pairwise_ENA_", Sys.Date(), ".csv"),
      content  = function(file) {
        r <- fst_pair_r(); mat <- round(r$matrix_ena, 6)
        d <- cbind(Population = rownames(mat), as.data.frame(mat))
        write.csv(d, file, row.names = FALSE)
      }
    )
    output$dl_fst_pair_txt <- downloadHandler(
      filename = function() paste0("fst_pairwise_ENA_", Sys.Date(), ".txt"),
      content  = function(file) {
        r <- fst_pair_r(); mat <- round(r$matrix_ena, 6)
        d <- cbind(Population = rownames(mat), as.data.frame(mat))
        write.table(d, file, sep = "\t", row.names = FALSE, quote = FALSE)
      }
    )
    dl4l <- make_dl(function() fst_pair_r()$long, "fst_pairwise_long_ENA",
      c("Pop1","Pop2","FST_raw","FST_ENA","Delta_FST"))
    output$dl_fst_pair_long_csv <- dl4l$csv; output$dl_fst_pair_long_txt <- dl4l$txt

    # Tab 5 — matrix (INA)
    output$dl_dc_csv <- downloadHandler(
      filename = function() paste0("dcse_pairwise_INA_", Sys.Date(), ".csv"),
      content  = function(file) {
        r <- dc_r(); mat <- round(r$matrix_ina, 6)
        d <- cbind(Population = rownames(mat), as.data.frame(mat))
        write.csv(d, file, row.names = FALSE)
      }
    )
    output$dl_dc_txt <- downloadHandler(
      filename = function() paste0("dcse_pairwise_INA_", Sys.Date(), ".txt"),
      content  = function(file) {
        r <- dc_r(); mat <- round(r$matrix_ina, 6)
        d <- cbind(Population = rownames(mat), as.data.frame(mat))
        write.table(d, file, sep = "\t", row.names = FALSE, quote = FALSE)
      }
    )
    dl5l <- make_dl(function() dc_r()$long, "dcse_pairwise_long_INA",
      c("Pop1","Pop2","DCSE_raw","DCSE_INA","Delta_DCSE"))
    output$dl_dc_long_csv <- dl5l$csv; output$dl_dc_long_txt <- dl5l$txt

    # Tab 6
    dl6 <- make_dl(function() fst_locus_r(), "fst_per_locus_pair_ENA",
      c("Locus","Pop1","Pop2","FST_raw","FST_ENA","Delta_FST",
        "N_i_raw","N_j_raw","N_i_ENA","N_j_ENA"))
    output$dl_fst_locus_csv <- dl6$csv; output$dl_fst_locus_txt <- dl6$txt

  }) # end moduleServer
}
