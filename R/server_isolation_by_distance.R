# server_isolation_by_distance.R
# Isolation by Distance (Rousset 1997) + Mantel test.
#
# This module is a CONTINUATION of the "Null alleles" module: it reuses the
# pairwise FST/FST-ENA/DCSE/DCSE-INA (+ bootstrap CI) already computed there,
# shared through `rv$null_alleles_results` — nothing is recomputed here
# except geographic distance (D_geo), which the Null Alleles module doesn't
# compute.
#
# Geographic distance (D_geo) is the Vincenty ellipsoidal geodesic distance
# (WGS84), in metres.
#
# References:
#   Rousset (1997)  — Isolation by distance regression: FR = FST/(1-FST)
#                      regressed on ln(geographic distance) (2D habitat) or
#                      on raw distance (1D habitat); Nb = 1/slope,
#                      Nem = Nb/(2*pi).
#   Mantel (1967)   — permutation test by joint row/column relabelling of
#                      one distance matrix. Two one-sided p-value formulas
#                      are offered: (b+1)/(m+1) (bias-corrected proportion,
#                      Davison & Hinkley 1997) and the plain proportion b/m.

server_isolation_by_distance <- function(id, rv) {
  moduleServer(id, function(input, output, session) {

    `%||%` <- function(a, b) if (!is.null(a) && !(length(a) == 1 && is.na(a))) a else b

    # ── DB plumbing (same conventions as other modules) ──────────────────────
    db_tick    <- reactive({ rv$db_tick })
    con_r      <- reactive({ shiny::req(rv$con); rv$con })
    tbl_meta_r <- reactive({ rv$tbl_meta %||% "meta" })

    db_ready <- reactive({
      db_tick(); con <- con_r()
      shiny::req(isTRUE(rv$db_ready))
      shiny::validate(shiny::need(DBI::dbExistsTable(con, tbl_meta_r()),
                                   "DuckDB meta table missing."))
      TRUE
    })

    # ── Null Alleles module results (shared via rv — nothing recomputed) ────
    na_results_r <- reactive({
      r <- rv$null_alleles_results
      shiny::validate(shiny::need(!is.null(r),
        paste0("No results yet. Go to the \"Null alleles\" module, choose your ",
               "per-locus coding, and click \"Compute + Bootstrap + Export\" first — ",
               "this module reuses those results directly (nothing is recomputed here).")))
      r
    })

    output$ui_run_status <- renderUI({
      if (isTRUE(identical(input$ibd_source, "external"))) {
        fname <- input$ibd_ext_file$name
        n_rows <- tryCatch(nrow(full_pair_table_external_r()), error = function(e) NA_integer_)
        return(tags$div(class = "na-info", icon("file-import"), " ",
          if (is.null(fname)) "No file uploaded yet \u2014 choose a pairwise file above."
          else tagList("Using uploaded file: ", tags$strong(fname),
                       sprintf(" (%s rows) \u2014 the Null Alleles module is not needed for this run.",
                               if (is.na(n_rows)) "?" else n_rows))))
      }
      r <- tryCatch(na_results_r(), error = function(e) NULL)
      if (is.null(r)) return(NULL)
      tags$div(class = "na-info",
        icon("check-circle"), " ",
        sprintf("Using Null Alleles results: %d loci \u00b7 %d populations \u00b7 %d pairwise combinations.",
                length(r$markers), length(r$pops), nrow(r$fst_pair$long)))
    })

    # ══════════════════════════════════════════════════════════════════════
    #  OUTPUT FILE NAMING — root auto-proposed from the imported data file's
    #  name (editable, never silently overwritten once the user has typed
    #  their own), + optional suffix.
    # ══════════════════════════════════════════════════════════════════════
    last_auto_root_ibd <- reactiveVal("")
    observeEvent(rv$dataset_filename, {
      fn <- rv$dataset_filename
      if (is.null(fn) || !nzchar(trimws(fn))) return(invisible(NULL))
      root_guess <- tools::file_path_sans_ext(basename(trimws(fn)))
      cur <- trimws(input$ibd_out_root %||% "")
      if (!nzchar(cur) || identical(cur, last_auto_root_ibd())) {
        updateTextInput(session, "ibd_out_root", value = root_guess, placeholder = root_guess)
        last_auto_root_ibd(root_guess)
      } else {
        updateTextInput(session, "ibd_out_root", placeholder = root_guess)
      }
    }, ignoreInit = FALSE, ignoreNULL = TRUE)

    ibd_out_root_r   <- reactive({
      r <- trimws(input$ibd_out_root %||% "")
      if (nzchar(r)) r else if (nzchar(last_auto_root_ibd())) last_auto_root_ibd() else "SPG_"
    })
    ibd_out_suffix_r <- reactive({ trimws(input$ibd_out_suffix %||% "") })
    ibd_out_filename <- function(desc) {
      suf <- ibd_out_suffix_r()
      paste0(ibd_out_root_r(), "-", desc, if (nzchar(suf)) paste0("-", suf) else "", ".txt")
    }

    # ── Population GPS centroids (needed for D_geo; IBD-specific) ───────────
    coords_r <- reactive({
      db_ready()
      con  <- con_r()
      cols <- tryCatch(DBI::dbGetQuery(con, sprintf(
        "SELECT column_name FROM information_schema.columns WHERE table_name = '%s'",
        tbl_meta_r()))$column_name, error = function(e) character(0))
      if (!all(c("Latitude", "Longitude") %in% cols)) return(NULL)
      df <- tryCatch(DBI::dbGetQuery(con, sprintf(
        "SELECT Population,
                AVG(CAST(Latitude  AS DOUBLE)) AS Latitude,
                AVG(CAST(Longitude AS DOUBLE)) AS Longitude
         FROM %s
         WHERE Population IS NOT NULL
           AND Latitude IS NOT NULL AND Longitude IS NOT NULL
         GROUP BY Population ORDER BY Population",
        sql_ident(con, tbl_meta_r()))), error = function(e) NULL)
      if (is.null(df) || nrow(df) < 2L) return(NULL)
      df
    })

    # ══════════════════════════════════════════════════════════════════════
    #  GEOGRAPHIC DISTANCE — Vincenty ellipsoidal geodesic (WGS84), in metres
    # ══════════════════════════════════════════════════════════════════════
    .vincenty_m <- function(lat1, lon1, lat2, lon2) {
      a <- 6378137.0; f <- 1/298.257223563; b <- (1 - f) * a
      L  <- (lon2 - lon1) * pi / 180
      U1 <- atan((1 - f) * tan(lat1 * pi / 180))
      U2 <- atan((1 - f) * tan(lat2 * pi / 180))
      sinU1 <- sin(U1); cosU1 <- cos(U1); sinU2 <- sin(U2); cosU2 <- cos(U2)
      lam <- L
      for (i in seq_len(200L)) {
        sinLam <- sin(lam); cosLam <- cos(lam)
        sinSigma <- sqrt((cosU2*sinLam)^2 + (cosU1*sinU2 - sinU1*cosU2*cosLam)^2)
        if (sinSigma == 0) return(0)
        cosSigma <- sinU1*sinU2 + cosU1*cosU2*cosLam
        sigma <- atan2(sinSigma, cosSigma)
        sinAlpha <- cosU1*cosU2*sinLam/sinSigma
        cosSqAlpha <- 1 - sinAlpha^2
        cos2SigmaM <- if (cosSqAlpha != 0) cosSigma - 2*sinU1*sinU2/cosSqAlpha else 0
        C <- f/16*cosSqAlpha*(4 + f*(4 - 3*cosSqAlpha))
        lamPrev <- lam
        lam <- L + (1 - C)*f*sinAlpha*(sigma + C*sinSigma*(cos2SigmaM + C*cosSigma*(-1 + 2*cos2SigmaM^2)))
        if (abs(lam - lamPrev) < 1e-12) break
      }
      uSq <- cosSqAlpha*(a^2 - b^2)/b^2
      A <- 1 + uSq/16384*(4096 + uSq*(-768 + uSq*(320 - 175*uSq)))
      B <- uSq/1024*(256 + uSq*(-128 + uSq*(74 - 47*uSq)))
      deltaSigma <- B*sinSigma*(cos2SigmaM + B/4*(cosSigma*(-1 + 2*cos2SigmaM^2) -
                    B/6*cos2SigmaM*(-3 + 4*sinSigma^2)*(-3 + 4*cos2SigmaM^2)))
      b * A * (sigma - deltaSigma)
    }

    # ══════════════════════════════════════════════════════════════════════
    #  FULL PAIRWISE TABLE — sourced ENTIRELY from rv$null_alleles_results —
    #  nothing recomputed here except D_geo/ln(D_geo).
    # ══════════════════════════════════════════════════════════════════════
    .linearise <- function(x) { x <- pmin(pmax(x, 0), 0.9999); x / (1 - x) }

    full_pair_table_r <- reactive({
      if (isTRUE(identical(input$ibd_source, "external"))) {
        full_pair_table_external_r()
      } else {
        full_pair_table_internal_r()
      }
    })

    # ── EXTERNAL SOURCE: re-load a previously exported (and freely edited)
    #    pairwise file. Column names matching the Null Alleles export
    #    (FST_raw, FST_ENA, DCSE_raw, DCSE_INA, their _lo/_hi CI bounds,
    #    FR/FR_raw + CI, Dgeo_m, lnDgeo) are used directly when present;
    #    anything missing is derived when possible (FR from FST via
    #    Rousset's linearisation, lnDgeo from Dgeo_m) or left NA otherwise.
    full_pair_table_external_r <- reactive({
      shiny::req(input$ibd_ext_file)
      ext <- .mt_read_file(input$ibd_ext_file, input$ibd_ext_sep, input$ibd_ext_header)
      nm <- names(ext)
      pop1_col <- .guess_col(nm, c("^Pop1$", "^Farm1$", "^ID1$"), nm[1])
      pop2_col <- .guess_col(nm, c("^Pop2$", "^Farm2$", "^ID2$"), nm[2])
      shiny::validate(shiny::need(!identical(pop1_col, pop2_col),
        "Could not identify two distinct Pop1/Pop2 columns in the uploaded file."))

      df <- data.frame(Pop1 = trimws(as.character(ext[[pop1_col]])),
                        Pop2 = trimws(as.character(ext[[pop2_col]])),
                        stringsAsFactors = FALSE)

      num_col <- function(pats) {
        c <- .guess_col(nm, pats, NA_character_)
        if (is.na(c) || !(c %in% nm)) rep(NA_real_, nrow(ext))
        else suppressWarnings(as.numeric(ext[[c]]))
      }
      df$FST_raw     <- num_col(c("^FST_raw$"))
      df$FST_ENA     <- num_col(c("^FST_ENA$"))
      df$FST_raw_lo  <- num_col(c("^FST_raw_CI_lo_loci$", "^FST_raw_lo$"))
      df$FST_raw_hi  <- num_col(c("^FST_raw_CI_hi_loci$", "^FST_raw_hi$"))
      df$FST_ENA_lo  <- num_col(c("^FST_ENA_CI_lo_loci$", "^FST_ENA_lo$"))
      df$FST_ENA_hi  <- num_col(c("^FST_ENA_CI_hi_loci$", "^FST_ENA_hi$"))
      df$DCSE_raw    <- num_col(c("^DCSE_raw$"))
      df$DCSE_INA    <- num_col(c("^DCSE_INA$"))
      df$DCSE_raw_lo <- num_col(c("^DCSE_raw_CI_lo_loci$", "^DCSE_raw_lo$"))
      df$DCSE_raw_hi <- num_col(c("^DCSE_raw_CI_hi_loci$", "^DCSE_raw_hi$"))
      df$DCSE_INA_lo <- num_col(c("^DCSE_INA_CI_lo_loci$", "^DCSE_INA_lo$"))
      df$DCSE_INA_hi <- num_col(c("^DCSE_INA_CI_hi_loci$", "^DCSE_INA_hi$"))

      fr_col <- function(pats, fallback_from) {
        c <- .guess_col(nm, pats, NA_character_)
        if (!is.na(c) && c %in% nm) suppressWarnings(as.numeric(ext[[c]]))
        else .linearise(fallback_from)
      }
      df$FR        <- fr_col(c("^FR$"),        df$FST_ENA)
      df$FR_lo     <- fr_col(c("^FR_lo$"),     df$FST_ENA_lo)
      df$FR_hi     <- fr_col(c("^FR_hi$"),     df$FST_ENA_hi)
      df$FR_raw    <- fr_col(c("^FR_raw$"),    df$FST_raw)
      df$FR_raw_lo <- fr_col(c("^FR_raw_lo$"), df$FST_raw_lo)
      df$FR_raw_hi <- fr_col(c("^FR_raw_hi$"), df$FST_raw_hi)

      df$Dgeo_m <- num_col(c("^Dgeo_m$", "^D_geo$", "^Distance$"))
      lnd <- num_col(c("^lnDgeo$", "^ln\\(D_geo\\)$"))
      df$lnDgeo <- ifelse(is.finite(lnd), lnd,
                           ifelse(is.finite(df$Dgeo_m) & df$Dgeo_m > 0, log(df$Dgeo_m), NA_real_))
      df
    })

    # ── INTERNAL SOURCE (default): built entirely from the Null Alleles
    #    module's results shared via rv$null_alleles_results.
    full_pair_table_internal_r <- reactive({
      na <- na_results_r()
      fst_long <- na$fst_pair$long
      dc_long  <- na$dc_pair$long
      bf       <- na$boot_pair_fst
      bd       <- na$boot_pair_dc

      df <- merge(fst_long, dc_long[, c("Pop1","Pop2","DCSE_raw","DCSE_INA")],
                  by = c("Pop1","Pop2"), sort = FALSE)

      if (!is.null(bf) && nrow(bf) > 0L) {
        bf2 <- bf[, c("Pop1","Pop2","FST_raw_CI_lo_loci","FST_raw_CI_hi_loci",
                      "FST_ENA_CI_lo_loci","FST_ENA_CI_hi_loci")]
        names(bf2)[3:6] <- c("FST_raw_lo","FST_raw_hi","FST_ENA_lo","FST_ENA_hi")
        df <- merge(df, bf2, by = c("Pop1","Pop2"), sort = FALSE)
      } else {
        df$FST_raw_lo <- NA_real_; df$FST_raw_hi <- NA_real_
        df$FST_ENA_lo <- NA_real_; df$FST_ENA_hi <- NA_real_
      }
      if (!is.null(bd) && nrow(bd) > 0L) {
        bd2 <- bd[, c("Pop1","Pop2","DCSE_raw_CI_lo_loci","DCSE_raw_CI_hi_loci",
                      "DCSE_INA_CI_lo_loci","DCSE_INA_CI_hi_loci")]
        names(bd2)[3:6] <- c("DCSE_raw_lo","DCSE_raw_hi","DCSE_INA_lo","DCSE_INA_hi")
        df <- merge(df, bd2, by = c("Pop1","Pop2"), sort = FALSE)
      } else {
        df$DCSE_raw_lo <- NA_real_; df$DCSE_raw_hi <- NA_real_
        df$DCSE_INA_lo <- NA_real_; df$DCSE_INA_hi <- NA_real_
      }

      df$FR        <- .linearise(df$FST_ENA)
      df$FR_lo     <- .linearise(df$FST_ENA_lo)
      df$FR_hi     <- .linearise(df$FST_ENA_hi)
      df$FR_raw    <- .linearise(df$FST_raw)
      df$FR_raw_lo <- .linearise(df$FST_raw_lo)
      df$FR_raw_hi <- .linearise(df$FST_raw_hi)

      use_external_dgeo <- isTRUE(identical(input$ibd_dgeo_source, "external"))

      if (use_external_dgeo) {
        shiny::req(input$ibd_dgeo_file)
        ext <- .mt_read_file(input$ibd_dgeo_file, input$ibd_dgeo_sep, input$ibd_dgeo_header)
        shiny::validate(shiny::need(ncol(ext) >= 3L,
          "External distance file must have at least 3 columns: Pop1, Pop2, Distance."))
        nm <- names(ext)
        pop1_col <- .guess_col(nm, c("^Pop1$", "^Farm1$", "^ID1$"), nm[1])
        pop2_col <- .guess_col(nm, c("^Pop2$", "^Farm2$", "^ID2$"), nm[2])
        dist_col <- .guess_col(nm, c("^Distance$", "^Dgeo", "^Dist$"), nm[3])

        key <- function(a, b) { a <- trimws(as.character(a)); b <- trimws(as.character(b))
                                 ifelse(a <= b, paste(a, b, sep = "__"), paste(b, a, sep = "__")) }

        ext2 <- data.frame(
          .key     = key(ext[[pop1_col]], ext[[pop2_col]]),
          Dgeo_ext = suppressWarnings(as.numeric(ext[[dist_col]])),
          stringsAsFactors = FALSE
        )
        ext2 <- ext2[!duplicated(ext2$.key), , drop = FALSE]

        df$.key <- key(df$Pop1, df$Pop2)
        df <- merge(df, ext2, by = ".key", sort = FALSE)
        df$.key <- NULL
        df$Dgeo_m <- df$Dgeo_ext
        df$Dgeo_ext <- NULL
        df$lnDgeo <- ifelse(is.finite(df$Dgeo_m) & df$Dgeo_m > 0, log(df$Dgeo_m), NA_real_)
      } else {
        coords <- tryCatch(coords_r(), error = function(e) NULL)
        if (!is.null(coords)) {
          get_d <- function(p1, p2) {
            c1 <- coords[coords$Population == p1, ]; c2 <- coords[coords$Population == p2, ]
            if (nrow(c1) >= 1L && nrow(c2) >= 1L)
              .vincenty_m(c1$Latitude[1L], c1$Longitude[1L], c2$Latitude[1L], c2$Longitude[1L])
            else NA_real_
          }
          df$Dgeo_m <- mapply(get_d, df$Pop1, df$Pop2)
          df$lnDgeo <- ifelse(df$Dgeo_m > 0, log(df$Dgeo_m), NA_real_)
        } else {
          df$Dgeo_m <- NA_real_; df$lnDgeo <- NA_real_
        }
      }

      df
    })

    # ══════════════════════════════════════════════════════════════════════
    #  TAB 1 — Isolation by Distance (Rousset 1997)
    # ══════════════════════════════════════════════════════════════════════

    .fit_line <- function(y, x) {
      ok <- is.finite(y) & is.finite(x)
      if (sum(ok) < 3L) return(list(slope = NA_real_, intercept = NA_real_, r2 = NA_real_))
      m <- lm(y[ok] ~ x[ok])
      list(slope = unname(coef(m)[2L]), intercept = unname(coef(m)[1L]), r2 = summary(m)$r.squared)
    }

    .ibd_numeric_cols <- reactive({
      df <- tryCatch(full_pair_table_r(), error = function(e) NULL)
      if (is.null(df)) return(character(0))
      names(df)[sapply(df, is.numeric)]
    })

    output$ibd_col_geo_ui <- renderUI({
      cols <- .ibd_numeric_cols()
      selectInput(session$ns("ibd_col_geo"), "geographic distance", choices = cols,
                  selected = .guess_col(cols, c("^lnDgeo$", "^Dgeo_m$", "Dgeo"), if (length(cols)) cols[1] else NULL))
    })
    output$ibd_col_avg_ui <- renderUI({
      cols <- .ibd_numeric_cols()
      selectInput(session$ns("ibd_col_avg"), "average", choices = cols,
                  selected = .guess_col(cols, c("^FR$", "^FR_raw$"), if (length(cols)) cols[1] else NULL))
    })
    output$ibd_col_lo_ui <- renderUI({
      cols <- .ibd_numeric_cols()
      selectInput(session$ns("ibd_col_lo"), "lower limit", choices = cols,
                  selected = .guess_col(cols, c("^FR_lo$", "^FR_raw_lo$"), if (length(cols)) cols[1] else NULL))
    })
    output$ibd_col_hi_ui <- renderUI({
      cols <- .ibd_numeric_cols()
      selectInput(session$ns("ibd_col_hi"), "higher limit", choices = cols,
                  selected = .guess_col(cols, c("^FR_hi$", "^FR_raw_hi$"), if (length(cols)) cols[1] else NULL))
    })

    ibd_results_r <- eventReactive(input$run_ibd, {
      df <- full_pair_table_r()
      shiny::req(input$ibd_col_geo, input$ibd_col_avg, input$ibd_col_lo, input$ibd_col_hi)
      shiny::validate(
        shiny::need(all(c(input$ibd_col_geo, input$ibd_col_avg, input$ibd_col_lo, input$ibd_col_hi) %in% names(df)),
                    "Selected columns not found."),
        shiny::need(any(is.finite(df[[input$ibd_col_geo]])),
          "No usable values in the selected geographic distance column.")
      )

      x <- suppressWarnings(as.numeric(df[[input$ibd_col_geo]]))
      x_label <- input$ibd_col_geo

      y_avg <- suppressWarnings(as.numeric(df[[input$ibd_col_avg]])); y_label <- input$ibd_col_avg
      y_lo  <- suppressWarnings(as.numeric(df[[input$ibd_col_lo]]))
      y_hi  <- suppressWarnings(as.numeric(df[[input$ibd_col_hi]]))

      reg_avg <- .fit_line(y_avg, x)
      reg_lo  <- .fit_line(y_lo,  x)
      reg_hi  <- .fit_line(y_hi,  x)

      nbnem <- function(reg) {
        b <- round(reg$slope, 4)
        if (is.na(b) || b == 0) return(c(b = b, Nb = NA_real_, Nem = NA_real_))
        Nb <- 1 / b
        c(b = b, Nb = Nb, Nem = Nb / (2 * pi))
      }
      summ <- rbind(
        c(Line = "Average",  nbnem(reg_avg)),
        c(Line = "95%CI-i",  nbnem(reg_lo)),
        c(Line = "95%CI-s",  nbnem(reg_hi))
      )

      list(df = df, x = x, y_avg = y_avg, y_lo = y_lo, y_hi = y_hi,
           x_label = x_label, y_label = y_label,
           col_geo = input$ibd_col_geo, col_avg = input$ibd_col_avg,
           col_lo = input$ibd_col_lo, col_hi = input$ibd_col_hi,
           reg_avg = reg_avg, reg_lo = reg_lo, reg_hi = reg_hi,
           summary = summ)
    })

    output$dt_ibd_table <- DT::renderDT({
      r <- ibd_results_r()
      d <- r$df
      out <- data.frame(
        Farm1          = d$Pop1,
        Farm2          = d$Pop2,
        D_geo          = round(d$Dgeo_m, 4),
        `FST-FreeNA`   = round(d$FST_ENA, 6),
        `FST-FreeNA-i` = round(d$FST_ENA_lo, 6),
        `FST-FreeNA-s` = round(d$FST_ENA_hi, 6),
        `ln(D_geo)`    = round(d$lnDgeo, 6),
        F_R            = round(d$FR, 6),
        `F_R-i`        = round(d$FR_lo, 6),
        `F_R-s`        = round(d$FR_hi, 6),
        `D_CSE-INA`    = round(d$DCSE_INA, 6),
        D_CSE          = round(d$DCSE_raw, 6),
        check.names = FALSE, stringsAsFactors = FALSE
      )
      DT::datatable(out, rownames = FALSE,
        options = list(scrollX = TRUE, pageLength = 28, dom = "lrtip"),
        class = "compact stripe hover") |>
        DT::formatRound(c("D_geo","FST-FreeNA","FST-FreeNA-i","FST-FreeNA-s",
                           "ln(D_geo)","F_R","F_R-i","F_R-s","D_CSE-INA","D_CSE"), 6)
    })
    output$dl_ibd_txt <- downloadHandler(
      filename = function() ibd_out_filename("regression"),
      content  = function(file) {
        r <- ibd_results_r()
        s <- as.data.frame(r$summary, stringsAsFactors = FALSE)
        hdr <- c(
          "Isolation by Distance \u2014 Rousset (1997) regression",
          sprintf("Geographic distance column: %s", r$col_geo),
          sprintf("Genetic distance columns \u2014 average: %s, lower limit: %s, higher limit: %s",
                   r$col_avg, r$col_lo, r$col_hi),
          sprintf("Data source: %s", if (isTRUE(identical(input$ibd_source, "external"))) "external re-loaded pairwise file" else "Null Alleles module (this session)"),
          sprintf("Slope (b) / Nb / Nem \u2014 average: b=%.6f Nb=%.6f Nem=%.6f",
                   r$reg_avg$slope, 1/r$reg_avg$slope, (1/r$reg_avg$slope)/(2*pi)),
          ""
        )
        con <- file(file, open = "w", encoding = "UTF-8"); on.exit(close(con))
        writeLines(hdr, con = con, useBytes = TRUE)
        writeLines("Regression summary (slope / b / Nb / Nem for average and CI bounds):", con = con)
        write.table(s, file = con, sep = "\t", row.names = FALSE, quote = FALSE, append = TRUE)
        writeLines("", con = con)
        writeLines("Full pairwise table:", con = con)
        write.table(r$df, file = con, sep = "\t", row.names = FALSE, quote = FALSE, append = TRUE)
      }
    )

    output$dt_ibd_reg <- DT::renderDT({
      r <- ibd_results_r()
      s <- as.data.frame(r$summary, stringsAsFactors = FALSE)
      s$b   <- round(as.numeric(s$b), 4)
      s$Nb  <- round(as.numeric(s$Nb), 4)
      s$Nem <- round(as.numeric(s$Nem), 4)
      names(s) <- c("slope", "b", "Nb", "Nem")
      DT::datatable(s, rownames = FALSE,
        options = list(dom = "t", pageLength = 3, ordering = FALSE),
        class = "compact stripe")
    })

    output$ui_ibd_interpretation <- renderUI({
      r <- ibd_results_r()
      slopes <- c(r$reg_avg$slope, r$reg_lo$slope, r$reg_hi$slope)
      if (any(is.na(slopes))) {
        return(tags$div(class = "spg-method-note", style = "border-left-color:#999;",
          "Could not fit all three regression lines (insufficient valid pairs)."))
      }
      all_pos <- all(slopes > 0)
      lo_neg  <- r$reg_lo$slope < 0 && r$reg_avg$slope > 0 && r$reg_hi$slope > 0
      if (all_pos) {
        tags$div(style = "padding:10px; background:#dcfce7; border:1px solid #86efac; border-radius:6px; color:#166534; font-size:13px;",
          icon("check-circle"), tags$strong(" All three slopes are positive: "),
          "this supports isolation by distance.")
      } else if (lo_neg) {
        tags$div(style = "padding:10px; background:#fffbeb; border:1px solid #fcd34d; border-radius:6px; color:#92400e; font-size:13px;",
          icon("exclamation-triangle"), tags$strong(" Lower-bound slope is negative: "),
          "this may indicate low power of the per-pair bootstrap rather than a true absence of IBD. ",
          "Consider running the Mantel test (next tab), ideally with DCSE, to confirm.")
      } else {
        tags$div(style = "padding:10px; background:#fef2f2; border:1px solid #fca5a5; border-radius:6px; color:#991b1b; font-size:13px;",
          icon("times-circle"), tags$strong(" No consistent positive trend: "),
          "no clear evidence of isolation by distance with this dataset/model.")
      }
    })

    # ══════════════════════════════════════════════════════════════════════
    #  TAB 2 — Mantel test (joint row/column permutation; rectangular-safe)
    #  Both p-value formulas — (b+1)/(m+1) and b/m — live in this single tab
    #  now, selected via a radio button, so results/formula are always shown
    #  together and never accidentally mismatched.
    # ══════════════════════════════════════════════════════════════════════

    .mt_build_square <- function(df, id1, id2, value_col, all_labels) {
      n <- length(all_labels)
      m <- matrix(NA_real_, n, n, dimnames = list(all_labels, all_labels))
      for (k in seq_len(nrow(df))) {
        i <- trimws(as.character(df[[id1]][k])); j <- trimws(as.character(df[[id2]][k])); v <- df[[value_col]][k]
        if (i %in% all_labels && j %in% all_labels && is.finite(v)) { m[i, j] <- v; m[j, i] <- v }
      }
      m
    }

    # Generic Mantel permutation test (pure-R fallback engine): joint
    # row/column relabelling of one matrix (valid on rectangular/incomplete
    # matrices too), Pearson r / Spearman rho / Rousset regression slope as
    # the statistic. p_formula selects (b+1)/(m+1) ["plus1"] or the plain
    # proportion b/m ["plain"].
    .mt_mantel_matrix <- function(mat1, mat2, n_perm = 9999L, stat = "r", p_formula = "plus1") {
      common <- intersect(rownames(mat1), rownames(mat2))
      if (length(common) < 3L)
        return(list(stat_obs = NA_real_, p_pos = NA_real_, p_neg = NA_real_, n_pairs = 0L,
                    slope = NA_real_, intercept = NA_real_, r2 = NA_real_,
                    x = numeric(0), y = numeric(0), pop1 = character(0), pop2 = character(0),
                    common = common, perm_stats = numeric(0)))
      m1 <- mat1[common, common, drop = FALSE]; m2 <- mat2[common, common, drop = FALSE]
      n  <- length(common)
      pair_idx  <- which(lower.tri(matrix(TRUE, n, n)), arr.ind = TRUE)
      lower_idx <- which(lower.tri(matrix(TRUE, n, n)))
      pop1_all  <- common[pair_idx[, "row"]]; pop2_all <- common[pair_idx[, "col"]]
      x_all <- m1[lower_idx]; y_all <- m2[lower_idx]
      stat_fn <- function(xx, yy) {
        ok <- is.finite(xx) & is.finite(yy)
        if (sum(ok) < 3L) return(NA_real_)
        if (stat == "b") unname(coef(lm(yy[ok] ~ xx[ok]))[2L])
        else if (stat == "spearman") suppressWarnings(cor(xx[ok], yy[ok], method = "spearman"))
        else suppressWarnings(cor(xx[ok], yy[ok]))
      }
      ok_obs   <- is.finite(x_all) & is.finite(y_all)
      stat_obs <- stat_fn(x_all, y_all)
      perm_stats <- vapply(seq_len(n_perm), function(.b) {
        perm <- sample.int(n); m2p <- m2[perm, perm, drop = FALSE]
        stat_fn(x_all, m2p[lower_idx])
      }, numeric(1L))
      perm_fin <- perm_stats[is.finite(perm_stats)]
      m_valid  <- length(perm_fin)
      EPS <- sqrt(.Machine$double.eps)
      if (m_valid > 0L && is.finite(stat_obs)) {
        b_pos <- sum(perm_fin >= stat_obs - EPS)
        b_neg <- sum(perm_fin <= stat_obs + EPS)
        if (identical(p_formula, "plain")) {
          p_pos <- b_pos / m_valid
          p_neg <- b_neg / m_valid
        } else {
          p_pos <- (b_pos + 1) / (m_valid + 1)
          p_neg <- (b_neg + 1) / (m_valid + 1)
        }
      } else {
        p_pos <- NA_real_; p_neg <- NA_real_
      }
      lm0 <- tryCatch(lm(y_all[ok_obs] ~ x_all[ok_obs]), error = function(e) NULL)
      list(stat_obs = stat_obs, p_pos = p_pos, p_neg = p_neg, n_pairs = sum(ok_obs),
           slope = if (!is.null(lm0)) unname(coef(lm0)[2L]) else NA_real_,
           intercept = if (!is.null(lm0)) unname(coef(lm0)[1L]) else NA_real_,
           r2 = if (!is.null(lm0)) summary(lm0)$r.squared else NA_real_,
           x = x_all[ok_obs], y = y_all[ok_obs],
           pop1 = pop1_all[ok_obs], pop2 = pop2_all[ok_obs],
           common = common, perm_stats = perm_fin)
    }

    .mt_read_file <- function(fileinfo, sep, header) {
      df <- tryCatch(read.table(fileinfo$datapath, header = header, sep = sep,
                                stringsAsFactors = FALSE, check.names = FALSE,
                                fill = TRUE, quote = "\""),
                     error = function(e) NULL)
      shiny::validate(shiny::need(!is.null(df) && nrow(df) >= 3L,
        "Could not parse the file. Check separator / header settings."))
      df
    }

    mt_base_df_r <- reactive({
      if (input$mt_source == "internal") {
        df <- full_pair_table_r()
        if (isTRUE(input$mt_use_extra)) {
          shiny::req(input$mt_extra_file)
          extra <- .mt_read_file(input$mt_extra_file, input$mt_extra_sep, input$mt_extra_header)
          shiny::validate(shiny::need(ncol(extra) >= 3L,
            "Extra file must have 2 ID columns + at least 1 distance column."))
          id_cols  <- names(extra)[1:2]
          val_cols <- setdiff(names(extra), id_cols)
          extra_keep <- extra[, val_cols, drop = FALSE]
          key <- function(a, b) { a<-trimws(as.character(a)); b<-trimws(as.character(b)); ifelse(a<=b, paste(a,b,sep="__"), paste(b,a,sep="__")) }
          extra_keep$.key <- key(extra[[1L]], extra[[2L]])
          extra_keep <- extra_keep[!duplicated(extra_keep$.key), , drop = FALSE]
          df$.key <- key(df$Pop1, df$Pop2)
          df <- merge(df, extra_keep, by = ".key", all.x = TRUE, sort = FALSE)
          df$.key <- NULL
        }
        df
      } else {
        shiny::req(input$mt_file)
        .mt_read_file(input$mt_file, input$mt_sep, input$mt_header)
      }
    })

    # ── Uploaded-file confirmations ──────────────────────────────────────
    .file_status_ui <- function(fileinfo, df_reactive) {
      if (is.null(fileinfo)) return(tags$p(style="color:#999;font-size:11px;", icon("info-circle"), " No file uploaded yet."))
      n <- tryCatch(nrow(df_reactive()), error = function(e) NA_integer_)
      tags$p(style="color:#166534;font-size:11px;background:#f0fdf4;border:1px solid #bbf7d0;border-radius:4px;padding:4px 6px;",
        icon("check-circle"), " Loaded: ", tags$strong(fileinfo$name),
        if (!is.na(n)) sprintf(" (%d rows)", n) else "")
    }
    output$ibd_ext_file_status <- renderUI(.file_status_ui(input$ibd_ext_file, full_pair_table_external_r))
    output$ibd_dgeo_file_status <- renderUI({
      .file_status_ui(input$ibd_dgeo_file,
        reactive(.mt_read_file(input$ibd_dgeo_file, input$ibd_dgeo_sep, input$ibd_dgeo_header)))
    })
    output$mt_file_status <- renderUI({
      .file_status_ui(input$mt_file, reactive(.mt_read_file(input$mt_file, input$mt_sep, input$mt_header)))
    })
    output$mt_extra_file_status <- renderUI({
      .file_status_ui(input$mt_extra_file,
        reactive(.mt_read_file(input$mt_extra_file, input$mt_extra_sep, input$mt_extra_header)))
    })

    # Scale-adaptive formatting: fixed "%.6f" makes any statistic smaller
    # than ~1e-6 collapse to the same visible value across many permutation
    # quantiles — this shows enough significant digits regardless of scale.
    .fmt_stat <- function(x, digits = 6) {
      if (!is.finite(x)) return("NA")
      if (x != 0 && abs(x) < 10^(-(digits - 1))) formatC(x, format = "e", digits = digits - 1)
      else formatC(x, format = "f", digits = digits)
    }

    .guess_col <- function(cols, patterns, fallback) {
      for (pat in patterns) { hit <- grep(pat, cols, value = TRUE, ignore.case = TRUE); if (length(hit)) return(hit[1L]) }
      fallback
    }

    output$mt_col_pop1_ui <- renderUI({
      cols <- tryCatch(names(mt_base_df_r()), error = function(e) character(0))
      selectInput(session$ns("mt_col_pop1"), "Population 1 column:", choices = cols,
                  selected = .guess_col(cols, c("^Pop1$"), cols[1]))
    })
    output$mt_col_pop2_ui <- renderUI({
      cols <- tryCatch(names(mt_base_df_r()), error = function(e) character(0))
      selectInput(session$ns("mt_col_pop2"), "Population 2 column:", choices = cols,
                  selected = .guess_col(cols, c("^Pop2$"), cols[min(2L, length(cols))]))
    })

    # The 2D isolation-by-distance habitat model (Rousset 1997) regresses
    # genetic distance on ln(geographic distance), not raw distance.
    .raw_dist_warning <- function(xcol, log_checked) {
      looks_raw <- !is.null(xcol) && nzchar(xcol) &&
        grepl("dgeo|dist", xcol, ignore.case = TRUE) && !grepl("ln|log", xcol, ignore.case = TRUE)
      if (looks_raw && !isTRUE(log_checked)) {
        tags$p(style="color:#92400e;background:#fffbeb;border:1px solid #fcd34d;border-radius:4px;padding:4px 6px;font-size:11px;margin-top:6px;",
          icon("exclamation-triangle"), " ", tags$strong(xcol), " looks raw (un-logged). Tick \"ln(transform) X\" ",
          "or pick an already-logged column such as ", tags$code("lnDgeo"), " for the 2D habitat model.")
      }
    }
    output$mt_double_log_warning <- renderUI(.raw_dist_warning(input$mt_col_x, input$mt_log_x))

    output$mt_col_x_ui <- renderUI({
      df <- tryCatch(mt_base_df_r(), error = function(e) NULL)
      cols <- if (is.null(df)) character(0) else names(df)[sapply(df, is.numeric)]
      selectInput(session$ns("mt_col_x"), "X column:", choices = cols,
                  selected = .guess_col(cols, c("lnDgeo", "Dgeo"), if (length(cols)) cols[1] else NULL))
    })
    output$mt_col_y_ui <- renderUI({
      df <- tryCatch(mt_base_df_r(), error = function(e) NULL)
      cols <- if (is.null(df)) character(0) else names(df)[sapply(df, is.numeric)]
      selectInput(session$ns("mt_col_y"), "Y column:", choices = cols,
                  selected = .guess_col(cols, c("^FR$", "^FR_raw$", "FST_ENA", "DCSE_INA"),
                                        if (length(cols) >= 2L) cols[2] else NULL))
    })

    # Build a symmetric matrix of RANKS from a symmetric matrix of raw
    # values — used to feed the C++ cross-product engines for a
    # Spearman-style test (rank first, then test the ranks with the same
    # cross-product engine as Pearson/slope).
    .rank_matrix <- function(m) {
      n <- nrow(m)
      idx <- which(lower.tri(matrix(TRUE, n, n)))
      vals <- m[idx]
      ok <- is.finite(vals)
      rk <- vals; rk[ok] <- rank(vals[ok])
      mr <- matrix(NA_real_, n, n, dimnames = dimnames(m))
      mr[idx] <- rk
      mr_t <- t(mr)
      mr[upper.tri(mr)] <- mr_t[upper.tri(mr)]
      mr
    }

    mantel_result_r <- eventReactive(input$run_mantel, {
      df <- mt_base_df_r()
      shiny::req(input$mt_col_pop1, input$mt_col_pop2, input$mt_col_x, input$mt_col_y)
      p1c <- input$mt_col_pop1; p2c <- input$mt_col_pop2; xcol <- input$mt_col_x; ycol <- input$mt_col_y

      shiny::validate(
        shiny::need(all(c(p1c, p2c, xcol, ycol) %in% names(df)), "Selected columns not found."),
        shiny::need(p1c != p2c, "Population 1 and 2 must differ."),
        shiny::need(xcol != ycol, "X and Y must differ.")
      )

      if (nzchar(trimws(input$mt_exclude %||% ""))) {
        excl <- trimws(strsplit(input$mt_exclude, ",")[[1L]]); excl <- excl[nzchar(excl)]
        if (length(excl)) {
          key <- function(a,b){a<-trimws(as.character(a));b<-trimws(as.character(b));ifelse(a<=b,paste(a,b,sep="__"),paste(b,a,sep="__"))}
          key_df <- key(df[[p1c]], df[[p2c]])
          key_excl <- vapply(excl, function(s) {
            ids <- trimws(strsplit(s, "-")[[1L]]); if (length(ids) == 2L) key(ids[1], ids[2]) else NA_character_
          }, character(1L))
          df <- df[!(key_df %in% key_excl), , drop = FALSE]
        }
      }

      x <- suppressWarnings(as.numeric(df[[xcol]]))
      y <- suppressWarnings(as.numeric(df[[ycol]]))
      stat <- input$mt_stat
      # Rousset's 1D/2D fix the ln-transform automatically (1D = raw X,
      # 2D = ln(X)); for Pearson/Spearman the checkbox applies as usual.
      use_log <- if (stat %in% c("rousset1d", "rousset2d")) identical(stat, "rousset2d") else isTRUE(input$mt_log_x)
      if (use_log) x <- ifelse(x > 0, log(x), NA_real_)
      # Both Rousset options compute the same underlying statistic (the
      # regression slope) — only the X pre-processing above differs.
      calc_stat <- if (stat %in% c("rousset1d", "rousset2d")) "b" else stat

      all_labels <- sort(unique(trimws(c(as.character(df[[p1c]]), as.character(df[[p2c]])))))
      tmp <- data.frame(P1 = trimws(as.character(df[[p1c]])), P2 = trimws(as.character(df[[p2c]])), X = x, Y = y)
      m_x <- .mt_build_square(tmp, "P1", "P2", "X", all_labels)
      m_y <- .mt_build_square(tmp, "P1", "P2", "Y", all_labels)

      n_perm <- as.integer(input$mt_n_perm)
      p_formula <- input$mt_p_formula %||% "plus1"
      seed <- 67144630L  # fixed internal seed, not exposed to the user

      # Always attempts the native C++ engine first (fast); if it errors for
      # any reason, silently falls back to the pure-R engine (same statistic,
      # same p-value formula) so the user always gets a result. Which C++
      # engine runs depends on the chosen p-value formula: mantel_plus1_cpp
      # for (b+1)/(m+1), mantel_genepop_cpp for the plain b/m proportion.
      res <- tryCatch({
        mx_eng <- if (identical(calc_stat, "spearman")) .rank_matrix(m_x) else m_x
        my_eng <- if (identical(calc_stat, "spearman")) .rank_matrix(m_y) else m_y
        set.seed(seed)
        withProgress(message = "Running Mantel test\u2026", value = 0.3, {
          cpp_res <- if (identical(p_formula, "plain"))
            mantel_genepop_cpp(mx_eng, my_eng, n_perm, as.double(seed))
          else
            mantel_plus1_cpp(mx_eng, my_eng, n_perm)
          setProgress(1.0)
        })
        n <- nrow(m_x)
        lower_idx <- which(lower.tri(matrix(TRUE, n, n)))
        x_all <- m_x[lower_idx]; y_all <- m_y[lower_idx]
        ok <- is.finite(x_all) & is.finite(y_all)
        stat_obs <- if (calc_stat == "b") unname(coef(lm(y_all[ok] ~ x_all[ok]))[2L])
                    else if (calc_stat == "spearman") suppressWarnings(cor(x_all[ok], y_all[ok], method = "spearman"))
                    else suppressWarnings(cor(x_all[ok], y_all[ok]))
        lm0 <- tryCatch(lm(y_all[ok] ~ x_all[ok]), error = function(e) NULL)
        pair_idx <- which(lower.tri(matrix(TRUE, n, n)), arr.ind = TRUE)
        list(
          stat_obs = stat_obs, p_pos = cpp_res$p_pos, p_neg = cpp_res$p_neg,
          n_pairs = cpp_res$n_pairs,
          slope = if (!is.null(lm0)) unname(coef(lm0)[2L]) else NA_real_,
          intercept = if (!is.null(lm0)) unname(coef(lm0)[1L]) else NA_real_,
          r2 = if (!is.null(lm0)) summary(lm0)$r.squared else NA_real_,
          x = x_all[ok], y = y_all[ok],
          pop1 = rownames(m_x)[pair_idx[ok, "row"]], pop2 = rownames(m_x)[pair_idx[ok, "col"]],
          common = rownames(m_x), perm_stats = as.numeric(cpp_res$perm_stats),
          engine = "cpp"
        )
      }, error = function(e) {
        set.seed(seed)
        r <- withProgress(message = "Running Mantel test\u2026", value = 0.2, {
          rr <- .mt_mantel_matrix(m_x, m_y, n_perm = n_perm, stat = calc_stat, p_formula = p_formula)
          setProgress(1.0)
          rr
        })
        r$engine <- "r"
        r
      })
      res$x_label <- paste0(xcol, if (use_log) " (ln)" else "")
      res$y_label <- ycol
      res$stat_label <- switch(stat, rousset1d = "Rousset's 1D", rousset2d = "Rousset's 2D",
                                spearman = "Spearman rho", "Pearson r")
      res$p_formula <- p_formula
      res
    })

    output$ui_mantel_key_values <- renderUI({
      r <- mantel_result_r()
      pv <- r$p_pos
      r2 <- r$r2
      fmt_lbl <- if (identical(r$p_formula, "plain")) "b/m" else "(b+1)/(m+1)"
      tags$div(style = "display:flex; flex-wrap:wrap; gap:28px; padding:6px 0 14px 0; font-size:14px; color:#333;",
        tags$div(tags$strong(r$stat_label, style="color:#555;"), tags$br(),
                 tags$span(round(r$stat_obs, 4), style="font-size:18px;font-weight:700;")),
        tags$div(tags$strong(paste0("p-value (", fmt_lbl, ")"), style="color:#555;"), tags$br(),
                 tags$span(if (is.na(pv)) "NA" else formatC(pv, format = "f", digits = 4), style="font-size:18px;font-weight:700;")),
        tags$div(tags$strong("Pairs used", style="color:#555;"), tags$br(),
                 tags$span(r$n_pairs, style="font-size:18px;font-weight:700;")),
        tags$div(tags$strong("Variance explained (R\u00b2)", style="color:#555;"), tags$br(),
                 tags$span(if (is.na(r2)) "NA" else paste0(round(r2 * 100, 1), "%"), style="font-size:18px;font-weight:700;"))
      )
    })

    output$ui_mantel_summary <- renderUI({
      r <- mantel_result_r()
      tags$div(style = "margin-top:8px; font-family:monospace; font-size:12px; color:#555;",
        sprintf("Engine: %s \u2014 Slope = %.6f, Intercept = %.6f",
                if (identical(r$engine, "cpp")) "C++ (native)" else "R (portable fallback)",
                r$slope, r$intercept), tags$br(),
        sprintf("One-sided p (positive association) = %s",
                if (is.na(r$p_pos)) "NA" else formatC(r$p_pos, format = "f", digits = 4)), tags$br(),
        sprintf("One-sided p (negative association) = %s",
                if (is.na(r$p_neg)) "NA" else formatC(r$p_neg, format = "f", digits = 4)), tags$br(),
        sprintf("Common populations: %d", length(r$common))
      )
    })

    .mantel_summary_df <- function(r) {
      data.frame(
        Quantity = c("Engine", "X variable", "Y variable", "Statistic", "Observed value",
                     "Slope b (Y ~ X)", "Intercept", "R\u00b2",
                     "p-value formula",
                     "p (one-sided, positive assoc.)", "p (one-sided, negative assoc.)",
                     "Pairs used (n)", "Common populations (N)", "Permutations"),
        Value = c(if (identical(r$engine, "cpp")) "C++ (native)" else "R (portable)",
                  r$x_label, r$y_label, r$stat_label, .fmt_stat(r$stat_obs),
                  .fmt_stat(r$slope), .fmt_stat(r$intercept),
                  sprintf("%.4f", r$r2),
                  if (identical(r$p_formula, "plain")) "b/m \u2014 plain proportion" else "(b+1)/(m+1) \u2014 corrected proportion",
                  if (is.na(r$p_pos)) "NA" else sprintf("%.4f", r$p_pos),
                  if (is.na(r$p_neg)) "NA" else sprintf("%.4f", r$p_neg),
                  r$n_pairs, length(r$common), length(r$perm_stats)),
        stringsAsFactors = FALSE
      )
    }

    output$dt_mantel_summary <- DT::renderDT({
      d <- .mantel_summary_df(mantel_result_r())
      DT::datatable(d, rownames = FALSE,
        options = list(dom = "t", pageLength = nrow(d), ordering = FALSE),
        class = "compact stripe hover")
    })

    output$dl_mantel_summary_txt <- downloadHandler(
      filename = function() paste0("mantel_result_summary_", Sys.Date(), ".txt"),
      content  = function(file) {
        d <- .mantel_summary_df(mantel_result_r())
        write.table(d, file, sep = "\t", row.names = FALSE, quote = FALSE)
      }
    )

    output$dt_mantel_quantiles <- DT::renderDT({
      r <- mantel_result_r()
      shiny::req(length(r$perm_stats) > 0L)
      probs <- c(0.005, 0.01, 0.025, 0.05, 0.10, 0.50, 0.90, 0.95, 0.975, 0.99, 0.995)
      q <- stats::quantile(r$perm_stats, probs = probs, na.rm = TRUE, type = 7)
      d <- data.frame(
        Percentile = paste0(probs * 100, "%"),
        `Null value` = vapply(unname(q), .fmt_stat, character(1L)),
        check.names = FALSE, stringsAsFactors = FALSE
      )
      d <- rbind(d, data.frame(Percentile = "OBSERVED", `Null value` = .fmt_stat(r$stat_obs), check.names = FALSE))
      DT::datatable(d, rownames = FALSE,
        options = list(dom = "t", pageLength = nrow(d), ordering = FALSE),
        class = "compact stripe hover") |>
        DT::formatStyle("Percentile", target = "row",
          backgroundColor = DT::styleEqual("OBSERVED", "#fef3c7"),
          fontWeight = DT::styleEqual("OBSERVED", "bold"))
    })

    output$dt_mantel_data <- DT::renderDT({
      r <- mantel_result_r()
      df <- data.frame(Pop1 = r$pop1, Pop2 = r$pop2, X = round(r$x, 6), Y = round(r$y, 6))
      names(df)[3:4] <- c(r$x_label, r$y_label)
      DT::datatable(df, rownames = FALSE,
        options = list(scrollX = TRUE, pageLength = 10, dom = "lrtip"),
        class = "compact stripe hover")
    })

    output$dl_mantel_txt <- downloadHandler(
      filename = function() paste0("mantel_test_", Sys.Date(), ".txt"),
      content  = function(file) {
        r <- mantel_result_r()
        d_summary <- .mantel_summary_df(r)
        probs <- c(0.005, 0.01, 0.025, 0.05, 0.10, 0.50, 0.90, 0.95, 0.975, 0.99, 0.995)
        q <- stats::quantile(r$perm_stats, probs = probs, na.rm = TRUE, type = 7)
        d_quant <- data.frame(
          Percentile = paste0(probs * 100, "%"),
          Null_value = vapply(unname(q), .fmt_stat, character(1L)),
          stringsAsFactors = FALSE
        )
        d_quant <- rbind(d_quant, data.frame(Percentile = "OBSERVED", Null_value = .fmt_stat(r$stat_obs)))
        d_data <- data.frame(Pop1 = r$pop1, Pop2 = r$pop2, X = round(r$x, 6), Y = round(r$y, 6))
        names(d_data)[3:4] <- c(r$x_label, r$y_label)

        con <- file(file, open = "w", encoding = "UTF-8"); on.exit(close(con))
        writeLines(c("Mantel test results", ""), con = con, useBytes = TRUE)
        writeLines("Summary:", con = con)
        write.table(d_summary, file = con, sep = "\t", row.names = FALSE, quote = FALSE, append = TRUE)
        writeLines("", con = con)
        writeLines("Null distribution quantiles:", con = con)
        write.table(d_quant, file = con, sep = "\t", row.names = FALSE, quote = FALSE, append = TRUE)
        writeLines("", con = con)
        writeLines("Data used:", con = con)
        write.table(d_data, file = con, sep = "\t", row.names = FALSE, quote = FALSE, append = TRUE)
      }
    )

  })
}
