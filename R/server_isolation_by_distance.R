# server_isolation_by_distance.R
# Isolation by Distance \u2014 Geographic distances + Mantel test
#
# Tab 1: Geographic distances (Dgeo + ln(Dgeo))
# Tab 2: Mantel test on square or rectangular (column-wise) matrices

server_isolation_by_distance <- function(id, rv) {
  moduleServer(id, function(input, output, session) {

    # ── Helpers ────────────────────────────────────────────────────────────────
    `%||%` <- function(a, b) if (!is.null(a)) a else b
    sql_id <- function(con, x) as.character(DBI::dbQuoteIdentifier(con, x))

    # ── Haversine distance (km) ────────────────────────────────────────────────
    haversine_km <- function(lat1, lon1, lat2, lon2) {
      R <- 6371.0
      dlat <- (lat2 - lat1) * pi / 180
      dlon <- (lon2 - lon1) * pi / 180
      a <- sin(dlat / 2)^2 +
           cos(lat1 * pi / 180) * cos(lat2 * pi / 180) * sin(dlon / 2)^2
      2 * R * asin(sqrt(a))
    }

    # Build symmetric geographic distance matrix from a data.frame of centroids
    # coords: data.frame with columns Population, Latitude, Longitude
    geo_dist_matrix <- function(coords) {
      pops <- coords$Population
      n <- nrow(coords)
      mat <- matrix(0.0, n, n, dimnames = list(pops, pops))
      for (i in seq_len(n - 1L)) {
        for (j in (i + 1L):n) {
          d <- haversine_km(coords$Latitude[i], coords$Longitude[i],
                            coords$Latitude[j], coords$Longitude[j])
          mat[i, j] <- mat[j, i] <- d
        }
      }
      mat
    }

    # ── Parse distance file (square or rectangular column-wise) ────────────────
    # Rectangular format: first row = pair labels (PopA-PopB), each column = one
    # distance value. Multiple rows = multiple loci/replicates. Average across rows.
    parse_dist_file <- function(file_path, format = "rectangular", sep = NULL) {
      # Auto-detect separator
      if (is.null(sep)) {
        first_lines <- readLines(file_path, n = 5, warn = FALSE)
        sep <- if (any(grepl("\t", first_lines))) "\t" else ","
      }
      raw <- tryCatch(
        read.csv(file_path, check.names = FALSE, stringsAsFactors = FALSE, sep = sep),
        error = function(e) {
          read.table(file_path, header = TRUE, sep = sep,
                     check.names = FALSE, stringsAsFactors = FALSE)
        }
      )

      if (format == "square") {
        # First column = row names, rest = matrix
        rn <- as.character(raw[[1L]])
        mat <- as.matrix(raw[, -1L, drop = FALSE])
        storage.mode(mat) <- "double"
        rownames(mat) <- rn
        colnames(mat) <- rn
        return(mat)
      }

      # Rectangular: column headers = pair labels "PopA-PopB" (or "PopA_PopB", "PopA vs PopB")
      pair_names <- names(raw)
      # Try several separators for pair labels
      split_pair <- function(s) {
        for (pat in c("-", "_", " vs ", " - ", " _ ")) {
          parts <- strsplit(s, pat, fixed = TRUE)[[1L]]
          if (length(parts) == 2L) return(trimws(parts))
        }
        return(NULL)
      }

      pops <- character(0)
      pair_list <- list()
      for (pn in pair_names) {
        parts <- split_pair(pn)
        if (!is.null(parts)) {
          pair_list[[pn]] <- parts
          pops <- c(pops, parts)
        }
      }
      pops <- unique(pops)

      # Average values per column (across rows = loci/replicates)
      avg_vals <- sapply(pair_names, function(cn) {
        v <- raw[[cn]]
        v <- suppressWarnings(as.numeric(v))
        mean(v, na.rm = TRUE)
      })
      names(avg_vals) <- pair_names

      # Build square matrix
      n <- length(pops)
      mat <- matrix(NA_real_, n, n, dimnames = list(pops, pops))
      for (pn in pair_names) {
        parts <- pair_list[[pn]]
        if (!is.null(parts)) {
          p1 <- parts[1L]; p2 <- parts[2L]
          if (p1 %in% pops && p2 %in% pops) {
            mat[p1, p2] <- mat[p2, p1] <- avg_vals[[pn]]
          }
        }
      }
      diag(mat) <- 0
      mat
    }

    # ── Mantel permutation test ────────────────────────────────────────────────
    # Permute rows/columns jointly (preserves symmetry)
    mantel_test <- function(mat1, mat2, n_perm = 9999, method = "pearson") {
      ok <- !is.na(mat1) & !is.na(mat2) & lower.tri(mat1)
      v1 <- mat1[ok]; v2 <- mat2[ok]
      n <- length(v1)

      if (n < 3L) {
        return(list(r = NA_real_, p_value = NA_real_, n = n,
                    v1 = v1, v2 = v2, perm_r = numeric(0),
                    message = "Not enough data points (need >= 3 pairs)"))
      }

      r_obs <- cor(v1, v2, method = method, use = "complete.obs")

      # Permute rows/columns of mat2 jointly
      n_mat <- nrow(mat2)
      perm_r <- numeric(n_perm)
      for (k in seq_len(n_perm)) {
        idx <- sample(n_mat)
        perm_mat2 <- mat2[idx, idx]
        v2_perm <- perm_mat2[ok]
        perm_r[k] <- cor(v1, v2_perm, method = method, use = "complete.obs")
      }
      perm_r <- perm_r[is.finite(perm_r)]
      p_value <- mean(perm_r >= r_obs)

      list(r = r_obs, p_value = p_value, n = n,
           v1 = v1, v2 = v2, perm_r = perm_r,
           message = "OK", n_perm = length(perm_r))
    }

    # ── DB plumbing ────────────────────────────────────────────────────────────
    db_tick    <- reactive({ rv$db_tick })
    con_r      <- reactive({ req(rv$con); rv$con })
    tbl_meta_r <- reactive({ rv$tbl_meta %||% "meta" })

    db_ready <- reactive({
      db_tick(); con <- con_r()
      shiny::req(isTRUE(rv$db_ready))
      shiny::validate(
        shiny::need(DBI::dbExistsTable(con, tbl_meta_r()), "DuckDB meta table missing.")
      )
      TRUE
    })

    meta_schema_r <- reactive({
      db_ready(); con <- con_r()
      info <- DBI::dbGetQuery(con,
        sprintf("PRAGMA table_info(%s)", DBI::dbQuoteIdentifier(con, tbl_meta_r())))
      cols    <- info$name
      ind_col <- if ("individual" %in% cols) "individual"
                 else if ("indiv_id" %in% cols) "indiv_id"
                 else shiny::validate(shiny::need(FALSE, "No individual column found in meta."))
      pop_col <- c("Population","population","pop","pop_code")[
        c("Population","population","pop","pop_code") %in% cols][1]
      shiny::validate(shiny::need(!is.na(pop_col), "No population column found in meta."))
      list(ind_col = ind_col, pop_col = pop_col,
           has_lat = "Latitude" %in% cols, has_lon = "Longitude" %in% cols)
    })

    # ── Population list ────────────────────────────────────────────────────────
    pops_r <- reactive({
      db_ready(); con <- con_r(); ms <- meta_schema_r()
      as.character(DBI::dbGetQuery(con, sprintf(
        "SELECT DISTINCT CAST(%s AS VARCHAR) AS p FROM %s WHERE %s IS NOT NULL ORDER BY p",
        sql_id(con,ms$pop_col), sql_id(con,tbl_meta_r()),
        sql_id(con,ms$pop_col)))$p)
    })

    # ── GPS centroids per population ───────────────────────────────────────────
    gps_coords_r <- reactive({
      db_ready(); con <- con_r(); ms <- meta_schema_r()
      shiny::validate(shiny::need(
        ms$has_lat && ms$has_lon,
        "No Latitude/Longitude columns in meta table. Please re-import with GPS data."
      ))
      df <- DBI::dbGetQuery(con, sprintf(
        "SELECT CAST(%s AS VARCHAR) AS Population,
                AVG(CAST(Latitude  AS DOUBLE)) AS Latitude,
                AVG(CAST(Longitude AS DOUBLE)) AS Longitude
         FROM %s
         WHERE %s IS NOT NULL
           AND Latitude IS NOT NULL AND Longitude IS NOT NULL
           AND CAST(Latitude  AS VARCHAR) <> ''
           AND CAST(Longitude AS VARCHAR) <> ''
         GROUP BY Population
         ORDER BY Population",
        sql_id(con, ms$pop_col),
        sql_id(con, tbl_meta_r()),
        sql_id(con, ms$pop_col)))
      shiny::validate(shiny::need(nrow(df) >= 2L,
        "At least 2 populations with GPS coordinates required."))
      df
    })

    # ── Geographic distances reactive ──────────────────────────────────────────
    geo_results_r <- reactive({
      coords <- gps_coords_r()
      geo_mat <- geo_dist_matrix(coords)

      # Build long format (lower triangle)
      pops <- coords$Population
      n <- length(pops)
      geo_rows <- list()
      for (ii in seq_len(n - 1L)) {
        for (jj in (ii + 1L):n) {
          d <- geo_mat[pops[ii], pops[jj]]
          geo_rows[[length(geo_rows) + 1L]] <- data.frame(
            Pop1   = pops[ii],
            Pop2   = pops[jj],
            Dgeo   = round(d, 4),
            lnDgeo = round(log(d), 4),
            stringsAsFactors = FALSE
          )
        }
      }
      geo_long <- do.call(rbind, geo_rows)

      list(
        geo_mat = geo_mat,
        geo_long = geo_long,
        coords = coords
      )
    })

    # ── Value boxes ────────────────────────────────────────────────────────────
    output$vb_pops <- renderUI({
      tryCatch(tags$span(length(pops_r())), error=function(e) tags$span("\u2014"))
    })
    output$vb_gps <- renderUI({
      tryCatch({
        coords <- gps_coords_r()
        tags$span(nrow(coords))
      }, error=function(e) tags$span("\u2014"))
    })
    output$vb_pairs <- renderUI({
      tryCatch({
        coords <- gps_coords_r()
        n <- nrow(coords)
        tags$span(n * (n - 1L) / 2L)
      }, error=function(e) tags$span("\u2014"))
    })

    # ══════════════════════════════════════════════════════════════════════════
    #  TAB 1 \u2014 Geographic Distances
    # ══════════════════════════════════════════════════════════════════════════

    # ── Geographic long table ──────────────────────────────────────────────────
    output$dt_geo_long <- DT::renderDT({
      r <- geo_results_r()
      DT::datatable(r$geo_long, rownames=FALSE,
        options=list(pageLength=25, scrollX=TRUE, dom="lftip"),
        class="compact hover stripe",
        colnames = c("Population 1","Population 2","Dgeo (km)","ln(Dgeo)")) |>
        DT::formatRound(c("Dgeo","lnDgeo"), digits = 4)
    }, server=TRUE)

    # ── Geographic matrix display ──────────────────────────────────────────────
    render_mat_html <- function(mat, fmt=2,
                                thr =c(10, 50, 200),
                                clrs=c("#eff6ff","#dbeafe","#fef9c3","#fef2f2")) {
      pops <- rownames(mat); n <- length(pops)
      cell <- function(i,j) {
        if (i==j) return('<td class="diag">\u2014</td>')
        if (i<j)  return('<td class="upper">\u00b7</td>')
        v <- mat[i,j]; if (is.na(v)) return('<td style="color:#94a3b8;">NA</td>')
        bg <- clrs[findInterval(v,thr)+1L]
        sprintf('<td style="background:%s;">%s</td>',bg,round(v,fmt))
      }
      thead <- paste0('<tr><th></th>',paste(sprintf('<th>%s</th>',pops[-n]),collapse=""),'</tr>')
      tbody <- paste(sapply(seq_len(n),function(i){
        if(i==1L) return("")
        paste0('<tr><td class="lbl">',pops[i],'</td>',
               paste(sapply(seq_len(n),function(j)cell(i,j)),collapse=""),'</tr>')
      }),collapse="")
      HTML(sprintf('<div class="ibd-matrix-wrap"><table class="ibd-matrix"><thead>%s</thead><tbody>%s</tbody></table></div>',
                   thead,tbody))
    }

    output$ui_geo_matrix <- renderUI({
      r <- tryCatch(geo_results_r(), error=function(e) NULL)
      if (is.null(r) || is.null(r$geo_mat))
        return(tags$p("No GPS data available.", style="color:#94a3b8;"))
      render_mat_html(r$geo_mat)
    })

    # ── Download geographic distances ──────────────────────────────────────────
    output$dl_geo_csv <- downloadHandler(
      filename = function() paste0("geographic_distances_", Sys.Date(), ".csv"),
      content = function(file) {
        r <- geo_results_r()
        write.csv(r$geo_long, file, row.names = FALSE)
      }
    )
    output$dl_geo_txt <- downloadHandler(
      filename = function() paste0("geographic_distances_", Sys.Date(), ".txt"),
      content = function(file) {
        r <- geo_results_r()
        write.table(r$geo_long, file, sep = "\t", row.names = FALSE, quote = FALSE)
      }
    )

    # ══════════════════════════════════════════════════════════════════════════
    #  TAB 2 \u2014 Mantel Test
    # ══════════════════════════════════════════════════════════════════════════

    # Reactive: matrix 1 (genetic) \u2014 always from upload
    mat1_r <- reactive({
      shiny::req(input$file_mat1)
      parse_dist_file(input$file_mat1$datapath, input$mat1_format)
    })

    # Reactive: matrix 2 (geographic)
    mat2_r <- reactive({
      src <- input$mat2_source
      if (src %in% c("gps_km","gps_ln")) {
        r <- shiny::req(geo_results_r())
        mat <- r$geo_mat
        if (src == "gps_ln") {
          mat[mat > 0] <- log(mat[mat > 0])
          diag(mat) <- 0
        }
        return(mat)
      }
      # Upload
      shiny::req(input$file_mat2)
      parse_dist_file(input$file_mat2$datapath, input$mat2_format)
    })

    # Run Mantel test
    mantel_results_r <- eventReactive(input$run_mantel, {
      m1 <- mat1_r()
      m2 <- mat2_r()

      # Align populations
      p1 <- rownames(m1); p2 <- rownames(m2)
      common <- intersect(p1, p2)
      shiny::validate(shiny::need(
        length(common) >= 3L,
        sprintf("Need at least 3 populations present in BOTH matrices. Found: %d common populations.",
                length(common))))

      m1 <- m1[common, common, drop = FALSE]
      m2 <- m2[common, common, drop = FALSE]

      n_perm <- as.integer(input$n_perm_mantel %||% 9999L)
      method <- input$mantel_method %||% "pearson"

      withProgress(message = "Running Mantel test...", value = 0.5, {
        res <- mantel_test(m1, m2, n_perm = n_perm, method = method)
        incProgress(0.5, detail = "Done")
      })

      res$mat1 <- m1
      res$mat2 <- m2
      res$method <- method
      res$n_perm <- n_perm
      res$pops <- common
      res
    })

    # Mantel value boxes
    output$vb_mantel_r <- renderUI({
      tryCatch({
        r <- mantel_results_r()
        v <- round(r$r, 4)
        col <- if (!is.na(v) && v > 0.5) "#9d174d"
               else if (!is.na(v) && v > 0.2) "#854d0e"
               else "#166534"
        tags$span(style=paste0("color:",col,";"), if(is.na(v)) "\u2014" else v)
      }, error=function(e) tags$span("\u2014"))
    })
    output$vb_mantel_p <- renderUI({
      tryCatch({
        r <- mantel_results_r()
        v <- r$p_value
        sig <- if (!is.na(v) && v < 0.001) "***"
               else if (!is.na(v) && v < 0.01) "**"
               else if (!is.na(v) && v < 0.05) "*"
               else "ns"
        col <- if (!is.na(v) && v < 0.05) "#166534" else "#9d174d"
        tags$span(style=paste0("color:",col,";"),
                  if(is.na(v)) "\u2014" else sprintf("%.4f %s", v, sig))
      }, error=function(e) tags$span("\u2014"))
    })
    output$vb_mantel_n <- renderUI({
      tryCatch(tags$span(mantel_results_r()$n), error=function(e) tags$span("\u2014"))
    })
    output$vb_mantel_pops <- renderUI({
      tryCatch(tags$span(length(mantel_results_r()$pops)), error=function(e) tags$span("\u2014"))
    })

    # Mantel result text
    output$ui_mantel_result <- renderUI({
      r <- tryCatch(mantel_results_r(), error=function(e) NULL)
      if (is.null(r))
        return(tags$p("Upload Matrix 1, configure Matrix 2, and click 'Run Mantel Test'.",
                      style="color:#94a3b8;"))
      sig <- if (r$p_value < 0.001) "Highly significant (p < 0.001) ***"
             else if (r$p_value < 0.01) "Very significant (p < 0.01) **"
             else if (r$p_value < 0.05) "Significant (p < 0.05) *"
             else "Not significant (p \u2265 0.05)"
      tags$div(class="ibd-mantel-result",
        tags$strong("Mantel Test Summary"),
        tags$br(),
        sprintf("Method:          %s", r$method),
        tags$br(),
        sprintf("Permutations:    %d", r$n_perm),
        tags$br(),
        sprintf("Pairs (n):       %d", r$n),
        tags$br(),
        sprintf("Populations:     %d", length(r$pops)),
        tags$br(), tags$br(),
        sprintf("Mantel r:        %.6f", r$r),
        tags$br(),
        sprintf("P-value:         %.6f", r$p_value),
        tags$br(), tags$br(),
        tags$strong(sprintf("Result: %s", sig))
      )
    })

    # Download Mantel results
    output$dl_mantel_csv <- downloadHandler(
      filename = function() paste0("mantel_test_", Sys.Date(), ".csv"),
      content = function(file) {
        r <- mantel_results_r()
        hdr <- c(
          "# Mantel Test Results",
          paste0("# Method: ", r$method),
          paste0("# Permutations: ", r$n_perm),
          paste0("# Pairs (n): ", r$n),
          paste0("# Populations: ", length(r$pops)),
          paste0("# Mantel r: ", r$r),
          paste0("# P-value: ", r$p_value),
          "#"
        )
        writeLines(hdr, con = file)
        df <- data.frame(Distance1 = r$v1, Distance2 = r$v2)
        write.table(df, file = file, sep = ",", row.names = FALSE,
                    quote = FALSE, append = TRUE, col.names = TRUE)
      }
    )

    # Mantel scatter plot
    output$mantel_plot <- plotly::renderPlotly({
      r <- tryCatch(mantel_results_r(), error=function(e) NULL)
      if (is.null(r) || length(r$v1) < 3L) {
        return(plotly::plot_ly() |>
          plotly::layout(title = "Run Mantel test to see the scatter plot",
                         xaxis = list(title = ""),
                         yaxis = list(title = "")))
      }

      df <- data.frame(x = r$v1, y = r$v2)
      fit <- lm(y ~ x, data = df)

      plotly::plot_ly() |>
        plotly::add_markers(
          data = df, x = ~x, y = ~y,
          marker = list(color = "#2CBF9F", size = 7, opacity = 0.85),
          name = "Pairs",
          hoverinfo = "text",
          text = ~paste0("Matrix 1: ", round(x, 4),
                        "<br>Matrix 2: ", round(y, 4))
        ) |>
        plotly::add_lines(
          data = data.frame(x = df$x, y = fitted(fit)),
          x = ~x, y = ~y,
          line = list(color = "#B40F20", width = 2),
          name = sprintf("Regression (r = %.3f)", r$r)
        ) |>
        plotly::layout(
          title = list(
            text = sprintf("Mantel r = %.4f, p = %.4f (%s)",
                           r$r, r$p_value, r$method),
            font = list(size = 14)),
          xaxis = list(title = "Matrix 1 distance"),
          yaxis = list(title = "Matrix 2 distance"),
          legend = list(x = 0.02, y = 0.98, bgcolor = "rgba(255,255,255,0.8)")
        )
    })

  }) # end moduleServer
}