# ui_isolation_by_distance.R
# Isolation by Distance (Rousset 1997) + Mantel test.
#
# This module is a CONTINUATION of the "Null alleles" module — it reuses the
# pairwise FST/FST-ENA/DCSE/DCSE-INA (+ bootstrap CI) already computed there
# (rv$null_alleles_results) instead of recomputing them.
#
# Workflow: go to the "Null alleles" module first, choose your per-locus
# coding, click "Compute + Bootstrap + Export" — THEN come here.

isolation_by_distance_UI <- function(id) {
  ns <- NS(id)

  fluidPage(
    tags$head(gs_head()),

    module_banner("atom","Isolation by Distance · Mantel Test",""),

    fluidRow(
      box(
        width = 12, solidHeader = FALSE,
        uiOutput(ns("ui_run_status"))
      )
    ),

    tabsetPanel(
      id = ns("ibd_tabs"), type = "tabs",

      # ══════════════════════════════════════════════════════════════════
      # TAB 1 — Isolation by Distance
      # ══════════════════════════════════════════════════════════════════
      tabPanel(title = tagList(icon("chart-line"), " Isolation by Distance"), value = "tab_ibd",
        br(),

        fluidRow(
          box(width = 12, solidHeader = TRUE, status = "primary",
              title = div(style="background:#FFFFFF;padding:10px;color:#333a43;font-weight:600;",
                          icon("database"), " Data source"),
            radioButtons(ns("ibd_source"), NULL,
              choices = c("Null Alleles module (this session)" = "internal",
                          "Re-load an exported pairwise file"   = "external"),
              selected = "internal", inline = TRUE),
            conditionalPanel(
              condition = sprintf("input['%s'] == 'external'", ns("ibd_source")),
              fluidRow(
                column(6,
                  fileInput(ns("ibd_ext_file"), "Pairwise file (Pop1, Pop2, FST_ENA, DCSE_INA, Dgeo_m\u2026):",
                            accept = c(".csv", ".txt", ".tsv")),
                  radioButtons(ns("ibd_ext_sep"), "Separator:",
                    choices = c("Tab" = "\t", "Comma" = ",", "Semicolon" = ";"),
                    selected = "\t", inline = TRUE),
                  checkboxInput(ns("ibd_ext_header"), "File has header row", value = TRUE)
                ),
                column(6, uiOutput(ns("ibd_ext_file_status")))
              )
            )
          )
        ),

        fluidRow(
          box(width = 12, solidHeader = TRUE, status = "primary",
              title = div(style="background:#FFFFFF;padding:10px;color:#333a43;font-weight:600;",
                          icon("sliders-h"), " Rousset's Isolation by Distance model"),
            # tags$p(style="color:#555;font-size:12.5px;line-height:1.5;",
            #   "Choose one column for geographic distances (raw for 1 dimension, ln(D_geo) for 2 dimensions).",
            #   tags$br(),
            #   "Choose three columns (average, lower and higher limits of the confidence interval) for ",
            #   "Rousset's genetic distances (\u0398/(1-\u0398)), corrected or not for null alleles."),
            # tags$hr(),
            fluidRow(
              column(3, uiOutput(ns("ibd_col_geo_ui"))),
              column(3, uiOutput(ns("ibd_col_avg_ui"))),
              column(3, uiOutput(ns("ibd_col_lo_ui"))),
              column(3, uiOutput(ns("ibd_col_hi_ui")))
            ),
            tags$hr(),
            fluidRow(
              column(4,
                conditionalPanel(
                  condition = sprintf("input['%s'] == 'internal'", ns("ibd_source")),
                  radioButtons(ns("ibd_dgeo_source"), "Distance (D_geo) source:",
                    choices = c("GPS centroid (auto, Vincenty)" = "gps",
                                "External pairs/distances file" = "external"),
                    selected = "gps"),
                  conditionalPanel(
                    condition = sprintf("input['%s'] == 'external'", ns("ibd_dgeo_source")),
                    fileInput(ns("ibd_dgeo_file"), "Pairs/distances file (Pop1, Pop2, Distance):",
                              accept = c(".csv", ".txt", ".tsv")),
                    radioButtons(ns("ibd_dgeo_sep"), "Separator:",
                      choices = c("Comma" = ",", "Tab" = "\t", "Semicolon" = ";"),
                      selected = ",", inline = TRUE),
                    checkboxInput(ns("ibd_dgeo_header"), "File has header row", value = TRUE),
                    uiOutput(ns("ibd_dgeo_file_status")),
                    tags$p(style = "color:#777;font-size:11px;",
                      "Only pairs present in the file are used.")
                  ),
                  tags$p(style="color:#777;font-size:11px;",
                    "GPS mode needs Latitude/Longitude set at import for \u2265 2 populations (Vincenty geodesic distance, metres).")
                ),
                conditionalPanel(
                  condition = sprintf("input['%s'] == 'external'", ns("ibd_source")),
                  tags$p(style = "color:#777;font-size:11px;",
                    "D_geo is read directly from the uploaded file's Dgeo_m / lnDgeo columns.")
                )
              ),
              column(4,
                tags$div(style="font-size:12px;color:#555;margin-bottom:4px;", "Output file name:"),
                fluidRow(
                  column(7, textInput(ns("ibd_out_root"), "Root:", value = "",
                                       placeholder = "auto-filled from imported file")),
                  column(5, textInput(ns("ibd_out_suffix"), "Suffix:", value = ""))
                ),
                tags$p(style="color:#777;font-size:11px;",
                  "File name = ", tags$code("<root>-IBD-<suffix>.txt"))
              ),
              column(4,
                tags$div(style="margin-top:22px;"),
                actionButton(ns("run_ibd"), "Run IBD Regression",
                             icon = icon("rocket"), class = "btn-action-primary btn-block",
                             style = "font-weight:bold;")
              )
            )
          )
        ),

        fluidRow(
          box(width = 12, solidHeader = TRUE, status = "primary",
              title = div(style="background:#FFFFFF;padding:10px;color:#333a43;font-weight:600;",
                          icon("chart-line"), " Regression summary (slope / b / Nb / Nem)"),
            DT::DTOutput(ns("dt_ibd_reg")),
            tags$br(),
            uiOutput(ns("ui_ibd_interpretation"))
          )
        ),

        fluidRow(
          box(width = 12, solidHeader = FALSE,
              title = div(style="background:#FFFFFF;padding:10px;color:#333a43;font-weight:600;",
                          icon("table"), " Full pairwise table"),
            DT::DTOutput(ns("dt_ibd_table")),
            tags$br(),
            downloadButton(ns("dl_ibd_txt"), ".txt", class = "btn-action-secondary btn-sm")
          )
        )
      ),

      # ══════════════════════════════════════════════════════════════════
      # TAB 2 — Mantel Test
      # ══════════════════════════════════════════════════════════════════
      tabPanel(title = tagList(icon("project-diagram"), " Mantel Test"), value = "tab_mantel",
        br(),

        fluidRow(
          box(width = 12, solidHeader = TRUE, status = "primary",
              title = div(style="background:#FFFFFF;padding:10px;color:#333a43;font-weight:600;",
                          icon("database"), " Data source"),
            radioButtons(ns("mt_source"), NULL,
              choices = c("Internal pairwise table (from Null Alleles module)" = "internal",
                          "Upload a file"                                      = "upload"),
              selected = "internal", inline = TRUE),

            conditionalPanel(
              condition = sprintf("input['%s'] == 'upload'", ns("mt_source")),
              fluidRow(
                column(6,
                  fileInput(ns("mt_file"), "File (Pop1, Pop2, dist1, dist2, ...):",
                            accept = c(".csv", ".txt", ".tsv")),
                  radioButtons(ns("mt_sep"), "Separator:",
                    choices = c("Comma"=",", "Tab"="\t", "Semicolon"=";"),
                    selected = ",", inline = TRUE),
                  checkboxInput(ns("mt_header"), "File has header row", value = TRUE)
                ),
                column(6, uiOutput(ns("mt_file_status")))
              )
            ),

            conditionalPanel(
              condition = sprintf("input['%s'] == 'internal'", ns("mt_source")),
              checkboxInput(ns("mt_use_extra"),
                "Merge an extra distance file (temporal / ecological / categorical)",
                value = FALSE),
              conditionalPanel(
                condition = sprintf("input['%s'] == true", ns("mt_use_extra")),
                fluidRow(
                  column(6,
                    fileInput(ns("mt_extra_file"), "Extra file (first 2 cols = Pop1, Pop2 IDs):",
                              accept = c(".csv", ".txt", ".tsv")),
                    radioButtons(ns("mt_extra_sep"), "Separator:",
                      choices = c("Comma"=",", "Tab"="\t", "Semicolon"=";"),
                      selected = ",", inline = TRUE),
                    checkboxInput(ns("mt_extra_header"), "File has header row", value = TRUE)
                  ),
                  column(6, uiOutput(ns("mt_extra_file_status")))
                )
              )
            ),

            tags$hr(),
            fluidRow(
              column(6, uiOutput(ns("mt_col_pop1_ui"))),
              column(6, uiOutput(ns("mt_col_pop2_ui")))
            )
          )
        ),

        fluidRow(
          box(width = 12, solidHeader = TRUE, status = "primary",
              title = div(style="background:#FFFFFF;padding:10px;color:#333a43;font-weight:600;",
                          icon("sliders-h"), " Mantel parameters"),
            fluidRow(
              column(3,
                uiOutput(ns("mt_col_x_ui")),
                uiOutput(ns("mt_col_y_ui"))
              ),
              column(3,
                radioButtons(ns("mt_stat"), "Statistic:",
                  choices = c("Pearson r" = "r", "Spearman rho" = "spearman",
                              "Rousset's 1D" = "rousset1d", "Rousset's 2D" = "rousset2d"),
                  selected = "r"),
                conditionalPanel(
                  condition = sprintf("input['%s'] == 'r' || input['%s'] == 'spearman'", ns("mt_stat"), ns("mt_stat")),
                  checkboxInput(ns("mt_log_x"), "ln(transform) X", value = FALSE),
                  uiOutput(ns("mt_double_log_warning"))
                ),
                conditionalPanel(
                  condition = sprintf("input['%s'] == 'rousset1d'", ns("mt_stat")),
                  tags$p(style="color:#777;font-size:11px;", icon("lock"), " X used as-is (raw distance, 1 dimension).")
                ),
                conditionalPanel(
                  condition = sprintf("input['%s'] == 'rousset2d'", ns("mt_stat")),
                  tags$p(style="color:#777;font-size:11px;", icon("lock"), " X automatically ln-transformed (2 dimensions).")
                )
              ),
              column(3,
                radioButtons(ns("mt_p_formula"), "p-value formula:",
                  choices = c("(b+1)/(m+1) \u2014 corrected" = "plus1",
                              "b/m \u2014 plain proportion"   = "plain"),
                  selected = "plus1"),
                numericInput(ns("mt_n_perm"), "Permutations:",
                             value = 10000, min = 99, max = 200000, step = 1000)
              ),
              column(3,
                # textInput(ns("mt_exclude"), "Exclude pairs ('ID1-ID2', comma-sep):", value = ""),
                actionButton(ns("run_mantel"), "Run Mantel Test",
                             icon = icon("random"), class = "btn-action-primary btn-block",
                             style = "font-weight:bold;")
              )
            )
          )
        ),

        fluidRow(
          box(width = 6, solidHeader = TRUE, status = "primary",
              title = div(style="background:#FFFFFF;padding:10px;color:#333a43;font-weight:600;",
                          icon("chart-bar"), " Results"),
            uiOutput(ns("ui_mantel_key_values")),
            uiOutput(ns("ui_mantel_summary")),
            tags$br(),
            downloadButton(ns("dl_mantel_txt"), ".txt", class = "btn-action-secondary btn-sm")
          ),
          box(width = 6, solidHeader = FALSE,
              title = div(style="background:#FFFFFF;padding:10px;color:#333a43;font-weight:600;",
                          icon("table"), " Result summary"),
            DT::DTOutput(ns("dt_mantel_summary")),
            tags$br(),
            downloadButton(ns("dl_mantel_summary_txt"), ".txt", class = "btn-action-secondary btn-sm")
          )
        )

        # fluidRow(
        #   box(width = 6, solidHeader = FALSE,
        #       title = div(style="background:#FFFFFF;padding:10px;color:#333a43;font-weight:600;",
        #                   icon("table"), " Result summary"),
        #     DT::DTOutput(ns("dt_mantel_summary")),
        #     tags$br(),
        #     downloadButton(ns("dl_mantel_summary_txt"), ".txt", class = "btn-action-secondary btn-sm")
        #   ),
        #   box(width = 6, solidHeader = FALSE,
        #       title = div(style="background:#FFFFFF;padding:10px;color:#333a43;font-weight:600;",
        #                   icon("table"), " Null distribution quantiles"),
        #     DT::DTOutput(ns("dt_mantel_quantiles"))
        #   )
        # ),

        # fluidRow(
        #   box(width = 12, solidHeader = FALSE,
        #       title = div(style="background:#FFFFFF;padding:10px;color:#333a43;font-weight:600;",
        #                   icon("table"), " Data used in the last run"),
        #     DT::DTOutput(ns("dt_mantel_data"))
        #   )
        # )
      )
    )
  )
}
