# ui_isolation_by_distance.R
# Isolation by Distance — Geographic distances + Mantel test
# Two tabs:
#   1. Geographic distances (Dgeo + ln(Dgeo) for all population pairs)
#   2. Mantel test (rectangular matrix format, as in RT/Fstat)

isolation_by_distance_UI <- function(id) {
  ns <- NS(id)

  custom_css <- tags$style(HTML("
    @import url('https://fonts.googleapis.com/css2?family=IBM+Plex+Mono:wght@400;500;600&family=IBM+Plex+Sans:wght@300;400;500;600&display=swap');

    .ibd-module * { font-family: 'IBM Plex Sans', sans-serif; }

    /* ── Header ─────────────────────────────────────────────────────── */
    .ibd-header {
      background: linear-gradient(135deg, #0f172a 0%, #1e293b 55%, #0c4a6e 100%);
      border-radius: 10px; padding: 1.2rem 1.6rem; margin-bottom: 1rem;
      position: relative; overflow: hidden;
    }
    .ibd-header::before {
      content: ''; position: absolute; inset: 0;
      background: repeating-linear-gradient(
        -45deg, transparent, transparent 28px,
        rgba(255,255,255,.018) 28px, rgba(255,255,255,.018) 29px);
    }
    .ibd-header-title { font-size:1.05rem; font-weight:600; color:#f1f5f9; letter-spacing:.01em; margin-bottom:.2rem; }
    .ibd-header-sub   { font-size:.75rem; color:#94a3b8; font-family:'IBM Plex Mono',monospace; }
    .ibd-badges { display:flex; gap:6px; margin-top:.5rem; flex-wrap:wrap; }
    .ibd-badge  { display:inline-block; border-radius:20px; padding:2px 10px; font-size:.67rem; font-family:'IBM Plex Mono',monospace; }
    .ibd-badge-green  { background:rgba(74,222,128,.12);  border:1px solid rgba(74,222,128,.3);  color:#4ade80; }
    .ibd-badge-purple { background:rgba(168,85,247,.15);  border:1px solid rgba(168,85,247,.3);  color:#a855f7; }

    /* ── Value boxes ─────────────────────────────────────────────────── */
    .ibd-vbox-row { display:flex; gap:9px; margin-bottom:1rem; flex-wrap:wrap; }
    .ibd-vbox { flex:1; min-width:110px; background:#fff; border:1px solid #e2e8f0; border-radius:9px; padding:.6rem .85rem; display:flex; align-items:center; gap:9px; }
    .ibd-vbox-icon  { width:30px; height:30px; border-radius:7px; display:flex; align-items:center; justify-content:center; font-size:12px; flex-shrink:0; }
    .ibd-vbox-label { font-size:10px; color:#94a3b8; text-transform:uppercase; letter-spacing:.06em; margin-bottom:1px; }
    .ibd-vbox-val   { font-size:18px; font-weight:600; color:#0f172a; line-height:1.1; font-family:'IBM Plex Mono',monospace; }

    /* ── Panels ──────────────────────────────────────────────────────── */
    .ibd-panel { background:#fff; border:1px solid #e2e8f0; border-radius:9px; margin-bottom:.85rem; overflow:hidden; }
    .ibd-panel-head { background:#f8fafc; border-bottom:1px solid #e2e8f0; padding:.55rem .9rem; }
    .ibd-panel-title { font-size:12px; font-weight:600; color:#1e293b; display:flex; align-items:center; gap:6px; flex-wrap:wrap; }
    .ibd-panel-body { padding:.85rem; }

    /* ── Info strips ─────────────────────────────────────────────────── */
    .ibd-info { background:#eff6ff; border:1px solid #bfdbfe; border-radius:7px; padding:.45rem .8rem; font-size:11.5px; color:#1d4ed8; margin-bottom:.85rem; line-height:1.65; }
    .ibd-success { background:#f0fdf4; border:1px solid #86efac; border-radius:7px; padding:.45rem .8rem; font-size:11.5px; color:#166534; margin-bottom:.85rem; line-height:1.65; }

    /* ── Buttons ─────────────────────────────────────────────────────── */
    .ibd-btn-mantel {
      background:linear-gradient(135deg,#7c3aed,#4c1d95) !important;
      border:none !important; color:#fff !important; border-radius:7px !important;
      font-weight:600 !important; font-size:13px !important; padding:7px 22px !important;
      box-shadow:0 2px 8px rgba(124,58,237,.3) !important;
    }
    .ibd-btn-mantel:hover { opacity:.9; }

    /* ── Mantel result ───────────────────────────────────────────────── */
    .ibd-mantel-result {
      background:#faf5ff; border:1px solid #d8b4fe; border-radius:8px;
      padding:.65rem 1rem; font-size:11.5px; color:#3b0764;
      font-family:'IBM Plex Mono',monospace; line-height:1.9;
      margin-top:.75rem;
    }
    .ibd-mantel-result strong { color:#6d28d9; }

    /* ── Matrix table ────────────────────────────────────────────────── */
    .ibd-matrix-wrap { overflow-x:auto; margin-top:.5rem; }
    .ibd-matrix { border-collapse:collapse; font-size:11px; font-family:'IBM Plex Mono',monospace; width:100%; }
    .ibd-matrix th { background:#f8fafc; color:#475569; font-weight:600; padding:4px 9px; border:1px solid #e2e8f0; font-size:10.5px; white-space:nowrap; }
    .ibd-matrix td { padding:4px 9px; border:1px solid #e2e8f0; color:#1e293b; text-align:right; white-space:nowrap; font-size:11px; }
    .ibd-matrix tr:nth-child(even) td { background:#f8fafc; }
    .ibd-matrix .diag  { background:#f1f5f9 !important; color:#94a3b8; text-align:center; }
    .ibd-matrix .upper { color:#cbd5e1; text-align:center; }
    .ibd-matrix .lbl   { font-weight:700; color:#0f172a; text-align:left; white-space:nowrap; }

    /* ── Download row ────────────────────────────────────────────────── */
    .ibd-dl-row { display:flex; gap:6px; flex-wrap:wrap; margin-top:.5rem; }
    .ibd-dl-row .btn { font-size:11px; padding:3px 12px; }

    /* ── DT tweaks ───────────────────────────────────────────────────── */
    .ibd-module .dataTables_wrapper { font-size:12px; }
    .ibd-module table.dataTable thead th {
      background:#f8fafc !important; color:#475569 !important;
      font-family:'IBM Plex Mono',monospace !important;
      font-size:10.5px !important; font-weight:600 !important;
    }
    .ibd-module table.dataTable tbody td {
      font-family:'IBM Plex Mono',monospace !important;
      font-size:11px !important; color:#1e293b !important;
    }
    .ibd-module .nav-tabs > li > a { font-size:12px; font-weight:500; color:#475569; padding:5px 13px; }
    .ibd-module .nav-tabs > li.active > a { color:#0f172a; font-weight:600; }
  "))

  tags$div(class="ibd-module", custom_css,

    # ── Header ─────────────────────────────────────────────────────────────
    tags$div(class="ibd-header",
      tags$div(class="ibd-header-title",
        icon("map-marked-alt"), " Isolation by Distance \u00b7 Geographic Distances \u00b7 Mantel Test"),
      tags$div(class="ibd-header-sub",
        "Haversine great-circle distances \u00b7 Mantel permutation test (Mantel 1967)"),
      tags$div(class="ibd-badges",
        tags$span(class="ibd-badge ibd-badge-green",  "Dgeo \u2014 Haversine (km)"),
        tags$span(class="ibd-badge ibd-badge-purple", "Mantel \u2014 rectangular format")
      )
    ),

    # ── Value boxes ─────────────────────────────────────────────────────────
    tags$div(class="ibd-vbox-row",
      tags$div(class="ibd-vbox",
        tags$div(class="ibd-vbox-icon",style="background:#dcfce7;color:#166534;",icon("map-marker-alt")),
        tags$div(tags$div(class="ibd-vbox-label","Populations"),
                 tags$div(class="ibd-vbox-val",uiOutput(ns("vb_pops"))))),
      tags$div(class="ibd-vbox",
        tags$div(class="ibd-vbox-icon",style="background:#e0f2fe;color:#0369a1;",icon("globe")),
        tags$div(tags$div(class="ibd-vbox-label","GPS pops"),
                 tags$div(class="ibd-vbox-val",uiOutput(ns("vb_gps"))))),
      tags$div(class="ibd-vbox",
        tags$div(class="ibd-vbox-icon",style="background:#fef9c3;color:#854d0e;",icon("project-diagram")),
        tags$div(tags$div(class="ibd-vbox-label","Pairs"),
                 tags$div(class="ibd-vbox-val",uiOutput(ns("vb_pairs")))))
    ),

    # ════════════════════════════════════════════════════════════════════════
    # RESULTS TABS \u2014 2 tabs only
    # ════════════════════════════════════════════════════════════════════════
    tabsetPanel(id = ns("ibd_tabs"), type = "tabs",

      # ── TAB 1: Geographic Distances ───────────────────────────────────────
      tabPanel(title = tagList(icon("globe"), " Geographic Distances"),
               value = "tab_geo", br(),

        tags$div(class="ibd-info",
          icon("info-circle"), " ",
          tags$strong("Geographic distances"), ": Haversine great-circle distance (km) between population GPS centroids. ",
          "Centroids are computed as the mean latitude and longitude of all individuals in each population. ",
          tags$strong("ln(Dgeo)"), " is provided for Rousset (1997) 2D isolation-by-distance regression."
        ),

        # ── Geographic distance matrix ──────────────────────────────────────
        tags$div(class="ibd-panel",
          tags$div(class="ibd-panel-head",
            tags$div(class="ibd-panel-title",
              icon("th"), " Pairwise geographic distance matrix (Haversine, km)")),
          tags$div(class="ibd-panel-body",
            uiOutput(ns("ui_geo_matrix")),
            br(),
            tags$div(class="ibd-info", style="margin-bottom:0;",
              icon("info-circle"), " ",
              "Dgeo = great-circle distance computed from population GPS centroids (mean latitude/longitude). ",
              "Formula: Dgeo = 2R \u00d7 arcsin(\u221a[sin\u00b2(\u0394lat/2) + cos(lat1)\u00d7cos(lat2)\u00d7sin\u00b2(\u0394lon/2)]), R = 6371 km.")),
          tags$div(class="ibd-panel-body",
            DT::DTOutput(ns("dt_geo_long")))),
        br(),

        # ── Download ────────────────────────────────────────────────────────
        tags$div(class="ibd-panel",
          tags$div(class="ibd-panel-head",
            tags$div(class="ibd-panel-title",
              icon("file-download"), " Download geographic distances")),
          tags$div(class="ibd-panel-body",
            tags$div(class="ibd-dl-row",
              downloadButton(ns("dl_geo_csv"), "Download CSV", class = "btn btn-default btn-sm"),
              downloadButton(ns("dl_geo_txt"), "Download TXT", class = "btn btn-default btn-sm"))))
      ),

      # ── TAB 2: Mantel Test ────────────────────────────────────────────────
      tabPanel(title = tagList(icon("chart-line"), " Mantel Test"),
               value = "tab_mantel", br(),

        tags$div(class="ibd-info",
          icon("info-circle"), " ",
          tags$strong("Mantel test (Mantel 1967)"), " assesses the correlation between two distance matrices. ",
          "Significance is tested by permutation of rows/columns of one matrix. ",
          tags$br(), tags$br(),
          tags$strong("Rectangular (column-wise) format"), " (as in RT / Fstat): ",
          "first row = population pair labels (e.g. 'PopA-PopB'), each column = one distance value. ",
          "Multiple rows = multiple loci/replicates. The module averages across rows to obtain one distance per pair, ",
          "then rebuilds a symmetric N\u00d7N matrix."
        ),

        # ── Configuration ───────────────────────────────────────────────────
        tags$div(class="ibd-panel",
          tags$div(class="ibd-panel-head",
            tags$div(class="ibd-panel-title",
              icon("sliders-h"), " Mantel test configuration")),
          tags$div(class="ibd-panel-body",
            fluidRow(
              # ── Matrix 1 (genetic) ────────────────────────────────────────
              column(4,
                tags$div(class="ibd-panel", style="border-color:#e9d5ff;",
                  tags$div(class="ibd-panel-head", style="background:#faf5ff;",
                    tags$div(class="ibd-panel-title", style="color:#6d28d9;",
                      icon("dna"), " Matrix 1 \u2014 Genetic distance")),
                  tags$div(class="ibd-panel-body",
                    fileInput(ns("file_mat1"), "Upload distance file (CSV/TXT):",
                              accept = c(".csv", ".txt", ".tab")),
                    radioButtons(ns("mat1_format"), "Format:",
                      choices = c("Square matrix" = "square",
                                  "Rectangular (column-wise)" = "rectangular"),
                      selected = "rectangular")
                  )
                )
              ),

              # ── Matrix 2 (geographic) ─────────────────────────────────────
              column(4,
                tags$div(class="ibd-panel", style="border-color:#99f6e4;",
                  tags$div(class="ibd-panel-head", style="background:#f0fdfa;",
                    tags$div(class="ibd-panel-title", style="color:#0d9488;",
                      icon("globe"), " Matrix 2 \u2014 Geographic distance")),
                  tags$div(class="ibd-panel-body",
                    radioButtons(ns("mat2_source"), "Source:",
                      choices = c(
                        "Computed (Dgeo, km)"  = "gps_km",
                        "Computed (ln Dgeo)"   = "gps_ln",
                        "Upload file"          = "upload2"
                      ),
                      selected = "gps_km"),
                    conditionalPanel(
                      condition = "input.mat2_source == 'upload2'", ns = ns,
                      fileInput(ns("file_mat2"), "Distance file (CSV/TXT):",
                                accept = c(".csv", ".txt", ".tab")),
                      radioButtons(ns("mat2_format"), "Format:",
                        choices = c("Square matrix" = "square",
                                    "Rectangular (column-wise)" = "rectangular"),
                        selected = "rectangular")
                    )
                  )
                )
              ),

              # ── Test parameters ───────────────────────────────────────────
              column(4,
                tags$div(class="ibd-panel", style="border-color:#fcd34d;",
                  tags$div(class="ibd-panel-head", style="background:#fffbeb;",
                    tags$div(class="ibd-panel-title", style="color:#92400e;",
                      icon("cog"), " Test parameters")),
                  tags$div(class="ibd-panel-body",
                    numericInput(ns("n_perm_mantel"), "Permutations:",
                                 value = 9999, min = 99, max = 99999, step = 1000),
                    selectInput(ns("mantel_method"), "Correlation method:",
                      choices = c("Pearson" = "pearson",
                                  "Spearman" = "spearman"),
                      selected = "pearson"),
                    tags$hr(),
                    actionButton(ns("run_mantel"),
                      label = tagList(icon("play"), tags$strong("  Run Mantel Test")),
                      class = "ibd-btn-mantel btn",
                      width = "100%")
                  )
                )
              )
            )
          )
        ),

        # ── Mantel results ──────────────────────────────────────────────────
        tags$div(class="ibd-panel",
          tags$div(class="ibd-panel-head",
            tags$div(class="ibd-panel-title",
              icon("chart-bar"), " Mantel test results")),
          tags$div(class="ibd-panel-body",
            fluidRow(
              column(3,
                tags$div(class="ibd-vbox",
                  tags$div(class="ibd-vbox-icon",style="background:#f3e8ff;color:#7e22ce;",icon("chart-line")),
                  tags$div(tags$div(class="ibd-vbox-label","Mantel r"),
                           tags$div(class="ibd-vbox-val",uiOutput(ns("vb_mantel_r")))))),
              column(3,
                tags$div(class="ibd-vbox",
                  tags$div(class="ibd-vbox-icon",style="background:#dcfce7;color:#166534;",icon("check-circle")),
                  tags$div(tags$div(class="ibd-vbox-label","P-value"),
                           tags$div(class="ibd-vbox-val",uiOutput(ns("vb_mantel_p")))))),
              column(3,
                tags$div(class="ibd-vbox",
                  tags$div(class="ibd-vbox-icon",style="background:#e0f2fe;color:#0369a1;",icon("hashtag")),
                  tags$div(tags$div(class="ibd-vbox-label","Pairs (n)"),
                           tags$div(class="ibd-vbox-val",uiOutput(ns("vb_mantel_n")))))),
              column(3,
                tags$div(class="ibd-vbox",
                  tags$div(class="ibd-vbox-icon",style="background:#fef9c3;color:#854d0e;",icon("exchange-alt")),
                  tags$div(tags$div(class="ibd-vbox-label","Pops aligned"),
                           tags$div(class="ibd-vbox-val",uiOutput(ns("vb_mantel_pops"))))))
            ),
            uiOutput(ns("ui_mantel_result")),
            br(),
            tags$div(class="ibd-dl-row",
              downloadButton(ns("dl_mantel_csv"), "Download results (CSV)",
                             class = "btn btn-default btn-sm"))
          )
        ),

        # ── Mantel scatter plot ─────────────────────────────────────────────
        tags$div(class="ibd-panel",
          tags$div(class="ibd-panel-head",
            tags$div(class="ibd-panel-title",
              icon("chart-scatter"), " Mantel scatter plot")),
          tags$div(class="ibd-panel-body",
            plotly::plotlyOutput(ns("mantel_plot"), height = "480px")))
      )

    ) # end tabsetPanel
  )   # end tags$div.ibd-module
} 