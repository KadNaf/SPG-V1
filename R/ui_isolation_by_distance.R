# ui_isolation_by_distance.R
# Isolation by Distance — Pairwise genetic/geographic distances + Mantel test
# Two tabs:
#   1. Pairwise distances (FST, FST-ENA, DCSE, DCSE-INA + Dgeo + bootstrap CI)
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
    .ibd-badge-blue   { background:rgba(56,189,248,.15);  border:1px solid rgba(56,189,248,.3);  color:#38bdf8; }
    .ibd-badge-green  { background:rgba(74,222,128,.12);  border:1px solid rgba(74,222,128,.3);  color:#4ade80; }
    .ibd-badge-amber  { background:rgba(251,191,36,.12);  border:1px solid rgba(251,191,36,.3);  color:#fbbf24; }
    .ibd-badge-teal   { background:rgba(20,184,166,.15);  border:1px solid rgba(20,184,166,.3);  color:#2dd4bf; }
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

    /* ── Bootstrap panel ─────────────────────────────────────────────── */
    .ibd-panel-boot { background:#faf5ff; border:1px solid #e9d5ff; border-radius:9px; margin-bottom:.85rem; overflow:hidden; }
    .ibd-panel-boot-head { background:#f3e8ff; border-bottom:1px solid #e9d5ff; padding:.55rem .9rem; }
    .ibd-panel-boot-title { font-size:12px; font-weight:600; color:#4c1d95; display:flex; align-items:center; gap:6px; }

    /* ── Info strips ─────────────────────────────────────────────────── */
    .ibd-info { background:#eff6ff; border:1px solid #bfdbfe; border-radius:7px; padding:.45rem .8rem; font-size:11.5px; color:#1d4ed8; margin-bottom:.85rem; line-height:1.65; }
    .ibd-warn { background:#fffbeb; border:1px solid #fcd34d; border-radius:7px; padding:.45rem .8rem; font-size:11.5px; color:#92400e; margin-bottom:.85rem; line-height:1.65; }
    .ibd-success { background:#f0fdf4; border:1px solid #86efac; border-radius:7px; padding:.45rem .8rem; font-size:11.5px; color:#166534; margin-bottom:.85rem; line-height:1.65; }

    /* ── Locus coding grid — radio buttons ───────────────────────────── */
    .ibd-locus-grid { display:flex; flex-wrap:wrap; gap:8px; margin-top:.5rem; }
    .ibd-locus-item {
      background:#f8fafc; border:1px solid #e2e8f0; border-radius:8px;
      padding:.45rem .7rem; min-width:160px; flex:1;
    }
    .ibd-locus-item .control-label { display:none; }
    .ibd-locus-name {
      font-size:11px; font-weight:700; color:#1e293b;
      font-family:'IBM Plex Mono',monospace; margin-bottom:3px;
    }
    .ibd-locus-item .radio { margin:2px 0; }
    .ibd-locus-item .radio label { font-size:11px; color:#475569; }

    /* ── Buttons ─────────────────────────────────────────────────────── */
    .ibd-btn-run {
      background:linear-gradient(135deg,#0369a1,#0c4a6e) !important;
      border:none !important; color:#fff !important; border-radius:7px !important;
      font-weight:600 !important; font-size:13px !important; padding:7px 22px !important;
      box-shadow:0 2px 8px rgba(3,105,161,.3) !important;
    }
    .ibd-btn-run:hover { opacity:.9; }
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

  dlrow <- function(...) tags$div(class="ibd-dl-row", ...)

  tags$div(class="ibd-module", custom_css,

    # ── Header ─────────────────────────────────────────────────────────────
    tags$div(class="ibd-header",
      tags$div(class="ibd-header-title",
        icon("map-marked-alt"), " Isolation by Distance \u00b7 Pairwise Distances \u00b7 Mantel Test"),
      tags$div(class="ibd-header-sub",
        "FreeNA (Chapuis & Estoup 2007) \u00b7 Haversine geographic distances \u00b7 ",
        "Bootstrap over loci \u00b7 Mantel permutation test (Mantel 1967)"),
      tags$div(class="ibd-badges",
        tags$span(class="ibd-badge ibd-badge-blue",   "EM \u2014 null allele freq."),
        tags$span(class="ibd-badge ibd-badge-teal",   "ENA \u2014 FST corrected"),
        tags$span(class="ibd-badge ibd-badge-green",  "INA \u2014 DCSE corrected"),
        tags$span(class="ibd-badge ibd-badge-amber",  "Bootstrap CI \u2014 loci"),
        tags$span(class="ibd-badge ibd-badge-purple", "Mantel \u2014 rectangular format")
      )
    ),

    # ── Value boxes ─────────────────────────────────────────────────────────
    tags$div(class="ibd-vbox-row",
      tags$div(class="ibd-vbox",
        tags$div(class="ibd-vbox-icon",style="background:#e0f2fe;color:#0369a1;",icon("dna")),
        tags$div(tags$div(class="ibd-vbox-label","Loci"),
                 tags$div(class="ibd-vbox-val",uiOutput(ns("vb_loci"))))),
      tags$div(class="ibd-vbox",
        tags$div(class="ibd-vbox-icon",style="background:#dcfce7;color:#166534;",icon("map-marker-alt")),
        tags$div(tags$div(class="ibd-vbox-label","Populations"),
                 tags$div(class="ibd-vbox-val",uiOutput(ns("vb_pops"))))),
      tags$div(class="ibd-vbox",
        tags$div(class="ibd-vbox-icon",style="background:#f3e8ff;color:#7e22ce;",icon("users")),
        tags$div(tags$div(class="ibd-vbox-label","Individuals"),
                 tags$div(class="ibd-vbox-val",uiOutput(ns("vb_n"))))),
      tags$div(class="ibd-vbox",
        tags$div(class="ibd-vbox-icon",style="background:#fef9c3;color:#854d0e;",icon("percentage")),
        tags$div(tags$div(class="ibd-vbox-label","Avg p_nulls"),
                 tags$div(class="ibd-vbox-val",uiOutput(ns("vb_avg_null"))))),
      tags$div(class="ibd-vbox",
        tags$div(class="ibd-vbox-icon",style="background:#fce7f3;color:#9d174d;",icon("globe")),
        tags$div(tags$div(class="ibd-vbox-label","GPS pops"),
                 tags$div(class="ibd-vbox-val",uiOutput(ns("vb_gps"))))),
      tags$div(class="ibd-vbox",
        tags$div(class="ibd-vbox-icon",style="background:#ccfbf1;color:#0d9488;",icon("chart-bar")),
        tags$div(tags$div(class="ibd-vbox-label","Global FST-ENA"),
                 tags$div(class="ibd-vbox-val",uiOutput(ns("vb_fst_ena")))))
    ),

    # ════════════════════════════════════════════════════════════════════════
    # SETUP PANEL — 3 user choices
    # ════════════════════════════════════════════════════════════════════════
    tags$div(class="ibd-panel",
      tags$div(class="ibd-panel-head",
        tags$div(class="ibd-panel-title",
          icon("sliders-h"), " Setup \u2014 3 parameters to configure")),
      tags$div(class="ibd-panel-body",

        # ── (1) Missing genotype coding per locus ───────────────────────────
        tags$div(class="ibd-warn",
          icon("exclamation-triangle"), " ",
          tags$strong("(1) Missing genotype coding per locus"),
          tags$br(),
          tags$span(style="font-size:11px;",
            tags$strong("000000"), " \u2014 missing coded as absent / PCR failure (recommended \u2014 Chapuis & Estoup 2007).",
            tags$br(),
            tags$strong("999999"), " \u2014 missing coded as null homozygote (only if Genepop file uses this convention)."
          )
        ),
        uiOutput(ns("locus_coding_ui")),

        tags$hr(style="margin:1rem 0;"),

        # ── (2) Bootstrap parameters ────────────────────────────────────────
        tags$strong("(2) Bootstrap parameters (over loci)", style="font-size:12px; color:#1e293b;"),
        tags$br(), tags$br(),
        fluidRow(
          column(4,
            numericInput(ns("nboot"),
              label = "Number of replicates:",
              value = 5000, min = 100, max = 99999, step = 1000)),
          column(4,
            selectInput(ns("ci_level"),
              label = "Confidence interval level:",
              choices = c(
                "99%  (alpha = 0.01)" = "0.01",
                "95%  (alpha = 0.05)" = "0.05",
                "90%  (alpha = 0.10)" = "0.10"
              ),
              selected = "0.05")),
          column(4,
            tags$div(style="margin-top:25px;font-size:11px;color:#64748b;",
              icon("info-circle"), " Bootstrap over loci: vectorised, ~5 000 reps in a few seconds.",
              tags$br(),
              "CI are computed for FST, FST-ENA, DCSE and DCSE-INA pairwise."
            ))
        ),

        tags$hr(style="margin:1rem 0;"),

        # ── (3) Run computation ─────────────────────────────────────────────
        tags$strong("(3) Run all computations + generate output files",
                    style="font-size:12px; color:#1e293b;"),
        tags$br(), tags$br(),
        fluidRow(
          column(4,
            actionButton(ns("run_all"),
              label = tagList(icon("play"), tags$strong("  Compute + Bootstrap + Export")),
              class = "ibd-btn-run btn",
              width = "100%"))
        ),
        br(),
        uiOutput(ns("ui_run_status"))
      )
    ),

    # ════════════════════════════════════════════════════════════════════════
    # OUTPUT FILES PANEL
    # ════════════════════════════════════════════════════════════════════════
    tags$div(class="ibd-panel",
      tags$div(class="ibd-panel-head",
        tags$div(class="ibd-panel-title",
          icon("file-download"), " Output files \u2014 automatically generated after computation")),
      tags$div(class="ibd-panel-body",
        tags$div(class="ibd-info",
          icon("info-circle"), " ",
          "All files are generated automatically when you click Compute above.",
          " Each file includes the method, references, locus coding, and bootstrap parameters."
        ),
        fluidRow(
          column(3,
            tags$div(class="ibd-panel", style="border-color:#bfdbfe;",
              tags$div(class="ibd-panel-head", style="background:#eff6ff;",
                tags$div(class="ibd-panel-title", style="color:#1d4ed8;",
                  icon("file-alt"), " File 1 \u2014 Null allele frequencies")),
              tags$div(class="ibd-panel-body", style="font-size:11px;color:#334155;",
                "p_nulls per locus \u00d7 population",
                tags$br(), "Global weighted mean per locus",
                tags$br(), "Locus coding reminder",
                tags$br(), br(),
                uiOutput(ns("ui_dl_file1"))
              )
            )
          ),
          column(3,
            tags$div(class="ibd-panel", style="border-color:#99f6e4;",
              tags$div(class="ibd-panel-head", style="background:#f0fdfa;",
                tags$div(class="ibd-panel-title", style="color:#0d9488;",
                  icon("chart-bar"), " File 2 \u2014 Global FST & FST-ENA")),
              tags$div(class="ibd-panel-body", style="font-size:11px;color:#334155;",
                "Per locus + multilocus FST / FST-ENA",
                tags$br(), "CI from bootstrap over loci",
                tags$br(), br(),
                uiOutput(ns("ui_dl_file2"))
              )
            )
          ),
          column(3,
            tags$div(class="ibd-panel", style="border-color:#e9d5ff;",
              tags$div(class="ibd-panel-head", style="background:#faf5ff;",
                tags$div(class="ibd-panel-title", style="color:#7c3aed;",
                  icon("table"), " File 3 \u2014 Pairwise long format + Dgeo")),
              tags$div(class="ibd-panel-body", style="font-size:11px;color:#334155;",
                "FST, FST-ENA, DCSE, DCSE-INA",
                tags$br(), "Dgeo (km) + ln(Dgeo)",
                tags$br(), "Bootstrap CI over loci",
                tags$br(), br(),
                uiOutput(ns("ui_dl_file3"))
              )
            )
          ),
          column(3,
            tags$div(class="ibd-panel", style="border-color:#fcd34d;",
              tags$div(class="ibd-panel-head", style="background:#fffbeb;",
                tags$div(class="ibd-panel-title", style="color:#92400e;",
                  icon("th"), " File 4 \u2014 Per-locus half-matrices")),
              tags$div(class="ibd-panel-body", style="font-size:11px;color:#334155;",
                "FST, FST-ENA, DCSE, DCSE-INA",
                tags$br(), "Half-matrix per locus",
                tags$br(), br(),
                uiOutput(ns("ui_dl_file4"))
              )
            )
          )
        )
      )
    ),

    # ════════════════════════════════════════════════════════════════════════
    # RESULTS TABS \u2014 2 tabs only
    # ════════════════════════════════════════════════════════════════════════
    tabsetPanel(id = ns("ibd_tabs"), type = "tabs",

      # ── TAB 1: Pairwise Distances + Geographic + Bootstrap CI ─────────────
      tabPanel(title = tagList(icon("exchange-alt"), " Pairwise Distances"),
               value = "tab_pairwise", br(),

        tags$div(class="ibd-info",
          icon("info-circle"), " ",
          tags$strong("Pairwise genetic distances"), ": FST (Weir 1996) and Cavalli-Sforza & Edwards (1967) chord distance (DCSE). ",
          tags$strong("FST-ENA"), " and ", tags$strong("DCSE-INA"),
          " are corrected for null alleles (Chapuis & Estoup 2007). ",
          tags$strong("Dgeo"), " is the Haversine great-circle distance (km) between population GPS centroids. ",
          "Bootstrap CI over loci are provided for all four statistics."
        ),

        # ── Pairwise long table with Dgeo + bootstrap CI ────────────────────
        tags$div(class="ibd-panel",
          tags$div(class="ibd-panel-head",
            tags$div(class="ibd-panel-title",
              icon("list"), " Pairwise distances \u2014 genetic + geographic + bootstrap CI")),
          tags$div(class="ibd-panel-body",
            DT::DTOutput(ns("dt_pairwise_long")))),
        br(),

        # ── Genetic distance matrices ───────────────────────────────────────
        tags$div(class="ibd-panel",
          tags$div(class="ibd-panel-head",
            tags$div(class="ibd-panel-title",
              icon("th"), " Pairwise genetic distance matrices (lower triangle)")),
          tags$div(class="ibd-panel-body",
            fluidRow(
              column(6,
                radioButtons(ns("gen_mat_display"), "Display:",
                  choices = c("FST-ENA" = "ena", "Raw FST" = "raw",
                              "DCSE-INA" = "ina", "Raw DCSE" = "dc_raw"),
                  selected = "ena", inline = TRUE))),
            uiOutput(ns("ui_gen_matrix")))),
        br(),

        # ── Geographic distance matrix ──────────────────────────────────────
        tags$div(class="ibd-panel",
          tags$div(class="ibd-panel-head",
            tags$div(class="ibd-panel-title",
              icon("globe"), " Pairwise geographic distances (Haversine, km)")),
          tags$div(class="ibd-panel-body",
            uiOutput(ns("ui_geo_matrix")),
            br(),
            tags$div(class="ibd-info", style="margin-bottom:0;",
              icon("info-circle"), " ",
              "Dgeo = great-circle distance computed from population GPS centroids (mean latitude/longitude). ",
              "ln(Dgeo) is provided for Rousset (1997) 2D IBD regression.")),
          tags$div(class="ibd-panel-body",
            DT::DTOutput(ns("dt_geo_long")))),
        br(),

        # ── Bootstrap CI table ──────────────────────────────────────────────
        tags$div(class="ibd-panel",
          tags$div(class="ibd-panel-head",
            tags$div(class="ibd-panel-title",
              icon("random"), " Bootstrap CI over loci \u2014 all pairwise statistics")),
          tags$div(class="ibd-panel-body",
            uiOutput(ns("ui_boot_pairwise"))))
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
                    radioButtons(ns("mat1_source"), "Source:",
                      choices = c(
                        "Computed (FST-ENA)"     = "tab1_fst_ena",
                        "Computed (Raw FST)"     = "tab1_fst_raw",
                        "Computed (DCSE-INA)"    = "tab1_dc_ina",
                        "Computed (Raw DCSE)"    = "tab1_dc_raw",
                        "Upload file"            = "upload1"
                      ),
                      selected = "tab1_fst_ena"),
                    conditionalPanel(
                      condition = "input.mat1_source == 'upload1'", ns = ns,
                      fileInput(ns("file_mat1"), "Distance file (CSV/TXT):",
                                accept = c(".csv", ".txt", ".tab")),
                      radioButtons(ns("mat1_format"), "Format:",
                        choices = c("Square matrix" = "square",
                                    "Rectangular (column-wise)" = "rectangular"),
                        selected = "rectangular")
                    )
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