# ui_null_alleles.R - Version sans random seed

null_alleles_UI <- function(id) {
  ns <- NS(id)

  unified_css <- tags$style(HTML("
    @import url('https://fonts.googleapis.com/css2?family=IBM+Plex+Mono:wght@400;500;600&family=IBM+Plex+Sans:wght@300;400;500;600&display=swap');

    .na-module * { font-family: 'IBM Plex Sans', sans-serif; }
    .na-module .mono { font-family: 'IBM Plex Mono', monospace; }

    .na-header {
      background: linear-gradient(135deg, #0f172a 0%, #1e293b 55%, #0c4a6e 100%);
      border-radius: 10px; padding: 1.2rem 1.6rem; margin-bottom: 1rem;
      position: relative; overflow: hidden;
    }
    .na-header::before {
      content: ''; position: absolute; inset: 0;
      background: repeating-linear-gradient(
        -45deg, transparent, transparent 28px,
        rgba(255,255,255,.018) 28px, rgba(255,255,255,.018) 29px);
    }
    .na-header-title {
      font-size: 1.1rem; font-weight: 600; color: #f1f5f9;
      letter-spacing: .01em; margin-bottom: .2rem;
    }
    .na-header-sub {
      font-size: .76rem; color: #94a3b8;
      font-family: 'IBM Plex Mono', monospace;
    }
    .na-badges { display: flex; gap: 6px; margin-top: .5rem; flex-wrap: wrap; }
    .na-badge {
      display: inline-block; border-radius: 20px;
      padding: 2px 10px; font-size: .68rem;
      font-family: 'IBM Plex Mono', monospace;
    }
    .na-badge-blue  { background:rgba(56,189,248,.15); border:1px solid rgba(56,189,248,.3); color:#38bdf8; }
    .na-badge-green { background:rgba(74,222,128,.12); border:1px solid rgba(74,222,128,.3); color:#4ade80; }
    .na-badge-amber { background:rgba(251,191,36,.12); border:1px solid rgba(251,191,36,.3); color:#fbbf24; }
    .na-badge-teal  { background:rgba(20,184,166,.15); border:1px solid rgba(20,184,166,.3); color:#2dd4bf; }
    .na-badge-purple{ background:rgba(168,85,247,.15); border:1px solid rgba(168,85,247,.3); color:#a855f7; }

    .na-vbox-row { display: flex; gap: 9px; margin-bottom: 1rem; flex-wrap: wrap; }
    .na-vbox {
      flex: 1; min-width: 120px; background: #fff;
      border: 1px solid #e2e8f0; border-radius: 9px;
      padding: .65rem .9rem; display: flex; align-items: center; gap: 9px;
    }
    .na-vbox-icon {
      width: 32px; height: 32px; border-radius: 7px;
      display: flex; align-items: center; justify-content: center;
      font-size: 13px; flex-shrink: 0;
    }
    .na-vbox-label {
      font-size: 10px; color: #94a3b8; text-transform: uppercase;
      letter-spacing: .06em; margin-bottom: 1px;
    }
    .na-vbox-val {
      font-size: 19px; font-weight: 600; color: #0f172a; line-height: 1.1;
      font-family: 'IBM Plex Mono', monospace;
    }

    .na-btn {
      background: linear-gradient(135deg, #0369a1, #0c4a6e) !important;
      border: none !important; color: #fff !important;
      border-radius: 7px !important; font-weight: 600 !important;
      font-size: 13px !important; padding: 7px 20px !important;
      box-shadow: 0 2px 8px rgba(3,105,161,.3) !important;
      transition: transform .15s, box-shadow .15s;
    }
    .na-btn:hover {
      transform: translateY(-1px);
      box-shadow: 0 4px 14px rgba(3,105,161,.45) !important;
    }
    .na-btn-boot {
      background: linear-gradient(135deg, #7c3aed, #5b21b6) !important;
      box-shadow: 0 2px 8px rgba(124,58,237,.3) !important;
    }
    .na-btn-boot:hover {
      box-shadow: 0 4px 14px rgba(124,58,237,.45) !important;
    }

    .na-panel {
      background: #fff; border: 1px solid #e2e8f0;
      border-radius: 9px; margin-bottom: .9rem; overflow: hidden;
    }
    .na-panel-head {
      background: #f8fafc; border-bottom: 1px solid #e2e8f0;
      padding: .6rem .95rem; display: flex; align-items: center; flex-wrap: wrap;
    }
    .na-panel-title {
      font-size: 12.5px; font-weight: 600; color: #1e293b;
      display: flex; align-items: center; gap: 6px; flex-wrap: wrap;
    }
    .na-panel-body { padding: .9rem; }

    .na-info {
      background: #eff6ff; border: 1px solid #bfdbfe; border-radius: 7px;
      padding: .5rem .85rem; font-size: 11.5px; color: #1d4ed8;
      display: flex; align-items: flex-start; gap: 7px;
      margin-bottom: .9rem; line-height: 1.7;
    }
    .na-warn {
      background: #fffbeb; border: 1px solid #fcd34d; border-radius: 7px;
      padding: .5rem .85rem; font-size: 11.5px; color: #92400e;
      display: flex; align-items: flex-start; gap: 7px;
      margin-bottom: .9rem; line-height: 1.7;
    }
    .na-success {
      background: #ecfdf5; border: 1px solid #6ee7b7; border-radius: 7px;
      padding: .5rem .85rem; font-size: 11.5px; color: #065f46;
      display: flex; align-items: flex-start; gap: 7px;
      margin-bottom: .9rem; line-height: 1.7;
    }
    .na-formula {
      background: #fafaf9; border: 1px solid #d6d3d1; border-radius: 7px;
      padding: .55rem .85rem; font-size: 11px; color: #292524;
      font-family: 'IBM Plex Mono', monospace;
      margin-bottom: .9rem; line-height: 1.8;
    }

    .na-treat-grid {
      display: flex; flex-wrap: wrap; gap: 8px; margin-top: .4rem;
    }
    .na-treat-item {
      background: #f8fafc; border: 1px solid #e2e8f0;
      border-radius: 8px; padding: .5rem .75rem;
      min-width: 175px; flex: 1;
    }
    .na-treat-lbl {
      font-size: 11px; font-weight: 700; color: #1e293b;
      font-family: 'IBM Plex Mono', monospace; margin-bottom: 4px;
    }

    .boot-controls {
      background: #f5f3ff; border: 1px solid #c4b5fd; border-radius: 9px;
      padding: 1rem; margin-bottom: 1rem;
    }
    .boot-stat {
      background: #fff; border-radius: 7px; padding: 0.5rem 0.75rem;
      border-left: 3px solid #7c3aed;
    }
    .boot-ci {
      font-family: 'IBM Plex Mono', monospace;
      font-size: 12px;
    }

    .na-compare-grid { display: flex; gap: 10px; margin-bottom: .9rem; flex-wrap: wrap; }
    .na-compare-card {
      flex: 1; min-width: 200px; border-radius: 9px;
      border: 1px solid #e2e8f0; overflow: hidden;
    }
    .na-compare-head {
      padding: .5rem .8rem; font-size: 11.5px; font-weight: 700;
      color: #fff; display: flex; align-items: center; gap: 6px;
    }
    .na-compare-head-uncorr { background: #475569; }
    .na-compare-head-corr   { background: #0d9488; }
    .na-compare-body { padding: .6rem .8rem; background: #fff; font-size: 11px; color: #334155; line-height: 1.7; }

    .na-matrix-wrap { overflow-x: auto; }
    .na-matrix {
      border-collapse: collapse; font-size: 11.5px;
      font-family: 'IBM Plex Mono', monospace; width: 100%;
    }
    .na-matrix th {
      background: #f8fafc; color: #475569; font-weight: 600;
      padding: 4px 10px; border: 1px solid #e2e8f0;
      font-size: 11px; white-space: nowrap;
    }
    .na-matrix td {
      padding: 4px 10px; border: 1px solid #e2e8f0;
      color: #1e293b; text-align: right; white-space: nowrap;
    }
    .na-matrix tr:nth-child(even) td { background: #f8fafc; }
    .na-matrix .diag { background: #f1f5f9 !important; color: #94a3b8; }
    .na-matrix .pop-label { font-weight: 700; color: #0f172a; text-align: left; }

    .boot-progress {
      margin-top: 0.5rem;
      margin-bottom: 0.5rem;
    }

    .na-export {
      display: flex; align-items: center; gap: 6px;
      padding-top: .55rem; border-top: 1px solid #f1f5f9; margin-top: .55rem;
    }
    .na-export-lbl { font-size: 11px; color: #94a3b8; }

    .na-module .dataTables_wrapper { font-size: 12px; }
    .na-module table.dataTable thead th {
      background: #f8fafc !important; color: #475569 !important;
      font-family: 'IBM Plex Mono', monospace !important;
      font-size: 11px !important; font-weight: 600 !important;
      letter-spacing: .03em !important;
    }
    .na-module table.dataTable tbody td {
      font-family: 'IBM Plex Mono', monospace !important;
      font-size: 11.5px !important; color: #1e293b !important;
    }
    .na-module .nav-tabs > li > a {
      font-size: 12px; font-weight: 500; color: #475569;
      border-radius: 6px 6px 0 0; padding: 5px 14px;
    }
    .na-module .nav-tabs > li.active > a { color: #0f172a; font-weight: 600; }
  "))

  xbtn <- function(csv_id, txt_id)
    tags$div(class = "na-export",
      tags$span(class = "na-export-lbl", "Export:"),
      downloadButton(ns(csv_id), "CSV", class = "btn btn-default btn-xs", style = "padding:2px 10px; font-size:11px;"),
      downloadButton(ns(txt_id), "TXT", class = "btn btn-default btn-xs", style = "padding:2px 10px; font-size:11px;"))

  tags$div(class = "na-module",
    unified_css,

    tags$div(class = "na-header",
      tags$div(class = "na-header-title",
        icon("atom"), icon("project-diagram"), " Null Allele Frequency & FST/DCSE Correction with Bootstrap"),
      tags$div(class = "na-header-sub",
        "EM algorithm · FreeNA · ENA/INA · Weir (1996) · Cavalli-Sforza & Edwards (1967) · Bootstrap over loci"),
      tags$div(class = "na-badges",
        tags$span(class = "na-badge na-badge-blue", "EM algorithm — null allele frequency"),
        tags$span(class = "na-badge na-badge-teal", "FST-ENA — corrected Fst"),
        tags$span(class = "na-badge na-badge-green", "DCSE-INA — corrected distance"),
        tags$span(class = "na-badge na-badge-purple", "Bootstrap — 95% CI"),
        tags$span(class = "na-badge na-badge-amber", "999999 → null · 000000 → absent")
      )
    ),

    tags$div(class = "na-vbox-row",
      tags$div(class = "na-vbox",
        tags$div(class = "na-vbox-icon", style = "background:#e0f2fe; color:#0369a1;", icon("dna")),
        tags$div(tags$div(class = "na-vbox-label", "Loci"), tags$div(class = "na-vbox-val", uiOutput(ns("vb_loci"))))
      ),
      tags$div(class = "na-vbox",
        tags$div(class = "na-vbox-icon", style = "background:#dcfce7; color:#166534;", icon("map-marker-alt")),
        tags$div(tags$div(class = "na-vbox-label", "Populations"), tags$div(class = "na-vbox-val", uiOutput(ns("vb_pops"))))
      ),
      tags$div(class = "na-vbox",
        tags$div(class = "na-vbox-icon", style = "background:#f3e8ff; color:#7e22ce;", icon("users")),
        tags$div(tags$div(class = "na-vbox-label", "Individuals"), tags$div(class = "na-vbox-val", uiOutput(ns("vb_n"))))
      ),
      tags$div(class = "na-vbox",
        tags$div(class = "na-vbox-icon", style = "background:#fef9c3; color:#854d0e;", icon("percentage")),
        tags$div(tags$div(class = "na-vbox-label", "Avg p_nulls"), tags$div(class = "na-vbox-val", uiOutput(ns("vb_avg_null"))))
      ),
      tags$div(class = "na-vbox",
        tags$div(class = "na-vbox-icon", style = "background:#ccfbf1; color:#0d9488;", icon("chart-bar")),
        tags$div(tags$div(class = "na-vbox-label", "Global FST-ENA"), tags$div(class = "na-vbox-val", uiOutput(ns("vb_fst_ena"))))
      )
    ),

    tags$div(class = "na-panel",
      tags$div(class = "na-panel-head",
        tags$div(class = "na-panel-title", icon("cog"), " Missing genotype coding — per locus",
          tags$span(style = "font-size:10.5px; color:#64748b; margin-left:8px; font-weight:400;",
            "(must match the original Genepop file coding for each locus)")
        )
      ),
      tags$div(class = "na-panel-body",
        tags$div(class = "na-warn",
          icon("exclamation-triangle"),
          tags$div(
            tags$strong("Select the coding used for missing genotypes in the original Genepop file, for each locus:"),
            tags$br(),
            tags$strong("999999"), " — missing coded as null homozygote → higher p_nulls.",
            tags$br(),
            tags$strong("000000"), " — missing coded as absent / PCR failure → lower p_nulls."
          )
        ),
        uiOutput(ns("locus_treatment_ui"))
      )
    ),

    tabsetPanel(
      id = ns("na_tabs"), type = "tabs",

      tabPanel(title = tagList(icon("table"), "1. Per locus × population"), value = "tab_per", br(),
        tags$div(class = "na-info", icon("info-circle"), tags$div("Null allele frequency estimated by EM per locus × population.")),
        tags$div(class = "na-panel",
          tags$div(class = "na-panel-head", tags$div(class = "na-panel-title", icon("sliders-h"), " Filter parameters")),
          tags$div(class = "na-panel-body",
            fluidRow(
              column(3, selectInput(ns("t1_locus"), "Locus:", choices = c("All loci" = "all"))),
              column(3, selectInput(ns("t1_pop"), "Population:", choices = c("All populations" = "all"))),
              column(3, tags$div(style = "margin-top:25px;", actionButton(ns("run_t1"), label = tagList(icon("play"), tags$strong(" Compute")), class = "na-btn btn")))
            )
          )
        ),
        tags$div(class = "na-panel",
          tags$div(class = "na-panel-head", tags$div(class = "na-panel-title", icon("list"), " EM estimates")),
          tags$div(class = "na-panel-body", DT::DTOutput(ns("dt_t1")), xbtn("dl_t1_csv", "dl_t1_txt"))
        )
      ),

      tabPanel(title = tagList(icon("globe"), "2. Global summary per locus"), value = "tab_global", br(),
        tags$div(class = "na-info", icon("info-circle"), tags$div("Global summary across all populations per locus.")),
        tags$div(class = "na-panel",
          tags$div(class = "na-panel-head", tags$div(class = "na-panel-title", icon("sliders-h"), " Filter parameters")),
          tags$div(class = "na-panel-body",
            fluidRow(
              column(3, selectInput(ns("t2_locus"), "Locus:", choices = c("All loci" = "all"))),
              column(3, tags$div(style = "margin-top:25px;", actionButton(ns("run_t2"), label = tagList(icon("play"), tags$strong(" Compute")), class = "na-btn btn")))
            )
          )
        ),
        tags$div(class = "na-panel",
          tags$div(class = "na-panel-head", tags$div(class = "na-panel-title", icon("globe"), " Global summary")),
          tags$div(class = "na-panel-body", DT::DTOutput(ns("dt_t2")), xbtn("dl_t2_csv", "dl_t2_txt"))
        )
      ),

      tabPanel(title = tagList(icon("globe"), "3. Global FST (multilocus)"), value = "tab_fst_global", br(),
        tags$div(class = "na-info", icon("info-circle"), tags$div("Global FST — Weir (1996). Raw vs ENA corrected.")),
        tags$div(class = "na-panel",
          tags$div(class = "na-panel-head", tags$div(class = "na-panel-title", icon("sliders-h"), " Parameters")),
          tags$div(class = "na-panel-body",
            fluidRow(column(4, tags$div(style = "margin-top:25px;", actionButton(ns("run_fst_global"), label = tagList(icon("play"), tags$strong(" Calculate")), class = "na-btn btn"))))
          )
        ),
        tags$div(class = "na-compare-grid",
          tags$div(class = "na-compare-card", tags$div(class = "na-compare-head na-compare-head-uncorr", icon("table"), " Raw FST"), tags$div(class = "na-compare-body", "Observed frequencies. Biased by null alleles.")),
          tags$div(class = "na-compare-card", tags$div(class = "na-compare-head na-compare-head-corr", icon("check-circle"), " FST-ENA"), tags$div(class = "na-compare-body", "Frequencies corrected by EM."))
        ),
        tags$div(class = "na-panel",
          tags$div(class = "na-panel-head", tags$div(class = "na-panel-title", icon("list"), " Global FST per locus")),
          tags$div(class = "na-panel-body", DT::DTOutput(ns("dt_fst_global")), xbtn("dl_fst_global_csv", "dl_fst_global_txt"))
        )
      ),

      tabPanel(title = tagList(icon("exchange-alt"), "4. Pairwise FST"), value = "tab_fst_pair", br(),
        tags$div(class = "na-info", icon("info-circle"), tags$div("Pairwise FST — lower triangle.")),
        tags$div(class = "na-panel",
          tags$div(class = "na-panel-head", tags$div(class = "na-panel-title", icon("sliders-h"), " Parameters")),
          tags$div(class = "na-panel-body",
            fluidRow(
              column(4, radioButtons(ns("fst_pair_type"), "Display:", choices = c("Raw FST" = "raw", "FST-ENA" = "ena", "Both" = "both"), selected = "both", inline = FALSE)),
              column(3, tags$div(style = "margin-top:25px;", actionButton(ns("run_fst_pair"), label = tagList(icon("play"), tags$strong(" Calculate")), class = "na-btn btn")))
            )
          )
        ),
        tags$div(class = "na-panel",
          tags$div(class = "na-panel-head", tags$div(class = "na-panel-title", icon("th"), " Pairwise FST matrix")),
          tags$div(class = "na-panel-body", uiOutput(ns("ui_fst_pair_matrix")), xbtn("dl_fst_pair_csv", "dl_fst_pair_txt"))
        ),
        tags$div(class = "na-panel",
          tags$div(class = "na-panel-head", tags$div(class = "na-panel-title", icon("list"), " Long format")),
          tags$div(class = "na-panel-body", DT::DTOutput(ns("dt_fst_pair")), xbtn("dl_fst_pair_long_csv", "dl_fst_pair_long_txt"))
        )
      ),

      tabPanel(title = tagList(icon("ruler-combined"), "5. Pairwise DCSE distance"), value = "tab_dc", br(),
        tags$div(class = "na-info", icon("info-circle"), tags$div("Cavalli-Sforza & Edwards distance — Raw vs INA corrected.")),
        tags$div(class = "na-panel",
          tags$div(class = "na-panel-head", tags$div(class = "na-panel-title", icon("sliders-h"), " Parameters")),
          tags$div(class = "na-panel-body",
            fluidRow(
              column(4, radioButtons(ns("dc_type"), "Display:", choices = c("Raw DCSE" = "raw", "DCSE-INA" = "ina", "Both" = "both"), selected = "both", inline = FALSE)),
              column(3, tags$div(style = "margin-top:25px;", actionButton(ns("run_dc"), label = tagList(icon("play"), tags$strong(" Calculate")), class = "na-btn btn")))
            )
          )
        ),
        tags$div(class = "na-panel",
          tags$div(class = "na-panel-head", tags$div(class = "na-panel-title", icon("th"), " Pairwise DCSE matrix")),
          tags$div(class = "na-panel-body", uiOutput(ns("ui_dc_matrix")), xbtn("dl_dc_csv", "dl_dc_txt"))
        ),
        tags$div(class = "na-panel",
          tags$div(class = "na-panel-head", tags$div(class = "na-panel-title", icon("list"), " Long format")),
          tags$div(class = "na-panel-body", DT::DTOutput(ns("dt_dc")), xbtn("dl_dc_long_csv", "dl_dc_long_txt"))
        )
      ),

      tabPanel(title = tagList(icon("table"), "6. FST per locus × pair"), value = "tab_fst_locus", br(),
        tags$div(class = "na-info", icon("info-circle"), tags$div("Per-locus FST for each population pair.")),
        tags$div(class = "na-panel",
          tags$div(class = "na-panel-head", tags$div(class = "na-panel-title", icon("sliders-h"), " Filters")),
          tags$div(class = "na-panel-body",
            fluidRow(
              column(3, selectInput(ns("fl_locus"), "Locus:", choices = c("All loci" = "all"))),
              column(3, selectInput(ns("fl_pop1"), "Population 1:", choices = c("All pairs" = "all"))),
              column(3, selectInput(ns("fl_pop2"), "Population 2:", choices = c("All pairs" = "all"))),
              column(3, tags$div(style = "margin-top:25px;", actionButton(ns("run_fst_locus"), label = tagList(icon("play"), tags$strong(" Calculate")), class = "na-btn btn")))
            )
          )
        ),
        tags$div(class = "na-panel",
          tags$div(class = "na-panel-head", tags$div(class = "na-panel-title", icon("list"), " FST per locus × pair")),
          tags$div(class = "na-panel-body", DT::DTOutput(ns("dt_fst_locus")), xbtn("dl_fst_locus_csv", "dl_fst_locus_txt"))
        )
      ),

      # ONGLET 7 — BOOTSTRAP (sans seed)
      tabPanel(
        title = tagList(icon("random"), "7. Bootstrap — 95% CI"),
        value = "tab_bootstrap",
        br(),

        tags$div(class = "na-info",
          icon("info-circle"),
          tags$div(
            tags$strong("Bootstrap resampling over loci"),
            " to compute 95% confidence intervals for FST and DCSE statistics.",
            tags$br(),
            "Method: non-parametric bootstrap with replacement over loci (Chapuis & Estoup 2007).",
            tags$br(),
            tags$strong("Optimized for speed"), " — 5000 replicates in seconds, up to 50000 possible."
          )
        ),

        tags$div(class = "boot-controls",
          fluidRow(
            column(4,
              numericInput(ns("boot_nperm"), 
                label = tagList(icon("repeat"), " Number of bootstrap replicates:"),
                value = 5000, min = 500, max = 50000, step = 500)
            ),
            column(4,
              tags$div(style = "margin-top:25px;",
                actionButton(ns("run_bootstrap"),
                  label = tagList(icon("play"), icon("random"), tags$strong(" Run Bootstrap")),
                  class = "na-btn na-btn-boot btn")
              )
            ),
            column(4,
              tags$div(style = "margin-top:25px;",
                downloadButton(ns("dl_bootstrap_results"),
                  label = tagList(icon("download"), " Download all results"),
                  class = "btn btn-default",
                  style = "background:#f3e8ff; border:1px solid #c4b5fd;")
              )
            )
          ),
          tags$div(class = "boot-progress", uiOutput(ns("boot_progress_ui")))
        ),

        tags$div(class = "na-panel",
          tags$div(class = "na-panel-head", tags$div(class = "na-panel-title", icon("chart-line"), " Bootstrap Summary — Global Statistics")),
          tags$div(class = "na-panel-body",
            fluidRow(
              column(6, tags$div(class = "boot-stat", tags$strong(icon("chart-bar"), " Global FST (raw)"), uiOutput(ns("boot_ci_fst_raw")))),
              column(6, tags$div(class = "boot-stat", tags$strong(icon("check-circle"), " Global FST-ENA"), uiOutput(ns("boot_ci_fst_ena"))))
            ),
            fluidRow(
              column(6, tags$div(class = "boot-stat", tags$strong(icon("ruler"), " Global DCSE (raw)"), uiOutput(ns("boot_ci_dc_raw")))),
              column(6, tags$div(class = "boot-stat", tags$strong(icon("ruler-combined"), " Global DCSE-INA"), uiOutput(ns("boot_ci_dc_ina"))))
            )
          )
        ),

        tags$div(class = "na-panel",
          tags$div(class = "na-panel-head", tags$div(class = "na-panel-title", icon("table"), " Pairwise FST — 95% CI")),
          tags$div(class = "na-panel-body", DT::DTOutput(ns("dt_boot_fst_pair")), xbtn("dl_boot_fst_pair_csv", "dl_boot_fst_pair_txt"))
        ),

        tags$div(class = "na-panel",
          tags$div(class = "na-panel-head", tags$div(class = "na-panel-title", icon("ruler-combined"), " Pairwise DCSE — 95% CI")),
          tags$div(class = "na-panel-body", DT::DTOutput(ns("dt_boot_dc_pair")), xbtn("dl_boot_dc_pair_csv", "dl_boot_dc_pair_txt"))
        ),

        tags$div(class = "na-panel",
          tags$div(class = "na-panel-head", tags$div(class = "na-panel-title", icon("list"), " Per locus FST — Bootstrap distribution")),
          tags$div(class = "na-panel-body", DT::DTOutput(ns("dt_boot_per_locus")), xbtn("dl_boot_per_locus_csv", "dl_boot_per_locus_txt"))
        ),

        tags$div(class = "na-panel",
          tags$div(class = "na-panel-head", tags$div(class = "na-panel-title", icon("bug"), " Bootstrap Diagnostics")),
          tags$div(class = "na-panel-body", uiOutput(ns("boot_diagnostics")))
        )
      )
    )
  )
}