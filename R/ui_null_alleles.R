# module/ui_null_alleles.R
# Module unifié : Null Allele Frequency Estimation + FST-ENA / DCSE-INA correction
# 6 onglets : 2 pour EM (p_nulls) + 4 pour FST-ENA / DCSE-INA

null_alleles_UI <- function(id) {
  ns <- NS(id)

  custom_css <- tags$style(HTML("
    @import url('https://fonts.googleapis.com/css2?family=IBM+Plex+Mono:wght@400;500;600&family=IBM+Plex+Sans:wght@300;400;500;600&display=swap');

    .fna-module * { font-family: 'IBM Plex Sans', sans-serif; }
    .fna-module .mono { font-family: 'IBM Plex Mono', monospace; }

    /* ── Header ─────────────────────────────────────────── */
    .fna-header {
      background: linear-gradient(135deg, #0f172a 0%, #1e293b 45%, #064e3b 100%);
      border-radius: 10px; padding: 1.2rem 1.6rem; margin-bottom: 1rem;
      position: relative; overflow: hidden;
    }
    .fna-header::before {
      content: ''; position: absolute; inset: 0;
      background: repeating-linear-gradient(
        -45deg, transparent, transparent 28px,
        rgba(255,255,255,.018) 28px, rgba(255,255,255,.018) 29px);
    }
    .fna-header-title {
      font-size: 1.1rem; font-weight: 600; color: #f1f5f9;
      letter-spacing: .01em; margin-bottom: .2rem;
    }
    .fna-header-sub {
      font-size: .76rem; color: #94a3b8;
      font-family: 'IBM Plex Mono', monospace;
    }
    .fna-badges { display: flex; gap: 6px; margin-top: .5rem; flex-wrap: wrap; }
    .fna-badge {
      display: inline-block; border-radius: 20px;
      padding: 2px 10px; font-size: .68rem;
      font-family: 'IBM Plex Mono', monospace;
    }
    .fna-badge-blue  { background:rgba(56,189,248,.15); border:1px solid rgba(56,189,248,.3); color:#38bdf8; }
    .fna-badge-green { background:rgba(74,222,128,.12); border:1px solid rgba(74,222,128,.3); color:#4ade80; }
    .fna-badge-amber { background:rgba(251,191,36,.12); border:1px solid rgba(251,191,36,.3); color:#fbbf24; }
    .fna-badge-teal  { background:rgba(20,184,166,.15); border:1px solid rgba(20,184,166,.3); color:#2dd4bf; }

    /* ── Section dividers ──────────────────────────────── */
    .fna-section-label {
      font-size: 11px; font-weight: 700; color: #64748b;
      text-transform: uppercase; letter-spacing: .08em;
      margin: .4rem 0 .5rem 0; padding-bottom: 4px;
      border-bottom: 1px solid #e2e8f0;
      display: flex; align-items: center; gap: 6px;
    }
    .fna-section-label .dot {
      width: 7px; height: 7px; border-radius: 50%;
      display: inline-block;
    }
    .fna-section-label .dot-blue { background: #0369a1; }
    .fna-section-label .dot-teal { background: #0d9488; }

    /* ── Value boxes ─────────────────────────────────── */
    .fna-vbox-row { display: flex; gap: 9px; margin-bottom: .7rem; flex-wrap: wrap; }
    .fna-vbox {
      flex: 1; min-width: 120px; background: #fff;
      border: 1px solid #e2e8f0; border-radius: 9px;
      padding: .65rem .9rem; display: flex; align-items: center; gap: 9px;
    }
    .fna-vbox-icon {
      width: 32px; height: 32px; border-radius: 7px;
      display: flex; align-items: center; justify-content: center;
      font-size: 13px; flex-shrink: 0;
    }
    .fna-vbox-label {
      font-size: 10px; color: #94a3b8; text-transform: uppercase;
      letter-spacing: .06em; margin-bottom: 1px;
    }
    .fna-vbox-val {
      font-size: 19px; font-weight: 600; color: #0f172a; line-height: 1.1;
      font-family: 'IBM Plex Mono', monospace;
    }

    /* ── Buttons ─────────────────────────────────────── */
    .fna-btn {
      background: linear-gradient(135deg, #0369a1, #064e3b) !important;
      border: none !important; color: #fff !important;
      border-radius: 7px !important; font-weight: 600 !important;
      font-size: 13px !important; padding: 7px 20px !important;
      box-shadow: 0 2px 8px rgba(3,105,161,.3) !important;
      transition: transform .15s, box-shadow .15s;
    }
    .fna-btn:hover {
      transform: translateY(-1px);
      box-shadow: 0 4px 14px rgba(3,105,161,.45) !important;
    }

    /* ── Panels ──────────────────────────────────────── */
    .fna-panel {
      background: #fff; border: 1px solid #e2e8f0;
      border-radius: 9px; margin-bottom: .9rem; overflow: hidden;
    }
    .fna-panel-head {
      background: #f8fafc; border-bottom: 1px solid #e2e8f0;
      padding: .6rem .95rem; display: flex; align-items: center; flex-wrap: wrap;
    }
    .fna-panel-title {
      font-size: 12.5px; font-weight: 600; color: #1e293b;
      display: flex; align-items: center; gap: 6px; flex-wrap: wrap;
    }
    .fna-panel-body { padding: .9rem; }

    /* ── Info / warn / formula strips ───────────────── */
    .fna-info {
      background: #eff6ff; border: 1px solid #bfdbfe; border-radius: 7px;
      padding: .5rem .85rem; font-size: 11.5px; color: #1d4ed8;
      display: flex; align-items: flex-start; gap: 7px;
      margin-bottom: .9rem; line-height: 1.7;
    }
    .fna-info-teal {
      background: #f0fdfa; border: 1px solid #99f6e4; border-radius: 7px;
      padding: .5rem .85rem; font-size: 11.5px; color: #134e4a;
      display: flex; align-items: flex-start; gap: 7px;
      margin-bottom: .9rem; line-height: 1.75;
    }
    .fna-formula {
      background: #fafaf9; border: 1px solid #d6d3d1; border-radius: 7px;
      padding: .55rem .85rem; font-size: 11px; color: #292524;
      font-family: 'IBM Plex Mono', monospace;
      margin-bottom: .9rem; line-height: 1.8;
    }
    .fna-warn {
      background: #fffbeb; border: 1px solid #fcd34d; border-radius: 7px;
      padding: .5rem .85rem; font-size: 11.5px; color: #92400e;
      display: flex; align-items: flex-start; gap: 7px;
      margin-bottom: .9rem; line-height: 1.7;
    }

    /* ── Comparison grid ─────────────────────────────── */
    .fna-compare-grid { display: flex; gap: 10px; margin-bottom: .9rem; flex-wrap: wrap; }
    .fna-compare-card {
      flex: 1; min-width: 200px; border-radius: 9px;
      border: 1px solid #e2e8f0; overflow: hidden;
    }
    .fna-compare-head {
      padding: .5rem .8rem; font-size: 11.5px; font-weight: 700;
      color: #fff; display: flex; align-items: center; gap: 6px;
    }
    .fna-compare-head-uncorr { background: #475569; }
    .fna-compare-head-corr   { background: #0d9488; }
    .fna-compare-body { padding: .6rem .8rem; background: #fff; font-size: 11px; color: #334155; line-height: 1.7; }

    /* ── Matrix table ────────────────────────────────── */
    .fna-matrix-wrap { overflow-x: auto; }
    .fna-matrix {
      border-collapse: collapse; font-size: 11.5px;
      font-family: 'IBM Plex Mono', monospace; width: 100%;
    }
    .fna-matrix th {
      background: #f8fafc; color: #475569; font-weight: 600;
      padding: 4px 10px; border: 1px solid #e2e8f0;
      font-size: 11px; white-space: nowrap;
    }
    .fna-matrix td {
      padding: 4px 10px; border: 1px solid #e2e8f0;
      color: #1e293b; text-align: right; white-space: nowrap;
    }
    .fna-matrix tr:nth-child(even) td { background: #f8fafc; }
    .fna-matrix .diag { background: #f1f5f9 !important; color: #94a3b8; }
    .fna-matrix .pop-label { font-weight: 700; color: #0f172a; text-align: left; }

    /* ── Export row ──────────────────────────────────── */
    .fna-export {
      display: flex; align-items: center; gap: 6px;
      padding-top: .55rem; border-top: 1px solid #f1f5f9; margin-top: .55rem;
    }
    .fna-export-lbl { font-size: 11px; color: #94a3b8; }

    /* ── DT tweaks ───────────────────────────────────── */
    .fna-module .dataTables_wrapper { font-size: 12px; }
    .fna-module table.dataTable thead th {
      background: #f8fafc !important; color: #475569 !important;
      font-family: 'IBM Plex Mono', monospace !important;
      font-size: 11px !important; font-weight: 600 !important;
      letter-spacing: .03em !important;
    }
    .fna-module table.dataTable tbody td {
      font-family: 'IBM Plex Mono', monospace !important;
      font-size: 11.5px !important; color: #1e293b !important;
    }
    .fna-module .nav-tabs > li > a {
      font-size: 12px; font-weight: 500; color: #475569;
      border-radius: 6px 6px 0 0; padding: 5px 14px;
    }
    .fna-module .nav-tabs > li.active > a { color: #0f172a; font-weight: 600; }

    /* ── Locus treatment grid ────────────────────────── */
    .fna-treat-grid {
      display: flex; flex-wrap: wrap; gap: 8px; margin-top: .4rem;
    }
    .fna-treat-item {
      background: #f8fafc; border: 1px solid #e2e8f0;
      border-radius: 8px; padding: .5rem .75rem;
      min-width: 175px; flex: 1;
    }
    .fna-treat-lbl {
      font-size: 11px; font-weight: 700; color: #1e293b;
      font-family: 'IBM Plex Mono', monospace; margin-bottom: 4px;
    }
  "))

  xbtn <- function(csv_id, txt_id)
    tags$div(class = "fna-export",
      tags$span(class = "fna-export-lbl", "Export :"),
      downloadButton(ns(csv_id), "CSV",
        class = "btn btn-default btn-xs",
        style = "padding:2px 10px; font-size:11px;"),
      downloadButton(ns(txt_id), "TXT",
        class = "btn btn-default btn-xs",
        style = "padding:2px 10px; font-size:11px;"))

  tags$div(class = "fna-module",
    custom_css,

    # ── Header ────────────────────────────────────────────────
    tags$div(class = "fna-header",
      tags$div(class = "fna-header-title",
        icon("atom"), " FreeNA Analysis — Null Alleles & FST / Distance Correction"),
      tags$div(class = "fna-header-sub",
        "EM algorithm \u00b7 Dempster, Laird & Rubin (1977)  \u00b7  ENA / INA \u00b7 Chapuis & Estoup (2007)  \u00b7  Weir (1996)  \u00b7  Cavalli-Sforza & Edwards (1967)"),
      tags$div(class = "fna-badges",
        tags$span(class = "fna-badge fna-badge-blue",
          "EM \u2014 p_nulls estimation"),
        tags$span(class = "fna-badge fna-badge-green",
          "999999 \u2192 null homozygote"),
        tags$span(class = "fna-badge fna-badge-amber",
          "000000 \u2192 absent / PCR failure"),
        tags$span(class = "fna-badge fna-badge-teal",
          "ENA \u2014 FST corrected"),
        tags$span(class = "fna-badge fna-badge-teal",
          "INA \u2014 DCSE corrected")
      )
    ),

    # ── Value boxes : données communes ────────────────────────
    tags$div(class = "fna-section-label",
      tags$span(class = "dot dot-blue"), "Dataset summary"),
    tags$div(class = "fna-vbox-row",
      tags$div(class = "fna-vbox",
        tags$div(class = "fna-vbox-icon",
          style = "background:#e0f2fe; color:#0369a1;", icon("dna")),
        tags$div(tags$div(class = "fna-vbox-label", "Loci"),
                 tags$div(class = "fna-vbox-val", uiOutput(ns("vb_loci"))))
      ),
      tags$div(class = "fna-vbox",
        tags$div(class = "fna-vbox-icon",
          style = "background:#dcfce7; color:#166534;", icon("map-marker-alt")),
        tags$div(tags$div(class = "fna-vbox-label", "Populations"),
                 tags$div(class = "fna-vbox-val", uiOutput(ns("vb_pops"))))
      ),
      tags$div(class = "fna-vbox",
        tags$div(class = "fna-vbox-icon",
          style = "background:#f3e8ff; color:#7e22ce;", icon("users")),
        tags$div(tags$div(class = "fna-vbox-label", "Individuals"),
                 tags$div(class = "fna-vbox-val", uiOutput(ns("vb_n"))))
      )
    ),

    # ── Value boxes : Null allele frequencies ─────────────────
    tags$div(class = "fna-section-label",
      tags$span(class = "dot dot-blue"), "Null allele frequencies (EM)"),
    tags$div(class = "fna-vbox-row",
      tags$div(class = "fna-vbox",
        tags$div(class = "fna-vbox-icon",
          style = "background:#fef9c3; color:#854d0e;", icon("percentage")),
        tags$div(tags$div(class = "fna-vbox-label", "Avg p_nulls"),
                 tags$div(class = "fna-vbox-val", uiOutput(ns("vb_avg_null"))))
      ),
      tags$div(class = "fna-vbox",
        tags$div(class = "fna-vbox-icon",
          style = "background:#fce7f3; color:#9d174d;", icon("exclamation-triangle")),
        tags$div(tags$div(class = "fna-vbox-label", "Max p_nulls"),
                 tags$div(class = "fna-vbox-val", uiOutput(ns("vb_max_null"))))
      )
    ),

    # ── Value boxes : FST / DCSE ──────────────────────────────
    tags$div(class = "fna-section-label",
      tags$span(class = "dot dot-teal"), "Differentiation & distance (ENA / INA)"),
    tags$div(class = "fna-vbox-row",
      tags$div(class = "fna-vbox",
        tags$div(class = "fna-vbox-icon",
          style = "background:#fef9c3; color:#854d0e;", icon("chart-bar")),
        tags$div(tags$div(class = "fna-vbox-label", "Raw FST"),
                 tags$div(class = "fna-vbox-val", uiOutput(ns("vb_fst_raw"))))
      ),
      tags$div(class = "fna-vbox",
        tags$div(class = "fna-vbox-icon",
          style = "background:#ccfbf1; color:#0d9488;", icon("chart-bar")),
        tags$div(tags$div(class = "fna-vbox-label", "FST-ENA"),
                 tags$div(class = "fna-vbox-val", uiOutput(ns("vb_fst_ena"))))
      ),
      tags$div(class = "fna-vbox",
        tags$div(class = "fna-vbox-icon",
          style = "background:#e0f2fe; color:#0369a1;", icon("ruler")),
        tags$div(tags$div(class = "fna-vbox-label", "DCSE-INA (mean)"),
                 tags$div(class = "fna-vbox-val", uiOutput(ns("vb_dc_ina"))))
      )
    ),

    # ── Per-locus treatment selector ──────────────────────────
    tags$div(class = "fna-panel",
      tags$div(class = "fna-panel-head",
        tags$div(class = "fna-panel-title",
          icon("cog"), " Missing genotype coding \u2014 per locus",
          tags$span(
            style = "font-size:10.5px; color:#64748b; margin-left:8px; font-weight:400;",
            "(must match the original Genepop file coding for each locus)")
        )
      ),
      tags$div(class = "fna-panel-body",
        tags$div(class = "fna-warn",
          icon("exclamation-triangle"),
          tags$div(
            tags$strong("Select the coding used for missing genotypes in the original Genepop file, for each locus:"),
            tags$br(),
            tags$strong("999999"), " \u2014 missing coded as null homozygote \u2192 higher p_nulls (inferred from excess homozygosity).",
            tags$br(),
            tags$strong("000000"), " \u2014 missing coded as absent / PCR failure \u2192 lower p_nulls (no null allele signal from missing data)."
          )
        ),
        uiOutput(ns("locus_treatment_ui"))
      )
    ),

    # ── Main tabs (6 onglets) ─────────────────────────────────
    tabsetPanel(
      id = ns("fna_tabs"), type = "tabs",

      # ════════════════════════════════════════════════════ #
      # TAB 1 — Per locus × population (EM p_nulls)         #
      # ════════════════════════════════════════════════════ #
      tabPanel(
        title = tagList(icon("table"), " Per locus \u00d7 pop"),
        value = "tab_per",
        br(),

        tags$div(class = "fna-info",
          icon("info-circle"),
          tags$div(
            "Null allele frequency estimated by EM per locus \u00d7 population.",
            tags$br(),
            tags$strong("p_nulls"), ": estimated null allele frequency.  ",
            tags$strong("N"), ": total individuals in population.  ",
            tags$strong("N_exp_blanks = N \u00d7 p_nulls\u00b2"),
            ": expected null homozygote count.  ",
            tags$strong("p_nulls\u00d7N"), ": expected null allele copies."
          )
        ),

        tags$div(class = "fna-panel",
          tags$div(class = "fna-panel-head",
            tags$div(class = "fna-panel-title",
              icon("sliders-h"), " Filter parameters")),
          tags$div(class = "fna-panel-body",
            fluidRow(
              column(3,
                selectInput(ns("t1_locus"), "Locus:",
                  choices = c("All loci" = "all"), selected = "all")),
              column(3,
                selectInput(ns("t1_pop"), "Population:",
                  choices = c("All populations" = "all"), selected = "all")),
              column(3,
                tags$div(style = "margin-top:25px;",
                  actionButton(ns("run_t1"),
                    label = tagList(icon("play"), tags$strong(" Compute")),
                    class = "fna-btn btn")))
            )
          )
        ),

        tags$div(class = "fna-panel",
          tags$div(class = "fna-panel-head",
            tags$div(class = "fna-panel-title",
              icon("list"),
              " EM algorithm (Dempster et al. 1977) \u2014 per locus \u00d7 population",
              tags$span(
                style = "font-size:10.5px; color:#64748b; margin-left:8px; font-weight:400;",
                "Locus \u00b7 Pop \u00b7 p_nulls \u00b7 N \u00b7 N_exp_blanks \u00b7 p_nulls\u00d7N")
            )
          ),
          tags$div(class = "fna-panel-body",
            DT::DTOutput(ns("dt_t1")),
            xbtn("dl_t1_csv", "dl_t1_txt")
          )
        )
      ),

      # ════════════════════════════════════════════════════ #
      # TAB 2 — Global summary per locus (EM p_nulls)       #
      # ════════════════════════════════════════════════════ #
      tabPanel(
        title = tagList(icon("globe"), " Global p_nulls"),
        value = "tab_global",
        br(),

        tags$div(class = "fna-info",
          icon("info-circle"),
          tags$div(
            "Global summary across all populations per locus.",
            tags$br(),
            tags$strong("Av(N_exp_blanks)"),
            " = \u03a3(N\u1d62 \u00d7 p\u1d62\u00b2) : total expected null homozygotes across all populations.",
            tags$br(),
            tags$strong("Av(p_nulls)"),
            " = \u03a3(N\u1d62 \u00d7 p\u1d62) / N_tot : N-weighted mean of p_nulls.  ",
            tags$strong("N_tot"), ": total individuals.  ",
            tags$strong("N_blanks"), ": observed missing genotypes.  ",
            tags$strong("f(expBlanks) = Av(N_exp_blanks) / N_tot"), "."
          )
        ),

        tags$div(class = "fna-panel",
          tags$div(class = "fna-panel-head",
            tags$div(class = "fna-panel-title",
              icon("sliders-h"), " Filter parameters")),
          tags$div(class = "fna-panel-body",
            fluidRow(
              column(3,
                selectInput(ns("t2_locus"), "Locus:",
                  choices = c("All loci" = "all"), selected = "all")),
              column(3,
                tags$div(style = "margin-top:25px;",
                  actionButton(ns("run_t2"),
                    label = tagList(icon("play"), tags$strong(" Compute")),
                    class = "fna-btn btn")))
            )
          )
        ),

        tags$div(class = "fna-panel",
          tags$div(class = "fna-panel-head",
            tags$div(class = "fna-panel-title",
              icon("globe"),
              " Global null allele frequency per locus",
              tags$span(
                style = "font-size:10.5px; color:#64748b; margin-left:8px; font-weight:400;",
                "Av(N_exp_blanks) \u00b7 Av(p_nulls) \u00b7 N_tot \u00b7 N_blanks \u00b7 f(expBlanks) \u00b7 p_nulls")
            )
          ),
          tags$div(class = "fna-panel-body",
            DT::DTOutput(ns("dt_t2")),
            xbtn("dl_t2_csv", "dl_t2_txt")
          )
        )
      ),

      # ════════════════════════════════════════════════════ #
      # TAB 3 — Global FST (multilocus)                     #
      # ════════════════════════════════════════════════════ #
      tabPanel(
        title = tagList(icon("chart-line"), " Global FST"),
        value = "tab_fst_global",
        br(),

        tags$div(class = "fna-info-teal",
          icon("info-circle"),
          tags$div(
            tags$strong("Multilocus global FST"), " \u2014 Weir (1996) / Genepop method.",
            tags$br(),
            tags$strong("Raw FST"), " : calculated on observed allele frequencies (null alleles excluded from denominator).",
            tags$br(),
            tags$strong("FST-ENA"), " : calculated on allele frequencies corrected by the EM algorithm ",
            tags$em("(Excluding Null Alleles)"), "."
          )
        ),

        tags$div(class = "fna-formula",
          tags$strong("Weir (1996) formula :"),
          tags$br(),
          "FST = S1 / S3   where   S1 = \u03a3_loci [ s\u00b2P \u00d7 nc ]   and   S3 = \u03a3_loci [ (s\u00b2P + s\u00b2I + s\u00b2G) \u00d7 nc ]",
          tags$br(),
          "nc = (N_tot \u2212 N_tot\u00b2 / N_tot) / (r \u2212 1)   ;   r = number of effective populations",
          tags$br(),
          tags$strong("ENA : corrected frequencies = corrdgenefreq[locus, pop, allele]   (from EM-FreeNA)")
        ),

        tags$div(class = "fna-panel",
          tags$div(class = "fna-panel-head",
            tags$div(class = "fna-panel-title",
              icon("sliders-h"), " Parameters")),
          tags$div(class = "fna-panel-body",
            fluidRow(
              column(4,
                tags$div(style = "margin-top:25px;",
                  actionButton(ns("run_fst_global"),
                    label = tagList(icon("play"), tags$strong(" Calculate")),
                    class = "fna-btn btn")))
            )
          )
        ),

        tags$div(class = "fna-compare-grid",
          tags$div(class = "fna-compare-card",
            tags$div(class = "fna-compare-head fna-compare-head-uncorr",
              icon("table"), " Raw FST \u2014 Weir (1996)"),
            tags$div(class = "fna-compare-body",
              "Observed allele frequencies, null alleles excluded from denominator.",
              tags$br(), "May be biased by the presence of null alleles."
            )
          ),
          tags$div(class = "fna-compare-card",
            tags$div(class = "fna-compare-head fna-compare-head-corr",
              icon("check-circle"), " FST-ENA \u2014 Chapuis & Estoup (2007)"),
            tags$div(class = "fna-compare-body",
              "Frequencies corrected by the EM algorithm. Null alleles are reintegrated,",
              tags$br(), "the bias due to null homozygotes is corrected."
            )
          )
        ),

        tags$div(class = "fna-panel",
          tags$div(class = "fna-panel-head",
            tags$div(class = "fna-panel-title",
              icon("list"), " Global FST multilocus \u2014 per locus")),
          tags$div(class = "fna-panel-body",
            DT::DTOutput(ns("dt_fst_global")),
            xbtn("dl_fst_global_csv", "dl_fst_global_txt")
          )
        )
      ),

      # ════════════════════════════════════════════════════ #
      # TAB 4 — Pairwise FST                                #
      # ════════════════════════════════════════════════════ #
      tabPanel(
        title = tagList(icon("exchange-alt"), " Pairwise FST"),
        value = "tab_fst_pair",
        br(),

        tags$div(class = "fna-info-teal",
          icon("info-circle"),
          tags$div(
            tags$strong("Pairwise FST"), " \u2014 Weir (1996) for each population pair.",
            tags$br(),
            "The lower triangle is displayed: raw FST (without correction) and FST-ENA (with ENA correction).",
            tags$br(),
            tags$strong("NA"), " : calculation not applicable (insufficient sample size for the pair)."
          )
        ),

        tags$div(class = "fna-panel",
          tags$div(class = "fna-panel-head",
            tags$div(class = "fna-panel-title",
              icon("sliders-h"), " Parameters")),
          tags$div(class = "fna-panel-body",
            fluidRow(
              column(4,
                radioButtons(ns("fst_pair_type"), "Display:",
                  choices = c(
                    "Raw FST (without correction)" = "raw",
                    "FST-ENA (corrected)"           = "ena",
                    "Both side by side"              = "both"
                  ), selected = "both", inline = FALSE)),
              column(3,
                tags$div(style = "margin-top:25px;",
                  actionButton(ns("run_fst_pair"),
                    label = tagList(icon("play"), tags$strong(" Calculate")),
                    class = "fna-btn btn")))
            )
          )
        ),

        tags$div(class = "fna-panel",
          tags$div(class = "fna-panel-head",
            tags$div(class = "fna-panel-title",
              icon("th"), " Pairwise FST matrix \u2014 lower triangle")),
          tags$div(class = "fna-panel-body",
            uiOutput(ns("ui_fst_pair_matrix")),
            xbtn("dl_fst_pair_csv", "dl_fst_pair_txt")
          )
        ),

        tags$div(class = "fna-panel",
          tags$div(class = "fna-panel-head",
            tags$div(class = "fna-panel-title",
              icon("list"), " Pairwise FST \u2014 long table format")),
          tags$div(class = "fna-panel-body",
            DT::DTOutput(ns("dt_fst_pair")),
            xbtn("dl_fst_pair_long_csv", "dl_fst_pair_long_txt")
          )
        )
      ),

      # ════════════════════════════════════════════════════ #
      # TAB 5 — Pairwise DCSE distance                      #
      # ════════════════════════════════════════════════════ #
      tabPanel(
        title = tagList(icon("ruler-combined"), " Pairwise DCSE"),
        value = "tab_dc",
        br(),

        tags$div(class = "fna-info-teal",
          icon("info-circle"),
          tags$div(
            tags$strong("Cavalli-Sforza & Edwards (1967) genetic distance"),
            " \u2014 DCSE pairwise.",
            tags$br(),
            tags$strong("Raw DCSE"), " : calculated on observed frequencies (null alleles excluded).",
            tags$br(),
            tags$strong("DCSE-INA"), " : calculated by including the null allele in the corrected frequencies ",
            tags$em("(Including Null Alleles)"), "."
          )
        ),

        tags$div(class = "fna-formula",
          tags$strong("Cavalli-Sforza & Edwards (1967) formula :"),
          tags$br(),
          "DCSE(i,j) = (2/\u03c0) \u00d7 \u221a[ 2 \u00d7 (1 \u2212 \u03a3_k \u221a(p_ik \u00d7 p_jk)) ]",
          tags$br(),
          "Mean distance over loci: mean(DCSE_locus) for valid loci (CSprod \u2264 1)",
          tags$br(),
          tags$strong("INA :"), " corrected frequencies + null allele added as an additional state (freq = rd[locus, pop])"
        ),

        tags$div(class = "fna-panel",
          tags$div(class = "fna-panel-head",
            tags$div(class = "fna-panel-title",
              icon("sliders-h"), " Parameters")),
          tags$div(class = "fna-panel-body",
            fluidRow(
              column(4,
                radioButtons(ns("dc_type"), "Display:",
                  choices = c(
                    "Raw DCSE (without correction)" = "raw",
                    "DCSE-INA (corrected)"          = "ina",
                    "Both side by side"             = "both"
                  ), selected = "both", inline = FALSE)),
              column(3,
                tags$div(style = "margin-top:25px;",
                  actionButton(ns("run_dc"),
                    label = tagList(icon("play"), tags$strong(" Calculate")),
                    class = "fna-btn btn")))
            )
          )
        ),

        tags$div(class = "fna-panel",
          tags$div(class = "fna-panel-head",
            tags$div(class = "fna-panel-title",
              icon("th"), " Pairwise DCSE matrix \u2014 lower triangle")),
          tags$div(class = "fna-panel-body",
            uiOutput(ns("ui_dc_matrix")),
            xbtn("dl_dc_csv", "dl_dc_txt")
          )
        ),

        tags$div(class = "fna-panel",
          tags$div(class = "fna-panel-head",
            tags$div(class = "fna-panel-title",
              icon("list"), " Pairwise DCSE \u2014 long table format")),
          tags$div(class = "fna-panel-body",
            DT::DTOutput(ns("dt_dc")),
            xbtn("dl_dc_long_csv", "dl_dc_long_txt")
          )
        )
      ),

      # ════════════════════════════════════════════════════ #
      # TAB 6 — FST per locus × pair                        #
      # ════════════════════════════════════════════════════ #
      tabPanel(
        title = tagList(icon("th"), " FST per locus \u00d7 pair"),
        value = "tab_fst_locus",
        br(),

        tags$div(class = "fna-info-teal",
          icon("info-circle"),
          tags$div(
            tags$strong("Per-locus FST"), " for each population pair.",
            tags$br(),
            "Allows identification of outlier loci and comparison",
            " of raw and ENA-corrected estimates locus by locus."
          )
        ),

        tags$div(class = "fna-panel",
          tags$div(class = "fna-panel-head",
            tags$div(class = "fna-panel-title",
              icon("sliders-h"), " Filters")),
          tags$div(class = "fna-panel-body",
            fluidRow(
              column(3,
                selectInput(ns("fl_locus"), "Locus:",
                  choices = c("All loci" = "all"), selected = "all")),
              column(3,
                selectInput(ns("fl_pop1"), "Population 1:",
                  choices = c("All pairs" = "all"), selected = "all")),
              column(3,
                selectInput(ns("fl_pop2"), "Population 2:",
                  choices = c("All pairs" = "all"), selected = "all")),
              column(3,
                tags$div(style = "margin-top:25px;",
                  actionButton(ns("run_fst_locus"),
                    label = tagList(icon("play"), tags$strong(" Calculate")),
                    class = "fna-btn btn")))
            )
          )
        ),

        tags$div(class = "fna-panel",
          tags$div(class = "fna-panel-head",
            tags$div(class = "fna-panel-title",
              icon("list"), " FST per locus \u00d7 pair (raw and ENA)")),
          tags$div(class = "fna-panel-body",
            DT::DTOutput(ns("dt_fst_locus")),
            xbtn("dl_fst_locus_csv", "dl_fst_locus_txt")
          )
        )
      )
    )
  )
}