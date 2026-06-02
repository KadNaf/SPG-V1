# module/ui_null_alleles.R
# Null allele frequency estimation (EM), FST-ENA correction, and DCSE-INA genetic distance
# References:
#   Dempster, Laird & Rubin (1977) — EM algorithm
#   Chapuis & Estoup (2007) — FreeNA: ENA and INA corrections for null alleles
#   Weir (1996) — FST following Genepop method
#   Cavalli-Sforza & Edwards (1967) — Chord genetic distance (DCSE)

null_alleles_UI <- function(id) {
  ns <- NS(id)

  custom_css <- tags$style(HTML("
    @import url('https://fonts.googleapis.com/css2?family=IBM+Plex+Mono:wght@400;500;600&family=IBM+Plex+Sans:wght@300;400;500;600&display=swap');

    .na-module * { font-family: 'IBM Plex Sans', sans-serif; }
    .na-module .mono { font-family: 'IBM Plex Mono', monospace; }

    /* ── Header ─────────────────────────────────────────────────────── */
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
    .na-badge-blue   { background:rgba(56,189,248,.15);  border:1px solid rgba(56,189,248,.3);  color:#38bdf8; }
    .na-badge-green  { background:rgba(74,222,128,.12);  border:1px solid rgba(74,222,128,.3);  color:#4ade80; }
    .na-badge-amber  { background:rgba(251,191,36,.12);  border:1px solid rgba(251,191,36,.3);  color:#fbbf24; }
    .na-badge-teal   { background:rgba(20,184,166,.15);  border:1px solid rgba(20,184,166,.3);  color:#2dd4bf; }
    .na-badge-violet { background:rgba(167,139,250,.15); border:1px solid rgba(167,139,250,.3); color:#a78bfa; }

    /* ── Value boxes ─────────────────────────────────────────────────── */
    .na-vbox-row { display:flex; gap:9px; margin-bottom:1rem; flex-wrap:wrap; }
    .na-vbox {
      flex:1; min-width:115px; background:#fff;
      border:1px solid #e2e8f0; border-radius:9px;
      padding:.65rem .9rem; display:flex; align-items:center; gap:9px;
    }
    .na-vbox-icon {
      width:32px; height:32px; border-radius:7px;
      display:flex; align-items:center; justify-content:center;
      font-size:13px; flex-shrink:0;
    }
    .na-vbox-label {
      font-size:10px; color:#94a3b8; text-transform:uppercase;
      letter-spacing:.06em; margin-bottom:1px;
    }
    .na-vbox-val {
      font-size:19px; font-weight:600; color:#0f172a; line-height:1.1;
      font-family:'IBM Plex Mono', monospace;
    }

    /* ── Buttons ─────────────────────────────────────────────────────── */
    .na-btn {
      background: linear-gradient(135deg, #0369a1, #0c4a6e) !important;
      border:none !important; color:#fff !important;
      border-radius:7px !important; font-weight:600 !important;
      font-size:13px !important; padding:7px 20px !important;
      box-shadow:0 2px 8px rgba(3,105,161,.3) !important;
      transition:transform .15s, box-shadow .15s;
    }
    .na-btn:hover {
      transform:translateY(-1px);
      box-shadow:0 4px 14px rgba(3,105,161,.45) !important;
    }
    .na-btn-teal {
      background: linear-gradient(135deg, #0d9488, #064e3b) !important;
      border:none !important; color:#fff !important;
      border-radius:7px !important; font-weight:600 !important;
      font-size:13px !important; padding:7px 20px !important;
      box-shadow:0 2px 8px rgba(13,148,136,.3) !important;
      transition:transform .15s, box-shadow .15s;
    }
    .na-btn-teal:hover {
      transform:translateY(-1px);
      box-shadow:0 4px 14px rgba(13,148,136,.45) !important;
    }

    /* ── Panels ──────────────────────────────────────────────────────── */
    .na-panel {
      background:#fff; border:1px solid #e2e8f0;
      border-radius:9px; margin-bottom:.9rem; overflow:hidden;
    }
    .na-panel-head {
      background:#f8fafc; border-bottom:1px solid #e2e8f0;
      padding:.6rem .95rem; display:flex; align-items:center; flex-wrap:wrap;
    }
    .na-panel-title {
      font-size:12.5px; font-weight:600; color:#1e293b;
      display:flex; align-items:center; gap:6px; flex-wrap:wrap;
    }
    .na-panel-body { padding:.9rem; }

    /* ── Info / warn / formula strips ───────────────────────────────── */
    .na-info {
      background:#eff6ff; border:1px solid #bfdbfe; border-radius:7px;
      padding:.5rem .85rem; font-size:11.5px; color:#1d4ed8;
      display:flex; align-items:flex-start; gap:7px;
      margin-bottom:.9rem; line-height:1.7;
    }
    .na-info-teal {
      background:#f0fdfa; border:1px solid #99f6e4; border-radius:7px;
      padding:.5rem .85rem; font-size:11.5px; color:#134e4a;
      display:flex; align-items:flex-start; gap:7px;
      margin-bottom:.9rem; line-height:1.75;
    }
    .na-warn {
      background:#fffbeb; border:1px solid #fcd34d; border-radius:7px;
      padding:.5rem .85rem; font-size:11.5px; color:#92400e;
      display:flex; align-items:flex-start; gap:7px;
      margin-bottom:.9rem; line-height:1.7;
    }
    .na-formula {
      background:#fafaf9; border:1px solid #d6d3d1; border-radius:7px;
      padding:.55rem .85rem; font-size:11px; color:#292524;
      font-family:'IBM Plex Mono', monospace;
      margin-bottom:.9rem; line-height:1.85;
    }

    /* ── Locus treatment grid ────────────────────────────────────────── */
    .na-treat-grid  { display:flex; flex-wrap:wrap; gap:8px; margin-top:.4rem; }
    .na-treat-item  {
      background:#f8fafc; border:1px solid #e2e8f0;
      border-radius:8px; padding:.5rem .75rem;
      min-width:175px; flex:1;
    }
    .na-treat-lbl {
      font-size:11px; font-weight:700; color:#1e293b;
      font-family:'IBM Plex Mono', monospace; margin-bottom:4px;
    }

    /* ── Compare cards ───────────────────────────────────────────────── */
    .na-compare-grid { display:flex; gap:10px; margin-bottom:.9rem; flex-wrap:wrap; }
    .na-compare-card {
      flex:1; min-width:200px; border-radius:9px;
      border:1px solid #e2e8f0; overflow:hidden;
    }
    .na-compare-head {
      padding:.5rem .8rem; font-size:11.5px; font-weight:700;
      color:#fff; display:flex; align-items:center; gap:6px;
    }
    .na-compare-head-raw  { background:#475569; }
    .na-compare-head-corr { background:#0d9488; }
    .na-compare-body {
      padding:.6rem .8rem; background:#fff;
      font-size:11px; color:#334155; line-height:1.7;
    }

    /* ── Pairwise matrix table ───────────────────────────────────────── */
    .na-matrix-wrap { overflow-x:auto; }
    .na-matrix {
      border-collapse:collapse; font-size:11.5px;
      font-family:'IBM Plex Mono', monospace; width:100%;
    }
    .na-matrix th {
      background:#f8fafc; color:#475569; font-weight:600;
      padding:4px 10px; border:1px solid #e2e8f0;
      font-size:11px; white-space:nowrap;
    }
    .na-matrix td {
      padding:4px 10px; border:1px solid #e2e8f0;
      color:#1e293b; text-align:right; white-space:nowrap;
    }
    .na-matrix tr:nth-child(even) td { background:#f8fafc; }
    .na-matrix .diag  { background:#f1f5f9 !important; color:#94a3b8; text-align:center; }
    .na-matrix .upper { color:#cbd5e1; text-align:center; }
    .na-matrix .pop-label { font-weight:700; color:#0f172a; text-align:left; }

    /* ── Export row ──────────────────────────────────────────────────── */
    .na-export {
      display:flex; align-items:center; gap:6px;
      padding-top:.55rem; border-top:1px solid #f1f5f9; margin-top:.55rem;
    }
    .na-export-lbl { font-size:11px; color:#94a3b8; }

    /* ── DT tweaks ───────────────────────────────────────────────────── */
    .na-module .dataTables_wrapper { font-size:12px; }
    .na-module table.dataTable thead th {
      background:#f8fafc !important; color:#475569 !important;
      font-family:'IBM Plex Mono', monospace !important;
      font-size:11px !important; font-weight:600 !important;
      letter-spacing:.03em !important;
    }
    .na-module table.dataTable tbody td {
      font-family:'IBM Plex Mono', monospace !important;
      font-size:11.5px !important; color:#1e293b !important;
    }
    .na-module .nav-tabs > li > a {
      font-size:12px; font-weight:500; color:#475569;
      border-radius:6px 6px 0 0; padding:5px 14px;
    }
    .na-module .nav-tabs > li.active > a { color:#0f172a; font-weight:600; }
  "))

  # ── Shared export button helper ────────────────────────────────────────────
  xbtn <- function(csv_id, txt_id)
    tags$div(class = "na-export",
      tags$span(class = "na-export-lbl", "Export:"),
      downloadButton(ns(csv_id), "CSV",
        class = "btn btn-default btn-xs",
        style = "padding:2px 10px; font-size:11px;"),
      downloadButton(ns(txt_id), "TXT",
        class = "btn btn-default btn-xs",
        style = "padding:2px 10px; font-size:11px;"))

  # ── Root container ─────────────────────────────────────────────────────────
  tags$div(class = "na-module",
    custom_css,

    # ── Header ────────────────────────────────────────────────────────────────
    tags$div(class = "na-header",
      tags$div(class = "na-header-title",
        icon("atom"),
        " Null Allele Estimation, FST-ENA Correction & DCSE-INA Genetic Distance"),
      tags$div(class = "na-header-sub",
        "Expectation-Maximization (EM) algorithm \u00b7 Dempster, Laird & Rubin (1977)",
        " \u00b7 FreeNA \u2014 Chapuis & Estoup (2007)",
        " \u00b7 Weir (1996) \u00b7 Cavalli-Sforza & Edwards (1967)"),
      tags$div(class = "na-badges",
        tags$span(class = "na-badge na-badge-blue",
          "EM algorithm \u2014 Chapuis & Estoup (2007)"),
        tags$span(class = "na-badge na-badge-green",
          "999999 \u2192 null homozygote"),
        tags$span(class = "na-badge na-badge-amber",
          "000000 \u2192 absent / PCR failure"),
        tags$span(class = "na-badge na-badge-teal",
          "ENA \u2014 FST corrected for null alleles"),
        tags$span(class = "na-badge na-badge-violet",
          "INA \u2014 DCSE corrected for null alleles")
      )
    ),

    # ── Value boxes ───────────────────────────────────────────────────────────
    tags$div(class = "na-vbox-row",
      tags$div(class = "na-vbox",
        tags$div(class = "na-vbox-icon",
          style = "background:#e0f2fe; color:#0369a1;", icon("dna")),
        tags$div(tags$div(class = "na-vbox-label", "Loci"),
                 tags$div(class = "na-vbox-val", uiOutput(ns("vb_loci"))))
      ),
      tags$div(class = "na-vbox",
        tags$div(class = "na-vbox-icon",
          style = "background:#dcfce7; color:#166534;", icon("map-marker-alt")),
        tags$div(tags$div(class = "na-vbox-label", "Populations"),
                 tags$div(class = "na-vbox-val", uiOutput(ns("vb_pops"))))
      ),
      tags$div(class = "na-vbox",
        tags$div(class = "na-vbox-icon",
          style = "background:#f3e8ff; color:#7e22ce;", icon("users")),
        tags$div(tags$div(class = "na-vbox-label", "Individuals"),
                 tags$div(class = "na-vbox-val", uiOutput(ns("vb_n"))))
      ),
      tags$div(class = "na-vbox",
        tags$div(class = "na-vbox-icon",
          style = "background:#fef9c3; color:#854d0e;", icon("percentage")),
        tags$div(tags$div(class = "na-vbox-label", "Avg p_nulls"),
                 tags$div(class = "na-vbox-val", uiOutput(ns("vb_avg_null"))))
      ),
      tags$div(class = "na-vbox",
        tags$div(class = "na-vbox-icon",
          style = "background:#fce7f3; color:#9d174d;", icon("exclamation-triangle")),
        tags$div(tags$div(class = "na-vbox-label", "Max p_nulls"),
                 tags$div(class = "na-vbox-val", uiOutput(ns("vb_max_null"))))
      ),
      tags$div(class = "na-vbox",
        tags$div(class = "na-vbox-icon",
          style = "background:#ccfbf1; color:#0d9488;", icon("chart-bar")),
        tags$div(tags$div(class = "na-vbox-label", "Global FST-ENA"),
                 tags$div(class = "na-vbox-val", uiOutput(ns("vb_fst_ena"))))
      )
    ),

    # ── Per-locus missing genotype treatment selector ──────────────────────────
    tags$div(class = "na-panel",
      tags$div(class = "na-panel-head",
        tags$div(class = "na-panel-title",
          icon("cog"), " Missing genotype coding \u2014 per locus",
          tags$span(
            style = "font-size:10.5px; color:#64748b; margin-left:8px; font-weight:400;",
            "(must match the coding used in the original Genepop file for each locus)")
        )
      ),
      tags$div(class = "na-panel-body",
        tags$div(class = "na-warn",
          icon("exclamation-triangle"),
          tags$div(
            tags$strong("Select the missing genotype coding used in the original Genepop file for each locus:"),
            tags$br(),
            tags$strong("999999"), " \u2014 missing coded as null homozygote \u2192 higher p_nulls",
            " (inferred from excess homozygosity).",
            tags$br(),
            tags$strong("000000"), " \u2014 missing coded as absent / PCR failure \u2192 lower p_nulls",
            " (no null allele signal from missing data)."
          )
        ),
        uiOutput(ns("locus_treatment_ui"))
      )
    ),

    # ── tabsetPanel — all 6 tabs ──────────────────────────────────────────────
    tabsetPanel(
      id = ns("na_tabs"), type = "tabs",

      # ════════════════════════════════════════════════════════════════════════ #
      # TAB 1  p_nulls per locus x population                                  #
      # ════════════════════════════════════════════════════════════════════════ #
      tabPanel(
        title = tagList(icon("table"), " Per locus \u00d7 population"),
        value = "tab_per",
        br(),

        tags$div(class = "na-info",
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

        tags$div(class = "na-panel",
          tags$div(class = "na-panel-head",
            tags$div(class = "na-panel-title",
              icon("sliders-h"), " Filter parameters")),
          tags$div(class = "na-panel-body",
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
                    class = "na-btn btn")))
            )
          )
        ),

        tags$div(class = "na-panel",
          tags$div(class = "na-panel-head",
            tags$div(class = "na-panel-title",
              icon("list"),
              " Estimating null allele frequency using the EM algorithm (Dempster et al. 1977)",
              tags$span(
                style = "font-size:10.5px; color:#64748b; margin-left:8px; font-weight:400;",
                "Locus names \u00b7 Farm \u00b7 p_nulls \u00b7 N \u00b7 N_exp_blanks \u00b7 p_nulls\u00d7N")
            )
          ),
          tags$div(class = "na-panel-body",
            DT::DTOutput(ns("dt_t1")),
            xbtn("dl_t1_csv", "dl_t1_txt")
          )
        )
      ),

      # ════════════════════════════════════════════════════════════════════════ #
      # TAB 2  Global summary per locus                                         #
      # ════════════════════════════════════════════════════════════════════════ #
      tabPanel(
        title = tagList(icon("globe"), " Global summary per locus"),
        value = "tab_global",
        br(),

        tags$div(class = "na-info",
          icon("info-circle"),
          tags$div(
            "Global summary across all populations per locus.",
            tags$br(),
            tags$strong("Av(N_exp_blanks)"),
            " = \u03a3(N\u1d62 \u00d7 p\u1d62\u00b2): total expected null homozygotes across all populations.",
            tags$br(),
            tags$strong("Av(p_nulls)"),
            " = \u03a3(N\u1d62 \u00d7 p\u1d62) / N_tot: N-weighted mean of p_nulls.  ",
            tags$strong("N_tot"), ": total individuals.  ",
            tags$strong("N_blanks"), ": observed missing genotypes.  ",
            tags$strong("f(expBlanks) = Av(N_exp_blanks) / N_tot"), "."
          )
        ),

        tags$div(class = "na-panel",
          tags$div(class = "na-panel-head",
            tags$div(class = "na-panel-title",
              icon("sliders-h"), " Filter parameters")),
          tags$div(class = "na-panel-body",
            fluidRow(
              column(3,
                selectInput(ns("t2_locus"), "Locus:",
                  choices = c("All loci" = "all"), selected = "all")),
              column(3,
                tags$div(style = "margin-top:25px;",
                  actionButton(ns("run_t2"),
                    label = tagList(icon("play"), tags$strong(" Compute")),
                    class = "na-btn btn")))
            )
          )
        ),

        tags$div(class = "na-panel",
          tags$div(class = "na-panel-head",
            tags$div(class = "na-panel-title",
              icon("globe"),
              " Global null allele frequency per locus",
              tags$span(
                style = "font-size:10.5px; color:#64748b; margin-left:8px; font-weight:400;",
                "Av(N_exp_blanks) \u00b7 Av(p_nulls) \u00b7 N_tot \u00b7 N_blanks \u00b7 f(expBlanks) \u00b7 p_nulls")
            )
          ),
          tags$div(class = "na-panel-body",
            DT::DTOutput(ns("dt_t2")),
            xbtn("dl_t2_csv", "dl_t2_txt")
          )
        )
      ),

      # ════════════════════════════════════════════════════════════════════════ #
      # TAB 3  Global FST — raw vs ENA-corrected (multilocus)                   #
      # ════════════════════════════════════════════════════════════════════════ #
      tabPanel(
        title = tagList(icon("chart-bar"), " Global FST (ENA)"),
        value = "tab_fst_global",
        br(),

        tags$div(class = "na-info-teal",
          icon("info-circle"),
          tags$div(
            tags$strong("Global multilocus FST"), " \u2014 Weir (1996) following Genepop's method.",
            tags$br(),
            tags$strong("Raw FST"), ": computed from observed allele frequencies",
            " (null homozygotes excluded from the denominator).",
            tags$br(),
            tags$strong("FST-ENA"), ": computed from EM-corrected allele frequencies",
            " (", tags$em("Excluding Null Alleles"), ") \u2014 Chapuis & Estoup (2007)."
          )
        ),

        tags$div(class = "na-formula",
          tags$strong("Weir (1996) formula:"), tags$br(),
          "FST = S1 / S3", tags$br(),
          "S1 = \u03a3_loci [s\u00b2P \u00d7 nc]   S3 = \u03a3_loci [(s\u00b2P + s\u00b2I + s\u00b2G) \u00d7 nc]", tags$br(),
          "nc = (N_tot \u2212 \u03a3ni\u00b2/N_tot) / (r\u22121)   ;   r = number of effective populations", tags$br(),
          tags$strong("ENA: nA = corrdgenefreq \u00d7 2\u00d7ni   ;   AA_corr = AA \u00d7 p / (p + 2r)")
        ),

        tags$div(class = "na-compare-grid",
          tags$div(class = "na-compare-card",
            tags$div(class = "na-compare-head na-compare-head-raw",
              icon("table"), " Raw FST \u2014 Weir (1996)"),
            tags$div(class = "na-compare-body",
              "Observed allele frequencies, null homozygotes excluded from denominator.",
              tags$br(),
              "May be upwardly biased when null alleles are present.")
          ),
          tags$div(class = "na-compare-card",
            tags$div(class = "na-compare-head na-compare-head-corr",
              icon("check-circle"), " FST-ENA \u2014 Chapuis & Estoup (2007)"),
            tags$div(class = "na-compare-body",
              "EM-corrected allele frequencies (FreeNA algorithm).",
              tags$br(),
              "Bias due to null homozygotes is removed.")
          )
        ),

        tags$div(class = "na-panel",
          tags$div(class = "na-panel-head",
            tags$div(class = "na-panel-title",
              icon("sliders-h"), " Parameters")),
          tags$div(class = "na-panel-body",
            fluidRow(
              column(3,
                tags$div(style = "margin-top:5px;",
                  actionButton(ns("run_fst_global"),
                    label = tagList(icon("play"), tags$strong(" Compute")),
                    class = "na-btn-teal btn")))
            )
          )
        ),

        tags$div(class = "na-panel",
          tags$div(class = "na-panel-head",
            tags$div(class = "na-panel-title",
              icon("list"),
              " Global multilocus FST \u2014 per locus (raw and ENA-corrected)",
              tags$span(
                style = "font-size:10.5px; color:#64748b; margin-left:8px; font-weight:400;",
                "Locus \u00b7 Raw FST \u00b7 FST-ENA \u00b7 \u0394FST \u00b7 N eff. pops (raw) \u00b7 N eff. pops (ENA)")
            )
          ),
          tags$div(class = "na-panel-body",
            DT::DTOutput(ns("dt_fst_global")),
            xbtn("dl_fst_global_csv", "dl_fst_global_txt")
          )
        )
      ),

      # ════════════════════════════════════════════════════════════════════════ #
      # TAB 4  Pairwise FST — raw vs ENA-corrected                              #
      # ════════════════════════════════════════════════════════════════════════ #
      tabPanel(
        title = tagList(icon("exchange-alt"), " Pairwise FST (ENA)"),
        value = "tab_fst_pair",
        br(),

        tags$div(class = "na-info-teal",
          icon("info-circle"),
          tags$div(
            tags$strong("Pairwise FST"), " \u2014 Weir (1996) for each pair of populations.",
            tags$br(),
            "Lower triangle matrix: raw FST (uncorrected) and FST-ENA (ENA-corrected).",
            tags$br(),
            tags$strong("NA"), ": computation not applicable (insufficient sample size for the pair)."
          )
        ),

        tags$div(class = "na-panel",
          tags$div(class = "na-panel-head",
            tags$div(class = "na-panel-title",
              icon("sliders-h"), " Parameters")),
          tags$div(class = "na-panel-body",
            fluidRow(
              column(5,
                radioButtons(ns("fst_pair_type"), "Display:",
                  choices = c(
                    "Raw FST (uncorrected)"      = "raw",
                    "FST-ENA (corrected)"        = "ena",
                    "Both side by side"          = "both"
                  ), selected = "both", inline = FALSE)),
              column(3,
                tags$div(style = "margin-top:25px;",
                  actionButton(ns("run_fst_pair"),
                    label = tagList(icon("play"), tags$strong(" Compute")),
                    class = "na-btn-teal btn")))
            )
          )
        ),

        tags$div(class = "na-panel",
          tags$div(class = "na-panel-head",
            tags$div(class = "na-panel-title",
              icon("th"), " Pairwise FST matrix \u2014 lower triangle")),
          tags$div(class = "na-panel-body",
            uiOutput(ns("ui_fst_pair_matrix")),
            xbtn("dl_fst_pair_csv", "dl_fst_pair_txt")
          )
        ),

        tags$div(class = "na-panel",
          tags$div(class = "na-panel-head",
            tags$div(class = "na-panel-title",
              icon("list"), " Pairwise FST \u2014 long tabular format",
              tags$span(
                style = "font-size:10.5px; color:#64748b; margin-left:8px; font-weight:400;",
                "Pop1 \u00b7 Pop2 \u00b7 Raw FST \u00b7 FST-ENA \u00b7 \u0394FST (ENA \u2212 raw)")
            )
          ),
          tags$div(class = "na-panel-body",
            DT::DTOutput(ns("dt_fst_pair")),
            xbtn("dl_fst_pair_long_csv", "dl_fst_pair_long_txt")
          )
        )
      ),

      # ════════════════════════════════════════════════════════════════════════ #
      # TAB 5  Pairwise DCSE — raw vs INA-corrected                             #
      # ════════════════════════════════════════════════════════════════════════ #
      tabPanel(
        title = tagList(icon("ruler-combined"), " DCSE distance (INA)"),
        value = "tab_dc",
        br(),

        tags$div(class = "na-info-teal",
          icon("info-circle"),
          tags$div(
            tags$strong("Cavalli-Sforza & Edwards (1967) chord genetic distance"), " \u2014 pairwise DCSE.",
            tags$br(),
            tags$strong("Raw DCSE"), ": computed from observed allele frequencies",
            " (null allele state excluded).",
            tags$br(),
            tags$strong("DCSE-INA"), ": null allele included as an additional allelic state",
            " (", tags$em("Including Null Alleles"), ") \u2014 Chapuis & Estoup (2007)."
          )
        ),

        tags$div(class = "na-formula",
          tags$strong("Cavalli-Sforza & Edwards (1967) formula:"), tags$br(),
          "DCSE(i,j) = (2/\u03c0) \u00d7 \u221a[ 2 \u00d7 (1 \u2212 \u03a3_k \u221a(p_ik \u00d7 p_jk)) ]", tags$br(),
          "Averaged over valid loci (CSprod \u2264 1)", tags$br(),
          tags$strong("INA:"),
          " corrected frequencies + null allele added as extra state (freq = rd[locus, pop])"
        ),

        tags$div(class = "na-panel",
          tags$div(class = "na-panel-head",
            tags$div(class = "na-panel-title",
              icon("sliders-h"), " Parameters")),
          tags$div(class = "na-panel-body",
            fluidRow(
              column(5,
                radioButtons(ns("dc_type"), "Display:",
                  choices = c(
                    "Raw DCSE (uncorrected)"     = "raw",
                    "DCSE-INA (corrected)"       = "ina",
                    "Both side by side"          = "both"
                  ), selected = "both", inline = FALSE)),
              column(3,
                tags$div(style = "margin-top:25px;",
                  actionButton(ns("run_dc"),
                    label = tagList(icon("play"), tags$strong(" Compute")),
                    class = "na-btn-teal btn")))
            )
          )
        ),

        tags$div(class = "na-panel",
          tags$div(class = "na-panel-head",
            tags$div(class = "na-panel-title",
              icon("th"), " Pairwise DCSE matrix \u2014 lower triangle")),
          tags$div(class = "na-panel-body",
            uiOutput(ns("ui_dc_matrix")),
            xbtn("dl_dc_csv", "dl_dc_txt")
          )
        ),

        tags$div(class = "na-panel",
          tags$div(class = "na-panel-head",
            tags$div(class = "na-panel-title",
              icon("list"), " Pairwise DCSE \u2014 long tabular format",
              tags$span(
                style = "font-size:10.5px; color:#64748b; margin-left:8px; font-weight:400;",
                "Pop1 \u00b7 Pop2 \u00b7 Raw DCSE \u00b7 DCSE-INA \u00b7 \u0394DCSE (INA \u2212 raw)")
            )
          ),
          tags$div(class = "na-panel-body",
            DT::DTOutput(ns("dt_dc")),
            xbtn("dl_dc_long_csv", "dl_dc_long_txt")
          )
        )
      ),

      # ════════════════════════════════════════════════════════════════════════ #
      # TAB 6  FST per locus x pair                                             #
      # ════════════════════════════════════════════════════════════════════════ #
      tabPanel(
        title = tagList(icon("table"), " FST per locus \u00d7 pair"),
        value = "tab_fst_locus",
        br(),

        tags$div(class = "na-info-teal",
          icon("info-circle"),
          tags$div(
            tags$strong("Per-locus FST"), " for each pair of populations.",
            tags$br(),
            "Useful for detecting outlier loci and comparing raw versus ENA-corrected",
            " estimates locus by locus."
          )
        ),

        tags$div(class = "na-panel",
          tags$div(class = "na-panel-head",
            tags$div(class = "na-panel-title",
              icon("sliders-h"), " Filters")),
          tags$div(class = "na-panel-body",
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
                    label = tagList(icon("play"), tags$strong(" Compute")),
                    class = "na-btn-teal btn")))
            )
          )
        ),

        tags$div(class = "na-panel",
          tags$div(class = "na-panel-head",
            tags$div(class = "na-panel-title",
              icon("list"), " FST per locus \u00d7 pair (raw and ENA-corrected)",
              tags$span(
                style = "font-size:10.5px; color:#64748b; margin-left:8px; font-weight:400;",
                "Locus \u00b7 Pop1 \u00b7 Pop2 \u00b7 Raw FST \u00b7 FST-ENA \u00b7 \u0394FST \u00b7 N_i (raw) \u00b7 N_j (raw) \u00b7 N_i (ENA) \u00b7 N_j (ENA)")
            )
          ),
          tags$div(class = "na-panel-body",
            DT::DTOutput(ns("dt_fst_locus")),
            xbtn("dl_fst_locus_csv", "dl_fst_locus_txt")
          )
        )
      )

    ) # end tabsetPanel
  )   # end tags$div.na-module
}
