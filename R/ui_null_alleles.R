# module/ui_null_alleles.R
# Null allele frequency estimation (EM), FST-ENA, DCSE-INA
# Simplified UI per supervisor feedback:
#   - Radio buttons for missing genotype coding, default = 000000
#   - Single bootstrap panel: n replicates + CI level choice
#   - 4 automatic output files
#
# UI rebuilt to match the shared ShinyPopGen design system used by every
# other module (module_banner, shinydashboard box(), valueBox, spg-method-note,
# btn-action-primary/secondary, btn-download-primary/secondary, tabsetPanel).
# Server output IDs are unchanged so server_null_alleles.R keeps working as-is.
#
# References:
#   Dempster, Laird & Rubin (1977)  — EM algorithm
#   Chapuis & Estoup (2007)         — FreeNA: ENA and INA corrections
#   Weir & Cockerham (1984)         — FST unbiased moment estimator
#   Cavalli-Sforza & Edwards (1967) — Chord genetic distance (DCSE)

null_alleles_UI <- function(id) {
  ns <- NS(id)

  # ── Supplemental CSS — only the bits with no shared-system equivalent:
  #    per-locus coding grid, pairwise matrices, bootstrap-result callouts.
  #    Recolored to the app's own palette (no more standalone dark/neon theme).
  supplemental_css <- tags$style(HTML("
    .na-info {
      background:#f5f7fa; border-left:4px solid #8D8680; border-radius:3px;
      padding:10px 14px; font-size:13px; line-height:1.65; color:#2c3e50; margin-bottom:14px;
    }
    .na-warn {
      background:#fff8e6; border-left:4px solid #E1AF00; border-radius:3px;
      padding:10px 14px; font-size:13px; line-height:1.65; color:#5c4400; margin-bottom:14px;
    }
    .na-locus-grid { display:flex; flex-wrap:wrap; gap:8px; margin-top:8px; }
    .na-locus-item {
      background:#f8fafc; border:1px solid #e2e8f0; border-radius:6px;
      padding:.5rem .8rem; min-width:150px; flex:1;
    }
    .na-locus-item .control-label { display:none; } /* hide redundant label */
    .na-locus-name {
      font-size:20px; font-weight:700; color:#333a43;
      font-family:'Consolas','IBM Plex Mono',monospace; margin-bottom:4px;
    }
    .na-locus-item .radio { margin:2px 0; }
    .na-locus-item .radio label { font-size:13px; color:#333a43; }

    .na-boot-result {
      background:#f5f7fa; border:1px solid #8ea1b9; border-radius:6px;
      padding:.65rem 1rem; font-size:12.5px; color:#333a43;
      font-family:'Consolas','IBM Plex Mono',monospace; line-height:1.9; margin-top:.5rem;
    }
    .na-boot-result strong { color:#0c4a6e; }

    .na-matrix-wrap { overflow-x:auto; margin-top:.5rem; }
    .na-matrix { border-collapse:collapse; font-size:11px; font-family:'Consolas','IBM Plex Mono',monospace; width:100%; }
    .na-matrix th { background:#f8fafc; color:#475569; font-weight:600; padding:4px 9px; border:1px solid #e2e8f0; font-size:10.5px; white-space:nowrap; }
    .na-matrix td { padding:4px 9px; border:1px solid #e2e8f0; color:#1e293b; text-align:right; white-space:nowrap; font-size:11px; }
    .na-matrix tr:nth-child(even) td { background:#f8fafc; }
    .na-matrix .diag  { background:#f1f5f9 !important; color:#94a3b8; text-align:center; }
    .na-matrix .upper { color:#cbd5e1; text-align:center; }
    .na-matrix .lbl   { font-weight:700; color:#333a43; text-align:left; white-space:nowrap; }

    .na-dl-row { display:flex; gap:6px; flex-wrap:wrap; margin-top:.5rem; }
    .na-dl-row .btn { font-size:11px; padding:3px 12px; }

    .na-filecard { margin-bottom: 14px; }
    .na-filecard .fname { margin-top:8px; font-size:11px; word-break:break-all; color:#666; }
  "))

  box_title_style <- "background-color: #FFFFFF; padding: 10px; color: #333a43; font-weight: 600;"

  fluidPage(
    tags$head(gs_head()),
    supplemental_css,

    module_banner("circle-notch", "Null Allele Estimation · FST-ENA · DCSE-INA",""),

    # tags$div(class = "spg-method-note", style = "border-left-color:#8D8680;",
    #   HTML(paste0(
    #     "<b>EM algorithm</b> (Dempster, Laird &amp; Rubin 1977) estimates the null allele frequency ",
    #     "at each locus \u00d7 population, following the <b>FreeNA</b> approach (Chapuis &amp; Estoup 2007). ",
    #     "<br><br>",
    #     "<b>F<sub>ST</sub>-ENA</b>: multilocus and pairwise F<sub>ST</sub> (Weir &amp; Cockerham 1984), ",
    #     "Excluding Null Alleles \u2014 corrected using the EM-estimated frequencies. ",
    #     "<b>D<sub>CSE</sub>-INA</b>: Cavalli-Sforza &amp; Edwards (1967) chord distance, Including the ",
    #     "null allele as an extra allelic state.",
    #     "<br><br>",
    #     "<b>Bootstrap confidence intervals</b> are computed by two resampling schemes:",
    #     "<ul style='margin:4px 0 0 16px;'>",
    #     "<li><b>Loci</b>, resampled with replacement across the whole locus set (multilocus estimates only).</li>",
    #     "<li><b>Sub-samples</b> (populations), resampled as whole blocks with replacement (multilocus and per-locus).</li>",
    #     "</ul>"
    #   ))
    # ),

    # ════════════════════════════════════════════════════════════════════
    # SUMMARY — value boxes
    # ════════════════════════════════════════════════════════════════════
    # fluidRow(
    #   box(
    #     width = 12, solidHeader = TRUE, status = "primary",
    #     title = div(style = "background:#FFFFFF; padding:10px; color:#333a43; font-weight:600;",
    #                 icon("chart-bar"), " Summary "),
    #     fluidRow(
    #       column(2, valueBoxOutput(ns("vb_loci"),  width = NULL)),
    #       column(2, valueBoxOutput(ns("vb_pops"),  width = NULL)),
    #       column(2, valueBoxOutput(ns("vb_n"),     width = NULL)),
    #       column(2, valueBoxOutput(ns("vb_avg_null"), width = NULL)),
    #       column(2, valueBoxOutput(ns("vb_max_null"), width = NULL)),
    #       column(2, valueBoxOutput(ns("vb_fst_ena"),  width = NULL))
    #     )
    #   )
    # ),

    # ════════════════════════════════════════════════════════════════════
    # SETUP
    # ════════════════════════════════════════════════════════════════════
    fluidRow(
      box(
        width = 12,
        title = div(style = box_title_style, icon("sliders"), "Setup"),
        solidHeader = TRUE, status = "primary",

        # h4(icon("code-branch"), "(1) Missing genotype coding per locus"),
        # tags$div(class = "na-warn",
        #   tags$p(style = "margin:.25rem 0;",
        #     "Please choose how to code missing data for each locus:", tags$br(),
        #     tags$strong("0"), " = true missing data (ignored by the algorithm);", tags$br(),
        #     tags$strong("999999"), " = homozygote for allele 999 (code for all null alleles)"),
        #   tags$p(style = "margin:.5rem 0 0;font-weight:600;",
        #     "Please make sure you do not already have any allele coded as 999.")
        # ),

        h4(icon("code-branch"), "(1) Missing genotype coding per locus"),
        tags$div(class = "na-warn",
          tags$p(style = "margin:.25rem 0;",
            "Choose the missing data code for each locus: ",
            tags$strong("0"), " = true missing (ignored); ",
            tags$strong("999999"), " = homozygote for allele 999 (null alleles)."
          ),
          tags$p(
            style = "margin:.25rem 0 0;font-weight:600;",
            "Ensure allele 999 is not already present in your dataset."
          )
        ),

        uiOutput(ns("locus_coding_ui")),

        tags$hr(),

        h4(icon("dice"), "(2) Bootstrap parameters"),
        p("Bootstrap over subsamples (for 5 subsamples at least) and over loci (for 5 loci at least)"),
        tags$div(style = "display:flex; align-items:flex-start; gap:0; flex-wrap:wrap;",
          tags$div(style = "flex:1; min-width:190px; padding-right:16px;",
            numericInput(ns("nboot"),
              label = "Number of bootstraps over loci (for all analyses) (at least 100):",
              value = 5000, min = 100, max = 99999, step = 1000, width = "100%")),
          tags$div(style = "border-left:1px solid #dcdfe4; flex:1; min-width:190px; padding:0 16px;",
            numericInput(ns("nboot_subs"),
              label = "Number of bootstraps over subsamples (for over all FST's) (at least 100):",
              value = 5000, min = 100, max = 99999, step = 1000, width = "100%")),
          tags$div(style = "border-left:1px solid #dcdfe4; flex:0 0 220px; padding:0 16px;",
            numericInput(ns("alpha"),
              label = "Critical level for confidence intervals (between 0.9999 and 0.0001):",
              value = 0.05, min = 0.0001, max = 0.5, step = 0.01, width = "100%")),
          tags$div(style = "border-left:1px solid #dcdfe4; flex:0 0 200px; padding:0 0 0 16px;",
            numericInput(ns("boot_seed"),
              label = "Random seed (reproducibility):",
              value = 12345, min = 1, max = 1000000, step = 1, width = "100%"))
        ),

        tags$hr(),

        h4(icon("map-marker-alt"), "GPS coordinates of subsamples"),
        radioButtons(ns("gps_available"), "Do you have GPS coordinates for your subsamples (decimal degrees)?",
          choices = c("Yes" = "yes", "No" = "no"), selected = "yes", inline = TRUE),
        conditionalPanel(
          condition = sprintf("input['%s'] == 'yes'", ns("gps_available")),
          fluidRow(
            column(3, uiOutput(ns("gps_lon_col_ui"))),
            column(3, uiOutput(ns("gps_lat_col_ui")))
          ),
          tags$p(style="color:#777;font-size:11px;",
            icon("info-circle"), " Auto-detected from your imported data when possible \u2014 change if the ",
            "wrong columns were picked. Used to compute geographic distances (Vincenty) for the Full pairwise table.")
        ),

        tags$hr(),

        h4(icon("save"), "Choose names for output files"),
        tags$div(style = "max-width:320px;",
          textInput(ns("out_root"), NULL, value = "", placeholder = "auto-filled from the imported data file name")),
        tags$p(style="color:#777;font-size:11px;",
          "Defaults to the imported dataset's name. Change it and every output file below will use that name instead."),

        tags$hr(),

        h4(icon("rocket"), "(3) Run all computations + generate output files"),
        checkboxInput(ns("include_pairwise_table"),
          "Also generate the Full pairwise table (needs GPS Latitude/Longitude at import)",
          value = TRUE),
        uiOutput(ns("ui_gps_status")),
        fluidRow(
          column(4,
            actionButton(ns("run_all"),
              label = tagList(icon("rocket"), tags$strong(" Compute")),
              class = "btn-action-primary btn-block",
              style = "font-weight: bold;"))
        ),
        br(),
        uiOutput(ns("ui_run_status"))
      )
    ),

    # ════════════════════════════════════════════════════════════════════
    # OUTPUT FILES — one card per exported file
    # ════════════════════════════════════════════════════════════════════
    h2("Output files", class = "section-title"),
    fluidRow(
      box(
        width = 12,
        title = uiOutput(ns("ui_output_files_title"), inline = TRUE),
        solidHeader = TRUE, status = "primary",
        tags$div(class = "spg-module-card na-filecard", style = "margin-bottom:14px; max-width:400px;",
          # tags$div(class = "card-icon", icon("table")),
          h5("p_nulls / locus"),
          p("Null allele frequencies per locus and subsamples, and averaged over subsamples."),
          tags$div(class = "fname", uiOutput(ns("ui_filename_1"), inline = TRUE)),
          uiOutput(ns("ui_dl_file1"))
        ),
        tags$div(class = "spg-module-card na-filecard", style = "margin-bottom:14px; max-width:400px;",
          # tags$div(class = "card-icon", icon("chart-bar")),
          h5("FST / FST-ENA"),
          p("Global FST per locus and over all, corrected or not for null alleles, with CI of bootstrap over subsamples and loci."),
          tags$div(class = "fname", uiOutput(ns("ui_filename_2"), inline = TRUE)),
          uiOutput(ns("ui_dl_file2"))
        ),
        tags$div(class = "spg-module-card na-filecard", style = "margin-bottom:14px; max-width:400px;",
          # tags$div(class = "card-icon", icon("route")),
          h5("Pairwise (long format)"),
          p("Table of all genetic distances between all subsample pairs, corrected or not for null alleles, with CI of bootstrap over loci."),
          tags$div(class = "fname", uiOutput(ns("ui_filename_3"), inline = TRUE)),
          uiOutput(ns("ui_dl_file3"))
        ),
        tags$div(class = "spg-module-card na-filecard", style = "margin-bottom:14px; max-width:400px;",
          # tags$div(class = "card-icon", icon("th")),
          h5("Per-locus half-matrices"),
          p("Paired genetic distances in half left matrices, for use by other software."),
          tags$div(class = "fname", uiOutput(ns("ui_filename_4"), inline = TRUE)),
          uiOutput(ns("ui_dl_file4"))
        ),
        tags$div(class = "spg-module-card na-filecard", style = "margin-bottom:14px; max-width:400px;",
          # tags$div(class = "card-icon", icon("dice")),
          h5("Bootstrap distributions"),
          p("Detailed bootstrap distribution for global FST's."),
          tags$div(class = "fname", uiOutput(ns("ui_filename_5"), inline = TRUE)),
          uiOutput(ns("ui_dl_file5"))
        ),
        tags$div(class = "spg-module-card na-filecard", style = "margin-bottom:14px; max-width:400px;",
          # tags$div(class = "card-icon", icon("sliders-h")),
          h5("Run parameters"),
          p("List of parameters you chose to use."),
          tags$div(class = "fname", uiOutput(ns("ui_filename_6"), inline = TRUE)),
          uiOutput(ns("ui_dl_file6"))
        ),
        uiOutput(ns("ui_file7_card")),
        tags$hr(),
        downloadButton(ns("dl_all_zip"), "Download all files (.zip)", class = "btn-action-primary")
      )
    ),

    # ════════════════════════════════════════════════════════════════════
    # RESULTS TABS — for visual inspection
    # ════════════════════════════════════════════════════════════════════
    h2("Results", class = "section-title"),
    fluidRow(
      box(
        width = 12,
        title = div(style = box_title_style, icon("table"), "Detailed results"),
        solidHeader = TRUE, status = "primary",

        tabsetPanel(id = ns("na_tabs"), type = "tabs",

          # ── TAB 1: Null allele frequencies ────────────────────────────────── #
          tabPanel(title = tagList(icon("percent"), " Null allele frequencies"),
                   value = "tab_na", br(),
            # tags$div(class = "na-info",
            #   "Reproduces FreeNA's own null-allele-frequency report: the EM algorithm ",
            #   "(Dempster, Laird & Rubin 1977) estimated per locus \u00d7 population below, ",
            #   "and the N-weighted per-locus summary (Av(p_nulls), Av(N_exp_blanks), ",
            #   "f(expBlanks), one-sided binomial test p-value, and chosen blank coding) further down."
            # ),
            h4(icon("info-circle"), "p_nulls per locus \u00d7 population (EM algorithm)"),
            DT::DTOutput(ns("dt_t1")), br(),
            h4(icon("info-circle"), "Per-locus summary (N-weighted mean, FreeNA report format)"),
            DT::DTOutput(ns("dt_t2"))
          ),

          # ── TAB 2: FST & FST-ENA ──────────────────────────────────────────── #
          tabPanel(title = tagList(icon("chart-bar"), " FST / FST-ENA"),
                   value = "tab_fst", br(),
            # tags$div(class = "na-info",
            #   tags$strong("Global multilocus FST"), " \u2014 Weir & Cockerham (1984) unbiased moment estimator. ",
            #   tags$strong("FST-ENA"), ": EM-corrected frequencies, Excluding Null Alleles \u2014 Chapuis & Estoup (2007).",
            #   tags$br(),
            #   "Bootstrap CI over loci (resample loci with replacement, multilocus estimates only) and over ",
            #   "sub-samples (resample populations as whole blocks with replacement, available both for the ",
            #   "multilocus estimate and per locus \u2014 see the per-locus table below)."
            # ),
            h4(icon("table"), "Per-locus FST and FST-ENA"),
            DT::DTOutput(ns("dt_fst_global")), br(),

            h4(icon("chart-area"), "Bootstrap CI \u2014 Global FST and FST-ENA"),
            uiOutput(ns("ui_boot_global_fst")), br(),

            h4(icon("th"), "Pairwise FST and FST-ENA \u2014 lower triangle matrix"),
            fluidRow(
              column(5,
                radioButtons(ns("fst_pair_display"), "Display:",
                  choices = c(
                    "Raw FST (uncorrected)" = "raw",
                    "FST-ENA (corrected)"   = "ena",
                    "Both side by side"     = "both"),
                  selected = "both", inline = TRUE))),
            uiOutput(ns("ui_fst_pair_matrix")), br(),

            h4(icon("chart-area"), "Bootstrap CI \u2014 Pairwise FST-ENA (over loci)"),
            uiOutput(ns("ui_boot_pair_fst"))
          ),

          # ── TAB 3: DCSE / DCSE-INA ────────────────────────────────────────── #
          tabPanel(title = tagList(icon("route"), " DCSE / DCSE-INA"),
                   value = "tab_dc", br(),
            # tags$div(class = "na-info",
            #   tags$strong("Cavalli-Sforza & Edwards (1967) chord distance."),
            #   " DCSE-INA includes the null allele as an extra state \u2014 Chapuis & Estoup (2007).",
            #   tags$br(),
            #   "DCSE(i,j) = (2/\u03c0)\u00d7\u221a[2\u00d7(1\u2212\u03a3\u221a(p_ik\u00d7p_jk))]  ",
            #   "INA: corrdgenefreq + null allele appended (freq = rd[locus, pop])."
            # ),
            h4(icon("th"), "Pairwise DCSE and DCSE-INA \u2014 lower triangle matrix"),
            fluidRow(
              column(5,
                radioButtons(ns("dc_display"), "Display:",
                  choices = c(
                    "Raw DCSE (uncorrected)" = "raw",
                    "DCSE-INA (corrected)"   = "ina",
                    "Both side by side"      = "both"),
                  selected = "both", inline = TRUE))),
            uiOutput(ns("ui_dc_matrix")), br(),

            h4(icon("chart-area"), "Bootstrap CI \u2014 Pairwise DCSE-INA (over loci)"),
            uiOutput(ns("ui_boot_pair_dc"))
          ),

          # ── TAB 4: Per-locus x pair ───────────────────────────────────────── #
          tabPanel(title = tagList(icon("border-all"), " Per-locus \u00d7 pair"),
                   value = "tab_locus_pair", br(),
            # tags$div(class = "na-info",
            #   "FST, FST-ENA, DCSE and DCSE-INA for each locus \u00d7 pair of populations.",
            #   " Useful for detecting outlier loci."
            # ),
            fluidRow(
              column(3, selectInput(ns("fl_locus"), "Locus:",
                choices = c("All loci" = "all"), selected = "all")),
              column(3, selectInput(ns("fl_pop1"), "Population 1:",
                choices = c("All pairs" = "all"), selected = "all")),
              column(3, selectInput(ns("fl_pop2"), "Population 2:",
                choices = c("All pairs" = "all"), selected = "all"))
            ),
            h4(icon("table"), "FST and FST-ENA per locus \u00d7 pair"),
            DT::DTOutput(ns("dt_fst_locus")), br(),
            h4(icon("table"), "DCSE and DCSE-INA per locus \u00d7 pair"),
            DT::DTOutput(ns("dt_dc_locus"))
          ),

          # ── TAB 5: Full pairwise table ──────────────────────────────────── #
          tabPanel(title = tagList(icon("route"), " Full pairwise table"),
                   value = "tab_full_pairwise", br(),
            uiOutput(ns("ui_full_pairwise_note")),
            DT::DTOutput(ns("dt_full_pairwise"))
          )
        ),
        style = "padding: 10px;"
      )
    )
  )
}