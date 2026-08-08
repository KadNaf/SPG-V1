# ui_null_alleles.R
# Null allele frequency estimation (EM), FST-ENA, DCSE-INA
# Simplified UI per supervisor feedback:
#   - Radio buttons for missing genotype coding, default = 000000
#   - Single bootstrap panel: n replicates + CI level choice
#   - 4 automatic output files
#
# References:
#   Dempster, Laird & Rubin (1977)  — EM algorithm
#   Chapuis & Estoup (2007)         — FreeNA: ENA and INA corrections
#   Weir & Cockerham (1984)         — FST unbiased moment estimator
#   Cavalli-Sforza & Edwards (1967) — Chord genetic distance (DCSE)

null_alleles_UI <- function(id) {
  ns <- NS(id)

  dlrow <- function(...) tags$div(class="na-dl-row", ...)

  box_title_style <- "background-color: #FFFFFF; padding: 10px; color: #333a43; font-weight: 600;"

  fluidPage(
    tags$head(gs_head()),

    module_banner("dna", "Null Allele Estimation \u00b7 FST-ENA \u00b7 DCSE-INA",
      "EM algorithm \u00b7 Dempster, Laird & Rubin (1977) \u00b7 FreeNA \u2014 Chapuis & Estoup (2007) \u00b7 Weir & Cockerham (1984) \u00b7 Cavalli-Sforza & Edwards (1967)",
      "#0369a1"),

    tags$div(class = "spg-method-note", style = "border-left-color:#0369a1;",
      "EM \u2014 null allele frequency \u00b7 ENA \u2014 FST corrected \u00b7 INA \u2014 DCSE corrected \u00b7 Bootstrap CI \u2014 loci & sub-samples"
    ),

    # ── Summary ──────────────────────────────────────────────────────────────
    fluidRow(
      box(
        width = 12,
        title = div(style = box_title_style, icon("info-circle"), " Summary"),
        solidHeader = TRUE, status = "primary",
        fluidRow(
          column(2, tags$div(tags$strong("Loci"), tags$div(uiOutput(ns("vb_loci"))))),
          column(2, tags$div(tags$strong("Populations"), tags$div(uiOutput(ns("vb_pops"))))),
          column(2, tags$div(tags$strong("Individuals"), tags$div(uiOutput(ns("vb_n"))))),
          column(2, tags$div(tags$strong("Avg p_nulls"), tags$div(uiOutput(ns("vb_avg_null"))))),
          column(2, tags$div(tags$strong("Max p_nulls"), tags$div(uiOutput(ns("vb_max_null"))))),
          column(2, tags$div(tags$strong("Global FST-ENA"), tags$div(uiOutput(ns("vb_fst_ena")))))
        )
      )
    ),

    # ── Setup ────────────────────────────────────────────────────────────────
    fluidRow(
      box(
        width = 12,
        title = div(style = box_title_style, icon("cogs"), " Setup"),
        solidHeader = TRUE, status = "primary",

        tags$div(class = "spg-method-note", style = "border-left-color:#f59e0b;",
          tags$p(style = "margin:4px 0;",
            "Please choose how to code missing data for each locus:", tags$br(),
            tags$strong("0"), " = true missing data (ignored by the algorithm);", tags$br(),
            tags$strong("999999"), " = homozygote for allele 999 (code for all null alleles)"),
          tags$p(style = "margin:4px 0 0; font-weight:600;",
            "Please make sure you do not already have any allele coded as 999.")
        ),

        uiOutput(ns("locus_coding_ui")),

        tags$hr(),

        tags$strong("Bootstrap parameters"),
        tags$br(), tags$br(),
        fluidRow(
          column(3,
            numericInput(ns("nboot"),
              label = "Number of replicates (bootstrap over loci):",
              value = 5000, min = 100, max = 99999, step = 1000)),
          column(3,
            numericInput(ns("nboot_subs"),
              label = "Number of replicates (bootstrap over sub-samples):",
              value = 5000, min = 100, max = 99999, step = 1000)),
          column(3,
            numericInput(ns("alpha"),
              label = "Confidence interval — alpha:",
              value = 0.05, min = 0.0001, max = 0.5, step = 0.01)),
          column(3,
            numericInput(ns("boot_seed"),
              label = "Random seed (reproducibility):",
              value = 12345, min = 1, max = 2147483647, step = 1))
        ),
        tags$p(class = "text-muted", style = "font-size:11px;",
          "Bootstrap resampling is random: point estimates (FST, FST-ENA, DCSE\u2026) never change, ",
          "but confidence interval bounds will shift slightly from run to run unless the seed is kept ",
          "the same. Re-run with the same seed, same data and same number of replicates to reproduce ",
          "the exact same confidence intervals \u2014 the seed used is recorded in every exported file."),

        tags$hr(),

        tags$strong("Output files"),
        tags$br(), tags$br(),
        fluidRow(
          column(6,
            tags$div(style = "display:flex; align-items:flex-end; gap:8px;",
              tags$div(style = "flex:1;",
                textInput(ns("out_dir_display"), "Please choose a folder for output files:",
                          value = "", placeholder = "(no folder chosen \u2014 files download to your browser instead)")),
              shinyFiles::shinyDirButton(ns("out_dir_browse"), "Browse", "Choose output folder",
                                          class = "btn-action-secondary", style = "margin-bottom:15px;"))),
          column(6,
            textInput(ns("out_root"), "Root for the name of output files:",
                      value = "", placeholder = "auto-filled from the imported data file name"))
        ),
        tags$p(class = "text-muted", style = "font-size:11px;",
          "The root is proposed automatically from the name of the data file you imported ",
          "(e.g. ", tags$code("BoophilusAdultsDataCattle"), "), and you can freely edit or extend it ",
          "\u2014 e.g. add your own notes such as which loci were recoded to 999999. ",
          "File names = root + description (e.g. ", tags$code("<root>null_allele_frequencies.txt"),
          "). No date is added (already shown by the file explorer) \u2014 if you re-run with a ",
          "different missing-data coding and want to keep both results, add your own suffix below."),
        textInput(ns("out_suffix"), "Optional suffix to distinguish this run (e.g. \"1\"):", value = ""),
        tags$p(class = "text-muted", style = "font-size:11px;",
          "Files are saved as tab-delimited ", tags$strong(".txt"), " (not .csv)."),

        tags$hr(),

        tags$strong("(3) Run all computations + generate output files"),
        tags$br(), tags$br(),
        fluidRow(
          column(4,
            actionButton(ns("run_all"),
              label = tagList(tags$strong("  Compute + Bootstrap + Export")),
              class = "btn-action-primary btn-block",
              style = "font-weight:bold;",
              width = "100%"))
        ),
        br(),
        uiOutput(ns("ui_run_status"))
      )
    ),

    # ── Output files ─────────────────────────────────────────────────────────
    fluidRow(
      box(
        width = 12,
        title = div(style = box_title_style, icon("file-export"), " Output files"),
        solidHeader = TRUE, status = "primary",
        fluidRow(
          # File 1
          column(2,
            box(
              width = NULL,
              title = uiOutput(ns("ui_filename_1"), inline = TRUE),
              status = "primary",
              solidHeader = TRUE,
              "p_nulls per locus \u00d7 subsample",
              tags$br(),
              "Global weighted mean per locus",
              tags$br(), tags$br(),
              uiOutput(ns("ui_dl_file1"))
            )
          ),
          # File 2
          column(2,
            box(
              width = NULL,
              title = uiOutput(ns("ui_filename_2"), inline = TRUE),
              status = "success",
              solidHeader = TRUE,
              "Per locus + multilocus FST / FST-ENA",
              tags$br(),
              "CI over loci and over sub-samples",
              uiOutput(ns("ui_dl_file2"))
            )
          ),
          # File 3
          column(2,
            box(
              width = NULL,
              title = uiOutput(ns("ui_filename_3"), inline = TRUE),
              status = "info",
              solidHeader = TRUE,
              "FST, FST-ENA, DCSE, DCSE-INA",
              tags$br(),
              "Per pair of sub-samples, all loci combined",
              uiOutput(ns("ui_dl_file3"))
            )
          ),
          # File 4
          column(2,
            box(
              width = NULL,
              title = uiOutput(ns("ui_filename_4"), inline = TRUE),
              status = "warning",
              solidHeader = TRUE,
              "FST, FST-ENA, DCSE, DCSE-INA",
              tags$br(),
              "Half-matrix per locus, per pair",
              uiOutput(ns("ui_dl_file4"))
            )
          ),
          # File 5
          column(4,
            box(
              width = NULL,
              title = uiOutput(ns("ui_filename_5"), inline = TRUE),
              status = "danger",
              solidHeader = TRUE,
              "All bootstrap replicate values (over loci and over sub-samples)",
              tags$br(),
              uiOutput(ns("ui_dl_file5"))
              # plotly::plotlyOutput(ns("boot_dist_plot"), height="220px")
            )
          )
        )
      )
    ),

    # ── Results ──────────────────────────────────────────────────────────────
    fluidRow(
      box(
        width = 12,
        title = div(style = box_title_style, icon("chart-bar"), " Results"),
        solidHeader = TRUE, status = "primary",
        tabsetPanel(id = ns("na_tabs"), type = "tabs",

          # ── TAB 1: Null allele frequencies ──────────────────────────────── #
          tabPanel(title = tagList(" Null allele frequencies"),
                   value = "tab_na", br(),
            tags$div(class = "spg-method-note", style = "border-left-color:#0369a1;",
              "Reproduces FreeNA's own null-allele-frequency report: the EM algorithm ",
              "(Dempster, Laird & Rubin 1977) estimated per locus \u00d7 population below, ",
              "and the N-weighted per-locus summary (Av(p_nulls), Av(N_exp_blanks), ",
              "f(expBlanks), one-sided binomial test p-value, and chosen blank coding) further down."
            ),

            h4(" p_nulls per locus \u00d7 population (EM algorithm)", class = "section-title"),
            div(style = "overflow-x:auto;", DT::DTOutput(ns("dt_t1"))),

            br(),

            h4(" Per-locus summary (N-weighted mean, FreeNA report format)", class = "section-title"),
            div(style = "overflow-x:auto;", DT::DTOutput(ns("dt_t2")))
          ),

          # ── TAB 2: FST & FST-ENA ────────────────────────────────────────── #
          tabPanel(title = tagList(" FST / FST-ENA"),
                   value = "tab_fst", br(),

            tags$div(class = "spg-method-note", style = "border-left-color:#0369a1;",
              tags$strong("Global multilocus FST"), " \u2014 Weir & Cockerham (1984) unbiased moment estimator. ",
              tags$strong("FST-ENA"), ": EM-corrected frequencies, Excluding Null Alleles \u2014 Chapuis & Estoup (2007).",
              tags$br(),
              "Bootstrap CI over loci (resample loci with replacement, multilocus estimates only) and over ",
              "sub-samples (resample populations as whole blocks with replacement, available both for the ",
              "multilocus estimate and per locus \u2014 see the per-locus table below)."
            ),

            h4(" Per-locus FST and FST-ENA", class = "section-title"),
            div(style = "overflow-x:auto;", DT::DTOutput(ns("dt_fst_global"))),

            br(),

            h4(" Bootstrap CI \u2014 Global FST and FST-ENA", class = "section-title"),
            uiOutput(ns("ui_boot_global_fst")),

            br(),

            h4(" Pairwise FST and FST-ENA \u2014 lower triangle matrix", class = "section-title"),
            fluidRow(
              column(5,
                radioButtons(ns("fst_pair_display"), "Display:",
                  choices = c(
                    "Raw FST (uncorrected)" = "raw",
                    "FST-ENA (corrected)"   = "ena",
                    "Both side by side"     = "both"),
                  selected = "both", inline = TRUE))),
            uiOutput(ns("ui_fst_pair_matrix")),

            br(),

            h4(" Bootstrap CI \u2014 Pairwise FST-ENA (over loci)", class = "section-title"),
            uiOutput(ns("ui_boot_pair_fst"))
          ),

          # ── TAB 3: DCSE / DCSE-INA ──────────────────────────────────────── #
          tabPanel(title = tagList(" DCSE / DCSE-INA"),
                   value = "tab_dc", br(),

            tags$div(class = "spg-method-note", style = "border-left-color:#0369a1;",
              tags$strong("Cavalli-Sforza & Edwards (1967) chord distance."),
              " DCSE-INA includes the null allele as an extra state \u2014 Chapuis & Estoup (2007).",
              tags$br(),
              "DCSE(i,j) = (2/\u03c0)\u00d7\u221a[2\u00d7(1\u2212\u03a3\u221a(p_ik\u00d7p_jk))]  ",
              "INA: corrdgenefreq + null allele appended (freq = rd[locus, pop])."
            ),

            h4(" Pairwise DCSE and DCSE-INA \u2014 lower triangle matrix", class = "section-title"),
            fluidRow(
              column(5,
                radioButtons(ns("dc_display"), "Display:",
                  choices = c(
                    "Raw DCSE (uncorrected)" = "raw",
                    "DCSE-INA (corrected)"   = "ina",
                    "Both side by side"      = "both"),
                  selected = "both", inline = TRUE))),
            uiOutput(ns("ui_dc_matrix")),

            br(),

            h4(" Bootstrap CI \u2014 Pairwise DCSE-INA (over loci)", class = "section-title"),
            uiOutput(ns("ui_boot_pair_dc"))
          ),

          # ── TAB 4: Per-locus x pair ─────────────────────────────────────── #
          tabPanel(title = tagList(" Per-locus \u00d7 pair"),
                   value = "tab_locus_pair", br(),

            tags$div(class = "spg-method-note", style = "border-left-color:#0369a1;",
              "FST, FST-ENA, DCSE and DCSE-INA for each locus \u00d7 pair of populations.",
              " Useful for detecting outlier loci."
            ),

            fluidRow(
              column(3, selectInput(ns("fl_locus"), "Locus:",
                choices = c("All loci" = "all"), selected = "all")),
              column(3, selectInput(ns("fl_pop1"), "Population 1:",
                choices = c("All pairs" = "all"), selected = "all")),
              column(3, selectInput(ns("fl_pop2"), "Population 2:",
                choices = c("All pairs" = "all"), selected = "all"))
            ),

            h4(" FST and FST-ENA per locus \u00d7 pair", class = "section-title"),
            div(style = "overflow-x:auto;", DT::DTOutput(ns("dt_fst_locus"))),

            br(),

            h4(" DCSE and DCSE-INA per locus \u00d7 pair", class = "section-title"),
            div(style = "overflow-x:auto;", DT::DTOutput(ns("dt_dc_locus")))
          )
        )
      )
    )
  )
}