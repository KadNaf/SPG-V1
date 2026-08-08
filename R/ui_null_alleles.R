# module/ui_null_alleles.R
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

  tags$div(class = "na-module",
    
    # ── Banner (same style as other modules) ──────────────────────────────
    module_banner("null_alleles", "Null Allele Estimation",
      "EM algorithm · FST-ENA · DCSE-INA · Bootstrap confidence intervals",
      "#6B64EF"),
    
    # ── Value boxes ─────────────────────────────────────────────────────────
    fluidRow(
      valueBoxOutput(ns("vb_loci"), width = 2),
      valueBoxOutput(ns("vb_pops"), width = 2),
      valueBoxOutput(ns("vb_n"), width = 2),
      valueBoxOutput(ns("vb_avg_null"), width = 2),
      valueBoxOutput(ns("vb_max_null"), width = 2),
      valueBoxOutput(ns("vb_fst_ena"), width = 2)
    ),

    # ════════════════════════════════════════════════════════════════════════
    # SETUP PANEL
    # ════════════════════════════════════════════════════════════════════════
    fluidRow(
      box(
        width = 12,
        title = div(style = "background-color: #FFFFFF; padding: 10px; color: #333a43; font-weight: 600;",
                    icon("cogs"), "Setup"),
        solidHeader = TRUE, status = "primary",
        
        # ── (1) Missing genotype coding per locus ────────────────────────────
        div(style = "background: #fffbeb; border: 1px solid #fcd34d; border-radius: 7px; padding: .45rem .8rem; margin-bottom: .85rem;",
          tags$p(style = "margin:.25rem 0;",
            "Please choose how to code missing data for each locus:", tags$br(),
            tags$strong("0"), " = true missing data (ignored by the algorithm);", tags$br(),
            tags$strong("999999"), " = homozygote for allele 999 (code for all null alleles)"),
          tags$p(style = "margin:.5rem 0 0;font-weight:600;color:#92400e;",
            "Please make sure you do not already have any allele coded as 999.")
        ),
        uiOutput(ns("locus_coding_ui")),

        hr(),

        # ── (2) Bootstrap parameters ────────────────────────────────────────
        h4(icon("random"), "Bootstrap parameters"),
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
        tags$p(style = "color:#777;font-size:11px;",
          "Bootstrap resampling is random: point estimates (FST, FST-ENA, DCSE\u2026) never change, ",
          "but confidence interval bounds will shift slightly from run to run unless the seed is kept ",
          "the same. Re-run with the same seed, same data and same number of replicates to reproduce ",
          "the exact same confidence intervals \u2014 the seed used is recorded in every exported file."),

        hr(),

        # ── (3) Output files ─────────────────────────────────────────────────
        h4(icon("folder-open"), "Output files"),
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
        tags$p(style = "color:#777;font-size:11px;",
          "The root is proposed automatically from the name of the data file you imported ",
          "(e.g. ", tags$code("BoophilusAdultsDataCattle"), "), and you can freely edit or extend it ",
          "\u2014 e.g. add your own notes such as which loci were recoded to 999999. ",
          "File names = root + description (e.g. ", tags$code("<root>null_allele_frequencies.txt"),
          "). No date is added (already shown by the file explorer) \u2014 if you re-run with a ",
          "different missing-data coding and want to keep both results, add your own suffix below."),
        textInput(ns("out_suffix"), "Optional suffix to distinguish this run (e.g. \"1\"):", value = ""),
        tags$p(style = "color:#777;font-size:11px;",
          "Files are saved as tab-delimited ", tags$strong(".txt"), " (not .csv)."),

        hr(),

        # ── (4) Run computation ─────────────────────────────────────────────
        h4(icon("play"), "Run all computations + generate output files"),
        fluidRow(
          column(4,
            actionButton(ns("run_all"),
              label = tagList(icon("rocket"), tags$strong(" Compute + Bootstrap + Export")),
              class = "btn-action-primary",
              width = "100%"))
        ),
        br(),
        uiOutput(ns("ui_run_status"))
      )
    ),

    # ════════════════════════════════════════════════════════════════════════
    # OUTPUT FILES PANEL
    # ════════════════════════════════════════════════════════════════════════
    fluidRow(
      box(
        width = 12,
        title = div(style = "background-color: #FFFFFF; padding: 10px; color: #333a43; font-weight: 600;",
                    icon("file-export"), "Output files"),
        solidHeader = TRUE, status = "primary",
        
        fluidRow(
          # File 1
          column(2,
            div(style = "background: #eff6ff; border: 1px solid #bfdbfe; border-radius: 7px; padding: .5rem;",
              h6(uiOutput(ns("ui_filename_1"), inline = TRUE), style = "color: #1d4ed8; font-weight: 600;"),
              p("p_nulls per locus × subsample", style = "font-size: 11px; color: #334155;"),
              p("Global weighted mean per locus", style = "font-size: 11px; color: #334155;"),
              br(),
              uiOutput(ns("ui_dl_file1"))
            )
          ),
          # File 2
          column(2,
            div(style = "background: #f0fdfa; border: 1px solid #99f6e4; border-radius: 7px; padding: .5rem;",
              h6(uiOutput(ns("ui_filename_2"), inline = TRUE), style = "color: #0d9488; font-weight: 600;"),
              p("Per locus + multilocus FST / FST-ENA", style = "font-size: 11px; color: #334155;"),
              p("CI over loci and over sub-samples", style = "font-size: 11px; color: #334155;"),
              uiOutput(ns("ui_dl_file2"))
            )
          ),
          # File 3
          column(2,
            div(style = "background: #faf5ff; border: 1px solid #e9d5ff; border-radius: 7px; padding: .5rem;",
              h6(uiOutput(ns("ui_filename_3"), inline = TRUE), style = "color: #7c3aed; font-weight: 600;"),
              p("FST, FST-ENA, DCSE, DCSE-INA", style = "font-size: 11px; color: #334155;"),
              p("Per pair of sub-samples, all loci combined", style = "font-size: 11px; color: #334155;"),
              uiOutput(ns("ui_dl_file3"))
            )
          ),
          # File 4
          column(2,
            div(style = "background: #fffbeb; border: 1px solid #fcd34d; border-radius: 7px; padding: .5rem;",
              h6(uiOutput(ns("ui_filename_4"), inline = TRUE), style = "color: #92400e; font-weight: 600;"),
              p("FST, FST-ENA, DCSE, DCSE-INA", style = "font-size: 11px; color: #334155;"),
              p("Half-matrix per locus, per pair", style = "font-size: 11px; color: #334155;"),
              uiOutput(ns("ui_dl_file4"))
            )
          ),
          # File 5
          column(4,
            div(style = "background: #fef2f2; border: 1px solid #fca5a5; border-radius: 7px; padding: .5rem;",
              h6(uiOutput(ns("ui_filename_5"), inline = TRUE), style = "color: #991b1b; font-weight: 600;"),
              p("All bootstrap replicate values (over loci and over sub-samples)", style = "font-size: 11px; color: #334155;"),
              uiOutput(ns("ui_dl_file5"))
            )
          )
        )
      )
    ),

    # ════════════════════════════════════════════════════════════════════════
    # RESULTS TABS — for visual inspection
    # ════════════════════════════════════════════════════════════════════════
    tabsetPanel(id = ns("na_tabs"), type = "tabs",

      # ── TAB 1: Null allele frequencies ────────────────────────────────── #
      tabPanel(title = tagList(icon("chart-bar"), " Null allele frequencies"),
               value = "tab_na", br(),
        
        div(style = "background: #eff6ff; border: 1px solid #bfdbfe; border-radius: 7px; padding: .45rem .8rem; margin-bottom: .85rem; font-size: 11.5px; color: #1d4ed8; line-height: 1.65;",
          "Reproduces FreeNA's own null-allele-frequency report: the EM algorithm ",
          "(Dempster, Laird & Rubin 1977) estimated per locus \u00d7 population below, ",
          "and the N-weighted per-locus summary (Av(p_nulls), Av(N_exp_blanks), ",
          "f(expBlanks), one-sided binomial test p-value, and chosen blank coding) further down."
        ),
        
        fluidRow(
          box(
            width = 12,
            title = div(style = "background-color: #FFFFFF; padding: 10px; color: #333a43; font-weight: 600;",
                        icon("table"), "p_nulls per locus × population (EM algorithm)"),
            solidHeader = TRUE, status = "primary",
            DT::DTOutput(ns("dt_t1"))
          )
        ),
        
        br(),
        
        fluidRow(
          box(
            width = 12,
            title = div(style = "background-color: #FFFFFF; padding: 10px; color: #333a43; font-weight: 600;",
                        icon("chart-line"), "Per-locus summary (N-weighted mean, FreeNA report format)"),
            solidHeader = TRUE, status = "primary",
            DT::DTOutput(ns("dt_t2"))
          )
        )
      ),

      # ── TAB 2: FST & FST-ENA ──────────────────────────────────────────── #
      tabPanel(title = tagList(icon("project-diagram"), " FST / FST-ENA"),
               value = "tab_fst", br(),

        div(style = "background: #eff6ff; border: 1px solid #bfdbfe; border-radius: 7px; padding: .45rem .8rem; margin-bottom: .85rem; font-size: 11.5px; color: #1d4ed8; line-height: 1.65;",
          tags$strong("Global multilocus FST"), " \u2014 Weir & Cockerham (1984) unbiased moment estimator. ",
          tags$strong("FST-ENA"), ": EM-corrected frequencies, Excluding Null Alleles \u2014 Chapuis & Estoup (2007).",
          tags$br(),
          "Bootstrap CI over loci (resample loci with replacement, multilocus estimates only) and over ",
          "sub-samples (resample populations as whole blocks with replacement, available both for the ",
          "multilocus estimate and per locus \u2014 see the per-locus table below)."
        ),

        fluidRow(
          box(
            width = 12,
            title = div(style = "background-color: #FFFFFF; padding: 10px; color: #333a43; font-weight: 600;",
                        icon("table"), "Per-locus FST and FST-ENA"),
            solidHeader = TRUE, status = "primary",
            DT::DTOutput(ns("dt_fst_global"))
          )
        ),
        
        br(),

        fluidRow(
          box(
            width = 12,
            title = div(style = "background-color: #FFFFFF; padding: 10px; color: #333a43; font-weight: 600;",
                        icon("random"), "Bootstrap CI — Global FST and FST-ENA"),
            solidHeader = TRUE, status = "primary",
            uiOutput(ns("ui_boot_global_fst"))
          )
        ),
        
        br(),

        fluidRow(
          box(
            width = 12,
            title = div(style = "background-color: #FFFFFF; padding: 10px; color: #333a43; font-weight: 600;",
                        icon("th"), "Pairwise FST and FST-ENA — lower triangle matrix"),
            solidHeader = TRUE, status = "primary",
            fluidRow(
              column(5,
                radioButtons(ns("fst_pair_display"), "Display:",
                  choices = c(
                    "Raw FST (uncorrected)" = "raw",
                    "FST-ENA (corrected)"   = "ena",
                    "Both side by side"     = "both"),
                  selected = "both", inline = TRUE))
            ),
            uiOutput(ns("ui_fst_pair_matrix"))
          )
        ),
        
        br(),

        fluidRow(
          box(
            width = 12,
            title = div(style = "background-color: #FFFFFF; padding: 10px; color: #333a43; font-weight: 600;",
                        icon("random"), "Bootstrap CI — Pairwise FST-ENA (over loci)"),
            solidHeader = TRUE, status = "primary",
            uiOutput(ns("ui_boot_pair_fst"))
          )
        )
      ),

      # ── TAB 3: DCSE / DCSE-INA ────────────────────────────────────────── #
      tabPanel(title = tagList(icon("ruler-combined"), " DCSE / DCSE-INA"),
               value = "tab_dc", br(),

        div(style = "background: #eff6ff; border: 1px solid #bfdbfe; border-radius: 7px; padding: .45rem .8rem; margin-bottom: .85rem; font-size: 11.5px; color: #1d4ed8; line-height: 1.65;",
          tags$strong("Cavalli-Sforza & Edwards (1967) chord distance."),
          " DCSE-INA includes the null allele as an extra state \u2014 Chapuis & Estoup (2007).",
          tags$br(),
          "DCSE(i,j) = (2/\u03c0)\u00d7\u221a[2\u00d7(1\u2212\u03a3\u221a(p_ik\u00d7p_jk))]  ",
          "INA: corrdgenefreq + null allele appended (freq = rd[locus, pop])."
        ),

        fluidRow(
          box(
            width = 12,
            title = div(style = "background-color: #FFFFFF; padding: 10px; color: #333a43; font-weight: 600;",
                        icon("th"), "Pairwise DCSE and DCSE-INA — lower triangle matrix"),
            solidHeader = TRUE, status = "primary",
            fluidRow(
              column(5,
                radioButtons(ns("dc_display"), "Display:",
                  choices = c(
                    "Raw DCSE (uncorrected)" = "raw",
                    "DCSE-INA (corrected)"   = "ina",
                    "Both side by side"      = "both"),
                  selected = "both", inline = TRUE))
            ),
            uiOutput(ns("ui_dc_matrix"))
          )
        ),
        
        br(),

        fluidRow(
          box(
            width = 12,
            title = div(style = "background-color: #FFFFFF; padding: 10px; color: #333a43; font-weight: 600;",
                        icon("random"), "Bootstrap CI — Pairwise DCSE-INA (over loci)"),
            solidHeader = TRUE, status = "primary",
            uiOutput(ns("ui_boot_pair_dc"))
          )
        )
      ),

      # ── TAB 4: Per-locus x pair ───────────────────────────────────────── #
      tabPanel(title = tagList(icon("table"), " Per-locus × pair"),
               value = "tab_locus_pair", br(),

        div(style = "background: #eff6ff; border: 1px solid #bfdbfe; border-radius: 7px; padding: .45rem .8rem; margin-bottom: .85rem; font-size: 11.5px; color: #1d4ed8; line-height: 1.65;",
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

        fluidRow(
          box(
            width = 12,
            title = div(style = "background-color: #FFFFFF; padding: 10px; color: #333a43; font-weight: 600;",
                        icon("table"), "FST and FST-ENA per locus × pair"),
            solidHeader = TRUE, status = "primary",
            DT::DTOutput(ns("dt_fst_locus"))
          )
        ),
        
        br(),

        fluidRow(
          box(
            width = 12,
            title = div(style = "background-color: #FFFFFF; padding: 10px; color: #333a43; font-weight: 600;",
                        icon("table"), "DCSE and DCSE-INA per locus × pair"),
            solidHeader = TRUE, status = "primary",
            DT::DTOutput(ns("dt_dc_locus"))
          )
        )
      )

    ) # end tabsetPanel
  )   # end tags$div
}