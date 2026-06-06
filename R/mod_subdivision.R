mod_subdivision_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    tags$head(gs_head()),

    module_banner("sitemap", "Population Subdivision \u2014 FST & G-test",
      "Population differentiation \u00b7 Weir & Cockerham (1984) \u00b7 Block bootstrap CI + permutation p-value \u00b7 G-based permutation test",
      "#B40F20"),
    tags$div(class = "spg-method-note", style = "border-left-color:#B40F20;",
      HTML(paste0(
        "Population subdivision: allele-frequency differences among populations (FST > 0). ",
        "HS and HT are also computed here \u2014 see the <b>Diversities</b> tab for locus bootstrap. ",
        "<br><br>",
        "<b>H<sub>0</sub> (FST):</b> FST = 0 (no differentiation). &nbsp;",
        "<b>Bootstrap:</b> population-block resampling; percentile CI. &nbsp;",
        "<b>Permutation (FST):</b> genotypes randomly reassigned among populations; one-sided test.",
        "<br>",
        "<b>H<sub>0</sub> (G-test):</b> allele frequencies homogeneous across populations. &nbsp;",
        "<b>Permutation (G):</b> G log-likelihood ratio statistic; population labels shuffled; ",
        "one-sided test (G &ge; G<sub>obs</sub>); FDR correction (Benjamini-Hochberg) per locus."
      ))
    ),

    # ==========================================================#
    # SECTION 1 — FST Bootstrap CI + permutation
    # ==========================================================#
    h2("1 \u2014 FST: Bootstrap CI & Permutation test", class = "section-title"),

    fluidRow(
      box(
        width = 12,
        title = div(style = "background-color:#FFFFFF; padding:10px; color:#333a43; font-weight:600;",
                    icon("sitemap"), "FST: CI & p-value parameters"),
        solidHeader = TRUE, status = "primary",
        fluidRow(
          column(3,
            h4(icon("sliders"), "Parameters"),
            numericInput(ns("n_perm_fst"),     "Number of Permutations:",        value = 5000,  min = 100,  max = 20000, step = 100),
            numericInput(ns("n_boot_fst"),     "Number of Bootstrap Replicates:", value = 5000,  min = 100,  max = 20000, step = 100),
            numericInput(ns("conf_level_fst"), "Confidence Level:",               value = 0.95,  min = 0.80, max = 0.99,  step = 0.01),
            actionButton(ns("run_FST_Analysis"), "Run FST Analysis",
                         icon = icon("rocket"),
                         class = "btn-action-primary btn-block", style = "font-weight:bold;"),
            tags$small(
              style = "color:#666; margin-top:6px; display:block;",
              icon("info-circle"),
              "Also computes HS, HT and locus bootstrap (see Genetic diversities tab)."
            )
          ),
          column(9,
            h4(icon("chart-line"), "FST Analysis Summary",
               style = "font-weight:600; color:#2c3e50; margin-bottom:15px;"),
            fluidRow(
              column(3,
                valueBoxOutput(ns("global_fst_box"),   width = NULL),
                valueBoxOutput(ns("fst_ci_width_box"), width = NULL)
              ),
              column(3,
                valueBoxOutput(ns("global_fst_pvalue_box"), width = NULL),
                valueBoxOutput(ns("fst_power_box"),         width = NULL)
              ),
              column(3,
                valueBoxOutput(ns("significant_loci_fst_box"), width = NULL),
                valueBoxOutput(ns("fst_convergence_box"),      width = NULL)
              ),
              column(3,
                valueBoxOutput(ns("analysis_time_fst_box"), width = NULL),
                valueBoxOutput(ns("fst_quality_box"),       width = NULL)
              )
            ),
            fluidRow(
              column(12,
                h5("Analysis Progress", style = "margin-top:15px; font-weight:600;"),
                shinyWidgets::progressBar(id = ns("fst_progress"), value = 0, title = "Overall Progress")
              )
            )
          )
        )
      )
    ),

    tags$p(HTML(paste0(
      "FST per locus with population-block bootstrap confidence intervals. ",
      "Permutation p-values derived from shuffling population labels (one-sided test, FST &ge; observed). ",
      "<br>HS and HT computed in the same run are reported in the ",
      "<b>Genetic Diversities</b> tab."
    )), style = "font-size:16px; line-height:1.5; color:#2c3e50;"),

    fluidRow(
      box(
        width = 12,
        title = div(style = "background-color:#FFFFFF; padding:10px; color:#333a43; font-weight:600;",
                    icon("table"), "FST Results"),
        solidHeader = TRUE, status = "primary",
        tabsetPanel(
          tabPanel("FST results",
            h4(icon("info-circle"), "Bootstrap confidence intervals"),
            p("FST per locus with population-block bootstrap CI and permutation p-values (one-sided)."),
            DTOutput(ns("fst_results_table")), br(),
            fluidRow(
              column(6, downloadButton(ns("download_fst_table"),     ".csv", class = "btn-download-primary btn-block")),
              column(6, downloadButton(ns("download_fst_table_txt"), ".txt", class = "btn-download-secondary btn-block"))
            )
          ),
          tabPanel("Visualization",
            h4(icon("chart-line"), "FST estimates by locus"),
            plotOutput(ns("fst_plot"), height = "400px"), br(),
            downloadButton(ns("download_fst_plot"), ".png", class = "btn-download-primary")
          )
        ),
        style = "padding:10px;"
      )
    ),

    # ==========================================================#
    # SECTION 2 — G-based permutation test
    # ==========================================================#
    tags$hr(style = "border-top:3px solid #B40F20; margin:30px 0;"),
    h2("2 \u2014 G-based Permutation Test (Subdivision)", class = "section-title"),
    tags$div(class = "spg-method-note", style = "border-left-color:#7c3aed;",
      HTML(paste0(
        "<b>G-statistic</b> (log-likelihood ratio): G = 2 &Sigma; n<sub>ijk</sub> ln(n<sub>ijk</sub> / e<sub>ijk</sub>). ",
        "Tests homogeneity of allele frequencies across populations. ",
        "<br>",
        "<b>Global test:</b> all loci pooled. &nbsp;",
        "<b>Per-locus test:</b> G per locus with FDR correction (Benjamini-Hochberg). &nbsp;",
        "<b>Pairwise test:</b> G for each pair of populations. &nbsp;",
        "<br>",
        "<b>Permutation scheme:</b> population labels randomly shuffled (",
        "individuals reassigned preserving sample sizes). ",
        "One-sided p-value: proportion of permuted G &ge; observed G."
      ))
    ),

    fluidRow(
      box(
        width = 12,
        title = div(style = "background-color:#FFFFFF; padding:10px; color:#333a43; font-weight:600;",
                    icon("flask"), "G-test: Parameters"),
        solidHeader = TRUE, status = "primary",
        fluidRow(
          column(3,
            h4(icon("sliders"), "Parameters"),
            numericInput(ns("n_perm_g"),
                         "Number of Permutations:",
                         value = 5000, min = 5000, max = 50000, step = 1000),
            numericInput(ns("conf_level_g"),
                         "Confidence Level:",
                         value = 0.95, min = 0.80, max = 0.99, step = 0.01),
            tags$hr(),
            h5(icon("check-square"), "Test types", style = "font-weight:600;"),
            checkboxInput(ns("g_test_global"),    "Global G-test",         value = TRUE),
            checkboxInput(ns("g_test_per_locus"), "Per-locus G-test",      value = TRUE),
            checkboxInput(ns("g_test_pairwise"),  "Pairwise G-test",       value = FALSE),
            tags$hr(),
            actionButton(ns("run_G_test"), "Run G-based Test",
                         icon  = icon("rocket"),
                         class = "btn-action-primary btn-block",
                         style = "font-weight:bold; background-color:#7c3aed; border-color:#7c3aed;"),
            tags$small(
              style = "color:#666; margin-top:6px; display:block;",
              icon("info-circle"),
              "Minimum 5 000 permutations required. Pairwise test is computationally intensive."
            )
          ),
          column(9,
            h4(icon("chart-area"), "G-test Summary",
               style = "font-weight:600; color:#2c3e50; margin-bottom:15px;"),
            fluidRow(
              column(3,
                valueBoxOutput(ns("g_global_obs_box"),    width = NULL),
                valueBoxOutput(ns("g_power_box"),         width = NULL)
              ),
              column(3,
                valueBoxOutput(ns("g_global_pvalue_box"), width = NULL),
                valueBoxOutput(ns("g_mean_pvalue_box"),   width = NULL)
              ),
              column(3,
                valueBoxOutput(ns("g_signif_loci_box"),   width = NULL),
                valueBoxOutput(ns("g_time_box"),          width = NULL)
              ),
              column(3,
                # Placeholder boxes for layout symmetry with FST panel
                valueBoxOutput(ns("g_n_perm_box"),        width = NULL),
                valueBoxOutput(ns("g_fdr_box"),           width = NULL)
              )
            ),
            fluidRow(
              column(12,
                h5("Analysis Progress", style = "margin-top:15px; font-weight:600;"),
                shinyWidgets::progressBar(id = ns("g_progress"), value = 0,
                                          title = "G-test permutation progress")
              )
            )
          )
        )
      )
    ),

    # ---- G-test results panels ----
    fluidRow(
      box(
        width = 12,
        title = div(style = "background-color:#FFFFFF; padding:10px; color:#333a43; font-weight:600;",
                    icon("table"), "G-test Results"),
        solidHeader = TRUE, status = "primary",

        tabsetPanel(

          # --- Global ---
          tabPanel("Global G-test",
            br(),
            h4(icon("globe"), "Global G-statistic"),
            p(HTML(paste0(
              "G computed over all loci and populations pooled. ",
              "H<sub>0</sub>: allele frequencies are homogeneous across all populations. ",
              "One-sided permutation p-value (G<sub>perm</sub> &ge; G<sub>obs</sub>)."
            ))),
            uiOutput(ns("g_global_result_ui")),
            br(),
            h5(icon("chart-bar"), "Null distribution (global G)"),
            plotOutput(ns("g_global_dist_plot"), height = "380px"), br(),
            fluidRow(
              column(6, downloadButton(ns("download_g_global_csv"), ".csv", class = "btn-download-primary btn-block")),
              column(6, downloadButton(ns("download_g_global_txt"), ".txt", class = "btn-download-secondary btn-block"))
            )
          ),

          # --- Per locus ---
          tabPanel("Per-locus G-test",
            br(),
            h4(icon("dna"), "G-statistic per locus"),
            p(HTML(paste0(
              "G computed independently for each locus. ",
              "FDR correction applied across loci (Benjamini-Hochberg). ",
              "Decision based on q-value &lt; 0.05."
            ))),
            DTOutput(ns("g_per_locus_table")), br(),
            fluidRow(
              column(6, downloadButton(ns("download_g_per_locus_csv"), ".csv", class = "btn-download-primary btn-block")),
              column(6, downloadButton(ns("download_g_per_locus_txt"), ".txt", class = "btn-download-secondary btn-block"))
            ),
            br(),
            h5(icon("chart-bar"), "G-statistic by locus (bar chart)"),
            plotOutput(ns("g_per_locus_plot"), height = "380px"), br(),
            downloadButton(ns("download_g_per_locus_plot"), ".png", class = "btn-download-primary")
          ),

          # --- Pairwise ---
          tabPanel("Pairwise G-test",
            br(),
            h4(icon("exchange-alt"), "Pairwise G-statistic"),
            p(HTML(paste0(
              "G computed for each pair of populations. ",
              "Enable the 'Pairwise G-test' checkbox above and re-run to populate this tab. ",
              "Upper triangle: G<sub>obs</sub>; lower triangle: p-value."
            ))),
            uiOutput(ns("g_pairwise_matrix_ui")),
            br(),
            DTOutput(ns("g_pairwise_table")), br(),
            fluidRow(
              column(6, downloadButton(ns("download_g_pairwise_csv"), ".csv", class = "btn-download-primary btn-block")),
              column(6, downloadButton(ns("download_g_pairwise_txt"), ".txt", class = "btn-download-secondary btn-block"))
            )
          ),

          # --- Diagnostics ---
          tabPanel("Diagnostics",
            br(),
            h4(icon("stethoscope"), "Permutation diagnostics"),
            fluidRow(
              column(6,
                h5("Q-Q plot: G null vs \u03c7\u00b2 theoretical"),
                p("Points on the diagonal indicate that the null G distribution is well approximated by a chi-squared."),
                plotOutput(ns("g_qq_plot"), height = "350px")
              ),
              column(6,
                h5("P-value convergence"),
                p("The cumulative global p-value should stabilise well before the last permutation."),
                plotOutput(ns("g_pvalue_convergence_plot"), height = "350px")
              )
            ),
            br(),
            downloadButton(ns("download_g_diagnostics_plot"), ".png — diagnostics", class = "btn-download-primary")
          )
        ),
        style = "padding:10px;"
      )
    )
  )
}