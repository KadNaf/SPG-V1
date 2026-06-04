# mod_subdivision.R
# Tab: Subdivision
# Population differentiation - FST (Weir & Cockerham): bootstrap CI + permutation test.
# G-based permutation test for subdivision significance.
# Note: HS and HT are also computed here but displayed in the Genetic diversities tab.
# Golem module UI - server: server_general_stats("general_stats", rv)

mod_subdivision_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    tags$head(gs_head()),

    # ── CSS spécifique pour le test G-based ─────────────────────────────
    tags$style(HTML("
      .g-based-panel {
        border-left: 4px solid #5b21b6 !important;
        background: linear-gradient(135deg, #faf5ff 0%, #ffffff 100%);
      }
      .g-based-header {
        background: linear-gradient(135deg, #5b21b6 0%, #7c3aed 100%) !important;
        color: #fff !important;
      }
      .g-based-info {
        background: #f5f3ff;
        border: 1px solid #c4b5fd;
        border-radius: 7px;
        padding: .6rem .9rem;
        font-size: 12px;
        color: #5b21b6;
        line-height: 1.7;
        margin-bottom: .9rem;
      }
      .g-based-formula {
        background: #fafaf9;
        border: 1px solid #d6d3d1;
        border-radius: 7px;
        padding: .55rem .85rem;
        font-size: 11px;
        color: #292524;
        font-family: 'IBM Plex Mono', monospace;
        margin-bottom: .9rem;
        line-height: 1.8;
      }
      .g-value-box {
        border-left: 4px solid #7c3aed !important;
      }
    ")),

    module_banner("sitemap", "Population Subdivision \u2014 FST & G-based test",
      "Population differentiation \u00b7 Weir & Cockerham (1984) \u00b7 G-test permutation \u00b7 Block bootstrap CI",
      "#B40F20"),

    tags$div(class = "spg-method-note", style = "border-left-color:#B40F20;",
      HTML(paste0(
        "Population subdivision: allele-frequency differences among populations (FST > 0). ",
        "HS and HT are also computed here \u2014 see the <b>Diversities</b> tab for locus bootstrap. ",
        "<br><br>",
        "<b>H<sub>0</sub>:</b> FST = 0 (no differentiation). &nbsp;",
        "<b>Bootstrap:</b> population-block resampling; percentile CI. &nbsp;",
        "<b>Permutation (FST):</b> genotypes randomly reassigned among populations; one-sided test. ",
        "<br>",
        "<b>Permutation (G-based):</b> likelihood-ratio G-statistic ; distribution built by shuffling individuals among populations ; ",
        "tests whether observed subdivision is stronger than expected by chance alone."
      ))
    ),

    # ═══════════════════════════════════════════════════════════════════
    # SECTION 1 : Paramètres FST (existant)
    # ═══════════════════════════════════════════════════════════════════
    fluidRow(
      box(
        width = 12,
        title = div(style = "background-color: #FFFFFF; padding: 10px; color: #333a43; font-weight: 600;",
                    icon("sitemap"),
                    "FST: CI & p-value parameters"),
        solidHeader = TRUE, status = "primary",
        fluidRow(
          column(3,
            h4(icon("sliders"), "Parameters"),
            numericInput(ns("n_perm_fst"),    "Number of Permutations:",        value = 5000, min = 100, max = 20000, step = 100),
            numericInput(ns("n_boot_fst"),    "Number of Bootstrap Replicates:", value = 5000, min = 100, max = 20000, step = 100),
            numericInput(ns("conf_level_fst"),"Confidence Level:",               value = 0.95, min = 0.80, max = 0.99, step = 0.01),
            actionButton(ns("run_FST_Analysis"), "Run FST Analysis",
                         icon = icon("rocket"),
                         class = "btn-action-primary btn-block", style = "font-weight: bold;"),
            tags$small(
              style = "color: #666; margin-top: 6px; display: block;",
              icon("info-circle"),
              "Also computes HS, HT and locus bootstrap (see Genetic diversities tab)."
            )
          ),
          column(9,
            h4(icon("chart-line"), "FST Analysis Summary",
               style = "font-weight: 600; color: #2c3e50; margin-bottom: 15px;"),
            fluidRow(
              column(3,
                valueBoxOutput(ns("global_fst_box"),       width = NULL),
                valueBoxOutput(ns("fst_ci_width_box"),     width = NULL)
              ),
              column(3,
                valueBoxOutput(ns("global_fst_pvalue_box"),width = NULL),
                valueBoxOutput(ns("fst_power_box"),        width = NULL)
              ),
              column(3,
                valueBoxOutput(ns("significant_loci_fst_box"),  width = NULL),
                valueBoxOutput(ns("fst_convergence_box"),       width = NULL)
              ),
              column(3,
                valueBoxOutput(ns("analysis_time_fst_box"),width = NULL),
                valueBoxOutput(ns("fst_quality_box"),      width = NULL)
              )
            ),
            fluidRow(
              column(12,
                h5("Analysis Progress", style = "margin-top: 15px; font-weight: 600;"),
                shinyWidgets::progressBar(id = ns("fst_progress"), value = 0,
                                          title = "Overall Progress")
              )
            )
          )
        )
      )
    ),

    # ═══════════════════════════════════════════════════════════════════
    # SECTION 2 : Paramètres du test G-based (NOUVEAU)
    # ═══════════════════════════════════════════════════════════════════
    fluidRow(
      box(
        width = 12,
        title = div(style = "background: linear-gradient(135deg, #5b21b6 0%, #7c3aed 100%); padding: 10px; color: #fff; font-weight: 600;",
                    icon("chart-area"),
                    "G-based permutation test \u2014 Subdivision significance"),
        solidHeader = TRUE, status = "primary",
        class = "g-based-panel",
        fluidRow(
          column(4,
            h4(icon("cogs"), "G-test parameters",
               style = "color: #5b21b6; font-weight: 600;"),
            tags$div(class = "g-based-info",
              icon("lightbulb"),
              HTML(paste0(
                "<b>What is the G-statistic?</b><br>",
                "G is the likelihood-ratio statistic measuring allele-frequency divergence among populations. ",
                "Unlike FST (ANOVA-based), G is derived from the G-test of independence (Sokal & Rohlf, 1981) ",
                "and has better statistical properties for small samples.<br><br>",
                "<b>Permutation principle:</b> individuals are randomly reassigned among populations ",
                "(preserving original sample sizes). The distribution of G under H<sub>0</sub> is built empirically. ",
                "The <b>p-value</b> is the proportion of permuted G values \u2265 the observed G."
              ))
            ),
            tags$div(class = "g-based-formula",
              tags$strong("G-statistic :"),
              tags$br(),
              "G = 2 \u00d7 \u03a3\u1d62 \u03a3\u2c7c \u03a3\u2098  n\u1d62\u2c7c\u2098 \u00d7 ln(n\u1d62\u2c7c\u2098 / e\u1d62\u2c7c\u2098)",
              tags$br(),
              "where e\u1d62\u2c7c\u2098 = (row total) \u00d7 (column total) / grand total",
              tags$br(),
              tags$strong("p-value = (count(G_perm \u2265 G_obs) + 1) / (B + 1)")
            ),
            numericInput(ns("n_perm_g"), "Number of permutations (B):",
                         value = 5000, min = 500, max = 50000, step = 500),
            numericInput(ns("conf_level_g"), "Confidence level for G CI:",
                         value = 0.95, min = 0.80, max = 0.99, step = 0.01),
            checkboxInput(ns("g_test_global"), "Global G-test (all loci)",   value = TRUE),
            checkboxInput(ns("g_test_per_locus"), "Per-locus G-test",        value = TRUE),
            checkboxInput(ns("g_test_pairwise"), "Pairwise G-test (populations)", value = FALSE),
            tags$hr(style = "border-color: #c4b5fd;"),
            actionButton(ns("run_G_test"), "Run G-based permutation test",
                         icon = icon("chart-area"),
                         class = "btn-block",
                         style = "font-weight: bold; background: linear-gradient(135deg, #5b21b6, #7c3aed); color: #fff; border: none;"),
            tags$small(
              style = "color: #666; margin-top: 6px; display: block;",
              icon("info-circle"),
              "Permutation of individuals among populations (not loci). ",
              "Minimum 500 permutations recommended for stable p-values."
            )
          ),
          column(8,
            h4(icon("chart-bar"), "G-test Summary",
               style = "font-weight: 600; color: #5b21b6; margin-bottom: 15px;"),
            fluidRow(
              column(4,
                valueBoxOutput(ns("g_global_obs_box"),   width = NULL),
                valueBoxOutput(ns("g_global_pvalue_box"), width = NULL)
              ),
              column(4,
                valueBoxOutput(ns("g_signif_loci_box"),  width = NULL),
                valueBoxOutput(ns("g_mean_pvalue_box"),  width = NULL)
              ),
              column(4,
                valueBoxOutput(ns("g_time_box"),         width = NULL),
                valueBoxOutput(ns("g_power_box"),        width = NULL)
              )
            ),
            fluidRow(
              column(12,
                h5("G-test Progress", style = "margin-top: 15px; font-weight: 600; color: #5b21b6;"),
                shinyWidgets::progressBar(id = ns("g_progress"), value = 0,
                                          title = "Permutation progress")
              )
            ),
            tags$div(class = "g-based-info", style = "margin-top: 15px;",
              icon("info-circle"),
              HTML(paste0(
                "<b>Interpretation:</b> ",
                "If <b>p &lt; 0.05</b>, reject H<sub>0</sub> and conclude that subdivision is statistically significant. ",
                "The G-test is more powerful than FST-based permutation for small sample sizes and provides ",
                "a direct likelihood-based measure of differentiation."
              ))
            )
          )
        )
      )
    ),

    # ═══════════════════════════════════════════════════════════════════
    # SECTION 3 : Résultats FST (existant)
    # ═══════════════════════════════════════════════════════════════════
    h2("FST \u2014 Bootstrap CI and permutation results", class = "section-title"),
    tags$p(HTML(paste0(
      "FST per locus with population-block bootstrap confidence intervals. ",
      "Permutation p-values derived from shuffling population labels (one-sided test, FST &ge; observed). ",
      "<br>HS and HT computed in the same run are reported in the ",
      "<b>Genetic Diversities</b> tab."
    )), style = "font-size: 16px; line-height: 1.5; color: #2c3e50;"),

    fluidRow(
      box(
        width = 12,
        title = div(style = "background-color: #FFFFFF; padding: 10px; color: #333a43; font-weight: 600;",
                    icon("table"),
                    "FST Results"),
        solidHeader = TRUE, status = "primary",
        tabsetPanel(
          tabPanel("FST results",
            h4(icon("info-circle"), "Bootstrap confidence intervals"),
            p("FST per locus with population-block bootstrap CI and
              permutation p-values (population labels shuffled, one-sided test)."),
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
        style = "padding: 10px;"
      )
    ),

    # ═══════════════════════════════════════════════════════════════════
    # SECTION 4 : Résultats du test G-based (NOUVEAU)
    # ═══════════════════════════════════════════════════════════════════
    h2("G-based permutation test \u2014 Subdivision significance",
       class = "section-title",
       style = "color: #5b21b6; border-bottom: 2px solid #7c3aed;"),
    tags$p(HTML(paste0(
      "Likelihood-ratio G-statistic with empirical null distribution built by ",
      "<b>permuting individuals among populations</b> (preserving sample sizes). ",
      "Tests whether the observed genetic subdivision is stronger than expected by chance alone. ",
      "<br><b>One-sided test:</b> p-value = P(G<sub>perm</sub> &ge; G<sub>obs</sub>)."
    )), style = "font-size: 16px; line-height: 1.5; color: #2c3e50;"),

    fluidRow(
      box(
        width = 12,
        title = div(style = "background: linear-gradient(135deg, #5b21b6 0%, #7c3aed 100%); padding: 10px; color: #fff; font-weight: 600;",
                    icon("chart-area"),
                    "G-based permutation test results"),
        solidHeader = TRUE, status = "primary",
        class = "g-based-panel",
        tabsetPanel(
          id = ns("g_test_tabs"),

          # ── Tab 1: Global G-test ─────────────────────────────────────
          tabPanel(
            title = tagList(icon("globe"), " Global G-test"),
            br(),
            tags$div(class = "g-based-info",
              icon("info-circle"),
              HTML(paste0(
                "<b>Global G-test:</b> multilocus statistic summed across all loci. ",
                "Tests whether the overall subdivision across the genome is significant. ",
                "The null distribution is built by permuting individuals among populations ",
                "and recomputing G on the permuted dataset."
              ))
            ),
            fluidRow(
              column(6,
                h5("Global G-test result", style = "color: #5b21b6; font-weight: 600;"),
                uiOutput(ns("g_global_result_ui")),
                tags$hr(),
                tags$div(class = "g-based-formula",
                  tags$strong("Decision rule:"),
                  tags$br(),
                  "If p-value < \u03b1 (e.g. 0.05): reject H\u2080 \u2192 significant subdivision",
                  tags$br(),
                  "If p-value \u2265 \u03b1: fail to reject H\u2080 \u2192 no evidence of subdivision"
                )
              ),
              column(6,
                h5("Null distribution of G", style = "color: #5b21b6; font-weight: 600;"),
                plotOutput(ns("g_global_dist_plot"), height = "320px"),
                tags$small(style = "color: #666;",
                  icon("info-circle"),
                  "Histogram of G under H\u2080 (permutations). Red line = observed G."
                )
              )
            ),
            tags$hr(style = "border-color: #c4b5fd;"),
            fluidRow(
              column(6, downloadButton(ns("download_g_global_csv"), "Download global G-test (.csv)",
                                       class = "btn-download-primary btn-block")),
              column(6, downloadButton(ns("download_g_global_txt"), "Download global G-test (.txt)",
                                       class = "btn-download-secondary btn-block"))
            )
          ),

          # ── Tab 2: Per-locus G-test ──────────────────────────────────
          tabPanel(
            title = tagList(icon("dna"), " Per-locus G-test"),
            br(),
            tags$div(class = "g-based-info",
              icon("info-circle"),
              HTML(paste0(
                "<b>Per-locus G-test:</b> G-statistic computed separately for each locus. ",
                "Identifies loci that contribute most to subdivision (potential outlier loci under selection). ",
                "P-values are corrected for multiple testing using the <b>Benjamini-Hochberg FDR</b> method."
              ))
            ),
            fluidRow(
              column(12,
                h5("G-test results per locus", style = "color: #5b21b6; font-weight: 600;"),
                DTOutput(ns("g_per_locus_table")),
                tags$small(style = "color: #666; margin-top: 6px; display: block;",
                  icon("info-circle"),
                  "Columns: Locus, G_obs, df, p-value (raw), q-value (FDR-adjusted), decision at \u03b1 = 0.05."
                ),
                tags$hr(),
                fluidRow(
                  column(6, downloadButton(ns("download_g_per_locus_csv"),
                                           "Download per-locus G-test (.csv)",
                                           class = "btn-download-primary btn-block")),
                  column(6, downloadButton(ns("download_g_per_locus_txt"),
                                           "Download per-locus G-test (.txt)",
                                           class = "btn-download-secondary btn-block"))
                )
              )
            ),
            tags$hr(style = "border-color: #c4b5fd;"),
            fluidRow(
              column(12,
                h5("Per-locus G visualization", style = "color: #5b21b6; font-weight: 600;"),
                plotOutput(ns("g_per_locus_plot"), height = "400px"),
                tags$small(style = "color: #666;",
                  icon("info-circle"),
                  "G_obs per locus (bars) with significance threshold (dashed line at -ln(0.05) \u00d7 2)."
                ),
                downloadButton(ns("download_g_per_locus_plot"), "Download plot (.png)",
                               class = "btn-download-primary")
              )
            )
          ),

          # ── Tab 3: Pairwise G-test (optionnel) ───────────────────────
          tabPanel(
            title = tagList(icon("exchange-alt"), " Pairwise G-test"),
            br(),
            tags$div(class = "g-based-info",
              icon("info-circle"),
              HTML(paste0(
                "<b>Pairwise G-test:</b> G-statistic computed for each pair of populations. ",
                "Identifies which population pairs are significantly differentiated. ",
                "Enable the checkbox 'Pairwise G-test' above and re-run the analysis."
              ))
            ),
            fluidRow(
              column(12,
                h5("Pairwise G-test matrix", style = "color: #5b21b6; font-weight: 600;"),
                uiOutput(ns("g_pairwise_matrix_ui")),
                tags$hr(),
                h5("Pairwise G-test \u2014 long format", style = "color: #5b21b6; font-weight: 600;"),
                DTOutput(ns("g_pairwise_table")),
                fluidRow(
                  column(6, downloadButton(ns("download_g_pairwise_csv"),
                                           "Download pairwise G-test (.csv)",
                                           class = "btn-download-primary btn-block")),
                  column(6, downloadButton(ns("download_g_pairwise_txt"),
                                           "Download pairwise G-test (.txt)",
                                           class = "btn-download-secondary btn-block"))
                )
              )
            )
          ),

          # ── Tab 4: Diagnostic plots ──────────────────────────────────
          tabPanel(
            title = tagList(icon("chart-bar"), " Diagnostics"),
            br(),
            tags$div(class = "g-based-info",
              icon("info-circle"),
              HTML(paste0(
                "<b>Diagnostic plots:</b> check the behavior of the permutation test. ",
                "The null distribution should be approximately \u03c7\u00b2-distributed under H<sub>0</sub>. ",
                "The P-P plot compares the empirical distribution to the theoretical \u03c7\u00b2."
              ))
            ),
            fluidRow(
              column(6,
                h5("Null distribution vs \u03c7\u00b2 theoretical",
                   style = "color: #5b21b6; font-weight: 600;"),
                plotOutput(ns("g_qq_plot"), height = "380px"),
                tags$small(style = "color: #666;",
                  icon("info-circle"),
                  "Q-Q plot: empirical quantiles vs theoretical \u03c7\u00b2 quantiles."
                )
              ),
              column(6,
                h5("P-value convergence",
                   style = "color: #5b21b6; font-weight: 600;"),
                plotOutput(ns("g_pvalue_convergence_plot"), height = "380px"),
                tags$small(style = "color: #666;",
                  icon("info-circle"),
                  "Evolution of the p-value estimate as permutations accumulate. ",
                  "Should stabilize after a few thousand permutations."
                )
              )
            ),
            fluidRow(
              column(12,
                downloadButton(ns("download_g_diagnostics_plot"),
                               "Download diagnostic plots (.png)",
                               class = "btn-download-primary")
              )
            )
          )
        ),
        style = "padding: 10px;"
      )
    ),

    # ═══════════════════════════════════════════════════════════════════
    # SECTION 5 : Note méthodologique (NOUVEAU)
    # ═══════════════════════════════════════════════════════════════════
    fluidRow(
      box(
        width = 12,
        title = div(style = "background-color: #FFFFFF; padding: 10px; color: #333a43; font-weight: 600;",
                    icon("book"),
                    "Methodological notes"),
        solidHeader = TRUE, status = "info",
        collapsible = TRUE, collapsed = TRUE,
        HTML(paste0(
          "<h4 style='color:#B40F20;'>FST \u2014 Weir & Cockerham (1984)</h4>",
          "<ul>",
          "<li><b>Bootstrap CI:</b> population-block resampling (resampling individuals within each population, ",
          "preserving population structure). Percentile method for CI construction.</li>",
          "<li><b>Permutation test:</b> individuals randomly reassigned among populations (preserving sample sizes). ",
          "One-sided test: p = P(FST<sub>perm</sub> \u2265 FST<sub>obs</sub>).</li>",
          "</ul>",
          "<h4 style='color:#5b21b6;'>G-based test \u2014 Sokal & Rohlf (1981)</h4>",
          "<ul>",
          "<li><b>G-statistic:</b> likelihood-ratio test of independence between allele frequencies and populations. ",
          "G = 2 \u00d7 \u03a3 n<sub>ijk</sub> \u00d7 ln(n<sub>ijk</sub> / e<sub>ijk</sub>).</li>",
          "<li><b>Permutation principle:</b> identical to FST permutation (individuals shuffled among populations), ",
          "but the test statistic is G instead of FST.</li>",
          "<li><b>Advantages over FST:</b> better statistical properties for small samples, ",
          "directly based on likelihood theory, asymptotically \u03c7\u00b2-distributed under H<sub>0</sub>.</li>",
          "<li><b>Multiple testing:</b> per-locus p-values are corrected using Benjamini-Hochberg FDR ",
          "(controls the false discovery rate at 5%).</li>",
          "</ul>",
          "<h4>References</h4>",
          "<ul>",
          "<li>Weir, B.S. & Cockerham, C.C. (1984) Estimating F-statistics for the analysis of population structure. ",
          "<i>Evolution</i> 38: 1358\u20131370.</li>",
          "<li>Sokal, R.R. & Rohlf, F.J. (1981) Biometry. W.H. Freeman, New York.</li>",
          "<li>Benjamini, Y. & Hochberg, Y. (1995) Controlling the false discovery rate. ",
          "<i>Journal of the Royal Statistical Society B</i> 57: 289\u2013300.</li>",
          "</ul>"
        ))
      )
    )
  )
}