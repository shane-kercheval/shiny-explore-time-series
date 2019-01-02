library(shiny)
library(shinyWidgets)
library(shinyjs)
library(shinyBS)

source('helper_scripts/definitions.R')

shinyUI(fluidPage(theme="custom.css",

    useShinyjs(),

    titlePanel("Explore Time Series"),
    navlistPanel(
        tabPanel(
            "Load Dataset",
            tabsetPanel(type='tabs',
                tabPanel(
                    "Preloaded Dataset",
                    tags$br(),
                    tags$div(style='margin-bottom: 40px !important',
                        selectInput(width='750px',
                                    inputId='preloaded_dataset',
                                          label="Choose a Dataset:",
                                          choices=c("Anti-Diabetic Drug Sales in Australia from 1991 to 2008",
                                                    "Total Weekly Air Passenger #s flights (Ansett) Melbourne/Sydney, 1987–1992.",
                                                    "Half-hourly/Daily Electricity Demand for Victoria, Australia, in 2014",
                                                    "Quarterly Visitor Nights for Various Regions of Australia",
                                                    "Quarterly Australian Beer production",
                                                    "Australian monthly electricity production: Jan 1956 – Aug 1995.",
                                                    "International Arrivals to Australia",
                                                    "Monthly Total # of Pigs Slaughtered in Victoria, Australia (Jan 1980 – Aug 1995)",
                                                    "Daily Closing Stock Prices of Google Inc (All)",
                                                    "Daily Closing Stock Prices of Google Inc (200)",
                                                    "Price of dozen eggs in US, 1900–1993, in constant dollars.",
                                                    "% Changes in Consumption/Income/Production?Savings/Unemployment Rates for the US, 1960 to 2016.",
                                                    "Australian Monthly Gas Production",
                                                    "Daily Morning Gold Prices",
                                                    "Quarterly Production of Woollen Yarn in Australia",
                                                    "Dow-Jones index on 251 trading days ending 26 Aug 1994.",
                                                    "Winning times (in minutes) for the Boston Marathon Men's Open Division. 1897-2016."),
                                          selected="Anti-Diabetic Drug Sales in Australia from 1991 to 2008"))
                ),
                tabPanel(
                    'Load .csv/.RDS',
                    tags$br(),
                    fileInput(inputId='uploadFile', "Choose a File:")
                )
            ),
            tags$br(),
            tags$br(),
            tabsetPanel(type='tabs',
                tabPanel(
                    "Dataset Values",
                    tags$div(class='results-table', dataTableOutput(outputId='source_data__head_table'))
                ),
                tabPanel(
                    "Info",
                    tags$div(class='results-table', verbatimTextOutput(outputId='source_data__info'))
                )
            )
        ),
        tabPanel(
            "Numeric Summary",
            column(2,
                   class='column-input-control-style',
                   tags$div(class='input-control-style', uiOutput('numeric_summary__options__UI'))
            ),
            column(10, tags$div(class='results-table', dataTableOutput(outputId='numeric_summary__table')))
        ),
        tabPanel(
            "Categoric Summary",
            tags$div(class='results-table', dataTableOutput(outputId='categoric_summary__table')),
            tags$br(),
            h4("Summary of Values"),
            tags$div(style='width: 800px', verbatimTextOutput(outputId='categoric_summary__text'))
        ),
        tabPanel(
            "Correlations",
            column(
                2,
                class='column-input-control-style',
                tags$div(
                    class='input-control-style',
                    sliderInput(inputId='correlation__corr_threshold',
                                    label="Min Correlation Threshold", ## percent increase
                                    min=0,
                                    max=1,
                                    step=0.05,
                                    value=0),
                    sliderInput(inputId='correlation__p_value_threshold',
                                    label="Max P-Value Treshold",
                                    min=0,
                                    max=1,
                                    step=0.05,
                                    value=1),
                    sliderInput(inputId='correlation__base_size',
                                    label="Text Size",
                                    min=6,
                                    max=20,
                                    step=1,
                                    value=15),
                    checkboxInput(inputId='correlation__pretty_text',
                                  label="Pretty Text", value=FALSE, width=NULL)
                    )
            ),
            column(10,
                   plotOutput(outputId='correlation__plot'),
                   tags$p("For multi-variable time-series data, this graph shows the correlation coefficients for each pair of variables."),
                   tags$p("Correlation is a measure of strength of the *linear* relationship between two variables, and can sometimes be misleading for non-linear relationships. The correlation coefficients lie between -1 and 1.")
            )
        ),
        tabPanel(
            "Plots",
            column(3,
                class='column-input-control-style',
                bsCollapse(id='var_plots__bscollapse', open=c("Options", "Transformations"), multiple=TRUE,
                    bsCollapsePanel(
                        "Options",
                        uiOutput('var_plots__date_slider__UI'),
                        bsTooltip(id='var_plots__date_slider__UI',
                                  title="Filters out underlying data (e.g. both from the graph and forecasting).",
                                  placement='bottom', trigger='hover'),
                        numericInput(inputId='var_plots__y_zoom_min',
                                     label="Y-Axis Zoom Min",
                                     value=NULL),
                        numericInput(inputId='var_plots__y_zoom_max',
                                     label="Y-Axis Zoom Max",
                                     value=NULL),
                        tooltip_zoom_not_filter('var_plots__y_zoom_min'),
                        tooltip_zoom_not_filter('var_plots__y_zoom_max'),
                        tags$div(class='bold_checkbox_input',
                                 checkboxInput(inputId='var_plots__show_values', label="Show Values", value=FALSE)
                        ),
                        checkboxInput(inputId='var_plots__facet', label="Seperate Axes", value=FALSE),
                        style='default'
                    ),
                    bsCollapsePanel(
                        "Transformations",
                        tags$div(class='bold_checkbox_input',
                                 checkboxInput(inputId='var_plots__transformation__daily_average',
                                               label="To Daily Average", value=FALSE)
                        ),
                        tooltip_transformation_not_backtransformed('var_plots__transformation__daily_average'),
                        inline_control(
                            label_text="Log:",
                            padding_top_px=22,
                            control=sliderTextInput(inputId='var_plots__transformation_log',
                                                    label="",
                                                    choices=c("None", "e", "2", "10"),
                                                    selected="None",
                                                    grid=TRUE)
                        ),
                        inline_control(
                            label_text="Power:",
                            padding_top_px=22,
                            control=sliderTextInput(inputId='var_plots__transformation_power',
                                                    label="",
                                                    choices=c("None",seq(2, 20, 1)),
                                                    selected="None",
                                                    grid=TRUE)
                        ),
                        inline_control(
                            label_text="Box-Cox:",
                            padding_top_px=22,
                            control=sliderTextInput(inputId='var_plots__transformation_boxcox',
                                                    label="",
                                                    choices=c("None", "Auto", seq(0, 1, 0.10)),
                                                    selected="None",
                                                    grid=TRUE)
                        ),
                        tooltip_transformation_not_backtransformed('var_plots__transformation_log'),
                        tooltip_transformation_not_backtransformed('var_plots__transformation_power'),
                        tooltip_transformation_not_backtransformed('var_plots__transformation_boxcox')
                    ),
                    bsCollapsePanel(
                        "Baseline Forecasts",
                        numericInput(inputId='var_plots__baseline__forecast_horizon',
                                     label="Forecast Horizon (periods)",
                                     value=NULL),
                        bsTooltip(id='var_plots__baseline__forecast_horizon',
                                  title="The number of future periods (e.g. weeks, months, quarters) to forecast.",
                                  placement='bottom', trigger='hover'),
                        checkboxGroupInput(inputId='var_plots__baseline_forecasts',
                                           label="Forecast Methods",
                                           choices = c("Mean",
                                                       "Naive",
                                                       "Seasonal Naive",
                                                       "Drift",
                                                       "Natural Cubic Smoothing Spline",
                                                       "Auto"), 
                                           selected = NULL),
                        sliderTextInput(inputId='var_plots__baseline__lambda',
                                                    label="Lambda (forecast() parameter)",
                                                    choices=c("None", "Auto", seq(0, 1, 0.10)),
                                                    selected="None",
                                                    grid=TRUE),
                        bsTooltip(id='var_plots__baseline__lambda',
                                  title="Applies Box-Cox transformation internally and back-transforms forecast values.",
                                  placement='bottom', trigger='hover'),
                        tags$div(class='bold_checkbox_input',
                                 checkboxInput(inputId='var_plots__baseline__biasadj',
                                               label="Bias Adjustment",
                                               value=FALSE)
                        ),
                        bsTooltip(id='var_plots__baseline__biasadj',
                                  title="Use adjusted back-transformed mean for Box-Cox transformations. If transformed data is used to produce forecasts and fitted values, a regular back transformation will result in median forecasts. If biasadj is TRUE, an adjustment will be made to produce mean forecasts and fitted values.",
                                  placement='bottom', trigger='hover'),
                        tags$div(class='bold_checkbox_input',
                                 checkboxInput(inputId='var_plots__baseline__show_forecast_values',
                                               label="Show Forecast Values",
                                               value=FALSE)
                        ),
                        tags$div(class='bold_checkbox_input',
                                 checkboxInput(inputId='var_plots__baseline__show_fitted_line',
                                               label="Show Fitted Line",
                                               value=FALSE)
                        ),
                        style='default'
                    ),
                    bsCollapsePanel(
                        "Variables",
                        fluidRow(
                            column(4,
                                tags$div(style='margin-bottom: 20px;',
                                    actionButton(inputId='var_plots__variables_apply', label="Apply")
                                )
                            ),
                            column(8,
                                tags$div(style='margin-bottom: 20px;',
                                    actionButton(inputId='var_plots__variables_toggle', label="Toggle")
                                )
                            )
                        ),
                        uiOutput('var_plots__ts_variables__UI'),
                        style='default'
                    )
                )
            ),
            column(9,
                tabsetPanel(type='tabs',
                    tabPanel("Standard",
                        plotOutput(outputId='var_plots'),
                        verbatimTextOutput(outputId='var_plots__ggplot_messages'),
                        tabsetPanel(type='tabs',
                            tabPanel("Autocorrelation",
                                tags$br(),
                                numericInput(inputId='var_plots__auto_correlation_lags',
                                             label="# of Auto-Correlation Lags",
                                             value=NULL),
                                plotOutput(outputId='var_plots__auto_correlation'),
                                tags$div(id='autocorrelation_explanation',
                                    tags$p("This graph (correlogram) shows the autocorrelation coefficients (or Autocorrelation Function, ACF) for each corresponding lagged value."),
                                    tags$p("Autocorrelation is a measure between the linear relationship between the lagged values and the original values of the time-series."),
                                    tags$p("\"When data have a trend, the autocorrelations for small lags tend to be large and positive because observations nearby in time are also nearby in size. So the ACF of trended time series tend to have positive values that slowly decrease as the lags increase."),
                                    tags$p("When data area seasonal, the autocorrelations will be larger for the seaonal lags (at multiples of the seaonal frequency) than for other lags."),
                                    tags$p("Time series that show no autocorrelation are called `white noise`.\" - Forecasting Principles and Practice")
                                )
                            ),
                            tabPanel("Forecast Residuals",
                                tags$br(),
                                tags$h4("Residual Stats"),
                                dataTableOutput(outputId='var_plots__residuals_means'),
                                tags$br(),
                                tags$h4("Residual Diagnostic Graphs"),
                                plotOutput(outputId='var_plots__residuals_plot'),
                                tags$br(),
                                tags$h4("Ljung-Box Test"),
                                verbatimTextOutput(outputId='var_plots__residuals_ljung_box')
                            ),
                            tabPanel("Cross Validation",
                                tags$br(),
                                selectInput(inputId='var_plots__cross_validation_metric',
                                            label="Metric:",
                                            choices=c("MAE",
                                                      "RMSE",
                                                      "MSE"),
                                            selected="MAE"),
                                plotOutput(outputId='var_plots__cross_validation')
                            )
                        )
                    ),
                    tabPanel("Seasonal",
                        tags$br(),
                        selectInput(width=NULL,
                                    inputId='var_plots__season_plot_type',
                                    label="Plot Type:",
                                    choices=c("Standard",
                                              "Polar",
                                              "Sub-series"),
                                    selected="Standard"),
                        tags$br(),
                        plotOutput(outputId='var_plots__seasonal'),
                        tags$p("For single-variable time-series data, these graphs show the data plotted against individual seasons.")
                    ),
                    tabPanel("Scatter Matrix",
                        plotOutput(outputId='var_plots__scatter_matrix'),
                        tags$p("For multi-variable time-series data, this graph shows a scatterplot and correlation coefficients for each pair of variables."),
                        tags$p("Correlation is a measure of strength of the *linear* relationship between two variables, and can sometimes be misleading for non-linear relationships. The correlation coefficients lie between -1 and 1.")
                    )
                )
            )
        ),
        tabPanel(
            "Regression",
            column(3,
                class='column-input-control-style',
                bsCollapse(id='regression__collapse_controls', open=c("Variables", "Options"), multiple=TRUE,
                    bsCollapsePanel(
                        "Variables",
                        uiOutput('regression__dependent_variable__UI'),
                        uiOutput('regression__independent_variables__UI'),
                        actionButton(inputId='regression__toggle_all_ind_variables',
                                     label="Toggle All Variables"),
                        style='default'
                    ),
                    bsCollapsePanel(
                        "Options",
                        uiOutput('regression__date_slider__UI'),
                        numericInput(inputId='regression__num_lags',
                                     label="Number of Lags",
                                     value=NULL),
                        bsTooltip(id='regression__num_lags',
                                  title="The number of lagged values (for each independent variable) to include in the model.",
                                  placement='bottom', trigger='hover'),
                        tags$div(class='bold_checkbox_input',
                                 checkboxInput(inputId='regression__include_dependent_lags',
                                               label="Include Dependent Lag Variables", value=TRUE)
                        ),
                        bsTooltip(id='regression__include_dependent_lags',
                                  title="For multi-variable datasets, when including lagged variables, indicates whether or not to include lags for the dependent variable.",
                                  placement='bottom', trigger='hover'),
                        numericInput(inputId='regression__ex_ante_forecast_horizon',
                                     label="Forecast (Ex Ante) Horizon",
                                     value=NULL),
                        bsTooltip(id='regression__ex_ante_forecast_horizon',
                                  title="'Ex Ante' means we are producing a 'true' forecast, and will only include (lagged) variables >= to the forecast horizon.",
                                  placement='bottom', trigger='hover'),
                        style='default'
                    ),
                    bsCollapsePanel(
                        "Interaction Effects",
                        uiOutput('regression__interaction_term1__UI'),
                        uiOutput('regression__interaction_term2__UI'),
                        style='default'
                    )
                )
            ),
            column(9,
                tabsetPanel(type="tabs",
                    tabPanel("Output",
                        tags$br(),
                        actionButton(inputId='regression__run_button', label="Run Regression"),
                        tags$br(),tags$br(),
                        hidden(tags$h4(id='regression__formula_header', 'Formula')),
                        verbatimTextOutput(outputId='regression__formula'),
                        tags$br(),
                        hidden(uiOutput('regression__summary_header__UI')),
                        verbatimTextOutput(outputId='regression__number_of_rows_missing_removed'),
                        tags$br(),
                        verbatimTextOutput(outputId='regression__summary_output'),
                        tags$br(),
                        verbatimTextOutput(outputId='regression__cross_validation')
                    ),
                    tabPanel("VIFs",
                        hidden(tags$h4(id='regression__vif_header', "Variance Inflation Factors")),
                        verbatimTextOutput(outputId='regression__summary_vif')
                    ),
                    tabPanel("Diagnostic Plots",
                        tags$br(),
                        tabsetPanel(type="tabs",
                            tabPanel("Fit & Forecast",
                                plotOutput(outputId='regression__diagnostic_fit_forecast')
                            ),
                            tabPanel("Check Residuals",
                                plotOutput(outputId='regression__diagnostic_check_residuals_plots')
                            ),
                            tabPanel("Actual vs Fitted",
                                plotOutput(outputId='regression__diagnostic_actual_vs_fitted')
                            ),
                            tabPanel("Residuals vs Fitted",
                                plotOutput(outputId='regression__diagnostic_residuals_vs_fitted')
                            ),
                            tabPanel("Residuals vs Predictors",
                                plotOutput(outputId='regression__diagnostic_residuals_vs_predictors')
                            ),
                            tabPanel("Residuals vs Period",
                                plotOutput(outputId='regression__diagnostic_residuals_vs_period')
                            ),
                            tabPanel("Residuals vs Season",
                                plotOutput(outputId='regression__diagnostic_residuals_vs_season')
                            ),
                            tabPanel("Normal Q-Q",
                                plotOutput(outputId='regression__diagnostic_normal_qq')
                            ),
                            tabPanel("Scale-Location",
                                plotOutput(outputId='regression__diagnostic_scale_location')
                            ),
                            tabPanel("Cooks Distance",
                                plotOutput(outputId='regression__diagnostic_cooks_distance')
                            ),
                            tabPanel("Residuals vs. Leverage",
                                plotOutput(outputId='regression__diagnostic_residuals_vs_leverage')
                            ),
                            tabPanel("Cooks Distance vs Leverage",
                                plotOutput(outputId='regression__diagnostic_cooks_distance_vs_leverage')
                            )
                        )
                    )
                )   
            )
        ),
        widths=c(2,10)
    )
))
