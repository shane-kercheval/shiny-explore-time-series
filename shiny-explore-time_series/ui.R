library(shiny)
library(shinyWidgets)
library(shinyjs)
library(shinyBS)

source('helper_scripts/definitions.R')

shinyUI(fluidPage(theme="custom.css",

    useShinyjs(),

    titlePanel('Explore Time Series'),
    navlistPanel(
        tabPanel(
            'Load Dataset',
            tabsetPanel(type='tabs',
                tabPanel(
                    'Preloaded Dataset',
                    tags$br(),
                    tags$div(style='margin-bottom: 40px !important',
                        selectInput(width='750px',
                                    inputId='preloaded_dataset',
                                          label='Choose a Dataset:',
                                          choices=c('Anti-Diabetic Drug Sales in Australia from 1991 to 2008',
                                                    'Total Weekly Air Passenger #s flights (Ansett) Melbourne/Sydney, 1987–1992.',
                                                    'Half-hourly/Daily Electricity Demand for Victoria, Australia, in 2014',
                                                    'Quarterly Visitor Nights for Various Regions of Australia',
                                                    'Quarterly Australian Beer production',
                                                    'Australian monthly electricity production: Jan 1956 – Aug 1995.',
                                                    'International Arrivals to Australia',
                                                    'Monthly Total # of Pigs Slaughtered in Victoria, Australia (Jan 1980 – Aug 1995)',
                                                    'Daily Closing Stock Prices of Google Inc (All)',
                                                    'Daily Closing Stock Prices of Google Inc (200)',
                                                    'Price of dozen eggs in US, 1900–1993, in constant dollars.',
                                                    '% Changes in Consumption/Income/Production?Savings/Unemployment Rates for the US, 1960 to 2016.',
                                                    'Australian Monthly Gas Production',
                                                    'Daily Morning Gold Prices',
                                                    'Quarterly Production of Woollen Yarn in Australia',
                                                    'Dow-Jones index on 251 trading days ending 26 Aug 1994.'),
                                          selected='Anti-Diabetic Drug Sales in Australia from 1991 to 2008'))
                ),
                tabPanel(
                    'Load .csv/.RDS',
                    tags$br(),
                    fileInput(inputId='uploadFile', 'Choose a File:')
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
                    'Info',
                    tags$div(class='results-table', verbatimTextOutput(outputId='source_data__info'))
                )
            )
        ),
        tabPanel(
            'Numeric Summary',
            column(2,
                   class='column-input-control-style',
                   tags$div(class='input-control-style', uiOutput('numeric_summary__options__UI'))
            ),
            column(10, tags$div(class='results-table', dataTableOutput(outputId='numeric_summary__table')))
        ),
        tabPanel(
            'Categoric Summary',
            tags$div(class='results-table', dataTableOutput(outputId='categoric_summary__table')),
            tags$br(),
            h4('Summary of Values'),
            tags$div(style='width: 800px', verbatimTextOutput(outputId='categoric_summary__text'))
        ),
        tabPanel(
            'Correlations',
            column(
                2,
                class='column-input-control-style',
                tags$div(
                    class='input-control-style',
                    sliderInput(inputId='correlation__corr_threshold',
                                    label='Min Correlation Threshold', ## percent increase
                                    min=0,
                                    max=1,
                                    step=0.05,
                                    value=0),
                    sliderInput(inputId='correlation__p_value_threshold',
                                    label='Max P-Value Treshold',
                                    min=0,
                                    max=1,
                                    step=0.05,
                                    value=1),
                    sliderInput(inputId='correlation__base_size',
                                    label='Text Size',
                                    min=6,
                                    max=20,
                                    step=1,
                                    value=15),
                    checkboxInput(inputId='correlation__pretty_text',
                                  label='Pretty Text', value=FALSE, width=NULL)
                    )
            ),
            column(10,
                   plotOutput(outputId='correlation__plot'),
                   tags$p('For multi-variable time-series data, this graph shows the correlation coefficients for each pair of variables.'),
                   tags$p('Correlation is a measure of strength of the *linear* relationship between two variables, and can sometimes be misleading for non-linear relationships. The correlation coefficients lie between -1 and 1.')
            )
        ),
        tabPanel(
            'Plots',
            column(3,
                class='column-input-control-style',
                bsCollapse(id='var_plots__bscollapse', open=c('Options', 'Transformations'), multiple=TRUE,
                    bsCollapsePanel(
                        'Options',
                        uiOutput('var_plots__date_slider__UI'),
                        numericInput(inputId='var_plots__y_zoom_min',
                                     label='Y-Axis Zoom Min',
                                     value=NULL),
                        numericInput(inputId='var_plots__y_zoom_max',
                                     label='Y-Axis Zoom Max',
                                     value=NULL),
                        tooltip_zoom_not_filter('var_plots__y_zoom_min'),
                        tooltip_zoom_not_filter('var_plots__y_zoom_max'),
                        tags$div(class='bold_checkbox_input',
                                 checkboxInput(inputId='var_plots__show_values', label='Show Values', value=FALSE)
                        ),
                        checkboxInput(inputId='var_plots__facet', label='Seperate Axes', value=FALSE),
                        style='default'
                    ),
                    bsCollapsePanel(
                        'Transformations',
                        tags$div(class='bold_checkbox_input',
                                 checkboxInput(inputId='var_plots__transformation__daily_average',
                                               label='To Daily Average', value=FALSE)
                        ),
                        tooltip_transformation_not_backtransformed('var_plots__transformation__daily_average'),
                        inline_control(
                            label_text='Log:',
                            padding_top_px=22,
                            control=sliderTextInput(inputId='var_plots__transformation_log',
                                                    label='',
                                                    choices=c('None', 'e', '2', '10'),
                                                    selected='None',
                                                    grid=TRUE)
                        ),
                        inline_control(
                            label_text='Power:',
                            padding_top_px=22,
                            control=sliderTextInput(inputId='var_plots__transformation_power',
                                                    label='',
                                                    choices=c('None',seq(2, 20, 1)),
                                                    selected='None',
                                                    grid=TRUE)
                        ),
                        inline_control(
                            label_text='Box-Cox:',
                            padding_top_px=22,
                            control=sliderTextInput(inputId='var_plots__transformation_boxcox',
                                                    label='',
                                                    choices=c('None', 'Auto', seq(0, 1, 0.10)),
                                                    selected='None',
                                                    grid=TRUE)
                        ),
                        tooltip_transformation_not_backtransformed('var_plots__transformation_log'),
                        tooltip_transformation_not_backtransformed('var_plots__transformation_power'),
                        tooltip_transformation_not_backtransformed('var_plots__transformation_boxcox')
                    ),
                    bsCollapsePanel(
                        'Baseline Forecasts',
                        numericInput(inputId='var_plots__baseline__forecast_horizon',
                                     label='Forecast Horizon (periods)',
                                     value=NULL),
                        checkboxGroupInput(inputId='var_plots__baseline_forecasts',
                                           label='Forecast Methods',
                                           choices = c('Mean',
                                                       'Naive',
                                                       'Seasonal Naive',
                                                       'Drift',
                                                       'Auto'), 
                                           selected = NULL),
                        tags$div(class='bold_checkbox_input',
                                 checkboxInput(inputId='var_plots__baseline__show_values', label='Show Forecast Values', value=FALSE)
                        ),
                        style='default'
                    ),
                    bsCollapsePanel(
                        'Variables',
                        fluidRow(
                            column(4,
                                tags$div(style='margin-bottom: 20px;', actionButton(inputId='var_plots__variables_apply', label='Apply'))
                            ),
                            column(8,
                                tags$div(style='margin-bottom: 20px;', actionButton(inputId='var_plots__variables_toggle', label='Toggle'))
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
                        numericInput(inputId='var_plots__auto_correlation_lags',
                                     label='# of Auto-Correlation Lags',
                                     value=NULL),
                        plotOutput(outputId='var_plots__auto_correlation'),
                        tags$div(id='autocorrelation_explanation',
                            tags$p('This graph (correlogram) shows the autocorrelation coefficients (or Autocorrelation Function, ACF) for each corresponding lagged value.'),
                            tags$p('Autocorrelation is a measure between the linear relationship between the lagged values and the original values of the time-series.'),
                            tags$p('"When data have a trend, the autocorrelations for small lags tend to be large and positive because observations nearby in time are also nearby in size. So the ACF of trended time series tend to have positive values that slowly decrease as the lags increase.'),
                            tags$p('When data area seasonal, the autocorrelations will be larger for the seaonal lags (at multiples of the seaonal frequency) than for other lags.'),
                            tags$p('Time series that show no autocorrelation are called `white noise`." - Forecasting Principles and Practice')
                        )
                    ),
                    tabPanel("Seasonal",
                        tags$br(),
                        selectInput(width=NULL,
                                    inputId='var_plots__season_plot_type',
                                    label='Plot Type:',
                                    choices=c('Standard',
                                              'Polar',
                                              'Sub-series'),
                                    selected='Standard'),
                        tags$br(),
                        plotOutput(outputId='var_plots__seasonal'),
                        tags$p('For single-variable time-series data, these graphs show the data plotted against individual seasons.')
                    ),
                    tabPanel("Scatter Matrix",
                        plotOutput(outputId='var_plots__scatter_matrix'),
                        tags$p('For multi-variable time-series data, this graph shows a scatterplot and correlation coefficients for each pair of variables.'),
                        tags$p('Correlation is a measure of strength of the *linear* relationship between two variables, and can sometimes be misleading for non-linear relationships. The correlation coefficients lie between -1 and 1.')
                    )
                )
            )
        ),
        tabPanel(
            'Regression',
            column(3,
                class='column-input-control-style',
                bsCollapse(id='regression__collapse_controls', open='Variables', multiple=TRUE,
                    bsCollapsePanel(
                        'Variables',
                        uiOutput('regression__dependent_variable__UI'),
                        uiOutput('regression__independent_variables__UI'),
                        actionButton(inputId='regression__toggle_all_ind_variables',
                                     label='Toggle All Variables'),
                        style='default'
                    ),
                    bsCollapsePanel(
                        'Interaction Effects',
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
                        actionButton(inputId='regression__run_button', label='Run Regression'),
                        tags$br(),tags$br(),
                        hidden(tags$h4(id='regression__formula_header', 'Formula')),
                        verbatimTextOutput(outputId='regression__formula'),
                        tags$br(),
                        hidden(uiOutput('regression__summary_header__UI')),
                        verbatimTextOutput(outputId='regression__number_of_rows_missing_removed'),
                        tags$br(),
                        verbatimTextOutput(outputId='regression__summary_output')
                    ),
                    tabPanel("VIFs",
                        hidden(tags$h4(id='regression__vif_header', 'Variance Inflation Factors')),
                        verbatimTextOutput(outputId='regression__summary_vif')
                    ),
                    tabPanel("Diagnostic Plots",
                        tags$br(),
                        tabsetPanel(type="tabs",
                            tabPanel("Actual vs Predicted",
                                plotOutput(outputId='regression__diagnostic_actual_vs_predicted')
                            ),
                            tabPanel("Residuals vs Fittted",
                                plotOutput(outputId='regression__diagnostic_residuals_vs_fitted')
                            ),
                            tabPanel("Actual vs Observed",
                                plotOutput(outputId='regression__diagnostic_actual_vs_observed')
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
