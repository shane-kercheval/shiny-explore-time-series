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
                                                    '% Changes in Consumption/Income/Production?Savings/Unemployment Rates for the US, 1960 to 2016.'),
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
                    sliderTextInput(inputId='correlation__corr_threshold',
                                    label='Min Correlation Threshold', ## percent increase
                                    choices=seq(0, 1, 0.05),
                                    selected=0,
                                    grid=TRUE),
                    sliderTextInput(inputId='correlation__p_value_threshold',
                                    label='Max P-Value Treshold',
                                    choices=seq(0, 1, 0.05),
                                    selected=1,
                                    grid=TRUE),
                    sliderTextInput(inputId='correlation__base_size',
                                    label='Text Size',
                                    choices=seq(6, 20, 1),
                                    selected=15,
                                    grid=TRUE),
                    checkboxInput(inputId='correlation__pretty_text',
                                  label='Pretty Text', value=FALSE, width=NULL)
                    )
            ),
            column(10,
                   plotOutput(outputId='correlation__plot')
            )
        ),
        tabPanel(
            'Plots',
            column(3,
                class='column-input-control-style',
                bsCollapse(id='var_plots__bscollapse', open='Options', multiple=TRUE,
                    bsCollapsePanel(
                        'Options',
                        uiOutput('var_plots__date_slider__UI'),
                        numericInput(inputId='var_plots__y_zoom_min',
                                     label='Y-Axis Zoom Min',
                                     value=NULL),
                        numericInput(inputId='var_plots__y_zoom_max',
                                     label='Y-Axis Zoom Max',
                                     value=NULL),
                        style='default'
                    ),
                    bsCollapsePanel(
                        'Variables',
                        uiOutput('var_plots__ts_variables__UI'),
                        style='default'
                    )
                )
            ),
            column(9,
                plotOutput(outputId='var_plots'),
                verbatimTextOutput(outputId='var_plots__ggplot_messages')
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
