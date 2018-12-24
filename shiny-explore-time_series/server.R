#devtools::install_github('shane-kercheval/rtools')

library(shiny)
library(shinyWidgets)
library(shinyBS)
library(rtools)
library(ggplot2)
library(fpp2)
#library(ggfortify)  ggfortify does not play nice with the graphs, I get errors like "Error: Invalid input: date_trans works with objects of class Date only"
library(stringr)
library(tidyverse)
library(scales)
library(lattice)
library(lubridate)
library(forecast)

source('helper_scripts/definitions.R')
source('helper_scripts/logging_functions.R')
source('helper_scripts/generic_helpers.R')
source('helper_scripts/plot_helpers.R')
source('helper_scripts/dataset_loading_helpers.R')
source('helper_scripts/numeric_summary_helpers.R')
source('helper_scripts/categoric_summary_helpers.R')
source('helper_scripts/correlation_helpers.R')
source('helper_scripts/variable_plots_helpers.R')
source('helper_scripts/regression_helpers.R')

options(shiny.maxRequestSize=30*1024^2)  # increase upload limit to 30MB


# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

    ##########################################################################################################
    # LOAD DATA
    ##########################################################################################################
    custom_triggers <- reactiveValues(reload_source_data=0)
    # loads dataset depending on drop down or upload
    reactive__source_data <- reactive__source_data__creator(input, custom_triggers)
    # shows the first 500 rows of the data
    output$source_data__head_table <- renderDataTable__source_data__head(reactive__source_data)
    # shows the types of the data's variables/columns
    output$source_data__info <- renderDataTable__source_data__info(reactive__source_data)
    
    ##########################################################################################################
    # numeric summary data
    ##########################################################################################################
    reactive__numeric_summary <- reactive__numeric_summary__creator(reactive__source_data)
    output$numeric_summary__table <- renderDataTable__numeric_summary__table(input, reactive__numeric_summary)
    output$numeric_summary__options__UI <- renderUI__numeric_summary__options__UI(reactive__numeric_summary)

    ##########################################################################################################
    # categoric summary data
    ##########################################################################################################
    reactive__categoric_summary <- reactive__categoric_summary__creator(reactive__source_data)
    output$categoric_summary__table <- renderDataTable__categoric_summary__table(reactive__categoric_summary)
    output$categoric_summary__text <- renderPrint__categoric_summary__text(reactive__source_data,
                                                                           reactive__categoric_summary)

    ##########################################################################################################
    # Correlation Plot
    ##########################################################################################################
    output$correlation__plot <- renderPlot__correlation__plot(input, session, reactive__source_data)

    ##########################################################################################################
    # Viarable Plot
    ##########################################################################################################
    # cached dataset after the filters have been applied (which is bad for large datasets :( ) so that the
    # filters don't have to be reapplied every time; sacrificing memory for speed 

    throttled__var_plots__date_slider <- throttle(reactive(input$var_plots__date_slider), 2000)
    reactiveValues__vp__transformations <- reactiveValues(message=NULL)  # used to capture the transformations to display on y-axix of graph
    reactive__var_plots__filtered_data <- reactive__var_plots__filtered_data__creator(input,
                                                                                      reactive__source_data,
                                                                                      throttled__var_plots__date_slider,
                                                                                      reactiveValues__vp__transformations)

    output$var_plots__date_slider__UI <- renderUI__var_plots__date_slider__UI(reactive__source_data)
    output$var_plots__ts_variables__UI <- renderUI__var_plots__ts_variables__UI(reactive__source_data)
    # creates the ggplot object
    reactiveValues__vp__models <- reactiveValues(models=NULL)
    reactive__var_plots__ggplot <- reactive__var_plots__ggplot__creator(input,
                                                                        reactive__var_plots__filtered_data,
                                                                        reactiveValues__vp__transformations,
                                                                        reactiveValues__vp__models)

    # stores any messages/warnings that ggplot produces when rendering the plot (outputs below the graph
    #(var_plots__ggplot_messages))
    reactiveValues__vp__ggplot_message <- reactiveValues(value=NULL)
    output$var_plots__ggplot_messages <- renderPrint__reactiveValues__vp__ggplot_message(reactiveValues__vp__ggplot_message)

    # main plot
    output$var_plots <- renderPlot__variable_plot(session,
                                                  reactive__var_plots__ggplot,
                                                  reactiveValues__vp__ggplot_message)
    #output$var_plots__variable__UI <- renderUI__var_plots__variable__UI(reactive__source_data)
    observe__var_plots__hide_show_uncollapse_on_dataset_type(session, reactive__source_data)
    reactive__var_plots__season__ggplot <- reactive__var_plots__season__ggplot__creator(input,
                                                                                        reactive__var_plots__filtered_data,
                                                                                        reactiveValues__vp__transformations)
    output$var_plots__seasonal <- renderPlot__var_plots__seasonal(session,
                                                                  reactive__var_plots__season__ggplot)
    reactive__var_plots__scatter_matrix__ggplot <- reactive__var_plots__scatter_matrix__ggplot__creator(input,
                                                                                                reactive__var_plots__filtered_data,
                                                                                                reactiveValues__vp__transformations)
    output$var_plots__scatter_matrix <- renderPlot__var_plots__scatter_matrix(session,
                                                                              reactive__var_plots__scatter_matrix__ggplot)
    
    reactive__var_plots__auto_correlation__ggplot <- reactive__var_plots__auto_correlation__ggplot__creator(input,
                                                                                                            reactive__var_plots__filtered_data,
                                                                                                            reactiveValues__vp__transformations)
    output$var_plots__auto_correlation <- renderPlot__var_plots__auto_correlation(session,
                                                                                  reactive__var_plots__auto_correlation__ggplot)

    output$var_plots__residuals_means <- renderDataTable__var_plots__residuals_means(reactiveValues__vp__models)
    output$var_plots__residuals_plot <- renderPlot__var_plots__residuals_plot(session, reactiveValues__vp__models)
    output$var_plots__residuals_ljung_box <- renderPrint__var_plots__residuals_ljung_box(reactiveValues__vp__models)

    output$var_plots__cross_validation <- renderPlot__var_plots__var_plots__cross_validation(session, input, reactive__var_plots__filtered_data)

    # observe update UI (i.e. seperate out UI changes from creating the plot in `helper_create_time_series_graph()`)
    observe__var_plots__hide_show_uncollapse_on_filtered_dataset_type(session, reactive__var_plots__filtered_data)

    # need a reactive value to know whether or not I can set the style of the Variables bsCollapsePanel to 
    # 'danger', because it is scheduled to do so when the variables checkboxlist is updated, which happens
    # after a new dataset is loaded (and we don't want to "endanger" since that is expected)
    # specifically, we cannot endanger immediately after a new dataset is loaded (value will be set to 0 when
    # loading), but but we can endanger after the first time i.e. the second time i.e. index >=1
    reactiveValues__vp_can_endanger_variables <- reactiveValues(index=0)

    observeEvent__var_plots__variables_collapse(session,
                                                reactive__source_data,
                                                reactiveValues__vp_can_endanger_variables)
    observeEvent__var_plots__variables_toggle(session, input, reactive__source_data)
    observeEvent__var_plots__variables_apply(session, input)
    observeEvent__var_plots__variables_endager(session, input, reactiveValues__vp_can_endanger_variables)

    ##########################################################################################################
    # Regression Output
    ##########################################################################################################
    # Run Regression when user clicks Run button
    eventReactive__regression__results <- eventReactive__regression__results__creator(input, reactive__source_data)
    output$regression__summary_output <- renderPrint__regression__summary_output(eventReactive__regression__results)
    output$regression__number_of_rows_missing_removed <- renderText__regression__number_of_rows_missing_removed(eventReactive__regression__results)
    output$regression__formula <- renderText__regression__formula(eventReactive__regression__results)
    output$regression__summary_vif <- renderPrint__regression__summary_vif(eventReactive__regression__results)
    output$regression__diagnostic_actual_vs_predicted <- render_diagnostic_plot__actual_vs_predicted(input, session, reactive__source_data, eventReactive__regression__results)
    output$regression__diagnostic_residuals_vs_fitted <- render_diagnostic_plot__residuals_vs_fitted(input, session, reactive__source_data, eventReactive__regression__results)
    output$regression__diagnostic_actual_vs_observed <- render_diagnostic_plot__actual_vs_observed(input, session, reactive__source_data, eventReactive__regression__results)
    output$regression__diagnostic_normal_qq <- render_diagnostic_plot__normal_qq(input, session, reactive__source_data, eventReactive__regression__results)
    output$regression__diagnostic_scale_location <- render_diagnostic_plot__scale_location(input, session, reactive__source_data, eventReactive__regression__results)
    output$regression__diagnostic_cooks_distance <- render_diagnostic_plot__cooks_distance(input, session, reactive__source_data, eventReactive__regression__results)
    output$regression__diagnostic_residuals_vs_leverage <- render_diagnostic_plot__residuals_vs_leverage(input, session, reactive__source_data, eventReactive__regression__results)
    output$regression__diagnostic_cooks_distance_vs_leverage <- render_diagnostic_plot__cooks_distance_vs_leverage(input, session, reactive__source_data, eventReactive__regression__results)
    # Regression Reactive UI
    output$regression__dependent_variable__UI <- renderUI__regression__dependent_variable__UI(reactive__source_data)
    output$regression__independent_variables__UI <- renderUI__regression__independent_variables__UI(input, reactive__source_data)
    output$regression__summary_header__UI <- renderUI__regression__summary_header__UI(eventReactive__regression__results)
    output$regression__interaction_term1__UI <- renderUI__regression__interaction_term1__UI(input, reactive__source_data)
    output$regression__interaction_term2__UI <- renderUI__regression__interaction_term2__UI(input, reactive__source_data)
    observeEvent__regression__toggle_all_ind_variables(input, reactive__source_data, session)
})
