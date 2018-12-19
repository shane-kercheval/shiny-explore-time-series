#devtools::install_github('shane-kercheval/rtools')

library(shiny)
library(shinyWidgets)
library(shinyBS)
library(rtools)
library(ggplot2)
library(fpp2)
library(ggfortify)
library(stringr)
library(tidyverse)
library(scales)
library(lattice)
library(lubridate)

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
    
    observeEvent(input$source_data__add_date_fields, {
        # this should trigger a reload of the dataset (perhaps not the best approach, but TBD on alternatives)
        # however, it shouldn't trigger a reload if it is set back to the default select_variable_optional
        # which will trigger false positive loadings
        if(!is.null(reactive__source_data()) &&
                !is.null(input$source_data__add_date_fields) &&
                input$source_data__add_date_fields != select_variable_optional &&
                input$source_data__add_date_fields %in% colnames(reactive__source_data())) {

            custom_triggers$reload_source_data <- runif(1, 1, 1000000)
        }
    })

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
    reactive__var_plots__filtered_data <- reactive__var_plots__filtered_data__creator(input,
                                                                                      reactive__source_data)

    output$var_plots__date_slider__UI <- renderUI__var_plots__date_slider__UI(reactive__var_plots__filtered_data)
    output$var_plots__ts_variables__UI <- renderUI__var_plots__ts_variables__UI(reactive__var_plots__filtered_data)
    # creates the ggplot object
    reactive__var_plots__ggplot <- reactive__var_plots__ggplot__creator(input,
                                                                        reactive__var_plots__filtered_data)

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
                                                                                        reactive__var_plots__filtered_data)

    output$var_plots__seasonal <- renderPlot__var_plots__seasonal(session,
                                                                  reactive__var_plots__season__ggplot)

    reactive__var_plots__scatter_matrix__ggplot <- reactive__var_plots__scatter_matrix__ggplot__creator(input,
                                                                                                reactive__var_plots__filtered_data)

    output$var_plots__scatter_matrix <- renderPlot__var_plots__scatter_matrix(session,
                                                                              reactive__var_plots__scatter_matrix__ggplot)
    
    # need a reactive value to know whether or not I can set the style of the Variables bsCollapsePanel to 
    # 'danger', because it is scheduled to do so when the variables checkboxlist is updated, which happens
    # after a new dataset is loaded (and we don't want to "endanger" since that is expected)
    # specifically, we cannot endanger immediately after a new dataset is loaded (value will be set to 0 when
    # loading), but but we can endanger after the first time i.e. the second time i.e. index >=1
    reactiveValues__vp_can_endanger_variables <- reactiveValues(index=0)
    observeEvent(reactive__source_data(), ({

        # when the source is updated, set the endanger index back to zero
        reactiveValues__vp_can_endanger_variables$index <- 0

        if(is_single_time_series(reactive__source_data())) {

            updateCollapse(session, "var_plots__bscollapse", style = list('Variables' = 'default'))

        } else {

            updateCollapse(session, "var_plots__bscollapse", style = list('Variables' = 'success'))
        }
    }))

    observeEvent(input$var_plots__variables_toggle, ({
        
        local_variables <- isolate(input$var_plots__ts_variables)
        local_dataset <- isolate(reactive__var_plots__filtered_data())
        # if none selected, select all, otherwise (if any selected); unselect all
        if(length(local_variables) == 0) {

            column_names <- colnames(as.data.frame(local_dataset) %>% select_if(is.numeric))
            updateCheckboxGroupInput(session=session,
                                     inputId='var_plots__ts_variables',
                                     selected=column_names)
        } else {

            updateCheckboxGroupInput(session=session,
                                     inputId='var_plots__ts_variables',
                                     selected=character(0))

            # seems like a bug, updateCheckboxGroupInput doesn't trigger observeEvent for var_plots__ts_variables
            updateCollapse(session, "var_plots__bscollapse", style = list('Variables' = 'danger'))
        }
    }))

    observeEvent(input$var_plots__variables_apply, ({

        updateCollapse(session, "var_plots__bscollapse", style = list('Variables' = 'success'))

    }))

    observeEvent(input$var_plots__ts_variables, ({

        local_endager_index <- isolate(reactiveValues__vp_can_endanger_variables$index)
        log_message_variable('reactiveValues__vp_can_endanger_variables$index', local_endager_index)

        if(local_endager_index >= 1) {

            updateCollapse(session, "var_plots__bscollapse", style = list('Variables' = 'danger'))
        }

        reactiveValues__vp_can_endanger_variables$index <- local_endager_index + 1

    }))
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
