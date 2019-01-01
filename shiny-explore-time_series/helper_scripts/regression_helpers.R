regression_build_formula <- function(dependent_variable, independent_variables=NULL, interaction_variables=NULL) {

    if(is.null(interaction_variables)) {
        
        interaction_variables_formula <- ''
    
    } else {

        interaction_variables_formula <- paste(map_chr(interaction_variables, ~ paste(., collapse =' * ')),
                                               collapse = ' + ')
    }

    if(is.null(independent_variables) || length(independent_variables) == 0) {

        independent_variables_formula <- interaction_variables_formula

    } else if(is.null(interaction_variables) || length(interaction_variables) == 0) {

        independent_variables_formula <- paste(independent_variables, collapse =' + ')

    } else {
        
        independent_variables_formula <- paste(interaction_variables_formula,
                                               '+',
                                               paste(independent_variables, collapse =' + '))
    }

    if(dependent_variable == single_time_series_variable_name) {

        dependent_variable <- 'dataset'
    }

    return (paste(dependent_variable, '~', independent_variables_formula))
}

easy_regression <- function(dataset,
                            dependent_variable,
                            independent_variables,
                            interaction_variables=NULL,
                            polynomial=NULL) {

    
    formula <- regression_build_formula(dependent_variable, independent_variables, interaction_variables)

    # variables other than trend/season
    dataset_variables <- independent_variables[! independent_variables %in% built_in_ts_variables]

    # get indexes of rows that contain NA, and remove
    if(length(dataset_variables) == 0) {

        indexes_to_remove <- which(!complete.cases(dataset))

    } else {
        
        indexes_to_remove <- which(!complete.cases(dataset[, dataset_variables]))
    }

    dataset <- na.omit(dataset)

    type <- 'Linear Regression'
    result <- tslm(formula, data=dataset)
    reference <- NULL

    return (
        list(rows_excluded=indexes_to_remove,
             type=type,
             formula=formula,
             results=result,
             reference=reference)
    )
}

##########################################################################################################
# Regression Results - Run Regression when user clicks Run button
##########################################################################################################    
eventReactive__regression__results__creator <- function(input, dataset) {

    eventReactive(input$regression__run_button, {

        if(input$regression__dependent_variable == select_variable) {

            return (NULL)
        }

        local_interaction_term1 <- input$regression__interaction_term1
        local_interaction_term2 <- input$regression__interaction_term2

        withProgress(value=1/2, message='Running Regression',{

            interaction_variables <- NULL

            if(!is.null(local_interaction_term1) && local_interaction_term1 != select_variable &&
               !is.null(local_interaction_term2) && local_interaction_term2 != select_variable) {

                interaction_variables <- list(c(local_interaction_term1,
                                                local_interaction_term2))
            }

            local_dataset <- dataset()
            local_date_slider <- input$regression__date_slider
            local_date_slider <- convert_start_end_window(local_dataset, local_date_slider)
            log_message_variable('input$regression__date_slider', paste0(local_date_slider, collapse='-'))
            local_dataset <- window(local_dataset,
                                start=local_date_slider[[1]],
                                end=local_date_slider[[2]])


            results <- easy_regression(dataset=local_dataset,
                                       dependent_variable=input$regression__dependent_variable,
                                       independent_variables=input$regression__independent_variables,
                                       # list of vectors, each element in the list is a pair of interaction terms
                                       # only supporting two interaction variables at the moment
                                       interaction_variables=interaction_variables)

            shinyjs::show('regression__formula_header')
            shinyjs::show('regression__summary_header__UI')
            shinyjs::show('regression__vif_header')
            
            return (results)
        })
    })
}

##############################################################################################################
# INPUT
##############################################################################################################
helper_create_ts_regression_variables <- function(dataset, variables_to_exclude=NULL){

    independent_variables <- built_in_ts_variables

    if(rt_ts_is_single_variable(dataset)) {

        independent_variables <- c(independent_variables, single_time_series_variable_name)

    } else {

        independent_variables <- c(independent_variables, colnames(dataset))
    }

    if(!is.null(variables_to_exclude)) {

        independent_variables <- independent_variables[! independent_variables %in% variables_to_exclude]
    }

    return (independent_variables)
}

renderUI__regression__dependent_variable__UI <- function(dataset) {

    renderUI({

        variables <- helper_create_ts_regression_variables(dataset(),
                                                           variables_to_exclude=built_in_ts_variables)


        if(rt_ts_is_single_variable(dataset())) {

            selected <- single_time_series_variable_name

        } else {

            selected <- select_variable
        }

        selectInput(inputId='regression__dependent_variable',
                    label='Dependent Variable',
                    choices=c(select_variable, variables),
                    selected=selected,
                    multiple=FALSE,
                    selectize=TRUE,
                    width=500,
                    size=NULL)
    })
}

renderUI__regression__independent_variables__UI <- function(input, dataset) {

    renderUI({

        req(input$regression__dependent_variable)

        independent_variables <- helper_create_ts_regression_variables(dataset(),
                                                                       variables_to_exclude=input$regression__dependent_variable)

        checkboxGroupInput(inputId='regression__independent_variables',
                           label="Independent Variables",
                           choices=independent_variables,
                           selected=independent_variables,
                           inline=FALSE,
                           width=NULL)
    })
}

renderUI__regression__summary_header__UI <- function(regression__results) {

    renderUI({

        req(regression__results())

        local_regression__results <- regression__results()

        if(is.null(local_regression__results$reference)) {  # reference is filled for logistic regression

            reference <- ''            

        } else {

            reference <- paste0('(reference: `', local_regression__results$reference, '`)')
        }

        tags$h4(paste(regression__results()$type, 'Summary', reference))
    })
}

renderUI__regression__interaction_term1__UI <- function(input, dataset) {

    renderUI({

        req(input$regression__dependent_variable)

        # cannot select dependent_variable
        variables <- helper_create_ts_regression_variables(dataset(),
                                                               variables_to_exclude=input$regression__dependent_variable)

        selectInput(inputId='regression__interaction_term1',
                    label='Interaction Variable 1',
                    choices=c(select_variable, variables),
                    selected=select_variable,
                    multiple=FALSE,
                    selectize=TRUE,
                    width=500,
                    size=NULL)
    })
}

renderUI__regression__interaction_term2__UI <- function(input, dataset) {

    renderUI({

        req(input$regression__dependent_variable)
        req(input$regression__interaction_term1)

        # cannot select dependent_variable or the first term
        variables <- helper_create_ts_regression_variables(dataset(),
                                                           variables_to_exclude=c(input$regression__dependent_variable,
                                                                                  input$regression__interaction_term1))

        selectInput(inputId='regression__interaction_term2',
                    label='Interaction Variable 2',
                    choices=c(select_variable, variables),
                    selected=select_variable,
                    multiple=FALSE,
                    selectize=TRUE,
                    width=500,
                    size=NULL)
    })
}

observeEvent__regression__toggle_all_ind_variables <- function(input, dataset, session) {

    observeEvent(input$regression__toggle_all_ind_variables, {

        # if none selected, select all, otherwise (if any selected); unselect all
        if(length(input$regression__independent_variables) == 0) {

            variables <- helper_create_ts_regression_variables(dataset(),
                                                               variables_to_exclude=input$regression__dependent_variable)

            updateCheckboxGroupInput(session=session,
                                     inputId='regression__independent_variables',
                                     selected=variables)

        } else {

            updateCheckboxGroupInput(session=session,
                                     inputId='regression__independent_variables',
                                     selected=character(0))
        }
    })
}

##############################################################################################################
# OUTPUT
##############################################################################################################
renderPrint__regression__summary_output <- function(regression__results) {

    renderPrint({

        req(regression__results())
        summary(regression__results()$results)
    })
}

renderText__regression__number_of_rows_missing_removed <- function(regression__results) {

    renderText({

        req(regression__results())
        paste('Number of missing/removed rows from dataset:', length(regression__results()$rows_excluded))
    })
}

renderText__regression__formula <- function(regression__results) {

    renderText({

        req(regression__results())
        regression__results()$formula
    })
}

renderPrint__regression__summary_vif <- function(regression__results) {

    renderPrint({

        req(regression__results())
        car::vif(regression__results()$results)
    })
}

render_diagnostic_plot <- function(regression__results, graph_function, graph_width_function) {

    return (
        renderPlot({

            req(regression__results())
            withProgress(value=1/2, message='Creating Regression Diagnostic Graph',{

                graph_function()
            })
    
        }, height = graph_width_function)
    )
}

render_diagnostic_plot__actual_vs_predicted <- function(input, session, dataset, regression__results) {

    render_diagnostic_plot(
        regression__results,
        graph_function=function() {
            xyplot(dataset()[, isolate({input$regression__dependent_variable})] ~ predict(regression__results()$results),
                   type=c('p', 'g'),
                   xlab='Predicted', ylab='Actual')
        },
        graph_width_function=function() {0.66 * session$clientData$output_regression__diagnostic_actual_vs_predicted_width}
    )
}

render_diagnostic_plot__residuals_vs_fitted <- function(input, session, dataset, regression__results) {

    render_diagnostic_plot(
        regression__results,
        graph_function=function() { plot(regression__results()$results, which=1) },
        graph_width_function=function() {0.66 * session$clientData$output_regression__diagnostic_residuals_vs_fitted_width}
    )
}

render_diagnostic_plot__actual_vs_observed <- function(input, session, dataset, regression__results) {

    render_diagnostic_plot(
        regression__results,
        graph_function=function() {
            xyplot(predict(regression__results()$results) ~ 1:nrow(dataset()),
                   type=c('p', 'g'),
                   xlab='Observation Number', ylab='Predicted')
        },
        graph_width_function=function() {0.66 * session$clientData$output_regression__diagnostic_actual_vs_observed_width}
    )
}

render_diagnostic_plot__normal_qq <- function(input, session, dataset, regression__results) {

    render_diagnostic_plot(
        regression__results,
        graph_function=function() { plot(regression__results()$results, which=2) },
        graph_width_function=function() {0.66 * session$clientData$output_regression__diagnostic_normal_qq_width}
    )
}

render_diagnostic_plot__scale_location <- function(input, session, dataset, regression__results) {

    render_diagnostic_plot(
        regression__results,
        graph_function=function() { plot(regression__results()$results, which=3) },
        graph_width_function=function() {0.66 * session$clientData$output_regression__diagnostic_scale_location_width}
    )
}

render_diagnostic_plot__cooks_distance <- function(input, session, dataset, regression__results) {

    render_diagnostic_plot(
        regression__results,
        graph_function=function() { plot(regression__results()$results, which=4) },
        graph_width_function=function() {0.66 * session$clientData$output_regression__diagnostic_cooks_distance_width}
    )
}

render_diagnostic_plot__residuals_vs_leverage <- function(input, session, dataset, regression__results) {

    render_diagnostic_plot(
        regression__results,
        graph_function=function() { plot(regression__results()$results, which=5) },
        graph_width_function=function() {0.66 * session$clientData$output_regression__diagnostic_residuals_vs_leverage_width}
    )
}

render_diagnostic_plot__cooks_distance_vs_leverage <- function(input, session, dataset, regression__results) {

    render_diagnostic_plot(
        regression__results,
        graph_function=function() { plot(regression__results()$results, which=6) },
        graph_width_function=function() {0.66 * session$clientData$output_regression__diagnostic_cooks_distance_vs_leverage_width}
    )
}
