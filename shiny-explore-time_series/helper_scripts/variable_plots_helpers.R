##############################################################################################################
# FILTERED DATASET - Variable Plot's filtered dataset
# duplicate dataset (which is bad for large datasets) so that the filters don't have to be reapplied every time.
##############################################################################################################
reactive__var_plots__filtered_data__creator <- function(input, dataset, date_slider, reactiveValue_trans) {

    reactive({

        req(date_slider())
        input$var_plots__variables_apply  # trigger update from apply, not from selecting the variables

        log_message_block_start('Creating filtered dataset...')

        local_dataset <- dataset()  # clear on new datasets

        # filter on window
        local_start_end <- date_slider()
        local_start_end <- convert_start_end_window(local_dataset, local_start_end)
        log_message_variable('input$var_plots__date_slider', paste0(local_start_end, collapse='-'))
        local_dataset <- window(local_dataset,
                                start=local_start_end[[1]],
                                end=local_start_end[[2]])

        if(is_single_time_series(local_dataset)) {

            log_message('**single ts**')
            
        } else if (is_multi_time_series(local_dataset)) {

            log_message('**multi ts**')

            local_ts_variables <- isolate(input$var_plots__ts_variables)  # don't update when selecting variables
            log_message_variable('input$var_plots__ts_variables', local_ts_variables)


            if(is.null(local_ts_variables)) {

                local_dataset <- NULL

            } else {

                local_dataset <- local_dataset[, local_ts_variables]
            }
        
        } else {
            
            stopifnot(FALSE)
        }

        local_dataset <- local_dataset %>% helper_apply_transformations(input, reactiveValue_trans)

        return (local_dataset)

    })
}

##############################################################################################################
# CREATE GGPLOT OBJECT
##############################################################################################################
convert_start_end_window <- function(dataset, start_end_window) {
    # this function is needed because `window()` gives a warning if the `start` parameter isn't different than
    # the dataset's actual start value
    s <- NULL
    e <- NULL

    # if the selected filter value is different than the actual start value, keep the value to filter on
    if(start_end_window[1] != start(dataset)[1]) {

        s <- c(start_end_window[1], 1)
    }
    # if the selected filter value is different than the actual end value, keep the value to filter on
    if(start_end_window[2] != end(dataset)[1]) {

        e <- c(start_end_window[2], frequency(dataset))
    }

    return (list(s, e))
}

helper_add_baseline_forecasts <- function(ggplot_object, input, dataset, reactiveValues_model) {

    ######################################################################################################
    # BASELINE FORECASTS
    ######################################################################################################
    local_baseline_forecasts <- input$var_plots__baseline_forecasts
    local_baseline_horizon <- input$var_plots__baseline__forecast_horizon

    if(is_single_time_series(dataset) &&
       !is.null(local_baseline_forecasts) &&
       !is.null(local_baseline_horizon)) {

        log_message_variable('input$var_plots__baseline__forecast_horizon', local_baseline_horizon)
        log_message_variable('input$var_plots__baseline_forecasts', local_baseline_forecasts)

        local_lambda <- input$var_plots__baseline__lambda
        if(local_lambda == 'None') {

            local_lambda <- NULL

        } else if(local_lambda == 'Auto') {

            local_lambda <- BoxCox.lambda(dataset)

        } else {

            local_lambda <- as.numeric(local_lambda)
        }
        log_message_variable('input$var_plots__baseline__lambda', local_lambda)

        show_PI <- length(local_baseline_forecasts) == 1  # if multiple forecasts, don't show PI
        if('Mean' %in% local_baseline_forecasts) {

            forecast_model <- meanf(dataset, h=local_baseline_horizon, lambda=local_lambda)
            ggplot_object <- ggplot_object +
                autolayer(forecast_model,
                          series='Mean',
                          PI=show_PI)
        }

        if('Naive' %in% local_baseline_forecasts) {

            forecast_model <- naive(dataset, h=local_baseline_horizon, lambda=local_lambda)
            ggplot_object <- ggplot_object +
                autolayer(forecast_model,
                          series='Naive',
                          PI=show_PI)
        }

        if('Seasonal Naive' %in% local_baseline_forecasts) {

            forecast_model <- snaive(dataset, h=local_baseline_horizon, lambda=local_lambda)
            ggplot_object <- ggplot_object +
                autolayer(forecast_model,
                          series='Seasonal Naive',
                          PI=show_PI)
        }

        if('Drift' %in% local_baseline_forecasts) {

            forecast_model <- rwf(dataset, h=local_baseline_horizon, lambda=local_lambda, drift=TRUE)
            ggplot_object <- ggplot_object +
                autolayer(forecast_model,
                          series='Drift',
                          PI=show_PI)
        }

        if('Auto' %in% local_baseline_forecasts) {

            forecast_model <- forecast(dataset, h=local_baseline_horizon, lambda=local_lambda)
            ggplot_object <- ggplot_object +
                autolayer(forecast_model,
                          series='Auto',
                          PI=show_PI)
        }

        if(length(local_baseline_forecasts) == 1) {

            log_message_variable('forecast model method', forecast_model$method)
            reactiveValues_model$model <- forecast_model

            if(input$var_plots__baseline__show_values) {

                s <- start(forecast_model$mean)
                e <- end(forecast_model$mean)
                f <- frequency(forecast_model$mean)

                df_forecast_model <- as.data.frame(forecast_model)
                ts_forecast <- ts(as.data.frame(forecast_model)$`Point Forecast`, start = s, end=e, frequency = f)

                ggplot_object <- ggplot_object + 
                    geom_point(data= ts_forecast) + 
                    geom_text(data=ts_forecast,
                              aes(label=format_labels(as.numeric(ts_forecast))), 
                              check_overlap=TRUE,
                              vjust=0,
                              hjust=0)

            }
        } else {

            reactiveValues_model$model <- NULL
        }

        if(!is.null(local_lambda)) {
            ggplot_object <- ggplot_object +
                labs(caption=paste0('NOTE: forecasted with lambda parameter (', round(local_lambda, 3),')'))
        }

    }

    return (ggplot_object)
}

helper_y_zoom <- function(ggplot_object, input, dataset) {
     ######################################################################################################
    # ZOOM
    # zoom in on graph if either parameter is set and it's not an auto-correlation plot
    ######################################################################################################

    local_y_zoom_min <- input$var_plots__y_zoom_min
    local_y_zoom_max <- input$var_plots__y_zoom_max

    if(!is.na(local_y_zoom_min) || !is.na(local_y_zoom_max)) {

        log_message_variable('var_plots__y_zoom_min', local_y_zoom_min)
        log_message_variable('var_plots__y_zoom_max', local_y_zoom_max)


        # if one of the zooms is specified then we hae to provide both, so get corresponding min/max
        if(is.na(local_y_zoom_min)) {

            local_y_zoom_min <- min(dataset, na.rm = TRUE)
        }

        if(is.na(local_y_zoom_max)) {

            local_y_zoom_max <- max(dataset, na.rm = TRUE)
        }

        ggplot_object <- ggplot_object + coord_cartesian(ylim = c(local_y_zoom_min, local_y_zoom_max))
    }

    return (ggplot_object)
}

format_labels <- function(values) {

    if(max(values) > 1000000) {

        values <- paste0(round(values / 1000000, 2), 'M')

    } else if(max(values) > 100000) {

        values <- paste0(round(values / 1000, 1), 'K')

    } else if(max(values) > 1000) {

        values <- paste0(round(values / 1000, 1), 'K')

    } else if(max(values) > 100) {

        values <- round(values, 0)

    } else if(max(values) < 1) {

        values <- round(values, 2)

    } else {

        values <- round(values, 1)
    }

    return (values)
}

helper_add_labels <- function(ggplot_object, input, dataset) {

    ######################################################################################################
    # PLOT OPTIONS
    ######################################################################################################
    if(!is.null(input$var_plots__show_values) && input$var_plots__show_values) {

        ggplot_object <- ggplot_object +
            geom_point() +
            geom_text(aes(label=format_labels(as.numeric(dataset))), check_overlap=TRUE, vjust=1, hjust=1)
    }

    return (ggplot_object)
}

helper_create_daily_average_dataset <- function(dataset, input, reactiveValue_trans) {

    if(!is.null(input$var_plots__transformation__daily_average) &&
            input$var_plots__transformation__daily_average) {

        log_message_variable('input$var_plots__transformation__daily_average',
                             input$var_plots__transformation__daily_average)

        freq <- frequency(dataset)
        
        if(freq == 12 || freq == 4) {  # monthly or quarterly

            reactiveValue_trans$message <- c(isolate(reactiveValue_trans$message),
                                             'Average Daily Value')
            return (dataset / monthdays(dataset))

        } else if (freq == 52) {  # weekly
        
            reactiveValue_trans$message <- c(isolate(reactiveValue_trans$message),
                                             'Average Daily Value')
            return (dataset / 7)

        } else {

            stopifnot(FALSE)
        }
    } else {

        return (dataset)
    }
}

helper_transform_log  <- function(dataset, input, reactiveValue_trans) {

    if(!is.null(input$var_plots__transformation_log) &&
            input$var_plots__transformation_log != 'None') {

        log_message_variable('input$var_plots__transformation_log',
                             input$var_plots__transformation_log)

        if(input$var_plots__transformation_log == 'e') {

            base <- exp(1)

        } else {

            base <- as.numeric(input$var_plots__transformation_log)
        }

        reactiveValue_trans$message <- c(isolate(reactiveValue_trans$message),
                                         paste0('Log (base ', round(base, 2), ')'))
        return (log(dataset + 1, base=base))

    } else {

        return (dataset)
    }
}
helper_transform_power  <- function(dataset, input, reactiveValue_trans) {

    if(!is.null(input$var_plots__transformation_power) &&
            input$var_plots__transformation_power != 'None') {

        log_message_variable('input$var_plots__transformation_power',
                             input$var_plots__transformation_power)

        power <- as.numeric(input$var_plots__transformation_power)
        reactiveValue_trans$message <- c(isolate(reactiveValue_trans$message),
                                         paste0('Power (', power, ')'))
        return(dataset^power)

    } else {

        return (dataset)
    }
}
helper_transform_box_cox  <- function(dataset, input, reactiveValue_trans) {

    if(!is.null(input$var_plots__transformation_boxcox) &&
            input$var_plots__transformation_boxcox != 'None') {

        log_message_variable('input$var_plots__transformation_boxcox',
                             input$var_plots__transformation_boxcox)

        if(input$var_plots__transformation_boxcox == 'Auto') {

            lambda <- BoxCox.lambda(dataset)

        } else {

            lambda <- as.numeric(input$var_plots__transformation_boxcox)
        }

        reactiveValue_trans$message <- c(isolate(reactiveValue_trans$message),
                                         paste0('BoxCox (lambda: ', round(lambda, 3), ')'))
        return (BoxCox(dataset, lambda = lambda))

    } else {

        return (dataset)
    }
}
helper_apply_transformations <- function(dataset, input, reactiveValue_trans) {

    reactiveValue_trans$message <- NULL

    return (

        dataset %>% 
            helper_create_daily_average_dataset(input, reactiveValue_trans) %>%
            helper_transform_log(input, reactiveValue_trans) %>%
            helper_transform_power(input, reactiveValue_trans) %>%
            helper_transform_box_cox(input, reactiveValue_trans)
    )
}

helper_add_transformation_y_axis_label <- function(ggplot_object, reactiveValue_trans){

    transfomration_messages <- isolate(reactiveValue_trans$message)
    if(!is.null(transfomration_messages) && length(transfomration_messages) > 0) {

        ggplot_object <- ggplot_object + ylab(paste0(transfomration_messages, collapse='; '))
    }

    return (ggplot_object)
}


reactive__var_plots__ggplot__creator <- function(input, dataset, reactiveValue_trans, reactiveValues_model) {
    reactive({

        req(dataset())

        if(is_single_time_series(dataset())) {

            req(input$var_plots__baseline__forecast_horizon)
        }

        log_message_block_start('Creating `time-series` graph...')

        # reactive data
        local_dataset <- dataset()
        
        ggplot_object <- local_dataset %>%
            autoplot(facets=input$var_plots__facet) %>%
            helper_add_baseline_forecasts(input, local_dataset, reactiveValues_model) %>%
            helper_y_zoom(input, local_dataset) %>%
            helper_add_labels(input, local_dataset) %>%
            helper_add_transformation_y_axis_label(reactiveValue_trans)
    })
}

reactive__var_plots__auto_correlation__ggplot__creator <- function(input, dataset, reactiveValue_trans) {
    
    reactive({

        req(dataset())

        if(is_single_time_series(dataset())) {

            req(input$var_plots__baseline__forecast_horizon)
        }

        log_message_block_start('Creating `auto-correlation` graph...')

        # reactive data
        local_dataset <- dataset()
        ggplot_object <- NULL

        if(is_single_time_series(local_dataset)) {

            lags <- NULL
            if(!is.na(input$var_plots__auto_correlation_lags)) {

                lags <- input$var_plots__auto_correlation_lags
            }

            log_message_variable('input$var_plots__auto_correlation_lags', lags)
            ggplot_object <- local_dataset %>% ggAcf(lag=lags)
        }
    })
}

reactive__var_plots__season__ggplot__creator <- function(input, dataset, reactiveValue_trans) {
    
    reactive({

        req(dataset())

        if(is_single_time_series(dataset())) {

            req(input$var_plots__baseline__forecast_horizon)
        }

        log_message_block_start('Creating `season` graph...')

        # reactive data
        local_dataset <- dataset()
        ggplot_object <- NULL

        if(is_single_time_series(local_dataset)) {

            log_message_variable('input$var_plots__season_plot_type', input$var_plots__season_plot_type)

            if(input$var_plots__season_plot_type == 'Polar') {

                ggplot_object <- local_dataset %>% 
                    ggseasonplot(polar=TRUE) %>%
                    helper_add_transformation_y_axis_label(reactiveValue_trans)

            } else if(input$var_plots__season_plot_type == 'Sub-series') {

                ggplot_object <- local_dataset %>%
                    ggsubseriesplot() %>%
                    helper_y_zoom(input, local_dataset) %>%
                    helper_add_transformation_y_axis_label(reactiveValue_trans)

            } else {

                ggplot_object <- local_dataset %>%
                    ggseasonplot(year.labels=TRUE, year.labels.left=TRUE) %>%
                    helper_y_zoom(input, local_dataset) %>%
                    helper_add_labels(input, local_dataset) %>%
                    helper_add_transformation_y_axis_label(reactiveValue_trans)
            }
        }

    })
}

reactive__var_plots__scatter_matrix__ggplot__creator <- function(input, dataset, reactiveValue_trans) {
    
    reactive({

        req(dataset())

        if(is_single_time_series(dataset())) {

            req(input$var_plots__baseline__forecast_horizon)
        }

        log_message_block_start('Creating `scatter-matrix` graph...')

        # reactive data
        local_dataset <- dataset()
        ggplot_object <- NULL

        if(is_multi_time_series(local_dataset)) {

            ggplot_object <- local_dataset %>%
                as.data.frame() %>%
                GGally::ggpairs()%>%
                helper_add_transformation_y_axis_label(reactiveValue_trans)
        } 
    })
}

##############################################################################################################
# INPUT
##############################################################################################################
renderUI__var_plots__date_slider__UI <- function(dataset) {

    renderUI({
        local_dataset <- dataset()

        s <- start(local_dataset)[1]
        e <- end(local_dataset)[1]

        sliderTextInput(inputId='var_plots__date_slider',
                        label='Date Window',
                        choices=seq(s, e, 1),
                        selected=c(s, e),
                        grid=TRUE)
    })
}

renderUI__var_plots__ts_variables__UI <- function(dataset) {

    renderUI({

        local_dataset <- dataset()

        if(is_multi_time_series(local_dataset)) {

            column_names <- colnames(as.data.frame(local_dataset) %>% select_if(is.numeric))

            return (checkboxGroupInput(inputId='var_plots__ts_variables',
                                       label=NULL,
                                       choices=column_names,
                                       selected=column_names[1],
                                       inline=FALSE,
                                       width=NULL))

        } else {

            return (NULL)
        }
    })
}

##############################################################################################################
# OUTPUT
##############################################################################################################
renderPlot__variable_plot <- function(session, ggplot_object, messages) {

    renderPlot({

        withProgress(value=1/2, message='Creating Time-Series Graph',{

           messages$value <- capture_messages_warnings(function() print(ggplot_object()))
           log_message_variable('messages$value', messages$value)

        })

    }, height = function() {

        session$clientData$output_var_plots_width * 0.66  # set height to % of width
    })
}

renderPlot__var_plots__helper <- function(session, ggplot_object, message, height_function) {

    renderPlot({

        withProgress(value=1/2, message=message,{

           print(ggplot_object())
        })

    }, height = function() {

        if(is.null(ggplot_object())) {

            return (1)

        } else {

            return (height_function())
        }
    })
}

renderPlot__var_plots__seasonal <- function(session, ggplot_object) {

    return(renderPlot__var_plots__helper(session,
                                         ggplot_object,
                                         'Creating Seasonal Graph',
                                         function() { session$clientData$output_var_plots_width * 0.66 }))
}

renderPlot__var_plots__scatter_matrix <- function(session, ggplot_object) {

    return(renderPlot__var_plots__helper(session,
                                         ggplot_object,
                                         'Creating Scatter Matrix Graph',
                                         function() { session$clientData$output_var_plots_width * 0.66 }))
}

renderPlot__var_plots__auto_correlation <- function(session, ggplot_object) {
    return(renderPlot__var_plots__helper(session,
                                         ggplot_object,
                                         'Creating Auto-Correlation Graph',
                                         function() { 300 }))
}

renderPrint__reactiveValues__vp__ggplot_message <- function(message) {

    renderPrint({
        cat(message$value)
    })
}

renderPlot__var_plots__residuals <- function(session, dataset, reactiveValues_model) {

    renderPlot({
        if(is.null(reactiveValues_model$model) || is_multi_time_series(dataset())) {

            return (NULL)

        } else {

            withProgress(value=1/2, message='Creating Residuals Graph',{

               checkresiduals(reactiveValues_model$model)
            })
        }

    }, height = function() {

        session$clientData$output_var_plots_width * 0.66  # set height to % of width
    })
}

renderPlot__var_plots__var_plots__cross_validation <- function(session, input, dataset) {

    renderPlot({
            
        withProgress(value=1/2, message='Creating Cross Validation Graph', {

            log_message_block_start('Creating `cross validation` graph...')

            req(input$var_plots__cross_validation_metric)

            local_dataset <- dataset()
            local_baseline_forecasts <- input$var_plots__baseline_forecasts
            local_baseline_horizon <- input$var_plots__baseline__forecast_horizon
            local_cross_val_metric <- input$var_plots__cross_validation_metric

            ggplot_object <- NULL

            if(is_single_time_series(local_dataset) &&
               !is.null(local_baseline_forecasts) && length(local_baseline_forecasts) > 0 &&
               !is.null(local_baseline_horizon)) {

                log_message_variable('input$var_plots__baseline__forecast_horizon', local_baseline_horizon)
                log_message_variable('input$var_plots__baseline_forecasts', local_baseline_forecasts)
                log_message_variable('input$var_plots__cross_validation_metric', local_cross_val_metric)

                # GET THE CORRESPONDING ERROR METRIC
                cross_valid_function <- NULL
                if(local_cross_val_metric == 'MAE') {

                    cross_valid_function <- cv_mae

                } else if(local_cross_val_metric == 'RMSE') {

                    cross_valid_function <- cv_rmse

                } else if(local_cross_val_metric == 'MSE') {

                    cross_valid_function <- cv_mse

                } else {

                    stopifnot(FALSE)
                }

                # CONVERT LAMBDA
                local_lambda <- input$var_plots__baseline__lambda
                if(local_lambda == 'None') {

                    local_lambda <- NULL

                } else if(local_lambda == 'Auto') {

                    local_lambda <- BoxCox.lambda(local_dataset)

                } else {

                    local_lambda <- as.numeric(local_lambda)
                }

                log_message_variable('input$var_plots__baseline__lambda', local_lambda)

                results <- NULL
                methods <- c()
                if('Mean' %in% local_baseline_forecasts) {

                    methods <- c(methods, 'Mean')
                    results <- rbind(results, cross_valid_function(tsCV(local_dataset,
                                                                        forecastfunction=meanf,
                                                                        lambda=local_lambda,
                                                                        h=local_baseline_horizon)))
                }

                if('Naive' %in% local_baseline_forecasts) {

                    methods <- c(methods, 'Naive')
                    results <- rbind(results, cross_valid_function(tsCV(local_dataset,
                                                                        forecastfunction=naive,
                                                                        lambda=local_lambda,
                                                                        h=local_baseline_horizon)))
                }

                if('Seasonal Naive' %in% local_baseline_forecasts) {

                    methods <- c(methods, 'Seasonal Naive')
                    results <- rbind(results, cross_valid_function(tsCV(local_dataset,
                                                                        forecastfunction=snaive,
                                                                        lambda=local_lambda,
                                                                        h=local_baseline_horizon)))
                }

                if('Drift' %in% local_baseline_forecasts) {

                    methods <- c(methods, 'Drift')
                    results <- rbind(results, cross_valid_function(tsCV(local_dataset,
                                                                        forecastfunction=rwf,
                                                                        drift=TRUE,
                                                                        lambda=local_lambda,
                                                                        h=local_baseline_horizon)))
                }

                if('Auto' %in% local_baseline_forecasts) {

                    methods <- c(methods, 'Auto')
                    results <- rbind(results, cross_valid_function(tsCV(local_dataset,
                                                                        forecastfunction=forecast,
                                                                        lambda=local_lambda,
                                                                        h=local_baseline_horizon)))
                }

                results <- as.data.frame(results)
                steps_ahead <- colnames(results)
                results$method <- methods

                ggplot_object <- results %>% 
                    gather(key, value, -method) %>%
                    mutate(key = factor(key, levels = steps_ahead)) %>%
                    ggplot(aes(x=key, y=value, color=method, group=method)) +
                        geom_line() +
                        geom_point() +
                        expand_limits(y=0) +
                        ylab(local_cross_val_metric) + xlab('Steps Ahead') +
                        geom_text(aes(label=round(value, 1)), check_overlap=TRUE, vjust=1, hjust=1) +
                        theme(axis.text.x = element_text(angle = 30, hjust = 1))

                if(!is.null(local_lambda)) {
                    ggplot_object <- ggplot_object +
                        labs(caption=paste0('NOTE: forecasted with lambda parameter (', round(local_lambda, 3),')'))
                }

            }

            return (ggplot_object)

        })
    }, height = function() {

        session$clientData$output_var_plots_width * 0.66  # set height to % of width
    })
}

renderPrint__var_plots__residuals_ljung_box <- function(dataset, reactiveValues_model) {

    renderPrint({
        if(is.null(reactiveValues_model$model) || is_multi_time_series(dataset())) {

            return (NULL)

        } else {

            checkresiduals(reactiveValues_model$model, plot=FALSE)
        }
    })
}

##############################################################################################################
# UI updates
##############################################################################################################
observe__var_plots__hide_show_uncollapse_on_dataset_type <- function(session, dataset) {
    observeEvent(dataset(), {

        if(is_single_time_series(dataset())) {

            updateCollapse(session, 'var_plots__bscollapse', close='Variables')
            shinyjs::hide('var_plots__variables_apply')
            shinyjs::hide('var_plots__variables_toggle')
            shinyjs::hide('var_plots__facet')
            
        } else {

            shinyjs::show('var_plots__variables_apply')
            shinyjs::show('var_plots__variables_toggle')
            shinyjs::show('var_plots__facet')
            updateCollapse(session, 'var_plots__bscollapse', open='Variables')
        }

        data_frequency <- frequency(dataset())
        if(data_frequency == 4 || data_frequency == 12 || data_frequency == 52) {

            shinyjs::show('var_plots__transformation__daily_average')

        } else {

            shinyjs::hide('var_plots__transformation__daily_average')
        }
    })
}

observe__var_plots__hide_show_uncollapse_on_filtered_dataset_type <- function(session, dataset) {
    observeEvent(dataset(), {

        if(is_single_time_series(dataset())) {

            shinyjs::show('var_plots__season_plot_type')
            shinyjs::show('var_plots__auto_correlation_lags')
            shinyjs::show('autocorrelation_explanation')

            updateNumericInput(session,
                               inputId='var_plots__baseline__forecast_horizon',
                               value = 2* frequency(dataset()))

            shinyjs::show('var_plots__baseline__forecast_horizon')
            shinyjs::show('var_plots__baseline_forecasts')
            updateCollapse(session, 'var_plots__bscollapse', open='Baseline Forecasts')

        } else {

            shinyjs::hide('var_plots__season_plot_type')
            shinyjs::hide('var_plots__auto_correlation_lags')
            shinyjs::hide('autocorrelation_explanation')


            updateCollapse(session, 'var_plots__bscollapse', close='Baseline Forecasts')
            shinyjs::hide('var_plots__baseline__forecast_horizon')
            shinyjs::hide('var_plots__baseline_forecasts')
        }
    })
}

observeEvent__var_plots__variables_collapse <- function(session, dataset, reactive_values) {

    observeEvent(dataset(), ({

        # when the source is updated, set the endanger index back to zero
        reactive_values$index <- 0

        if(is_single_time_series(dataset())) {

            updateCollapse(session, "var_plots__bscollapse", style = list('Variables' = 'default'))

        } else {

            updateCollapse(session, "var_plots__bscollapse", style = list('Variables' = 'success'))
        }
    }))
}

observeEvent__var_plots__variables_toggle <- function(session, input, dataset) {

    observeEvent(input$var_plots__variables_toggle, ({
        
        local_variables <- isolate(input$var_plots__ts_variables)
        local_dataset <- isolate(dataset())
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
}

observeEvent__var_plots__variables_apply <- function(session, input) {
    
    observeEvent(input$var_plots__variables_apply, ({

        updateCollapse(session, "var_plots__bscollapse", style = list('Variables' = 'success'))

    }))
}

observeEvent__var_plots__variables_endager <- function(session, input, reactive_endager_values) {

    observeEvent(input$var_plots__ts_variables, ({

        local_endager_index <- isolate(reactive_endager_values$index)
        log_message_variable('reactiveValues__vp_can_endanger_variables$index', local_endager_index)

        if(local_endager_index >= 1) {

            updateCollapse(session, "var_plots__bscollapse", style = list('Variables' = 'danger'))
        }

        reactive_endager_values$index <- local_endager_index + 1
    }))
}
