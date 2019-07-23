##############################################################################################################
# FILTERED DATASET - Variable Plot's filtered dataset
# duplicate dataset (which is bad for large datasets) so that the filters don't have to be reapplied every time.
##############################################################################################################
reactive__var_plots__filtered_data__creator <- function(input, dataset, date_slider, reactiveValue_trans) {

    reactive({

        req(date_slider())
        input$var_plots__variables_apply  # trigger update from apply, not from selecting the variables

        log_message_block_start("Creating filtered dataset...")

        local_dataset <- dataset()  # clear on new datasets

        if(input$var_plots__include_last_point == FALSE) {
            # remove last point
            if(rt_ts_is_single_variable(local_dataset)) {

                local_dataset <- head(local_dataset, length(local_dataset) - 1)
            } else {

                local_dataset <- head(local_dataset, nrow(local_dataset) - 1)
            }
        }

        # filter on window
        local_start_end <- date_slider()

        # if the start of the date slider is before the start of the time-series or
        # the end of the date slider is after the end of the time-series, we've probably switched datasets
        # and the slider is not valid, so let's skip the update and this code will be triggered when the 
        # date_slider is updated
        if(local_start_end[1] < start(local_dataset)[1] || local_start_end[2] > end(local_dataset)[1]) {

            log_message_variable('date slider before start of ts:', local_start_end[1] < start(local_dataset)[1])
            log_message_variable('date slider after end of ts:', local_start_end[2] > end(local_dataset)[1])
            log_message('Aborting creation of filtered dataset...')
            return (NULL)
        }

        local_start_end <- convert_start_end_window(local_dataset, local_start_end)
        log_message_variable('input$var_plots__date_slider', paste0(local_start_end, collapse='-'))
        local_dataset <- window(local_dataset,
                                start=local_start_end[[1]],
                                end=local_start_end[[2]])

        if(rt_ts_is_single_variable(local_dataset)) {

            log_message('**single ts**')
            
        } else if (rt_ts_is_multi_variable(local_dataset)) {

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

helper_add_baseline_forecasts <- function(ggplot_object, input, dataset, reactiveValues_models) {

    ######################################################################################################
    # BASELINE FORECASTS
    ######################################################################################################
    local_baseline_forecasts <- input$var_plots__baseline_forecasts
    local_baseline_horizon <- input$var_plots__baseline__forecast_horizon

    if(rt_ts_is_single_variable(dataset) &&
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

        local_biasadj <- input$var_plots__baseline__biasadj
        log_message_variable('input$var_plots__baseline__biasadj', local_biasadj)

        local_damped <- input$var_plots__baseline__damped
        log_message_variable('input$var_plots__baseline__damped', local_damped)


        local_models <- NULL
        show_PI <- length(local_baseline_forecasts) == 1  # if multiple forecasts, don't show PI

        if('Mean' %in% local_baseline_forecasts) {

            forecast_model <- meanf(dataset,
                                    h=local_baseline_horizon,
                                    lambda=local_lambda,
                                    biasadj=local_biasadj)
            comment(forecast_model) <- 'Mean'

            log_message_variable('forecast model method', forecast_model$method)
            local_models <- c(local_models, list(forecast_model))

            ggplot_object <- ggplot_object +
                autolayer(forecast_model,
                          series='Mean',
                          PI=show_PI)
        }

        if('Naive' %in% local_baseline_forecasts) {

            forecast_model <- naive(dataset,
                                    h=local_baseline_horizon,
                                    lambda=local_lambda,
                                    biasadj=local_biasadj)
            comment(forecast_model) <- 'Naive'

            log_message_variable('forecast model method', forecast_model$method)
            local_models <- c(local_models, list(forecast_model))

            ggplot_object <- ggplot_object +
                autolayer(forecast_model,
                          series='Naive',
                          PI=show_PI)
        }

        if('Seasonal Naive' %in% local_baseline_forecasts) {

            forecast_model <- snaive(dataset,
                                     h=local_baseline_horizon,
                                     lambda=local_lambda,
                                     biasadj=local_biasadj)
            comment(forecast_model) <- 'Seasonal Naive'

            log_message_variable('forecast model method', forecast_model$method)
            local_models <- c(local_models, list(forecast_model))

            ggplot_object <- ggplot_object +
                autolayer(forecast_model,
                          series='Seasonal Naive',
                          PI=show_PI)
        }

        if('Drift' %in% local_baseline_forecasts) {

            forecast_model <- rwf(dataset,
                                  h=local_baseline_horizon,
                                  lambda=local_lambda,
                                  biasadj=local_biasadj,
                                  drift=TRUE)
            comment(forecast_model) <- 'Drift'

            log_message_variable('forecast model method', forecast_model$method)
            local_models <- c(local_models, list(forecast_model))

            ggplot_object <- ggplot_object +
                autolayer(forecast_model,
                          series='Drift',
                          PI=show_PI)
        }

        if('Natural Cubic Smoothing Spline' %in% local_baseline_forecasts) {

            forecast_model <- splinef(dataset,
                                      h=local_baseline_horizon,
                                      lambda=local_lambda,
                                      biasadj=local_biasadj)
            comment(forecast_model) <- 'Natural Cubic Smoothing Spline'

            log_message_variable('forecast model method', forecast_model$method)
            local_models <- c(local_models, list(forecast_model))

            ggplot_object <- ggplot_object +
                autolayer(forecast_model,
                          series='Natural Cubic Smoothing Spline',
                          PI=show_PI)
        }

        if('Auto' %in% local_baseline_forecasts) {

            forecast_model <- forecast(dataset,
                                       h=local_baseline_horizon,
                                       lambda=local_lambda,
                                       damped=local_damped,
                                       biasadj=local_biasadj)
            comment(forecast_model) <- 'Auto'

            log_message_variable('forecast model method', forecast_model$method)
            local_models <- c(local_models, list(forecast_model))

            ggplot_object <- ggplot_object +
                autolayer(forecast_model,
                          series='Auto',
                          PI=show_PI)
        }

        if("ETS" %in% local_baseline_forecasts) {

            forecast_model <- ets(dataset,
                                  lambda=local_lambda,
                                  damped=local_damped,
                                  biasadj=local_biasadj) %>%
                 forecast(h=local_baseline_horizon)

            comment(forecast_model) <- "ETS"

            log_message_variable('forecast model method', forecast_model$method)
            local_models <- c(local_models, list(forecast_model))

            ggplot_object <- ggplot_object +
                autolayer(forecast_model,
                          series="ETS",
                          PI=show_PI)
        }

        if("STL + ETS" %in% local_baseline_forecasts) {

            forecast_model <- stlf(dataset,
                                   h=local_baseline_horizon,
                                   lambda=local_lambda,
                                   damped=local_damped,
                                   biasadj=local_biasadj)
            comment(forecast_model) <- "STL + ETS"

            log_message_variable('forecast model method', forecast_model$method)
            local_models <- c(local_models, list(forecast_model))

            ggplot_object <- ggplot_object +
                autolayer(forecast_model,
                          series="STL + ETS",
                          PI=show_PI)
        }

        if("Auto ARIMA (approx.)" %in% local_baseline_forecasts) {

            forecast_model <- auto.arima(dataset,
                                   lambda=local_lambda,
                                   biasadj=local_biasadj,
                                   stepwise=TRUE,
                                   approximation=TRUE) %>% 
                forecast(h=local_baseline_horizon)

            comment(forecast_model) <- "Auto ARIMA (approx.)"

            log_message_variable('forecast model method', forecast_model$method)
            local_models <- c(local_models, list(forecast_model))

            ggplot_object <- ggplot_object +
                autolayer(forecast_model,
                          series="Auto ARIMA (approx.)",
                          PI=show_PI)
        }

        if("Auto ARIMA (all)" %in% local_baseline_forecasts) {

            forecast_model <- auto.arima(dataset,
                                   lambda=local_lambda,
                                   biasadj=local_biasadj,
                                   stepwise=FALSE,
                                   approximation=FALSE) %>% 
                forecast(h=local_baseline_horizon)

            comment(forecast_model) <- "Auto ARIMA (all)"

            log_message_variable('forecast model method', forecast_model$method)
            local_models <- c(local_models, list(forecast_model))

            ggplot_object <- ggplot_object +
                autolayer(forecast_model,
                          series="Auto ARIMA (all)",
                          PI=show_PI)
        }

        reactiveValues_models$models <- local_models

        if(input$var_plots__baseline__show_fitted_line) {
            for(model in local_models) {

                ggplot_object <- ggplot_object + 
                    autolayer(fitted(model), series=comment(model))
            }
        }

        if(input$var_plots__baseline__show_forecast_values) {

            add_forecasts_from_model <- function(ggplot_object, model) {

                s <- start(model$mean)
                e <- end(model$mean)
                f <- frequency(model$mean)

                ts_forecast <- ts(as.data.frame(model)$`Point Forecast`,
                                  start = s,
                                  end=e,
                                  frequency = f)

                return (ggplot_object + 
                    geom_point(data= ts_forecast) + 
                    geom_text(data=ts_forecast,
                              aes(label=format_labels(as.numeric(ts_forecast))), 
                              check_overlap=TRUE,
                              vjust=0,
                              hjust=0))
            }

            for(model in local_models) {

                ggplot_object <- ggplot_object %>% add_forecasts_from_model(model)
            }
        }

        if(!is.null(local_lambda) || local_biasadj || local_damped) {

            caption <- ''
            if(!is.null(local_lambda)) {

                caption <- paste0('NOTE: forecasted with lambda parameter (', round(local_lambda, 3),')')
            }
            if(local_biasadj) {

                caption <- paste0(c(caption, 'NOTE: using Bias Adjustment'), collapse='\n')
            }
            if(local_damped) {

                caption <- paste0(c(caption, 'NOTE: using Damped Trend'), collapse='\n')
            }

            ggplot_object <- ggplot_object + labs(caption=caption)
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

    if(max(values, na.rm=TRUE) > 1000000) {

        values <- paste0(round(values / 1000000, 2), 'M')

    } else if(max(values, na.rm=TRUE) > 100000) {

        values <- paste0(round(values / 1000, 1), 'K')

    } else if(max(values, na.rm=TRUE) > 1000) {

        values <- paste0(round(values / 1000, 1), 'K')

    } else if(max(values, na.rm=TRUE) > 100) {

        values <- round(values, 0)

    } else if(max(values, na.rm=TRUE) < 1) {

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
    if(!is.null(input$var_plots__show_points) && input$var_plots__show_points) {

        ggplot_object <- ggplot_object +
            geom_point()
    }

    if(!is.null(input$var_plots__show_values) && input$var_plots__show_values) {

        ggplot_object <- ggplot_object +
            geom_text(aes(label=format_labels(as.numeric(dataset))), check_overlap=TRUE, vjust=1, hjust=1, na.rm = TRUE)
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


reactive__var_plots__ggplot__creator <- function(input, dataset, reactiveValue_trans, reactiveValues_models) {
    reactive({

        req(dataset())

        if(rt_ts_is_single_variable(dataset())) {

            req(input$var_plots__baseline__forecast_horizon)
        }



        log_message_block_start("Creating `time-series` graph...")

       # reactive data
        local_dataset <- dataset()

        # for now, if we have forecasts, let's use auto-layer so the forecasting autolayers work
        if(is.null(input$var_plots__baseline_forecasts) && length(input$var_plots__baseline_forecasts) == 0) {

            ggplot_object <- rt_ts_plot_time_series(dataset=local_dataset,
                                                    show_values=input$var_plots__show_values,
                                                    show_points=input$var_plots__show_points,
                                                    show_dates=input$var_plots__show_dates,
                                                    #include_last_point=input$var_plots__show_last_point,
                                                    y_zoom_min=input$var_plots__y_zoom_min,
                                                    y_zoom_max=input$var_plots__y_zoom_max,
                                                    facet_multi_variables=input$var_plots__facet,
                                                    base_size=global__theme_base_size) %>%
                helper_add_baseline_forecasts(input, local_dataset, isolate(reactiveValues_models)) %>%
                #helper_y_zoom(input, local_dataset) %>%
                #helper_add_labels(input, local_dataset) %>%
                helper_add_transformation_y_axis_label(reactiveValue_trans)

        } else {

            if(is.null(input$var_plots__facet) || !input$var_plots__facet || rt_ts_is_single_variable(local_dataset)) {
        
                ggplot_object <- local_dataset %>%
                    autoplot()
            
            } else {

                ggplot_object <- local_dataset %>%
                    autoplot(facets=input$var_plots__facet)
                
            }

            ggplot_object <- ggplot_object %>%
                helper_add_baseline_forecasts(input, local_dataset, isolate(reactiveValues_models)) %>%
                helper_y_zoom(input, local_dataset) %>%
                helper_add_labels(input, local_dataset) %>%
                helper_add_transformation_y_axis_label(reactiveValue_trans)
        }


        forecast_horizon <- NULL
        if(!is.null(input$var_plots__baseline_forecasts) && length(input$var_plots__baseline_forecasts) > 0 && !is.null(input$var_plots__baseline__forecast_horizon)) {
            forecast_horizon <- input$var_plots__baseline__forecast_horizon
        }
        local_start_end <- convert_start_end_window_decimal(local_dataset,
                                                           input$var_plots__date_zoom_slider,
                                                           forecast_horizon=forecast_horizon)
        ggplot_object <- ggplot_object +
            coord_cartesian(xlim=c(local_start_end[[1]], local_start_end[[2]]))
    })
}

reactive__var_plots__auto_correlation__ggplot__creator <- function(input, dataset, reactiveValue_trans) {
    
    reactive({

        req(dataset())

        log_message_block_start("Creating `auto-correlation` graph...")

        # reactive data
        local_dataset <- dataset()
        ggplot_object <- NULL

        if(rt_ts_is_single_variable(local_dataset)) {

            lags <- NULL
            if(!is.na(input$var_plots__auto_correlation_lags)) {

                lags <- input$var_plots__auto_correlation_lags
            }

            log_message_variable('input$var_plots__auto_correlation_lags', lags)
            ggplot_object <- local_dataset %>% ggAcf(lag=lags) +
            theme_light()
        }
    })
}

reactive__var_plots__season__ggplot__creator <- function(input, dataset, reactiveValue_trans) {
    
    reactive({

        req(dataset())

        if(rt_ts_is_single_variable(dataset())) {

            req(input$var_plots__baseline__forecast_horizon)
        }

        log_message_block_start("Creating `season` graph...")

        # reactive data
        local_dataset <- dataset()
        ggplot_object <- NULL

        if(rt_ts_is_single_variable(local_dataset)) {

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
                    helper_add_transformation_y_axis_label(reactiveValue_trans) +
                    theme_light() +
                    expand_limits(y=0)
            }
        }

    })
}

reactive__var_plots__scatter_matrix__ggplot__creator <- function(input, dataset, reactiveValue_trans) {
    
    reactive({

        req(dataset())

        if(rt_ts_is_single_variable(dataset())) {

            req(input$var_plots__baseline__forecast_horizon)
        }

        log_message_block_start("Creating `scatter-matrix` graph...")

        # reactive data
        local_dataset <- dataset()
        ggplot_object <- NULL

        if(rt_ts_is_multi_variable(local_dataset)) {

            ggplot_object <- local_dataset %>%
                as.data.frame() %>%
                GGally::ggpairs()%>%
                helper_add_transformation_y_axis_label(reactiveValue_trans) +
                theme_light()
        } 
    })
}

##############################################################################################################
# INPUT
##############################################################################################################
renderUI__var_plots__ts_variables__UI <- function(dataset) {

    renderUI({

        local_dataset <- dataset()

        if(rt_ts_is_multi_variable(local_dataset)) {

            column_names <- colnames(as.data.frame(local_dataset) %>% select_if(is.numeric))

            selectInput(inputId='var_plots__ts_variables',
                        label = NULL,
                        choices = column_names,
                        selected = column_names[1],
                        multiple = TRUE,
                        selectize = TRUE,
                        #width = 500,
                        size = NULL)

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

        withProgress(value=1/2, message="Creating Time-Series Graph",{

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
                                         function() { session$clientData$output_var_plots__seasonal_width * 0.55 }))
}

renderPlot__var_plots__scatter_matrix <- function(session, ggplot_object) {

    return(renderPlot__var_plots__helper(session,
                                         ggplot_object,
                                         'Creating Scatter Matrix Graph',
                                         function() { session$clientData$output_var_plots__scatter_matrix_width * 0.66 }))
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

renderDataTable__var_plots__residuals_means <- function(reactiveValues_models) {

    renderDataTable({
        
        local_models <- reactiveValues_models$models
        
        if(is.null(local_models) || length(local_models) == 0) {

            return (NULL)
        }


        resid_stats <- map(local_models, ~ {
            resids <- residuals(.)
            resid_mean <- mean(resids, na.rm = TRUE)
            resid_stan_dev <- sd(resids, na.rm = TRUE)
                
            return (data.frame(Model=comment(.),
                               Mean=round(resid_mean, 3),
                               `Standard Deviation`=round(resid_stan_dev, 3),
                               `Coefficient of Variation`=round(resid_stan_dev / resid_mean, 3)))
        })

        resid_stats_df <- do.call("rbind", resid_stats)
        colnames(resid_stats_df) <- str_replace_all(colnames(resid_stats_df), '\\.', ' ')
        
        return (resid_stats_df)

    }, options = list(searching = FALSE, paging = FALSE))
}

renderPlot__var_plots__residuals_plot <- function(session, reactiveValues_models) {

    renderPlot({

        local_models <- reactiveValues_models$models
        if(is.null(local_models) || length(local_models) != 1) {

            return (NULL)

        } else {

            withProgress(value=1/2, message="Creating Residuals Graph",{

               checkresiduals(local_models[[1]])
            })
        }

    }, height = function() {

        session$clientData$output_var_plots__residuals_plot_width * 0.66  # set height to % of width
    })
}

renderPrint__var_plots__residuals_ljung_box <- function(reactiveValues_models) {

    renderPrint({

        local_models <- reactiveValues_models$models
        if(is.null(local_models) || length(local_models) != 1) {

            return (NULL)

        } else {

            checkresiduals(local_models[[1]], plot=FALSE)
        }
    })
}

renderPlot__var_plots__cross_validation <- function(session, input, dataset) {

    renderPlot({
            
        withProgress(value=1/2, message="Creating Cross Validation Graph",{

            log_message_block_start("Creating `cross validation` graph...")

            req(input$var_plots__cross_validation_metric)

            local_dataset <- dataset()
            local_baseline_forecasts <- input$var_plots__baseline_forecasts
            local_baseline_horizon <- input$var_plots__baseline__forecast_horizon
            local_cross_val_metric <- input$var_plots__cross_validation_metric

            ggplot_object <- NULL

            if(rt_ts_is_single_variable(local_dataset) &&
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

                local_biasadj <- input$var_plots__baseline__biasadj
                log_message_variable('input$var_plots__baseline__biasadj', local_biasadj)

                local_damped <- input$var_plots__baseline__damped
                log_message_variable('input$var_plots__baseline__damped', local_damped)

                results <- NULL
                methods <- c()
                if('Mean' %in% local_baseline_forecasts) {

                    methods <- c(methods, 'Mean')
                    results <- rbind(results, cross_valid_function(tsCV(local_dataset,
                                                                        forecastfunction=meanf,
                                                                        lambda=local_lambda,
                                                                        biasadj=local_biasadj,
                                                                        h=local_baseline_horizon)))
                }

                if('Naive' %in% local_baseline_forecasts) {

                    methods <- c(methods, 'Naive')
                    results <- rbind(results, cross_valid_function(tsCV(local_dataset,
                                                                        forecastfunction=naive,
                                                                        lambda=local_lambda,
                                                                        biasadj=local_biasadj,
                                                                        h=local_baseline_horizon)))
                }

                if('Seasonal Naive' %in% local_baseline_forecasts) {

                    methods <- c(methods, 'Seasonal Naive')
                    results <- rbind(results, cross_valid_function(tsCV(local_dataset,
                                                                        forecastfunction=snaive,
                                                                        lambda=local_lambda,
                                                                        biasadj=local_biasadj,
                                                                        h=local_baseline_horizon)))
                }

                if('Drift' %in% local_baseline_forecasts) {

                    methods <- c(methods, 'Drift')
                    results <- rbind(results, cross_valid_function(tsCV(local_dataset,
                                                                        forecastfunction=rwf,
                                                                        drift=TRUE,
                                                                        lambda=local_lambda,
                                                                        biasadj=local_biasadj,
                                                                        h=local_baseline_horizon)))
                }

                if('Natural Cubic Smoothing Spline' %in% local_baseline_forecasts) {

                    methods <- c(methods, 'Natural Cubic Smoothing Spline')
                    results <- rbind(results, cross_valid_function(tsCV(local_dataset,
                                                                        forecastfunction=splinef,
                                                                        lambda=local_lambda,
                                                                        biasadj=local_biasadj,
                                                                        h=local_baseline_horizon)))
                }

                if('Auto' %in% local_baseline_forecasts) {

                    methods <- c(methods, 'Auto')
                    results <- rbind(results, cross_valid_function(tsCV(local_dataset,
                                                                        forecastfunction=forecast,
                                                                        lambda=local_lambda,
                                                                        biasadj=local_biasadj,
                                                                        damped=local_damped,
                                                                        h=local_baseline_horizon)))
                }

                if("ETS" %in% local_baseline_forecasts) {

                    forecast.ets <- function(x, h) {

                        forecast(ets(dataset,
                                     lambda=local_lambda,
                                     damped=local_damped,
                                     biasadj=local_biasadj),
                                h=h)
                    }


                    methods <- c(methods, "ETS")
                    results <- rbind(results, cross_valid_function(tsCV(local_dataset,
                                                                        forecastfunction=forecast.ets,
                                                                        # lambda=local_lambda,
                                                                        # biasadj=local_biasadj,
                                                                        h=local_baseline_horizon)))
                }

                if("STL + ETS" %in% local_baseline_forecasts) {

                    methods <- c(methods, "STL + ETS")
                    results <- rbind(results, cross_valid_function(tsCV(local_dataset,
                                                                        forecastfunction=stlf,
                                                                        lambda=local_lambda,
                                                                        damped=local_damped,
                                                                        biasadj=local_biasadj,
                                                                        h=local_baseline_horizon)))
                }

                if("Auto ARIMA (approx.)" %in% local_baseline_forecasts) {

                    forecast.arima <- function(x, h) {

                        auto.arima(x,
                                   lambda=local_lambda,
                                   biasadj=local_biasadj,
                                   stepwise=TRUE,
                                   approximation=TRUE) %>% 
                            forecast(h=h)
                    }

                    methods <- c(methods, "Auto ARIMA (approx.)")
                    results <- rbind(results, cross_valid_function(tsCV(local_dataset,
                                                                        forecastfunction=forecast.arima,
                                                                        # lambda=local_lambda,
                                                                        # damped=local_damped,
                                                                        # biasadj=local_biasadj,
                                                                        h=local_baseline_horizon)))
                }

                if("Auto ARIMA (all)" %in% local_baseline_forecasts) {

                    forecast.arima <- function(x, h) {

                        auto.arima(x,
                                   lambda=local_lambda,
                                   biasadj=local_biasadj,
                                   stepwise=FALSE,
                                   approximation=FALSE) %>% 
                            forecast(h=h)
                    }


                    methods <- c(methods, "Auto ARIMA (all)")
                    results <- rbind(results, cross_valid_function(tsCV(local_dataset,
                                                                        forecastfunction=forecast.arima,
                                                                        # lambda=local_lambda,
                                                                        # damped=local_damped,
                                                                        # biasadj=local_biasadj,
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
                        theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
                        theme_light()

                if(!is.null(local_lambda) || local_biasadj) {

                    caption <- ''
                    if(!is.null(local_lambda)) {

                        caption <- paste0('NOTE: forecasted with lambda parameter (', round(local_lambda, 3),')')
                    }
                    if(local_biasadj) {

                        caption <- paste0(c(caption, 'NOTE: using Bias Adjustment'), collapse='\n')
                    }

                    ggplot_object <- ggplot_object + labs(caption=caption)
                }

            }

            return (ggplot_object)
        })
    }, height = function() {

        session$clientData$output_var_plots__cross_validation_width * 0.66  # set height to % of width
    })
}


regression_decomposition <- function(dataset) {

    regression <- tslm(dataset ~ trend + season)
    # trend <- Intercept + Trend ... seq_along creates a sequence of trends corresponding to the number of data-points in local_dataset
    trend <- coef(regression)[1] + coef(regression)['trend'] * seq_along(dataset)
    season <- dataset - trend - residuals(regression)
    
    # the seasonal components may throw off the trend (e.g. reference season is 0 and all the rest are negative);
    # so lets move the trend up/down by the median i.e. center the trend around the season (i.e. season coefficiients);
    adjustment <- median(season) * -1
    trend <- trend - adjustment
    
    return (cbind(
        data = dataset,
        trend = trend,
        season = dataset - trend - residuals(regression),
        remainder = residuals(regression)
    ))
}

renderPlot__var_plots__decomposition <- function(session, input, dataset) {

    renderPlot({
            
        req(dataset())
        req(input$var_plots__decomposition_type)

        local_dataset <- dataset()
        decom_object <- NULL

        withProgress(value=1/2, message="Creating Decomposition Graph",{

            log_message_block_start("Creating `decomposition` graph...")
            log_message_variable("input$var_plots__decomposition_type", input$var_plots__decomposition_type)


            if(input$var_plots__decomposition_type == 'X11') {

                decom_object <- local_dataset %>%
                                seas(x11="") %>%
                                autoplot() +
                                    ggtitle("X11 Decomposition")

            } else if(input$var_plots__decomposition_type == 'SEATS') {

                decom_object <- local_dataset %>%
                                seas() %>%
                                autoplot() +
                                    ggtitle("SEATS Decomposition")

            } else if(input$var_plots__decomposition_type == 'STL') {

                decom_object <- local_dataset %>%
                                mstl() %>%
                                autoplot() +
                                    ggtitle("STL Decomposition")

            } else if(input$var_plots__decomposition_type == 'Regression') {

                components <- regression_decomposition(dataset = local_dataset)
                decom_object <- autoplot(components, facet=TRUE, scales = "fixed") +
                    ggtitle("Trend/Season Regression Decomposition")

            }

            decom_object <- decom_object +
            theme_light()

        })

        return (decom_object)
    }, height = function() {

        session$clientData$output_var_plots__decomposition_width * 0.65  # set height to % of width
    })
}

renderPlot__var_plots__decomposition_trend_season <- function(session, input, dataset) {

    renderPlot({
        log_message_block_start("Creating `decomposition` data graph...")
            
        req(dataset())
        req(input$var_plots__decomposition_type)
        # req(input$var_plots__decomposition__show_trend)
        # req(input$var_plots__decomposition__show_season)

        local_dataset <- dataset()
        decom_object <- NULL

        withProgress(value=1/2, message="Creating Decomposition Data Graph",{

            log_message_variable("input$var_plots__decomposition_type", input$var_plots__decomposition_type)
            log_message_variable("input$var_plots__decomposition__show_trend", input$var_plots__decomposition__show_trend)
            log_message_variable("input$var_plots__decomposition__show_season", input$var_plots__decomposition__show_season)

            if(input$var_plots__decomposition_type == 'X11') {

                decom_fit <- local_dataset %>% seas(x11="")
                trend_layer <- autolayer(trendcycle(decom_fit), series="Trend")
                seas_adj_layer <- autolayer(seasadj(decom_fit), series="Seasonally Adjusted")

            } else if(input$var_plots__decomposition_type == 'SEATS') {

                decom_fit <- local_dataset %>% seas()
                trend_layer <- autolayer(trendcycle(decom_fit), series="Trend")
                seas_adj_layer <- autolayer(seasadj(decom_fit), series="Seasonally Adjusted")

            } else if(input$var_plots__decomposition_type == 'STL') {

                decom_fit <- local_dataset %>% mstl()
                trend_layer <- autolayer(trendcycle(decom_fit), series="Trend")
                seas_adj_layer <- autolayer(seasadj(decom_fit), series="Seasonally Adjusted")
            
            } else if(input$var_plots__decomposition_type == "Regression") {

                components <- regression_decomposition(dataset = local_dataset)

                                # create a ts object of the trend
                ts_trend <- ts(components[, 'trend'],
                               start=start(local_dataset),
                               frequency = frequency(local_dataset))
                trend_layer <- autolayer(ts_trend, series="Trend")

                # calculate the seasonally adjusted values by subtracting the seasonal component from the local_dataset.
                adjust_df <- local_dataset - components[,'season']
                seas_adj_layer <- autolayer(adjust_df, series="Seasonally Adjusted")
            }

            decom_object <- local_dataset %>% autoplot(series="Data")
            scale_colour_manual_values <- c('gray')
            scale_colour_manual_breaks <- c("Data")

            if(input$var_plots__decomposition__show_season) {

                decom_object <- decom_object + trend_layer
                scale_colour_manual_values <- c(scale_colour_manual_values, 'blue')
                scale_colour_manual_breaks <- c(scale_colour_manual_breaks, "Seasonally Adjusted")
            }

            if(input$var_plots__decomposition__show_trend) {

                decom_object <- decom_object + seas_adj_layer
                scale_colour_manual_values <- c(scale_colour_manual_values, 'red')
                scale_colour_manual_breaks <- c(scale_colour_manual_breaks, "Trend")
            }
        })

        decom_object <- decom_object + scale_colour_manual(values=scale_colour_manual_values,
                                                           breaks=scale_colour_manual_breaks) +
        theme_light() +
        expand_limits(y=0)

        return (decom_object)

    }, height = function() {

        session$clientData$output_var_plots__decomposition_trend_season_width * 0.55  # set height to % of width
    })
}

##############################################################################################################
# UI updates
##############################################################################################################
observe__var_plots__hide_show_uncollapse_on_dataset_type <- function(session, dataset) {
    observeEvent(dataset(), {

        if(rt_ts_is_single_variable(dataset())) {

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

        if(rt_ts_is_single_variable(dataset())) {

            shinyjs::show('var_plots__season_plot_type')
            shinyjs::show('var_plots__auto_correlation_lags')
            shinyjs::show('autocorrelation_explanation')

            updateNumericInput(session,
                               inputId='var_plots__baseline__forecast_horizon',
                               value = 2 * frequency(dataset()))

            shinyjs::show('var_plots__baseline__forecast_horizon')
            shinyjs::show('var_plots__baseline_forecasts')
            #updateCollapse(session, 'var_plots__bscollapse', open='Baseline Forecasts')

        } else {

            shinyjs::hide('var_plots__season_plot_type')
            shinyjs::hide('var_plots__auto_correlation_lags')
            shinyjs::hide('autocorrelation_explanation')


            #updateCollapse(session, 'var_plots__bscollapse', close='Baseline Forecasts')
            shinyjs::hide('var_plots__baseline__forecast_horizon')
            shinyjs::hide('var_plots__baseline_forecasts')
        }
    })
}

observeEvent__var_plots__variables_collapse <- function(session, dataset, reactive_values) {

    observeEvent(dataset(), ({

        # when the source is updated, set the endanger index back to zero
        reactive_values$index <- 0

        if(rt_ts_is_single_variable(dataset())) {

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

            updateSelectInput(session=session,
                              inputId='var_plots__ts_variables',
                              selected=column_names)
        } else {

            updateSelectInput(session=session,
                              inputId='var_plots__ts_variables',
                              selected=character(0))

            # seems like a bug, updateSelectInput doesn't trigger observeEvent for var_plots__ts_variables
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
