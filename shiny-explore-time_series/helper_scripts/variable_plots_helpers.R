##############################################################################################################
# FILTERED DATASET - Variable Plot's filtered dataset
# duplicate dataset (which is bad for large datasets) so that the filters don't have to be reapplied every time.
##############################################################################################################
reactive__var_plots__filtered_data__creator <- function(input, dataset) {

    reactive({

        local_dataset <- dataset()  # clear on new datasets
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

        s <- start_end_window[1]
    }
    # if the selected filter value is different than the actual end value, keep the value to filter on
    if(start_end_window[2] != end(dataset)[1]) {

        e <- start_end_window[2]
    }

    return (list(s, e))
}

helper_create_time_series_graph <- function(input, dataset, type) {

    req(input$var_plots__date_slider)

    input$var_plots__variables_apply  # trigger update from apply, not from selecting the variables

    custom_plot <- function(dataset) {

        log_message_variable('plot type', type)
        log_message_variable('input$var_plots__season_plot_type', input$var_plots__season_plot_type)

        plot_object <- NULL

        if(type == 'time-series') {

            plot_object <- dataset %>% autoplot()

        } else if(type == 'season') {

            if(input$var_plots__season_plot_type == 'Polar') {

                plot_object <- dataset %>%  ggseasonplot(polar=TRUE)

            } else if(input$var_plots__season_plot_type == 'Sub-series') {

                plot_object <- dataset %>% ggsubseriesplot()

            } else {

                plot_object <- dataset %>% ggseasonplot(year.labels=TRUE, year.labels.left=TRUE)
            }

        } else if(type == 'scatter-matrix') {

            plot_object <- dataset %>% as.data.frame() %>% GGally::ggpairs()

        } else {

            stopifnot(FALSE)
        }

        return (plot_object)
    }

    # reactive data
    local_dataset <- dataset()
    local_y_zoom_min <- input$var_plots__y_zoom_min
    local_y_zoom_max <- input$var_plots__y_zoom_max
    local_ts_variables <- isolate(input$var_plots__ts_variables)  # don't update when selecting variables

    # filter on window
    local_start_end <- input$var_plots__date_slider
    local_start_end <- convert_start_end_window(local_dataset, local_start_end)
    local_dataset <- window(local_dataset, start=local_start_end[[1]], end=local_start_end[[2]])


    log_message_variable('input$var_plots__date_slider', paste0(local_start_end, collapse='-'))
    log_message_variable('var_plots__y_zoom_min', local_y_zoom_min)
    log_message_variable('var_plots__y_zoom_max', local_y_zoom_max)
    log_message_variable('input$var_plots__ts_variables', local_ts_variables)


    ggplot_object <- NULL

    if(is_single_time_series(local_dataset)) {

        log_message('**single ts**')
        ggplot_object <- local_dataset %>% custom_plot()
        
    } else if (is_multi_time_series(local_dataset)) {

        log_message('**multi ts**')

        if(is.null(local_ts_variables)) {

            ggplot_object <- NULL

        } else {

            local_dataset <- local_dataset[, local_ts_variables]
            ggplot_object <- local_dataset %>% custom_plot()
        }
    
    } else {
        
        stopifnot(FALSE)
    }

    # zoom in on graph if either parameter is set
    if(!is.null(local_dataset) && (!is.na(local_y_zoom_min) || !is.na(local_y_zoom_max))) {
        # if one of the zooms is specified then we hae to provide both, so get corresponding min/max

        if(is.na(local_y_zoom_min)) {

            local_y_zoom_min <- min(local_dataset, na.rm = TRUE)
        }

        if(is.na(local_y_zoom_max)) {

            local_y_zoom_max <- max(local_dataset, na.rm = TRUE)
        }

        ggplot_object <- ggplot_object + coord_cartesian(ylim = c(local_y_zoom_min, local_y_zoom_max))
    }

    return (ggplot_object)

}

reactive__var_plots__ggplot__creator <- function(input, dataset) {
    reactive({
        return (helper_create_time_series_graph(input, dataset, type='time-series'))
    })
}

reactive__var_plots__season__ggplot__creator <- function(input, dataset) {
    
    reactive({
        return (helper_create_time_series_graph(input, dataset, type='season'))
    })
}

reactive__var_plots__scatter_matrix__ggplot__creator <- function(input, dataset) {
    
    reactive({
        return (helper_create_time_series_graph(input, dataset, type='scatter-matrix'))
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


# renderUI__var_plots__variable__UI <- function(dataset) {

#     renderUI({
#         selectInput(inputId='var_plots__variable',
#                     label = 'Variable',
#                     choices = c(select_variable, colnames(dataset())),
#                     selected = select_variable,
#                     multiple = FALSE,
#                     selectize = TRUE,
#                     width = 500,
#                     size = NULL)
#     })
# }

observe__var_plots__hide_show_uncollapse_on_dataset_type <- function(session, dataset) {
    observeEvent(dataset(), {

        if(is_multi_time_series(dataset())) {

            shinyjs::show('var_plots__variables_apply')
            shinyjs::show('var_plots__variables_toggle')
            updateCollapse(session, 'var_plots__bscollapse', open='Variables')

        } else {

            updateCollapse(session, 'var_plots__bscollapse', close='Variables')
            shinyjs::hide('var_plots__variables_apply')
            shinyjs::hide('var_plots__variables_toggle')
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

renderPlot__var_plots__seasonal <- function(session, ggplot_object) {
    renderPlot({

        withProgress(value=1/2, message='Creating Seasonal Graph',{

           print(ggplot_object())
        })

    }, height = function() {

        session$clientData$output_var_plots_width * 0.66  # set height to % of width
    })
}

renderPlot__var_plots__scatter_matrix <- function(session, ggplot_object) {
    renderPlot({

        withProgress(value=1/2, message='Creating Scatter Matrix Graph',{

           print(ggplot_object())
        })

    }, height = function() {

        session$clientData$output_var_plots_width * 0.66  # set height to % of width
    })
}


renderPrint__reactiveValues__vp__ggplot_message <- function(message) {

    renderPrint({
        cat(message$value)
    })
}

