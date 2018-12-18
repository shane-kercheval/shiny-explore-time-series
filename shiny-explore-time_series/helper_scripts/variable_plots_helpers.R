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

reactive__var_plots__ggplot__creator <- function(input, session, dataset) {
    reactive({

        req(input$var_plots__date_slider)

        # reactive data
        local_dataset <- dataset()
        local_start_end <- input$var_plots__date_slider
        local_start_end <- convert_start_end_window(local_dataset, local_start_end)
        log_message_variable('input$var_plots__date_slider', paste0(local_start_end, collapse='-'))
        local_dataset <- window(local_dataset, start=local_start_end[[1]], end=local_start_end[[2]])


        local_ts_variables <- input$var_plots__ts_variables
        log_message_variable('input$var_plots__ts_variables', local_ts_variables)
# local_annotate_points <- input$var_plots__annotate_points
# local_base_size <- input$var_plots__base_size
# local_pretty_text <- input$var_plots__pretty_text

# local_y_zoom_min <- input$var_plots__y_zoom_min
# local_y_zoom_max <- input$var_plots__y_zoom_max



        ggplot_object <- NULL

        if(is_single_time_series(local_dataset)) {

            log_message('**single ts**')
            ggplot_object <- local_dataset %>% autoplot()
            
        } else if (is_multi_time_series(local_dataset)) {

            log_message('**multi ts**')

            if(is.null(local_ts_variables)) {

                ggplot_object <- NULL

            } else {

                local_dataset <- local_dataset[, input$var_plots__ts_variables]
                ggplot_object <- local_dataset %>% autoplot()
            }
        
        } else {
            
            stopifnot(FALSE)
        }

        return (ggplot_object)
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

            updateCollapse(session, 'var_plots__bscollapse', open='Variables')

        } else {

            updateCollapse(session, 'var_plots__bscollapse', close='Variables')
        }
    })
}

##############################################################################################################
# OUTPUT
##############################################################################################################
renderPlot__variable_plot <- function(session, ggplot_object, messages) {

    renderPlot({

        withProgress(value=1/2, message='Plotting Graph',{

           messages$value <- capture_messages_warnings(function() print(ggplot_object()))
           log_message_variable('messages$value', messages$value)

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

