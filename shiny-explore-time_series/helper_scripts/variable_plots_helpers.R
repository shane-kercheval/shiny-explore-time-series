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
reactive__var_plots__ggplot__creator <- function(input, session, dataset) {
    reactive({

        #req(input$var_plots__variable)

        # reactive data
        local_dataset <- dataset()

        local_annotate_points <- input$var_plots__annotate_points
        local_base_size <- input$var_plots__base_size
        local_pretty_text <- input$var_plots__pretty_text

        local_y_zoom_min <- input$var_plots__y_zoom_min
        local_y_zoom_max <- input$var_plots__y_zoom_max

        log_message_variable('', '')

        ggplot_object <- NULL

        classes <- class(local_dataset)
        if(length(classes) == 1 && classes == 'ts') {

            log_message('**ts**')

            ggplot_object <- local_dataset %>% autoplot()
            
        } else if (any(classes == 'mts') || any(classes == 'msts')) {
            
            ggplot_object <- NULL
            
        } else {
            
            stopifnot(FALSE)
        }

        return (ggplot_object)
    })
}

##############################################################################################################
# INPUT
##############################################################################################################
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
