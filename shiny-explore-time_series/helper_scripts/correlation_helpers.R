##############################################################################################################
# OUTPUT
##############################################################################################################
renderPlot__correlation__plot <- function(input, session, dataset) {

    renderPlot({

        withProgress(value=1/2, message='Calculating Correlations', {

            local_dataset <- as.data.frame(dataset()) %>% select_if(is.numeric)

            if(ncol(local_dataset) <= 1) {

                return (print('Not enough numeric columns.'))
            }


            if(input$correlation__pretty_text) {

                local_dataset <- rt_pretty_dataset(local_dataset)
            }

            log_message_block_start('Calculating Correlations & Creating Plot')
            log_message_variable('correlation__corr_threshold', input$correlation__corr_threshold)
            log_message_variable('correlation__p_value_threshold', input$correlation__p_value_threshold)
            log_message_variable('correlation__base_size', input$correlation__base_size)
            log_message_variable('correlation__pretty_text', input$correlation__pretty_text)
            log_message_variable('correlation__num_lags', input$correlation__num_lags)

            if(!is.na(input$correlation__num_lags) && 
                input$correlation__num_lags > 0) {

                local_dataset <- rt_ts_create_lagged_dataset(local_dataset,
                                                             num_lags=input$correlation__num_lags)
            }

            # see note about why I use print, in `variable plot` section below.
            return (
                print(rt_explore_plot_correlations(dataset=as.data.frame(local_dataset),
                                                   corr_threshold=input$correlation__corr_threshold,
                                                   p_value_threshold=input$correlation__p_value_threshold,
                                                   base_size=input$correlation__base_size,
                                                   type='pearson'))
            )
        })
    }, height = function() {

        if(rt_ts_is_single_variable(dataset())) {

            return (1)

        } else {

            return (session$clientData$output_correlation__plot_width * 0.66)
        }
    })
}
