##############################################################################################################
# CATEGORIC SUMMARY - calculate the categoric summary; it is an expensive operation for large datasets
##############################################################################################################
reactive__categoric_summary__creator <- function(dataset) {

    reactive({

        withProgress(value=1/2, message='Calculating Categoric Summary',{

            log_message_block_start('Calculating Categoric Summary')
            categoric_dataset <- as.data.frame(dataset()) %>% select_if(Negate(is.numeric))
            if(ncol(categoric_dataset) == 0) {

                return (data.frame())

            } else {

                return (rt_explore_categoric_summary(categoric_dataset))
            }
        })
    })
}

##############################################################################################################
# INPUT
##############################################################################################################
renderPrint__categoric_summary__text <- function(dataset, categoric_summary) {

    renderPrint({
        
        # get R's summary of the categoric data
        if(nrow(categoric_summary()) == 0) {

            return (cat('No Categoric Columns'))

        } else {

            return (summary(dataset()[, as.character(categoric_summary()$feature)]))
        }
    })
}

##############################################################################################################
# OUTPUT
##############################################################################################################
renderDataTable__categoric_summary__table <- function(categoric_summary) {

    renderDataTable({
        
        return (categoric_summary())
    })
}
