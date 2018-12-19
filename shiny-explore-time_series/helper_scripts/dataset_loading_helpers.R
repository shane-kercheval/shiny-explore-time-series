##########################################################################################################
# MAIN DATASET
# initialize with small default dataset or upload from file, by user
##########################################################################################################
reactive__source_data__creator <- function(input, custom_triggers) {
    reactive({

        withProgress(value=1/2, message='Loading Data',{

            req(input$preloaded_dataset)

            custom_triggers$reload_source_data  # update based on changes to this reactiveValue

            # reactive data
            upload_file_path <- input$uploadFile$datapath
            local_preloaded_dataset <- input$preloaded_dataset

            log_message_block_start('Loading Dataset')
            log_message_variable('input$uploadFile$datapath', upload_file_path)
            log_message_variable('input$preloaded_dataset', local_preloaded_dataset)

            loaded_dataset <- NULL

            if(is.null(upload_file_path)) {
                
                if(local_preloaded_dataset == 'Credit') {

                    loaded_dataset <- dataset_or_null('example_datasets/credit.csv')

                } else if (local_preloaded_dataset == 'Anti-Diabetic Drug Sales in Australia from 1991 to 2008') {

                    loaded_dataset <- a10

                } else if (local_preloaded_dataset == 'Total Weekly Air Passenger #s flights (Ansett) Melbourne/Sydney, 1987–1992.') {

                    loaded_dataset <- melsyd

                } else if (local_preloaded_dataset == 'Half-hourly/Daily Electricity Demand for Victoria, Australia, in 2014') {

                    loaded_dataset <- elecdemand

                } else if (local_preloaded_dataset == 'Quarterly Visitor Nights for Various Regions of Australia') {

                    loaded_dataset <- visnights

                } else if (local_preloaded_dataset == 'Quarterly Australian Beer production') {

                    loaded_dataset <- ausbeer

                } else if (local_preloaded_dataset == 'Australian monthly electricity production: Jan 1956 – Aug 1995.') {

                    loaded_dataset <- elec

                } else if (local_preloaded_dataset == 'International Arrivals to Australia') {

                    loaded_dataset <- arrivals

                } else if (local_preloaded_dataset == 'Monthly Total # of Pigs Slaughtered in Victoria, Australia (Jan 1980 – Aug 1995)') {

                    loaded_dataset <- pigs

                } else if (local_preloaded_dataset == 'Daily Closing Stock Prices of Google Inc (All)') {

                    loaded_dataset <- goog

                } else if (local_preloaded_dataset == 'Daily Closing Stock Prices of Google Inc (200)') {

                    loaded_dataset <- goog200

                } else if (local_preloaded_dataset == 'Price of dozen eggs in US, 1900–1993, in constant dollars.') {

                    loaded_dataset <- eggs

                } else if (local_preloaded_dataset == '% Changes in Consumption/Income/Production?Savings/Unemployment Rates for the US, 1960 to 2016.') {

                    loaded_dataset <- uschange

                }

            } else {

                if(str_sub(upload_file_path, -4) == '.csv') {
                    
                    loaded_dataset <- read.csv(upload_file_path, header=TRUE)

                } else if(str_sub(upload_file_path, -4) == '.RDS') {
                
                    loaded_dataset <- readRDS(file=upload_file_path)

                } else {

                    showModal(
                        modalDialog(title = 'Unknown File Type',
                                    'Only `.csv` and `.RDS` files are supported at this time.'))
                }
            }
        })

        

        return (loaded_dataset)
    })
}

##############################################################################################################
# OUTPUT
##############################################################################################################
pretty_dataset <- function(dataset) {

    if(is_single_time_series(dataset)) {

        return (data.frame(date=time(dataset), value=as.matrix(dataset)))
        
    } else if (is_multi_time_series(dataset)) {
        
        return (cbind(data.frame(date = time(dataset)), as.data.frame(dataset)))
        
    } else {
        
        stopifnot(FALSE)
    }
}

renderDataTable__source_data__head <- function(dataset) {

    renderDataTable({

        return (pretty_dataset(dataset=dataset()))
    })
}

renderDataTable__source_data__info <- function(dataset) {

    renderText({

        s <- paste0(start(dataset()), collapse = ', ')
        e <- paste0(end(dataset()), collapse = ', ')
        f <- frequency(dataset())

        return (paste0('Start:\t\t', s, '\n', 'End:\t\t', e, '\n', 'Frequency:\t', f))
    })
}
