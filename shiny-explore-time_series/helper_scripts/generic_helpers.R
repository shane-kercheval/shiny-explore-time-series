dataset_or_null <- function(file) {
    # loads the file if it exists, otherwise returns NULL.    

    if(file.exists(file)) {

        return (read.csv(file, header=TRUE))

    } else {

        return (NULL)
    }
}

null_if_select_variable_optional <- function(value) {

    if(is.null(value) || value == select_variable_optional) {

        value <- NULL
    }

    return (value)
}

null_if_na <- function(value) {

    if(is.na(value)) {

        return (NULL)
    }

    return (value)
}

cv_mse <- function(cv_result) {

    return (colMeans(cv_result^2, na.rm = TRUE))
}

cv_rmse <- function(cv_result) {

    return (map_dbl(cv_mse(cv_result), ~ sqrt(.)))
}

cv_mae <- function(cv_result) {
    
    return (colSums(abs(cv_result), na.rm = TRUE) / nrow(cv_result))
}

capture_messages_warnings <- function(func) {
    
    messages <- list()
    withCallingHandlers(
        warning = function(cnd) {
            messages <<- append(messages, cnd$message)
            rlang::cnd_muffle(cnd)
        },
        message = function(cnd) {
            messages <<- append(messages, cnd$message)
            rlang::cnd_muffle(cnd)
        },
        func()
    )
    return (paste0(messages, collapse = '\n'))
}

create_date_slider <- function(dataset, inputId, control_label="Date Window") {

    s <- start(dataset)[1]
    e <- end(dataset)[1]

    return (
        sliderTextInput(inputId=inputId,
                        label=control_label,
                        choices=seq(s, e, 1),
                        selected=c(s, e),
                        grid=TRUE)
    )
}

convert_start_end_window <- function(dataset, start_end_window) {
    # this function is needed because `window()` gives a warning if the `start` parameter isn't different than
    # the dataset's actual start value
    s <- NULL
    e <- NULL

    log_message_variable('start_end_window', paste0(start_end_window, collapse=', '))

    # if the selected filter value is different than the actual start value, keep the value to filter on
    if(start_end_window[1] != start(dataset)[1]) {

        s <- c(start_end_window[1], 1)
    }
    # if the selected filter value is different than the actual end value, keep the value to filter on
    if(start_end_window[2] != end(dataset)[1]) {

        e <- c(start_end_window[2], frequency(dataset))
    }

    log_message_variable('Converted start_end_window', paste0(list(s, e), collapse=', '))    

    return (list(s, e))
}


convert_start_end_window_decimal <- function(dataset, start_end_window, forecast_horizon) {

    log_message_variable('start_end_window', paste0(start_end_window, collapse=', '))
    log_message_variable('forecast_horizon', forecast_horizon)

    # converts a period e.g. c(1991, 12) to 1991.9167 
    freq_to_dec <- function(period, freq) {

        return(period[1] + (period[2] - 1) / freq)
    }

    # this function is needed because `window()` gives a warning if the `start` parameter isn't different than
    # the dataset's actual start value
    s <- freq_to_dec(start(dataset), frequency(dataset))
    e <- freq_to_dec(end(dataset), frequency(dataset))

    log_message_variable('default start dec', s)
    log_message_variable('default end dec', e)


    if(!is.null(forecast_horizon)) {

        e <- e + (forecast_horizon / frequency(dataset))
    }

    # if the selected filter value is different than the actual start value, keep the value to filter on
    if(start_end_window[1] != start(dataset)[1]) {

        s <- freq_to_dec(c(start_end_window[1], 1), frequency(dataset))
    }
    # if the selected filter value is different than the actual end value, keep the value to filter on
    if(start_end_window[2] != end(dataset)[1]) {

        e <- freq_to_dec(c(start_end_window[2], frequency(dataset)), frequency(dataset))
    }

    log_message_variable('final start dec', s)
    log_message_variable('final end dec', e)


    log_message_variable('Converted start_end_window', paste0(c(s, e), collapse=', '))    

    return (c(s, e))
}
