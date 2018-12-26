dataset_or_null <- function(file) {
    # loads the file if it exists, otherwise returns NULL.    

    if(file.exists(file)) {

        return (read.csv(file, header=TRUE))

    } else {

        return (NULL)
    }
}

is_single_time_series <- function(x) {

    classes <- class(x)
    return (length(classes) == 1 && classes == 'ts')
}

is_multi_time_series <- function(x) {

    classes <- class(x)
    return (any(classes == 'mts') || any(classes == 'msts'))
}

null_if_select_variable_optional <- function(value) {

    if(is.null(value) || value == select_variable_optional) {

        value <- NULL
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
