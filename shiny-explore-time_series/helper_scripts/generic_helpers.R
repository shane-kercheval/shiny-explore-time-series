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

easy_regression <- function(dataset,
                            dependent_variable,
                            independent_variables,
                            interaction_variables=NULL,
                            polynomial=NULL) {

    if(is.null(interaction_variables)) {
        
        interaction_variables_formula <- ''
    
    } else {

        interaction_variables_formula <- paste(map_chr(interaction_variables, ~ paste(., collapse =' * ')),
                                                   collapse = ' + ')
    }

    if(is.null(independent_variables) || length(independent_variables) == 0) {

        independent_variables_formula <- interaction_variables_formula

    } else if(is.null(interaction_variables) || length(interaction_variables) == 0) {

        independent_variables_formula <- paste(independent_variables, collapse =' + ')

    } else {
        
        independent_variables_formula <- paste(interaction_variables_formula,
                                               '+',
                                               paste(independent_variables, collapse =' + '))
    }

    formula <- paste(dependent_variable, '~', independent_variables_formula)
    
    if(is.numeric(dataset[, dependent_variable])) {

        type <- 'Linear Regression'
        result <- lm(formula, data=dataset, na.action = na.exclude)
        reference <- NULL
        
    } else {
        
        if(length(unique(dataset[, dependent_variable])) == 2) {
         
            type <- 'Logistic Regression'
            result <- glm(formula, data=dataset, na.action = na.exclude, family=binomial)
            reference <- rownames(contrasts(dataset[, dependent_variable]))[2]

        } else {
            
            return (NULL)
        }
    }

    return (
        list(rows_excluded=which(!complete.cases(dataset[, independent_variables])),
             type=type,
             formula=formula,
             results=result,
             reference=reference)
    )
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
