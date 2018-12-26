should_log_message <- TRUE

select_variable <- "<Select>"
select_variable_optional <- "<Select (optional)>"

single_time_series_variable_name <- "Loaded Data"
built_in_ts_variables <- c("trend", "season")
theme_base_size <- 16



tooltip_transformation_not_backtransformed <- function(id_to_attach) {
    return (bsTooltip(id=id_to_attach,
                      title='Transformation is not back-transformed for forecasts.',
                      placement = 'bottom', trigger = 'hover'))
}

tooltip_zoom_not_filter <- function(id_to_attach) {
    return (bsTooltip(id=id_to_attach,
                      title='Adjusting min/max axes values does not filter out data.',
                      placement = 'bottom', trigger = 'hover'))
}

inline_control <- function(label_text, control, padding_top_px=10) {

    padding_top_string <- paste0('padding-top: ', padding_top_px,'px !important;')

    return (
        tags$div(class='inline_label_control',
            tags$div(class='inline_label',
                     style=padding_top_string,#paste0('width: ', label_width_string,' !important;'),
                label_text),
            control
        )
    )
}
