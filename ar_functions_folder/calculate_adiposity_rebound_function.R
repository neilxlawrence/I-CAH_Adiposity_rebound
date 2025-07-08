#function to calculate adiposity rebound
find_first_peak_trough_by_id <- function(df, id_col, value_col, age_col) {
  df %>%
    group_by(.data[[id_col]]) %>%
    arrange(.data[[age_col]]) %>%  # Ensure data is ordered by age
    summarise(
      adiposity_peak_age = {
        peaks <- which((!!sym(value_col)) > dplyr::lag(!!sym(value_col)) & (!!sym(value_col)) > dplyr::lead(!!sym(value_col)))
        if (length(peaks) > 0) .data[[age_col]][peaks[1]] else NA
      },
      adiposity_peak_bmi = {
        peaks <- which((!!sym(value_col)) > dplyr::lag(!!sym(value_col)) & (!!sym(value_col)) > dplyr::lead(!!sym(value_col)))
        if (length(peaks) > 0) .data[[value_col]][peaks[1]] else NA
      },
      adiposity_rebound_age = {
        troughs <- which((!!sym(value_col)) < dplyr::lag(!!sym(value_col)) & (!!sym(value_col)) < dplyr::lead(!!sym(value_col)))
        if (length(troughs) > 0) .data[[age_col]][troughs[1]] else NA
      },
      adiposity_rebound_bmi = {
        troughs <- which((!!sym(value_col)) < dplyr::lag(!!sym(value_col)) & (!!sym(value_col)) < dplyr::lead(!!sym(value_col)))
        if (length(troughs) > 0) .data[[value_col]][troughs[1]] else NA
      },
      .groups = "drop"
    )
}

#have a version of this function to use just for fixed effects
find_first_peak_trough <- function(df, value_col, age_col) {
  df <- df %>%
    arrange(.data[[age_col]])  # Ensure data is ordered by age
  
  # Find first peak
  peaks <- which((df[[value_col]] > dplyr::lag(df[[value_col]]) & 
                    df[[value_col]] > dplyr::lead(df[[value_col]])))
  first_peak_age <- if (length(peaks) > 0) df[[age_col]][peaks[1]] else NA
  first_peak_bmi <- if (length(peaks) > 0) df[[value_col]][peaks[1]] else NA
  
  # Find first trough
  troughs <- which((df[[value_col]] < dplyr::lag(df[[value_col]]) & 
                      df[[value_col]] < dplyr::lead(df[[value_col]])))
  first_trough_age <- if (length(troughs) > 0) df[[age_col]][troughs[1]] else NA
  first_trough_bmi <- if (length(troughs) > 0) df[[value_col]][troughs[1]] else NA
  
  return(tibble(
    adiposity_peak_age = first_peak_age,
    adiposity_peak_bmi = first_peak_bmi,
    adiposity_rebound_age = first_trough_age,
    adiposity_rebound_bmi = first_trough_bmi
  ))
}

