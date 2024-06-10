#' Calculate Seasonal Cycles from daily weather data
#'
#' @param df a data.frame() of daily weather observations, with the first column
#' containing DATE or POSIXct objects and the remaining columns containing the
#' weather obs (e.g. maximum temperature, precipitation)
#' @param .cols a character vector listing variables for which we want to calculate
#' the seasonal cycle
#' @param method string of the particular method used to calculate seasonal cycles;
#' currently, only "harmonic_quantile_regression" is supported
#' @param ... extra arguments passed to the chosen method
#'
#' @return a data.frame() with the date, the chosen variables, and their annual
#' cycles
#' @export
seasonal_cycle <- function(df,
                           .cols,
                           method = "harmonic_quantile_regression",
                           ...){
  df %>%
    # rename first column to "date"
    dplyr::rename("date" = 1) %>%
    # reduce columns only to date and selected data columns
    dplyr::select(dplyr::all_of(c("date", .cols))) %>%
    # convert dates to day number in a 4-year leap year cycle
    dplyr::mutate(lycday = lycday(date)) %>%
    {
      if(method == "harmonic_quantile_regression") {
        harmonic_quantile_regression(., ...)
      }
      else if(method == "harmonic_ols_regression") {
        harmonic_ols_regression(., ...)
      }
    }
}


#' Harmonic Quantile Regression method for calculating seasonal cycles
#'
#' This function calculates quantiles conditioned on the time of year.
#'
#' For example, if you wanted to know the median of the daily maximum
#' temperature on 30 June, you could take the median of all observed maximum
#' temperatures on 30 June, or a narrow window of days about 30 June. This
#' approach does not make the best use of available data and the results are
#' needlessly messy. If, however, we accept that annual cycles are, by
#' definition, periodic functions, we can instead regress on the time of year.
#' We use the function quantreg::rq() for this purpose.
#'
#' This function should not be called directly. Instead, use seasonal_cycle()
#' from this package.
#'
#' @inheritParams seasonal_cycle
#' @param quantile quantiles for which we want a seasonal cycle; common ones
#' include 0.05, 0.5 (the median), and 0.95
#' @param n_harmonics how many cosine waves we wish to superimpose; two might be
#' too coarse and six might be too noisy
#'
#' @return a data.frame() with the original variable and its quantiles
#' @export
harmonic_quantile_regression <- function(df, quantile = 0.5, n_harmonics = 2) {
  df %>%
    append_harmonics(n_harmonics = n_harmonics) %>%
    tidyr::pivot_longer(cols = !c("date", "lycday") & !dplyr::starts_with("harmonic"),
                        names_to = "name",
                        values_to = "value") %>%
    dplyr::group_by(.data$name) %>%
    dplyr::group_map(function(data, group_info) {
      data.frame(data,
                 group_info$name,
                 harmonic_quantile_regression_core(data,
                                                   quantile,
                                                   group_info$name)) %>%
        dplyr::select(-c("group_info.name", "date", "value")) %>%
        dplyr::select(-lycday) %>%
        dplyr::select(-dplyr::starts_with("harmonic"))}) %>%
    dplyr::bind_cols() %>%
    dplyr::ungroup() %>%
    dplyr::mutate(date = df$date) %>%
    dplyr::select(date, dplyr::everything())
}

harmonic_ols_regression <- function(df, n_harmonics = 2) {
  df %>%
    append_harmonics(n_harmonics = n_harmonics) %>%
    tidyr::pivot_longer(cols = !c("date", "lycday") & !dplyr::starts_with("harmonic"),
                        names_to = "name",
                        values_to = "value") %>%
    dplyr::group_by(name) %>%
    dplyr::group_map(function(data, group_info) {
      data.frame(data,
                 group_info$name,
                 harmonic_ols_regression_core(data,
                                              group_info$name)) %>%
        dplyr::select(-c("group_info.name", "date", "value")) %>%
        dplyr::select(-lycday) %>%
        dplyr::select(-dplyr::starts_with("harmonic"))}) %>%
    dplyr::bind_cols() %>%
    dplyr::ungroup() %>%
    dplyr::mutate(date = df$date) %>%
    dplyr::select(date, dplyr::everything())
}


#' Core calculation function for harmonic_quantile_regression()
#'
#' @inheritParams harmonic_quantile_regression
#' @param variable_name name of variable for which we calcualte the seasonal cycle
#'
#'@return a data.frame() with values of a variable and its conditional quantiles
harmonic_quantile_regression_core <- function(df, quantile, variable_name) {
  quantreg::rq(data = df %>%
                 dplyr::rename(!!dplyr::quo_name(variable_name) := "value"),
               formula = stats::as.formula(paste0(variable_name,
                                                  " ~ ",
                                                  paste(names(dplyr::select(.data = df,
                                                                            dplyr::starts_with("harmonic"))),
                                                        collapse = " + "))),
               tau = quantile)$fitted.values %>%
    data.frame() %>%
    dplyr::rename_with(.fn = \(x) {
      gsub(pattern = "\\.",
           replacement = paste0(variable_name, "_", 100*quantile, "_ptile")[[1]],
           x = x)}) %>%
    dplyr::rename_with(.fn = \(x) {
      paste0(variable_name, "_",
             100 * quantile[as.numeric(gsub(pattern = "X",
                                            replacement = "",
                                            x = x))],
             "_ptile")}) %>%
    dplyr::mutate(df %>%
                    dplyr::select("value") %>%
                    dplyr::rename(!!dplyr::quo_name(variable_name) := "value")) %>%
    dplyr::select({{variable_name}}, dplyr::everything())
}

harmonic_ols_regression_core <- function(df, variable_name) {
  stats::lm(data = df %>%
              dplyr::rename(!!dplyr::quo_name(variable_name) := "value"),
            formula = stats::as.formula(paste0(variable_name,
                                               " ~ ",
                                               paste(names(dplyr::select(.data = df,
                                                                         dplyr::starts_with("harmonic"))),
                                                     collapse = " + "))))$fitted.values %>%
    data.frame() %>%
    dplyr::rename_with(.fn = \(x) {
      gsub(pattern = "\\.",
           replacement = paste0(variable_name, "_conditional_mean")[[1]],
           x = x)}) %>%
    # dplyr::rename_with(.fn = \(x) {
    #   gsub(pattern = "\\.",
    #        replacement = paste0(variable_name, "_", 100*quantile, "_ptile")[[1]],
    #        x = x)}) %>%
    # dplyr::rename_with(.fn = \(x) {
    #   paste0(variable_name, "_",
    #          100 * quantile[as.numeric(gsub(pattern = "X",
    #                                         replacement = "",
    #                                         x = x))],
    #          "_ptile")}) %>%
    dplyr::mutate(df %>%
                    dplyr::select("value") %>%
                    dplyr::rename(!!dplyr::quo_name(variable_name) := "value")) %>%
    dplyr::select({{variable_name}}, dplyr::everything())
}

append_harmonics <- function(df, n_harmonics) {
  df %>%
    dplyr::mutate(data.frame(sapply(X = paste0("harmonic_s", seq_len(n_harmonics)),
                                    FUN = function(name) {
                                      sin(lycday*
                                            2*pi*
                                            as.numeric(gsub("harmonic_s","",name))/annual_period_in_days
                                      )
                                    },
                                    simplify = FALSE, USE.NAMES = TRUE)),
                  data.frame(sapply(X = paste0("harmonic_c", seq_len(n_harmonics)),
                                    FUN = function(name) {
                                      cos(lycday*
                                            2*pi*
                                            as.numeric(gsub("harmonic_c","",name))/annual_period_in_days
                                      )
                                    },
                                    simplify = FALSE, USE.NAMES = TRUE)))
}

lycday <- function(date) {
  dplyr::if_else(lubridate::year(date) %% 4 == 0,
                 lubridate::yday(date),
                 lubridate::yday(date) +
                   vapply(X = date, FUN = function(d) {
                     sum(c(366, 365, 365)[seq_len(lubridate::year(d) %% 4)])
                   },
                   FUN.VALUE = numeric(1))
  )
}

