#' Functions for temporally aggregating data or reshaping data into a temporally
#' aggregated form
#'
#' @param data_df; data.frame with weather observations from Synoptic Data PBC
#' or GHCN-Daily.
#' @param type; string specifying the data source; so far, "synoptic" and
#' "ghcnd" are supported
#' @param agg_period; string specifying the period to aggregate over; So far, "day",
#' "month", and "year" are supported.
#' @param agg_functions; list of functions for aggregating data
#' @return a data.frame() with observations aggregated to the chosen temporal
#' resolution with the chosen aggregation functions
#' @export
#'
#' @examples
#' \dontrun{
#' weatherAndClimatePlots::kska_asos_weather_obs %>%
#'  dplyr::mutate(Date_Time_Local = lubridate::with_tz(Date_Time, tzone = "America/Los_Angeles")) %>%
#'  aggregate_data(type = "synoptic", agg_period = "day") %>%
#'  dplyr::select(tidyselect::all_of(c("date","tmax", "tmin", "precip")))
#'
#'  weatherAndClimatePlots::san_diego_airport_daily_weather_obs %>%
#'  aggregate_data_ghcnd(agg_period = "wy",
#'  agg_vars = c("precip", "tmin", "tmax"))
#'  }
aggregate_data <- function(data_df, type = "synoptic", agg_period = "day",
                           agg_vars = NULL, agg_functions = default_agg_functions()) {
  if(type == "synoptic") {
    aggregate_data_synoptic(data_df = data_df, agg_period = agg_period,
                            agg_vars = agg_vars, agg_functions = agg_functions)
  }
  else if(type == "ghcnd") {
    aggregate_data_ghcnd(data_df = data_df, agg_period = agg_period,
                         agg_vars = agg_vars, agg_functions = agg_functions)
  }
}

#' Submethods for aggregate weather data
#'
#' @name aggregate_data_submethods
#' @inheritParams aggregate_data

#' @rdname aggregate_data_submethods
aggregate_data_subdaily <- function(data_df, agg_period = "day", agg_vars = NULL,
                                    agg_functions = default_agg_functions()) {
  data_df %>%
    # Add new time columns for grouping
    dplyr::mutate(Year_Local = lubridate::year(.data$Date_Time_Local),
                  Month_Local = lubridate::month(.data$Date_Time_Local),
                  Day_Local = lubridate::day(.data$Date_Time_Local),
                  Date_Local = lubridate::as_date(.data$Date_Time_Local)) %>%
    {
      switch(agg_period,
             day = ((.) %>% dplyr::group_by(.data$Date_Local)),
             month = ((.) %>%
                        dplyr::group_by(.data$Year_Local, .data$Month_Local)),
             year = ((.) %>% dplyr::group_by(.data$Year_Local))
      )
    } %>%
    dplyr::summarise(dplyr::across(tidyselect::any_of(agg_vars),
                                   .fns = agg_functions)) %>%
    dplyr::ungroup()
}

aggregate_data_ghcnd <- function(data_df, agg_period = "year", agg_vars = NULL,
                                 agg_functions = default_agg_functions()) {
  data_df %>%
    dplyr::rename("Date_Local" = "date") %>%
    # Add new time columns for grouping
    dplyr::mutate(Year_Local = lubridate::year(.data$Date_Local),
                  Month_Local = lubridate::month(.data$Date_Local),
                  WY_Local = tag_wy(.data$Year_Local, .data$Month_Local)) %>%
    {
      switch(agg_period,
             month = ((.) %>%
                        dplyr::group_by(.data$Year_Local, .data$Month_Local)),
             year = ((.) %>% dplyr::group_by(.data$Year_Local)),
             wy = ((.) %>% dplyr::group_by(.data$WY_Local))
      )
    } %>%
    tidyr::nest(.key = "daily_obs") %>%
    dplyr::mutate(
      summary_stats = purrr::map(
        .x = .data$daily_obs,
        .f = \(df) {
          df %>%
            dplyr::summarise(dplyr::across(tidyselect::any_of(agg_vars),
                                           .fns = agg_functions))}),
      precip_accum = purrr::map(.x = .data$daily_obs,
                         .f = \(df) {
                           df %>%
                             dplyr::select(.data$Date_Local, .data$precip) %>%
                             dplyr::mutate(
                               precip_accum = .data$precip %>%
                                 cumsum())
                         })) %>%
    dplyr::select(-c("daily_obs")) %>%
    tidyr::unnest("summary_stats") %>%
    dplyr::ungroup()
}


#' @rdname aggregate_data_submethods
aggregate_data_synoptic <- function(data_df, agg_period = "day",
                                    agg_vars = default_agg_vars_synoptic(),
                                    agg_functions = default_agg_functions()) {
  if(is.null(agg_vars)) {agg_vars <- default_agg_vars_synoptic()}
  #print(paste0("Inside aggregate_data_synotpic: ", agg_vars))
  aggregate_data_subdaily(data_df = data_df, agg_period = agg_period,
                          agg_vars = agg_vars, agg_functions = agg_functions) %>%
    dplyr::rename_with(
      .cols = tidyselect::everything(),
      .fn = synoptic_to_ghcnd_names)

}

#' Helper functions and data for aggregate_data()
#' @name aggregate_data_helpers

#' @rdname default_agg_functions
default_agg_functions <- function() {
  return_na_if_missing <- function(x) {
    if(length(stats::na.omit(x)) == 0) {
      call <- rlang::expr(return(NA))
      rlang::eval_bare(call, env = parent.frame())
    }
  }
  list(n_obs = \(x) {length(stats::na.omit(x))},
       max = \(x) {return_na_if_missing(x); max(x, na.rm = TRUE)},
       min = \(x) {return_na_if_missing(x); min(x, na.rm = TRUE)},
       total = \(x) {return_na_if_missing(x); sum(x, na.rm = TRUE)},
       mean = \(x) {return_na_if_missing(x); mean(x, na.rm = TRUE)},
       median = \(x) {return_na_if_missing(x); stats::median(x, na.rm = TRUE)})
}

#' @rdname default_agg_functions
default_agg_vars_synoptic <- function() {
  c("air_temp_set_1",
    "dew_point_temperature_set_1", "dew_point_temperature_set_1d",
    "solar_radiation_set_1",
    "precip_intervals_set_1d",
    "relative_humidity_set_1",
    "wind_speed_set_1", "wind_gust_set_1", "peak_wind_speed_set_1",
    "sea_level_pressure_set_1")
}

#' @name default_agg_functions
#' @param col_name string; column name which will be standardized
synoptic_to_ghcnd_names <- function(col_name) {
  dplyr::case_when(
    col_name == "Date_Local" ~ "date",
    col_name == "air_temp_set_1_max" ~ "tmax",
    col_name == "air_temp_set_1_n_obs" ~ "tmax_tmin_n_obs",
    col_name == "air_temp_set_1_min" ~ "tmin",
    col_name == "precip_intervals_set_1d_max_n_obs" ~ "precip_n_obs",
    col_name == "precip_intervals_set_1d_total" ~ "precip",
    TRUE ~ col_name)
}

#' Tag water year
#' @name tag_wy
#' @param year numeric; year
#' @param month numeric; month
#' @param wy_first_month numeric; the month which defines the beginning of the
#' water year. In the western United States, this is 10 (1 October).
tag_wy <- function (year, month, wy_first_month = 10)
{
  dplyr::if_else(month >= wy_first_month, year + 1, year)
}
