#' Process raw synoptic .csv files into standard tibbles
#'
#' @name clean_synoptic
NULL

#' @rdname clean_synoptic
#' @param raw_synoptic_file string; the path to a raw synoptic comma-delimited
#' file, as obtained by get_data_synoptic(); The first 6 lines of such a file
#' contain identifying station metadata, followed by the names of observed
#' variables, their units, and, finally, the observations themselves
#'
#' @return a nested tibble with columns Station_ID, Station Name, LATITUDE,
#' LONGITUDE, ELEVATION \[ft\], STATE, and obs
#' @export
#'
#' @examples
#' #today <- Sys.Date()
#' #get_data_synoptic(ids = "KSAN",
#' #dest_dir = file.path('~', "Downloads"),
#' #recent = 120)
#' #KSAN_tbl <- load_data_synoptic(file.path("~", "Downloads", paste0(today, "_KSAN.csv")))
load_data_synoptic <- function(raw_synoptic_file) {
  if(grepl(pattern = "json|JSON", x = raw_synoptic_file)) {
    # if(grepl(x = readr::read_lines(file = raw_synoptic_file, n_max = 1),
    #          "# NUMBER_OF_OBJECTS: 0|\"NUMBER_OF_OBJECTS\":0|NUMBER_OF_OBJECTS")) {
    #   warning(paste0(raw_synoptic_file,
    #                  " contains no weather observations. Skipping processing this file. Check your station id and time interval."))
    #   return(NULL)
    # }

    tidyjson::read_json(raw_synoptic_file) %>%
      tidyr::unnest_wider("..JSON") %>%
      {
        if(!("STATION" %in% colnames(.))) {
          warning(paste0(raw_synoptic_file,
                         " contains no weather observations. Skipping processing this file. Check your station id and time interval."))
          return(NULL)
        }
        else .
      } %>%
      tidyr::unnest_longer("STATION") %>%
      dplyr::select(-c("document.id")) %>%
      tidyr::hoist("STATION",
                   "Station_ID" = "STID",
                   "STATION NAME" = "NAME",
                   "LATITUDE" = "LATITUDE",
                   "LONGITUDE" = "LONGITUDE",
                   "ELEVATION [ft]" = "ELEVATION",
                   "PERIOD_OF_RECORD" = "PERIOD_OF_RECORD",
                   "local_timezone" = "TIMEZONE",
                   "obs" = "OBSERVATIONS")  %>%
      dplyr::mutate(obs = purrr::map(.x = .data$obs,
                                     .f = \(obs) {
                                       tibble::as_tibble(obs) %>%
                                         tidyr::unnest(cols = tidyselect::everything()) %>%
                                         dplyr::rename("Date_Time" = "date_time") %>%
                                         dplyr::mutate(Date_Time = lubridate::as_datetime(.data$Date_Time))}),
                    dplyr::across(c("LATITUDE", "LONGITUDE", "ELEVATION [ft]"), as.numeric)) %>%
      dplyr::mutate(obs = purrr::pmap(.l = list(.data$obs, .data$local_timezone),
                                      .f = add_local_time)) %>%
      dplyr::mutate(obs = purrr::map(.x = .data$obs,
                                     .f = \(obs) {
                                       obs %>%
                                         dplyr::select(-tidyselect::matches("cloud_layer_1_set_1d")) %>%
                                         unique()}))
  }
  else if (grepl(pattern = "csv|CSV", x = raw_synoptic_file)) {
    if(grepl(x = readr::read_lines(file = raw_synoptic_file, n_max = 1),
             "# NUMBER_OF_OBJECTS: 0|\"NUMBER_OF_OBJECTS\":0|NUMBER_OF_OBJECTS")) {
      warning(paste0(raw_synoptic_file,
                     " contains no weather observations. Skipping processing this file. Check your station id and time interval."))
      return(NULL)
    }
    # Account for the possibility that weather data were manually downloaded
    offset <- 0
    if(readr::read_lines(file = raw_synoptic_file,
                         n_max = 1,
                         lazy = TRUE) == "# The provisional data available here are intended for diverse user applications.") {
      offset <- 4
    }

    process_header_synoptic(raw_synoptic_file, offset) %>%
      dplyr::left_join(
        readr::read_csv(raw_synoptic_file, skip = 8 + offset,
                        col_names = names(readr::read_csv(raw_synoptic_file,
                                                          comment = "#",
                                                          n_max = 0,
                                                          show_col_types = FALSE)),
                        show_col_types = FALSE),
        by = "Station_ID") %>%
      # Make sure Date_Time is not a character.
      #dplyr::mutate(Date_Time = lubridate::ymd_hm(Date_Time)) %>%
      # Make sure pressure variables are numeric
      dplyr::mutate(dplyr::across(tidyselect::contains("pressure_set"),
                                  .fns = as.numeric)) %>%
      dplyr::group_by(dplyr::across(
        tidyselect::all_of(c("Station_ID", "STATION NAME",
                             "LATITUDE", "LONGITUDE", "ELEVATION [ft]",
                             "STATE", "local_timezone")))) %>%
      tidyr::nest(.key = "obs") %>%
      dplyr::mutate(obs = purrr::pmap(.l = list(.data$obs, .data$local_timezone),
                                      .f = add_local_time))
  }
  else {
    # ERROR
  }
}

#' @rdname clean_synoptic
#' @param offset the number of rows to skip in a file before expecting header
#' information. If data are downloaded with the API, this is 0. If the data were
#' downloaded manually from the web interface, this is 4 due to an addition
#' message appended to each file.
#' @export
process_header_synoptic <- function(raw_synoptic_file, offset = 0) {
  readr::read_delim(raw_synoptic_file,
                    n_max = 6,
                    skip = offset,
                    delim = ":",
                    col_names = FALSE,
                    show_col_types = FALSE) %>%
    dplyr::mutate(dplyr::across(.cols = tidyselect::everything(),
                                .fn = \(x) gsub(x = x, pattern = "#", replacement = ""))) %>%
    dplyr::mutate(dplyr::across(.cols = tidyselect::everything(),
                                .fn = \(x) stringr::str_trim(string = x, side = "both"))) %>%
    t() %>%
    dplyr::as_tibble() %>%
    janitor::row_to_names(1) %>%
    dplyr::mutate(dplyr::across(.cols = c("LATITUDE", "LONGITUDE", "ELEVATION [ft]"),
                                 .fns = as.numeric)) %>%
    dplyr::mutate(local_timezone = lutz::tz_lookup_coords(lat = .data$LATITUDE,
                                                          lon = .data$LONGITUDE,
                                                          method = "fast",
                                                          warn = FALSE)) %>%
    dplyr::rename("Station_ID" = "STATION")
}

add_local_time <- function(raw_synoptic_obs_tbl,
                           local_tzone) {
  raw_synoptic_obs_tbl %>%
    dplyr::mutate(Date_Time_Local = lubridate::with_tz(time = .data$Date_Time,
                                                       tzone = local_tzone))
}





#load_data_synoptic("~/Downloads/2024-06-07_KSAN")
#process_header_synoptic("~/Downloads/2024-06-07_KSAN")
# readr::read_delim("~/Downloads/2024-06-07_KSAN",
#                   n_max = 6,
#                   delim = ":", col_names = FALSE) %>%
#   dplyr::mutate(dplyr::across(.fn = \(x) gsub(x = x, pattern = "#", replacement = ""))) %>%
#   dplyr::mutate(dplyr::across(.fn = \(x) stringr::str_trim(string = x, side = "both"))) %>%
#   t() %>%
#   dplyr::as_tibble() %>%
#   janitor::row_to_names(1) %>%
#   dplyr:::mutate(dplyr::across(.cols = c("LATITUDE", "LONGITUDE", "ELEVATION [ft]"),
#                                .fns = as.numeric))
