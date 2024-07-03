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
#' LONGITUDE, ELEVATION [ft], STATE, and obs
#' @export
#'
#' @examples
#' today <- Sys.Date()
#' get_data_synoptic(ids = "KSAN",
#' dest_dir = file.path('~', "Downloads"),
#' recent = 120)
#' KSAN_tbl <- load_data_synoptic(file.path("~", "Downloads", paste0(today, "_KSAN.csv")))
load_data_synoptic <- function(raw_synoptic_file) {
  # Check to make sure the file contains weather data
  if(readr::read_lines(file = raw_synoptic_file, n_max = 1) == "# NUMBER_OF_OBJECTS: 0") {
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
    dplyr::left_join(readr::read_csv(raw_synoptic_file, skip = 8 + offset,
                                     col_names = names(readr::read_csv(raw_synoptic_file,
                                                                       comment = "#",
                                                                       n_max = 0))),
                     by = "Station_ID") %>%
    dplyr::group_by(across(all_of(c("Station_ID", "STATION NAME",
                                    "LATITUDE", "LONGITUDE", "ELEVATION [ft]",
                                    "STATE", "local_timezone")))) %>%
    tidyr::nest(.key = "obs") %>%
    dplyr::mutate(obs = purrr::pmap(.l = list(.data$obs, .data$local_timezone),
                                    .f = add_local_time))
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
                    delim = ":", col_names = FALSE) %>%
    dplyr::mutate(dplyr::across(.cols = everything(),
                                .fn = \(x) gsub(x = x, pattern = "#", replacement = ""))) %>%
    dplyr::mutate(dplyr::across(.cols = everything(),
                                .fn = \(x) stringr::str_trim(string = x, side = "both"))) %>%
    t() %>%
    dplyr::as_tibble() %>%
    janitor::row_to_names(1) %>%
    dplyr:::mutate(dplyr::across(.cols = c("LATITUDE", "LONGITUDE", "ELEVATION [ft]"),
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
