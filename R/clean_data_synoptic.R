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
  if(readr::read_lines(file = raw_synoptic_file, n_max = 1) == "# NUMBER_OF_OBJECTS: 0") {
    warning(paste0(raw_synoptic_file, " contains no weather observations. Skipping processing this file. Check your station id and time interval."))
    return(NULL)
  }
  process_header_synoptic(raw_synoptic_file) %>%
    dplyr::left_join(readr::read_csv(raw_synoptic_file, skip = 8,
                                     col_names = names(readr::read_csv(raw_synoptic_file, comment = "#", n_max = 0))) %>%
                       dplyr::group_by(Station_ID) %>%
                       tidyr::nest(.key = "obs"),
                     by = "Station_ID")
}

#' @rdname clean_synoptic
#' @export
process_header_synoptic <- function(raw_synoptic_file) {
  readr::read_delim(raw_synoptic_file,
                    n_max = 6,
                    delim = ":", col_names = FALSE) %>%
    dplyr::mutate(dplyr::across(.fn = \(x) gsub(x = x, pattern = "#", replacement = ""))) %>%
    dplyr::mutate(dplyr::across(.fn = \(x) stringr::str_trim(string = x, side = "both"))) %>%
    t() %>%
    dplyr::as_tibble() %>%
    janitor::row_to_names(1) %>%
    dplyr:::mutate(dplyr::across(.cols = c("LATITUDE", "LONGITUDE", "ELEVATION [ft]"),
                                 .fns = as.numeric)) %>%
    dplyr::rename("Station_ID" = "STATION")
}

clean_data_synoptic <- function(raw_synoptic_df) {

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
