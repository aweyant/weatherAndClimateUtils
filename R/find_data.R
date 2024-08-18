#' Check for the existence of files on the system
#'
#' @param ids vector of strings; synoptic station ids
#' @param dir string; the directory to search in
#' @param start,end strings; the the start and end times of the record. The
#' format is YYYYMMDDHHMM in Universal Coordinated Time \[UTC\]
#' @param format string; file format: csv or json
#'
#' @return a vector of strings with filepaths of the files on the system which
#' match the given description
#'
#' @details
#' If given format is "csv", then each station ID is iterated through.
#'
#' If the given format is "json", only json files which contain *all* station
#' IDs will come up in the search.
#'
#' @export
find_data_synoptic <- function(ids,
                               dir,
                               start,
                               end,
                               format) {
  if(format == "csv") {
    vapply(X = ids,
           FUN = \(id) {
             candidate_files = list.files(path = file.path(dir),
                                          pattern = "csv",
                                          full.names = TRUE)
             #print(candidate_files)
             ids_pattern = id
             candidate_files = candidate_files[which(x = grepl(pattern = ids_pattern,
                                                               x = candidate_files))]
             #print(candidate_files)
             indices_of_files_containing_time_window <- sapply(
               X = candidate_files,
               FUN = \(file) {
                 tryCatch(
                   expr = {
                     print(file)
                     #if(is.null())
                     df <- (load_data_synoptic(file))$obs[[1]] %>%
                       dplyr::summarize(
                         dplyr::across(.cols = "Date_Time",
                                       .fns = list(min = min, max = max)))
                     df$Date_Time_min <= lubridate::ymd_hm(start) & df$Date_Time_max >= lubridate::ymd_hm(end)
                   },
                   error = \(e) {
                     return(FALSE)
                   })
                 })
             #print(indices_of_files_containing_time_window)
             candidate_files <- candidate_files[indices_of_files_containing_time_window]
             #print(candidate_files)
             # TODO: ADD LINE TO CONSOLIDATE CSVs
             #print(candidate_files)
             file_name <- ifelse(length(candidate_files) > 0,
                                 candidate_files[1],
                                 NA_character_)
             file_name
           },
           FUN.VALUE = character(1))
  }
  else if(format == "json") {
    raw_file_name = file.path(dir,
                              paste0(Sys.Date(),"_",paste0(ids, collapse = "&"),
                                     ".",format))

    candidate_files = list.files(path = file.path(dir), pattern = "json")
    ids_pattern = paste0("(?=.*",ids,")", collapse = "")
    candidate_files = candidate_files[which(x = grepl(pattern = ids_pattern, x = candidate_files, perl = TRUE))]

    # TODO: ADD LINE TO CONSOLIDATE JSONs
    file_name <- ifelse(length(candidate_files) > 0,
                        file.path(dir, candidate_files[1]),
                        NA_character_)
    file_name
  }
}
