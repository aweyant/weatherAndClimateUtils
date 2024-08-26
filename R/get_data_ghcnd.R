get_data_ghcnd <- function(dest_dir, ids, check_existence = FALSE) {
  # URL for all GHCND files; should move to an internal package variable
  ghcnd_url_base <- "https://www.ncei.noaa.gov/data/global-historical-climatology-network-daily/access/"

  ghcnd_dl_wrapper <- function(id) {
    url <- paste0(ghcnd_url_base, id, ".csv")
    dest <- file.path(dest_dir, paste0(id, ".csv"))
    utils::download.file(url = url, destfile = dest)
  }
  safely_download_ghcnd <- purrr::safely(.f = \(id) {ghcnd_dl_wrapper(id)})
  purrr::map(.x = ids,
             .f = safely_download_ghcnd)
}
