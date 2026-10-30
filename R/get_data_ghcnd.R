#' Download GHCN-daily data
#'
#' @param dest_dir [character] the destination directory for downloaded files
#' @param ids [character vector] the GHCN IDs of sites
#' @param check_existence [logical] should files of sites already present in dest_dir be skipped?
#'
#' @returns NULL; function is called for its side effect, which is downloading files
#' @export
#'
#' @examples
#' \dontrun{
#' # Download the daily record from Chula Vista, CA
#' get_data_ghcnd(dest_dir = file.path("", "home", "alexander", "Downloads"), ids = "USC00041758")
#' # Confirm that the file is present on your system
#' file.exists(file.path("", "home", "alexander", "Downloads", paste0("USC00041758", ".csv")))
#' # Try to download again; If we check for its existence, it will not be re-downloaded
#' get_data_ghcnd(dest_dir = file.path("", "home", "alexander", "Downloads"), ids = "USC00041758", check_existence = TRUE)
#' }
get_data_ghcnd <- function(dest_dir, ids, check_existence = FALSE) {
  # URL for all GHCND files; should move to an internal package variable
  ghcnd_url_base <- "https://www.ncei.noaa.gov/data/global-historical-climatology-network-daily/access/"

  ghcnd_dl_wrapper <- function(id, check_existence) {
    url <- paste0(ghcnd_url_base, id, ".csv")
    dest <- file.path(dest_dir, paste0(id, ".csv"))
    if(check_existence && file.exists(dest)) {return(NULL)}
    utils::download.file(url = url, destfile = dest, quiet = "TRUE")
  }

  safely_download_ghcnd <- purrr::safely(.f = \(id, check) {ghcnd_dl_wrapper(id, check)})

  purrr::walk(
    .x = ids,
    .f = \(id) {safely_download_ghcnd(id, check_existence)}
    )
}
