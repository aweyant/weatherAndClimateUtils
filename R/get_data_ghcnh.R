#' Download GHCN-hourly data
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
#' get_data_ghcnh(dest_dir = file.path("", "home", "alexander", "Downloads"), ids = "USC00041758")
#' # Confirm that the file is present on your system
#' file.exists(file.path("", "home", "alexander", "Downloads", paste0("USC00041758", ".psv")))
#' # Try to download again; If we check for its existence, it will not be re-downloaded
#' get_data_ghcnh(dest_dir = file.path("", "home", "alexander", "Downloads"), ids = "USC00041758", check_existence = TRUE)
#'}
get_data_ghcnh <- function(dest_dir, ids, check_existence = FALSE) {
  # URL for all GHCNH files; should move to an internal package variable
  ghcnh_url_base <- "https://www.ncei.noaa.gov/oa/global-historical-climatology-network/hourly/access/by-station/"

  ghcnh_dl_wrapper <- function(id, check_existence) {
    url <- paste0(ghcnh_url_base, "GHCNh_", id, "_por", ".psv")
    dest <- file.path(dest_dir, paste0(id, ".psv"))
    if(check_existence && file.exists(dest)) {return(NULL)}
    utils::download.file(url = url, destfile = dest, quiet = "TRUE")
  }

  safely_download_ghcnh <- purrr::safely(.f = \(id, check) {ghcnh_dl_wrapper(id, check)})

  purrr::walk(
    .x = ids,
    .f = \(id) {safely_download_ghcnh(id, check_existence)}
  )
}
