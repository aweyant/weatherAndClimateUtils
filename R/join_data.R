#' Functions for consolidating observations from different data sources
#'
#' @param synoptic_df data.frame; observations from Synoptic Data PBC, as
#' outputted by weatherAndClimateUitls::load_data_synoptic()
#' @param ghcnd_df data.frame; observations from GHCNdm, as outputted by
#' weatherAndClimatePlots::clean_raw_ghcnd_to_standard()
#'
#' @return a data.frame() with GHCNd observations augmented with observations
#' from Synoptic. Any observations which exist in GHCNd take precidence, because
#' their maxima and minima are more trustworthy (since they are not as dependent)
#' on the temporal resolution of the full hi-res data as the synoptic obs, which
#' are sometimes hourly.
#' @export
merge_data <- function(synoptic_df, ghcnd_df) {
  dplyr::rows_upsert(x = aggregate_data_synoptic(synoptic_df),
                     y = ghcnd_df,
                     by = "date") %>%
    dplyr::arrange(.data$date)
}
