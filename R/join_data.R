#' Functions for consolidating observations from different data sources
#'
#' @param synoptic_df data.frame; observations from Synoptic Data PBC, as
#' outputted by weatherAndClimateUitls::load_data_synoptic()
#' @param synoptic_daily_df data.frame; observations from synoptic, summarized
#' to daily resolution by weatherAndClimateUtils::aggregate_data()
#' @param ghcnd_df data.frame; observations from GHCNd, as outputted by
#' weatherAndClimatePlots::clean_raw_ghcnd_to_standard()
#'
#' @return a data.frame() with GHCNd observations augmented with observations
#' from Synoptic. Any observations which exist in GHCNd take precidence, because
#' their maxima and minima are more trustworthy (since they are not as dependent)
#' on the temporal resolution of the full hi-res data as the synoptic obs, which
#' are sometimes hourly.
#' @export
merge_data <- function(synoptic_df = NULL, synoptic_daily_df = NULL, ghcnd_df = NULL) {
  if(!is.null(synoptic_daily_df) & !is.null(ghcnd_df)) {
    return(dplyr::rows_upsert(x = synoptic_daily_df, y = ghcnd_df,
                              by = "date") %>%
             dplyr::arrange(.data$date))
  }
  if(!is.null(synoptic_df) & !is.null(ghcnd_df)) {
    return(dplyr::rows_upsert(x = aggregate_data_synoptic(synoptic_df), y = ghcnd_df,
                              by = "date") %>%
             dplyr::arrange(.data$date))
  }
  if(is.null(synoptic_df) & is.null(synoptic_daily_df) & !is.null(ghcnd_df)) {
    return(ghcnd_df)
  }
  }
