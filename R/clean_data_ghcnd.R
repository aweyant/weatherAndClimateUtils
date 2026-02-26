#' GHCND cleaning functions
#'
#' Files downloaded from NOAA's Global Historical Climatological Network (GHCN)
#' daily set are transformed into a more sensible format. Namely,
#' \enumerate{
#'   \item Units are converted; temperature from tenths of degrees celcius to
#'   celcius and precipitation from tenths of mm to mm
#'   \item The three/four attribute flags for temperature and precipitation
#'   are separated into columns. In the raw GHCN files, the flags of different
#'   types show up as a comma-separated list within a single column. The
#'   three/four flags consist of "measurement", "quality", and "source"
#'   information (with ``time of observation'' occasionally being included as
#'   a fourth element). These are briefly described in the \href{https://www.ncei.noaa.gov/data/global-historical-climatology-network-daily/doc/GHCND_documentation.pdf}{GHCNd Documentation}.
#'   \item column names are made lower-case and precipitation is called "precip"
#'   rather than "prcp"
#'   \item In the case that a day is completely missing from the record (the
#'   row corresponding to a day simply does not exist), a blank/NA record is
#'   added
#' }
#'
#' @param raw_ghcnd_df the pure, raw data.frame you get from reading a GHCNd
#' comma-separated file into R with a function such as read.csv() or
#' readr::read_csv()
#' @param select_SOD_cols [logical] Should only the data columns concerning
#' precip, tmin, and tmax which constitute a traditional "summary of the day"
#' (SOD)?
#' @param begin_date,end_date [POSIXct] (optional) prescribed beginning and end
#' dates for the record. The default is first and last date contained in
#' raw_ghcnd_df
#'
#' @details
#' \strong{Quality flags:}
#' The automated QA flagging procedures are described in \href{https://doi.org/10.1175/2010JAMC2375.1}{Durre et al. 2010}.
#'
#' One must be cautious about eliminating precipitation values based on outlier
#' checks or temperature values based on spatial inconsistency checks. All
#' decisions are application dependent.
#'
#' \strong{Measurement flags:}
#' There is one measurement flag which is a complete deal-breaker for
#' precipitation observations at a daily scale. ``A'' means that a single
#' value in fact represents a multi-day total.
#'
#' Flag ``L'' means that a temperature observation is probably offset from its
#' purported calendar day. This is a dealbreaker for spatial interpolation.
#'
#' @returns a data.frame with a GHCNd record in a more sensible format
#' @export
#'
#' @examples
#' \dontrun{
#' path <- file.path("","home","alexander","Downloads")
#' id <- "USW00093112" # North Island NAS
#' get_data_ghcnd(dest_dir = path,
#'                ids = id,
#'                check_existence = FALSE)
#' raw_df <- readr::read_csv(file.path(path, paste0(id,".csv")))
#'
#' clean_raw_ghcnd_to_standard(raw_df)
#'
#' clean_raw_ghcnd_to_standard(raw_df) %>% dplyr::filter(is.na(tmax))
#'
#' clean_raw_ghcnd_to_standard(raw_df, begin_date = lubridate::ymd("2000-01-01"), end_date = lubridate::ymd("2000-12-31")) %>% View()
#' }
clean_raw_ghcnd_to_standard <- function(raw_ghcnd_df, select_SOD_cols = TRUE, begin_date = NULL, end_date = NULL) {
  raw_ghcnd_df %>%
    {
      if(all(c("TMAX_ATTRIBUTES", "TMIN_ATTRIBUTES", "PRCP_ATTRIBUTES") %in%
             names(.))) {
        (. %>%
           dplyr::mutate(
             dplyr::across(
               .cols = tidyselect::all_of(c("TMAX_ATTRIBUTES",
                                            "TMIN_ATTRIBUTES",
                                            "PRCP_ATTRIBUTES")),
               .fns = \(x) {dplyr::if_else(is.na(x), ",,", x)}
             )
           ) %>%
           tidyr::separate_wider_delim(cols = tidyselect::all_of(c("TMAX_ATTRIBUTES",
                                                                   "TMIN_ATTRIBUTES",
                                                                   "PRCP_ATTRIBUTES")),
                                       delim = ",",
                                       names_sep = "_",
                                       too_few = "align_start",
                                       too_many = "drop") %>%
           dplyr::mutate(TMAX = dplyr::case_when(TMAX_ATTRIBUTES_1 %in% c("L") ~ NA,
                                                 TMAX_ATTRIBUTES_2 != "" ~ NA,
                                                 TRUE ~ TMAX),
                         TMIN = dplyr::case_when(TMIN_ATTRIBUTES_1 %in% c("L") ~ NA,
                                                 TMIN_ATTRIBUTES_2 != "" ~ NA,
                                                 TRUE ~ TMIN),
                         PRCP = dplyr::case_when(PRCP_ATTRIBUTES_1 %in% c("A") ~ NA,
                                                 PRCP_ATTRIBUTES_2 != "" ~ NA,
                                                 TRUE ~ PRCP))# %>%
         # dplyr::rename_with(
         #   .cols = tidyselect::any_of("PRCP_ATTRIBUTES_4"), .fn = \(str) {str;return("tobs_precip")})
        )(.)
      }
      else .
    } %>%
    dplyr::rename_with(.cols = tidyselect::everything(),
                       .fn = tolower) %>%
    dplyr::rename_with(.cols = tidyselect::any_of("prcp"), .fn = \(str){return("precip")}) %>%
    dplyr::mutate(dplyr::across(
      .cols = tidyselect::any_of(c("tmin", "tmax", "precip", "tave")),
      .fns = \(x) x/10)) %>%
    # Adds blank/NA rows for any days completely missing from the record
    {
      dplyr::left_join(
        tibble::tibble(
          date = seq.Date(from = ifelse(is.null(begin_date), min((.)$date), begin_date),
                          to = ifelse(is.null(end_date), max((.)$date), end_date),
                          by = '1 day')
        ),
        (.),
        by = "date"
      )
    } %>%
    {
      if(select_SOD_cols) {
        (.) %>%
          dplyr::select(
            tidyselect::any_of(c("date","station","latitude","longitude","elevation","name")),
            tidyselect::starts_with(c("precip", "tmin", "tmax"))
          )
        }
      else (.)
    }
}
