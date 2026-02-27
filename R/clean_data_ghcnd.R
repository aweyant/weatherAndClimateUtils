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
#' @param select_SOD_cols [logical] Should only the data columns pertaining to
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
#' Overall, the QC checks described in Durre et al. 2010 rarely flag valid
#' observations. However, the level of culling is left to the end user.
#'
#' @returns a data.frame with a GHCNd record in a more sensible format; NB, the
#' default behaviour is such that no data are removed by this function. The
#' single-character measurement and quality flags are expanded into short
#' descriptions for easier reading.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' path <- file.path("","home","alexander","Downloads")
#' id <- "USC00040983" # Anza Borrego
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
    # Add blank/NA rows for any days completely missing from the record
    {
      dplyr::left_join(
        tibble::tibble(
          DATE = seq.Date(from = ifelse(is.null(begin_date), min((.)$DATE), begin_date),
                          to = ifelse(is.null(end_date), max((.)$DATE), end_date),
                          by = '1 day')
        ),
        (.),
        by = "DATE"
      )
    } %>%
    # Convert units of tmin, tmax, and precip
    dplyr::mutate(
      dplyr::across(
        .cols = tidyselect::any_of(c("TMIN", "TMAX", "PRCP", "TAVE")),
        .fns = \(x) x/10)
      ) %>%
    dplyr::mutate(
      dplyr::across(
        .cols = tidyselect::any_of(
          c("TMAX_ATTRIBUTES","TMIN_ATTRIBUTES","PRCP_ATTRIBUTES")
        ),
        .fns = \(x) {dplyr::if_else(is.na(x), ",,", x)}
      )
    ) %>%
    # Split the single attribute column into three, potentially four types
    tidyr::separate_wider_delim(
      cols = tidyselect::any_of(c("TMAX_ATTRIBUTES","TMIN_ATTRIBUTES","PRCP_ATTRIBUTES")),
      delim = ",", names_sep = "_", too_few = "align_start", too_many = "drop") %>%
    # Name the attribute columns
    dplyr::rename_with(
      .cols = tidyselect::contains("ATTRIBUTES"),
      .fn = \(colname) {
        # PRCP_ATTRIBUTES_1 becomes "prcp_measurement_flag"
        # PRCP_ATTRIBUTES_4 becomes "prcp_tobs"
        stringr::str_remove_all(colname, "ATTRIBUTES_") %>%
          stringr::str_replace_all(
            pattern = c(
              `1` = "measurement_flag",
              `2` = "quality_flag",
              `3` = "source_flag",
              `4` = "tobs"
            )
          )
        }
      ) %>%
    # Add TOBS columns, if none exist
    {
      add_column_if_does_not_exist((.), colname = c("PRCP_tobs", "TMIN_tobs", "TMAX_tobs"), filler = NA_character_)
    } %>%
    # Rewrite the measurement and quality codes as proper notes
    dplyr::mutate(
      dplyr::across(
        .cols = tidyselect::contains("measurement_flag"),
        .fns = list(
          \(flg) {
            ghcn_measurement_dict$note[match(flg, ghcn_measurement_dict$flag)]
            }
          ),
         .names = "{.col}_expanded"
        )
      ) %>%
    dplyr::mutate(
      dplyr::across(
        .cols = tidyselect::contains("quality_flag"),
      .fns = list(
        expanded = function(flg) {
          ghcn_quality_dict$note[match(flg, ghcn_quality_dict$flag)]
        })
      )
    ) %>%
    # Make all column names lower case
    dplyr::rename_with(.cols = tidyselect::everything(), .fn = tolower) %>%
    # Rename "prcp" to "precip"
    dplyr::rename_with(.cols = tidyselect::contains("prcp"), .fn = \(str){gsub(x = str, pattern = "prcp", replacement = "precip")}) %>%
    # (OPTIONALLY) return only columns concerning the basic daily SOD (tmin,
    # tmax, and precip)
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
