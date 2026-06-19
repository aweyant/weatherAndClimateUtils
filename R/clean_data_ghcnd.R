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
#' precip, tmin, tmax, and tobs (meaning temperature at the time of observation)
#' which constitute a traditional "summary of the day" (SOD)?
#' @param begin_date,end_date [POSIXct] (optional) prescribed beginning and end
#' dates for the record. The default is first and last date contained in
#' raw_ghcnd_df
#' @param expand_flag_codes [logical] Should the single-character flag codes be
#' expanded into proper descriptions? Set to FALSE, if you intend to keep small
#' file sizes, otherwise TRUE is recommended.
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
#' cleaned_df <- clean_raw_ghcnd_to_standard(raw_df)
#'
#' cleaned_df %>%
#' dplyr::select(date, tmin, tobs, tmax, tmax_quality_flag, ends_with("tobs")) %>%
#' dplyr::filter(!is.na(tobs)) %>%
#' dplyr::filter(tmax_quality_flag != "" |
#' dplyr::lead(tmax_quality_flag) != "" |
#' dplyr::lag(tmax_quality_flag) != "")
#'
#' clean_raw_ghcnd_to_standard(raw_df) %>% dplyr::filter(is.na(tmax))
#'
#' clean_raw_ghcnd_to_standard(raw_df, begin_date = lubridate::ymd("2000-01-01"), end_date = lubridate::ymd("2000-12-31")) %>% View()
#'
#' # Checking the flagging behavior
#' raw_df %>%
#'   dplyr::select(DATE, TMIN, TOBS, TMAX, TMAX_ATTTRIBUTES) %>%
#'   dplyr::filter(lubridate::month(DATE) == 3, lubridate::year(DATE)==2000)
#' # There is an example of a TMAX not being consistent with instantaneously
#' # observed temperature on the previous day.
#' }
clean_raw_ghcnd_to_standard <- function(raw_ghcnd_df, select_SOD_cols = TRUE, begin_date = NULL, end_date = NULL, expand_flag_codes = TRUE) {
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
        .cols = tidyselect::any_of(c("TMIN", "TMAX", "PRCP", "TOBS", "TAVE")),
        .fns = \(x) x/10)
      ) %>%
    dplyr::mutate(
      dplyr::across(
        .cols = tidyselect::any_of(
          c("TMAX_ATTRIBUTES","TMIN_ATTRIBUTES","PRCP_ATTRIBUTES", "TOBS_ATTRIBUTES")
        ),
        .fns = \(x) {dplyr::if_else(is.na(x), ",,", x)}
      )
    ) %>%
    # Split the single attribute column into three, potentially four types
    tidyr::separate_wider_delim(
      cols = tidyselect::any_of(c("TMAX_ATTRIBUTES","TMIN_ATTRIBUTES","PRCP_ATTRIBUTES", "TOBS_ATTRIBUTES")),
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
    # Add the column called "TOBS" (meaning temperature at time of observation)
    {
      add_column_if_does_not_exist(
        (.),
        colname = c("TOBS"), filler = NA_real_)
    } %>%
    # Add TOBS columns, if none exist
    {
      add_column_if_does_not_exist(
        (.),
        colname = c("PRCP_tobs", "TMIN_tobs", "TMAX_tobs", "TOBS_tobs"),
        filler = NA_character_)
    } %>%
    # (optionally) Rewrite the measurement and quality codes as proper notes
    {
      if(expand_flag_codes) {
        (.) %>%
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
                  }
                )
              )
            )
        }
      else (.)
      } %>%
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
            tidyselect::starts_with(c("precip", "tmin", "tmax", "tobs"))
          )
        }
      else (.)
    }

}


#' Convert "human standard" tidy summary of the day to the ".dly" format of NOAA
#'
#' Some programs such as NOAA's QA/QC routine expect data to be written in a
#' rather wide format. This format is described in a NOAA readme file (ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/readme.txt), which is
#' somewhat reproduced here.
#'
#' General format is as follows:
#' ------------------------------
#' Variable   Columns   Type
#' ------------------------------
#' ID            1-11   Character
#' YEAR         12-15   Integer
#' MONTH        16-17   Integer
#' ELEMENT      18-21   Character
#' VALUE1       22-26   Integer
#' MFLAG1       27-27   Character
#' QFLAG1       28-28   Character
#' SFLAG1       29-29   Character
#' VALUE2       30-34   Integer
#' MFLAG2       35-35   Character
#' QFLAG2       36-36   Character
#' SFLAG2       37-37   Character
#'   .           .          .
#'   .           .          .
#'   .           .          .
#' VALUE31    262-266   Integer
#' MFLAG31    267-267   Character
#' QFLAG31    268-268   Character
#' SFLAG31    269-269   Character
#'
#'
#' @param SOD_in_df [data.frame] data.frame with the summary of the day
#' @param ghcn_id [character] the GHCN ID for the station
#' @param elements_v [character] vector the variables to include. For now, this
#' is set to c("tmin", "tmax", "precip")
#' @param out_dir [character] the directory to write the file
#'
#' @returns a data.frame reminiscient of the format or nothing/NULL, if a out_dir
#' is provided
#'
#' @details
#' Now, it is assumed that the units of SOD_in_df are celcius for temperature and
#' mm for precipitation.
#'
#' The output is written in tenths of mm and tenths of degrees C (as integers)
#'
#' It is also assumed that at least some month has in the range has a 31st. This
#' will be true for any dataset more than a couple of months long.#'
#'
#' @export
#'
#' @examples
standard_summary_of_day_to_GHCNd_wide_format <- function(SOD_in_df, ghcn_id, out_dir, elements_v = c("tmin", "tmax", "precip")) {
  wide_df <- SOD_in_df %>%
    # Add blank/NA rows for any days completely missing from the record
    {
      dplyr::left_join(
        tibble::tibble(
          date = seq.Date(from = min((.)$date),
                          to = max((.)$date),
                          by = '1 day')
        ),
        (.),
        by = "date"
      )
    } %>%
    {
      purrr::map(
        .x = elements_v,
        .f = \(var) {
          if(!(var %in% colnames((.)))) {return(NULL)}
          (.) %>%
            dplyr::select(tidyselect::all_of(c('date', var))) %>%
            dplyr::mutate(
              "{var}" := round( .data[[var]] * 10 ),
              year = lubridate::year(date),
              month = lubridate::month(date),
              day = lubridate::day(date),
              .keep = "unused"
            ) %>%
            dplyr::rename('value' = var) %>%
            # Add dummy flags
            dplyr::mutate(
              mflag = " ",
              qflag = " ",
              sflag = " "
            ) %>%
            # Convert everything to characters
            dplyr::mutate(
              dplyr::across(
                .cols = tidyselect::everything(),
                .fns = as.character
              )
            ) %>%
            # Replace NA with "-9999"
            dplyr::mutate(
              value = dplyr::if_else(is.na(value), "-9999", value)
            ) %>%
            # Pad values and month with whitespace
            dplyr::mutate(
              value = stringr::str_pad(string = value, width = 5, side = "left", pad = " "),
              month = stringr::str_pad(string = month, width = 2, side = "left", pad = " ")
            ) %>%
            tidyr::pivot_wider(
              names_from = day,
              values_from = c(value, mflag, qflag, sflag),
              names_vary = 'slowest') %>%
            dplyr::mutate(id = ghcn_id, .before = 1) %>%
            dplyr::mutate(element = var, .after = 3)
          }
        ) %>%
        dplyr::bind_rows()
    } %>%
    # CHANGE NAMES OF ELEMENTS IN THE OUTPUT
    # convert 'precip' to 'prcp'
    dplyr::mutate(element = dplyr::if_else(element == "precip", "prcp", element)) %>%
    # Elements are in upper case
    dplyr::mutate(element = toupper(element))
  if(is.null(out_dir)) {return(wide_df)}
  else {
    # write_fwf(
    #   dt = wide_df,
    #   file = file.path(out_dir, paste0(ghcn_id,".dly")),
    #   justify = "r",
    #   replace_na = "-9999",
    #   width = c(11,4,2,4,rep(c(5,1,1,1),31))
    #   )
    readr::write_delim(
      x = wide_df,
      delim = "",
      na = " ",
      file = file.path(out_dir, paste0(ghcn_id,".dly")),
      col_names = FALSE
    )
    return(NULL)
    }

}

#' Convert NOAA's ".dly" fixed width file format to their '3-flag' style csv
#' format
#'
#' @param wide_fwf_in [character] the path to a NOAA ".dly" fixed width file
#' @param SOD_csv_out [character] the path to write a NOAA 3-flag style csv
#'
#' @returns a data.frame reminiscient of the format or nothing/NULL, if a SOD_csv_out
#' is provided.
#'
#' If a data.frame() is returned, this can be fed to clean_raw_ghcnd_to_standard()
#'
#' @details
#'
#' @export
#'
#' @examples
#' \dontrun{
#' path <- file.path(
#' "","home","alexander","data", "observations",
#' "ghcn-daily", "subsets", "socal-temperature-qc",
#' "qc1out")
#'
#' id <- "USC00040983"
#'
#' wide_fwf_in <- file.path(path, paste0(id, ".dly"))
#'
#' #SOD_csv_out
#'
#'NOAA_3flag_df <- GHCNd_wide_format_to_standard_summary_of_day(wide_fwf_in = wide_fwf_in)
#'
#'clean_raw_ghcnd_to_standard(NOAA_3flag_df) %>%
#'  dplyr::filter(date > lubridate::ymd("20260501"), date < lubridate::ymd("20260520"))
#' }
#'

GHCNd_wide_format_to_standard_summary_of_day <- function(wide_fwf_in, SOD_csv_out = NULL) {
  cols_v <- c("STATION", "YEAR", "MONTH", "ELEMENT",
              # Followed by values and flags...
              unlist(
                purrr::map(
                  .x = 1:31, .f = \(i) {
                    paste0(c("VALUE", "MFLAG", "QFLAG", "SFLAG"), i
                    )
                  }
                )
              )
  )
  widths_v <- c(11,4,2,4,rep(c(5,1,1,1),31))

  wide_df <- readr::read_fwf(
    file = wide_fwf_in,
    col_positions = readr::fwf_widths(widths = widths_v, col_names = cols_v),
    paste0(rep("c", length.out = length(cols_v)), collapse = "")
  )

  elements_tbl <- wide_df %>%
    dplyr::group_by(ELEMENT) %>%
    tidyr::nest(.key = "el_df") %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      el_df = purrr::pmap(
        .l = list(ELEMENT, el_df),
        .f = \(cur_el, df) {

          # Data values for the element
          values_df <- df %>%
            dplyr::select(YEAR, MONTH, starts_with("VALUE")) %>%
            tidyr::pivot_longer(
              cols = tidyselect::starts_with("VALUE"),
              names_to = "DAY",
              names_prefix = "VALUE"
            ) %>%
            dplyr::rename( "{cur_el}" := "value")

          # All flags for the element
          atts_df <- df %>%
            dplyr::select(YEAR, MONTH, tidyselect::contains("FLAG")) %>%
            tidyr::pivot_longer(
              cols = tidyselect::contains("FLAG"),
              names_to = "FLAG_DAY",
              names_repair = "minimal"
            ) %>%
            dplyr::mutate(
              DAY = stringr::str_sub(FLAG_DAY, start = 6),
              FLAG_TYPE = stringr::str_sub(FLAG_DAY, start = 1, end = 5),
              .keep = "unused"
            ) %>%
            dplyr::mutate(value = dplyr::if_else(is.na(value), " ", value)) %>%
            tidyr::pivot_wider(
              names_from = "FLAG_TYPE", values_from = "value"
            ) %>%
            dplyr::mutate(TOBS = " ") %>%
            dplyr::rowwise() %>%
            dplyr::mutate(
              "{cur_el}_ATTRIBUTES" := paste0(c(MFLAG, QFLAG, SFLAG, TOBS), collapse = ","),
              .keep = "unused"
            ) %>%
            dplyr::ungroup()

          # Return data.frame with all bogus dates eliminated (e.g 31st of September)
          suppressWarnings(
            {
              dplyr::left_join(values_df, atts_df, by = c("YEAR", "MONTH", "DAY")) %>%
                dplyr::mutate(DATE= lubridate::ymd(paste0(YEAR, MONTH, DAY), quiet = TRUE),
                              .keep = "unused",
                              .before = 1) %>%
                dplyr::filter(!is.na(DATE))
            }
          )
        }
      )
    )

  df_out <- purrr::reduce(.x = elements_tbl$el_df, .f = dplyr::left_join, by = "DATE")

  if(!is.null(SOD_csv_out)) {
    readr::write_csv(x = df_out, file = SOD_csv_out)
    return(NULL)
  }
  else{return(df_out)}
}



