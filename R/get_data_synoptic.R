#' Download station observations from Synoptic Data PBC
#'
#' With a basic, free account, anybody can download up to a year of station
#' observations from Synoptic.
#'
#' @name synoptic
NULL

#' @rdname synoptic
#' @param dest_dir string, the directory to download to
#' @param api_key string, API Token obtained from Synoptic Data PBC. A free
#' account is required. See https://docs.synopticdata.com/account/public-api-tokens
#' for more information
#' @param ids vector of strings, the IDs of the stations from which you want to
#' download observations
#' @param start,end strings, the boundaries of the time interval over which you
#' wish to fetch observations formatted as "yyyymmddhhmm". The timezone is
#' universal coordinated time (UTC), so be sure to consider offset from your
#' local time
#' @param recent numeric, integer specifically; Instead of providing an interval,
#' you can specify the "recent", the number of minutes you wish to count back from
#' the present moments. Set recent=120 to download the most recent two hours of
#' obs.
#' @param check_existence logical; should there be an effort to check the
#' dest_dir to see if the data you want have already been downloaded? This harms
#' performance, but is less wasteful of your Synoptic account privileges.
#'
#' @return NULL; the function is called for its side-effect
#'
#' @examples
#' #get_data_synoptic(dest_dir = "~/Downloads", ids = c("KSAN", "G3667"),
#' #start = "202308080000", end = "202308090000")
#'
#' @export
get_data_synoptic <- function(dest_dir,
                              api_key = NULL,
                              ids, # stid; vector of strings for station IDs
                              #states, # state; vector of strings with 2 digit state abbrvs
                              #countries, # country;
                              #nwszones, #nwszone;
                              #nwsfirezones, #nwsfirezone;
                              #cwas, # cwa
                              #counties, # county
                              #required_vars, # vars
                              #vars_operator = "or", # varsoperator
                              #networks = NULL, #network
                              #cental_coords, #lat,lon
                              #central_station_id, radius_from_central_point,
                              #bbox,
                              start = NULL,
                              end = NULL,
                              recent = NULL,
                              format = "csv",
                              check_existence = FALSE) {
  # Look for saved API key if none is given
  if(is.null(api_key)) {
    get_api_key_synoptic()
    api_key <- api_key_env$api_key
  }

  # Download new files if they do not already exist
  if(format == "csv") {
    if(check_existence) {
      # Check if files already exist
      # Some files may exist for the given time period; the ids vector is winnowed
      # down accordingly.
      if(is.null(recent)) {
        ids <- ids[which(is.na(find_data_synoptic(ids = ids,
                                                  dir = dest_dir,
                                                  start = start, end = end,
                                                  format =  format)))]
      }
    }
    for(id in ids) {
      api_call <- build_synoptic_api_call(api_key = api_key,
                                          ids = id,
                                          start = start,
                                          end = end,
                                          recent = recent,
                                          format = format)
      file_name = file.path(dest_dir,
                            paste0(Sys.Date(),"_",id,".csv"))
      utils::download.file(url = api_call,
                           destfile = file_name)
      if(readr::read_lines(file = file_name, n_max = 1) == "# NUMBER_OF_OBJECTS: 0") {
        warning(paste0("Synoptic could note download data for station ", id))
      }
    }
  }
  else if(format == "json") {
    if(check_existence) {
      if(is.null(recent)) {
        if(!is.na(find_data_synoptic(ids = ids,
                                     dir = dest_dir,
                                     start = start, end = end,
                                     format =  format)))
          return(NULL)
      }
    }
    api_call <- build_synoptic_api_call(api_key = api_key,
                                        ids = ids,
                                        start = start,
                                        end = end,
                                        recent = recent,
                                        format = format)
    file_name = file.path(dest_dir,
                          paste0(Sys.Date(),"_",paste0(ids, collapse = "&"),".",format))
    #print(api_call)
    utils::download.file(url = api_call,
                         destfile = file_name)
  }
  else {
    warning("Only \"csv\" and \"json\" are recognized formats")
  }
}

#' @rdname synoptic
#' @export
build_synoptic_api_call <- function(api_key = NULL,
                                    #station_selection_str = NULL,
                                    ids, # stid; vector of strings for station IDs
                                    #states, # state; vector of strings with 2 digit state abbrvs
                                    #countries, # country;
                                    #nwszones, #nwszone;
                                    #nwsfirezones, #nwsfirezone;
                                    #cwas, # cwa
                                    #counties, # county
                                    #required_vars, # vars
                                    #vars_operator = "or", # varsoperator
                                    #networks = NULL, #network
                                    #cental_coords, #lat,lon
                                    #central_station_id, radius_from_central_point,
                                    #bbox,
                                    start = NULL,
                                    end = NULL,
                                    recent = NULL,
                                    format = "csv") {
  basic_timeseries_stub_str = "https://api.synopticdata.com/v2/stations/timeseries?"
  api_key_str = paste0("token=", api_key)
  format_str=paste0("output=",format)
  time_str=ifelse(!is.null(recent),
                  paste0("recent=", recent),
                  paste0("start=", start, "&end=", end))
  station_selection_str=paste0("stid=", paste0(ids, collapse = ","))
  return(paste0(c(basic_timeseries_stub_str,
                  api_key_str,
                  station_selection_str,
                  time_str,
                  "precip=1",
                  format_str), collapse = "&"))
}

api_key_env <- new.env(parent = emptyenv())

#' @rdname synoptic
#' @export
get_api_key_synoptic <- function() {
  user_data_dir <- rappdirs::user_data_dir(appname = "weatherAndClimateUtils")
  if(!is.null(api_key_env$api_key) && exists(api_key_env$api_key)) {
    return(api_key_env$api_key)
  }
  else if(file.exists(file.path(user_data_dir, "api_key.Rds"))){
    api_key_env$api_key <- readRDS(file.path(user_data_dir, "api_key.Rds"))
    return(api_key_env$api_key)
  }
  else{
    print("You must set your Synoptic API token with set_api_key_synoptic().")
    return(NULL)
  }}

#' @rdname synoptic
#' @param given_api_key string of the API token associated with your
#' synoptic account
#' @param force logical; if TRUE, the user consents to writing the key to a file
#' which persists between R sessions; if FALSE, the user is asked for consent to
#' write.
#' @export
set_api_key_synoptic <- function(given_api_key, force = FALSE) {
  user_data_dir <- rappdirs::user_data_dir(appname = "weatherAndClimateUtils")
  api_key_env$api_useekey <- given_api_key
  if(!force && interactive()){
    result <- utils::select.list(c("Yes", "No"),
                                 title = "API token set. Would you like this to persist between sessions?")
    if(result == "Yes"){
      if(!dir.exists(user_data_dir)) {
        print(paste0("Creating ", user_data_dir))
        dir.create(file.path(user_data_dir))
      }
      saveRDS(object = api_key_env$api_key,
              file = file.path(user_data_dir,"api_key.Rds"))
    }
  } else if(force){
    if(!dir.exists(user_data_dir)) {
      print(paste0("Creating ", user_data_dir))
      dir.create(file.path(user_data_dir))
    }
    saveRDS(object = api_key_env$api_key,
            file = file.path(user_data_dir,"api_key.Rds"))
  } else {
    warning("The API token will be forgotten when this R session is terminated.")
  }
}

#' @rdname synoptic
#' @param dest_file_processed_data string; the desired name of the standardized
#' synoptic tibble, including its file path
#' @param dest_dir_raw_data same as dest_dir in get_data_synoptic
#' @param format string; file format for downloaded data. The valid options are
#' "json" and "csv".
#'
#' @examples
#' get_clean_and_save_data_synoptic(dest_dir_raw_data = file.path("~", "Downloads"),
#' dest_file_processed_data = file.path("~","data","weather_today.Rda"),
#' ids = c("KSAN", "G3667", "G4747"),
#' recent = 120)
#' @export
get_clean_and_save_data_synoptic <- function(dest_dir_raw_data,
                                             dest_file_processed_data = NULL,
                                             api_key = NULL,
                                             ids,
                                             start = NULL,
                                             end = NULL,
                                             recent = NULL,
                                             format = "json",
                                             check_existence = FALSE) {

  # Download synoptic data or do nothing, if specified files already exist
  get_data_synoptic(dest_dir = dest_dir_raw_data,
                    api_key = api_key,
                    ids = ids,
                    start = start,
                    end = end,
                    recent = recent,
                    format = format,
                    check_existence = check_existence)

  if(format == "csv") {
    # NEED TO UPDATE THIS SO THAT FILE NAMES CAN BE FOUND
    raw_file_names = file.path(dest_dir_raw_data,
                               paste0(Sys.Date(),"_",ids,".csv"))

    lapply(X = raw_file_names,
           FUN = function(raw_synoptic_file) {
             load_data_synoptic(raw_synoptic_file)
           }) %>%
      dplyr::bind_rows() %>%
      {if(!is.null(dest_file_processed_data)) {
        readr::write_rds(x = .,
                         file = dest_file_processed_data)}
        else {
          .
        }}
  }
  else if(format == "json") {
    # [ MUST DETERMINE NAME OF RAW FILE]
    raw_file_name = find_data_synoptic(ids = ids,
                                       dir = dest_dir_raw_data,
                                       start = start,
                                       end = end,
                                       format = format)
    load_data_synoptic(raw_file_name) %>%
      {if(!is.null(dest_file_processed_data)) {
        readr::write_rds(x = .,
                         file = dest_file_processed_data)}
        else {
          .
        }}

  }
}
