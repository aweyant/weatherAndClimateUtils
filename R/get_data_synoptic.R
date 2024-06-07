get_data_synoptic <- function(dest_dir,
                              api_key = NULL,
                              station_selection_str = NULL,
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
                              recent = NULL) {
  if(is.null(api_key)) {
    # INTERACTIVE QUERY TO OBTAIN API KEY
    # API KEY SET TO RESULT
  }

  lapply(X = ids,
         FUN = function(id) {
           # PROBABLY GOOD TO WRAP IN A TRY/CATCH OR SOMETHING
           api_call <- build_synoptic_api_call(api_key = api_key,
                                               ids = id,
                                               start = start,
                                               end = end,
                                               recent = recent)
           # print(api_call)
           download.file(url = api_call,
                         destfile = file.path(dest_dir,
                                              paste0(c(Sys.Date(),
                                                       id),
                                                     collapse = "_")))
         })
}

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
                                    recent = NULL) {
  basic_timeseries_stub_str = "https://api.synopticdata.com/v2/stations/timeseries?"
  api_key_str = paste0("token=", api_key)
  time_str=ifelse(!is.null(recent),
                  paste0("recent=", recent),
                  paste0("start=", start, "end=", end))
  station_selection_str=paste0("stid=", paste0(ids, collapse = ","))
  return(paste0(c(basic_timeseries_stub_str,
                  api_key_str,
                  station_selection_str,
                  time_str,
                  "precip=1",
                  "output=csv"), collapse = "&"))
}

get_station_ids_synoptic <- function() {

}

get_api_key_synoptic <- function() {

}

set_api_key_synoptic <- function() {

}
