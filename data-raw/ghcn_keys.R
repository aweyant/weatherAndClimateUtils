## code to prepare `ghcn_keys` dataset goes here

ghcn_measurement_dict <- data.frame(
  flag = c("A","B","D","H","K","L","O","P","T","W"),
  note = c(
    "value in precipitation or snow is a multi-day total, accumulated since last measurement",
    "precipitation total formed from two twelve-hour totals",
    "precipitation total formed from four six-hour totals",
    "represents highest or lowest hourly temperature (TMAX or TMIN) or average of hourly values (TAVG)",
    "converted from knots",
    "temperature appears to be lagged with respect to reported hour of observation",
    "converted from oktas",
    "identified as missing and presumed zero in DSI 3200 and 3206",
    "trace of precipitation, snowfall, or snow depth",
    "converted from 16-point WBAN code (for wind direction)"
  )
)

ghcn_quality_dict <- data.frame(
  flag = c("D", "G", "I", "K", "L", "M", "N", "O", "R", "S", "T", "W", "X", "Z"),
  note = c(
    "failed duplicate check",
    "failed gap check",
    "failed internal consistency check",
    "failed streak/frequent-value check",
    "failed check on length of multiday period",
    "failed mega-consistency check",
    "failed naught check",
    "failed climatological outlier check",
    "failed lagged range check",
    "failed spatial consistency check",
    "failed temporal consistency check",
    "temperature too warm for snow",
    "failed bounds check",
    "flagged as a result of an official Datzilla investigation"
  )
)

usethis::use_data(ghcn_measurement_dict, ghcn_quality_dict, overwrite = TRUE)
