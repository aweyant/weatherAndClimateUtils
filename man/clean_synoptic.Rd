% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clean_data_synoptic.R
\name{clean_synoptic}
\alias{clean_synoptic}
\alias{load_data_synoptic}
\alias{process_header_synoptic}
\title{Process raw synoptic .csv files into standard tibbles}
\usage{
load_data_synoptic(raw_synoptic_file)

process_header_synoptic(raw_synoptic_file, offset = 0)
}
\arguments{
\item{raw_synoptic_file}{string; the path to a raw synoptic comma-delimited
file, as obtained by get_data_synoptic(); The first 6 lines of such a file
contain identifying station metadata, followed by the names of observed
variables, their units, and, finally, the observations themselves}

\item{offset}{the number of rows to skip in a file before expecting header
information. If data are downloaded with the API, this is 0. If the data were
downloaded manually from the web interface, this is 4 due to an addition
message appended to each file.}
}
\value{
a nested tibble with columns Station_ID, Station Name, LATITUDE,
LONGITUDE, ELEVATION [ft], STATE, and obs
}
\description{
Process raw synoptic .csv files into standard tibbles
}
\examples{
today <- Sys.Date()
get_data_synoptic(ids = "KSAN",
dest_dir = file.path('~', "Downloads"),
recent = 120)
KSAN_tbl <- load_data_synoptic(file.path("~", "Downloads", paste0(today, "_KSAN.csv")))
}
