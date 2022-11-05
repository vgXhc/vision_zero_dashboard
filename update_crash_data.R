# script to download crash data, pre-process, and then upload to a {{pins}} board
# script is run weekly via a Github action

library(pins)
library(sf)
library(tidyverse)
library(jsonlite)


board <- board_s3("vzpins", 
                  region = "us-east-1", 
                  access_key = Sys.getenv("S3_ACCESS_KEY"), 
                  secret_access_key = Sys.getenv("S3_SECRET_ACCESS_KEY"))


download_crashes <- function(year) {
  uri <- paste0("https://CommunityMaps.wi.gov/crash/public/crashesKML.do?filetype=json&startyear=", 
                year, 
                "&endyear=", 
                year, 
                "&injsvr=K&injsvr=A&injsvr=B&injsvr=C&injsvr=O&county=dane")
  download.file(uri, "crashes_hist.json")
  
  df_hist <- st_read("crashes_hist.json")
  
  # to access the various flags in the data, we need to parse the json once more
  # and then add that to the original crashes data frame
  crashesJSON <- fromJSON("crashes_hist.json")
  crashes_hist <- df_hist %>%
    add_column(crashesJSON$features$properties$flags)
  crashes_hist |> 
    select(-flags)
}

crashes_all_dane <- map_dfr(2017:year(today()), download_crashes)


board |> pin_write(crashes_all_dane, "crashes_all_dane", versioned = F, type = "rds")



