# script to download crash data, pre-process, and then upload to a {{pins}} board
# script is run weekly via a Github action

library(pins)
library(sf)
library(tidyverse)
library(lubridate)
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
  
#if there are no crashes, return NULL so that the function doesn't throw an error  
  if (nrow(df_hist) == 0) {
    return(NULL)
  }
  
  # to access the various flags in the data, we need to parse the json once more
  # and then add that to the original crashes data frame
  crashesJSON <- fromJSON("crashes_hist.json")
  crashes_hist <- df_hist %>%
    add_column(crashesJSON$features$properties$flags)
  crashes_hist |> 
    select(-flags)
}

crashes_all_dane <- map_dfr(2017:year(today()), download_crashes)

crashes_all_dane <- crashes_all_dane |> 
  mutate(date = mdy(date),
         year = year(date),
         severity = case_when(injsvr == "K" ~ "fatal crash",
                              injsvr == "A" ~ "serious injury crash",
                              injsvr == "B" ~ "minor injury crash",
                              injsvr == "C" ~ "suspected injury crash",
                              injsvr == "O" ~ "no injury crash"
         ),
         severity = factor(severity, levels = c("fatal crash",
                                                "serious injury crash",
                                                "minor injury crash",
                                                "suspected injury crash",
                                                "no injury crash"))
  )

board |> pin_write(crashes_all_dane, "crashes_all_dane", versioned = F, type = "rds")



