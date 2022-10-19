library(pins)
library(sf)

board <- board_s3("vzpins", 
                  region = "us-east-1", 
                  access_key = Sys.getenv("S3_ACCESS_KEY"), 
                  secret_access_key = Sys.getenv("S3_SECRET_ACCESS_KEY"))


download.file("https://CommunityMaps.wi.gov/crash/public/crashesKML.do?filetype=json&startyear=2017&endyear=2018&injsvr=K&injsvr=A&injsvr=B&injsvr=C&injsvr=O&county=dane", "crashes_hist.json")
df_hist <- st_read("crashes_hist.json")

board |> pin_write(df_hist, "crashes_2017", versioned = F, type = "rds")
