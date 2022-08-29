### Data prep
library(tidyverse)
library(gghighlight)
library(toOrdinal)
library(lubridate)
library(sf)
library(jsonlite)
library(tmap)
library(shinydashboard)
library(DT)

# download crash data and save it locally
# 
download.file("https://CommunityMaps.wi.gov/crash/public/crashesKML.do?filetype=json&startyear=2017&injsvr=K&injsvr=A&county=dane", "crashes_hist.json")
df_hist <- st_read("crashes_hist.json")

# Madison city limits
# downloaded from OpenData portal https://data-cityofmadison.opendata.arcgis.com/datasets/cityofmadison::city-limit/about
madison <- st_read("data/City_Limit.shp") %>% 
  st_make_valid()


# set up time intervals
d <- today()

# last year YTD for comparison
last_year_YTD <- interval(start = floor_date(today() - years(1), unit = "year"),
                          end = today() - years(1))


# to access the various flags in the data, we need to parse the json once more
# and then add that to the original crashes data frame
crashesJSON <- fromJSON("crashes_hist.json")


# data frame for map that keeps geography
crashes_map <- df_hist %>% 
  add_column(crashesJSON$features$properties$flags) %>% 
  filter(muniname == "MADISON") %>% 
  mutate(date = mdy(date),
         severity = case_when(injsvr == "A" ~ "serious injury crash",
                              injsvr == "K" ~ "fatal crash"),
         year = year(date),
         location = paste0(stringr::str_to_title(onrdwy), " at ", stringr::str_to_title(atrdwy)))


# historic numbers
crashes_hist <- df_hist %>%
  mutate(date = mdy(date),
         totfatl = as.numeric(totfatl),
         totinj = as.numeric(totinj),
         year = year(date),
         month = month(date, label = T)) %>%
  st_drop_geometry()


crashes_hist <- crashes_hist %>%
  add_column(crashesJSON$features$properties$flags) %>% 
  filter(muniname == "MADISON")



# data frame for current year
crashes <- crashes_hist |> 
  filter(year == year(today()))



crashes_last_year_YTD <- crashes_hist %>% 
  filter(date %within% last_year_YTD)

last_month <- month(floor_date(d, unit = "month") -1, label = T, abbr = T)
last_month_long <- month(floor_date(d, unit = "month") -1, label = T, abbr = F)

crashes_hist_by_mo <-  crashes_hist %>% 
  group_by(year, month) %>% 
  summarize(tot_fat_mo = sum(totfatl), 
            tot_inj_mo = sum(totinj), 
            tot_fat_inj_mo = tot_fat_mo + tot_inj_mo
  ) %>% 
  group_by(month) %>% 
  summarise(year, tot_fat_inj_mo, max_fat_inj_mo = max(tot_fat_inj_mo))

ranked <- crashes_hist_by_mo %>% 
  filter(month == last_month) %>% 
  pull(tot_fat_inj_mo)
crashes_last_mo <- tail(ranked, 1)
rank_mo <- tail(rank(-ranked), n = 1)
rank_mo_str <- ifelse(rank_mo == 1, "", toOrdinal(rank_mo))
title_month <- paste0("Fatal and serious traffic injuries in Madison in ", last_month_long, ", 2017-2022")

# function that returns the number of crashes by mode (flag) and type (serious/fatal)
# setting flag to "all" returns all crashes
sumCrashes <- function(df, flag, type){
  if(flag == "all")
     {
          df %>% 
            summarise(sum(eval(parse(text = type)))) %>% 
            pull()

  } else
          df %>% 
            filter(eval(parse(text = flag)) == "Y") %>% 
            summarise(sum(eval(parse(text = type)))) %>% 
            pull()
  
}


bike_fat <- sumCrashes(crashes, "bikeflag", "totfatl")
ped_fat <- sumCrashes(crashes, "pedflag", "totfatl")
bike_svr <- sumCrashes(crashes, "bikeflag", "totinj")
ped_svr <- sumCrashes(crashes, "pedflag", "totinj")
mcyc_fat <- sumCrashes(crashes, "mcycflag", "totfatl")
mcyc_svr <- sumCrashes(crashes, "mcycflag", "totinj")

# function to calculate percent change from last year YTD to current year YTD
pct_change_YTD <- function(flag, type){
  scales::percent((sumCrashes(crashes, flag, type) - sumCrashes(crashes_last_year_YTD, flag, type)) / sumCrashes(crashes_last_year_YTD, flag, type))
}

# function for creating text output for YTD infoboxes, with number and percent change
create_YTD_text <- function(flag, type){
  HTML(paste0(sumCrashes(crashes, flag, type), br(), pct_change_YTD(flag, type)))
}

ranked <- crashes_hist_by_mo %>% 
  filter(month == last_month) %>% 
  pull(tot_fat_inj_mo)
crashes_last_mo <- tail(ranked, 1)
rank_mo <- tail(rank(-ranked), n = 1)
rank_mo_str <- ifelse(rank_mo == 1, "", toOrdinal(rank_mo))
title_month <- paste0("Fatal and serious traffic injuries in Madison in ", last_month_long, ", 2017-2022")
subtitle_month <- paste0("With ", 
                         crashes_last_mo, 
                         " fatalities and serious injuries, this year's ", 
                         last_month_long ,
                         " was the ",
                         rank_mo_str,
                         " worst ",
                         "since 2017."
)



# chart for comparing year-to-date crashes
## create labels
crash_names <- c(
  "fatl" = "Fatalities",
  "inj" = "Serious injuries",
  "fat_inj" = "Fatalities + serious injuries"
)

ytd_chart <- 
  crashes_hist %>% 
  filter(yday(date) <= yday(today())) %>% #filter to crashes YTD for every year
  group_by(year = year(date)) %>% 
  summarize(inj = sum(totinj), fatl = sum(totfatl), fat_inj = inj + fatl) %>% 
  pivot_longer(2:4, names_to = "variable", values_to = "value") %>% 
  mutate(variable = reorder(variable, value)) %>% 
  ggplot(aes(year, value, color = variable)) +
  geom_line() +
  geom_point() +
  geom_text(aes(label = value), nudge_y = 6) +
    scale_color_manual(values = c("red", "black", "blue")) +
  facet_wrap(~ variable, labeller = as_labeller(crash_names)) +
  labs(title = "Fatalities and serious injuries in Madison",
       subtitle = paste0("Year to date (Jan 1 to ", 
                      month(today(), label = T, abbr = T), " ", day(today()), ")")) +
  ylab(element_blank()) +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = "none")

# Define UI 
ui <- dashboardPage(
  dashboardHeader(title = "Madison Vision Zero Dashboard"),
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("Crashes year-to-date", tabName = "all_crashes", icon = icon("calendar")),
      menuItem("Bike crashes", tabName = "bikes", icon = icon("bicycle")),
      menuItem("Impairment", tabName = "impaired", icon = icon("glass-martini")),
      menuItem("Data notes/FAQ", tabName = "data", icon = icon("database"))
    )
  ),
    
    ## Body content
    dashboardBody(
      tabItems(
        # First tab content
        tabItem(tabName = "all_crashes",
                fluidRow(
                  box(title = "All fatalities and serious injuries", width = 12,
                      p("Number year-to-date and change from previous year-to-date"),
                  valueBoxOutput("fatYTDBox", width = 6),
                  valueBoxOutput("injYTDBox", width = 6)
                  )), 
                fluidRow(
                  box(title = "Fatalities and serious injuries involving someone biking", 
                      width = 6,
                  valueBoxOutput("bikeFatYTDBox", width = 6),
                  valueBoxOutput("bikeInjYTDBox", width = 6)),
                  box(title = "Fatalities and serious injuries involving someone walking", width = 6,
                  valueBoxOutput("pedFatYTDBox"),
                  valueBoxOutput("pedInjYTDBox"))),
                fluidRow(
                  box(title = "Fatalities and serious injuries involving someone riding a motorcycle",
                  valueBoxOutput("mcycFatYTDBox", width = 6),
                  valueBoxOutput("mcycInjYTDBox", width = 6),
                  )
                  
                ),
                fluidRow(
                  tmapOutput("ytdMap")
                ),
                fluidRow(
                  plotOutput("monthChart")
                )
        ),
        
        # Second tab content
        tabItem(tabName = "bikes",
                h2("Fatal and serious injury bike crashes"),
                fluidRow(
                  crashes_hist |> 
                    filter(bikeflag == "Y") |> 
                    group_by(year) |> 
                    summarize(sum(totfatl), sum(totinj)) |> 
                    datatable(rownames = F, 
                              width = "80%",
                              options = list(dom = "T"),
                              colnames = c('Year', 'Number of bike fatalities', 'Number of bike serious injuries'))
                  
                ),
                fluidRow(h2("Where did serious and fatal bike crashes occur?"),
                         tmapOutput("bikeMap"))
        ),
        tabItem(tabName = "impaired",
                h2("Serious and fatal crashes that involve impairment"),
                fluidRow(
                  crashes_hist |> 
                    filter(impaired == "Y") |> 
                    group_by(year) |> 
                    summarize(sum(totfatl), sum(totinj)) |> 
                    datatable(rownames = F, 
                              width = "80%",
                              options = list(dom = "T"),
                              colnames = c('Year', 'Fatal injuries involving impairment', 'Serious injuries involving impairment'))
                  
                ),
                fluidRow(h2("Where did serious and fatal crashes involving impairment occur?"),
                         tmapOutput("impairedMap"))
        ),
        tabItem(tabName = "data",
                h2("Data notes/FAQ"),
                p("All crash data is sourced from Community Maps. Community Maps provide the following disclaimer:"),
                tags$blockquote("Community Maps provides a statewide map of all police reported motor vehicle crashes in
Wisconsin from 2010 to the current year. Fatal crashes are included from 2001. Crashes
occurring on or after January 1, 2017 are mapped using geo-coded locations from the Wisconsin
DT4000 police crash report. Prior year crashes have been geo-coded from the crash report
location descriptions. Crashes that have not been geo-coded are not displayed on the map.
Community Maps is maintained by the Wisconsin Traffic Operations and Safety (TOPS)
Laboratory for research purposes and as a service to the Wisconsin Department of
Transportation Bureau of Transportation Safety. See Community Maps for more information:
https://CommunityMaps.wi.gov/."),
                h3("Known limitations"),
                p("Especially for bike and pedestrian crashes, the data have limitations: Crashes that do not involve a motor vehicle are not included in the data. For overall crash data, bike or pedestrian crashes that do involve a motor vehicle but do not result in injury or property damage over $1000 are not included as well."),
                p("The time between when a crash happens and when it appears in CommunityMaps varies. Year-to-date figures therefore may exclude crashes that occurred very recently."),
                h3("Why is there no data before 2017?"),
                p("While Community Maps has data going back before 2017, the method for classifying injuries changed in 2017. Therefore pre-2017 injury data cannot be compared with more current data."),
                h3("Is this data just for the City of Madison?"),
                p("Yes, the data only include crashes coded to have occurred in the City of Madison. This is intentional, as the Vision Zero policy is a City policy and no other municipalities in Dane County (or the county itself have a Vision Zero policy.")
                )
      ),
      tags$footer("Created and maintained by Harald Kliems. This website is not affiliated with the City of Madison. Feature requests? Suggestions? Bug? Submit ", a("an issue on Github", href="https://github.com/vgXhc/vision_zero_dashboard/issues/"), "or reach out via ", a("Twitter", href= "https://twitter.com/HaraldKliems"))
    )
)

# Define server logic 
server <- function(input, output) {

    
    #fatalities YTD
    output$fatYTDBox <- renderInfoBox({
      valueBox(
        "Fatalities", 
        create_YTD_text("all", "totfatl"), 
        icon = icon("square"),
        color = "black"
      )
    })

    
    # serious injuries YTD
    output$injYTDBox <- renderInfoBox({
      valueBox(
        "Serious injuries", 
        create_YTD_text("all", "totinj"),
        icon = icon("ambulance"),
        color = "red"
      )
    }
    )

    
    # bike fatalities YTD
    output$bikeFatYTDBox <- renderInfoBox({
      valueBox(
        bike_fat, 
        "Bike fatalities YTD",
        icon = icon("bicycle"),
        color = "black"
      )
    }
    )
    
    # serious bike injuries YTD
    output$bikeInjYTDBox <- renderInfoBox({
      valueBox(
        bike_svr,
        "Bike serious injuries YTD", 
        icon = icon("bicycle"),
        color = "red"
      )
    }
    )
    
    # ped fatalities YTD
    output$pedFatYTDBox <- renderInfoBox({
      valueBox(
        ped_fat, 
        "Ped fatalities YTD", 
        icon = icon("male"),
        color = "black"
      )
    }
    )
    
    # serious ped injuries YTD
    output$pedInjYTDBox <- renderInfoBox({
      valueBox(
        ped_svr,
        "Ped serious injuries YTD", 
        icon = icon("male"),
        color = "red"
      )
    }
    )
    
    # motorcycle fatalities YTD
    output$mcycFatYTDBox <- renderInfoBox({
      valueBox(
        mcyc_fat, 
        "Motorcycle fatalities YTD", 
        icon = icon("motorcycle"),
        color = "black"
      )
    }
    )
    
    # serious motorcyle injuries YTD
    output$mcycInjYTDBox <- renderInfoBox({
      valueBox(
        mcyc_svr,
        "Motorcycle serious injuries YTD", 
        icon = icon("motorcycle"),
        color = "red"
      )
    }
    )
    
    # map of crashes YTD
    output$ytdMap <- renderTmap(
      tm_shape(madison) +
        tm_polygons(alpha = .2,
                    id = "") +
        tm_shape(crashes_map |> 
                   filter(year == year(today()))) +
        tm_dots("severity", 
                id = "location",
                popup.vars=c("Date"="date", 
                             "Number of fatalities" = "totfatl", 
                             "Number of serious injuries" = "totinj"),
                palette = c("black", "red")) 
    )
    
    output$monthChart <- renderPlot({
      ytd_chart
    })
    
    # map of bike crashes
    output$bikeMap <- renderTmap(
      tm_shape(madison) +
        tm_polygons(alpha = .2,
                    id = "") +
        tm_shape(crashes_map |> 
          filter(bikeflag == "Y")) +
        tm_dots("severity",
                id = "location",
                popup.vars=c("Date"="date", 
                             "Number of fatalities" = "totfatl", 
                             "Number of serious injuries" = "totinj"),
                palette = c("black", "red"))
    )
    
    # map of bike crashes
    output$impairedMap <- renderTmap(
      tm_shape(madison) +
        tm_polygons(alpha = .2,
                    id = "") +
        tm_shape(crashes_map |> 
                   filter(impaired == "Y")) +
        tm_dots("severity",
                id = "location",
                popup.vars=c("Date"="date", 
                             "Number of fatalities" = "totfatl", 
                             "Number of serious injuries" = "totinj"),
                palette = c("black", "red"))
    )

}

# Run the application 
shinyApp(ui = ui, server = server)