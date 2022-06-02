#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


### Data prep
library(tidyverse)
library(gghighlight)
library(toOrdinal)
library(lubridate)
library(sf)

# download 2022 crash data
download.file("https://CommunityMaps.wi.gov/crash/public/crashesKML.do?filetype=json&startyear=2022&injsvr=K&injsvr=A&county=dane", "crashes.json")
df <- st_read("crashes.json")

# download historic crash data and save it locally
# 
download.file("https://CommunityMaps.wi.gov/crash/public/crashesKML.do?filetype=json&startyear=2017&injsvr=K&injsvr=A&county=dane", "crashes_hist.json")
df_hist <- st_read("crashes_hist.json")


# set up time intervals
d <- today()

crashes <- df %>%
  mutate(date = mdy(date),
         totfatl = as.numeric(totfatl),
         totinj = as.numeric(totinj)) %>%
  st_drop_geometry() %>% 
  filter(muniname == "MADISON")

# historic numbers
crashes_hist <- df_hist %>%
  filter(muniname == "MADISON") %>% 
  mutate(date = mdy(date),
         totfatl = as.numeric(totfatl),
         totinj = as.numeric(totinj),
         year = year(date),
         month = month(date, label = T)) %>%
  st_drop_geometry()

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
subtitle_month <- paste0("With ", 
                         crashes_last_mo, 
                         " fatalities and serious injuries, this year's ", 
                         last_month_long ,
                         " was the ",
                         rank_mo_str,
                         " worst ",
                         "since 2017."
)

p <- crashes_hist_by_mo %>% 
  ggplot(aes(year, tot_fat_inj_mo, fill = tot_fat_inj_mo)) +
  scale_fill_viridis_c()+
  geom_col() +
  scale_x_continuous() +
  scale_y_continuous(breaks = NULL)+
  gghighlight(month(floor_date(d, unit = "month") -1, label = T)  == month) +
  geom_text(aes(label = tot_fat_inj_mo), nudge_y = 4.5) +
  facet_wrap(~month, ncol = 3) +
  #gghighlight(tot_fat_inj_mo == max_fat_inj_mo, max_highlight = 12L, calculate_per_facet = F) +
  theme_minimal()+
  labs(x = element_blank(),
       y = "Fatal and serious injuries",
       title = title_month,
       subtitle = subtitle_month) +
  theme(legend.position = "none",
        axis.text.y = element_blank())


library(shinydashboard)

# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title = "Vision Zero Dashboard"),
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Widgets", tabName = "widgets", icon = icon("th"))
    )
  ),
    
    ## Body content
    dashboardBody(
      tabItems(
        # First tab content
        tabItem(tabName = "dashboard",
                fluidRow(
                  infoBoxOutput("fatYTDBox"),
                  infoBoxOutput("injYTDBox"),
                  infoBoxOutput("bikeFatYTDBox"),
                  infoBoxOutput("bikeInjYTDBox"),
                  infoBoxOutput("pedFatYTDBox"),
                  infoBoxOutput("pedInjYTDBox"),
                  
                  box(
                    title = "Controls",
                    sliderInput("slider", "Number of observations:", 1, 100, 50)
                  )
                )
        ),
        
        # Second tab content
        tabItem(tabName = "widgets",
                h2("Widgets tab content")
        )
      )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
    #fatalities YTD
    output$fatYTDBox <- renderInfoBox({
      infoBox(
        "Fatalities YTD", 
        tot_fat_yr <- crashes %>%
          summarise(sum(totfatl)) %>%
          pull(), 
        icon = icon("car"),
        color = "black"
      )
    })
    # serious injuries YTD
    output$injYTDBox <- renderInfoBox({
      infoBox(
        "Serious injuries YTD", 
        tot_inj_yr <- crashes %>%
          summarise(sum(totinj)) %>%
          pull(), 
        icon = icon("car"),
        color = "red"
      )
    }
      
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
