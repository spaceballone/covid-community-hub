#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(dplyr)
library(stringr)
library(shiny)
library(shinymaterial)
library(plotly)
library(forecast)
library(viridisLite)
library(tidyverse)
library(shinyWidgets)
library(DT)
library(RColorBrewer)
library(shinyjs)
library(leaflet)
library(RSocrata)
library(leaflet.extras)
library(scales)
library(jsonlite)
library(gt)

#source("data_gen.R")
source("resource.R")
source("map.R")
source("getDataLocal.R")

#---- population data ----
# population_data <- read.csv("./population-figures-by-country-csv_csv.csv")
# population_data_short <- population_data %>% select(Country, Country_Code, Year_2016)

ui <- navbarPage(
  "COVID-19",
  tabPanel("Cases",
           mainPanel(width = 12,
             mapUI("map_module")
             )
  ),
  tabPanel("Resources",
          mainPanel(width = 12,
            resourceUI("resource_module")
          )),
  tabPanel("Return to work",
           mainPanel(width = 12,
               "Placeholder for metrics around return to work"
           )),
  navbarMenu("More")
)

server <- function(input, output, session) {
  
  #---- git pull ----
  # git_pull <- reactive({
  #   progress <- shiny::Progress$new()
  #   on.exit(progress$close())
  #   
  #   progress$set(message = "Updating CSSE data", value = 0.5)
  #   wait_show(session)
  #   if (dir.exists("COVID-19")) {
  #     setwd("COVID-19")
  #     system("git pull")
  #     setwd("..")
  #   } else {
  #     
  #     system("git clone https://github.com/CSSEGISandData/COVID-19.git", timeout = 1000)
  #   }
  #   wait_hide(session)
  # })
  
  map_data <- reactive({
    # git_pull()
    wait_show(session)
    # map_data <- generate_all_from_daily("./COVID-19/csse_covid_19_data/csse_covid_19_daily_reports") %>%
    #   arrange(desc(recovered))
    daily_report_url <- "https://covid-19-response.demo.socrata.com/dataset/CSSE-COVID-19-Daily-Reports-with-Deltas/xxnh-qb7c"
    map_data2 <- read.socrata(daily_report_url) %>% 
      mutate(Combined_Key = paste0(admin2,", ",province_state)) %>%
      mutate(date = as.Date(last_update)) %>%
      rename(`country.x` = `country_region`,
             `province.y` = `province_state`,
             `Long` = `long_`,
             `Lat` = `lat`)
    wait_hide(session)
    return(map_data2)
  })
  
  all_dates <- reactive({
    map_data()$date %>% unique() %>% as.POSIXct(origin = "1970-01-01")
  })
  
  callModule(resource, "resource_module")
  callModule(map, "map_module", map_data = map_data, all_dates = all_dates)
}

# Run the application 
shinyApp(ui = ui, server = server)
