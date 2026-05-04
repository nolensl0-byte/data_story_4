################################################################################
# Datasets for Data Story 4: Sewanee utilities & weather
################################################################################

# ******************************************************************************
# Ensure "sewanee_weather.rds" & "utilities.rds" are in your working directory
# ******************************************************************************

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(dplyr)
library(ggplot2)
library(readr)
library(shinydashboard)
library(bslib)
library(tidyr)

rm(list = ls()) # clear environment first
dir() # look at files in your working directory

# weather ======================================================================
load('sewanee_weather.rds') # loads 3 datasets

# dataset #1: Monthly rainfall in Sewanee, 1895 - 2023
sewanee_rain %>% head
sewanee_rain %>% tail

# dataset #2: Monthly temperature in Sewanee, 1958 - 2023
# Note some years have wonky data
sewanee_temp$year %>% unique
# So let's take those rows out
sewanee_temp <- sewanee_temp %>% filter(!is.na(as.numeric(year)))
# Now take a look
sewanee_temp %>% head
sewanee_temp %>% tail

# dataset #3: Hourly weather (air temp, soil temp, humidity, rain) from Split Creek Observatory
# Aug 18, 2018 - June 14 2022
split_creek %>% head
split_creek %>% tail

# utilities  ===================================================================
load('utilities.rds') # loads two datasets

# dataset #1: Utilities data for every campus building (water, electricity, natural gas)
# caution: many rows have missing data
utilities %>% as.data.frame %>% head

utilities %>% as.data.frame %>% tail

# dataset #2: Same data for Fall 2025, but with residence hall occupancy information added
# broken down by gender
# caution again: many rows have missing data
fall2025 %>% as.data.frame %>% head
fall2025 %>% as.data.frame %>% tail



# ------------------------------------------------------------------
# UI
# ------------------------------------------------------------------
ui <- fluidPage(
  
  titlePanel("Sewanee Climate Explorer"),
  
  tabsetPanel(
    
    # ==========================================================
    # TAB 1: TEMPERATURE
    # ==========================================================
    tabPanel("Temperature",
             
             sidebarLayout(
               
               sidebarPanel(
                 
                 selectInput(
                   "temp_month",
                   "Select Month(s):",
                   choices = sort(unique(sewanee_temp$month)),
                   selected = unique(sewanee_temp$month)[1],
                   multiple = TRUE
                 )
                 
               ),
               
               mainPanel(
                 plotOutput("temp_plot")
               )
             )
    ),
    
    # ==========================================================
    # TAB 2: RAINFALL
    # ==========================================================
    tabPanel("Rainfall",
             
             sidebarLayout(
               
               sidebarPanel(
                 
                 selectInput(
                   "rain_month",
                   "Select Month(s):",
                   choices = sort(unique(sewanee_rain$month)),
                   selected = unique(sewanee_rain$month)[1],
                   multiple = TRUE
                 )
                 
               ),
               
               mainPanel(
                 plotOutput("rain_plot")
               )
             )
    )
  )
)

# ------------------------------------------------------------------
# SERVER
# ------------------------------------------------------------------
server <- function(input, output, session) {
  
  # ==========================================================
  # TEMPERATURE PLOT
  # ==========================================================
  output$temp_plot <- renderPlot({
    
    req(input$temp_month)
    
    df <- sewanee_temp %>%
      filter(month %in% input$temp_month)
    
    ggplot(df, aes(x = year, y = temp, color = month)) +
      geom_line() +
      geom_point() +
      theme_minimal() +
      labs(
        title = "Temperature by Month",
        x = "Year",
        y = "Temperature"
      )
  })
  
  # ==========================================================
  # RAINFALL PLOT
  # ==========================================================
  output$rain_plot <- renderPlot({
    
    req(input$rain_month)
    
    df <- sewanee_rain %>%
      filter(month %in% input$rain_month)
    
    ggplot(df, aes(x = year, y = inches, color = month)) +
      geom_line() +
      geom_point() +
      theme_minimal() +
      labs(
        title = "Rainfall by Month",
        x = "Year",
        y = "Inches"
      )
  })
}

# ------------------------------------------------------------------
# RUN APP
# ------------------------------------------------------------------
shinyApp(ui, server)