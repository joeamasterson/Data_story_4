# Datasets for Data Story 4: Sewanee utilities & weather

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(shiny)
library(dplyr)
library(ggplot2)
library(DT)
library(readr)
library(rstudioapi)

rm(list = ls())

load("sewanee_weather.rds")
load("utilities.rds")

sewanee_temp <- sewanee_temp %>%
  filter(!is.na(as.numeric(year))) %>%
  mutate(year = as.numeric(year))

ui <- fluidPage(
  titlePanel("Sewanee by the Numbers: Weather, Water, and Energy"),
  
  tabsetPanel(
    
    tabPanel("Weather",
             sidebarLayout(
               sidebarPanel(
                 sliderInput("year_range",
                             "Select years:",
                             min = min(sewanee_temp$year, na.rm = TRUE),
                             max = max(sewanee_temp$year, na.rm = TRUE),
                             value = c(1980, 2023)),
                 selectInput("weather_var",
                             "Choose weather variable:",
                             choices = c("Temperature", "Rainfall"))
               ),
               mainPanel(
                 plotOutput("weather_plot"),
                 uiOutput("weather_text")
               )
             )
    ),
    
    tabPanel("Utilities",
             sidebarLayout(
               sidebarPanel(
                 uiOutput("building_ui"),
                 selectInput("utility_var",
                             "Choose utility:",
                             choices = c("water", "electricity", "natural_gas"))
               ),
               mainPanel(
                 plotOutput("utilities_plot"),
                 DTOutput("utilities_table")
               )
             )
    ),
    
    tabPanel("Fall 2025 Housing",
             sidebarLayout(
               sidebarPanel(
                 selectInput("hall", "Choose residence hall:", choices = NULL)
               ),
               mainPanel(
                 plotOutput("fall_plot"),
                 uiOutput("fall_text")
               )
             )
    )
  )
)

server <- function(input, output, session) {
  
  rv <- reactiveValues()
  rv$temp <- sewanee_temp
  rv$rain <- sewanee_rain
  rv$util <- utilities
  rv$fall <- fall2025
  
  observe({
    if (!is.null(fall2025)) {
      updateSelectInput(session, "hall",
                        choices = unique(fall2025$building))
    }
  })
  
  output$building_ui <- renderUI({
    selectInput("building",
                "Choose building:",
                choices = unique(utilities$building),
                selected = unique(utilities$building)[1],
                multiple = TRUE)
  })
  
  output$weather_plot <- renderPlot({
    if (input$weather_var == "Temperature") {
      plot_data <- rv$temp %>%
        filter(year >= input$year_range[1],
               year <= input$year_range[2])
      
      ggplot(plot_data, aes(x = year, y = temp)) +
        geom_line() +
        labs(title = "Temperature in Sewanee",
             x = "Year", y = "Temperature")
    } else {
      plot_data <- rv$rain %>%
        filter(year >= input$year_range[1],
               year <= input$year_range[2])
      
      ggplot(plot_data, aes(x = year, y = rain)) +
        geom_line() +
        labs(title = "Rainfall in Sewanee",
             x = "Year", y = "Rainfall")
    }
  })
  
  output$weather_text <- renderUI({
    HTML(paste0(
      "<b>Story note:</b> This tab lets you compare long-term rainfall and temperature patterns in Sewanee."
    ))
  })
  
  output$utilities_plot <- renderPlot({
    req(input$building)
    
    plot_data <- rv$util %>%
      filter(building %in% input$building)
    
    ggplot(plot_data, aes_string(x = "building", y = input$utility_var)) +
      geom_col() +
      labs(title = paste("Utility comparison:", input$utility_var),
           x = "Building", y = input$utility_var) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$utilities_table <- renderDT({
    rv$util
  })
  
  output$fall_plot <- renderPlot({
    req(input$hall)
    
    plot_data <- rv$fall %>%
      filter(building == input$hall)
    
    ggplot(plot_data, aes(x = building, y = occupancy)) +
      geom_col() +
      labs(title = paste("Occupancy for", input$hall),
           x = "Residence Hall", y = "Occupancy")
  })
  
  output$fall_text <- renderUI({
    req(input$hall)
    HTML(paste0("<b>", input$hall, "</b> lets us compare housing and utility patterns in Fall 2025."))
  })
}

shinyApp(ui, server)