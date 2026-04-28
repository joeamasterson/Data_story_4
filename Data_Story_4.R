library(shiny)
library(dplyr)
library(ggplot2)
library(DT)
library(lubridate)


load("sewanee_weather.rds")
load("utilities.rds")

sewanee_rain$year <- as.numeric(sewanee_rain$year)
sewanee_temp$year <- as.numeric(sewanee_temp$year)
sewanee_temp$temp <- as.numeric(sewanee_temp$temp)

split_creek$dtf  <- as.POSIXct(split_creek$dtf)
split_creek$year <- lubridate::year(split_creek$dtf)
split_creek$humidity <- as.numeric(split_creek$humidity)
split_creek$rain_in  <- as.numeric(split_creek$rain_in)

utilities$gallons          <- as.numeric(utilities$gallons)
utilities$gal_per_day      <- as.numeric(utilities$gal_per_day)
utilities$capacity         <- as.numeric(utilities$capacity)
utilities$ac               <- as.character(utilities$ac)
utilities$central_chiller  <- as.character(utilities$central_chiller)
utilities$building         <- as.character(utilities$building)
utilities$gallons_per_person <- utilities$gallons / utilities$capacity

fall2025$month    <- as.numeric(fall2025$month)
fall2025$capacity <- as.numeric(fall2025$capacity)
fall2025$male     <- as.numeric(fall2025$male)
fall2025$female   <- as.numeric(fall2025$female)
fall2025$gallons  <- as.numeric(fall2025$gallons)
fall2025$gal_per_day <- as.numeric(fall2025$gal_per_day)
fall2025$building <- as.character(fall2025$building)



ui <- fluidPage(
  titlePanel("Sewanee Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      
      # Weather controls
      conditionalPanel(
        condition = "input.tabs == 'weather_tab'",
        selectInput("weather_var", "Variable", choices = c("Temperature", "Rainfall")),
        sliderInput("weather_years", "Year Range",
                    min = 1895, max = 2025, value = c(2000, 2025), sep = ""),
        selectInput("temp_stat", "Temp Statistic",
                    choices = c("avg", "max", "min"), selected = "avg")
      ),
      
      # Split Creek controls
      conditionalPanel(
        condition = "input.tabs == 'creek_tab'",
        sliderInput("creek_years", "Year Range",
                    min = 2018, max = 2025, value = c(2018, 2024), sep = "", ticks = FALSE)
      ),
      
      # Utilities controls
      conditionalPanel(
        condition = "input.tabs == 'utilities_tab'",
        radioButtons("utility_measure", "Measure",
                     choices = c("Gallons/Day" = "gal_per_day",
                                 "Gallons/Person" = "gallons_per_person",
                                 "Total Gallons" = "gallons"),
                     selected = "gal_per_day"),
        radioButtons("ac_filter", "AC Filter",
                     choices = c("Compare AC vs No AC" = "compare",
                                 "AC Only" = "Yes",
                                 "No AC Only" = "No",
                                 "All Buildings" = "All"),
                     selected = "compare")
      ),
      

      conditionalPanel(
        condition = "input.tabs == 'housing_tab'",
        selectInput("housing_var", "Variable",
                    choices = c("capacity", "male", "female", "gallons", "gal_per_day")),
        sliderInput("housing_months", "Month Range",
                    min = 8, max = 11, value = c(8, 11), step = 1)
      )
    ),
    
    mainPanel(
      tabsetPanel(
        id = "tabs",
        
        tabPanel("Weather",        value = "weather_tab",
                 plotOutput("weather_plot", height = "500px"),
                 DTOutput("weather_table")),
        
        tabPanel("Split Creek",    value = "creek_tab",
                 plotOutput("creek_plot", height = "500px"),
                 DTOutput("creek_table")),
        
        tabPanel("Utilities",      value = "utilities_tab",
                 plotOutput("utilities_plot", height = "500px"),
                 DTOutput("utilities_table")),
        
        tabPanel("Fall 2025 Housing", value = "housing_tab",
                 plotOutput("housing_plot", height = "500px"),
                 DTOutput("housing_table"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  weather_data <- reactive({
    if (input$weather_var == "Temperature") {
      sewanee_temp %>%
        filter(year >= input$weather_years[1],
               year <= input$weather_years[2],
               stat == input$temp_stat) %>%
        group_by(year) %>%
        summarize(value = mean(temp, na.rm = TRUE), .groups = "drop")
    } else {
      sewanee_rain %>%
        filter(year >= input$weather_years[1],
               year <= input$weather_years[2]) %>%
        group_by(year) %>%
        summarize(value = sum(inches, na.rm = TRUE), .groups = "drop")
    }
  })
  
  output$weather_plot <- renderPlot({
    ggplot(weather_data(), aes(x = year, y = value)) +
      geom_line(linewidth = 1) +
      geom_point(size = 2) +
      labs(title = paste(input$weather_var, "by Year"), x = "Year", y = input$weather_var) +
      theme_minimal(base_size = 15)
  })
  
  output$weather_table <- renderDT({
    datatable(
      weather_data() %>% mutate(value = round(value, 1)),
      colnames = c("Year", input$weather_var),
      options = list(pageLength = 10)
    )
  })
  
  creek_data <- reactive({
    hum <- split_creek %>%
      filter(year >= input$creek_years[1], year <= input$creek_years[2]) %>%
      group_by(year) %>%
      summarize(humidity = mean(humidity, na.rm = TRUE), .groups = "drop")
    
    rain <- sewanee_rain %>%
      filter(year >= input$creek_years[1], year <= input$creek_years[2]) %>%
      group_by(year) %>%
      summarize(rainfall = sum(inches, na.rm = TRUE), .groups = "drop")
    
    inner_join(hum, rain, by = "year")
  })
  
  output$creek_plot <- renderPlot({
    ggplot(creek_data(), aes(x = year)) +
      geom_line(aes(y = humidity, color = "Humidity"), linewidth = 1) +
      geom_line(aes(y = rainfall, color = "Rainfall"), linewidth = 1) +
      labs(title = "Split Creek Humidity vs Sewanee Rainfall",
           x = "Year", y = "Value", color = "Variable") +
      theme_minimal(base_size = 15)
  })
  
  output$creek_table <- renderDT({
    datatable(
      creek_data() %>% mutate(humidity = round(humidity, 1), rainfall = round(rainfall, 1)),
      colnames = c("Year", "Humidity (%)", "Rainfall (inches)"),
      options = list(pageLength = 10)
    )
  })
  
  utility_data <- reactive({
    dat <- utilities
    
    if (input$ac_filter %in% c("Yes", "No")) {
      dat <- dat %>% filter(ac == input$ac_filter)
    }
    
    dat %>%
      group_by(building, ac) %>%
      summarize(value = mean(.data[[input$utility_measure]], na.rm = TRUE),
                capacity = mean(capacity, na.rm = TRUE),
                .groups = "drop") %>%
      filter(is.finite(value)) %>%
      arrange(desc(value))
  })
  
  output$utilities_plot <- renderPlot({
    if (input$ac_filter == "compare") {
      ggplot(utilities, aes(x = ac, y = .data[[input$utility_measure]], fill = ac)) +
        geom_boxplot(alpha = 0.8) +
        geom_jitter(width = 0.15, alpha = 0.4) +
        scale_fill_manual(values = c("Yes" = "red", "No" = "blue")) +
        scale_y_continuous(labels = scales::comma) +
        labs(title = paste("AC vs No AC -", input$utility_measure),
             x = "Has AC?", y = input$utility_measure) +
        theme_minimal(base_size = 15) +
        theme(legend.position = "none")
    } else {
      plot_dat <- utility_data() %>% slice_head(n = 15)
      ggplot(plot_dat, aes(x = reorder(building, value), y = value, fill = ac)) +
        geom_col(alpha = 0.85) +
        coord_flip() +
        scale_fill_manual(values = c("Yes" = "red", "No" = "blue")) +
        scale_y_continuous(labels = scales::comma) +
        labs(title = paste("Top Buildings by", input$utility_measure),
             x = "Building", y = input$utility_measure, fill = "AC?") +
        theme_minimal(base_size = 15)
    }
  })
  
  output$utilities_table <- renderDT({
    datatable(
      utility_data() %>%
        mutate(value = round(value, 1)) %>%
        rename(!!input$utility_measure := value),
      options = list(pageLength = 10, scrollX = TRUE)
    )
  })
  
  # ---- Housing ----
  housing_data <- reactive({
    fall2025 %>%
      filter(month >= input$housing_months[1],
             month <= input$housing_months[2]) %>%
      group_by(building) %>%
      summarize(value = mean(.data[[input$housing_var]], na.rm = TRUE),
                capacity = mean(capacity, na.rm = TRUE),
                .groups = "drop") %>%
      arrange(desc(value))
  })
  
  output$housing_plot <- renderPlot({
    plot_dat <- housing_data() %>% slice_head(n = 15)
    ggplot(plot_dat, aes(x = reorder(building, value), y = value)) +
      geom_col() +
      coord_flip() +
      labs(title = paste("Fall 2025 Housing:", input$housing_var),
           x = "Building", y = input$housing_var) +
      theme_minimal(base_size = 15)
  })
  
  output$housing_table <- renderDT({
    datatable(
      housing_data() %>%
        mutate(value = round(value, 1)) %>%
        rename(!!input$housing_var := value),
      options = list(pageLength = 10, scrollX = TRUE)
    )
  })
}

shinyApp(ui = ui, server = server)