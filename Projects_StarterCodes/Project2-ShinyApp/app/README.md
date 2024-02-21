# Project 2: Shiny App Development
### App folder

The App directory contains the app files for the Shiny App (i.e., ui.R and server.R).
 - ui.R and server.R are two key components for the Shiny App 
#install.packages('shiny')
#install.packages('ggplot2')
#install.packages('dplyr')
#install.packages('readr')
#install.packages("leaflet", type = "binary")
#install.packages("leaflet.extra", type = "binary")

library(shiny)
library(ggplot2)
library(dplyr)
library(readr)
library(leaflet)
library(leaflet.extras)


# Load data for Disaster Declarations
data_og <- read.csv("https://raw.githubusercontent.com/yrcho2k/stat-projects/main/DisasterDeclarationsSummaries.csv")
data_df <- read.csv("https://raw.githubusercontent.com/yrcho2k/stat-projects/main/cleaned_data.csv")

# Define UI
ui <- navbarPage(
  title = "Dominant Disaster Type by State and COVID-19 Cases",
  
  tabPanel("Heatmap",
           titlePanel("Dominant Disaster Type by State"),
           sidebarLayout(
             sidebarPanel(
               selectInput("incidentTypeSelect", 
                           "Select Incident Type:", 
                           choices = unique(data_og$incidentType), 
                           selected = "Biological")
             ),
             mainPanel(
               leafletOutput("map")
             )
           )
  ),
  
  tabPanel("COVID-19 Cases Heatmap",
           titlePanel("COVID-19 Cases Heatmap"),
           sidebarLayout(
             sidebarPanel(
               selectInput("stateSelect", 
                           "Select State(s):", 
                           choices = unique(data_df$state), 
                           selected = NULL, 
                           multiple = TRUE),
               checkboxInput("weekend", "Compare by Weekends", value = FALSE)
             ),
             mainPanel(
               plotlyOutput("heatmap")
             )
           )
  ),
  
  tabPanel("Total Cases",
           titlePanel("Total COVID Cases by State"),
           sidebarLayout(
             sidebarPanel(
               selectInput("stateSelectTotal", 
                           "Select State(s):", 
                           choices = unique(data_df$state), 
                           selected = NULL, 
                           multiple = TRUE)
             ),
             mainPanel(
               plotOutput("stateComparisonPlot")
             )
           )
  ),
  
  tabPanel("COVID Cases Comparison",
           titlePanel("Compare COVID Cases at Different Times and in Different States"),
           sidebarLayout(
             sidebarPanel(
               selectInput("stateSelectCompare", 
                           "Select State(s):", 
                           choices = unique(data_df$state), 
                           selected = NULL, 
                           multiple = TRUE),
               checkboxInput("weekendCompare", "Compare by Weekends", value = FALSE)
             ),
             mainPanel(
               leafletOutput("mapCompare"),
               plotOutput("timeComparisonPlot")
             )
           )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Filter data for selected incident type
  filtered_data <- reactive({
    data_og %>%
      filter(incidentType == input$incidentTypeSelect)  
  })
  
  # Aggregate data to find dominant disaster type for each state
  dominant_disaster <- reactive({
    filtered_data() %>%
      group_by(state, incidentType) %>%
      summarise(count = n()) %>%
      arrange(state, desc(count)) %>%
      slice(1)  # Get the dominant disaster type for each state
  })
  
  # Create geographic visualization
  output$map <- renderLeaflet({
    leaflet(data = dominant_disaster()) %>%
      addTiles() %>%
      addMarkers(data = dominant_disaster(), 
                 lng = data_df$longitude, 
                 lat = data_df$latitude, 
                 popup = ~paste(state, "<br>", incidentType, "<br>", count))
  })
  
  # Process the data for Disaster Declarations
  processed_data_disaster <- reactive({
    req(data_og)
    data_og %>%
      filter(incidentType == input$disasterType) %>%
      group_by(state, incidentType) %>%
      summarise(count = n()) %>%
      arrange(desc(count)) %>%
      slice(1) # Get the dominant disaster type for each state
  })
  
  # Create heatmap plot for Disaster Declarations
  output$heatmap <- renderPlotly({
    req(processed_data_disaster())  # Ensure processed_data is available before rendering
    
    # Plotly heatmap for Disaster Declarations
    plot_ly(processed_data_disaster(), x = ~state, y = ~incidentType, z = ~count, type = "heatmap") %>%
      layout(title = paste("Dominant", input$disasterType, "by State"),
             xaxis = list(title = "State"),
             yaxis = list(title = "Dominant Disaster Type"))
  })
  
  # Filtered data reactive expression for COVID-19 cases
  filteredData <- reactive({
    data <- data_df
    if(input$weekend) {
      # Filter for weekends
      data <- data %>% mutate(weekday = weekdays(DeclarationDate)) %>%
        filter(weekday %in% c('Saturday', 'Sunday'))
    }
    
    if(!is.null(input$stateSelect)) {
      # Filter for selected states
      data <- data %>% filter(state %in% input$stateSelect)
    }
    
    data
  })
  
  # Heatmap render for COVID-19 cases
  output$heatmap <- renderPlotly({
    req(filteredData())  # Ensure filteredData is available before rendering
    
    # Plotly heatmap for COVID-19 cases
    plot_ly(filteredData(), x = ~longitude, y = ~latitude, z = ~cases, type = "heatmap") %>%
      layout(title = "COVID-19 Cases Heatmap",
             xaxis = list(title = "Longitude"),
             yaxis = list(title = "Latitude"))
  })
  
  # Plot for comparing COVID-19 cases over time
  output$timeComparisonPlot <- renderPlot({
    data <- data_df %>%
      filter(state %in% input$stateSelectCompare)
    ggplot(data, aes(x = DeclarationDate, y = cases, group = state, color = state)) +
      geom_line() +
      labs(title = "COVID Cases Over Time by State", x = "Date", y = "Number of Cases") +
      theme_minimal()
  })
  
  # Create geographic visualization for COVID-19 cases comparison
  output$mapCompare <- renderLeaflet({
    leaflet(data = filteredData()) %>%
      addTiles() %>%
      addHeatmap(lng = ~longitude, lat = ~latitude, intensity = ~cases,
                 blur = 20, max = 0.05, radius = 15) %>%
      setView(lng = -95.7129, lat = 37.0902, zoom = 4) # Center of the USA
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
