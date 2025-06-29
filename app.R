library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(DT)
library(jsonlite)
library(countrycode)
library(maps)

# Load data
data <- fromJSON("data_cia2.json")

# Get map shapes
world_map <- map_data("world")

# Add ISO3 codes
world_map$ISO3 <- countrycode(world_map$region, origin = "country.name", destination = "iso3c")

# Join map and data
data_map <- left_join(world_map, data, by = "ISO3", relationship = "many-to-many")

# ui
ui <- fluidPage(
  titlePanel("World Data Explorer"),
  
  p("This app allows you to explore global indicators from the CIA World Factbook (2020)."),
  
  tabsetPanel(
    
    # Tab 1: Univariate Analysis
    tabPanel("Univariate Analysis",
             sidebarLayout(
               sidebarPanel(
                 selectInput("selected_var", "Choose a variable to explore:",
                             choices = c(
                               "Education Expenditure" = "expenditure",
                               "Youth Unemployment Rate" = "youth_unempl_rate",
                               "Net Migration Rate" = "net_migr_rate",
                               "Population Growth Rate" = "pop_growth_rate",
                               "Electricity from Fossil Fuels" = "electricity_fossil_fuel",
                               "Life Expectancy" = "life_expectancy"
                             )),
                 
                 actionButton("show_data", "View Raw Data"),
                 br(), br(),
                 DTOutput("raw_table")
               ),
               
               mainPanel(
                 tabsetPanel(
                   tabPanel("Map", plotlyOutput("map_plot")),
                   tabPanel("Global Analysis", 
                            plotlyOutput("boxplot_global"),
                            plotlyOutput("histogram_global")),
                   tabPanel("Analysis per Continent", 
                            plotlyOutput("boxplot_continent"),
                            plotlyOutput("density_continent"))
                 )
               )
             )
    ),
    
    # Tab 2: Multivariate Analysis
    tabPanel("Multivariate Analysis",
             sidebarLayout(
               sidebarPanel(
                 selectInput("x_var", "X-axis Variable:",
                             choices = c(
                               "Education Expenditure" = "expenditure",
                               "Youth Unemployment Rate" = "youth_unempl_rate",
                               "Net Migration Rate" = "net_migr_rate",
                               "Population Growth Rate" = "pop_growth_rate",
                               "Electricity from Fossil Fuels" = "electricity_fossil_fuel",
                               "Life Expectancy" = "life_expectancy"
                             )),
                 selectInput("y_var", "Y-axis Variable:",
                             choices = c(
                               "Education Expenditure" = "expenditure",
                               "Youth Unemployment Rate" = "youth_unempl_rate",
                               "Net Migration Rate" = "net_migr_rate",
                               "Population Growth Rate" = "pop_growth_rate",
                               "Electricity from Fossil Fuels" = "electricity_fossil_fuel",
                               "Life Expectancy" = "life_expectancy"
                             )),
                 selectInput("size_var", "Size by:",
                             choices = c("Population" = "population", "Area" = "area"))
               ),
               mainPanel(
                 plotlyOutput("scatter_plot")
               )
             )
    )
    
  )
)

# --- Define Server ---
server <- function(input, output, session) {
  
  # 1. Store the selected variable (like "net_migr_rate") reactively
  selected_var <- reactive({
    input$selected_var
  })
  
  # User-friendly variable labels
  var_labels <- c(
    expenditure = "Education Expenditure",
    youth_unempl_rate = "Youth Unemployment Rate",
    net_migr_rate = "Net Migration Rate",
    pop_growth_rate = "Population Growth Rate",
    electricity_fossil_fuel = "Electricity from Fossil Fuels",
    life_expectancy = "Life Expectancy"
  )
  
  # 2. Render the interactive map
  output$map_plot <- renderPlotly({
    var <- selected_var()  # Get the selected column name
    
    # Basic ggplot map using that variable
    p <- ggplot(data_map, aes(x = long, y = lat, group = group,
                              fill = .data[[var]], text = paste0(region, ": ", .data[[var]]))) +
      geom_polygon(color = "white") +
      scale_fill_viridis_c(option = "C", na.value = "gray80") +
      labs(
        title = paste("Map of", var_labels[[var]]),
        fill = var_labels[[var]]
      ) +
      theme_void()
    
    ggplotly(p, tooltip = "text")
  })
  
  # Boxplot (global)

  output$boxplot_global <- renderPlotly({
    var <- selected_var()
    
    p <- ggplot(data %>% filter(!is.na(.data[[var]])),
                aes(y = .data[[var]])) +
      geom_boxplot(fill = "steelblue", color = "black") +
      labs(
        y = var_labels[[var]],
        title = paste("Boxplot of", var_labels[[var]])
      ) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  
  # Histogram + Density (global)
  
  output$histogram_global <- renderPlotly({
    var <- selected_var()
    
    p <- ggplot(data %>% filter(!is.na(.data[[var]])), aes(x = .data[[var]])) +
      geom_histogram(aes(y = after_stat(density)), bins = 30,
                     fill = "skyblue", color = "black", alpha = 0.6) +
      geom_density(color = "darkblue", size = 1.2) +
      labs(
        x = var_labels[[var]],
        title = paste("Histogram & Density of", var_labels[[var]])
      ) +
      theme_minimal()
    
    ggplotly(p)
  })
  
 # Boxplot by continent
  
  output$boxplot_continent <- renderPlotly({
    var <- selected_var()
    
    p <- ggplot(data %>% filter(!is.na(.data[[var]])),
                aes(x = continent, y = .data[[var]], fill = continent)) +
      geom_boxplot() +
      labs(
        x = "Continent",
        y = var_labels[[var]],
        title = paste("Boxplot of", var_labels[[var]], "by Continent")
      ) +
      scale_fill_brewer(palette = "Set2")+
      theme_minimal()
    
    ggplotly(p)
  })
  
  #Grouped density plot by continent
  
  output$density_continent <- renderPlotly({
    var <- selected_var()
    
    p <- ggplot(data %>% filter(!is.na(.data[[var]])),
                aes(x = .data[[var]], fill = continent, color = continent)) +
      geom_density(alpha = 0.4) +
      labs(
        x = var_labels[[var]],
        title = paste("Density Plot of", var_labels[[var]], "by Continent")
      ) +
      scale_fill_viridis_d(option = "D") +
      scale_color_viridis_d(option = "D") +
      theme_minimal()
    
    ggplotly(p)
  })
  
}

# --- Run the App ---
shinyApp(ui = ui, server = server)
