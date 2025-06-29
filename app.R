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


ui <- fluidPage(
  titlePanel("World Data Explorer"),
  
  p("This app allows you to explore global indicators from the CIA World Factbook (2020)."),
  
  tabsetPanel(
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
    )
  
  )
)

# --- Define Server ---
server <- function(input, output, session) {
}

# --- Run the App ---
shinyApp(ui = ui, server = server)
