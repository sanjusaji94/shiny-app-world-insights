library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(DT)
library(jsonlite)
library(countrycode)
library(maps)
library(bslib)

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
  theme = bs_theme(bootswatch = "flatly", base_font = font_google("Roboto")),
  div(
    style = "text-align: center;",
    titlePanel("World Development Insights"),
    p("Explore and compare key development statistics by country and continent from the CIA World Factbook 2020.")
  ),
  
  tabsetPanel(
    
    # Tab 1: Univariate Analysis
    
    tabPanel("Univariate Analysis",
             sidebarLayout(
               sidebarPanel(
                 style = "overflow-x: auto; max-width: 100%;",
                 
                 selectInput("selected_var", "Choose a variable to explore:",
                             choices = c(
                               "Education Expenditure" = "expenditure",
                               "Youth Unemployment Rate" = "youth_unempl_rate",
                               "Net Migration Rate" = "net_migr_rate",
                               "Population Growth Rate" = "pop_growth_rate",
                               "Electricity from Fossil Fuels" = "electricity_fossil_fuel",
                               "Life Expectancy" = "life_expectancy"
                             ),
                             width = "100%"),  # ✅ Makes dropdown responsive
                 
                 actionButton("show_data", "View Raw Data"),
                 br(), br(),
                 
                 div(
                   style = "max-width: 100%; overflow-x: auto;",  # Makes table scrollable
                   DTOutput("raw_table")
                 )
               ),
               
               
               mainPanel(
                 tabsetPanel(
                   tabPanel("Map", plotlyOutput("map_plot")),
                   tabPanel("Global Analysis", 
                            plotlyOutput("boxplot_global"),
                            br(),
                            br(),
                            plotlyOutput("histogram_global")),
                   tabPanel("Analysis per Continent", 
                            plotlyOutput("boxplot_continent"),
                            br(),
                            br(),
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
    ),
    tags$footer(
      style = "text-align:center; padding: 10px; font-size: 13px; color: #aaa;",
      "© 2025 AKSTA Statistical Computing | Case Study 4"
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
       theme_minimal() +
    theme(
      plot.title = element_text(margin = margin(t = 20, b = 10)),
      plot.margin = margin(t = 40, r = 10, b = 10, l = 10)
    )
    ggplotly(p, tooltip = "text")
  })
  
  # Render raw table (always visible, max 15 rows)
  output$raw_table <- renderDT({
    var <- selected_var()
    
    datatable(
      data %>%
        select(country, continent, !!sym(var)) %>%
        rename(`Selected Variable` = !!sym(var)) %>%
        head(15),
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        autoWidth = TRUE,
        dom = 'tip'
      ),
      rownames = FALSE
    )
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
      theme_minimal() +
      theme(
        plot.title = element_text(margin = margin(t = 20, b = 10)),
        plot.margin = margin(t = 40, r = 10, b = 10, l = 10)
      )
    
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
      theme_minimal() +
      theme(
        plot.title = element_text(margin = margin(t = 20, b = 10)),
        plot.margin = margin(t = 40, r = 10, b = 10, l = 10)
      )
    
    ggplotly(p)
  })
  
  #Grouped density plot by continent
  
  output$density_continent <- renderPlotly({
    var <- selected_var()
    
    p <- ggplot(data %>% filter(!is.na(.data[[var]])),
                aes(x = .data[[var]], fill = continent, color = continent)) +
      geom_density(alpha = 0.6, color = "black") +
      labs(
        x = var_labels[[var]],
        title = paste("Density Plot of", var_labels[[var]], "by Continent")
      ) +
      scale_fill_brewer(palette = "Set1") +
    
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Scatter Plot
  
  output$scatter_plot <- renderPlotly({
    xvar <- input$x_var
    yvar <- input$y_var
    sizevar <- input$size_var
    
    df <- data %>%
      filter(!is.na(.data[[xvar]]), !is.na(.data[[yvar]]), !is.na(.data[[sizevar]]))
    
    p <- ggplot(df, aes(x = .data[[xvar]], y = .data[[yvar]])) +
      geom_point(aes(color = continent, size = .data[[sizevar]]), alpha = 0.6) +
      suppressWarnings(
        geom_smooth(aes(group = continent, color = continent), method = "loess", se = FALSE, size = 0.7)
      ) +
      labs(
        x = var_labels[[xvar]],
        y = var_labels[[yvar]],
        size = stringr::str_to_title(sizevar),
        title = paste(var_labels[[yvar]], "vs", var_labels[[xvar]])
      ) +
      scale_size_continuous(range = c(1, 10)) +
      scale_color_brewer(palette = "Set1")+
      theme_minimal()
    
    ggplotly(p)
  })
  
}

# --- Run the App ---
shinyApp(ui = ui, server = server)
