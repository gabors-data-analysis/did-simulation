# install.packages(c("shiny", "tidyverse", "did", "fixest", "plotly"))

# Install and load required packages
# install.packages(c("shiny", "tidyverse", "did", "fixest", "plotly"))
library(shiny)
library(tidyverse)
library(did)
library(fixest)
library(plotly)

# UI Definition
ui <- fluidPage(
  titlePanel("Difference-in-Differences Estimation Comparison"),
  
  sidebarLayout(
    sidebarPanel(
      # Main controls
      radioButtons("timing", "Treatment Timing:",
                   choices = c("Simultaneous" = "once",
                               "Staggered" = "staggered")),
      
      radioButtons("effect_size", "Treatment Effect:",
                   choices = c("Uniform" = "uniform",
                               "Heterogeneous" = "heterogeneous")),
      
      checkboxInput("early_smaller", "Early interventions are smaller", FALSE),
      checkboxInput("global_trend", "Include global trend", FALSE),
      checkboxInput("individual_trend", "Include individual trends", FALSE),
      
      # Advanced settings button
      actionButton("show_advanced", "Show Advanced Settings"),
      
      # Advanced settings panel (initially hidden)
      conditionalPanel(
        condition = "input.show_advanced % 2 == 1",
        numericInput("base_a", "Base value Country A:", 1000),
        numericInput("base_b", "Base value Country B:", 2000),
        numericInput("base_c", "Base value Country C:", 4000),
        numericInput("base_d", "Base value Country D:", 5000),
        numericInput("base_e", "Base value Country E:", 3000),
        numericInput("base_f", "Base value Country F:", 6000),
        numericInput("uniform_effect", "Uniform effect size:", -1000),
        textInput("hetero_effects", "Heterogeneous effects (comma-separated):", 
                  "-500,-1500,-500,-1500"),
        numericInput("global_trend_size", "Global trend increment:", 100),
        textInput("individual_trends", "Individual trends (comma-separated):", 
                  "100,200,300,400,0,0")
      )
    ),
    
    mainPanel(
      plotlyOutput("did_plot"),
      verbatimTextOutput("model_results")
    )
  )
)

# Server logic
server <- function(input, output, session) {
  
  # Generate dataset based on inputs
  generate_data <- reactive({
    # Base parameters
    years <- 2010:2020
    countries <- LETTERS[1:6]
    
    # Treatment timing
    if (input$timing == "once") {
      treat_years <- rep(2015, 4)
    } else {
      treat_years <- c(2013, 2014, 2016, 2017)
    }
    
    # Base values
    base_values <- c(input$base_a, input$base_b, input$base_c, 
                     input$base_d, input$base_e, input$base_f)
    
    # Treatment effects
    if (input$effect_size == "uniform") {
      effects <- rep(input$uniform_effect, 4)
    } else {
      effects <- as.numeric(strsplit(input$hetero_effects, ",")[[1]])
      if (input$early_smaller) {
        effects <- sort(effects)
      }
    }
    
    # Create base dataset
    data <- expand.grid(year = years, country = countries) %>%
      arrange(country, year) %>%
      mutate(
        base_value = rep(base_values, each = length(years)),
        value = base_value
      )
    
    # Add trends
    if (input$global_trend) {
      data <- data %>%
        group_by(country) %>%
        mutate(value = value + input$global_trend_size * (year - min(year)))
    }
    
    if (input$individual_trend) {
      ind_trends <- as.numeric(strsplit(input$individual_trends, ",")[[1]])
      data <- data %>%
        group_by(country) %>%
        mutate(value = value + rep(ind_trends, each = length(years)) * (year - min(year)))
    }
    
    # Add treatment effects
    for(i in 1:4) {
      data <- data %>%
        mutate(
          value = ifelse(country == LETTERS[i] & year >= treat_years[i],
                         value + effects[i],
                         value)
        )
    }
    
    # Add treatment indicators
    data <- data %>%
      mutate(
        treated = country %in% LETTERS[1:4],
        post = year >= 2015,
        rel_time = case_when(
          country %in% LETTERS[1:4] ~ year - treat_years[match(country, LETTERS[1:4])],
          TRUE ~ 0
        )
      )
    
    return(data)
  })
  
  # Create plot
  output$did_plot <- renderPlotly({
    data <- generate_data()
    
    p <- ggplot(data, aes(x = year, y = value, color = country, group = country)) +
      geom_line() +
      geom_point() +
      theme_minimal() +
      labs(title = "Treatment Effects Over Time",
           x = "Year",
           y = "Sales of Sugary Drinks")
    
    ggplotly(p)
  })
  
  # Run models and display results
  output$model_results <- renderPrint({
    data <- generate_data()
    
    # TWFE model
    twfe_model <- feols(value ~ treated:post | country + year, data = data)
    
    # First difference model
    fd_data <- data %>%
      group_by(country) %>%
      mutate(value_diff = value - lag(value)) %>%
      filter(!is.na(value_diff))
    
    fd_model <- feols(value_diff ~ treated:post | year, data = fd_data)
    
    # CS DiD model
    cs_model <- att_gt(
      yname = "value",
      gname = "country",
      tname = "year",
      idname = "country",
      data = data %>% filter(country != "E" & country != "F"),
      control_group = "nevertreated"
    )
    
    # Print results
    cat("TWFE Model Results:\n")
    print(summary(twfe_model))
    
    cat("\nFirst Difference Model Results:\n")
    print(summary(fd_model))
    
    cat("\nCallaway & Sant'Anna DiD Results:\n")
    print(summary(cs_model))
  })
}

# Run the app
shinyApp(ui = ui, server = server)