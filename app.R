# Install and load required packages
library(shiny)
library(tidyverse)
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
    base_values <- c(input$base_a, input$base_b, input$base_c, 
                     input$base_d, input$base_e, input$base_f)
    
    # Treatment timing
    if (input$timing == "once") {
      treat_timing <- rep(2015, 4)
    } else {
      treat_timing <- c(2013, 2014, 2016, 2017)
    }
    
    # Set up treatment effects
    if (input$effect_size == "uniform") {
      effects <- rep(input$uniform_effect, 4)
    } else {
      effects <- numeric(4)
      het_effects <- as.numeric(strsplit(input$hetero_effects, ",")[[1]])
      effects[1:4] <- if(length(het_effects) >= 4) het_effects[1:4] else c(het_effects, rep(0, 4-length(het_effects)))
      if (input$early_smaller) {
        effects[1:4] <- sort(effects[1:4])
      }
    }
    
    # Create base dataset
    data <- expand.grid(year = years, country = countries) %>%
      arrange(country, year) %>%
      mutate(
        base_value = rep(base_values, each = length(years)),
        value = base_value,
        treated = country %in% LETTERS[1:4]
      )
    
    # Add global trend if selected
    if (input$global_trend) {
      data <- data %>%
        mutate(value = value + input$global_trend_size * (year - min(year)))
    }
    
    # Add individual trends if selected
    if (input$individual_trend) {
      ind_trends <- numeric(6)
      input_trends <- as.numeric(strsplit(input$individual_trends, ",")[[1]])
      ind_trends[1:length(input_trends)] <- input_trends[1:min(length(input_trends), 6)]
      
      data <- data %>%
        group_by(country) %>%
        mutate(
          trend_value = ind_trends[match(country, LETTERS[1:6])] * (year - min(year)),
          value = value + trend_value
        ) %>%
        ungroup()
    }
    
    # Add treatment effects
    data <- data %>%
      mutate(
        cohort = case_when(
          country == "A" ~ treat_timing[1],
          country == "B" ~ treat_timing[2],
          country == "C" ~ treat_timing[3],
          country == "D" ~ treat_timing[4],
          TRUE ~ NA_real_  # Control units get NA
        ),
        time_to_treat = case_when(
          country == "A" ~ year - treat_timing[1],
          country == "B" ~ year - treat_timing[2],
          country == "C" ~ year - treat_timing[3],
          country == "D" ~ year - treat_timing[4],
          TRUE ~ 0
        ),
        post = time_to_treat >= 0 & treated,
        effect = case_when(
          country == "A" & post ~ effects[1],
          country == "B" & post ~ effects[2],
          country == "C" & post ~ effects[3],
          country == "D" & post ~ effects[4],
          TRUE ~ 0
        ),
        value = value + effect
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
    
    # Sun and Abraham style DiD
    sa_model <- feols(value ~ sunab(cohort, year) | country + year, 
                      data = data)
    
    # Create formatted table
    results_table <- data.frame(
      Model = c("TWFE", "First Difference", "Sun-Abraham"),
      Estimate = c(
        coef(twfe_model)["treated:post"],
        coef(fd_model)["treated:post"],
        mean(coef(sa_model)[grep("^treated::", names(coef(sa_model)))], na.rm = TRUE)
      ),
      SE = c(
        sqrt(vcov(twfe_model)["treated:post", "treated:post"]),
        sqrt(vcov(fd_model)["treated:post", "treated:post"]),
        NA
      )
    )
    
    results_table$CI_Lower <- results_table$Estimate - 1.96 * results_table$SE
    results_table$CI_Upper <- results_table$Estimate + 1.96 * results_table$SE
    
    # Format numbers
    results_table <- results_table %>%
      mutate(across(where(is.numeric), ~round(., 2)))
    
    # Print formatted table
    cat("Summary of Results:\n\n")
    print(knitr::kable(results_table, 
                       format = "pipe",
                       col.names = c("Model", "Estimate", "Std. Error", "CI Lower", "CI Upper")))
    
    # Print detailed results
    cat("\n\nDetailed Model Results:\n")
    cat("\nTWFE Model:\n")
    print(summary(twfe_model))
    
    cat("\nFirst Difference Model:\n")
    print(summary(fd_model))
    
    cat("\nSun-Abraham DiD Results:\n")
    print(summary(sa_model))
  })
}

# Run the app
shinyApp(ui = ui, server = server)