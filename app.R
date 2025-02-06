# This is a shiny app to show how variation in interventions are estimated in two different models: TWFE and DiD. 
# Let us have 6 countries, A to F. and 11 time periods 2010 to 2020. 
# Outcome (y) is sales of sugary drinks, set at a 1000,  b 2000, c 4000, d 5000, e 3000, f 6000. E and F and controls, no intervention. 
# When the intervention happens at once, it is 2015. When staggered it is 2013, 2014, 2016 and 2017. 
# Uniform intervention effect is -1000. Heterogenous is -500, -1500, -500, -1500. 

# Now I want the app to create a graph and show regression results. 
# We'll have 4 models. TWFE, TWFD (first difference) and event time for Staggered DiD. The last will be as in Callaway and sant'anna or sun and abraham
# I want users to be able to set the following aspects: time: once or staggered. size: uniform or heterogenous. 
# Checkbox if early interventions are smaller (ie -500, -500, -1500, -1500). 
# Checkbox if global trend: instead of fixed at values, every year consumption rises by 100.  
# Checkbox if individual trend: global rise + annual + 100,200,300,400, 0, 0. 
# It would be great if all these values could also be set once user clicks on a 'under the hood' bottom.

# Install and load required packages
library(shiny)
library(tidyverse)
library(fixest)
library(plotly)
library(broom)

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
      checkboxInput("year_fe", "Include Year Fixed Effects", FALSE),
      
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
      verbatimTextOutput("model_results"),
      textOutput("warning_message")
    )
  )
)

# Server logic
server <- function(input, output, session) {
  
  observe({
    if (input$year_fe & input$timing == "once" & !input$global_trend & !input$individual_trend) {
      showModal(modalDialog(
        title = "Warning",
        "For uniformly timed single intervention and no global or individual trends, time dummies can not be added.",
        easyClose = TRUE
      ))
    }
  })
  
  # Generate dataset based on inputs
  generate_data <- reactive({
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
    first_treat <- min(treat_timing)
    
    # Set up treatment effects
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
      ind_trends <- as.numeric(strsplit(input$individual_trends, ",")[[1]])
      data <- data %>%
        group_by(country) %>%
        mutate(
          trend_value = ind_trends[match(country, LETTERS[1:6])] * (year - min(year)),
          value = value + trend_value
        ) %>%
        ungroup()
    }
    
    # Add treatment effects and timing variables
    data <- data %>%
      mutate(
        cohort = case_when(
          country == "A" ~ treat_timing[1],
          country == "B" ~ treat_timing[2],
          country == "C" ~ treat_timing[3],
          country == "D" ~ treat_timing[4],
          TRUE ~ Inf
        ),
        relative_time = year - cohort,
        post = !is.infinite(cohort) & year >= cohort,
        event_time = relative_time,
        post_event = !is.infinite(cohort) & relative_time >= 0,
        treatment_fd = case_when(
          post & year == cohort ~ 1,
          TRUE ~ 0
        ),
        effect = case_when(
          post ~ effects[match(country, LETTERS[1:4])],
          TRUE ~ 0
        ),
        value = value + effect
      )
    
    return(data)
  })
    
    
  output$did_plot <- renderPlotly({
    data <- generate_data()
    p <- ggplot(data, aes(x = year, y = value, color = country, group = country)) +
      geom_line() +
      geom_point() +
      theme_minimal() +
      labs(title = "Treatment Effects Over Time", x = "Year", y = "Sales of Sugary Drinks")
    ggplotly(p)
  })
  
  

# In the server function:
  output$model_results <- renderPrint({
    data <- generate_data()
    
    # First difference data preparation
    fd_data <- data %>% 
      group_by(country) %>%
      mutate(
        value_diff = value - lag(value),
        treatment_fd_diff = treatment_fd - lag(treatment_fd)
      ) %>% 
      filter(!is.na(value_diff))
    
    # Model formulas
    fe_formula <- if(input$year_fe) "| country + year" else "| country"
    fd_formula <- if(input$year_fe) "| year" else ""
    
    # TWFE model
    twfe_model <- feols(as.formula(paste("value ~ treated:post", fe_formula)), 
                        data = data,
                        cluster = "country")
    
    # First difference model
    fd_model <- feols(as.formula(paste("value_diff ~ treatment_fd_diff", fd_formula)),
                      data = fd_data,
                      cluster = "country")
    
    # Event study dataset
    data_event <- data %>%
      mutate(
        rel_year = factor(
          case_when(
            relative_time < -5 ~ "-5+",
            relative_time > 5 ~ "5+",
            TRUE ~ as.character(relative_time)
          ),
          levels = c("-5+", as.character(-4:5), "5+")
        )
      ) %>%
      filter(!is.infinite(cohort) | is.infinite(relative_time))
    
    # Event study model
    event_model <- feols(
      value ~ i(rel_year, ref = "-1") | country + year,
      cluster = "country",
      data = data_event
    )
    
    # Print all results
    etable(twfe_model, fd_model, event_model,
           headers = c("TWFE", "First Difference", "Event Study"),
           cluster = "country")
  })
}
# Run the app
shinyApp(ui = ui, server = server)
