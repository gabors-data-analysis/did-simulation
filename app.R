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

options(repos = c(CRAN = "https://cloud.r-project.org"))

if (!require(tidyverse)) install.packages("tidyverse")
if (!require(fixest)) install.packages("fixest") 
if (!require(plotly)) install.packages("plotly")
if (!require(broom)) install.packages("broom")
if (!require(shiny)) install.packages("shiny")

library(shiny)
library(tidyverse)
library(fixest)
library(plotly)
library(broom)

# UI Definition
ui <- fluidPage(
  titlePanel("Difference-in-Differences Estimation Comparison"),

  # Add description panel at the top
  fluidRow(
    column(12,
           div(
             class = "well",
             style = "background-color: #f8f9fa; padding: 15px; border-left: 4px solid #0275d8; margin-bottom: 20px;",
             p(
               "In development. Suggestions: contact ",
               tags$a(href = "mailto:bekesg@ceu.edu", "Gabor"),
               " / add an issue to ",
               tags$a(
                 href = "https://github.com/gabors-data-analysis/did-simulation/",
                 target = "_blank",
                 "repo"                )),
             p("This is a panel estimation illustration. Imagine we have 6 countries, A to F, and 13 time periods 2010 to 2022. 
          Outcome (y) is average per capita sales of sugary drinks (ml/week), set at a 1000, b 2000, c 4000, d 5000, e 3000, f 6000. 
          E and F are controls, no intervention."),
             p("The intervention is sales tax that cuts consumption by -1000 as default. You can set many aspects of the intervention, 
          even have 2.")
           )
    )
  ),
  
  

  
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

      
      #  shock controls:
      radioButtons("num_shocks", "Number of Shocks:",
                   choices = c("One" = "1",
                               "Two (Same Effect)" = "2_same",
                               "Two (Varied Effect)" = "2_varied"),
                   selected = "1"),
      
      conditionalPanel(
        condition = "input.num_shocks == '2_varied'",
        sliderInput("second_shock_percent", "Second Shock Effect (%)", 
                    min = -100, max = 200, value = 50, step = 10)
      ),
      
      
      # Add to UI in sidebarPanel before actionButton:
      sliderInput("noise_sd", "Noise SD:", 
                  min = 0, max = 1000, value = 0, step = 50),
      
      # Advanced settings button
      actionButton("show_advanced", "Show Advanced Settings"),
      
      # Advanced settings panel (initially hidden)
      conditionalPanel(
        condition = "input.show_advanced % 2 == 1",
        numericInput("base_a", "Base value Country A:", 2000),
        numericInput("base_b", "Base value Country B:", 3000),
        numericInput("base_c", "Base value Country C:", 5000),
        numericInput("base_d", "Base value Country D:", 6000),
        numericInput("base_e", "Base value Country E:", 4000),
        numericInput("base_f", "Base value Country F:", 7000),
        numericInput("uniform_effect", "Uniform effect size:", -1000),
        textInput("hetero_effects", "Heterogeneous effects (comma-separated):", 
                  "-500,-1500,-500,-1500"),
        numericInput("global_trend_size", "Global trend increment:", 100),
        textInput("individual_trends", "Individual trends (comma-separated):", 
                  "100,200,300,400,0,0")
      )
    ),

    # In the mainPanel section of the UI:
    mainPanel(
      plotlyOutput("did_plot"),
      
      # Add a div for spacing
      div(style = "margin: 40px 0;"),  # This creates 40 pixels of vertical space
      
      # Add info box before model results
      div(
        style = "background-color: #f8f9fa; padding: 15px; border-left: 4px solid #0275d8; margin-bottom: 20px;",
        p(style = "margin: 0;", "We have three models: FE, FD, and event study. The event study recenters the intervention when it's a single one. 
          For multiple ones, does not display. Models estimated in R with feols.")
      ),
      
      
      verbatimTextOutput("model_results"),
      textOutput("warning_message"),
      downloadButton("downloadData", "Download Data"),
      tags$div(
        style = "margin-top: 20px;",
        tags$p("Full app code available at: ",
               tags$a(href = "https://github.com/gabors-data-analysis/did-simulation/blob/main/app.R",
                      "GitHub Repository",
                      target = "_blank"))
      )
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
  
  # Updated generate_data function:
  generate_data <- reactive({
    years <- 2010:2022
    countries <- LETTERS[1:6]
    base_values <- c(input$base_a, input$base_b, input$base_c, 
                     input$base_d, input$base_e, input$base_f)
    
    if (input$timing == "once") {
      treat_timing <- rep(2015, 4)
    } else {
      treat_timing <- c(2013, 2014, 2016, 2017)
    }
    first_treat <- min(treat_timing)
    second_treat_timing <- treat_timing + 3
    
    if (input$effect_size == "uniform") {
      effects <- rep(input$uniform_effect, 4)
    } else {
      effects <- as.numeric(strsplit(input$hetero_effects, ",")[[1]])
      if (input$early_smaller) {
        effects <- effects[order(abs(effects))]
      }
    }
    
    data <- expand.grid(year = years, country = countries) %>%
      arrange(country, year) %>%
      mutate(
        base_value = rep(base_values, each = length(years)),
        value = base_value,
        treated = country %in% LETTERS[1:4]
      )
    
    if (input$global_trend) {
      data <- data %>%
        mutate(value = value + input$global_trend_size * (year - min(year)))
    }
    
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
    
    # In the generate_data reactive function, reorder the mutations, update the treatment calculation:
    data <- data %>%
      mutate(
        cohort = case_when(
          country == "A" ~ treat_timing[1],
          country == "B" ~ treat_timing[2],
          country == "C" ~ treat_timing[3],
          country == "D" ~ treat_timing[4],
          TRUE ~ Inf
        ),
        # Only set second_cohort if multiple shocks selected
        second_cohort = case_when(
          input$num_shocks == "1" ~ Inf,  # No second treatment for single shock
          country == "A" ~ second_treat_timing[1],
          country == "B" ~ second_treat_timing[2],
          country == "C" ~ second_treat_timing[3],
          country == "D" ~ second_treat_timing[4],
          TRUE ~ Inf
        ),
        relative_time = year - cohort,
        post = !is.infinite(cohort) & year >= cohort,
        post_second = !is.infinite(second_cohort) & year >= second_cohort,
        
        # Simplified treatment with no second shock for single treatment
        treatment = case_when(
          !treated ~ 0,
          treated & !post ~ 0,
          treated & post & (input$num_shocks == "1") ~ 1,  # Stay at 1 for single shock
          treated & post & !post_second ~ 1,
          treated & post & post_second ~ 2,
          TRUE ~ 0
        ),
        
        # Updated effect calculation
        effect = case_when(
          treatment == 0 ~ 0,
          treatment == 1 ~ effects[match(country, LETTERS[1:4])],
          treatment == 2 & input$num_shocks == "2_same" ~ 2 * effects[match(country, LETTERS[1:4])],
          treatment == 2 & input$num_shocks == "2_varied" ~ 
            effects[match(country, LETTERS[1:4])] * (1 + input$second_shock_percent/100),
          TRUE ~ 0
        ),
        noise = rnorm(n(), mean = 0, sd = input$noise_sd),
        value = value + effect + noise
      )
    return(data)
  })    
    
  output$did_plot <- renderPlotly({
    data <- generate_data()
    p <- ggplot(data, aes(x = year, y = value, color = country, group = country)) +
      geom_hline(yintercept = seq(0, max(data$value) + 1000, by = 500), 
                 color = "grey90", size = 0.2) +
      geom_line() +
      geom_point() +
      theme_minimal() +
      theme(panel.grid.major = element_line(color = "grey85")) +
      scale_x_continuous(breaks = 2010:2022) +
      scale_y_continuous(breaks = seq(-1000, max(data$value) + 1000, by = 1000), 
                         expand = c(0, 0),
                         limits = c(-1000, max(data$value) + 1000)) +
      labs(title = "Treatment Effects Over Time", x = "Year", y = "Sales of Sugary Drinks")
    ggplotly(p)    
  })
  
  

# In the server function:
 # Update the regression models in the model_results output:
  # Update the regression models to use the simplified treatment variable:
  # Update the model_results output section:
  # Update the model specification part in the model_results:
  output$model_results <- renderPrint({
    data <- generate_data()
    
    # First difference data preparation
    fd_data <- data %>% 
      group_by(country) %>%
      mutate(
        value_diff = value - lag(value),
        treatment_diff = treatment - lag(treatment)
      ) %>% 
      filter(!is.na(value_diff))
    
    # Event study data preparation - only include -3 to +3 years
    data_event <- data %>%
      group_by(country) %>%
      mutate(
        value_diff = value - lag(value),
        rel_year = factor(
          case_when(
            relative_time < -3 ~ "pre",
            relative_time > 3 ~ "post",
            TRUE ~ as.character(relative_time)
          ),
          levels = c("pre", as.character(-3:3), "post")
        )
      ) %>%
      filter(!is.infinite(cohort) | is.infinite(relative_time)) %>%
      filter(!is.na(value_diff))  # Remove first observation due to differencing
    
    
    # Proper formula construction for fixed effects
    if(input$year_fe) {
      twfe_model <- feols(value ~ treatment | country + year, 
                          data = data,
                          cluster = "country")
      
      fd_model <- feols(value_diff ~ treatment_diff | year, 
                        data = fd_data,
                        cluster = "country")
    } else {
      twfe_model <- feols(value ~ treatment | country, 
                          data = data,
                          cluster = "country")
      
      fd_model <- feols(value_diff ~ treatment_diff, 
                        data = fd_data,
                        cluster = "country")
    }
    
#    Event study model - only run for single intervention
    # Event study model in first differences - only run for single intervention
    if(input$num_shocks == "1") {
      event_model <- feols(value_diff ~ i(rel_year, ref = "-1"), 
                           cluster = "country",
                           data = data_event)
    } else {
      # For multiple interventions, create a model with NA coefficient
      event_model <- feols(value_diff ~ 1, 
                           cluster = "country",
                           data = data_event)
    }

    
    # Custom etable output
    etable(twfe_model, fd_model, event_model,
           headers = c("TWFE", "First Difference", "Event Study FD (t=0)"),
           drop = c("Constant", "_other", "Intercept", "pre", "post"),
           signif.code = NA)
    
  })
}

# Run the app
shinyApp(ui = ui, server = server)
