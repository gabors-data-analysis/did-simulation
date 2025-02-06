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
      
      actionButton("show_advanced", "Show Advanced Settings"),
      
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
      textOutput("warning_message"),
      
      # Fixed Effects Visualization Below
      hr(),
      h3("Understanding Fixed Effects"),
      checkboxInput("apply_country_fe", "Apply Country Fixed Effects", FALSE),
      checkboxInput("apply_year_fe", "Apply Year Fixed Effects", FALSE),
      actionButton("reset_fe", "Reset"),
      helpText("Toggle the checkboxes to see how Fixed Effects adjust the data."),
      plotlyOutput("fe_plot"),
      helpText("\nCountry FE removes baseline differences across countries. \nYear FE removes common trends. \nTWFE applies both.")
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
  
  generate_data <- reactive({
    years <- 2010:2020
    countries <- LETTERS[1:6]
    base_values <- c(input$base_a, input$base_b, input$base_c, 
                     input$base_d, input$base_e, input$base_f)
    
    # Define treatment timing
    if (input$timing == "once") {
      treat_timing <- rep(2015, 4)
    } else {
      treat_timing <- c(2013, 2014, 2016, 2017)
    }
    first_treat <- min(treat_timing)
    
    # Define treatment effects
    if (input$effect_size == "uniform") {
      effects <- rep(input$uniform_effect, 4)
    } else {
      effects <- as.numeric(strsplit(input$hetero_effects, ",")[[1]])
      if (input$early_smaller) {
        effects <- sort(effects)
      }
    }
    
    data <- expand.grid(year = years, country = countries) %>%
      arrange(country, year) %>%
      mutate(
        base_value = rep(base_values, each = length(years)),
        value = base_value,
        treated = country %in% LETTERS[1:4],
        post = case_when(
          country == "A" ~ year >= treat_timing[1],
          country == "B" ~ year >= treat_timing[2],
          country == "C" ~ year >= treat_timing[3],
          country == "D" ~ year >= treat_timing[4],
          TRUE ~ FALSE
        ),
        effect = ifelse(treated & post, effects[match(country, LETTERS[1:4])], 0),
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
}

shinyApp(ui = ui, server = server)
