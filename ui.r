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

# Load required packages
library(shiny)
library(plotly)

# UI Definition
ui <- fluidPage(
  titlePanel("Gabor's Panel Models Estimation Comparison"),
  
  # Top description panel
  fluidRow(
    column(12,
           div(
             class = "well",
             style = "background-color: #f8f9fa; padding: 15px; border-left: 4px solid #0275d8; margin-bottom: 20px;",
             p(
               "v0.3.2 2025-02-21. In development. Suggestions: contact ",
               tags$a(href = "mailto:bekesg@ceu.edu", "Gabor"),
               " / add an issue to ",
               tags$a(
                 href = "https://github.com/gabors-data-analysis/did-simulation/",
                 target = "_blank",
                 "repo"                )),
             p("This is part of Gabors Data Analysis ecosystem.", tags$a(
               href = "https://gabors-data-analysis.com/chapters/#chapter-23-methods-for-panel-data",
               target = "_blank",
               "Chapter 23 (panel data)") , 
               tags$a(
                 href = "https://gabors-data-analysis.com/chapters/#chapter-24-appropriate-control-groups-for-panel-data",
                 target = "_blank",
                 "Chapter 24 (event studies)")
             ) ,            
             p("When you observe many units over time and want understand the effect of some policy change, you consider TWFE or FD panel models or do an event study design for first differences. This is a toy simulation to illustrate some points."),
             p("This is a panel estimation illustration. Imagine we have 6 countries, A to F, and 13 time periods 2010 to 2022."), 
             p("Outcome (y) is average per capita sales of sugary drinks (ml/week), set at a 1000, b 2000, c 4000, d 5000, e 3000, f 6000. E and F are controls, no intervention. "),
             p("The intervention is a 1 dollar sales tax. The true effect of the tax cuts consumption by -1000 as default."), 
             p("You can set many aspects of the intervention: size, timing, you can even have 2. You can add noise. You can also add dynamics (time to build), and reversal. ")
           )
    )
  ),
  
  # Two graphs side by side
  fluidRow(
    column(6, plotlyOutput("panel_view")),
    column(6, plotlyOutput("did_plot"))
  ),
  
  # Treatment explanation
  fluidRow(
    column(12,
           div(
             style = "background-color: #f0f9ff; padding: 10px; border-left: 4px solid #17a2b8; margin-top: 10px; margin-bottom: 20px;",
             textOutput("treatment_explanation")
           )
    )
  ),
  
  # Settings section - all below the textbox and graphs
  fluidRow(
    column(12,
           tags$hr(),
           h3("Simulation Settings"),
           div(style = "display: flex; flex-wrap: wrap;")
    )
  ),
  
  fluidRow(
    column(3,
           # Main controls
           div(class = "well",
               h4("Treatment Settings"),
               radioButtons("timing", "Treatment Timing:",
                            choices = c("Simultaneous" = "once",
                                        "Staggered" = "staggered")),
               
               radioButtons("effect_size", "Treatment Effect:",
                            choices = c("Uniform" = "uniform",
                                        "Heterogeneous" = "heterogeneous")),
               
               checkboxInput("early_smaller", "Early interventions are smaller", FALSE),
               
               checkboxInput("dynamic_effect", "Dynamic effect build-up", FALSE),
               tags$span(
                 style = "font-size: 0.8em; color: #6c757d; display: block; margin-top: -10px; margin-bottom: 10px;",
                 "When enabled, treatment effect builds up gradually according to percentages."
               ),
               
               checkboxInput("reversal", "Treatment reversal", FALSE),
               tags$span(
                 style = "font-size: 0.8em; color: #6c757d; display: block; margin-top: -10px; margin-bottom: 10px;",
                 "When enabled, treatment effect disappears after specified years."
               ),
               h4("Other Factors (trends)"),
               
               checkboxInput("global_trend", "Include global trend", FALSE),
               checkboxInput("individual_trend", "Include individual trends", FALSE)
               
               )
    ),
    
    
    column(3,
           # Treatment complexity settings
           div(class = "well",
               h4("Treatment Complexity"),
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
               
               sliderInput("noise_sd", "Noise SD:", 
                           min = 0, max = 1000, value = 0, step = 50))
    ),
  
    column(3,
           # Model settings
           div(class = "well",
               h4("Model Settings"),
               checkboxInput("year_fe", "Include Year Fixed Effects", FALSE),
               checkboxInput("country_fe_fd", "Add Country FE to First Difference Model", FALSE),
               
               # Event Study Controls
               h4("Event Study Settings"),
               sliderInput("min_event_time", "Minimum Event Time:",
                           min = -10, max = -1, value = -3, step = 1),
               sliderInput("max_event_time", "Maximum Event Time:",
                           min = 1, max = 10, value = 3, step = 1))
    ),
    
    
      
    column(3,
           # Advanced settings
           div(class = "well",
               h4("Advanced Settings"),
               actionButton("show_advanced", "Show/Hide Advanced Settings"),
               
               # Advanced settings panel
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
                           "100,200,300,400,0,0"),
                 
                 # New advanced settings
                 textInput("dynamic_effect_values", "Dynamic effect progression (%):", 
                           "50,75,100"),
                 numericInput("years_to_reversal", "Years until reversal:", 3, min = 1, max = 10)
               ))
    )
  ),
  
  # Regression results
  
  # Regression results
  fluidRow(
    column(12,
           tags$hr(),
           div(
             style = "background-color: #f8f9fa; padding: 15px; border-left: 4px solid #0275d8; margin-bottom: 20px;",
             p(style = "margin: 0;", "We have three models: FE, FD, and event study. FE do not have time dummies as default. The event study recenters the intervention when it's a single one. 
             For multiple ones, does not display. Models estimated in R with feols.")
           ),
           verbatimTextOutput("model_results"),
           div(
             style = "background-color: #f0f9ff; padding: 15px; border-left: 4px solid #17a2b8; margin-bottom: 20px;",
             textOutput("model_explanation")
           ),
           downloadButton("downloadData", "Download Data")
    )
  ),
  
  # TWFE Transformation Section - no button needed
  fluidRow(
    column(12,
           tags$hr(),
           tags$h3("Illustrating TWFE: Removing Fixed Effects"),
           tags$p("This section demonstrates how the TWFE model works step by step by removing country and year fixed effects."),
           plotlyOutput("twfe_plot"),
           textOutput("twfe_explanation")
    )
  ),
  
  # Event Study Transformation Section - no button needed
  
  
  fluidRow(
    column(12,
           tags$hr(),
           tags$h3("Event Study Design: Data Transformation"),
           tags$p("This section illustrates how event study analysis transforms the data to estimate dynamic treatment effects."),
           
           # Only show event study for single intervention
           conditionalPanel(
             condition = "input.num_shocks == '1'",
             div(style = "margin: 20px 0;"),
             plotlyOutput("event_study_plot"),
             div(style = "margin: 20px 0;"),
             plotlyOutput("event_coef_plot"),
             textOutput("event_study_explanation")
           ),
           
           # Warning message for multiple interventions
           conditionalPanel(
             condition = "input.num_shocks != '1'",
             div(
               style = "background-color: #fff3cd; padding: 15px; border-left: 4px solid #ffc107; margin: 20px 0;",
               p("Event study visualization is only available for single intervention scenarios.")
             )
           ),
           
           textOutput("warning_message"),
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