# Server logic
function(input, output, session) {
  
  # Warning for year fixed effects
  observe({
    if (input$year_fe & input$timing == "once" & !input$global_trend & !input$individual_trend) {
      showModal(modalDialog(
        title = "Warning",
        "For uniformly timed single intervention and no global or individual trends, time dummies can not be added.",
        easyClose = TRUE
      ))
    }
  })
  
  # Validate event time window
  observe({
    if (input$min_event_time >= -1) {
      updateSliderInput(session, "min_event_time", value = -2)
      showModal(modalDialog(
        title = "Invalid Event Window",
        "Minimum event time must be less than -1 (the reference period)",
        easyClose = TRUE
      ))
    }
    if (input$max_event_time <= 1) {
      updateSliderInput(session, "max_event_time", value = 2)
      showModal(modalDialog(
        title = "Invalid Event Window",
        "Maximum event time must be greater than 1",
        easyClose = TRUE
      ))
    }
  })
  
  # Generate reactive dataset
  data <- reactive({
    generate_data(input)
  })
  
  # TWFE transformation reactive
  twfe_data <- reactive({
    req(input$run_twfe)
    isolate({
      run_twfe_transform(data())
    })
  })
  
  # Event study transformation reactive
  event_study_data <- reactive({
    req(input$run_event_study)
    req(input$num_shocks == "1")  # Only run for single intervention
    isolate({
      transform_event_study_data(
        data(), 
        input$min_event_time, 
        input$max_event_time
      )
    })
  })
  
  # Main plot output
  output$did_plot <- renderPlotly({
    p <- create_did_plot(data())
    ggplotly(p)
  })
  
  # Model results output
  output$model_results <- renderPrint({
    models <- run_models(data(), input)
    
    etable(models$twfe, models$fd, models$event,
           headers = c("FE", "First Difference", "Event Study FD (t=0)"),
           drop = c("Constant", "rel_year = -3", "rel_year = -2", "rel_year = -1",  "rel_year = 1", 
                    "rel_year = 2", "rel_year = 3", "pre", "post", "Intercept"),
           signif.code = NA)
  })
  
  # TWFE plot output
  output$twfe_plot <- renderPlotly({
    req(twfe_data())
    
    p <- ggplot(twfe_data(), aes(x = year, y = value, color = country)) +
      geom_line() +
      geom_point() +
      facet_wrap(~transformation, nrow = 1) +
      theme_minimal() +
      labs(title = "TWFE Transformation: Step-by-Step Fixed Effects Removal",
           x = "Year", y = "Adjusted Outcome")
    
    ggplotly(p)
  })
  
  # TWFE explanation output
  output$twfe_explanation <- renderText({
    req(input$run_twfe)
    paste(
      "1. Raw Data: Observed sales by country over time.",
      "2. Country FE Removed: Adjusted for country-level differences, showing within-country variation.",
      "3. Country & Year FE Removed: Adjusted for both country and year effects, isolating within-country, within-year variation.",
      sep = "\n"
    )
  })
  
  # Event Study plot output
  output$event_study_plot <- renderPlotly({
    req(event_study_data())
    p <- create_event_study_plot(event_study_data())
    ggplotly(p, height = 400) |>  # Set consistent height
      layout(
        margin = list(l = 50, r = 50, b = 50, t = 50),  # Adjust margins
        showlegend = TRUE
      )
  })
  
  # Event Study explanation output
  output$event_study_explanation <- renderText({
    req(input$run_event_study)
    paste(
      "1. Original Data: Shows the raw time series with vertical lines marking treatment timing.",
      "2. Recentered Around Event: Shows the same data with time recentered around the treatment (time 0).",
      "3. Treatment vs Control: Shows the difference between treated and control group averages over event time.",
      sep = "\n"
    )
  })
  
  # Download handler
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("panel_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(data(), file, row.names = FALSE)
    }
  )
}