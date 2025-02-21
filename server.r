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
  
  # Validate dynamic effect values
  observe({
    if(input$dynamic_effect) {
      # Validate dynamic effect values
      dyn_values <- as.numeric(strsplit(input$dynamic_effect_values, ",")[[1]])
      
      if(length(dyn_values) != 3) {
        showModal(modalDialog(
          title = "Invalid Dynamic Effect Values",
          "Please provide exactly 3 comma-separated percentages for dynamic effect progression.",
          easyClose = TRUE
        ))
        updateTextInput(session, "dynamic_effect_values", value = "50,75,100")
      } else if(any(is.na(dyn_values)) || any(dyn_values < 0) || any(dyn_values > 100)) {
        showModal(modalDialog(
          title = "Invalid Dynamic Effect Values",
          "All percentages must be between 0 and 100.",
          easyClose = TRUE
        ))
        updateTextInput(session, "dynamic_effect_values", value = "50,75,100")
      } else if(dyn_values[1] > dyn_values[2] || dyn_values[2] > dyn_values[3]) {
        showModal(modalDialog(
          title = "Invalid Dynamic Effect Values",
          "Percentages should be in ascending order.",
          easyClose = TRUE
        ))
        updateTextInput(session, "dynamic_effect_values", value = "50,75,100")
      }
    }
  })
  
  # Validate years to reversal
  observe({
    if(input$reversal) {
      if(input$years_to_reversal < 1) {
        showModal(modalDialog(
          title = "Invalid Reversal Timing",
          "Years until reversal must be at least 1.",
          easyClose = TRUE
        ))
        updateNumericInput(session, "years_to_reversal", value = 3)
      }
    }
  })
  
  # Generate reactive dataset
  data <- reactive({
    generate_data(input)
  })
  
  # TWFE transformation reactive - now automatically runs without a button
  twfe_data <- reactive({
    run_twfe_transform(data())
  })
  
  # Event study transformation reactive - now automatically runs without a button
  event_study_data <- reactive({
    # Only run for single intervention
    if(input$num_shocks == "1") {
      transform_event_study_data(
        data(), 
        input$min_event_time, 
        input$max_event_time
      )
    } else {
      return(NULL)
    }
  })
  
  # Main plot output
  output$did_plot <- renderPlotly({
    p <- create_did_plot(data())
    ggplotly(p) %>%
      layout(legend = list(orientation = "h", y = -0.2))
  })
  
  # NEW: PanelView plot output
  output$panel_view <- renderPlotly({
    p <- create_panel_view(data())
    ggplotly(p) %>%
      layout(legend = list(orientation = "h", y = -0.2))
  })
  
  # Treatment explanation output
  output$treatment_explanation <- renderText({
    if(input$dynamic_effect && input$reversal) {
      paste("Treatment: Dynamic build-up over time, then reversal after", 
            input$years_to_reversal, "years.")
    } else if(input$dynamic_effect) {
      dyn_values <- strsplit(input$dynamic_effect_values, ",")[[1]]
      paste("Treatment: Dynamic build-up over time. Year 1:", 
            dyn_values[1], "%, Year 2:", 
            dyn_values[2], "%, Year 3+:", 
            dyn_values[3], "%")
    } else if(input$reversal) {
      paste("Treatment: Full effect immediately, then reversal after", 
            input$years_to_reversal, "years.")
    } else {
      "Treatment: Full effect immediately after implementation."
    }
  })
  
  # Model results output
  output$model_results <- renderPrint({
    models <- run_models(data(), input)
    
    if(input$num_shocks == "1") {
      etable(models$twfe, models$fd, models$event,
             headers = c("FE", "First Difference", "Event Study FD"),
             drop = c("Constant", "Intercept"), # Keep lag coefficients
             signif.code = NA)
    } else {
      etable(models$twfe, models$fd,
             headers = c("FE", "First Difference"),
             drop = c("Constant", "Intercept"),
             signif.code = NA)
    }
  })
  
  # TWFE plot output - now runs automatically
  output$twfe_plot <- renderPlotly({
    req(twfe_data())
    
    p <- ggplot(twfe_data(), aes(x = year, y = value, color = country)) +
      geom_line() +
      geom_point() +
      facet_wrap(~transformation, nrow = 1) +
      theme_minimal() +
      theme(legend.position = "bottom") +
      labs(title = "TWFE Transformation: Step-by-Step Fixed Effects Removal",
           x = "Year", y = "Adjusted Outcome")
    
    ggplotly(p) %>%
      layout(legend = list(orientation = "h", y = -0.2))
  })
  
  # TWFE explanation output
  output$twfe_explanation <- renderText({
    paste(
      "1. Raw Data: Observed sales by country over time.",
      "2. Country FE Removed: Adjusted for country-level differences, showing within-country variation.",
      "3. Country & Year FE Removed: Adjusted for both country and year effects, isolating within-country, within-year variation.",
      sep = "\n"
    )
  })
  
  # Event Study plot output - now runs automatically for single intervention scenarios
  output$event_study_plot <- renderPlotly({
    req(event_study_data())
    p <- create_event_study_plot(event_study_data())
    ggplotly(p, height = 400) |>  # Set consistent height
      layout(
        margin = list(l = 50, r = 50, b = 80, t = 50),  # Adjust bottom margin for legend
        legend = list(orientation = "h", y = -0.2),
        showlegend = TRUE
      )
  })
  
  # Event Study explanation output
  output$event_study_explanation <- renderText({
    req(input$num_shocks == "1")  # Only show for single intervention
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
