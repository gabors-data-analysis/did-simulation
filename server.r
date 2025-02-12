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
}