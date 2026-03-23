# Transform data for event study visualization
transform_event_study_data <- function(data, min_time, max_time) {
  # Get treatment timing for controls (use first treated unit's timing)
  first_treatment <- min(data$cohort[!is.infinite(data$cohort)])
  
  # Add basic indicators
  base_data <- data %>%
    mutate(
      treated_group = !is.infinite(cohort),
      # For controls, calculate relative time based on first treatment
      relative_time = if_else(is.infinite(cohort),
                              year - first_treatment,
                              year - cohort)
    )
  
  # 1. Original data panel - keep all countries
  p1_data <- base_data %>%
    mutate(
      panel = "1. Original Time Series",
      vline = if_else(!is.infinite(cohort), cohort, NA_real_)
    )
  
  # 2. Event time panel - all countries
  p2_data <- base_data %>%
    filter(relative_time >= min_time,
           relative_time <= max_time) %>%
    mutate(
      panel = "2. Event Time",
      vline = 0
    )
  
  # 3. Group averages only - with normalization at t-1
  p3_data <- base_data %>%
    filter(relative_time >= min_time, relative_time <= max_time) %>%
    group_by(relative_time, treated_group) %>%
    summarize(
      value = mean(value),
      .groups = 'drop'
    ) %>%
    # Add a group identifier for the next step
    mutate(
      panel = "3. Group Averages",
      country = if_else(treated_group, "Treated Average", "Control Average"),
      vline = 0
    )
  
  # Normalize values at t-1 for panel 3
  # First, get the values at t-1 for each group
  baseline_values <- p3_data %>%
    filter(relative_time == -1) %>%
    select(treated_group, baseline_value = value)
  
  # Join and normalize
  p3_data <- p3_data %>%
    left_join(baseline_values, by = "treated_group") %>%
    mutate(
      value = value - baseline_value,  # Normalize by subtracting t-1 value
      panel = "3. Group Averages (Normalized)"  # Update panel name to indicate normalization
    )
  
  # Combine all data
  plot_data <- bind_rows(
    p1_data,
    p2_data,
    p3_data
  ) %>%
    mutate(panel = factor(panel, levels = c(
      "1. Original Time Series",
      "2. Event Time",
      "3. Group Averages (Normalized)"
    )))
  
  return(plot_data)
}
