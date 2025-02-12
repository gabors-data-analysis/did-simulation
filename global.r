# Load required packages
library(shiny)
library(tidyverse)
library(fixest)
library(plotly)
library(broom)
library(gridExtra)



# Data generation function
generate_data <- function(input) {
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
  
  data <- data %>%
    mutate(
      cohort = case_when(
        country == "A" ~ treat_timing[1],
        country == "B" ~ treat_timing[2],
        country == "C" ~ treat_timing[3],
        country == "D" ~ treat_timing[4],
        TRUE ~ Inf
      ),
      second_cohort = case_when(
        input$num_shocks == "1" ~ Inf,
        country == "A" ~ second_treat_timing[1],
        country == "B" ~ second_treat_timing[2],
        country == "C" ~ second_treat_timing[3],
        country == "D" ~ second_treat_timing[4],
        TRUE ~ Inf
      ),
      relative_time = year - cohort,
      post = !is.infinite(cohort) & year >= cohort,
      post_second = !is.infinite(second_cohort) & year >= second_cohort,
      
      treatment = case_when(
        !treated ~ 0,
        treated & !post ~ 0,
        treated & post & (input$num_shocks == "1") ~ 1,
        treated & post & !post_second ~ 1,
        treated & post & post_second ~ 2,
        TRUE ~ 0
      ),
      
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
}

# TWFE transformation function
run_twfe_transform <- function(data) {
  # Step 1: Raw Data (Baseline)
  raw_data <- data %>%
    mutate(transformation = "1. Raw Data")
  
  # Step 2: Remove Country FE
  country_means <- data %>%
    group_by(country) %>%
    summarize(country_avg = mean(value), .groups = 'drop')
  
  country_adjusted <- data %>%
    left_join(country_means, by = "country") %>%
    mutate(value = value - country_avg,
           transformation = "2. Country FE Removed")
  
  # Step 3: Remove Country & Year FE
  year_means <- country_adjusted %>%
    group_by(year) %>%
    summarize(year_avg = mean(value), .groups = 'drop')
  
  final_data <- country_adjusted %>%
    left_join(year_means, by = "year") %>%
    mutate(value = value - year_avg,
           transformation = "3. Country & Year FE Removed")
  
  # Combine all steps
  twfe_data <- bind_rows(raw_data, country_adjusted, final_data) %>%
    mutate(transformation = factor(transformation, 
                                   levels = c("1. Raw Data", 
                                              "2. Country FE Removed", 
                                              "3. Country & Year FE Removed")))
  
  return(twfe_data)
}

# Create base plot
create_did_plot <- function(data) {
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
  return(p)
}

# Run regression models
run_models <- function(data, input) {
  # First difference data preparation
  fd_data <- data %>% 
    group_by(country) %>%
    mutate(
      value_diff = value - lag(value),
      treatment_diff = treatment - lag(treatment)
    ) %>% 
    filter(!is.na(value_diff))
  
  # Event study data preparation
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
    filter(!is.na(value_diff))
  
  # Model specifications
  if(input$year_fe) {
    twfe_model <- feols(value ~ treatment | country + year, 
                        data = data,
                        cluster = "country")
    
    if(input$country_fe_fd) {
      fd_model <- feols(value_diff ~ treatment_diff | country + year, 
                        data = fd_data,
                        cluster = "country")
    } else {
      fd_model <- feols(value_diff ~ treatment_diff | year, 
                        data = fd_data,
                        cluster = "country")
    }
  } else {
    twfe_model <- feols(value ~ treatment | country, 
                        data = data,
                        cluster = "country")
    
    if(input$country_fe_fd) {
      fd_model <- feols(value_diff ~ treatment_diff | country, 
                        data = fd_data,
                        cluster = "country")
    } else {
      fd_model <- feols(value_diff ~ treatment_diff, 
                        data = fd_data,
                        cluster = "country")
    }
  }
  
  # Event study model
  if(input$num_shocks == "1") {
    event_model <- feols(value_diff ~ i(rel_year, ref = "-1"), 
                         cluster = "country",
                         data = data_event)
  } else {
    event_model <- feols(value_diff ~ 1, 
                         cluster = "country",
                         data = data_event)
  }
  
  return(list(
    twfe = twfe_model,
    fd = fd_model,
    event = event_model
  ))
}


# Transform data for event study visualization
# Transform data for event study visualization
transform_event_study_data <- function(data, min_time, max_time) {
  # Add basic indicators
  base_data <- data %>%
    mutate(
      treated_group = !is.infinite(cohort),
      relative_time = year - cohort
    )
  
  # 1. Original data panel - keep exactly as in main plot
  p1_data <- base_data %>%
    mutate(
      panel = "1. Original Time Series",
      vline = cohort  # Store treatment timing for vertical lines
    )
  
  # 2. Event time panel - only treated units
  p2_data <- base_data %>%
    filter(
      treated_group,  # only treated units
      relative_time >= min_time,
      relative_time <= max_time
    ) %>%
    mutate(
      panel = "2. Event Time",
      vline = 0  # Store zero for vertical line
    )
  
  # 3. Group averages by relative time
  p3_data <- base_data %>%
    filter(relative_time >= min_time, relative_time <= max_time) %>%
    group_by(relative_time, treated_group) %>%
    summarize(
      value = mean(value),
      .groups = 'drop'
    ) %>%
    mutate(
      panel = "3. Group Averages",
      country = if_else(treated_group, "Treated", "Control"),  # for legend
      vline = 0  # Store zero for vertical line
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
      "3. Group Averages"
    )))
  
  return(plot_data)
}

# Create event study plot
create_event_study_plot <- function(data) {
  # Create vertical line data
  vlines <- bind_rows(
    # Treatment timing for panel 1
    data %>% 
      filter(panel == "1. Original Time Series", !is.infinite(cohort)) %>%
      distinct(panel, cohort) %>%
      rename(xint = cohort),
    # Zero line for panels 2 & 3
    data %>%
      filter(panel != "1. Original Time Series") %>%
      distinct(panel) %>%
      mutate(xint = 0)
  )
  
  # Base plot
  p <- ggplot() +
    # Panel 1: Original time series
    geom_line(data = filter(data, panel == "1. Original Time Series"),
              aes(x = year, y = value, color = country, group = country)) +
    geom_point(data = filter(data, panel == "1. Original Time Series"),
               aes(x = year, y = value, color = country)) +
    
    # Panel 2: Event time data
    geom_line(data = filter(data, panel == "2. Event Time"),
              aes(x = relative_time, y = value, color = country, group = country)) +
    geom_point(data = filter(data, panel == "2. Event Time"),
               aes(x = relative_time, y = value, color = country)) +
    
    # Panel 3: Group averages
    geom_line(data = filter(data, panel == "3. Group Averages"),
              aes(x = relative_time, y = value, color = country, group = country),
              size = 1) +
    geom_point(data = filter(data, panel == "3. Group Averages"),
               aes(x = relative_time, y = value, color = country),
               size = 2) +
    
    # Add vertical lines using consistent data-driven approach
    geom_vline(data = vlines,
               aes(xintercept = xint),
               linetype = "dashed") +
    
    # Faceting and theme
    facet_wrap(~panel, scales = "free_x", ncol = 3) +
    theme_minimal() +
    theme(
      strip.text = element_text(size = 11, face = "bold"),
      legend.position = "bottom",
      panel.grid.major = element_line(color = "grey90"),
      panel.grid.minor = element_blank(),
      axis.text = element_text(size = 9),
      axis.title = element_text(size = 10)
    ) +
    scale_y_continuous(breaks = seq(-1000, max(data$value) + 1000, by = 1000), 
                       expand = c(0, 0),
                       limits = c(-1000, max(data$value) + 1000)) +
    labs(x = "Time", y = "Value", color = "Group")
  
  return(p)
}
# Updated run_models function
run_models <- function(data, input) {
  # First difference data preparation
  fd_data <- data %>% 
    group_by(country) %>%
    mutate(
      value_diff = value - lag(value),
      treatment_diff = treatment - lag(treatment)
    ) %>% 
    filter(!is.na(value_diff))
  
  # Event study data preparation with dynamic min/max time
  data_event <- data %>%
    group_by(country) %>%
    mutate(
      value_diff = value - lag(value),
      rel_year = factor(
        case_when(
          relative_time < input$min_event_time ~ paste("<", input$min_event_time),
          relative_time > input$max_event_time ~ paste(">", input$max_event_time),
          TRUE ~ as.character(relative_time)
        ),
        levels = c(
          paste("<", input$min_event_time),
          as.character(input$min_event_time:input$max_event_time),
          paste(">", input$max_event_time)
        )
      )
    ) %>%
    filter(!is.infinite(cohort) | is.infinite(relative_time)) %>%
    filter(!is.na(value_diff))
  
  # [Rest of the function remains the same]
  
  # Model specifications
  if(input$year_fe) {
    twfe_model <- feols(value ~ treatment | country + year, 
                        data = data,
                        cluster = "country")
    
    if(input$country_fe_fd) {
      fd_model <- feols(value_diff ~ treatment_diff | country + year, 
                        data = fd_data,
                        cluster = "country")
    } else {
      fd_model <- feols(value_diff ~ treatment_diff | year, 
                        data = fd_data,
                        cluster = "country")
    }
  } else {
    twfe_model <- feols(value ~ treatment | country, 
                        data = data,
                        cluster = "country")
    
    if(input$country_fe_fd) {
      fd_model <- feols(value_diff ~ treatment_diff | country, 
                        data = fd_data,
                        cluster = "country")
    } else {
      fd_model <- feols(value_diff ~ treatment_diff, 
                        data = fd_data,
                        cluster = "country")
    }
  }
  
  # Event study model
  if(input$num_shocks == "1") {
    event_model <- feols(value_diff ~ i(rel_year, ref = "-1"), 
                         cluster = "country",
                         data = data_event)
  } else {
    event_model <- feols(value_diff ~ 1, 
                         cluster = "country",
                         data = data_event)
  }
  
  return(list(
    twfe = twfe_model,
    fd = fd_model,
    event = event_model
  ))
}