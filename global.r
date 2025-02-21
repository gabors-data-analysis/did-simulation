# Install and load required packages

options(repos = c(CRAN = "https://cloud.r-project.org"))

if (!require(tidyverse)) install.packages("tidyverse")
if (!require(fixest)) install.packages("fixest") 
if (!require(plotly)) install.packages("plotly")
if (!require(broom)) install.packages("broom")
if (!require(shiny)) install.packages("shiny")
if (!require(gridExtra)) install.packages("gridExtra")

# Load required packages
library(shiny)
library(tidyverse)
library(fixest)
library(plotly)
library(broom)
library(gridExtra)



#########################################
# Data generation function
#########################################

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


#########################################
# Enhanced TWFE transformation function
#########################################

run_twfe_transform <- function(data) {
  # Step 1: Raw Data (Baseline)
  raw_data <- data %>%
    mutate(transformation = "1. Raw Data")
  
  # Step 2: Calculate and Remove Unit (Country) FE
  unit_fe <- data %>%
    group_by(country) %>%
    summarize(
      unit_fe = mean(value),
      pre_treatment_mean = mean(value[treatment == 0]),
      post_treatment_mean = mean(value[treatment > 0]),
      treatment_effect = post_treatment_mean - pre_treatment_mean,
      .groups = 'drop'
    )
  
  unit_adjusted <- data %>%
    left_join(unit_fe, by = "country") %>%
    mutate(
      value_unit_adjusted = value - unit_fe,
      transformation = "2. Unit FE Removed"
    )
  
  # Step 3: Calculate and Remove Time FE
  time_fe <- unit_adjusted %>%
    group_by(year) %>%
    summarize(
      time_fe = mean(value_unit_adjusted),
      treated_mean = mean(value_unit_adjusted[treatment > 0]),
      control_mean = mean(value_unit_adjusted[treatment == 0]),
      .groups = 'drop'
    )
  
  final_data <- unit_adjusted %>%
    left_join(time_fe, by = "year") %>%
    mutate(
      value_final = value_unit_adjusted - time_fe,
      transformation = "3. Unit & Time FE Removed"
    )
  
  # Step 4: Calculate Treatment Effect Components
  treatment_components <- final_data %>%
    filter(treatment > 0) %>%
    group_by(country) %>%
    summarize(
      avg_treatment_effect = mean(value_final),
      timing_component = mean(value_final) - mean(value_final[year == min(year[treatment > 0])]),
      heterogeneity_component = mean(value_final) - mean(value_final[treatment == 1]),
      .groups = 'drop'
    )
  
  # Combine all steps with components
  twfe_data <- bind_rows(
    raw_data %>% select(year, country, value, transformation),
    unit_adjusted %>% select(year, country, value = value_unit_adjusted, transformation),
    final_data %>% select(year, country, value = value_final, transformation)
  ) %>%
    left_join(unit_fe, by = "country") %>%
    left_join(time_fe, by = "year") %>%
    left_join(treatment_components, by = "country") %>%
    mutate(
      transformation = factor(transformation,
                              levels = c("1. Raw Data",
                                         "2. Unit FE Removed",
                                         "3. Unit & Time FE Removed"))
    )
  
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
    theme(
      panel.grid.major = element_line(color = "grey85"),
      legend.position = "bottom"  # Move legend to bottom
    ) +
    scale_x_continuous(breaks = 2010:2022) +
    scale_y_continuous(breaks = seq(-1000, max(data$value) + 1000, by = 1000), 
                       expand = c(0, 0),
                       limits = c(-1000, max(data$value) + 1000)) +
    labs(title = "Treatment Effects Over Time", x = "Year", y = "Sales of Sugary Drinks")
  return(p)
}

#########################################
# Run regression models
#########################################

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

####################################################################
# Event Study Transformation Functions
####################################################################


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
  
  # 3. Group averages only
  p3_data <- base_data %>%
    filter(relative_time >= min_time, relative_time <= max_time) %>%
    group_by(relative_time, treated_group) %>%
    summarize(
      value = mean(value),
      .groups = 'drop'
    ) %>%
    mutate(
      panel = "3. Group Averages",
      country = if_else(treated_group, "Treated Average", "Control Average"),
      vline = 0
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
  vlines <- data %>%
    filter(!is.infinite(vline)) %>%
    distinct(panel, vline) %>%
    rename(xint = vline)
  
  # Get axis breaks
  year_breaks <- sort(unique(filter(data, panel == "1. Original Time Series")$year))
  relative_breaks <- sort(unique(filter(data, panel != "1. Original Time Series")$relative_time))
  
  # Base plot
  p <- ggplot() +
    # Panel 1: Original time series with all countries
    geom_line(data = filter(data, panel == "1. Original Time Series"),
              aes(x = year, y = value, color = country, group = country)) +
    geom_point(data = filter(data, panel == "1. Original Time Series"),
               aes(x = year, y = value, color = country)) +
    
    # Panel 2: Event time data with all countries
    geom_line(data = filter(data, panel == "2. Event Time"),
              aes(x = relative_time, y = value, color = country, group = country)) +
    geom_point(data = filter(data, panel == "2. Event Time"),
               aes(x = relative_time, y = value, color = country)) +
    
    # Panel 3: Group averages
    geom_line(data = filter(data, panel == "3. Group Averages"),
              aes(x = relative_time, y = value, color = country, group = country),
              size = 1.5) +
    geom_point(data = filter(data, panel == "3. Group Averages"),
               aes(x = relative_time, y = value, color = country),
               size = 3) +
    
    # Add vertical lines
    geom_vline(data = vlines,
               aes(xintercept = xint),
               linetype = "dashed") +
    
    # Faceting and theme
    facet_wrap(~panel, scales = "free_x", ncol = 3) +
    theme_minimal() +
    theme(
      strip.text = element_text(size = 11, face = "bold"),
      legend.position = "bottom",  # Legend at bottom
      panel.grid.major = element_line(color = "grey90"),
      panel.grid.minor = element_blank(),
      axis.text = element_text(size = 9),
      axis.title = element_text(size = 10)
    ) +
    scale_y_continuous(breaks = seq(-1000, max(data$value) + 1000, by = 1000), 
                       expand = c(0, 0),
                       limits = c(-1000, max(data$value) + 1000)) +
    # Custom x-axis labels for each panel
    scale_x_continuous(breaks = function(x) {
      if(x[2] > 2000) {  # Panel 1 (years)
        year_breaks
      } else {  # Panels 2 & 3 (relative time)
        relative_breaks
      }
    },
    labels = function(x) {
      if(x[1] > 2000) {  # Panel 1 (years)
        paste0("'", substr(as.character(x), 3, 4))
      } else {  # Panels 2 & 3 (relative time)
        x
      }
    }) +
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

# Create PanelView heatmap function
create_panel_view <- function(data) {
  # Create a simplified dataset for the heatmap
  panel_data <- data %>%
    select(year, country, treatment) %>%
    mutate(
      treatment_status = case_when(
        treatment == 0 ~ "No treatment",
        treatment == 1 ~ "Treated once",
        treatment == 2 ~ "Treated twice",
        TRUE ~ NA_character_
      ),
      treatment_status = factor(treatment_status, 
                                levels = c("No treatment", "Treated once", "Treated twice"))
    )
  
  # Create a heatmap using ggplot2
  p <- ggplot(panel_data, aes(x = year, y = country, fill = treatment_status)) +
    geom_tile(color = "white", size = 0.5) +
    scale_fill_manual(values = c("No treatment" = "#f0f8ff", 
                                 "Treated once" = "#6495ed", 
                                 "Treated twice" = "#000080")) +
    theme_minimal() +
    theme(panel.grid = element_blank(),
          axis.text.x = element_text(angle = 0),
          legend.position = "bottom") +
    scale_x_continuous(breaks = 2010:2022) +
    labs(title = "Treatment Status by Country and Year",
         x = "Year", y = "Country", fill = "Treatment Status")
  
  return(p)
}