#####################################################
## ADD STRUCTURAL BIAS ADJUSTMENT CODE HERE

# Define the bias adjustment rate
rate <- 0.001421964 # Structural bias adjustment rate

# Get the rows where forecasts exist (NA for historical data)
forecast_rows <- which(is.na(forecast_df_m$Actual))

# Create a vector of powers (0 for first forecast period, 1 for second, etc.)
powers <- 0:(length(forecast_rows) - 1)

# Initialize the adjusted forecast column in the male forecast dataframe
forecast_df_m$Forecasted_male_adj <- forecast_df_m$Forecasted

# Apply the adjustment formula only to forecasted values
forecast_df_m$Forecasted_male_adj[forecast_rows] <- 
  forecast_df_m$Forecasted[forecast_rows] * (1 + rate)^powers

# Also adjust the CI bounds
forecast_df_m$Lower_CI_adj <- forecast_df_m$Lower_CI
forecast_df_m$Upper_CI_adj <- forecast_df_m$Upper_CI
forecast_df_m$Lower_CI_adj[forecast_rows] <- 
  forecast_df_m$Lower_CI[forecast_rows] * (1 + rate)^powers
forecast_df_m$Upper_CI_adj[forecast_rows] <- 
  forecast_df_m$Upper_CI[forecast_rows] * (1 + rate)^powers

# Create a final forecast dataframe combining adjusted male and female forecasts
forecast_df_final <- left_join(
  forecast_df_m %>% 
    rename(Actual_male = Actual, 
           Forecasted_male = Forecasted,
           Lower_CI_male = Lower_CI,
           Upper_CI_male = Upper_CI),
  forecast_df_f %>% 
    rename(Actual_female = Actual, 
           Forecasted_female = Forecasted,
           Lower_CI_female = Lower_CI,
           Upper_CI_female = Upper_CI),
  by = "Quarter_Year"
)

# Calculate the adjusted total forecast (sum of adjusted male and female forecasts)
forecast_df_final$Actual_total <- 
  ifelse(!is.na(forecast_df_final$Actual_male) & !is.na(forecast_df_final$Actual_female), 
         forecast_df_final$Actual_male + forecast_df_final$Actual_female, 
         NA)

# For forecasted periods, add the adjusted male forecast and female forecast
forecast_df_final$Forecasted_total_adj <- 
  ifelse(is.na(forecast_df_final$Actual_male) & is.na(forecast_df_final$Actual_female),
         forecast_df_final$Forecasted_male_adj + forecast_df_final$Forecasted_female,
         NA)

# Adjust the CI bounds for the total forecast
forecast_df_final$Lower_CI_total_adj <- 
  ifelse(is.na(forecast_df_final$Actual_male) & is.na(forecast_df_final$Actual_female),
         forecast_df_final$Lower_CI_adj + forecast_df_final$Lower_CI_female,
         NA)

forecast_df_final$Upper_CI_total_adj <- 
  ifelse(is.na(forecast_df_final$Actual_male) & is.na(forecast_df_final$Actual_female),
         forecast_df_final$Upper_CI_adj + forecast_df_final$Upper_CI_female,
         NA)

# Create a simplified version of the forecast dataframe for output
forecast_adjusted <- data.frame(
  Quarter_Year = forecast_df_final$Quarter_Year,
  Actual = forecast_df_final$Actual_total,
  Forecasted = forecast_df_final$Forecasted_total_adj,
  Lower_CI = forecast_df_final$Lower_CI_total_adj,
  Upper_CI = forecast_df_final$Upper_CI_total_adj
)

# Create a forecast table from the adjusted forecast for the forecasted periods only
forecast_table_adjusted <- forecast_adjusted %>%
  filter(!is.na(Forecasted)) %>%
  select(Quarter_Year, Forecasted, Lower_CI, Upper_CI) %>%
  rename(Forecasted_hybrid = Forecasted) %>%
  mutate(
    Percent_Change = round((as.numeric(Forecasted_hybrid) - lag(as.numeric(Forecasted_hybrid))) / 
                             lag(as.numeric(Forecasted_hybrid)) * 100, 1)
  )

# Save adjusted forecasts
write.csv(forecast_adjusted, "data/forecast_total_adjusted.csv")
write.csv(forecast_table_adjusted, "data/forecast_table_total_adjusted.csv")
write.csv(forecast_df_m %>% select(Quarter_Year, Actual, Forecasted, Forecasted_male_adj, Lower_CI, Upper_CI, Lower_CI_adj, Upper_CI_adj), 
          "data/forecast_male_adjusted.csv")
write.csv(forecast_df_final, "data/forecast_combined_adjusted.csv")

#####################################################

# Calculate summary metrics, now using adjusted forecasts
# Calculate current metrics
current_total <- tail(prison_data_plot$Inmates[!is.na(prison_data_plot$Inmates)], 1)
current_male <- tail(prison_data_m$Inmates[!is.na(prison_data_m$Inmates)], 1)
current_female <- tail(prison_data_f$Inmates[!is.na(prison_data_f$Inmates)], 1)

# Get the forecasted values (5 years out) - USING ADJUSTED VALUES
projected_total_adj <- tail(forecast_table_adjusted$Forecasted_hybrid, 1)
projected_male_adj <- tail(forecast_df_m$Forecasted_male_adj[forecast_rows], 1)
projected_female <- tail(forecast_table_hybrid_f$Forecasted_hybrid, 1)

# Calculate percent changes - USING ADJUSTED VALUES
pct_change_total_adj <- round(((projected_total_adj - current_total) / current_total) * 100, 1)
pct_change_male_adj <- round(((projected_male_adj - current_male) / current_male) * 100, 1)
pct_change_female <- round(((projected_female - current_female) / current_female) * 100, 1)

# Calculate annual changes - USING ADJUSTED VALUES
annual_change_total_adj <- round(pct_change_total_adj / 5, 1)
annual_change_male_adj <- round(pct_change_male_adj / 5, 1)
annual_change_female <- round(pct_change_female / 5, 1)

# Create summary metrics dataframe - BOTH ORIGINAL AND ADJUSTED VALUES
summary_metrics_adjusted <- data.frame(
  population_type = c("total", "total_adjusted", "male", "male_adjusted", "female"),
  current_pop = c(current_total, current_total, current_male, current_male, current_female),
  projected_pop = c(
    tail(forecast_table_hybrid$Forecasted_hybrid, 1), # Original total
    projected_total_adj, # Adjusted total
    tail(forecast_table_hybrid_m$Forecasted_hybrid, 1), # Original male
    projected_male_adj, # Adjusted male
    projected_female  # Female (unchanged)
  ),
  pct_change = c(
    round(((tail(forecast_table_hybrid$Forecasted_hybrid, 1) - current_total) / current_total) * 100, 1), # Original total
    pct_change_total_adj, # Adjusted total
    round(((tail(forecast_table_hybrid_m$Forecasted_hybrid, 1) - current_male) / current_male) * 100, 1), # Original male
    pct_change_male_adj, # Adjusted male
    pct_change_female  # Female (unchanged)
  ),
  annual_change = c(
    round(round(((tail(forecast_table_hybrid$Forecasted_hybrid, 1) - current_total) / current_total) * 100, 1) / 5, 1), # Original total
    annual_change_total_adj, # Adjusted total
    round(round(((tail(forecast_table_hybrid_m$Forecasted_hybrid, 1) - current_male) / current_male) * 100, 1) / 5, 1), # Original male
    annual_change_male_adj, # Adjusted male
    annual_change_female  # Female (unchanged)
  )
)

# Save to CSV - ADJUSTED METRICS
write.csv(summary_metrics_adjusted, "data/summary_metrics_adjusted.csv")

# Keep the original summary metrics for backward compatibility
summary_metrics <- data.frame(
  population_type = c("total", "male", "female"),
  current_pop = c(current_total, current_male, current_female),
  projected_pop = c(projected_total_adj, projected_male_adj, projected_female), # Use adjusted values here
  pct_change = c(pct_change_total_adj, pct_change_male_adj, pct_change_female), # Use adjusted values here
  annual_change = c(annual_change_total_adj, annual_change_male_adj, annual_change_female) # Use adjusted values here
)

# Save to CSV - ORIGINAL FORMAT BUT WITH ADJUSTED VALUES
write.csv(summary_metrics, "data/summary_metrics.csv")

# Create capacity limits dataframe with three different capacity types
capacity_limits <- data.frame(
  type = rep(c("total", "male", "female"), each = 3),
  capacity_type = rep(c("absolute_max", "emergency", "operational"), 3),
  capacity = c(
    # Total population capacities
    7220, 7076, 6967,
    # Male population capacities
    6588, 6456, 6357,
    # Female population capacities 
    632, 619, 610
  )
)

# Also create the original format capacity dataframe for backward compatibility
capacity_data <- data.frame(
  type = c("total", "male", "female"),
  capacity = c(7220, 6588, 632)  # Using absolute_max as the default capacity
)

# Save capacity limits
write.csv(capacity_limits, "data/capacity_limits.csv", row.names = FALSE)
write.csv(capacity_data, "data/capacity_data.csv", row.names = FALSE)

# Create gender proportion data with adjusted values
# Define date range
all_dates <- unique(c(
  forecast_adjusted$Quarter_Year[!is.na(forecast_adjusted$Actual)],
  forecast_adjusted$Quarter_Year[is.na(forecast_adjusted$Actual)][1:20]  # 5 years of projection
))

# Create proportion data frame
proportion_data <- data.frame(
  Quarter_Year = all_dates,
  Male_Count = NA,
  Female_Count = NA,
  Total_Count = NA,
  Male_Proportion = NA,
  Female_Proportion = NA,
  Type = NA
)

# Last actual date
last_actual_date <- max(forecast_adjusted$Quarter_Year[!is.na(forecast_adjusted$Actual)])

# Fill historical data and projections with adjusted values
for (i in 1:nrow(proportion_data)) {
  date <- proportion_data$Quarter_Year[i]
  
  # Check if historical data exists for this date
  if (date <= last_actual_date) {
    idx <- which(forecast_df_final$Quarter_Year == date)
    if (length(idx) > 0) {
      male_val <- forecast_df_final$Actual_male[idx]
      female_val <- forecast_df_final$Actual_female[idx]
      proportion_data$Type[i] <- "Historical"
    }
  } else {
    # Get forecasted values - using adjusted male forecasts
    idx <- which(forecast_df_final$Quarter_Year == date)
    if (length(idx) > 0) {
      male_val <- forecast_df_final$Forecasted_male_adj[idx]
      female_val <- forecast_df_final$Forecasted_female[idx]
      proportion_data$Type[i] <- "Projected"
    }
  }
  
  if (exists("male_val") && exists("female_val") && length(male_val) > 0 && length(female_val) > 0 && !is.na(male_val) && !is.na(female_val)) {
    proportion_data$Male_Count[i] <- male_val
    proportion_data$Female_Count[i] <- female_val
    proportion_data$Total_Count[i] <- male_val + female_val
    proportion_data$Male_Proportion[i] <- male_val / (male_val + female_val) * 100
    proportion_data$Female_Proportion[i] <- female_val / (male_val + female_val) * 100
  }
}

# Save the adjusted proportion data
write.csv(proportion_data, "data/proportion_data.csv", row.names = FALSE)
write.csv(proportion_data, "data/proportion_data_adjusted.csv", row.names = FALSE)

