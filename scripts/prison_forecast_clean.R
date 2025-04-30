## PRE-PROCESS CLEANING TO IMPORT FOR DASHBOARD
library(knitr)
library(readxl)
library(dplyr)
library(lubridate)
library(hts)
library(fable)
library(fabletools)
library(tsibble)
library(ggplot2)
library(data.table)
library(plotly)
library(scales)
library(DT)
library(bsts)
library(forecast)
library(lubridate)
library(feasts)
library(tsibbledata)
library(tidyr)
library(kableExtra)

## TOTAL POPULATION
# read in general pop data
df_pop = read.csv("data/utah_population_data.csv")

# Expand data to quarterly level
df_quarterly = df_pop[rep(1:nrow(df_pop), each = 4), ]
df_quarterly$quarter <- rep(1:4, times = nrow(df_pop))

# Adjust population for quarterly distribution (assume same quarterly values by year)
df_quarterly$pop_quarterly <- df_quarterly$population_18_65 / 4

# Create Year_Quarter variable
df_quarterly$Quarter_Year = paste0(df_quarterly$year, "-Q", df_quarterly$quarter)

# Select relevant columns
df_quarterly <- df_quarterly[, c("Quarter_Year", "pop_quarterly")]

# Print the result
#print(df_quarterly)

# read in prison data
prison1 = read_excel("data/PPP 2025 Data_Feb_25.xlsx", sheet = "Quarterly Population Summary")
#head(prison1)

# select only x first columns 
prison_df = prison1[, c(1:5)]
#prison_df

# rename select variables
prison_df = prison_df %>% rename(Quarter_Year = "Month/Year", gender = Sex, top_ofnse_typ_desc = `Primary Offense Type`, admin_type = `Prison Entry Group`, count = `Avg. Daily Population`)

# ensure all data points have full data (meaning complete quarters):
prison_df = prison_df %>% filter(Quarter_Year != "2025 Q1")
#prison_df = prison_df %>% filter(admin_type != "Other") # any final forecast should never exclude groups

# Group/clean variables ##

# rename offense_type to "UNKNOWN" if missing values
prison_df$top_ofnse_typ_desc[is.na(prison_df$top_ofnse_typ_desc)] = "OTHER"

# group certain offenses (that have small n's)
prison_df$top_ofnse_typ_desc = ifelse(prison_df$top_ofnse_typ_desc == "SEX/REGISTERABLE", "SEX OFFENSE", prison_df$top_ofnse_typ_desc) 
prison_df$top_ofnse_typ_desc = ifelse(prison_df$top_ofnse_typ_desc == "SEX/NON-REGISTERABLE", "SEX OFFENSE", prison_df$top_ofnse_typ_desc) 
prison_df$top_ofnse_typ_desc = ifelse(prison_df$top_ofnse_typ_desc == "?", "OTHER", prison_df$top_ofnse_typ_desc) 
#prison_df$top_ofnse_typ_desc = ifelse(prison_df$top_ofnse_typ_desc == "WEAPONS", "OTHER", prison_df$top_ofnse_typ_desc) 
# admin_type category Other has outliers for the last year (due to lagged time in processing), need to fix
prison_df$admin_type = ifelse(prison_df$admin_type == "Other", "Technical Commitment", prison_df$admin_type)

# group "G" & "N" gender labels (due to small n's)
prison_df$gender = ifelse(prison_df$gender == "G", "MALE", prison_df$gender) 
prison_df$gender = ifelse(prison_df$gender == "N", "MALE", prison_df$gender) 
#prison_df

# rename data frame
prison_df1 = prison_df

# Convert to <S3: yearquarter>
prison_df1$Quarter_Year = yearquarter(prison_df1$Quarter_Year)

# EXPORT # 
write.csv(prison_df1, "data/prison_data_clean.csv")


#======================
## GENDER DATA 

# Extract data by gender for forecasting
# Male data
prison_data_m <- prison_df1 %>% 
  filter(gender == "M") %>%
  group_by(Quarter_Year) %>%
  summarize(Inmates = sum(count)) %>%
  mutate(
    pc_change = round((Inmates - lag(Inmates)) / lag(Inmates) * 100, 2),
    Quarter_Year = as.character(Quarter_Year)
  )


# use the aggregated data
data = prison_data_m[, c(1,2)]

write.csv(prison_data_m, "data/prison_data_m.csv")


#===============
# Female data

prison_data_f <- prison_df1 %>% 
  filter(gender == "F") %>%
  group_by(Quarter_Year) %>%
  summarize(Inmates = sum(count)) %>%
  mutate(
    pc_change = round((Inmates - lag(Inmates)) / lag(Inmates) * 100, 2),
    Quarter_Year = as.character(Quarter_Year)
  )

write.csv(prison_data_f, "data/prison_data_f.csv")


# Total population data
prison_data_plot <- prison_df1 %>%
  group_by(Quarter_Year) %>%
  summarize(Inmates = sum(count)) %>%
  mutate(
    pc_change = round((Inmates - lag(Inmates)) / lag(Inmates) * 100, 2),
    Quarter_Year = as.character(Quarter_Year)
  )

write.csv(prison_data_plot, "data/prison_data_plot.csv")


# Function to generate forecasts
generate_forecast <- function(data, forecast_horizon = 20, alpha = 0.8) {
  # Convert Quarter_Year to yearquarter if it's not already
  if(class(data$Quarter_Year)[1] != "yearquarter") {
    data$Quarter_Year <- yearquarter(data$Quarter_Year)
  }
  
  # Create structural break variables
  data$jri <- ifelse(data$Quarter_Year >= yearquarter("2015 Q1"), 1, 0)
  data$covid <- ifelse(data$Quarter_Year >= yearquarter("2020 Q1"), 1, 0)
  
  # Create time variable
  data <- data %>%
    mutate(time = row_number())
  
  # Extract quarter
  data$quarter <- quarter(data$Quarter_Year)
  
  # Merge with population data
  df_quarterly$Quarter_Year <- yearquarter(df_quarterly$Quarter_Year)
  data <- left_join(data, df_quarterly, by = "Quarter_Year")
  
  # Fit linear model
  model <- lm(Inmates ~ time + jri + covid, data = data)
  
  # Prepare data for forecast
  new_data <- data.frame(
    time = (nrow(data) + 1):(nrow(data) + forecast_horizon),
    jri = 1,
    covid = 1
  )
  
  # Generate forecasts with 80% CI
  forecasts <- predict(model, newdata = new_data, interval = "prediction", level = 0.8)
  
  # Fit ETS model to residuals
  residuals <- model$residuals
  model_ets <- ets(residuals, alpha = alpha)
  
  # Generate ETS forecasts
  forecasts_ets <- forecast(model_ets, h = forecast_horizon, level = 80)
  
  # Combine forecasts
  combined_forecasts <- forecasts[, "fit"] + forecasts_ets$mean
  lower_ci <- forecasts[, "lwr"] + forecasts_ets$lower[, 1]
  upper_ci <- forecasts[, "upr"] + forecasts_ets$upper[, 1]
  
  # Create forecast dataframe
  forecast_df <- data.frame(
    Quarter_Year = c(data$Quarter_Year, seq(data$Quarter_Year[nrow(data)] + 1, by = 1, length.out = forecast_horizon)),
    Actual = c(data$Inmates, rep(NA, forecast_horizon)),
    Forecasted = c(rep(NA, nrow(data)), combined_forecasts),
    Lower_CI = c(rep(NA, nrow(data)), lower_ci),
    Upper_CI = c(rep(NA, nrow(data)), upper_ci)
  )
  
  # Create forecast table
  forecast_table <- data.frame(
    Quarter_Year = seq(data$Quarter_Year[nrow(data)] + 1, by = 1, length.out = forecast_horizon),
    Forecasted_hybrid = combined_forecasts,
    Lower_CI = lower_ci,
    Upper_CI = upper_ci
  ) %>%
    mutate(
      Percent_Change = round((as.numeric(Forecasted_hybrid) - lag(as.numeric(Forecasted_hybrid))) / lag(as.numeric(Forecasted_hybrid)) * 100, 1)
    )
  
  return(list(forecast_df = forecast_df, forecast_table = forecast_table))
}


# Generate forecasts for total, male, and female populations
total_forecast <- generate_forecast(prison_data_plot)
male_forecast <- generate_forecast(prison_data_m)
female_forecast <- generate_forecast(prison_data_f)

# Extract forecast dataframes
forecast_df <- total_forecast$forecast_df
forecast_df_m <- male_forecast$forecast_df
forecast_df_f <- female_forecast$forecast_df


# Extract forecast tables
forecast_table_hybrid <- total_forecast$forecast_table
forecast_table_hybrid_m <- male_forecast$forecast_table
forecast_table_hybrid_f <- female_forecast$forecast_table



# Save forecast data
write.csv(total_forecast$forecast_df, "data/forecast_total.csv")
write.csv(total_forecast$forecast_table, "data/forecast_table_total.csv")
write.csv(male_forecast$forecast_df, "data/forecast_male.csv")
write.csv(male_forecast$forecast_table, "data/forecast_table_male.csv")
write.csv(female_forecast$forecast_df, "data/forecast_female.csv")
write.csv(female_forecast$forecast_table, "data/forecast_table_female.csv")









#==========
# Calculate and save summary metrics

# Calculate current metrics
current_total <- tail(prison_data_plot$Inmates[!is.na(prison_data_plot$Inmates)], 1)
current_male <- tail(prison_data_m$Inmates[!is.na(prison_data_m$Inmates)], 1)
current_female <- tail(prison_data_f$Inmates[!is.na(prison_data_f$Inmates)], 1)

# Get the forecasted values (5 years out)
projected_total <- tail(forecast_table_hybrid$Forecasted_hybrid, 1)
projected_male <- tail(forecast_table_hybrid_m$Forecasted_hybrid, 1)
projected_female <- tail(forecast_table_hybrid_f$Forecasted_hybrid, 1)

# Calculate percent changes
pct_change_total <- round(((projected_total - current_total) / current_total) * 100, 1)
pct_change_male <- round(((projected_male - current_male) / current_male) * 100, 1)
pct_change_female <- round(((projected_female - current_female) / current_female) * 100, 1)

# Calculate annual changes
annual_change_total <- round(pct_change_total / 5, 1)
annual_change_male <- round(pct_change_male / 5, 1)
annual_change_female <- round(pct_change_female / 5, 1)

# Create summary metrics dataframe
summary_metrics <- data.frame(
  population_type = c("total", "male", "female"),
  current_pop = c(current_total, current_male, current_female),
  projected_pop = c(projected_total, projected_male, projected_female),
  pct_change = c(pct_change_total, pct_change_male, pct_change_female),
  annual_change = c(annual_change_total, annual_change_male, annual_change_female)
)

# Save to CSV
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


# Create gender proportion data
# This calculates the male/female proportions historically and in projections

# Define date range
all_dates <- unique(c(
  forecast_df$Quarter_Year[!is.na(forecast_df$Actual)],
  forecast_df$Quarter_Year[is.na(forecast_df$Actual)][1:20]  # 5 years of projection
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
last_actual_date <- max(forecast_df$Quarter_Year[!is.na(forecast_df$Actual)])

# Fill historical data and projections
for (i in 1:nrow(proportion_data)) {
  date <- proportion_data$Quarter_Year[i]
  
  # Check if historical data exists for this date
  if (date <= last_actual_date) {
    male_val <- forecast_df_m$Actual[forecast_df_m$Quarter_Year == date]
    female_val <- forecast_df_f$Actual[forecast_df_f$Quarter_Year == date]
    proportion_data$Type[i] <- "Historical"
  } else {
    # Get forecasted values
    male_val <- forecast_df_m$Forecasted[forecast_df_m$Quarter_Year == date]
    female_val <- forecast_df_f$Forecasted[forecast_df_f$Quarter_Year == date]
    proportion_data$Type[i] <- "Projected"
  }
  
  if (length(male_val) > 0 && length(female_val) > 0) {
    proportion_data$Male_Count[i] <- male_val
    proportion_data$Female_Count[i] <- female_val
    proportion_data$Total_Count[i] <- male_val + female_val
    proportion_data$Male_Proportion[i] <- male_val / (male_val + female_val) * 100
    proportion_data$Female_Proportion[i] <- female_val / (male_val + female_val) * 100
  }
}

# Save the proportion data
write.csv(proportion_data, "data/proportion_data.csv")








#====





# Calculate when capacity is reached
capacity_reached <- NA
for (i in 1:length(horizon_dates)) {
  date <- horizon_dates[i]
  projected_value <- forecast_data$Forecasted[forecast_data$Quarter_Year == date]
  if (!is.na(projected_value) && projected_value > capacity_limit) {
    capacity_reached <- date
    break
  }
}

# Create plot
p <- plot_ly() %>%
  # Historical data
  add_lines(
    data = limited_forecast,
    x = ~Quarter_Year,
    y = ~Actual,
    name = "Historical",
    line = list(color = color_actual)
  ) %>%
  # Forecasted data
  add_lines(
    data = limited_forecast,
    x = ~Quarter_Year,
    y = ~Forecasted,
    name = "Projection",
    line = list(color = "#fdbf11", dash = "dash")
  ) %>%
  # Confidence interval
  add_ribbons(
    data = limited_forecast,
    x = ~Quarter_Year,
    ymin = ~Lower_CI,
    ymax = ~Upper_CI,
    name = "80% Confidence Interval",
    fillcolor = "rgba(22, 150, 210, 0.2)",
    line = list(color = "transparent")
  ) %>%
  # Capacity line
  add_lines(
    data = capacity_df,
    x = ~Quarter_Year,
    y = ~Capacity,
    name = "Capacity Limit",
    line = list(color = "red", dash = "dot", width = 2)
  )

# Add capacity reached annotation if applicable
if (!is.na(capacity_reached)) {
  projected_value <- forecast_data$Forecasted[forecast_data$Quarter_Year == capacity_reached]
  p <- p %>% add_annotations(
    x = capacity_reached,
    y = projected_value,
    text = paste("Capacity reached:", format(capacity_reached, "%Y Q%q")),
    showarrow = TRUE,
    arrowhead = 1,
    arrowsize = 1,
    arrowwidth = 2,
    arrowcolor = "red",
    ax = 0,
    ay = -40,
    font = list(color = "red")
  )
}

# Complete the plot
p %>% layout(
  title = title,
  xaxis = list(title = "Quarter/Year"),
  yaxis = list(title = "Prison Population", tickformat = ","),
  hovermode = "x unified",
  showlegend = TRUE
)
}) current metrics for value boxes
current_total <- tail(prison_data_plot$Inmates[!is.na(prison_data_plot$Inmates)], 1)
current_male <- tail(prison_data_m$Inmates[!is.na(prison_data_m$Inmates)], 1)
current_female <- tail(prison_data_f$Inmates[!is.na(prison_data_f$Inmates)], 1)

# Calculate projected metrics (5 years out)
projected_total <- tail(forecast_table_hybrid$Forecasted_hybrid, 1)
projected_male <- tail(forecast_table_hybrid_m$Forecasted_hybrid, 1)
projected_female <- tail(forecast_table_hybrid_f$Forecasted_hybrid, 1)

# Calculate percent changes
pct_change_total <- round(((projected_total - current_total) / current_total) * 100, 1)
pct_change_male <- round(((projected_male - current_male) / current_male) * 100, 1)
pct_change_female <- round(((projected_female - current_female) / current_female) * 100, 1)

# Calculate annual changes
annual_change_total <- round(pct_change_total / 5, 1)
annual_change_male <- round(pct_change_male / 5, 1)
annual_change_female <- round(pct_change_female / 5, 1)
