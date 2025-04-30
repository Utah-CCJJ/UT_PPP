
# libraries needed
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

## Hybrid Model (Linear/ETS)

### Total

```{r}

# aggregate the data and calculate pc change
prison_data_plot <- prison_df1 %>%
  group_by(Quarter_Year) %>%
  summarize(Inmates = sum(count)) %>%
  mutate(
    pc_change = round((Inmates - lag(Inmates)) / lag(Inmates) * 100, 2),
    Quarter_Year = as.character(Quarter_Year)
  )

# use the aggregated data
data = prison_data_plot[, c(1,2)]
data

# Convert Quarter_Year to yearquarter
data$Quarter_Year <- yearquarter(data$Quarter_Year)

# create the structural break variables
data$jri <- ifelse(data$Quarter_Year >= yearquarter("2015 Q1"), 1, 0)
data$covid <- ifelse(data$Quarter_Year >= yearquarter("2020 Q1"), 1, 0)

# create a time variable
data = data %>%
  mutate(time = row_number())

# Extract the quarter to add a seasonal effect
data$quarter <- quarter(data$Quarter_Year)

# merge w pop data
df_quarterly$Quarter_Year = yearquarter(df_quarterly$Quarter_Year)
data = left_join(data, df_quarterly, by = "Quarter_Year")
#data

# Fit a linear model on all data via lm()
model <- lm(Inmates ~ time + jri + covid, data = data)
#+ pop_quarterly 

# view model
summary(model)

# Forecast
forecast_horizon <- 20  # Set forecast horizon
new_data <- data.frame(
  time = (nrow(data) + 1):(nrow(data) + forecast_horizon),  # Adjust time index for new data
  #pop_quarterly = rep(df_quarterly$pop_quarterly[nrow(df_quarterly)], forecast_horizon),  # Extend pop_quarterly for forecast horizon
  #quarter = factor(rep(1:4, length.out = forecast_horizon)),  # Generate quarterly values for forecast horizon
  jri = 1,
  covid = 1
)
# use the predict function to forecast out, 80% CI
forecasts <- predict(model, newdata = new_data, interval = "prediction", level = 0.8) # 80% CI

# Combine data (adjust the lengths of variables based on the forecast horizon)
df_lm_1 <- data.frame(
  time = 1:(nrow(data) + forecast_horizon),
  Quarter_Year = c(data$Quarter_Year, seq(data$Quarter_Year[nrow(data)] + 1, by = 1, length.out = forecast_horizon)),
  Actual = c(data$Inmates, rep(NA, forecast_horizon)),
  Forecast_LM = c(rep(NA, nrow(data)), forecasts[, "fit"]),
  Lower_LM = c(rep(NA, nrow(data)), forecasts[, "lwr"]),
  Upper_LM = c(rep(NA, nrow(data)), forecasts[, "upr"])
)

# Fit ETS model to residuals of linear regression
residuals <- model$residuals
model_ets <- ets(residuals, alpha = 0.8) # choose value of alpha (0-1), higher alpha responds more to recent trends

# Forecast from ETS model with prediction intervals using forecast()
forecasts_ets <- forecast(model_ets, h = forecast_horizon, level = 80)

# Combine forecasts and prediction intervals
combined_forecasts <- forecasts[, "fit"] + forecasts_ets$mean
lower_ci <- forecasts[, "lwr"] + forecasts_ets$lower[, 1]
upper_ci <- forecasts[, "upr"] + forecasts_ets$upper[, 1]

# Create data frame for plotting
forecast_df <- data.frame(
  Quarter_Year = c(data$Quarter_Year, seq(data$Quarter_Year[nrow(data)] + 1, by = 1, length.out = forecast_horizon)),
  Actual = c(data$Inmates, rep(NA, forecast_horizon)),
  Forecasted = c(rep(NA, nrow(data)), combined_forecasts),
  Lower_CI = c(rep(NA, nrow(data)), lower_ci),
  Upper_CI = c(rep(NA, nrow(data)), upper_ci)
)

# Create the plot
p = ggplot(forecast_df, aes(x = Quarter_Year)) +
  geom_line(aes(y = Actual, color = "Actual"), linetype = "solid") +
  geom_line(aes(y = Forecasted, color = "Forecasted"), linetype = "dashed") +
  geom_ribbon(aes(ymin = Lower_CI, ymax = Upper_CI), fill = "grey70", alpha = 0.5) +
  labs(title = "Actual vs Forecasted Prison Population: Linear Model w. ETS residual adjustment",
       x = "Quarter/Year",
       y = "Prison Population") +
  scale_color_manual(
    values = c("Actual" = "#00A7C4", "Forecasted" = "red"),
    labels = c("Actual", "Forecast")
  ) +
  theme_minimal() +
  ylim(0, 10000) +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(title = "Series"))

p # view ggplot

pp = ggplotly(p) # convert to plotly

pp # view plotly

```

### Table 

```{r}

# Create data frame for the table
forecast_table_hybrid <- data.frame(
  Quarter_Year = seq(data$Quarter_Year[nrow(data)] + 1, by = 1, length.out = forecast_horizon),
  Forecasted_hybrid = combined_forecasts,
  Lower_CI = lower_ci,
  Upper_CI = upper_ci
) %>%
  mutate(
    Percent_Change = round((as.numeric(Forecasted_hybrid) - lag(as.numeric(Forecasted_hybrid))) / lag(as.numeric(Forecasted_hybrid)) * 100, 1)  # Convert to numeric before lag(), round to 1 decimals
  )

# Print the table via kable()
kable(forecast_table_hybrid)

```

### And by gender:

```{r}

# aggregate the data and calculate pc change: males
prison_data_m <- prison_df1 %>% filter(gender == "M") %>%
  group_by(Quarter_Year) %>%
  summarize(Inmates = sum(count)) %>%
  mutate(
    pc_change = round((Inmates - lag(Inmates)) / lag(Inmates) * 100, 2),
    Quarter_Year = as.character(Quarter_Year)
  )

# use the aggregated data
data = prison_data_m[, c(1,2)]
#data

# Convert Quarter_Year to yearquarter
data$Quarter_Year <- yearquarter(data$Quarter_Year)

# create the structural break variables
data$jri <- ifelse(data$Quarter_Year >= yearquarter("2015 Q1"), 1, 0)
data$covid <- ifelse(data$Quarter_Year >= yearquarter("2020 Q1"), 1, 0)

# create a time variable
data = data %>%
  mutate(time = row_number())

# Extract the quarter to add a seasonal effect
data$quarter <- quarter(data$Quarter_Year)

# merge w pop data
df_quarterly$Quarter_Year = yearquarter(df_quarterly$Quarter_Year)
data = left_join(data, df_quarterly, by = "Quarter_Year")
data

# Fit model on all data
model <- lm(Inmates ~ time + jri + covid, data = data)
#+ pop_quarterly 
summary(model)

# Forecast
forecast_horizon <- 20  # Forecast horizon is now 20 quarters
new_data <- data.frame(
  time = (nrow(data) + 1):(nrow(data) + forecast_horizon),  # Adjust time index for new data
  #pop_quarterly = rep(df_quarterly$pop_quarterly[nrow(df_quarterly)], forecast_horizon),  # Extend pop_quarterly for forecast horizon
  #quarter = factor(rep(1:4, length.out = forecast_horizon)),  # Generate quarterly values for forecast horizon
  jri = 1,
  covid = 1
)
forecasts <- predict(model, newdata = new_data, interval = "prediction", level = 0.8)

# Combine data (adjust the lengths of variables based on the forecast horizon)
df_lm_1_m <- data.frame(
  time = 1:(nrow(data) + forecast_horizon),
  Quarter_Year = c(data$Quarter_Year, seq(data$Quarter_Year[nrow(data)] + 1, by = 1, length.out = forecast_horizon)),
  Actual_m = c(data$Inmates, rep(NA, forecast_horizon)),
  Forecast_LM = c(rep(NA, nrow(data)), forecasts[, "fit"]),
  Lower_LM = c(rep(NA, nrow(data)), forecasts[, "lwr"]),
  Upper_LM = c(rep(NA, nrow(data)), forecasts[, "upr"])
)

# Fit ETS model to residuals of linear regression (using all data)
residuals <- model$residuals
model_ets <- ets(residuals, alpha = 0.8)

# Forecast from ETS model with prediction intervals
forecasts_ets <- forecast(model_ets, h = forecast_horizon, level = 80)

# Combine forecasts and prediction intervals
combined_forecasts <- forecasts[, "fit"] + forecasts_ets$mean
lower_ci <- forecasts[, "lwr"] + forecasts_ets$lower[, 1]
upper_ci <- forecasts[, "upr"] + forecasts_ets$upper[, 1]

# Create data frame for plotting
forecast_df_m <- data.frame(
  Quarter_Year = c(data$Quarter_Year, seq(data$Quarter_Year[nrow(data)] + 1, by = 1, length.out = forecast_horizon)),
  Actual = c(data$Inmates, rep(NA, forecast_horizon)),
  Forecasted = c(rep(NA, nrow(data)), combined_forecasts),
  Lower_CI = c(rep(NA, nrow(data)), lower_ci),
  Upper_CI = c(rep(NA, nrow(data)), upper_ci)
)

# Create the plot
ggplot(forecast_df_m, aes(x = Quarter_Year)) +
  geom_line(aes(y = Actual, color = "Actual"), linetype = "solid") +
  geom_line(aes(y = Forecasted, color = "Forecasted"), linetype = "dashed") +
  geom_ribbon(aes(ymin = Lower_CI, ymax = Upper_CI), fill = "grey70", alpha = 0.5) +
  labs(title = "Actual vs Forecasted Male Prison Population: Linear Model w. ETS residual adjustment",
       x = "Quarter/Year",
       y = "Prison Population") +
  scale_color_manual(
    values = c("Actual" = "#00A7C4", "Forecasted" = "red"),
    labels = c("Actual", "Forecast")
  ) +
  theme_minimal() +
  ylim(0, 8500) +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(title = "Series"))

```

### Table 

```{r}

# Create data frame for the table
forecast_table_hybrid_m <- data.frame(
  Quarter_Year = seq(data$Quarter_Year[nrow(data)] + 1, by = 1, length.out = forecast_horizon),
  Forecasted_hybrid = combined_forecasts,
  Lower_CI = lower_ci,
  Upper_CI = upper_ci
) %>%
  mutate(
    Percent_Change = round((as.numeric(Forecasted_hybrid) - lag(as.numeric(Forecasted_hybrid))) / lag(as.numeric(Forecasted_hybrid)) * 100, 1)  # Convert to numeric before lag(), round to 1 decimals
  )

# Print the table via kable()
kable(forecast_table_hybrid_m)

```


```{r}
# aggregate the data and calculate pc change: males
prison_data_f <- prison_df1 %>% filter(gender == "F") %>%
  group_by(Quarter_Year) %>%
  summarize(Inmates = sum(count)) %>%
  mutate(
    pc_change = round((Inmates - lag(Inmates)) / lag(Inmates) * 100, 2),
    Quarter_Year = as.character(Quarter_Year)
  )

# use the aggregated data
data = prison_data_f[, c(1,2)]
#data

# Convert Quarter_Year to yearquarter
data$Quarter_Year <- yearquarter(data$Quarter_Year)

# create the structural break variables
data$jri <- ifelse(data$Quarter_Year >= yearquarter("2015 Q1"), 1, 0)
data$covid <- ifelse(data$Quarter_Year >= yearquarter("2020 Q1"), 1, 0)

# create a time variable
data = data %>%
  mutate(time = row_number())

# Extract the quarter to add a seasonal effect
data$quarter <- quarter(data$Quarter_Year)

# merge w pop data
df_quarterly$Quarter_Year = yearquarter(df_quarterly$Quarter_Year)
data = left_join(data, df_quarterly, by = "Quarter_Year")
data

# Fit model on all data
model <- lm(Inmates ~ time + jri + covid, data = data)
#+ pop_quarterly 
summary(model)

# Forecast
forecast_horizon <- 20  # Forecast horizon is now 20 quarters
new_data <- data.frame(
  time = (nrow(data) + 1):(nrow(data) + forecast_horizon),  # Adjust time index for new data
  #pop_quarterly = rep(df_quarterly$pop_quarterly[nrow(df_quarterly)], forecast_horizon),  # Extend pop_quarterly for forecast horizon
  #quarter = factor(rep(1:4, length.out = forecast_horizon)),  # Generate quarterly values for forecast horizon
  jri = 1,
  covid = 1
)
forecasts <- predict(model, newdata = new_data, interval = "prediction", level = 0.8)

# Combine data (adjust the lengths of variables based on the forecast horizon)
df_lm_1_f <- data.frame(
  time = 1:(nrow(data) + forecast_horizon),
  Quarter_Year = c(data$Quarter_Year, seq(data$Quarter_Year[nrow(data)] + 1, by = 1, length.out = forecast_horizon)),
  Actual_f = c(data$Inmates, rep(NA, forecast_horizon)),
  Forecast_LM = c(rep(NA, nrow(data)), forecasts[, "fit"]),
  Lower_LM = c(rep(NA, nrow(data)), forecasts[, "lwr"]),
  Upper_LM = c(rep(NA, nrow(data)), forecasts[, "upr"])
)

# Fit ETS model to residuals of linear regression (using all data)
residuals <- model$residuals
model_ets <- ets(residuals, alpha = 0.8)

# Forecast from ETS model with prediction intervals
forecasts_ets <- forecast(model_ets, h = forecast_horizon, level = 80)

# Combine forecasts and prediction intervals
combined_forecasts <- forecasts[, "fit"] + forecasts_ets$mean
lower_ci <- forecasts[, "lwr"] + forecasts_ets$lower[, 1]
upper_ci <- forecasts[, "upr"] + forecasts_ets$upper[, 1]

# Create data frame for plotting
forecast_df_f <- data.frame(
  Quarter_Year = c(data$Quarter_Year, seq(data$Quarter_Year[nrow(data)] + 1, by = 1, length.out = forecast_horizon)),
  Actual = c(data$Inmates, rep(NA, forecast_horizon)),
  Forecasted = c(rep(NA, nrow(data)), combined_forecasts),
  Lower_CI = c(rep(NA, nrow(data)), lower_ci),
  Upper_CI = c(rep(NA, nrow(data)), upper_ci)
)

# Create the plot
ggplot(forecast_df_f, aes(x = Quarter_Year)) +
  geom_line(aes(y = Actual, color = "Actual"), linetype = "solid") +
  geom_line(aes(y = Forecasted, color = "Forecasted"), linetype = "dashed") +
  geom_ribbon(aes(ymin = Lower_CI, ymax = Upper_CI), fill = "grey70", alpha = 0.5) +
  labs(title = "Actual vs Forecasted Female Prison Population: Linear Model w. ETS residual adjustment",
       x = "Quarter/Year",
       y = "Prison Population") +
  scale_color_manual(
    values = c("Actual" = "#00A7C4", "Forecasted" = "red"),
    labels = c("Actual", "Forecast")
  ) +
  theme_minimal() +
  ylim(0, 1500) +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(title = "Series"))

```

### Table 

```{r}

# Create data frame for the table
forecast_table_hybrid_f <- data.frame(
  Quarter_Year = seq(data$Quarter_Year[nrow(data)] + 1, by = 1, length.out = forecast_horizon),
  Forecasted_hybrid = combined_forecasts,
  Lower_CI = lower_ci,
  Upper_CI = upper_ci
) %>%
  mutate(
    Percent_Change = round((as.numeric(Forecasted_hybrid) - lag(as.numeric(Forecasted_hybrid))) / lag(as.numeric(Forecasted_hybrid)) * 100, 1)  # Convert to numeric before lag(), round to 1 decimals
  )

# Print the table via kable()
kable(forecast_table_hybrid_f)

```