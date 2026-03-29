# 313-Time-Analysis-Statistical-Report
# Draft research question(RQ1 fixed, RQ2 need more discussion)
# 1.RQ1 (Statistical Focus): "To what extent do the Total Live Births (TLB) and Total Fertility Rate (TFR) in Singapore exhibit non-stationary characteristics, and can simple ARIMA models capture the structural shifts observed between 1960 and 2012?"
# 2.RQ2 (Contextual Focus): "How do socio-economic landmarks in Singapore’s history correlate with the observed 'shocks' in the TLB and TFR time series, and should these influences be modeled through deterministic trends or stochastic processes?"

library(tidyverse)
library(dplyr)
library(feasts)
library(tsibble)
library(ggplot2)
library(ggtime)
library(fable)
library(fpp3)

# Data loading & cleaning

# Load data
raw_data <- read_csv("BirthsAndFertilityRatesAnnual.csv")
raw_data

# data wrangling

# clean data
raw_data_char <- raw_data |> 
  mutate(across(everything(), as.character))

data_long <- raw_data_char |> 
  pivot_longer(
    cols = -DataSeries, 
    names_to = "Year", 
    values_to = "Value"
  )

# filter TLB & TFR at first(NA in dataset)
data_selected <- data_long |>
  filter(
    DataSeries == "Total Live-Births" | 
      DataSeries == "Total Fertility Rate (TFR)"
  )

# Convert to numeric, filter 1960–2024, drop any NA rows
data_final <- data_selected |>
  mutate(Year  = as.numeric(Year),
         Value = as.numeric(Value)) |>
  filter(Year >= 1960 & Year <= 2024) |>
  filter(!is.na(Value))

# tsibble 
sg_ts <- data_final |>
  as_tsibble(index = Year, key = DataSeries)


# 2. Visualisation

# Extract TLB for plotting
tlb_series <- sg_ts |> 
  filter(DataSeries == "Total Live-Births")

# Extract TFR
tfr_series <- sg_ts |> 
  filter(DataSeries == "Total Fertility Rate (TFR)")

# Plot 1: Total Live Births (TLB)
tlb_series |>
  autoplot(Value) +
  ggtitle("Total Live Births in Singapore (1960–2024)") +
  xlab("Year") + ylab("Number of Live Births")

# Plot 2: Total Fertility Rate (TFR)
tfr_series |>
  autoplot(Value) +
  ggtitle("Total Fertility Rate in Singapore (1960–2024)") +
  xlab("Year") + ylab("TFR (children per woman)")



# 3. Time series features analysis

# ACF & PACF of TLB
tlb_series |> ACF(Value, lag_max = 20)  |> autoplot() +
  ggtitle("ACF of Total Live Births (raw)")

tlb_series |> PACF(Value, lag_max = 20) |> autoplot() +
  ggtitle("PACF of Total Live Births (raw)")

# ACF & PACF of TFR
tfr_series |> ACF(Value, lag_max = 20)  |> autoplot() +
  ggtitle("ACF of Total Fertility Rate (raw)")

tfr_series |> PACF(Value, lag_max = 20) |> autoplot() +
  ggtitle("PACF of Total Fertility Rate (raw)")

# Portmanteau tests
tlb_series |> features(Value, portmanteau_tests)
tfr_series |> features(Value, portmanteau_tests)



# 4. Time series features analysis2 - Stationary Check

# Plot first difference
tlb_series <- tlb_series |> mutate(D_Value = difference(Value, lag = 1))
tfr_series <- tfr_series |> mutate(D_Value = difference(Value, lag = 1))

# TLB
tlb_series |> autoplot(D_Value) +
  ggtitle("First Difference of Total Live Births") +
  xlab("Year") + ylab("Change in Live Births")

# TFR
tfr_series |> autoplot(D_Value) +
  ggtitle("First Difference of Total Fertility Rate") +
  xlab("Year") + ylab("Change in TFR")

# ACF & PACF of first differences for TLB
tlb_series |> filter(!is.na(D_Value)) |>
  ACF(D_Value, lag_max = 20) |> autoplot() +
  ggtitle("ACF of First-Differenced TLB")

tlb_series |> filter(!is.na(D_Value)) |>
  PACF(D_Value, lag_max = 20) |> autoplot() +
  ggtitle("PACF of First-Differenced TLB")

# ACF & PACF of first differences for TFR
tfr_series |> filter(!is.na(D_Value)) |>
  ACF(D_Value, lag_max = 20) |> autoplot() +
  ggtitle("ACF of First-Differenced TFR")

tfr_series |> filter(!is.na(D_Value)) |>
  PACF(D_Value, lag_max = 20) |> autoplot() +
  ggtitle("PACF of First-Differenced TFR")



# 5. Initial model fitting

tlb_train <- tlb_series |> filter(Year <= 2012)
tfr_train <- tfr_series |> filter(Year <= 2012)

# TLB: try ARIMA(1,1,0)
tlb_model <- tlb_train |>
  model(arima110 = ARIMA(Value ~ pdq(1,1,0)))
tidy(tlb_model)
glance(tlb_model)

# Check residual - TLB
augment(tlb_model) |> ACF(.resid,  lag_max = 20) |> autoplot() +
  ggtitle("ACF of Residuals — TLB ARIMA(1,1,0) on log scale")

augment(tlb_model) |> PACF(.resid, lag_max = 20) |> autoplot() +
  ggtitle("PACF of Residuals — TLB ARIMA(1,1,0) on log scale")

# TFR: try ARIMA(1,1,0)
tfr_model <- tfr_train |>
  model(arima110 = ARIMA(Value ~ pdq(1,1,0)))
tidy(tfr_model)
glance(tfr_model)

# Check residuals - TFR
augment(tfr_model) |> ACF(.resid,  lag_max = 20) |> autoplot() +
  ggtitle("ACF of Residuals — TFR ARIMA(1,1,0)")

augment(tfr_model) |> PACF(.resid, lag_max = 20) |> autoplot() +
  ggtitle("PACF of Residuals — TFR ARIMA(1,1,0)")



