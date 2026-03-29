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


# Visualisation

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
