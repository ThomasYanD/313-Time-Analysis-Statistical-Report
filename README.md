---
title: "EDA: Singapore Total Live Births and Total Fertility Rate (1960–2024)"
author: "Yan Dai"
date: "`r Sys.Date()`"
output:
  pdf_document:
    latex_engine: xelatex
---

**GitHub Repository:** [https://github.com/ThomasYanD/313-Time-Analysis-Statistical-Report/tree/d36e89eb0d3cf8f9c30b295ed84f30c16f92ed04]

---

## 1. Research Questions

This EDA investigates two research questions motivated by the observed temporal features of Singapore's birth data:

**RQ1 (Statistical):** To what extent do Total Live Births (TLB) and Total Fertility Rate (TFR) in Singapore exhibit non-stationary characteristics, and can ARIMA models adequately capture the structural shifts observed between 1960 and 2012?

**RQ2 (Contextual):** How do key socio-economic events in Singapore's history — such as the 1966 "Stop at Two" policy, and the 1987 "Have Three or More" — manifest as structural shifts in the TLB and TFR series?

Both questions are strongly motivated by the visual and statistical features identified in the EDA below. RQ1 focuses on core time series analysis, while RQ2 aims to find out whether statistical model selection is insufficient when the data is influenced by policy interventions. Therefore, the choice of model may vary as research goes on.

---

## 2. Data Description and Cleaning

The data is sourced from Immigration and Checkpoints Authority, Singapore Department of Statistics. For this analysis, only two series in the raw dataset are extracted: **Total Live Births (TLB)** and **Total Fertility Rate (TFR)**, for the period **1960 to 2024**, giving 65 annual observations per series.

The raw dataset is in wide format, with years as column headers and series types as row identifiers. Because some earlier year columns were coded as `na`, all columns were first converted to character before pivoting to long format. Values were then converted to numeric, with suppressed `NA`s subsequently dropped. No observations for TLB or TFR were missing within the 1960--2024 range.


```{r setup, message=FALSE, warning=FALSE}
library(tidyverse)
library(dplyr)
library(feasts)
library(tsibble)
library(ggplot2)
library(ggtime)
library(fable)
library(fpp3)
```

```{r data-cleaning}
# Load data
raw_data <- read_csv("BirthsAndFertilityRatesAnnual.csv")

# Convert all columns to character to safely handle suppressed values
raw_data_char <- raw_data |>
  mutate(across(everything(), as.character))

# Pivot to long format
data_long <- raw_data_char |>
  pivot_longer(
    cols = -DataSeries,
    names_to  = "Year",
    values_to = "Value"
  )

# Filter to TLB and TFR only
data_selected <- data_long |>
  filter(DataSeries == "Total Live-Births" |
           DataSeries == "Total Fertility Rate (TFR)")

# Convert to numeric, filter 1960-2024, drop NA
data_final <- data_selected |>
  mutate(Year  = as.numeric(Year),
         Value = as.numeric(Value)) |>
  filter(Year >= 1960 & Year <= 2024) |>
  filter(!is.na(Value))

# Build tsibble
sg_ts <- data_final |>
  as_tsibble(index = Year, key = DataSeries)

# Separate series
tlb_series <- sg_ts |> filter(DataSeries == "Total Live-Births")
tfr_series <- sg_ts |> filter(DataSeries == "Total Fertility Rate (TFR)")
```

---

## 3. Visualisation and Temporal Features

### 3.1 Raw Series

```{r plot-tlb, fig.width=8, fig.height=4}
tlb_series |>
  autoplot(Value) +
  ggtitle("Total Live Births in Singapore (1960-2024)") +
  xlab("Year") + ylab("Number of Live Births")
```

The TLB series shows a strong overall **downward trend** from approximately 62,000 births in 1960 to around 30,000 in 2024, punctuated by a notable **local peak around 1988** (approximately 52,000 births). This peak coincides with the 1987 "Have Three or More" population policy**[1]**, when the government introduced incentives for larger families.

A secondary feature is the sharp drop around 1966-1967, consistent with the impact of the original "Two-child policy**[2]**. The series is clearly non-stationary, exhibiting both a persistent downward trend and visible level shifts at policy change points.

```{r plot-tfr, fig.width=8, fig.height=4}
tfr_series |>
  autoplot(Value) +
  ggtitle("Total Fertility Rate in Singapore (1960-2024)") +
  xlab("Year") + ylab("TFR (children per woman)")
```

The TFR series shows an even more pronounced and sustained **monotonic decline**, from 5.76 children per woman in 1960 to 0.97 in 2024. The rate of decline is highest in the 1960s and gradually flattens from the mid-1980s onward, suggesting that the series may be approaching a structural floor. Like TLB, TFR is clearly non-stationary.

---

## 4. Statistical Tests

### 4.1 ACF and PACF of Raw Series

```{r acf-pacf-raw, fig.width=8, fig.height=4}
tlb_series |> ACF(Value, lag_max = 20) |> autoplot() +
  ggtitle("ACF of Total Live Births (raw)")

tlb_series |> PACF(Value, lag_max = 20) |> autoplot() +
  ggtitle("PACF of Total Live Births (raw)")

tfr_series |> ACF(Value, lag_max = 20) |> autoplot() +
  ggtitle("ACF of Total Fertility Rate (raw)")

tfr_series |> PACF(Value, lag_max = 20) |> autoplot() +
  ggtitle("PACF of Total Fertility Rate (raw)")
```

The ACF of both raw series shows very slow decay -- remaining significantly positive across all 20 lags -- which is a classic signature of non-stationarity, confirming that differencing is required before fitting ARIMA models.

### 4.2 Portmanteau Tests

```{r portmanteau}
tlb_series |> features(Value, portmanteau_tests)
tfr_series |> features(Value, portmanteau_tests)
```

Both series strongly reject the null hypothesis of no autocorrelation (p << 0.05), confirming that neither series is white noise. Time series modelling is warranted.

---

## 5. Stationarity Check -- First Differences

### 5.1 Differenced Series Plots

```{r differencing}
tlb_series <- tlb_series |> mutate(D_Value = difference(Value, lag = 1))
tfr_series <- tfr_series |> mutate(D_Value = difference(Value, lag = 1))
```

```{r plot-diff, fig.width=8, fig.height=4, warning=FALSE}
tlb_series |> autoplot(D_Value) +
  ggtitle("First Difference of Total Live Births") +
  xlab("Year") + ylab("Change in Live Births")

tfr_series |> autoplot(D_Value) +
  ggtitle("First Difference of Total Fertility Rate") +
  xlab("Year") + ylab("Change in TFR")
```

After first differencing, both series fluctuate around zero without obvious trend.

### 5.2 ACF and PACF of Differenced Series

```{r train-split}
# Define training set here so auto ARIMA below can use it
tlb_train <- tlb_series |> filter(Year <= 2012)
tfr_train <- tfr_series |> filter(Year <= 2012)
```

```{r acf-pacf-diff, fig.width=8, fig.height=4}
tlb_series |> filter(!is.na(D_Value)) |>
  ACF(D_Value, lag_max = 20) |> autoplot() +
  ggtitle("ACF of First-Differenced TLB")

tlb_series |> filter(!is.na(D_Value)) |>
  PACF(D_Value, lag_max = 20) |> autoplot() +
  ggtitle("PACF of First-Differenced TLB")

tfr_series |> filter(!is.na(D_Value)) |>
  ACF(D_Value, lag_max = 20) |> autoplot() +
  ggtitle("ACF of First-Differenced TFR")

tfr_series |> filter(!is.na(D_Value)) |>
  PACF(D_Value, lag_max = 20) |> autoplot() +
  ggtitle("PACF of First-Differenced TFR")
```

```{r auto-arima}
tlb_best_fit <- tlb_train |>
  model(auto_arima = ARIMA(Value, stepwise = FALSE, approximation = FALSE))
report(tlb_best_fit)

tfr_best_fit <- tfr_train |>
  model(auto_arima = ARIMA(Value, stepwise = FALSE, approximation = FALSE))
report(tfr_best_fit)
```

For the differenced TLB, most lags fall within the 95% confidence bounds, with the exception of several notable spikes around lag 11-13. This spike is unlikely to be removed by additional AR or MA terms, as it likely reflects the demographic echo of the 1987 "Have Three or More" policy reversal, which produced a structural break in the series rather than a regular autocorrelation pattern. ARIMA(0,1,0) is therefore selected as the initial candidate model for TLB.

For the differenced TFR, both the ACF and PACF show a significant spike at lag 1, with several additional spikes at later lags (lags 3, 6, and 10). The pattern does not show a clean cut-off consistent with a pure AR(1) or MA(1) process. Nevertheless, the most prominent spike occurs at lag 1 in both plots, suggesting that an AR(1) component captures the dominant structure. ARIMA(1,1,0) is therefore selected as the initial candidate model. A more complex specification may be needed in the Final Report.

As shown above, automatic model selection suggested ARIMA(0,1,0) for TLB and ARIMA(0,2,1) for TFR. The result for TFR suggests that a second order of differencing may be needed to achieve stationarity, which is consistent with the decline observed in the raw series. However, the residual spike at lag 11 remains even under automatic selection, indicating the policy intervention instead of flawless of model. The choice will be further justified or varied in the final report.

---

## 6. Initial Model Fitting (Training Period: 1960-2012)

Models are fitted on the training period 1960-2012 as instructed, leaving 2013-2024 for forecast evaluation in the Final Report.

### 6.1 TLB -- ARIMA(0,1,0)

An initial ARIMA(1,1,0) was fitted but the AR(1) coefficient was not statistically significant (estimate = $-0.069$, $t = -0.497$), so the model was simplified to ARIMA(0,1,0), equivalent to a **random walk**:

$$Y_t = Y_{t-1} + Z_t, \quad Z_t \sim \text{WN}(0, \sigma^2)$$

where $Y_t$ denotes TLB at year $t$ and $Z_t$ is white noise.

```{r tlb-model}
# TLB: ARIMA(0,1,0) -- AR(1) term not significant, simplified to random walk
tlb_model <- tlb_train |>
  model(arima010 = ARIMA(Value ~ pdq(0,1,0)))

tidy(tlb_model)
glance(tlb_model)
```

```{r tlb-resid, fig.width=8, fig.height=4}
augment(tlb_model) |> ACF(.resid, lag_max = 20) |> autoplot() +
  ggtitle("ACF of Residuals -- TLB ARIMA(0,1,0)")

augment(tlb_model) |> PACF(.resid, lag_max = 20) |> autoplot() +
  ggtitle("PACF of Residuals -- TLB ARIMA(0,1,0)")
```

The residual ACF and PACF show that most lags are within bounds, with the exception of a spike at lag 11-13. This spike may reflect the delayed influence of the 1987 policy reversal and represents a limitation of the current model that will be addressed in the Final Report.

### 6.2 TFR -- ARIMA(1,1,0)

The differenced TFR series shows a significant positive spike at lag 1 in both the ACF and PACF, with the PACF being most prominent. This suggests an AR(1) component, motivating ARIMA(1,1,0) as the initial model. Thus, a constant term was excluded. Now, The model is:

$$Y_t - Y_{t-1} = \phi_1 (Y_{t-1} - Y_{t-2}) + Z_t, \quad Z_t \sim \text{WN}(0, \sigma^2)$$

where $\phi_1$ is the AR(1) coefficient.

```{r tfr-model}
# TFR: ARIMA(1,1,0) -- AR(1) indicated by dominant spike at lag 1 in PACF
tfr_model <- tfr_train |>
  model(arima110 = ARIMA(Value ~ 0 + pdq(1,1,0)))

tidy(tfr_model)
glance(tfr_model)
```

```{r tfr-resid, fig.width=8, fig.height=4}
augment(tfr_model) |> ACF(.resid, lag_max = 20) |> autoplot() +
  ggtitle("ACF of Residuals -- TFR ARIMA(1,1,0)")

augment(tfr_model) |> PACF(.resid, lag_max = 20) |> autoplot() +
  ggtitle("PACF of Residuals -- TFR ARIMA(1,1,0)")
```

The residual ACF and PACF both show a significant spike at lag 11 and 15. This may reflect the influence of the 1987 policy reversal approximately 11 years after the 1976 trough in fertility, creating a structural pattern that a standard ARIMA model cannot capture without intervention terms. ARIMA(0,2,1), as suggested by automatic model selection, will also be considered as an alternative candidate in the Final Report.

---

## 7. Preliminary Forecasts (2013-2024)

```{r forecast, fig.width=8, fig.height=4}
tlb_fc <- tlb_model |> forecast(h = 12)
tfr_fc <- tfr_model |> forecast(h = 12)

tlb_series |> autoplot(Value) +
  autolayer(tlb_fc) +
  ggtitle("TLB: ARIMA(0,1,0) Forecast vs Observed (2013-2024)") +
  xlab("Year") + ylab("Number of Live Births")

tfr_series |> autoplot(Value) +
  autolayer(tfr_fc) +
  ggtitle("TFR: ARIMA(1,1,0) Forecast vs Observed (2013-2024)") +
  xlab("Year") + ylab("TFR (children per woman)")
```

For TLB, the ARIMA(0,1,0) forecast is essentially flat (random walk), with a wide 95% prediction interval reflecting high uncertainty. For TFR, the ARIMA(1,1,0) forecast predicts a gradual stabilisation. However, the 95% prediction interval for TFR extends below zero, since ARIMA does not enforce a non-negativity constraint.

---


## 8. Brief Literature Discussion

A recent methodological concern is raised by Mazzuco and Zanotto -- the observed TFR can diverge from the true underlying fertility level (quantum) when women shift the timing of childbearing. Crucially, they find that rapid shifts in fertility timing produce larger distortions in the period TFR. This is directly relevant to Singapore, where the 1987 "Have Three or More" policy 
reversal likely induced a sudden change in birth timing, which may 
have temporarily inflated the observed TFR rather than reflecting a 
genuine increase in completed fertility.**[3]**

This suggests that the visible peak in the Singapore TFR series around 1988 should be interpreted with caution as a policy-driven tempo effect, rather than a true recovery in quantum fertility. For the scope of this project, 
ARIMA-based approaches on the aggregate annual series are appropriate, 
but this limitation should be acknowledged when interpreting forecast 
results in the Final Report.

---

## 9. Reference
[1]John, A. (1987, March 2). Have 3, or more if you can afford it. The Straits Times, p. 1. Retrieved from NewspaperSG.

[2]Saw Swee-Hock, Population Policies and Programmes in Singapore (Singapore: Institute of Southeast Asian Studies, 2005), 81. (Call no. RSING 363.96095957 SAW)

[3]Mazzuco, S. & Zanotto, L. (2025). Tempo effects in period TFR: Inspecting the role of shape and scale variations in a cohort model. Demographic Research, 52(19), 559–588.
