library(tidyverse)
library(tikzDevice)
library(forecast)
library(patchwork)
library(pROC)
library(fable)
library(tsibble)
library(fpp3)

# Load data
tr <- read_rds("data/trainingData/trainingData_1_2.rds")
val <- read_rds("data/validationData/validationData_1_2.rds")
# Combine
df <- bind_rows(tr, val)
# Create a time series tibble
ts_df <- df |> 
  filter(day == "1") |> 
  mutate_at("faultDate", as.Date) |> 
  as_tsibble(index = faultDate, 
             key = region) |> 
  summarise(MeanFaults = mean(faultCount)) |> 
  fill_gaps()
# Plot PACF
plot_pacf <- ts_df |> 
  ACF(MeanFaults) |> 
  autoplot() +
  labs(title = "", x = "Lag", y = "ACF")
# Plot PACF
plot_acf <- ts_df |> 
  PACF(MeanFaults) |> 
  autoplot() +
  labs(title = "", x = "Lag", y = "PACF")
ts_plots <- plot_acf + plot_pacf
# Save
tikzDevice::tikz(file = "res/figures/ts_plots.tex", width = 5, height = 2.3)
print(ts_plots)
dev.off()