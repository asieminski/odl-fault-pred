library(tidyverse)
library(tikzDevice)
library(forecast)
library(patchwork)
library(pROC)
library(fable)
library(tsibble)
library(fpp3)

# Load data
short_data <- read_rds("res/augmented_data/augmented_1_2_data.rds")
long_data <- read_rds("res/augmented_data/augmented_3_5_data.rds")
#mean(short_data$faultCount==0)
(dist_nonzero_faults <- short_data |> 
  filter(faultCount>0) |> 
  ggplot(aes(x = faultCount, y = zeroFault))+
  geom_jitter(alpha = 0.1, fill = "white")+
  theme_minimal()+
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank())+
  labs(y = "", x = "Number of faults") +
  geom_vline(color = 'firebrick', 
             xintercept = short_data$faultCount[short_data$faultCount>0] |> 
               median())+
  geom_vline(color = 'orange', size = .5,
             xintercept = short_data$faultCount[short_data$faultCount>0] |> 
               quantile(c(1/4, 3/4)  )))
tikzDevice::tikz(file = "res/figures/dist_nonzero_faults.tex", width = 5, height = 2)
print(dist_nonzero_faults)
dev.off()




# ROC AUC
# tapply(short_data$xi0, short_data$zeroFault, mean) 
sproc <- pROC::roc(as.numeric(short_data$faultCount==0), short_data$xi0)
lproc <- pROC::roc(as.numeric(long_data$faultCount==0), long_data$xi0)
len <- length(lproc$specificities)+
  length(sproc$specificities)
tibble(
  Model = c(rep("1-2", length(lproc$specificities)),
                rep("3-5", length(sproc$specificities))),
  sens = c(lproc$sensitivities,sproc$sensitivities),
  spec = c(lproc$specificities,sproc$specificities),
  base = seq(0, 1, length.out = length(lproc$specificities)+length(sproc$specificities)))|> 
    ggplot(aes(y = sens, x = 1 - spec, color = Model))+
      geom_line() +
      geom_line(mapping = aes(x = base, y = base), 
                alpha = 0.5, colour = "darkgrey")+
      theme_minimal()+
      scale_colour_viridis_d()+
      labs(x = "False Positive Rate", y = "True Positive Rate")


# Residuals over time    
reg_ord <- levels(short_data$region)[
  c(9, 6, 8, 5,4, 1, 2,3,7)]
(resid_over_time <- short_data |> 
  mutate(region = factor(region, levels = reg_ord)) |>
  ggplot(aes(faultDate, qnorm(CDF), color = day))+
    geom_point(alpha = 0.5, size = .7)+
    geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"))+
    facet_wrap(~region) +
    scale_y_continuous(breaks = c(-4:4), limits = c(-4, 4)) +
    geom_hline(yintercept = 0, alpha = 0.7)+
    geom_hline(yintercept = c(-1, 1), alpha = 0.4)+
    scale_colour_viridis_d()+
    labs(x = "Date", y = "Quantile Residuals"))
# Save
tikzDevice::tikz(file = "res/figures/resid_over_time.tex", width = 5, height = 2.3)
print(resid_over_time)
dev.off()


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
