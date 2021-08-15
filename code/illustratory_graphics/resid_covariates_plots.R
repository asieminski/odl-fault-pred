library(tidyverse)
library(tikzDevice)

# Load data
short_data <- read_rds("res/augmented_data/augmented_1_2_data.rds")
long_data <- read_rds("res/augmented_data/augmented_3_5_data.rds")

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


