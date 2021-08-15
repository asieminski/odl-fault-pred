library(tidyverse)
library(tikzDevice)

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
