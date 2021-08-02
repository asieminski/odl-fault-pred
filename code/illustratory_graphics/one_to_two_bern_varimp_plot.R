library(ggplot2)
library(tikzDevice)
library(mboost)
mod <- readRDS("res/final_models/1_2_day_forecasts/bern_gam_4098_p3.rds")

mod_to_varimp_plot <- function(mod){
  df <- data.frame(varimp(mod)) 
  dplyr::mutate(df,
    `Term type` = ifelse(variable %in% c("risk", "lightningCat"),
                         "smooth",
                         ifelse(stringr::str_detect(blearner, "bols"),
                                "linear", "smooth"))) |> 
    dplyr::filter(reduction > 0) |> 
    dplyr::mutate(total_reduction = reduction*length(fitted(mod)))
}
p <- mod_to_varimp_plot(mod) |> 
  ggplot(aes(variable, total_reduction/1000, fill = `Term type`)) + 
  geom_bar(stat = "identity", alpha = 0.7, colour = "black") + 
  coord_flip() +
  labs(x = "", y = "$\\Delta\\ell_{\\xi_0}$ in $000$'s")+
  theme_minimal()+
  scale_x_discrete(labels = rev(c("Region", 
                                  "Rain max.", 
                                  "Wind gust max.", 
                              "Temperature max.", 
                              "Risk", 
                              "Lightning category", 
                              "Wind direction $\\times$ Region $\\times$ Wind mean",
                              "Wind mean", 
                              "Wind direction $\\times$ Wind gust max.",
                              "Wind mean $\\times$ Region", 
                              "Wind direction $\\times$ Region", 
                              "Temperature min.",
                              "Wind direction")))+
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(), legend.position = "bottom")+
  scale_fill_manual(values = c("darkgrey", "firebrick"))
p
tikzDevice::tikz(file = "res/figures/varimp_bern_mod.tex", width = 5, height = 3)
print(p)
dev.off()
