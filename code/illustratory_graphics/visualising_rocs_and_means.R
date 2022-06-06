library(tidyverse)
library(gamlss.dist)
library(gamlss.inf)
gamlss.inf::gen.Zadj(family = "BCTo")
df1_2 <- readRDS("res/1_2_test_data_augmented.rds")

df <- df1_2 |> 
    dplyr::select(day, faultDate, faultCount, xi0:tau) |> 
    dplyr::mutate(day = as.character(day))

quants <- df |> 
  dplyr::select(mu:tau, xi0) |> 
  summarise(across(mu:xi0, \(x)quantile(x, 1:3/4))) |> 
  mutate(quants = 1:3) 
# Initialise
res <- quants[NULL,] |> 
  unnest(everything()) |> 
  mutate_at("quants", as.character)
for(i in 1:5){
  parameter_name <- colnames(quants)[i]
  #print(parameter_name)
  new_res <- expand_grid(a = quants[2, -i], b = quants[, i])  |> 
    unnest(everything()) |> 
    mutate(quants = str_c(parameter_name, ", q",25*1:3))
  #print(new_res)
  res <- bind_rows(res, new_res)
}

plot_data <- expand_grid(res, x = seq(0, to = 6, length.out = 1000)) |> 
  mutate(dens = ifelse(x == 0, xi0,
                       dBCTo(x, mu, sigma, nu, tau) * (1-xi0))) |> 
  mutate(quants = ifelse(str_detect(quants, "q50"), "sigma, q50", quants)) |> 
  distinct() |> 
  mutate(
    param = str_c("\\", 
                  str_split(quants, ", ") |>
                   map_chr(pluck, 1) |> 
                   str_replace("xi0", "xi_0")
                  ) |> 
            factor(ordered = TRUE, 
                  levels = c("\\xi_0", "\\mu", "\\sigma", "\\nu", "\\tau"),
                  labels = str_c("$", c("\\xi_0", "\\mu", "\\sigma", "\\nu", "\\tau"), "$")),
    quantile = map_chr(str_split(quants, ", "), pluck, 2)
    )

plot_col <- "firebrick"
p <- ggplot(plot_data)+
  geom_segment(aes(yend = xi0), 
               x = 0, xend = 0, y = 0, 
               data = filter(plot_data, x == 0), color = plot_col) +
  geom_point(aes(y = xi0), x = 0, 
             data = filter(plot_data, x == 0), color = plot_col) +
  facet_grid(param~quantile) +
  geom_line(aes(x, dens), data = filter(plot_data, x != 0), 
            color = plot_col) +
  theme_bw() +
  theme(panel.grid = element_blank())+
  labs(y = NULL, x = "Fault count")+
  coord_cartesian(ylim = c(0, 0.75)) +
  geom_text(data = plot_data |> 
               filter(param == "$\\sigma$", quantile == "q50") |> 
               slice(1), 
             x = 3.5, y = 0.4, label = "All parameters\nat median",
            colour = "black", size = 3)

tikzDevice::tikz("res/figures/zadj_bcto_vis.tex", 
                 height = 7, width = 5.5)
print(p)
dev.off()
beepr::beep()
######################################

for(i in 1:nrow(df)){
  cat("\niteration:", i, "\n")
  df$p_over_14[i] <- 
    pBCToZadj(14, df$mu[i], df$sigma[i], df$nu[i],
              df$tau[i], df$xi0[i], 
              lower.tail = FALSE)   
}
# Draw 1000 samples and calc their mean
# df$y_hat <- 0
# for(i in 1:nrow(df)){
#   cat("\niteration:", i, "\n")
#   y_hat <- mean(rBCToZadj(1e4, df$mu[i], df$sigma[i], 
#                  df$nu[i], df$tau[i], df$xi0[i]))   
#   df$y_hat[i] <- y_hat
# }

df$p_over_14 <- 0
for(i in 1:nrow(df)){
  cat("\niteration:", i, "\n")
  df$p_over_14[i] <- 
    pBCToZadj(14, df$mu[i], df$sigma[i], df$nu[i],
              df$tau[i], df$xi0[i], 
              lower.tail = FALSE)   
}

df$is_geq_14 <- df$faultCount >= 14

library(pROC)
ROCS <- list()
n_days <- length(unique(df$day))
length(ROCS) <- n_days
for(i in 1:n_days){
  day_ind <- which(df$day == as.character(i))
  ROCS[[i]] <- pROC::roc(
    df$is_geq_14[day_ind], 
    df$p_over_14[day_ind]
    ) 
}

p_roc <- tibble(roc = lapply(ROCS, coords), 
       day = as.ordered(1:length(ROCS)),
       roc_auc = lapply(ROCS, \(x) auc(x)[[1]])
       ) |> 
unnest(cols = c(roc, roc_auc)) |> 
mutate(model = ifelse(day %in% as.ordered(1:2),
              "1-2 day forecast", "3-5 day forecast"),
       `Day; ROC AUC` = str_c(as.character(day),
                   "; ", round(roc_auc, 2)
                   ) |> as.ordered()) |> 
  ggplot(aes(1 - specificity, sensitivity, colour = `Day; ROC AUC`)) + 
    geom_line(size = 1.1) +
    geom_line(aes(x, y), tibble(x = 0:1, y = 0:1), 
              color = "darkgrey", linetype = "dashed")+
    facet_wrap(~model) +
    theme_minimal()+
    scale_color_brewer(type = "qual",palette = 3)+
    labs(x = "False Positive Rate", 
         y = "True Positive Rate") + 
    theme(legend.position = "right") 
tikzDevice::tikz("res/figures/roc_curves.tex", 
                 height = 2.5, width = 5.5)
p_roc
dev.off()

# 
# 
# 
# 
# for(i in 1:10){
#   plotBCToZadj(df$mu[i], df$sigma[i], 
#                df$nu[i], df$tau[i], df$xi0[i])
#     
#   params <- c(df$mu[i], df$sigma[i], df$nu[i], 
#               df$tau[i], df$xi0[i])
#   names(params) <- c("mu", "sigma", "nu", 
#                      "tau",  "xi0")
#   
#   title(xlab = "Number of faults",
#     main = 
#           paste0(
#           paste0(df$region[i], "\n"),
#           paste(names(params),
#                 round(params, 2),
#                 sep = "=") |>
#           paste0(collapse = "; ")))
#   abline(v = mean(
#     rBCToZadj(1e4, df$mu[i], df$sigma[i], 
#               df$nu[i], df$tau[i], df$xi0[i])),
#     col = "firebrick")
#   rug(x = df$faultCount[i], 
#     col = "green", ticksize = 0.15 ,lwd = 2)
# }
# dev.off()
# rBCToZadj(1000, df$mu[i], df$sigma[i], 
#              df$nu[i], df$tau[i], df$xi0[i]) |> summary()
# i <- which.max(df$y_hat)
# 
# 
# df |> 
#   ggplot(aes(x = faultDate)) +
#   geom_line(mapping = aes(y = -log(y_hat), colour = day)) +
#   geom_point(mapping = aes(y = -log(y_hat), colour = day)) +
#   geom_line(mapping = aes(y = faultCount, group = day),
#             colour = "blue") +
#   coord_cartesian(ylim = c(-40, 40), 
#                   xlim = as.Date(c("2018-01-01", "2019-01-01")))+
#   geom_hline(yintercept = c(-15, 15), linetype = "dashed")+
#   facet_wrap(~region, nrow = 3)
#   
# table(df$faultCount>15, as.numeric(df$y_hat >15))
# library(tsibble)
# library(fpp3)
# library(fable)

