library(splines)
library(tidyverse)
library(tikzDevice)
splines::bs(x = 0:100, degree = 1,intercept = FALSE, knots = 1:4*20)


x <- 0:100
x_fac <- cut(0:100, breaks = 5)
x_mod <- model.matrix(~x_fac-1 + x)
df_zero_degree <- as_tibble(x_mod) |> 
  pivot_longer(-x, names_to = "Basis") |> 
  mutate(Basis = rep(str_c("B", 1:5), n()/5),
         Degree = str_c("D = ", 0)) 

df_zero_degree

for(deg in 1:3){
  
  df_zero_degree <- 
    splines::bs(x = 0:100, degree = deg, 
                intercept = TRUE,
                knots = 1:4 * 20) |>
    as_tibble() |>
    mutate(x = x) |>
    pivot_longer(-x, names_to = "Basis") |>
    mutate(Basis = str_c("B", Basis),
           Degree = str_c("D = ", deg)) |>
    rbind(df_zero_degree)
}
tikz("res/splines.tex", width = 5.5, 
     height = 3)
p <- df_zero_degree |>
  ggplot(aes(x = x, y = value, colour = Basis)) +
    geom_line()+
    scale_y_continuous(breaks = 0:1)+
    scale_x_continuous(breaks = 0:5 * 20)+
    theme_minimal()+
    facet_wrap(~Degree)+
    scale_color_viridis_d() +
    labs(y = "", x = "")+
    theme(panel.grid.minor = element_blank(),
          legend.position = "none")

print(p)
dev.off()