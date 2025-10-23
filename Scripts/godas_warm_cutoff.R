library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)

godas <- read_csv("Outputs/godas_allyears_5_15_25m.csv") |> 
  filter(MM %in% 7:9,
         YY %in% 1989:2024)

quantile(godas$godas_15, probs = 0.95, na.rm = TRUE) #19.13

quantile(godas$godas_25, probs = 0.95, na.rm = TRUE) #17.6

hist(godas$godas_15)
hist(godas$godas_25)

godas_monthly_long <- godas |>
  pivot_longer(cols = -c(Date, YY, MM), names_to = "Depth", values_to = "Temperature")

View(godas_monthly_long)

ggplot(godas_monthly_long, aes(x = Date, y = Temperature, color = Depth)) +
  geom_line(show.legend = FALSE)+
  facet_wrap(~Depth)

cbbPalette <- c("#E69F00", "#009E73", "#56B4E9")

ggplot(transform(godas_monthly_long,
                 Depth=factor(Depth,levels=c("godas_5","godas_15","godas_25"))),
       aes(x = Date, y = Temperature, color = Depth)) +
  scale_colour_manual(values=cbbPalette)+
  theme_bw(base_size = 20)+
  geom_line(show.legend = FALSE)+
  labs(y = expression(paste("Temperature", "\u00b0C")))+
  facet_wrap(~Depth)
ggsave("Figures/godas_all_years_summer_plot.jpg")  
