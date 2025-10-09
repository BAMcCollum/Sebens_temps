#Make a GODAS temp change over time plot
setwd(here::here())

library(tidyverse)

godas_monthly <- read_csv("Outputs/godas_allyears_5_15_25m.csv")

godas_monthly_long <- godas_monthly |>
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
  theme_bw()+
  geom_line(show.legend = FALSE)+
  facet_wrap(~Depth)
ggsave("Figures/godas_all_years_plot.jpg")  
