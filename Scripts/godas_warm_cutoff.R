#Make Mean summer temp plots at all godas depths

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)

library(performance)
library(car)
library(broom)
library(modelr)
library(emmeans)

library(GGally)
library(visreg)
library(piecewiseSEM)
library(gt)
#filter data to just show summer months
godas <- read_csv("Outputs/godas_allyears_5_15_25m.csv") |> 
  filter(MM %in% 7:9,
         YY %in% 1989:2024)

View(godas)

#determine 95% quantile temp thresholds
quantile(godas$godas_15, probs = 0.95, na.rm = TRUE) #18.6
quantile(godas$godas_25, probs = 0.95, na.rm = TRUE) #17.2

Preheat_godas <- godas |>
  filter(YY %in% 1989:2004)

quantile(Preheat_godas$godas_15, probs = 0.95, na.rm = TRUE) #17.5
quantile(Preheat_godas$godas_25, probs = 0.95, na.rm = TRUE) #15.3

Postheat_godas <- godas |>
  filter(YY %in% 2004:2024)

quantile(Postheat_godas$godas_15, probs = 0.95, na.rm = TRUE) #19.6
quantile(Postheat_godas$godas_25, probs = 0.95, na.rm = TRUE) #17.9

#Visualize
hist(godas$godas_15)
hist(godas$godas_25)

#make long format
godas_monthly_long <- godas |>
  pivot_longer(cols = -c(Date, YY, MM), names_to = "Depth", values_to = "Temperature")

View(godas_monthly_long)

godas_means <- godas_monthly_long |>
  group_by(YY, Depth) |>
  summarise_at(vars(Temperature), list(mean = mean))

View(godas_means)
#plot
ggplot(godas_monthly_long, aes(x = Date, y = Temperature, color = Depth)) +
  geom_line(show.legend = FALSE)+
  facet_wrap(~Depth)

cbbPalette <- c("#E69F00", "#009E73", "#56B4E9")

#plot by depth
ggplot(transform(godas_monthly_long,
                 Depth=factor(Depth,levels=c("godas_5","godas_15","godas_25"))),
       aes(x = Date, y = Temperature, color = Depth)) +
  scale_colour_manual(values=cbbPalette)+
  theme_bw(base_size = 20)+
  geom_line(show.legend = FALSE)+
  labs(y = expression(paste("Temperature", "\u00b0C")))+
  facet_wrap(~Depth)
ggsave("Figures/godas_all_years_summer_plot.jpg")  


#plot means by depth
ggplot(transform(godas_means,
                 Depth=factor(Depth,levels=c("godas_5","godas_15","godas_25"))),
       aes(x = YY, y = mean, color = Depth)) +
  scale_colour_manual(values=cbbPalette)+
  stat_smooth(method="lm")+
  theme_bw(base_size = 20)+
  geom_line(show.legend = FALSE)+
  labs(y = expression(paste("Temperature", "\u00b0C")), x = expression(paste("Year")))+
  facet_wrap(~Depth)
ggsave("Figures/godas_summer_means_plot.jpg")  

##
# model
##
Temp_by_depth_and_yearlm <- lm(mean ~ Depth*YY,
                               data = godas_means)

##
# check model assumptions
##
check_model(Temp_by_depth_and_yearlm)


check_model(Temp_by_depth_and_yearlm, check = "linearity") |> plot()

tidy(Temp_by_depth_and_yearlm)
temp_means_em <- emmeans(Temp_by_depth_and_yearlm,  ~Depth)
temp_means_em2 <- emmeans(Temp_by_depth_and_yearlm, ~YY)

contrast(temp_means_em, method = "pairwise") |>
  confint()

car::Anova(Temp_by_depth_and_yearlm) 

emtrends(Temp_by_depth_and_yearlm, ~Depth, "YY") |> plot() +
  geom_vline(xintercept = 0, lty = 2)

#make a better looking table
gt::gt(Anova(Temp_by_depth_and_yearlm)) |> 
  gtsave("tables/temp_change_by_depth.docx")

modelbased::estimate_relation(Temp_by_depth_and_yearlm, by = c("YY", "Depth")) |> plot()

modelbased::estimate_relation(Temp_by_depth_and_yearlm, by = c("YY", "Depth")) |> 
  plot() +
  scale_colour_manual(values=cbbPalette)+ 
  facet_wrap(~Depth)

emmeans(Temp_by_depth_and_yearlm, ~YY|Depth, at = c(list(YY = c(1989, 2023))))
emmeans(Temp_by_depth_and_yearlm, ~Depth|YY, at = c(list(YY = c(1989, 2023))))  |> 
                                                      contrast(method = "pairwise", adjust = "none")
emmeans(Temp_by_depth_and_yearlm, ~Depth+YY, at = c(list(YY = c(1989, 2023))))  |> 
  contrast(method = "pairwise", adjust = "none")
