# Calculate Summer Mean temp across all Sebens Sites with interpolated GODAS Monthly data

setwd(here::here()) 

library(purrr)
library(dplyr)
library(lubridate)
library(tidyverse)
library(broom)



###################################################################################################################

interp_data <- read_csv("Outputs/godas_15_interpolated.csv")
View(interp_data)

#Get summer months only
interp_data2 <- interp_data |>
  filter(MM %in% c(6:9))|>
  group_by(YY, tidbit)|>
  summarize(mean = mean(temp_c_complete))|>
  ungroup()

View(interp_data2)

#Rename values in the tidbit variable
interp_data2 <- interp_data2 %>%
  mutate(tidbit = recode(tidbit, DB_Tidbit = "DB (7m)", HRI_Tidbit = "HRI (20m)", HRO_Tidbit =  "HRO (11m)", 
                         SHI_Tidbit = "SHI (8m)", SHO_Tidbit = "SHO (8m)" ))

#Plot means by site
All_means_plot <- ggplot(interp_data2, aes(x = YY,
                                                y = mean,
                                                colour = tidbit)) +
  geom_point(size=2)+
  geom_line()+
  labs(x = "Year",
       y = "Temperature (°C)")+
  theme_set(theme_classic(base_size = 18)) +
  ggtitle("Annual summer mean temperature across all sites")

ggsave("Figures/All_means_plot.jpg")

#Get total mean temp
interp_data3 <- interp_data2 |>
  filter(YY %in% c(1980:2022))|>
  group_by(YY)|>
  summarize(Average_temp = mean(mean))

total_mean <- mean(interp_data3$Average_temp)
print(total_mean) #11.66569

#Get Old and New temp means
interp_data4 <- interp_data2 |>
  filter(YY %in% c(1980:1985))|>
  group_by(YY)|>
  summarize(Average_temp = mean(mean))

old_mean <- mean(interp_data4$Average_temp)
print(old_mean) #10.71873

interp_data5 <- interp_data2 |>
  filter(YY %in% c(2015:2020))|>
  group_by(YY)|>
  summarize(Average_temp = mean(mean))

new_mean <- mean(interp_data5$Average_temp)
print(new_mean) #14.06974

#Get summer mean across sites
interp_data6 <- interp_data2 |>
  pivot_wider(names_from = tidbit, values_from = mean)

View(interp_data6)

Summer_means <- interp_data6 %>% 
  mutate(Average_temp= rowMeans(select(.,"DB (7m)", "HRI (20m)", "HRO (11m)", "SHI (8m)", 
                                       "SHO (8m)"), na.rm = TRUE))
View(Summer_means)

model <- lm(Average_temp ~YY, data = Summer_means)
performance::r2(model) #0.446
slope <- coef(model)[2] #0.08C/year

#Plot summer means across all sites

lm_eqn <- function(df){
  m <- lm(Average_temp ~YY, data = Summer_means);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(unname(coef(m)[1]), digits = 2),
                        b = format(unname(coef(m)[2]), digits = 2),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));
}

Summer_means_plot <- ggplot(Summer_means,aes(x=YY,y=Average_temp)) + 
  geom_point() + 
  geom_line(aes(y = Average_temp)) +
  geom_smooth(method="lm",formula=y~x,col="red")+
  theme_set(theme_classic(base_size = 18)) +
  annotate("text", x = 1985, y = 15.5, label = lm_eqn(Summer_means), parse = TRUE)+
  ggtitle("Interpolated summer mean temperature across all sites") +
  labs(y = expression(paste("Temperature", "\u00b0C")), x = expression("Date"))
  
ggsave("Figures/Summer_means_plot.jpg")

#Plot summer means across all sites with line showing Old and New means
Summer_means_tipping_point_plot <- ggplot(Summer_means,aes(x=YY,y=Average_temp)) + 
  geom_point() + 
  geom_line(aes(y = Average_temp)) +
  geom_smooth(method="lm",formula=y~x,col="red")+
  geom_hline(yintercept = 10.71873, col = "purple")+
  geom_hline(yintercept = 14.06974, col = "turquoise")+
  theme_set(theme_classic(base_size = 18)) +
  annotate("text", x = 1985, y = 15.5, label = lm_eqn(Summer_means), parse = TRUE)+
  ggtitle("Interpolated summer mean temperature across all sites") +
  labs(y = expression(paste("Temperature", "\u00b0C")), x = expression("Date"))

ggsave("Figures/Summer_means_tipping_point_plot.jpg")
