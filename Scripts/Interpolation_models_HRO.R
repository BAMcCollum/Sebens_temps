# Interpolation models for Halfway Rock Outer

setwd(here::here()) 

library(purrr)
library(dplyr)
library(lubridate)
library(tidyverse)
library(broom)

###################################################################################################################

#data inputs
Tidbit_and_Buoy_data <- read_csv("Outputs/Tidbit_and_Buoy_data.csv")%>%
  mutate(MM=as.character(MM))

#str(Tidbit_and_Buoy_data)
#visdat::vis_dat(Tidbit_and_Buoy_data)
View(Tidbit_and_Buoy_data)
#######################################################################################################
#### HRO Models ####

#fit a MLR model to each temperature record (Buoys 44029, 44013, 44005, 44098, & OISST)

HROmod_44029_2m <- lm(HRO_Tidbit ~ Buoy44029_2m *MM, data = Tidbit_and_Buoy_data) 
car::Anova(HROmod_44029_2m)
#broom::glance(HROmod_44029_2m)
performance::r2(HROmod_44029_2m) #0.777
#summary(HROmod_44029_2m)

HROmod_44029_20m <- lm(HRO_Tidbit ~ Buoy44029_20m *MM, data = Tidbit_and_Buoy_data)
car::Anova(HROmod_44029_20m)
#broom::glance(HROmod_44029_20m)
performance::r2(HROmod_44029_20m) #0.879
#summary(HROmod_44029_20m)

HROmod_44005 <- lm(HRO_Tidbit ~ Buoy44005 *MM, data = Tidbit_and_Buoy_data)
car::Anova(HROmod_44005)
#broom::glance(HROmod_44005)
performance::r2(HROmod_44005) #0.808
#summary(HROmod_44005)

HROmod_44013 <- lm(HRO_Tidbit ~ Buoy44013 *MM, data = Tidbit_and_Buoy_data)
car::Anova(HROmod_44013)
#broom::glance(HROmod_44013)
performance::r2(HROmod_44013) #0.834
#summary(HROmod_44013)

HROmod_44098 <- lm(HRO_Tidbit ~ Buoy44098 *MM, data = Tidbit_and_Buoy_data)
car::Anova(HROmod_44098)
#broom::glance(HROmod_44098)
performance::r2(HROmod_44098) #0.813
#summary(HROmod_44098)

HROmod_OISST <- lm(HRO_Tidbit ~ OISST *MM, data = Tidbit_and_Buoy_data)
car::Anova(HROmod_OISST)
#broom::glance(HROmod_OISST)
performance::r2(HROmod_OISST) #0.829
#summary(HROmod_OISST)

HROmod_HRI <- lm(HRO_Tidbit ~ HRI_Tidbit *MM, data = Tidbit_and_Buoy_data)
car::Anova(HROmod_HRI)
#broom::glance(HROmod_HRI)
performance::r2(HROmod_HRI) #0.883
#summary(HROmod_HRI)

HROmod_DB <- lm(HRO_Tidbit ~ DB_Tidbit *MM, data = Tidbit_and_Buoy_data)
car::Anova(HROmod_DB)
#broom::glance(HROmod_DB)
performance::r2(HROmod_DB) #0.910
#summary(HROmod_DB)

HROmod_SHI <- lm(HRO_Tidbit ~ SHI_Tidbit *MM, data = Tidbit_and_Buoy_data)
car::Anova(HROmod_SHI)
#broom::glance(HROmod_SHI)
performance::r2(HROmod_SHI) #0.919
#summary(HROmod_SHI)

HROmod_SHO <- lm(HRO_Tidbit ~ SHO_Tidbit *MM, data = Tidbit_and_Buoy_data)
car::Anova(HROmod_SHO)
#broom::glance(HROmod_SHO)
performance::r2(HROmod_SHO) #0.919
#summary(HROmod_SHO)

#show the fit

#Buoy 44029_2m
Tidbit_and_Buoy_data %>%
  modelr::add_predictions(HROmod_44029_2m) %>%
  tidyr::pivot_longer(cols = c(Buoy44029_2m, HRO_Tidbit, pred),
                      names_to = "type",
                      values_to = "HRO_Tidbit") %>%
  filter(type != "Buoy44029_2m") %>%
  ggplot(aes(x = Date, y = HRO_Tidbit, color = type)) +
  geom_point(alpha = 0.5) +
  facet_wrap(~MM, scale = "free_x")+
  ggtitle("Buoy 44029_2M - HRO model fit by Month")

#Buoy 44029_20m
Tidbit_and_Buoy_data %>%
  modelr::add_predictions(HROmod_44029_20m) %>%
  tidyr::pivot_longer(cols = c(Buoy44029_20m, HRO_Tidbit, pred),
                      names_to = "type",
                      values_to = "HRO_Tidbit") %>%
  filter(type != "Buoy44029_20m") %>%
  ggplot(aes(x = Date, y = HRO_Tidbit, color = type)) +
  geom_point(alpha = 0.5) +
  facet_wrap(~MM, scale = "free_x")+
  ggtitle("Buoy 44029_20M - HRO model fit by Month")

#Buoy 44005
Tidbit_and_Buoy_data %>%
  modelr::add_predictions(HROmod_44005) %>%
  tidyr::pivot_longer(cols = c(Buoy44005, HRO_Tidbit, pred),
                      names_to = "type",
                      values_to = "HRO_Tidbit") %>%
  filter(type != "Buoy44005") %>%
  ggplot(aes(x = Date, y = HRO_Tidbit, color = type)) +
  geom_point(alpha = 0.5) +
  facet_wrap(~MM, scale = "free_x")+
  ggtitle("Buoy 44005 - HRO model fit by Month")

#Buoy 44013
Tidbit_and_Buoy_data %>%
  modelr::add_predictions(HROmod_44013) %>%
  tidyr::pivot_longer(cols = c(Buoy44013, HRO_Tidbit, pred),
                      names_to = "type",
                      values_to = "HRO_Tidbit") %>%
  filter(type != "Buoy44013") %>%
  ggplot(aes(x = Date, y = HRO_Tidbit, color = type)) +
  geom_point(alpha = 0.5) +
  facet_wrap(~MM, scale = "free_x")+
  ggtitle("Buoy 44013 - HRO model fit by Month")

#Buoy 44098
Tidbit_and_Buoy_data %>%
  modelr::add_predictions(HROmod_44098) %>%
  tidyr::pivot_longer(cols = c(Buoy44098, HRO_Tidbit, pred),
                      names_to = "type",
                      values_to = "HRO_Tidbit") %>%
  filter(type != "Buoy44098") %>%
  ggplot(aes(x = Date, y = HRO_Tidbit, color = type)) +
  geom_point(alpha = 0.5) +
  facet_wrap(~MM, scale = "free_x")+
  ggtitle("Buoy 44098 - HRO model fit by Month")

#OISST
Tidbit_and_Buoy_data %>%
  modelr::add_predictions(HROmod_OISST) %>%
  tidyr::pivot_longer(cols = c(OISST, HRO_Tidbit, pred),
                      names_to = "type",
                      values_to = "HRO_Tidbit") %>%
  filter(type != "OISST") %>%
  ggplot(aes(x = Date, y = HRO_Tidbit, color = type)) +
  geom_point(alpha = 0.5) +
  facet_wrap(~MM, scale = "free_x")+
  ggtitle("OISST - HRO model fit by Month")

#######################################################################################################
#'--------------------------------------------------------------
#' Create extrapolated records based on model fits & Real data
#'--------------------------------------------------------------

#make buoy predicted Data Frames

#HRO-Buoy44029_2m
make_pred_ts <- function(dat, HROmod_44029_2m){ # 'dat' is created in/for/by the function in the next line
  pred_frame <- dat #now 'dat' exists
  pred_frame <-pred_frame %>%
    modelr::add_predictions(HROmod_44029_2m)
  pred_frame <- left_join(pred_frame, Tidbit_and_Buoy_data)
  pred_frame %>%
    mutate(HRO_bestguess = ifelse(is.na(HRO_Tidbit), pred, HRO_Tidbit))
}

#Make those predictions!
buoy44029_2m_predHRO <- make_pred_ts(Tidbit_and_Buoy_data, HROmod_44029_2m)

#Save those predictions!
saveRDS(buoy44029_2m_predHRO, "Sebens_temps/Outputs/HRObuoy44029_2m_pred.RDS")
#readr::write_csv(buoy44029_2m_predHRO, "Sebens_temps/Outputs/HRObuoy44029_2m_pred.csv")

#test
ggplot(buoy44029_2m_predHRO,
       aes(x = Date)) +
  geom_line(aes(y = pred), color = "red") +
  geom_point(aes(y = HRO_Tidbit), color = "blue", size = 0.1)+
  ggtitle("Buoy 44029 (2m depth) Interpolated prediction of HRO")

#HRO-Buoy44029_20m
make_pred_ts <- function(dat, HROmod_44029_20m){ # 'dat' is created in/for/by the function in the next line
  pred_frame <- dat #now 'dat' exists
  pred_frame <-pred_frame %>%
    modelr::add_predictions(HROmod_44029_20m)
  pred_frame <- left_join(pred_frame, Tidbit_and_Buoy_data)
  pred_frame %>%
    mutate(HRO_bestguess = ifelse(is.na(HRO_Tidbit), pred, HRO_Tidbit))
}

#Make those predictions!
buoy44029_20m_predHRO <- make_pred_ts(Tidbit_and_Buoy_data, HROmod_44029_20m)

#Save those predictions!
saveRDS(buoy44029_20m_predHRO, "Sebens_temps/Outputs/HRObuoy44029_20m_pred.RDS")
#readr::write_csv(buoy44029_20m_pred, "Sebens_temps/Outputs/buoy44029_20m_pred.csv")

#test
ggplot(buoy44029_20m_predHRO,
       aes(x = Date)) +
  geom_line(aes(y = pred), color = "red") +
  geom_point(aes(y = HRO_Tidbit), color = "blue", size = 0.1)+
  ggtitle("Buoy 44029 (20m depth) Interpolated prediction of HRO")

#Buoy44005
make_pred_ts <- function(dat, HROmod_44005){ # 'dat' is created in/for/by the function in the next line
  pred_frame <- dat #now 'dat' exists
  pred_frame <-pred_frame %>%
    modelr::add_predictions(HROmod_44005)
  pred_frame <- left_join(pred_frame, Tidbit_and_Buoy_data)
  pred_frame %>%
    mutate(HRO_bestguess = ifelse(is.na(HRO_Tidbit), pred, HRO_Tidbit))
}

#Make those predictions!
buoy44005_predHRO <- make_pred_ts(Tidbit_and_Buoy_data, HROmod_44005)

#Save those predictions!
saveRDS(buoy44005_predHRO, "Sebens_temps/Outputs/buoy44005_predHRO.RDS")
#readr::write_csv(buoy44005_pred, "Sebens_temps/Outputs/buoy44005_pred.csv")

#test
ggplot(buoy44005_predHRO,
       aes(x = Date)) +
  geom_line(aes(y = pred), color = "red") +
  geom_point(aes(y = HRO_Tidbit), color = "blue", size = 0.1)+
  ggtitle("Buoy 44005 Interpolated prediction of HRO")

#Buoy44013
make_pred_ts <- function(dat, HROmod_44013){ # 'dat' is created in/for/by the function in the next line
  pred_frame <- dat #now 'dat' exists
  pred_frame <-pred_frame %>%
    modelr::add_predictions(HROmod_44013)
  pred_frame <- left_join(pred_frame, Tidbit_and_Buoy_data)
  pred_frame %>%
    mutate(HRO_bestguess = ifelse(is.na(HRO_Tidbit), pred, HRO_Tidbit))
}

#Make those predictions!
buoy44013_predHRO <- make_pred_ts(Tidbit_and_Buoy_data, HROmod_44013)

#Save those predictions!
saveRDS(buoy44013_predHRO, "Sebens_temps/Outputs/buoy44013_predHRO.RDS")
#readr::write_csv(buoy44013_pred, "Sebens_temps/Outputs/buoy44013_pred.csv")

#test
ggplot(buoy44013_predHRO,
       aes(x = Date)) +
  geom_line(aes(y = pred), color = "red") +
  geom_point(aes(y = HRO_Tidbit), color = "blue", size = 0.1)+
  ggtitle("Buoy 44013 Interpolated prediction of HRO")


#Buoy44098
make_pred_ts <- function(dat, HROmod_44098){ # 'dat' is created in/for/by the function in the next line
  pred_frame <- dat #now 'dat' exists
  pred_frame <-pred_frame %>%
    modelr::add_predictions(HROmod_44098)
  pred_frame <- left_join(pred_frame, Tidbit_and_Buoy_data)
  pred_frame %>%
    mutate(HRO_bestguess = ifelse(is.na(HRO_Tidbit), pred, HRO_Tidbit))
}

#Make those predictions!
buoy44098_predHRO <- make_pred_ts(Tidbit_and_Buoy_data, HROmod_44098)

#Save those predictions!
saveRDS(buoy44098_predHRO, "Sebens_temps/Outputs/buoy44098_predHRO.RDS")
#readr::write_csv(buoy44098_pred, "Sebens_temps/Outputs/buoy44098_pred.csv")

#test
ggplot(buoy44098_predHRO,
       aes(x = Date)) +
  geom_line(aes(y = pred), color = "red") +
  geom_point(aes(y = HRO_Tidbit), color = "blue", size = 0.1)+
  ggtitle("Buoy 44098 Interpolated prediction of HRO")


#For HRO-OISST
make_pred_ts <- function(dat, HROmod_OISST){ # 'dat' is created in/for/by the function in the next line
  pred_frame <- dat #now 'dat' exists
  pred_frame <-pred_frame %>%
    modelr::add_predictions(HROmod_OISST)
  pred_frame <- left_join(pred_frame, Tidbit_and_Buoy_data)
  pred_frame %>%
    mutate(HRO_bestguess = ifelse(is.na(HRO_Tidbit), pred, HRO_Tidbit))
}

#Make those predictions!
oisst_predHRO <- make_pred_ts(Tidbit_and_Buoy_data, HROmod_OISST)

#Save those predictions!
saveRDS(oisst_predHRO, "Sebens_temps/Outputs/HROoisst_pred.RDS")
#readr::write_csv(oisst_pred, "Sebens_temps/Outputs/oisst_pred.csv")

#test
ggplot(oisst_predHRO,
       aes(x = Date)) +
  geom_line(aes(y = pred), color = "red") +
  geom_point(aes(y = HRO_Tidbit), color = "blue", size = 0.1)+
  ggtitle("OISST Interpolated prediction of HRO")

#For HRI-HRO
make_pred_ts <- function(dat, HROmod_HRI){ # 'dat' is created in/for/by the function in the next line
  pred_frame <- dat #now 'dat' exists
  pred_frame <-pred_frame %>%
    modelr::add_predictions(HROmod_HRI)
  pred_frame <- left_join(pred_frame, Tidbit_and_Buoy_data)
  pred_frame %>%
    mutate(HRO_bestguess = ifelse(is.na(HRO_Tidbit), pred, HRO_Tidbit))
}

#Make those predictions!
HROmod_HRI_pred <- make_pred_ts(Tidbit_and_Buoy_data, HROmod_HRI)

#Save those predictions!
saveRDS(HROmod_HRI_pred, "Sebens_temps/Outputs/HROmod_HRI_pred.RDS")
#readr::write_csv(HROmod_HRI_pred, "Sebens_temps/Outputs/HROmod_HRI_pred.csv")

#test
ggplot(HROmod_HRI_pred,
       aes(x = Date)) +
  geom_line(aes(y = pred), color = "red") +
  geom_point(aes(y = HRO_Tidbit), color = "blue", size = 0.1)+
  ggtitle("HRI Interpolated prediction of HRO")

#For DB-HRO
make_pred_ts <- function(dat, HROmod_DB){ # 'dat' is created in/for/by the function in the next line
  pred_frame <- dat #now 'dat' exists
  pred_frame <-pred_frame %>%
    modelr::add_predictions(HROmod_DB)
  pred_frame <- left_join(pred_frame, Tidbit_and_Buoy_data)
  pred_frame %>%
    mutate(HRO_bestguess = ifelse(is.na(HRO_Tidbit), pred, HRO_Tidbit))
}

#Make those predictions!
HROmod_DB_pred <- make_pred_ts(Tidbit_and_Buoy_data, HROmod_DB)

#Save those predictions!
saveRDS(HROmod_DB_pred, "Sebens_temps/Outputs/HROmod_HRI_pred.RDS")
#readr::write_csv(HROmod_HRI_pred, "Sebens_temps/Outputs/HROmod_HRI_pred.csv")

#test
ggplot(HROmod_DB_pred,
       aes(x = Date)) +
  geom_line(aes(y = pred), color = "red") +
  geom_point(aes(y = HRO_Tidbit), color = "blue", size = 0.1)+
  ggtitle("DB Interpolated prediction of HRO")

#For SHI-HRO
make_pred_ts <- function(dat, HROmod_SHI){ # 'dat' is created in/for/by the function in the next line
  pred_frame <- dat #now 'dat' exists
  pred_frame <-pred_frame %>%
    modelr::add_predictions(HROmod_SHI)
  pred_frame <- left_join(pred_frame, Tidbit_and_Buoy_data)
  pred_frame %>%
    mutate(HRO_bestguess = ifelse(is.na(HRO_Tidbit), pred, HRO_Tidbit))
}

#Make those predictions!
HROmod_SHI_pred <- make_pred_ts(Tidbit_and_Buoy_data, HROmod_SHI)

#Save those predictions!
saveRDS(HROmod_SHI_pred, "Sebens_temps/Outputs/HROmod_SHI_pred.RDS")
#readr::write_csv(HROmod_HRI_pred, "Sebens_temps/Outputs/HROmod_HRI_pred.csv")

#test
ggplot(HROmod_SHI_pred,
       aes(x = Date)) +
  geom_line(aes(y = pred), color = "red") +
  geom_point(aes(y = HRO_Tidbit), color = "blue", size = 0.1)+
  ggtitle("SHI Interpolated prediction of HRO")

#For SHO-HRO
make_pred_ts <- function(dat, HROmod_SHO){ # 'dat' is created in/for/by the function in the next line
  pred_frame <- dat #now 'dat' exists
  pred_frame <-pred_frame %>%
    modelr::add_predictions(HROmod_SHO)
  pred_frame <- left_join(pred_frame, Tidbit_and_Buoy_data)
  pred_frame %>%
    mutate(HRO_bestguess = ifelse(is.na(HRO_Tidbit), pred, HRO_Tidbit))
}

#Make those predictions!
HROmod_SHO_pred <- make_pred_ts(Tidbit_and_Buoy_data, HROmod_SHO)

#Save those predictions!
saveRDS(HROmod_SHO_pred, "Sebens_temps/Outputs/HROmod_SHO_pred.RDS")
#readr::write_csv(HROmod_SHO_pred, "Sebens_temps/Outputs/HROmod_SHO_pred.csv")

#test
ggplot(HROmod_SHO_pred,
       aes(x = Date)) +
  geom_line(aes(y = pred), color = "red") +
  geom_point(aes(y = HRO_Tidbit), color = "blue", size = 0.1)+
  ggtitle("SHO Interpolated prediction of HRO")

#######################################################################################################
#######################################################################################################
#'--------------------------------------------------------------
#' Create Hierarchical Supplementary Tidbit Data
#'--------------------------------------------------------------

# Select Needed Columns from Predictions
#HRO

View(HROmod_HRI_pred)
Interp_HRI_HRO <- as.data.frame(HROmod_HRI_pred) %>%
  subset(., select = c(1:4,12,16)) %>%
  rename(pred_via_HRI = 'pred')

Interp_DB_HRO <- as.data.frame(HROmod_DB_pred) %>%
  subset(., select = c(1:4,12,16)) %>%
  rename(pred_via_DB = 'pred')

Interp_SHI_HRO <- as.data.frame(HROmod_SHI_pred) %>%
  subset(., select = c(1:4,12,16)) %>%
  rename(pred_via_SHI = 'pred')

Interp_SHO_HRO <- as.data.frame(HROmod_SHO_pred) %>%
  subset(., select = c(1:4,12,16)) %>%
  rename(pred_via_SHO = 'pred')

Interp_HRO_44029_20m <- buoy44029_20m_predHRO %>%
  subset(., select = c(1:4,12,16)) %>%
  rename(pred_via_44029_20m = 'pred')

Interp_HRO_44029_2m <- buoy44029_2m_predHRO %>%
  subset(., select = c(1:4,12,16)) %>%
  rename(pred_via_44029_2m = 'pred')

Interp_HRO_440013 <- buoy44013_predHRO %>%
  subset(., select = c(1:4,12,16)) %>%
  rename(pred_via_44013 = 'pred')

Interp_HRO_440098 <- buoy44098_predHRO %>%
  subset(., select = c(1:4,12,16)) %>%
  rename(pred_via_44098 = 'pred')

Interp_HRO_440005 <- buoy44005_predHRO %>%
  subset(., select = c(1:4,12,16)) %>%
  rename(pred_via_44005 = 'pred')

Interp_HRO_OISST <- oisst_predHRO %>%
  subset(., select = c(1:4,12,16)) %>%
  rename(pred_via_OISST = 'pred')

HRO_Interpolation_raw <- (list(Interp_DB_HRO,Interp_HRO_44029_20m,Interp_HRI_HRO,Interp_HRO_440013,
                               Interp_HRO_440098,Interp_HRO_440005,Interp_HRO_OISST, Interp_SHI_HRO,
                               Interp_SHO_HRO,Interp_HRO_44029_2m) %>% 
                            reduce(left_join))

HRO_Interpolation <- HRO_Interpolation_raw %>%
  mutate(final_value = 
           case_when(
             !is.na(HRO_Tidbit) ~ HRO_Tidbit,
             !is.na(pred_via_SHO) ~ pred_via_SHO,
             !is.na(pred_via_SHI) ~ pred_via_SHI,
             !is.na(pred_via_DB) ~ pred_via_DB,
             !is.na(pred_via_HRI) ~ pred_via_HRI,
             !is.na(pred_via_44029_20m) ~ pred_via_44029_20m,
             !is.na(pred_via_44013) ~ pred_via_44013,
             !is.na(pred_via_OISST) ~ pred_via_OISST,
             !is.na(pred_via_44098) ~ pred_via_44098,
             !is.na(pred_via_44005) ~ pred_via_44005,
             !is.na(pred_via_44029_2m) ~ pred_via_44029_2m,
             TRUE ~ NA_real_))

View(HRO_Interpolation)
#write_csv(HRO_Interpolation,"Outputs/HRO_Interpolation.csv")

#############################################################
#Plot
HRO_Interpolation_plot <- ggplot(HRO_Interpolation,
       aes(x = Date)) +
  geom_line(aes(y = final_value), color = "red") +
  geom_point(aes(y = HRO_Tidbit), color = "blue", size = 0.1)+
  ggtitle("Interpolated prediction of HRO Temperatures at 11m")+
  labs(y = expression(paste("Temperature", "\u00b0C")), x = expression("Date"))

ggsave("Figures/HRO_Interpolation_plot.jpg")

##################################################################################################################
# NOW TRY TO RECREATE FOR Annual summer means RATHER THAN DAILY MEANS!
###################################################################################################################### Notes and other code bits ####

HRO_Interpolation2 <- HRO_Interpolation |>
  filter(MM %in% c(6:9))|>
  group_by(YY)|>
  summarize(mean = mean(final_value))

HRO_Summer_plot <- ggplot(HRO_Interpolation2,
       aes(x = YY)) +
  geom_line(aes(y = mean))+
  geom_point(aes(y = mean))+
  theme_set(theme_classic(base_size = 18)) +
  ggtitle("Interpolated prediction of HRO Temperatures at 11m") +
  labs(y = expression(paste("Mean Annual Summer Temperature", "\u00b0C")), x = expression("Date"))

ggsave("Figures/HRO_Summer_plot.jpg")

#write_csv(HRO_Interpolation2,"Outputs/HRO_Interpolation_Summer.csv")
