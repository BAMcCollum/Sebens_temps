# Interpolation models for Halfway Rock Inner

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
#View(Tidbit_and_Buoy_data)
#######################################################################################################


#### HRI Models ####
#fit a MLR model to each temperature record (Buoys 44029, 44013, 44005, 44098, OISST, HRO, & DB)

#Buoy44029_2m
mod_44029_2m <- lm(HRI_Tidbit ~ Buoy44029_2m *MM, data = Tidbit_and_Buoy_data) 
car::Anova(mod_44029_2m)
#broom::glance(mod_44029_2m)
performance::r2(mod_44029_2m) #0.810
#summary(mod_44029_2m)

Buoy44029_2m_corplot <- ggplot(Tidbit_and_Buoy_data, aes(x = Buoy44029_2m, y = HRI_Tidbit, color = MM))+
  geom_point()+
  stat_smooth(method = "lm")

ggsave("Figures/Buoy44029_2m_corplot.pdf")


#Buoy44029_20m
mod_44029_20m <- lm(HRI_Tidbit ~ Buoy44029_20m *MM, data = Tidbit_and_Buoy_data) 
car::Anova(mod_44029_20m)
#broom::glance(mod_44029_2m)
performance::r2(mod_44029_20m) #0.934
#summary(mod_44029_2m)

Buoy44029_20m_corplot <- ggplot(Tidbit_and_Buoy_data, aes(x = Buoy44029_20m, y = HRI_Tidbit, color = MM))+
  geom_point()+
  stat_smooth(method = "lm")

ggsave("Figures/Buoy44029_20m_corplot.pdf")


#Buoy44005
mod_44005 <- lm(HRI_Tidbit ~ Buoy44005 *MM, data = Tidbit_and_Buoy_data)
summary(mod_44005)
car::Anova(mod_44005)
#broom::glance(mod_44005)
performance::r2(mod_44005) #0.833

Buoy44005_corplot <- ggplot(Tidbit_and_Buoy_data, aes(x = Buoy44005, y = HRI_Tidbit, color = MM))+
  geom_point()+
  stat_smooth(method = "lm")

ggsave("Figures/Buoy44005_corplot.pdf")


#Buoy44013
mod_44013 <- lm(HRI_Tidbit ~ Buoy44013 *MM, data = Tidbit_and_Buoy_data)
summary(mod_44013)
car::Anova(mod_44013)
#broom::glance(mod_44013)
performance::r2(mod_44013) #0.834

Buoy44013_corplot <- ggplot(Tidbit_and_Buoy_data, aes(x = Buoy44013, y = HRI_Tidbit, color = MM))+
  geom_point()+
  stat_smooth(method = "lm")

ggsave("Figures/Buoy44013_corplot.pdf")


#Buoy44098
mod_44098 <- lm(HRI_Tidbit ~ Buoy44098 *MM, data = Tidbit_and_Buoy_data)
summary(mod_44098)
car::Anova(mod_44098)
#broom::glance(mod_44098)
performance::r2(mod_44098) #0.866

Buoy44098_corplot <- ggplot(Tidbit_and_Buoy_data, aes(x = Buoy44098, y = HRI_Tidbit, color = MM))+
  geom_point()+
  stat_smooth(method = "lm")

ggsave("Figures/Buoy44098_corplot.pdf")

#OISST
mod_OISST <- lm(HRI_Tidbit ~ OISST *MM, data = Tidbit_and_Buoy_data)
summary(mod_OISST)
car::Anova(mod_OISST)
#broom::glance(mod_OISST)
performance::r2(mod_OISST) #0.832

OISST_HRI_corplot <- ggplot(Tidbit_and_Buoy_data, aes(x = OISST, y = HRI_Tidbit, color = MM))+
  geom_point()+
  stat_smooth(method = "lm")

ggsave("Figures/OISST_HRI_corplot.pdf")

#HRO
mod_HRO_Tidbit <- lm(HRI_Tidbit ~ HRO_Tidbit *MM, data = Tidbit_and_Buoy_data)
summary(mod_HRO_Tidbit)
car::Anova(mod_HRO_Tidbit)
#broom::glance(mod_HRO_Tidbit)
performance::r2(mod_HRO_Tidbit) #0.888

HRO_HRI_corplot <- ggplot(Tidbit_and_Buoy_data, aes(x = HRO_Tidbit, y = HRI_Tidbit, color = MM))+
  geom_point()+
  stat_smooth(method = "lm")

ggsave("Figures/HRO_HRI_corplot.pdf")

#DB
mod_DB_Tidbit <- lm(HRI_Tidbit ~ DB_Tidbit *MM, data = Tidbit_and_Buoy_data)
summary(mod_DB_Tidbit)
car::Anova(mod_DB_Tidbit)
#broom::glance(mod_DB_Tidbit)
performance::r2(mod_DB_Tidbit) #0.898

DB_HRI_corplot <- ggplot(Tidbit_and_Buoy_data, aes(x = DB_Tidbit, y = HRI_Tidbit, color = MM))+
  geom_point()+
  stat_smooth(method = "lm")

ggsave("Figures/DB_HRI_corplot.pdf")

#SHI
mod_SHI_Tidbit <- lm(HRI_Tidbit ~ SHI_Tidbit *MM, data = Tidbit_and_Buoy_data)
summary(mod_SHI_Tidbit)
car::Anova(mod_SHI_Tidbit)
#broom::glance(mod_SHI_Tidbit)
performance::r2(mod_SHI_Tidbit) #0.916

SHI_HRI_corplot <- ggplot(Tidbit_and_Buoy_data, aes(x = DB_Tidbit, y = HRI_Tidbit, color = MM))+
  geom_point()+
  stat_smooth(method = "lm")

ggsave("Figures/SHI_HRI_corplot.pdf")

#SHO
mod_SHO_Tidbit <- lm(HRI_Tidbit ~ SHO_Tidbit *MM, data = Tidbit_and_Buoy_data)
summary(mod_SHO_Tidbit)
car::Anova(mod_SHO_Tidbit)
#broom::glance(mod_SHO_Tidbit)
performance::r2(mod_SHO_Tidbit) #0.927

SHI_HRI_corplot <- ggplot(Tidbit_and_Buoy_data, aes(x = DB_Tidbit, y = HRI_Tidbit, color = MM))+
  geom_point()+
  stat_smooth(method = "lm")

ggsave("Figures/SHI_HRI_corplot.pdf")

ggplot(Tidbit_and_Buoy_data|> filter(MM %in% as.character(7:10)), aes(x = OISST ,
                                 y = HRI_Tidbit, color = MM))+
  geom_point()+
  stat_smooth(method = "lm") +
  facet_wrap(vars(MM))

ggplot(Tidbit_and_Buoy_data|> filter(MM %in% as.character(7:10)), aes(x = Buoy44005 ,
                                                                      y = HRI_Tidbit, color = MM))+
  geom_point()+
  stat_smooth(method = "lm") +
  facet_wrap(vars(MM))

ggplot(Tidbit_and_Buoy_data|> filter(MM %in% as.character(7:10)), aes(x = Buoy44029_20m,
                                                                      y = HRI_Tidbit, color = MM))+
  geom_point()+
  stat_smooth(method = "lm") +
  facet_wrap(vars(MM))

ggplot(Tidbit_and_Buoy_data|> filter(MM %in% as.character(7:10)), aes(x = HRO_Tidbit,
                                                                      y = HRI_Tidbit, color = MM))+
  geom_point()+
  stat_smooth(method = "lm") +
  facet_wrap(vars(MM))

ggplot(Tidbit_and_Buoy_data|> filter(MM %in% as.character(7:10)), aes(x = DB_Tidbit,
                                                                      y = HRI_Tidbit, color = MM))+
  geom_point()+
  stat_smooth(method = "lm") +
  facet_wrap(vars(MM))

#save the models
saveRDS(mod_44029_2m, "Sebens_temps/Models/mod_44029_2m.RDS") 
saveRDS(mod_44029_20m, "Sebens_temps/Models/mod_44029_20m.RDS") 
saveRDS(mod_44005, "Sebens_temps/Models/mod_44005.RDS")
saveRDS(mod_44013, "Sebens_temps/Models/mod_44013.RDS")
saveRDS(mod_44098, "Sebens_temps/Models/mod_44098.RDS")
saveRDS(mod_OISST, "Sebens_temps/Models/mod_OISST.RDS")
saveRDS(mod_HRO_Tidbit,"Sebens_temps/Models/mod_HRO_Tidbit.RDS")
saveRDS(mod_DB_Tidbit, "Sebens_temps/Models/mod_DB_Tidbit.RDS")



#### Add Predictions and Plot ####

#Buoy 44029_2m
Tidbit_and_Buoy_data %>%
  modelr::add_predictions(mod_44029_2m) %>%
  tidyr::pivot_longer(cols = c(Buoy44029_2m, HRI_Tidbit, pred),
                      names_to = "type",
                      values_to = "HRI_Tidbit") %>%
  filter(type != "Buoy44029_2m") %>%
  ggplot(aes(x = Date, y = HRI_Tidbit, color = type)) +
  geom_point(alpha = 0.5) +
  facet_wrap(~MM, scale = "free_x")+
  ggtitle("Buoy 44029_2M - HRI model fit by Month")

#Buoy 44029_20m
Tidbit_and_Buoy_data %>%
  modelr::add_predictions(mod_44029_20m) %>%
  tidyr::pivot_longer(cols = c(Buoy44029_20m, HRI_Tidbit, pred),
                      names_to = "type",
                      values_to = "HRI_Tidbit") %>%
  filter(type != "Buoy44029_20m") %>%
  ggplot(aes(x = Date, y = HRI_Tidbit, color = type)) +
  geom_point(alpha = 0.5) +
  facet_wrap(~MM, scale = "free_x")+
  ggtitle("Buoy 44029_20M - HRI model fit by Month")

#Buoy 44005
Tidbit_and_Buoy_data %>%
  modelr::add_predictions(mod_44005) %>%
  tidyr::pivot_longer(cols = c(Buoy44005, HRI_Tidbit, pred),
                      names_to = "type",
                      values_to = "HRI_Tidbit") %>%
  filter(type != "Buoy44005") %>%
  ggplot(aes(x = Date, y = HRI_Tidbit, color = type)) +
  geom_point(alpha = 0.5) +
  facet_wrap(~MM, scale = "free_x")+
  ggtitle("Buoy 44005 - HRI model fit by Month")

#Buoy 44013
Tidbit_and_Buoy_data %>%
  modelr::add_predictions(mod_44013) %>%
  tidyr::pivot_longer(cols = c(Buoy44013, HRI_Tidbit, pred),
                      names_to = "type",
                      values_to = "HRI_Tidbit") %>%
  filter(type != "Buoy44013") %>%
  ggplot(aes(x = Date, y = HRI_Tidbit, color = type)) +
  geom_point(alpha = 0.5) +
  facet_wrap(~MM, scale = "free_x")+
  ggtitle("Buoy 44013 - HRI model fit by Month")

#Buoy 44098
Tidbit_and_Buoy_data %>%
  modelr::add_predictions(mod_44098) %>%
  tidyr::pivot_longer(cols = c(Buoy44098, HRI_Tidbit, pred),
                      names_to = "type",
                      values_to = "HRI_Tidbit") %>%
  filter(type != "Buoy44098") %>%
  ggplot(aes(x = Date, y = HRI_Tidbit, color = type)) +
  geom_point(alpha = 0.5) +
  facet_wrap(~MM, scale = "free_x")+
  ggtitle("Buoy 44098 - HRI model fit by Month")

#OISST
Tidbit_and_Buoy_data %>%
  modelr::add_predictions(mod_OISST) %>%
  tidyr::pivot_longer(cols = c(OISST, HRI_Tidbit, pred),
                      names_to = "type",
                      values_to = "HRI_Tidbit") %>%
  filter(type != "OISST") %>%
  ggplot(aes(x = Date, y = HRI_Tidbit, color = type)) +
  geom_point(alpha = 0.5) +
  facet_wrap(~MM, scale = "free_x")+
  ggtitle("OISST - HRI model fit by Month")

#######################################################################################################
#'--------------------------------------------------------------
#' Create extrapolated records based on model fits & Real data
#'--------------------------------------------------------------

#make buoy predicted Data Frames

#Buoy44029_2m
make_pred_ts <- function(dat, mod_44029_2m){ # 'dat' is created in/for/by the function in the next line
  pred_frame <- dat #now 'dat' exists
  pred_frame <-pred_frame %>%
    modelr::add_predictions(mod_44029_2m)
  pred_frame <- left_join(pred_frame, Tidbit_and_Buoy_data)
  pred_frame %>%
    mutate(HRI_bestguess = ifelse(is.na(HRI_Tidbit), pred, HRI_Tidbit))
}


#Make those predictions!
buoy44029_2m_pred <- make_pred_ts(Tidbit_and_Buoy_data, mod_44029_2m)

#Save those predictions!
saveRDS(buoy44029_2m_pred, "Sebens_temps/Outputs/buoy44029_2m_pred.RDS")
#readr::write_csv(buoy44029_2m_pred, "Sebens_temps/Outputs/buoy44029_2m_pred.csv")

#test
ggplot(buoy44029_2m_pred,
       aes(x = Date)) +
  geom_line(aes(y = pred), color = "red") +
  geom_point(aes(y = HRI_Tidbit), color = "blue", size = 0.1)+
  ggtitle("Buoy 44029 (2m depth) Interpolated prediction of HRI")

#Buoy44029_20m
make_pred_ts <- function(dat, mod_44029_20m){ # 'dat' is created in/for/by the function in the next line
  pred_frame <- dat #now 'dat' exists
  pred_frame <-pred_frame %>%
    modelr::add_predictions(mod_44029_20m)
  pred_frame <- left_join(pred_frame, Tidbit_and_Buoy_data)
  pred_frame %>%
    mutate(HRI_bestguess = ifelse(is.na(HRI_Tidbit), pred, HRI_Tidbit))
}

#Make those predictions!
buoy44029_20m_pred <- make_pred_ts(Tidbit_and_Buoy_data, mod_44029_20m)

#Save those predictions!
saveRDS(buoy44029_20m_pred, "Sebens_temps/Outputs/buoy44029_20m_pred.RDS")
#readr::write_csv(buoy44029_20m_pred, "Sebens_temps/Outputs/buoy44029_20m_pred.csv")

#test
ggplot(buoy44029_20m_pred,
       aes(x = Date)) +
  geom_line(aes(y = pred), color = "red") +
  geom_point(aes(y = HRI_Tidbit), color = "blue", size = 0.1)+
  ggtitle("Buoy 44029 (20m depth) Interpolated prediction of HRI")

#Buoy44005
make_pred_ts <- function(dat, mod_44005){ # 'dat' is created in/for/by the function in the next line
  pred_frame <- dat #now 'dat' exists
  pred_frame <-pred_frame %>%
    modelr::add_predictions(mod_44005)
  pred_frame <- left_join(pred_frame, Tidbit_and_Buoy_data)
  pred_frame %>%
    mutate(HRI_bestguess = ifelse(is.na(HRI_Tidbit), pred, HRI_Tidbit))
}

#Make those predictions!
buoy44005_pred <- make_pred_ts(Tidbit_and_Buoy_data, mod_44005)

#Save those predictions!
saveRDS(buoy44005_pred, "Sebens_temps/Outputs/buoy44005_pred.RDS")
#readr::write_csv(buoy44005_pred, "Sebens_temps/Outputs/buoy44005_pred.csv")

#test
ggplot(buoy44005_pred,
       aes(x = Date)) +
  geom_line(aes(y = pred), color = "red") +
  geom_point(aes(y = HRI_Tidbit), color = "blue", size = 0.1)+
  ggtitle("Buoy 44005 Interpolated prediction of HRI")

#Buoy44013
make_pred_ts <- function(dat, mod_44013){ # 'dat' is created in/for/by the function in the next line
  pred_frame <- dat #now 'dat' exists
  pred_frame <-pred_frame %>%
    modelr::add_predictions(mod_44013)
  pred_frame <- left_join(pred_frame, Tidbit_and_Buoy_data)
  pred_frame %>%
    mutate(HRI_bestguess = ifelse(is.na(HRI_Tidbit), pred, HRI_Tidbit))
}

#Make those predictions!
buoy44013_pred <- make_pred_ts(Tidbit_and_Buoy_data, mod_44013)

#Save those predictions!
saveRDS(buoy44013_pred, "Sebens_temps/Outputs/buoy44013_pred.RDS")
#readr::write_csv(buoy44013_pred, "Sebens_temps/Outputs/buoy44013_pred.csv")

#test
ggplot(buoy44013_pred,
       aes(x = Date)) +
  geom_line(aes(y = pred), color = "red") +
  geom_point(aes(y = HRI_Tidbit), color = "blue", size = 0.1)+
  ggtitle("Buoy 44005 Interpolated prediction of HRI")


#Buoy44098
make_pred_ts <- function(dat, mod_44098){ # 'dat' is created in/for/by the function in the next line
  pred_frame <- dat #now 'dat' exists
  pred_frame <-pred_frame %>%
    modelr::add_predictions(mod_44098)
  pred_frame <- left_join(pred_frame, Tidbit_and_Buoy_data)
  pred_frame %>%
    mutate(HRI_bestguess = ifelse(is.na(HRI_Tidbit), pred, HRI_Tidbit))
}

#Make those predictions!
buoy44098_pred <- make_pred_ts(Tidbit_and_Buoy_data, mod_44098)

#Save those predictions!
saveRDS(buoy44098_pred, "Sebens_temps/Outputs/buoy44098_pred.RDS")
#readr::write_csv(buoy44098_pred, "Sebens_temps/Outputs/buoy44098_pred.csv")

#test
ggplot(buoy44098_pred,
       aes(x = Date)) +
  geom_line(aes(y = pred), color = "red") +
  geom_point(aes(y = HRI_Tidbit), color = "blue", size = 0.1)+
  ggtitle("Buoy 44098 Interpolated prediction of HRI")


#For OISST
make_pred_ts <- function(dat, mod_OISST){ # 'dat' is created in/for/by the function in the next line
  pred_frame <- dat #now 'dat' exists
  pred_frame <-pred_frame %>%
    modelr::add_predictions(mod_OISST)
  pred_frame <- left_join(pred_frame, Tidbit_and_Buoy_data)
  pred_frame %>%
    mutate(HRI_bestguess = ifelse(is.na(HRI_Tidbit), pred, HRI_Tidbit))
}

#Make those predictions!
oisst_pred <- make_pred_ts(Tidbit_and_Buoy_data, mod_OISST)

#Save those predictions!
saveRDS(oisst_pred, "Sebens_temps/Outputs/oisst_pred.RDS")
#readr::write_csv(oisst_pred, "Sebens_temps/Outputs/oisst_pred.csv")

#test
ggplot(oisst_pred,
       aes(x = Date)) +
  geom_line(aes(y = pred), color = "red") +
  geom_point(aes(y = HRI_Tidbit), color = "blue", size = 0.1)+
  ggtitle("OISST Interpolated prediction of HRI")

#For HRO
make_pred_ts <- function(dat, mod_HRO_Tidbit){ # 'dat' is created in/for/by the function in the next line
  pred_frame <- dat #now 'dat' exists
  pred_frame <-pred_frame %>%
    modelr::add_predictions(mod_HRO_Tidbit)
  pred_frame <- left_join(pred_frame, Tidbit_and_Buoy_data)
  pred_frame %>%
    mutate(HRI_bestguess = ifelse(is.na(HRI_Tidbit), pred, HRI_Tidbit))
}

#Make those predictions!
HRO_HRI_pred <- make_pred_ts(Tidbit_and_Buoy_data, mod_HRO_Tidbit)

#Save those predictions!
saveRDS(HRO_HRI_pred, "Sebens_temps/Outputs/HRO_HRI_pred.RDS")
#readr::write_csv(HRO_HRI_pred, "Sebens_temps/Outputs/HRO_HRI_pred.csv")

#test
ggplot(HRO_HRI_pred,
       aes(x = Date)) +
  geom_line(aes(y = pred), color = "red") +
  geom_point(aes(y = HRI_Tidbit), color = "blue", size = 0.1)+
  ggtitle("HRO Interpolated prediction of HRI")

#For DB
make_pred_ts <- function(dat, mod_HRO_Tidbit){ # 'dat' is created in/for/by the function in the next line
  pred_frame <- dat #now 'dat' exists
  pred_frame <-pred_frame %>%
    modelr::add_predictions(mod_DB_Tidbit)
  pred_frame <- left_join(pred_frame, Tidbit_and_Buoy_data)
  pred_frame %>%
    mutate(HRI_bestguess = ifelse(is.na(HRI_Tidbit), pred, HRI_Tidbit))
}

#Make those predictions!
DB_HRI_pred <- make_pred_ts(Tidbit_and_Buoy_data, mod_DB_Tidbit)

#Save those predictions!
saveRDS(DB_HRI_pred, "Sebens_temps/Outputs/DB_HRI_pred.RDS")
#readr::write_csv(DB_HRI_pred, "Sebens_temps/Outputs/DB_HRI_pred.csv")

#test
ggplot(DB_HRI_pred,
       aes(x = Date)) +
  geom_line(aes(y = pred), color = "red") +
  geom_point(aes(y = HRI_Tidbit), color = "blue", size = 0.1)+
  ggtitle("DB Interpolated prediction of HRI")

#For SHI
make_pred_ts <- function(dat, mod_SHI_Tidbit){ # 'dat' is created in/for/by the function in the next line
  pred_frame <- dat #now 'dat' exists
  pred_frame <-pred_frame %>%
    modelr::add_predictions(mod_SHI_Tidbit)
  pred_frame <- left_join(pred_frame, Tidbit_and_Buoy_data)
  pred_frame %>%
    mutate(HRI_bestguess = ifelse(is.na(HRI_Tidbit), pred, HRI_Tidbit))
}

#Make those predictions!
SHI_HRI_pred <- make_pred_ts(Tidbit_and_Buoy_data, mod_SHI_Tidbit)

#Save those predictions!
saveRDS(SHI_HRI_pred, "Sebens_temps/Outputs/SHI_HRI_pred.RDS")
#readr::write_csv(SHI_HRI_pred, "Sebens_temps/Outputs/SHI_HRI_pred.csv")

#test
ggplot(DB_HRI_pred,
       aes(x = Date)) +
  geom_line(aes(y = pred), color = "red") +
  geom_point(aes(y = HRI_Tidbit), color = "blue", size = 0.1)+
  ggtitle("SHI Interpolated prediction of HRI")

#For SHO
make_pred_ts <- function(dat, mod_SHO_Tidbit){ # 'dat' is created in/for/by the function in the next line
  pred_frame <- dat #now 'dat' exists
  pred_frame <-pred_frame %>%
    modelr::add_predictions(mod_SHO_Tidbit)
  pred_frame <- left_join(pred_frame, Tidbit_and_Buoy_data)
  pred_frame %>%
    mutate(HRI_bestguess = ifelse(is.na(HRI_Tidbit), pred, HRI_Tidbit))
}

#Make those predictions!
SHO_HRI_pred <- make_pred_ts(Tidbit_and_Buoy_data, mod_SHO_Tidbit)

#Save those predictions!
saveRDS(SHO_HRI_pred, "Sebens_temps/Outputs/SHO_HRI_pred.RDS")
#readr::write_csv(SHO_HRI_pred, "Sebens_temps/Outputs/SHO_HRI_pred.csv")

#test
ggplot(DB_HRI_pred,
       aes(x = Date)) +
  geom_line(aes(y = pred), color = "red") +
  geom_point(aes(y = HRI_Tidbit), color = "blue", size = 0.1)+
  ggtitle("SHI Interpolated prediction of HRI")

#######################################################################################################
#'--------------------------------------------------------------
#' Create Hierarchical Supplementary Tidbit Data
#'--------------------------------------------------------------

# Select Needed Columns from Predictions

#HRI
View(buoy44029_20m_pred)
Interp_44029_20m <- buoy44029_20m_pred %>%
  subset(., select = c(1:4,11,16)) %>% #These numbers will change as you add in additional sites
  rename(pred_via_44029_20m = 'pred')

Interp_44029_2m <- buoy44029_2m_pred %>%
  subset(., select = c(1:4,11,16)) %>% 
  rename(pred_via_44029_2m = 'pred')

Interp_DB_HRI <- as.data.frame(DB_HRI_pred) %>%
  subset(., select = c(1:4,11,16)) %>%
  rename(pred_via_DB = 'pred')

Interp_HRO_HRI <- as.data.frame(HRO_HRI_pred) %>%
  subset(., select = c(1:4,11,16)) %>%
  rename(pred_via_HRO = 'pred')

Interp_44098 <- buoy44098_pred %>%
  subset(., select = c(1:4,11,16)) %>%
  rename(pred_via_44098 = 'pred')

Interp_44013 <- buoy44013_pred %>%
  subset(., select = c(1:4,11,16)) %>%
  rename(pred_via_44013 = 'pred')

Interp_44005 <- buoy44005_pred %>%
  subset(., select = c(1:4,11,16)) %>%
  rename(pred_via_44005 = 'pred')

Interp_OISST_HRI <- oisst_pred %>%
  subset(., select = c(1:4,11,16)) %>%
  rename(pred_via_OISST = 'pred')

Interp_SHI_HRI <- SHI_HRI_pred %>%
  subset(., select = c(1:4,11,16)) %>%
  rename(pred_via_SHI = 'pred')

Interp_SHO_HRI <- SHO_HRI_pred %>%
  subset(., select = c(1:4,11,16)) %>%
  rename(pred_via_SHO = 'pred')

HRI_Interpolation_raw <-(list(Interp_44029_20m,Interp_DB_HRI,Interp_HRO_HRI,Interp_44098,
                              Interp_44013,Interp_44005,Interp_OISST_HRI, Interp_SHI_HRI,
                              Interp_SHO_HRI,Interp_44029_2m) %>% 
                           reduce(left_join))

HRI_Interpolation <- HRI_Interpolation_raw %>%
mutate(final_value = 
         case_when(
           !is.na(HRI_Tidbit) ~ HRI_Tidbit,
           !is.na(pred_via_44029_20m) ~ pred_via_44029_20m,
           !is.na(pred_via_SHO) ~ pred_via_SHO,
           !is.na(pred_via_SHI) ~ pred_via_SHI,
           !is.na(pred_via_DB) ~ pred_via_DB,
           !is.na(pred_via_HRO) ~ pred_via_HRO,
           !is.na(pred_via_44013) ~ pred_via_44013,
           !is.na(pred_via_44005) ~ pred_via_44005,
           !is.na(pred_via_44098) ~ pred_via_44098,
           !is.na(pred_via_OISST) ~ pred_via_OISST,
           !is.na(pred_via_44029_2m) ~ pred_via_44029_2m,
           TRUE ~ NA_real_
         ))

View(HRI_Interpolation)
#write_csv(HRI_Interpolation,"Outputs/HRI_Interpolation.csv")

#############################################################
#Plot
HRI_Interpolation_plot <- ggplot(HRI_Interpolation,
       aes(x = Date)) +
  geom_line(aes(y = final_value), color = "red") +
  geom_point(aes(y = HRI_Tidbit), color = "blue", size = 0.1)+
  ggtitle("Interpolated prediction of HRI Temperatures at 20m")+
  labs(y = expression(paste("Temperature", "\u00b0C")), x = expression("Date"))

ggsave("Figures/HRI_Interpolation_plot.jpg")

##################################################################################################################
# NOW TRY TO RECREATE FOR Annual summer means RATHER THAN DAILY MEANS!
###################################################################################################################### Notes and other code bits ####


HRI_Interpolation2 <- HRI_Interpolation |>
  filter(MM %in% c(6:9))|>
  group_by(YY)|>
  summarize(mean = mean(final_value))

HRI_Summer_plot <- ggplot(HRI_Interpolation2,
       aes(x = YY)) +
  geom_line(aes(y = mean))+
  geom_point(aes(y = mean))+
  theme_set(theme_classic(base_size = 18)) +
  ggtitle("Interpolated prediction of HRI Temperatures at 20m") +
  labs(y = expression(paste("Mean Annual Summer Temperature", "\u00b0C")), x = expression("Date"))

ggsave("Figures/HRI_Summer_plot.jpg")

#write_csv(HRI_Interpolation2,"Outputs/HRI_Interpolation_Summer.csv")



