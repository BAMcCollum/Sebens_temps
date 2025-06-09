# Interpolation models for Shag Rocks Outer

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
#### SHO Models ####

#fit a MLR model to each temperature record (Buoys 44029, 44013, 44005, 44098, OISST, HRO, & HRI)

SHOmod_44029_2m <- lm(SHO_Tidbit ~ Buoy44029_2m *MM, data = Tidbit_and_Buoy_data) 
car::Anova(SHOmod_44029_2m)
performance::r2(SHOmod_44029_2m) #0.858

SHOmod_44029_20m <- lm(SHO_Tidbit ~ Buoy44029_20m *MM, data = Tidbit_and_Buoy_data)
car::Anova(SHOmod_44029_20m)
performance::r2(SHOmod_44029_20m) #0.914

SHOmod_44005 <- lm(SHO_Tidbit ~ Buoy44005 *MM, data = Tidbit_and_Buoy_data)
car::Anova(SHOmod_44005)
performance::r2(SHOmod_44005) #0.851

SHOmod_44013 <- lm(SHO_Tidbit ~ Buoy44013 *MM, data = Tidbit_and_Buoy_data)
car::Anova(SHOmod_44013)
performance::r2(SHOmod_44013) #0.858

SHOmod_44098 <- lm(SHO_Tidbit ~ Buoy44098 *MM, data = Tidbit_and_Buoy_data)
car::Anova(SHOmod_44098)
performance::r2(SHOmod_44098) #0.871

SHOmod_OISST <- lm(SHO_Tidbit ~ OISST *MM, data = Tidbit_and_Buoy_data)
car::Anova(SHOmod_OISST)
performance::r2(SHOmod_OISST) #0.858

SHOmod_HRI <- lm(SHO_Tidbit ~ HRI_Tidbit *MM, data = Tidbit_and_Buoy_data)
car::Anova(SHOmod_HRI)
performance::r2(SHOmod_HRI) #0.928

SHOmod_HRO <- lm(SHO_Tidbit ~ HRO_Tidbit *MM, data = Tidbit_and_Buoy_data)
car::Anova(SHOmod_HRO)
performance::r2(SHOmod_HRO) #0.949

SHOmod_DB <-lm(SHO_Tidbit ~ DB_Tidbit *MM, data = Tidbit_and_Buoy_data)
car::Anova(SHOmod_DB)
performance::r2(SHOmod_DB) #0.984

SHOmod_SHI <-lm(SHO_Tidbit ~ SHI_Tidbit *MM, data = Tidbit_and_Buoy_data)
car::Anova(SHOmod_SHI)
performance::r2(SHOmod_SHI) #0.989
#######################################################################################################
#'--------------------------------------------------------------
#' Create extrapolated records based on model fits & Real data
#'--------------------------------------------------------------

#make buoy predicted Data Frames

#SHO-Buoy44029_2m
make_pred_ts <- function(dat, SHOmod_44029_2m){ # 'dat' is created in/for/by the function in the next line
  pred_frame <- dat #now 'dat' exists
  pred_frame <-pred_frame %>%
    modelr::add_predictions(SHOmod_44029_2m)
  pred_frame <- left_join(pred_frame, Tidbit_and_Buoy_data)
  pred_frame %>%
    mutate(SHO_bestguess = ifelse(is.na(SHO_Tidbit), pred, SHO_Tidbit))
}

#Make those predictions!
buoy44029_2m_predSHO <- make_pred_ts(Tidbit_and_Buoy_data, SHOmod_44029_2m)

#Save those predictions!
saveRDS(buoy44029_2m_predSHO, "Sebens_temps/Outputs/SHObuoy44029_2m_pred.RDS")

#test
ggplot(buoy44029_2m_predSHO,
       aes(x = Date)) +
  geom_line(aes(y = pred), color = "red") +
  geom_point(aes(y = SHO_Tidbit), color = "blue", size = 0.1)+
  ggtitle("Buoy 44029 (2m depth) Interpolated prediction of SHO")

#SHO-Buoy44029_20m
make_pred_ts <- function(dat, SHOmod_44029_20m){ # 'dat' is created in/for/by the function in the next line
  pred_frame <- dat #now 'dat' exists
  pred_frame <-pred_frame %>%
    modelr::add_predictions(SHOmod_44029_20m)
  pred_frame <- left_join(pred_frame, Tidbit_and_Buoy_data)
  pred_frame %>%
    mutate(SHO_bestguess = ifelse(is.na(SHO_Tidbit), pred, SHO_Tidbit))
}

#Make those predictions!
buoy44029_20m_predSHO <- make_pred_ts(Tidbit_and_Buoy_data, SHOmod_44029_20m)

#Save those predictions!
saveRDS(buoy44029_20m_predSHO, "Sebens_temps/Outputs/SHObuoy44029_20m_pred.RDS")

#test
ggplot(buoy44029_20m_predSHO,
       aes(x = Date)) +
  geom_line(aes(y = pred), color = "red") +
  geom_point(aes(y = SHO_Tidbit), color = "blue", size = 0.1)+
  ggtitle("Buoy 44029 (20m depth) Interpolated prediction of SHO")

#Buoy44005
make_pred_ts <- function(dat, SHOmod_44005){ # 'dat' is created in/for/by the function in the next line
  pred_frame <- dat #now 'dat' exists
  pred_frame <-pred_frame %>%
    modelr::add_predictions(SHOmod_44005)
  pred_frame <- left_join(pred_frame, Tidbit_and_Buoy_data)
  pred_frame %>%
    mutate(SHO_bestguess = ifelse(is.na(SHO_Tidbit), pred, SHO_Tidbit))
}

#Make those predictions!
buoy44005_predSHO <- make_pred_ts(Tidbit_and_Buoy_data, SHOmod_44005)

#Save those predictions!
saveRDS(buoy44005_predSHO, "Sebens_temps/Outputs/buoy44005_predSHO.RDS")

#test
ggplot(buoy44005_predSHO,
       aes(x = Date)) +
  geom_line(aes(y = pred), color = "red") +
  geom_point(aes(y = SHO_Tidbit), color = "blue", size = 0.1)+
  ggtitle("Buoy 44005 Interpolated prediction of SHO")

#Buoy44013
make_pred_ts <- function(dat, SHOmod_44013){ # 'dat' is created in/for/by the function in the next line
  pred_frame <- dat #now 'dat' exists
  pred_frame <-pred_frame %>%
    modelr::add_predictions(SHOmod_44013)
  pred_frame <- left_join(pred_frame, Tidbit_and_Buoy_data)
  pred_frame %>%
    mutate(SHO_bestguess = ifelse(is.na(SHO_Tidbit), pred, SHO_Tidbit))
}

#Make those predictions!
buoy44013_predSHO <- make_pred_ts(Tidbit_and_Buoy_data, SHOmod_44013)

#Save those predictions!
saveRDS(buoy44013_predSHO, "Sebens_temps/Outputs/buoy44013_predSHO.RDS")
#readr::write_csv(buoy44013_pred, "Sebens_temps/Outputs/buoy44013_pred.csv")

#test
ggplot(buoy44013_predSHO,
       aes(x = Date)) +
  geom_line(aes(y = pred), color = "red") +
  geom_point(aes(y = SHO_Tidbit), color = "blue", size = 0.1)+
  ggtitle("Buoy 44013 Interpolated prediction of SHO")


#Buoy44098
make_pred_ts <- function(dat, SHOmod_44098){ # 'dat' is created in/for/by the function in the next line
  pred_frame <- dat #now 'dat' exists
  pred_frame <-pred_frame %>%
    modelr::add_predictions(SHOmod_44098)
  pred_frame <- left_join(pred_frame, Tidbit_and_Buoy_data)
  pred_frame %>%
    mutate(SHO_bestguess = ifelse(is.na(SHO_Tidbit), pred, SHO_Tidbit))
}

#Make those predictions!
buoy44098_predSHO <- make_pred_ts(Tidbit_and_Buoy_data, SHOmod_44098)

#Save those predictions!
saveRDS(buoy44098_predSHO, "Sebens_temps/Outputs/buoy44098_predSHO.RDS")

#test
ggplot(buoy44098_predSHO,
       aes(x = Date)) +
  geom_line(aes(y = pred), color = "red") +
  geom_point(aes(y = SHO_Tidbit), color = "blue", size = 0.1)+
  ggtitle("Buoy 44098 Interpolated prediction of SHO")


#For SHO-OISST
make_pred_ts <- function(dat, SHOmod_OISST){ # 'dat' is created in/for/by the function in the next line
  pred_frame <- dat #now 'dat' exists
  pred_frame <-pred_frame %>%
    modelr::add_predictions(SHOmod_OISST)
  pred_frame <- left_join(pred_frame, Tidbit_and_Buoy_data)
  pred_frame %>%
    mutate(SHO_bestguess = ifelse(is.na(SHO_Tidbit), pred, SHO_Tidbit))
}

#Make those predictions!
oisst_predSHO <- make_pred_ts(Tidbit_and_Buoy_data, SHOmod_OISST)

#Save those predictions!
saveRDS(oisst_predSHO, "Sebens_temps/Outputs/SHOoisst_pred.RDS")
#readr::write_csv(oisst_pred, "Sebens_temps/Outputs/oisst_pred.csv")

#test
ggplot(oisst_predSHO,
       aes(x = Date)) +
  geom_line(aes(y = pred), color = "red") +
  geom_point(aes(y = SHO_Tidbit), color = "blue", size = 0.1)+
  ggtitle("OISST Interpolated prediction of SHO")

#For HRI-SHO
make_pred_ts <- function(dat, SHOmod_HRI){ # 'dat' is created in/for/by the function in the next line
  pred_frame <- dat #now 'dat' exists
  pred_frame <-pred_frame %>%
    modelr::add_predictions(SHOmod_HRI)
  pred_frame <- left_join(pred_frame, Tidbit_and_Buoy_data)
  pred_frame %>%
    mutate(SHO_bestguess = ifelse(is.na(SHO_Tidbit), pred, SHO_Tidbit))
}

#Make those predictions!
SHOmod_HRI_pred <- make_pred_ts(Tidbit_and_Buoy_data, SHOmod_HRI)

#Save those predictions!
saveRDS(SHOmod_HRI_pred, "Sebens_temps/Outputs/SHOmod_HRI_pred.RDS")

#test
ggplot(SHOmod_HRI_pred,
       aes(x = Date)) +
  geom_line(aes(y = pred), color = "red") +
  geom_point(aes(y = SHO_Tidbit), color = "blue", size = 0.1)+
  ggtitle("HRI Interpolated prediction of SHO")

#For HRO-SHO
make_pred_ts <- function(dat, SHOmod_HRO){ # 'dat' is created in/for/by the function in the next line
  pred_frame <- dat #now 'dat' exists
  pred_frame <-pred_frame %>%
    modelr::add_predictions(SHOmod_HRO)
  pred_frame <- left_join(pred_frame, Tidbit_and_Buoy_data)
  pred_frame %>%
    mutate(SHO_bestguess = ifelse(is.na(SHO_Tidbit), pred, SHO_Tidbit))
}

#Make those predictions!
SHOmod_HRO_pred <- make_pred_ts(Tidbit_and_Buoy_data, SHOmod_HRO)

#Save those predictions!
saveRDS(SHOmod_HRO_pred, "Sebens_temps/Outputs/SHOmod_HRO_pred.RDS")

#test
ggplot(SHOmod_HRO_pred,
       aes(x = Date)) +
  geom_line(aes(y = pred), color = "red") +
  geom_point(aes(y = SHO_Tidbit), color = "blue", size = 0.1)+
  ggtitle("HRO Interpolated prediction of SHO")

#For DB-SHO
make_pred_ts <- function(dat, SHOmod_DB){ # 'dat' is created in/for/by the function in the next line
  pred_frame <- dat #now 'dat' exists
  pred_frame <-pred_frame %>%
    modelr::add_predictions(SHOmod_DB)
  pred_frame <- left_join(pred_frame, Tidbit_and_Buoy_data)
  pred_frame %>%
    mutate(SHO_bestguess = ifelse(is.na(SHO_Tidbit), pred, SHO_Tidbit))
}

#Make those predictions!
SHOmod_DB_pred <- make_pred_ts(Tidbit_and_Buoy_data, SHOmod_DB)

#Save those predictions!
saveRDS(SHOmod_DB_pred, "Sebens_temps/Outputs/SHOmod_DB_pred.RDS")

#test
ggplot(SHOmod_DB_pred,
       aes(x = Date)) +
  geom_line(aes(y = pred), color = "red") +
  geom_point(aes(y = SHO_Tidbit), color = "blue", size = 0.1)+
  ggtitle("DB Interpolated prediction of SHO")

#For SHI-SHO
make_pred_ts <- function(dat, SHOmod_SHI){ # 'dat' is created in/for/by the function in the next line
  pred_frame <- dat #now 'dat' exists
  pred_frame <-pred_frame %>%
    modelr::add_predictions(SHOmod_SHI)
  pred_frame <- left_join(pred_frame, Tidbit_and_Buoy_data)
  pred_frame %>%
    mutate(SHO_bestguess = ifelse(is.na(SHO_Tidbit), pred, SHO_Tidbit))
}

#Make those predictions!
SHOmod_SHI_pred <- make_pred_ts(Tidbit_and_Buoy_data, SHOmod_SHI)

#Save those predictions!
saveRDS(SHOmod_SHI_pred, "Sebens_temps/Outputs/SHOmod_SHI_pred.RDS")

#test
ggplot(SHOmod_SHI_pred,
       aes(x = Date)) +
  geom_line(aes(y = pred), color = "red") +
  geom_point(aes(y = SHO_Tidbit), color = "blue", size = 0.1)+
  ggtitle("SHI Interpolated prediction of SHO")

#######################################################################################################
#######################################################################################################
#'--------------------------------------------------------------
#' Create Hierarchical Supplementary Tidbit Data
#'--------------------------------------------------------------

# Select Needed Columns from Predictions
#SHO

View(SHOmod_HRI_pred)
Interp_HRI_SHO <- as.data.frame(SHOmod_HRI_pred) %>%
  subset(., select = c(1:4,15,16)) %>%
  rename(pred_via_HRI = 'pred')

Interp_HRO_SHO <- as.data.frame(SHOmod_HRO_pred) %>%
  subset(., select = c(1:4,15,16)) %>%
  rename(pred_via_HRO = 'pred')

Interp_DB_SHO <- as.data.frame(SHOmod_DB_pred) %>%
  subset(., select = c(1:4,15,16)) %>%
  rename(pred_via_DB = 'pred')

Interp_SHI_SHO <- as.data.frame(SHOmod_SHI_pred) %>%
  subset(., select = c(1:4,15,16)) %>%
  rename(pred_via_SHI = 'pred')

Interp_SHO_44029_20m <- buoy44029_20m_predSHO %>%
  subset(., select = c(1:4,15,16)) %>%
  rename(pred_via_44029_20m = 'pred')

Interp_SHO_44029_2m <- buoy44029_2m_predSHO %>%
  subset(., select = c(1:4,15,16)) %>%
  rename(pred_via_44029_2m = 'pred')

Interp_SHO_44013 <- buoy44013_predSHO %>%
  subset(., select = c(1:4,15,16)) %>%
  rename(pred_via_44013 = 'pred')

Interp_SHO_44098 <- buoy44098_predSHO %>%
  subset(., select = c(1:4,15,16)) %>%
  rename(pred_via_44098 = 'pred')

Interp_SHO_44005 <- buoy44005_predSHO %>%
  subset(., select = c(1:4,15,16)) %>%
  rename(pred_via_44005 = 'pred')

Interp_SHO_OISST <- oisst_predSHO %>%
  subset(., select = c(1:4,15,16)) %>%
  rename(pred_via_OISST = 'pred')

SHO_Interpolation_raw <- (list(Interp_HRI_SHO,Interp_HRO_SHO,Interp_DB_SHO,Interp_SHO_44029_20m,
                               Interp_SHO_44013,Interp_SHO_44098,Interp_SHO_44005,Interp_SHO_OISST,
                               Interp_SHI_SHO, Interp_SHO_44029_2m) %>% 
                            reduce(left_join))

SHO_Interpolation <- SHO_Interpolation_raw %>%
  mutate(final_value = 
           case_when(
             !is.na(SHO_Tidbit) ~ SHO_Tidbit,
             !is.na(pred_via_SHI) ~ pred_via_SHI,
             !is.na(pred_via_DB) ~ pred_via_DB,
             !is.na(pred_via_HRO) ~ pred_via_HRO,
             !is.na(pred_via_HRI) ~ pred_via_HRI,
             !is.na(pred_via_44029_20m) ~ pred_via_44029_20m,
             !is.na(pred_via_44098) ~ pred_via_44098,
             !is.na(pred_via_44029_2m) ~ pred_via_44029_2m,
             !is.na(pred_via_44013) ~ pred_via_44013,
             !is.na(pred_via_OISST) ~ pred_via_OISST,
             !is.na(pred_via_44005) ~ pred_via_44005,
             TRUE ~ NA_real_))

View(SHO_Interpolation)
write_csv(SHO_Interpolation,"Outputs/SHO_Interpolation.csv")

#############################################################
#Plot
SHO_Interpolation_plot <- ggplot(SHO_Interpolation,
       aes(x = Date)) +
  geom_line(aes(y = final_value), color = "red") +
  geom_point(aes(y = SHO_Tidbit), color = "blue", size = 0.1)+
  ggtitle("Interpolated prediction of SHO Temperatures at 8m")+
  labs(y = expression(paste("Temperature", "\u00b0C")), x = expression("Date"))

ggsave("Figures/SHO_Interpolation_plot.jpg")


##################################################################################################################
# NOW TRY TO RECREATE FOR Annual summer means RATHER THAN DAILY MEANS!
###################################################################################################################### Notes and other code bits ####

SHO_Interpolation2 <- SHO_Interpolation |>
  filter(MM %in% c(6:9))|>
  group_by(YY)|>
  summarize(mean = mean(final_value))

View(SHO_Interpolation2)

SHO_Summer_plot <- ggplot(SHO_Interpolation2,
       aes(x = YY)) +
  geom_line(aes(y = mean))+
  geom_point(aes(y = mean))+
  theme_set(theme_classic(base_size = 18)) +
  ggtitle("Interpolated prediction of SHO Temperatures at 8m") +
  labs(y = expression(paste("Mean Annual Summer Temperature", "\u00b0C")), x = expression("Date"))

ggsave("Figures/SHO_Summer_plot.jpg")

#write_csv(SHO_Interpolation2,"Outputs/SHO_Interpolation_Summer.csv")
