# Interpolation models for Shag Rocks Inner

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
#### SHI Models ####

#fit a MLR model to each temperature record (Buoys 44029, 44013, 44005, 44098, OISST, HRO, & HRI)

SHImod_44029_2m <- lm(SHI_Tidbit ~ Buoy44029_2m *MM, data = Tidbit_and_Buoy_data) 
car::Anova(SHImod_44029_2m)
performance::r2(SHImod_44029_2m) #0.877

SHImod_44029_20m <- lm(SHI_Tidbit ~ Buoy44029_20m *MM, data = Tidbit_and_Buoy_data)
car::Anova(SHImod_44029_20m)
performance::r2(SHImod_44029_20m) #0.919

SHImod_44005 <- lm(SHI_Tidbit ~ Buoy44005 *MM, data = Tidbit_and_Buoy_data)
car::Anova(SHImod_44005)
performance::r2(SHImod_44005) #0.860

SHImod_44013 <- lm(SHI_Tidbit ~ Buoy44013 *MM, data = Tidbit_and_Buoy_data)
car::Anova(SHImod_44013)
performance::r2(SHImod_44013) #0.877

SHImod_44098 <- lm(SHI_Tidbit ~ Buoy44098 *MM, data = Tidbit_and_Buoy_data)
car::Anova(SHImod_44098)
performance::r2(SHImod_44098) #0.895

SHImod_OISST <- lm(SHI_Tidbit ~ OISST *MM, data = Tidbit_and_Buoy_data)
car::Anova(SHImod_OISST)
performance::r2(SHImod_OISST) #0.873

SHImod_HRI <- lm(SHI_Tidbit ~ HRI_Tidbit *MM, data = Tidbit_and_Buoy_data)
car::Anova(SHImod_HRI)
performance::r2(SHImod_HRI) #0.922

SHImod_HRO <- lm(SHI_Tidbit ~ HRO_Tidbit *MM, data = Tidbit_and_Buoy_data)
car::Anova(SHImod_HRO)
performance::r2(SHImod_HRO) #0.947

SHImod_DB <-lm(SHI_Tidbit ~ DB_Tidbit *MM, data = Tidbit_and_Buoy_data)
car::Anova(SHImod_DB)
performance::r2(SHImod_DB) #0.992

SHImod_SHO <-lm(SHI_Tidbit ~ SHO_Tidbit *MM, data = Tidbit_and_Buoy_data)
car::Anova(SHImod_SHO)
performance::r2(SHImod_SHO) #0.989

#######################################################################################################
#'--------------------------------------------------------------
#' Create extrapolated records based on model fits & Real data
#'--------------------------------------------------------------

#make buoy predicted Data Frames

#SHI-Buoy44029_2m
make_pred_ts <- function(dat, SHImod_44029_2m){ # 'dat' is created in/for/by the function in the next line
  pred_frame <- dat #now 'dat' exists
  pred_frame <-pred_frame %>%
    modelr::add_predictions(SHImod_44029_2m)
  pred_frame <- left_join(pred_frame, Tidbit_and_Buoy_data)
  pred_frame %>%
    mutate(SHI_bestguess = ifelse(is.na(SHI_Tidbit), pred, SHI_Tidbit))
}

#Make those predictions!
buoy44029_2m_predSHI <- make_pred_ts(Tidbit_and_Buoy_data, SHImod_44029_2m)

#Save those predictions!
saveRDS(buoy44029_2m_predSHI, "Sebens_temps/Outputs/SHIbuoy44029_2m_pred.RDS")

#test
ggplot(buoy44029_2m_predSHI,
       aes(x = Date)) +
  geom_line(aes(y = pred), color = "red") +
  geom_point(aes(y = SHI_Tidbit), color = "blue", size = 0.1)+
  ggtitle("Buoy 44029 (2m depth) Interpolated prediction of SHI")

#SHI-Buoy44029_20m
make_pred_ts <- function(dat, SHImod_44029_20m){ # 'dat' is created in/for/by the function in the next line
  pred_frame <- dat #now 'dat' exists
  pred_frame <-pred_frame %>%
    modelr::add_predictions(SHImod_44029_20m)
  pred_frame <- left_join(pred_frame, Tidbit_and_Buoy_data)
  pred_frame %>%
    mutate(SHI_bestguess = ifelse(is.na(SHI_Tidbit), pred, SHI_Tidbit))
}

#Make those predictions!
buoy44029_20m_predSHI <- make_pred_ts(Tidbit_and_Buoy_data, SHImod_44029_20m)

#Save those predictions!
saveRDS(buoy44029_20m_predSHI, "Sebens_temps/Outputs/SHIbuoy44029_20m_pred.RDS")

#test
ggplot(buoy44029_20m_predSHI,
       aes(x = Date)) +
  geom_line(aes(y = pred), color = "red") +
  geom_point(aes(y = SHI_Tidbit), color = "blue", size = 0.1)+
  ggtitle("Buoy 44029 (20m depth) Interpolated prediction of SHI")

#Buoy44005
make_pred_ts <- function(dat, SHImod_44005){ # 'dat' is created in/for/by the function in the next line
  pred_frame <- dat #now 'dat' exists
  pred_frame <-pred_frame %>%
    modelr::add_predictions(SHImod_44005)
  pred_frame <- left_join(pred_frame, Tidbit_and_Buoy_data)
  pred_frame %>%
    mutate(SHI_bestguess = ifelse(is.na(SHI_Tidbit), pred, SHI_Tidbit))
}

#Make those predictions!
buoy44005_predSHI <- make_pred_ts(Tidbit_and_Buoy_data, SHImod_44005)

#Save those predictions!
saveRDS(buoy44005_predSHI, "Sebens_temps/Outputs/buoy44005_predSHI.RDS")

#test
ggplot(buoy44005_predSHI,
       aes(x = Date)) +
  geom_line(aes(y = pred), color = "red") +
  geom_point(aes(y = SHI_Tidbit), color = "blue", size = 0.1)+
  ggtitle("Buoy 44005 Interpolated prediction of SHI")

#Buoy44013
make_pred_ts <- function(dat, SHImod_44013){ # 'dat' is created in/for/by the function in the next line
  pred_frame <- dat #now 'dat' exists
  pred_frame <-pred_frame %>%
    modelr::add_predictions(SHImod_44013)
  pred_frame <- left_join(pred_frame, Tidbit_and_Buoy_data)
  pred_frame %>%
    mutate(SHI_bestguess = ifelse(is.na(SHI_Tidbit), pred, SHI_Tidbit))
}

#Make those predictions!
buoy44013_predSHI <- make_pred_ts(Tidbit_and_Buoy_data, SHImod_44013)

#Save those predictions!
saveRDS(buoy44013_predSHI, "Sebens_temps/Outputs/buoy44013_predSHI.RDS")
#readr::write_csv(buoy44013_pred, "Sebens_temps/Outputs/buoy44013_pred.csv")

#test
ggplot(buoy44013_predSHI,
       aes(x = Date)) +
  geom_line(aes(y = pred), color = "red") +
  geom_point(aes(y = SHI_Tidbit), color = "blue", size = 0.1)+
  ggtitle("Buoy 44013 Interpolated prediction of SHI")


#Buoy44098
make_pred_ts <- function(dat, SHImod_44098){ # 'dat' is created in/for/by the function in the next line
  pred_frame <- dat #now 'dat' exists
  pred_frame <-pred_frame %>%
    modelr::add_predictions(SHImod_44098)
  pred_frame <- left_join(pred_frame, Tidbit_and_Buoy_data)
  pred_frame %>%
    mutate(SHI_bestguess = ifelse(is.na(SHI_Tidbit), pred, SHI_Tidbit))
}

#Make those predictions!
buoy44098_predSHI <- make_pred_ts(Tidbit_and_Buoy_data, SHImod_44098)

#Save those predictions!
saveRDS(buoy44098_predSHI, "Sebens_temps/Outputs/buoy44098_predSHI.RDS")

#test
ggplot(buoy44098_predSHI,
       aes(x = Date)) +
  geom_line(aes(y = pred), color = "red") +
  geom_point(aes(y = SHI_Tidbit), color = "blue", size = 0.1)+
  ggtitle("Buoy 44098 Interpolated prediction of SHI")


#For SHI-OISST
make_pred_ts <- function(dat, SHImod_OISST){ # 'dat' is created in/for/by the function in the next line
  pred_frame <- dat #now 'dat' exists
  pred_frame <-pred_frame %>%
    modelr::add_predictions(SHImod_OISST)
  pred_frame <- left_join(pred_frame, Tidbit_and_Buoy_data)
  pred_frame %>%
    mutate(SHI_bestguess = ifelse(is.na(SHI_Tidbit), pred, SHI_Tidbit))
}

#Make those predictions!
oisst_predSHI <- make_pred_ts(Tidbit_and_Buoy_data, SHImod_OISST)

#Save those predictions!
saveRDS(oisst_predSHI, "Sebens_temps/Outputs/SHIoisst_pred.RDS")
#readr::write_csv(oisst_pred, "Sebens_temps/Outputs/oisst_pred.csv")

#test
ggplot(oisst_predSHI,
       aes(x = Date)) +
  geom_line(aes(y = pred), color = "red") +
  geom_point(aes(y = SHI_Tidbit), color = "blue", size = 0.1)+
  ggtitle("OISST Interpolated prediction of SHI")

#For HRI-SHI
make_pred_ts <- function(dat, SHImod_HRI){ # 'dat' is created in/for/by the function in the next line
  pred_frame <- dat #now 'dat' exists
  pred_frame <-pred_frame %>%
    modelr::add_predictions(SHImod_HRI)
  pred_frame <- left_join(pred_frame, Tidbit_and_Buoy_data)
  pred_frame %>%
    mutate(SHI_bestguess = ifelse(is.na(SHI_Tidbit), pred, SHI_Tidbit))
}

#Make those predictions!
SHImod_HRI_pred <- make_pred_ts(Tidbit_and_Buoy_data, SHImod_HRI)

#Save those predictions!
saveRDS(SHImod_HRI_pred, "Sebens_temps/Outputs/SHImod_HRI_pred.RDS")

#test
ggplot(SHImod_HRI_pred,
       aes(x = Date)) +
  geom_line(aes(y = pred), color = "red") +
  geom_point(aes(y = SHI_Tidbit), color = "blue", size = 0.1)+
  ggtitle("HRI Interpolated prediction of SHI")

#For HRO-SHI
make_pred_ts <- function(dat, SHImod_HRO){ # 'dat' is created in/for/by the function in the next line
  pred_frame <- dat #now 'dat' exists
  pred_frame <-pred_frame %>%
    modelr::add_predictions(SHImod_HRO)
  pred_frame <- left_join(pred_frame, Tidbit_and_Buoy_data)
  pred_frame %>%
    mutate(SHI_bestguess = ifelse(is.na(SHI_Tidbit), pred, SHI_Tidbit))
}

#Make those predictions!
SHImod_HRO_pred <- make_pred_ts(Tidbit_and_Buoy_data, SHImod_HRO)

#Save those predictions!
saveRDS(SHImod_HRO_pred, "Sebens_temps/Outputs/SHImod_HRO_pred.RDS")

#test
ggplot(SHImod_HRO_pred,
       aes(x = Date)) +
  geom_line(aes(y = pred), color = "red") +
  geom_point(aes(y = SHI_Tidbit), color = "blue", size = 0.1)+
  ggtitle("HRO Interpolated prediction of SHI")

#For DB-SHI
make_pred_ts <- function(dat, SHImod_DB){ # 'dat' is created in/for/by the function in the next line
  pred_frame <- dat #now 'dat' exists
  pred_frame <-pred_frame %>%
    modelr::add_predictions(SHImod_DB)
  pred_frame <- left_join(pred_frame, Tidbit_and_Buoy_data)
  pred_frame %>%
    mutate(SHI_bestguess = ifelse(is.na(SHI_Tidbit), pred, SHI_Tidbit))
}

#Make those predictions!
SHImod_DB_pred <- make_pred_ts(Tidbit_and_Buoy_data, SHImod_DB)

#Save those predictions!
saveRDS(SHImod_DB_pred, "Sebens_temps/Outputs/SHImod_DB_pred.RDS")

#test
ggplot(SHImod_DB_pred,
       aes(x = Date)) +
  geom_line(aes(y = pred), color = "red") +
  geom_point(aes(y = SHI_Tidbit), color = "blue", size = 0.1)+
  ggtitle("DB Interpolated prediction of SHI")

#For SHO-SHI
make_pred_ts <- function(dat, SHImod_SHO){ # 'dat' is created in/for/by the function in the next line
  pred_frame <- dat #now 'dat' exists
  pred_frame <-pred_frame %>%
    modelr::add_predictions(SHImod_SHO)
  pred_frame <- left_join(pred_frame, Tidbit_and_Buoy_data)
  pred_frame %>%
    mutate(SHI_bestguess = ifelse(is.na(SHI_Tidbit), pred, SHI_Tidbit))
}

#Make those predictions!
SHImod_SHO_pred <- make_pred_ts(Tidbit_and_Buoy_data, SHImod_SHO)

#Save those predictions!
saveRDS(SHImod_SHO_pred, "Sebens_temps/Outputs/SHImod_SHO_pred.RDS")

#test
ggplot(SHImod_SHO_pred,
       aes(x = Date)) +
  geom_line(aes(y = pred), color = "red") +
  geom_point(aes(y = SHI_Tidbit), color = "blue", size = 0.1)+
  ggtitle("SHO Interpolated prediction of SHI")


#######################################################################################################
#######################################################################################################
#'--------------------------------------------------------------
#' Create Hierarchical Supplementary Tidbit Data
#'--------------------------------------------------------------

# Select Needed Columns from Predictions
#SHI

#View(SHImod_HRI_pred)
Interp_HRI_SHI <- as.data.frame(SHImod_HRI_pred) %>%
  subset(., select = c(1:4,14,16)) %>%
  rename(pred_via_HRI = 'pred')

Interp_HRO_SHI <- as.data.frame(SHImod_HRO_pred) %>%
  subset(., select = c(1:4,14,16)) %>%
  rename(pred_via_HRO = 'pred')

Interp_DB_SHI <- as.data.frame(SHImod_DB_pred) %>%
  subset(., select = c(1:4,14,16)) %>%
  rename(pred_via_DB = 'pred')

Interp_SHO_SHI <- as.data.frame(SHImod_SHO_pred) %>%
  subset(., select = c(1:4,14,16)) %>%
  rename(pred_via_SHO = 'pred')

Interp_SHI_44029_20m <- buoy44029_20m_predSHI %>%
  subset(., select = c(1:4,14,16)) %>%
  rename(pred_via_44029_20m = 'pred')

Interp_SHI_44029_2m <- buoy44029_2m_predSHI %>%
  subset(., select = c(1:4,14,16)) %>%
  rename(pred_via_44029_2m = 'pred')

Interp_SHI_44013 <- buoy44013_predSHI %>%
  subset(., select = c(1:4,14,16)) %>%
  rename(pred_via_44013 = 'pred')

Interp_SHI_44098 <- buoy44098_predSHI %>%
  subset(., select = c(1:4,14,16)) %>%
  rename(pred_via_44098 = 'pred')

Interp_SHI_44005 <- buoy44005_predSHI %>%
  subset(., select = c(1:4,14,16)) %>%
  rename(pred_via_44005 = 'pred')

Interp_SHI_OISST <- oisst_predSHI %>%
  subset(., select = c(1:4,14,16)) %>%
  rename(pred_via_OISST = 'pred')

SHI_Interpolation_raw <- (list(Interp_HRI_SHI,Interp_HRO_SHI,Interp_DB_SHI,Interp_SHI_44029_20m,
                               Interp_SHI_44013,Interp_SHI_44098,Interp_SHI_44005,Interp_SHI_OISST,
                               Interp_SHI_44029_2m,Interp_SHO_SHI) %>% 
                           reduce(left_join))

SHI_Interpolation <- SHI_Interpolation_raw %>%
  mutate(final_value = 
           case_when(
             !is.na(SHI_Tidbit) ~ SHI_Tidbit,
             !is.na(pred_via_DB) ~ pred_via_DB,
             !is.na(pred_via_SHO) ~ pred_via_SHO,
             !is.na(pred_via_HRO) ~ pred_via_HRO,
             !is.na(pred_via_HRI) ~ pred_via_HRI,
             !is.na(pred_via_44029_20m) ~ pred_via_44029_20m,
             !is.na(pred_via_44098) ~ pred_via_44098,
             !is.na(pred_via_44029_2m) ~ pred_via_44029_2m,
             !is.na(pred_via_44013) ~ pred_via_44013,
             !is.na(pred_via_OISST) ~ pred_via_OISST,
             !is.na(pred_via_44005) ~ pred_via_44005,
             TRUE ~ NA_real_))

View(SHI_Interpolation)
#write_csv(SHI_Interpolation,"Outputs/SHI_Interpolation.csv")

#############################################################
#Plot
SHI_Interpolation_plot <- ggplot(SHI_Interpolation,
       aes(x = Date)) +
  geom_line(aes(y = final_value), color = "red") +
  geom_point(aes(y = SHI_Tidbit), color = "blue", size = 0.1)+
  ggtitle("Interpolated prediction of SHI Temperatures at 8m")+
  labs(y = expression(paste("Temperature", "\u00b0C")), x = expression("Date"))

ggsave("Figures/SHI_Interpolation_plot.jpg")

##################################################################################################################
# NOW TRY TO RECREATE FOR Annual summer means RATHER THAN DAILY MEANS!
###################################################################################################################### Notes and other code bits ####

SHI_Interpolation2 <- SHI_Interpolation |>
  filter(MM %in% c(6:9))|>
  group_by(YY)|>
  summarize(mean = mean(final_value))

View(SHI_Interpolation2)

SHI_Summer_plot <- ggplot(SHI_Interpolation2,
       aes(x = YY)) +
  geom_line(aes(y = mean))+
  geom_point(aes(y = mean))+
  theme_set(theme_classic(base_size = 18)) +
  ggtitle("Interpolated prediction of SHI Temperatures at 8m") +
  labs(y = expression(paste("Mean Annual Summer Temperature", "\u00b0C")), x = expression("Date"))

ggsave("Figures/SHI_Summer_plot.jpg")

#write_csv(SHI_Interpolation2,"Outputs/SHI_Interpolation_Summer.csv")
