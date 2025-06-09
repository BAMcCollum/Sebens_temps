# Interpolation models for Dive Beach

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
#### DB Models ####

#fit a MLR model to each temperature record (Buoys 44029, 44013, 44005, 44098, OISST, HRO, & HRI)

DBmod_44029_2m <- lm(DB_Tidbit ~ Buoy44029_2m *MM, data = Tidbit_and_Buoy_data) 
car::Anova(DBmod_44029_2m)
performance::r2(DBmod_44029_2m) #0.883

DBmod_44029_20m <- lm(DB_Tidbit ~ Buoy44029_20m *MM, data = Tidbit_and_Buoy_data)
car::Anova(DBmod_44029_20m)
performance::r2(DBmod_44029_20m) #0.918

DBmod_44005 <- lm(DB_Tidbit ~ Buoy44005 *MM, data = Tidbit_and_Buoy_data)
car::Anova(DBmod_44005)
performance::r2(DBmod_44005) #0.864

DBmod_44013 <- lm(DB_Tidbit ~ Buoy44013 *MM, data = Tidbit_and_Buoy_data)
car::Anova(DBmod_44013)
performance::r2(DBmod_44013) #0.879

DBmod_44098 <- lm(DB_Tidbit ~ Buoy44098 *MM, data = Tidbit_and_Buoy_data)
car::Anova(DBmod_44098)
performance::r2(DBmod_44098) #0.907

DBmod_OISST <- lm(DB_Tidbit ~ OISST *MM, data = Tidbit_and_Buoy_data)
car::Anova(DBmod_OISST)
performance::r2(DBmod_OISST) #0.876

DBmod_HRI <- lm(DB_Tidbit ~ HRI_Tidbit *MM, data = Tidbit_and_Buoy_data)
car::Anova(DBmod_HRI)
performance::r2(DBmod_HRI) #0.914

DBmod_HRO <- lm(DB_Tidbit ~ HRO_Tidbit *MM, data = Tidbit_and_Buoy_data)
car::Anova(DBmod_HRO)
performance::r2(DBmod_HRO) #0.949

DBmod_SHI <- lm(DB_Tidbit ~ SHI_Tidbit *MM, data = Tidbit_and_Buoy_data)
car::Anova(DBmod_SHI)
performance::r2(DBmod_SHI) #0.992

DBmod_SHO <- lm(DB_Tidbit ~ SHO_Tidbit *MM, data = Tidbit_and_Buoy_data)
car::Anova(DBmod_SHO)
performance::r2(DBmod_SHO) #0.986

#######################################################################################################
#'--------------------------------------------------------------
#' Create extrapolated records based on model fits & Real data
#'--------------------------------------------------------------

#make buoy predicted Data Frames

#DB-Buoy44029_2m
make_pred_ts <- function(dat, DBmod_44029_2m){ # 'dat' is created in/for/by the function in the next line
  pred_frame <- dat #now 'dat' exists
  pred_frame <-pred_frame %>%
    modelr::add_predictions(DBmod_44029_2m)
  pred_frame <- left_join(pred_frame, Tidbit_and_Buoy_data)
  pred_frame %>%
    mutate(DB_bestguess = ifelse(is.na(DB_Tidbit), pred, DB_Tidbit))
}

#Make those predictions!
buoy44029_2m_predDB <- make_pred_ts(Tidbit_and_Buoy_data, DBmod_44029_2m)

#Save those predictions!
saveRDS(buoy44029_2m_predDB, "Sebens_temps/Outputs/DBbuoy44029_2m_pred.RDS")

#test
ggplot(buoy44029_2m_predDB,
       aes(x = Date)) +
  geom_line(aes(y = pred), color = "red") +
  geom_point(aes(y = DB_Tidbit), color = "blue", size = 0.1)+
  ggtitle("Buoy 44029 (2m depth) Interpolated prediction of DB")

#DB-Buoy44029_20m
make_pred_ts <- function(dat, DBmod_44029_20m){ # 'dat' is created in/for/by the function in the next line
  pred_frame <- dat #now 'dat' exists
  pred_frame <-pred_frame %>%
    modelr::add_predictions(DBmod_44029_20m)
  pred_frame <- left_join(pred_frame, Tidbit_and_Buoy_data)
  pred_frame %>%
    mutate(DB_bestguess = ifelse(is.na(DB_Tidbit), pred, DB_Tidbit))
}

#Make those predictions!
buoy44029_20m_predDB <- make_pred_ts(Tidbit_and_Buoy_data, DBmod_44029_20m)

#Save those predictions!
saveRDS(buoy44029_20m_predDB, "Sebens_temps/Outputs/DBbuoy44029_20m_pred.RDS")

#test
ggplot(buoy44029_20m_predDB,
       aes(x = Date)) +
  geom_line(aes(y = pred), color = "red") +
  geom_point(aes(y = DB_Tidbit), color = "blue", size = 0.1)+
  ggtitle("Buoy 44029 (20m depth) Interpolated prediction of DB")

#Buoy44005
make_pred_ts <- function(dat, DBmod_44005){ # 'dat' is created in/for/by the function in the next line
  pred_frame <- dat #now 'dat' exists
  pred_frame <-pred_frame %>%
    modelr::add_predictions(DBmod_44005)
  pred_frame <- left_join(pred_frame, Tidbit_and_Buoy_data)
  pred_frame %>%
    mutate(DB_bestguess = ifelse(is.na(DB_Tidbit), pred, DB_Tidbit))
}

#Make those predictions!
buoy44005_predDB <- make_pred_ts(Tidbit_and_Buoy_data, DBmod_44005)

#Save those predictions!
saveRDS(buoy44005_predDB, "Sebens_temps/Outputs/buoy44005_predDB.RDS")

#test
ggplot(buoy44005_predDB,
       aes(x = Date)) +
  geom_line(aes(y = pred), color = "red") +
  geom_point(aes(y = DB_Tidbit), color = "blue", size = 0.1)+
  ggtitle("Buoy 44005 Interpolated prediction of DB")

#Buoy44013
make_pred_ts <- function(dat, DBmod_44013){ # 'dat' is created in/for/by the function in the next line
  pred_frame <- dat #now 'dat' exists
  pred_frame <-pred_frame %>%
    modelr::add_predictions(DBmod_44013)
  pred_frame <- left_join(pred_frame, Tidbit_and_Buoy_data)
  pred_frame %>%
    mutate(DB_bestguess = ifelse(is.na(DB_Tidbit), pred, DB_Tidbit))
}

#Make those predictions!
buoy44013_predDB <- make_pred_ts(Tidbit_and_Buoy_data, DBmod_44013)

#Save those predictions!
saveRDS(buoy44013_predDB, "Sebens_temps/Outputs/buoy44013_predDB.RDS")
#readr::write_csv(buoy44013_pred, "Sebens_temps/Outputs/buoy44013_pred.csv")

#test
ggplot(buoy44013_predDB,
       aes(x = Date)) +
  geom_line(aes(y = pred), color = "red") +
  geom_point(aes(y = DB_Tidbit), color = "blue", size = 0.1)+
  ggtitle("Buoy 44013 Interpolated prediction of DB")


#Buoy44098
make_pred_ts <- function(dat, DBmod_44098){ # 'dat' is created in/for/by the function in the next line
  pred_frame <- dat #now 'dat' exists
  pred_frame <-pred_frame %>%
    modelr::add_predictions(DBmod_44098)
  pred_frame <- left_join(pred_frame, Tidbit_and_Buoy_data)
  pred_frame %>%
    mutate(DB_bestguess = ifelse(is.na(DB_Tidbit), pred, DB_Tidbit))
}

#Make those predictions!
buoy44098_predDB <- make_pred_ts(Tidbit_and_Buoy_data, DBmod_44098)

#Save those predictions!
saveRDS(buoy44098_predDB, "Sebens_temps/Outputs/buoy44098_predDB.RDS")

#test
ggplot(buoy44098_predDB,
       aes(x = Date)) +
  geom_line(aes(y = pred), color = "red") +
  geom_point(aes(y = DB_Tidbit), color = "blue", size = 0.1)+
  ggtitle("Buoy 44098 Interpolated prediction of DB")


#For DB-OISST
make_pred_ts <- function(dat, DBmod_OISST){ # 'dat' is created in/for/by the function in the next line
  pred_frame <- dat #now 'dat' exists
  pred_frame <-pred_frame %>%
    modelr::add_predictions(DBmod_OISST)
  pred_frame <- left_join(pred_frame, Tidbit_and_Buoy_data)
  pred_frame %>%
    mutate(DB_bestguess = ifelse(is.na(DB_Tidbit), pred, DB_Tidbit))
}

#Make those predictions!
oisst_predDB <- make_pred_ts(Tidbit_and_Buoy_data, DBmod_OISST)

#Save those predictions!
saveRDS(oisst_predDB, "Sebens_temps/Outputs/DBoisst_pred.RDS")
#readr::write_csv(oisst_pred, "Sebens_temps/Outputs/oisst_pred.csv")

#test
ggplot(oisst_predDB,
       aes(x = Date)) +
  geom_line(aes(y = pred), color = "red") +
  geom_point(aes(y = DB_Tidbit), color = "blue", size = 0.1)+
  ggtitle("OISST Interpolated prediction of DB")

#For HRI-DB
make_pred_ts <- function(dat, DBmod_HRI){ # 'dat' is created in/for/by the function in the next line
  pred_frame <- dat #now 'dat' exists
  pred_frame <-pred_frame %>%
    modelr::add_predictions(DBmod_HRI)
  pred_frame <- left_join(pred_frame, Tidbit_and_Buoy_data)
  pred_frame %>%
    mutate(DB_bestguess = ifelse(is.na(DB_Tidbit), pred, DB_Tidbit))
}

#Make those predictions!
DBmod_HRI_pred <- make_pred_ts(Tidbit_and_Buoy_data, DBmod_HRI)

#Save those predictions!
saveRDS(DBmod_HRI_pred, "Sebens_temps/Outputs/DBmod_HRI_pred.RDS")

#test
ggplot(DBmod_HRI_pred,
       aes(x = Date)) +
  geom_line(aes(y = pred), color = "red") +
  geom_point(aes(y = DB_Tidbit), color = "blue", size = 0.1)+
  ggtitle("HRI Interpolated prediction of DB")

#For HRO-DB
make_pred_ts <- function(dat, DBmod_HRO){ # 'dat' is created in/for/by the function in the next line
  pred_frame <- dat #now 'dat' exists
  pred_frame <-pred_frame %>%
    modelr::add_predictions(DBmod_HRO)
  pred_frame <- left_join(pred_frame, Tidbit_and_Buoy_data)
  pred_frame %>%
    mutate(DB_bestguess = ifelse(is.na(DB_Tidbit), pred, DB_Tidbit))
}

#Make those predictions!
DBmod_HRO_pred <- make_pred_ts(Tidbit_and_Buoy_data, DBmod_HRO)

#Save those predictions!
saveRDS(DBmod_HRO_pred, "Sebens_temps/Outputs/DBmod_HRO_pred.RDS")

#test
ggplot(DBmod_HRO_pred,
       aes(x = Date)) +
  geom_line(aes(y = pred), color = "red") +
  geom_point(aes(y = DB_Tidbit), color = "blue", size = 0.1)+
  ggtitle("HRO Interpolated prediction of DB")

#For SHI-DB
make_pred_ts <- function(dat, DBmod_SHI){ # 'dat' is created in/for/by the function in the next line
  pred_frame <- dat #now 'dat' exists
  pred_frame <-pred_frame %>%
    modelr::add_predictions(DBmod_SHI)
  pred_frame <- left_join(pred_frame, Tidbit_and_Buoy_data)
  pred_frame %>%
    mutate(DB_bestguess = ifelse(is.na(DB_Tidbit), pred, DB_Tidbit))
}

#Make those predictions!
DBmod_SHI_pred <- make_pred_ts(Tidbit_and_Buoy_data, DBmod_SHI)

#Save those predictions!
saveRDS(DBmod_SHI_pred, "Sebens_temps/Outputs/DBmod_SHI_pred.RDS")

#test
ggplot(DBmod_SHI_pred,
       aes(x = Date)) +
  geom_line(aes(y = pred), color = "red") +
  geom_point(aes(y = DB_Tidbit), color = "blue", size = 0.1)+
  ggtitle("SHI Interpolated prediction of DB")

#For SHO-DB
make_pred_ts <- function(dat, DBmod_SHO){ # 'dat' is created in/for/by the function in the next line
  pred_frame <- dat #now 'dat' exists
  pred_frame <-pred_frame %>%
    modelr::add_predictions(DBmod_SHO)
  pred_frame <- left_join(pred_frame, Tidbit_and_Buoy_data)
  pred_frame %>%
    mutate(DB_bestguess = ifelse(is.na(DB_Tidbit), pred, DB_Tidbit))
}

#Make those predictions!
DBmod_SHO_pred <- make_pred_ts(Tidbit_and_Buoy_data, DBmod_SHO)

#Save those predictions!
saveRDS(DBmod_SHO_pred, "Sebens_temps/Outputs/DBmod_SHO_pred.RDS")

#test
ggplot(DBmod_SHO_pred,
       aes(x = Date)) +
  geom_line(aes(y = pred), color = "red") +
  geom_point(aes(y = DB_Tidbit), color = "blue", size = 0.1)+
  ggtitle("SHO Interpolated prediction of DB")

#######################################################################################################
#######################################################################################################
#'--------------------------------------------------------------
#' Create Hierarchical Supplementary Tidbit Data
#'--------------------------------------------------------------

# Select Needed Columns from Predictions
#DB

View(DBmod_HRI_pred)
Interp_HRI_DB <- as.data.frame(DBmod_HRI_pred) %>%
  subset(., select = c(1:4,13,16)) %>%
  rename(pred_via_HRI = 'pred')

Interp_HRO_DB <- as.data.frame(DBmod_HRO_pred) %>%
  subset(., select = c(1:4,13,16)) %>%
  rename(pred_via_HRO = 'pred')

Interp_SHI_DB <- as.data.frame(DBmod_SHI_pred) %>%
  subset(., select = c(1:4,13,16)) %>%
  rename(pred_via_SHI = 'pred')

Interp_SHO_DB <- as.data.frame(DBmod_SHO_pred) %>%
  subset(., select = c(1:4,13,16)) %>%
  rename(pred_via_SHO = 'pred')

Interp_DB_44029_20m <- buoy44029_20m_predDB %>%
  subset(., select = c(1:4,13,16)) %>%
  rename(pred_via_44029_20m = 'pred')

Interp_DB_44029_2m <- buoy44029_2m_predDB %>%
  subset(., select = c(1:4,13,16)) %>%
  rename(pred_via_44029_2m = 'pred')

Interp_DB_44013 <- buoy44013_predDB %>%
  subset(., select = c(1:4,13,16)) %>%
  rename(pred_via_44013 = 'pred')

Interp_DB_44098 <- buoy44098_predDB %>%
  subset(., select = c(1:4,13,16)) %>%
  rename(pred_via_44098 = 'pred')

Interp_DB_44005 <- buoy44005_predDB %>%
  subset(., select = c(1:4,13,16)) %>%
  rename(pred_via_44005 = 'pred')

Interp_DB_OISST <- oisst_predDB %>%
  subset(., select = c(1:4,13,16)) %>%
  rename(pred_via_OISST = 'pred')

DB_Interpolation_raw <- (list(Interp_HRI_DB,Interp_HRO_DB,Interp_DB_44029_20m,Interp_DB_44013,
                              Interp_DB_44098,Interp_DB_44005,Interp_DB_OISST, Interp_SHI_DB,
                              Interp_DB_44029_2m,Interp_SHO_DB) %>% 
                           reduce(left_join))

DB_Interpolation <- DB_Interpolation_raw %>%
  mutate(final_value = 
           case_when(
             !is.na(DB_Tidbit) ~ DB_Tidbit,
             !is.na(pred_via_SHI) ~ pred_via_SHI,
             !is.na(pred_via_SHI) ~ pred_via_SHO,
             !is.na(pred_via_HRO) ~ pred_via_HRO,
             !is.na(pred_via_44029_20m) ~ pred_via_44029_20m,
             !is.na(pred_via_HRI) ~ pred_via_HRI,
             !is.na(pred_via_44098) ~ pred_via_44098,
             !is.na(pred_via_44013) ~ pred_via_44013,
             !is.na(pred_via_44029_2m) ~ pred_via_44029_2m,
             !is.na(pred_via_OISST) ~ pred_via_OISST,
             !is.na(pred_via_44005) ~ pred_via_44005,
             TRUE ~ NA_real_))

#View(DB_Interpolation)

#write_csv(DB_Interpolation,"Outputs/DB_Interpolation.csv")

#############################################################
#Plot
DB_Interpolation_plot <- ggplot(DB_Interpolation,
       aes(x = Date)) +
  geom_line(aes(y = final_value), color = "red") +
  geom_point(aes(y = DB_Tidbit), color = "blue", size = 0.1)+
  ggtitle("Interpolated prediction of DB Temperatures at 8m")+
  labs(y = expression(paste("Temperature", "\u00b0C")), x = expression("Date"))

ggsave("Figures/DB_Interpolation_plot.jpg")

##################################################################################################################
# NOW RECREATE FOR Annual summer means RATHER THAN DAILY MEANS!
###################################################################################################################### Notes and other code bits ####

DB_Interpolation2 <- DB_Interpolation |>
  filter(MM %in% c(6:9))|>
  group_by(YY)|>
  summarize(mean = mean(final_value))

#View(DB_Interpolation2)

DB_Summer_plot <- ggplot(DB_Interpolation2,
       aes(x = YY)) +
  geom_line(aes(y = mean))+
  geom_point(aes(y = mean))+
  theme_set(theme_classic(base_size = 18)) +
  ggtitle("Interpolated prediction of DB Temperatures at 8m") +
  labs(y = expression(paste("Mean Annual Summer Temperature", "\u00b0C")), x = expression("Date"))

ggsave("Figures/DB_Summer_plot.jpg")

#write_csv(DB_Interpolation2,"Outputs/DB_Interpolation_Summer.csv")
