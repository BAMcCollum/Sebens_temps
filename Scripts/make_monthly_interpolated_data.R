#' ---------------
#' Make interpolated data sets for monthly data
#' ---------------
setwd(here::here()) 

library(purrr)
library(dplyr)
library(lubridate)
library(readr)
library(broom)
library(tidyr)

monthly_temps <- read_csv("Outputs/monthly_joined_data.csv")
proxies <- names(monthly_temps)[c(3:8, 14:16)]
tidbits <- names(monthly_temps)[9:13]

mt <- monthly_temps |>
  pivot_longer(!!proxies,
               names_to = "proxy",
               values_to = "proxy_temp_c")|>
  pivot_longer(!!tidbits,
               names_to = "tidbit",
               values_to = "temp_c") |>
  mutate(MM = as.character(MM)) |>
  group_by(proxy, tidbit) |>
  nest() |>
  mutate(
    mod = map(data, ~lm(temp_c ~ proxy_temp_c, data = .x)),
    mod_month = map(data, ~lm(temp_c ~ proxy_temp_c*MM, data = .x)),
    rmse = map2(mod, data, modelr::rmse),
    rmse_month = map2(mod_month, data, modelr::rmse),
    r2 = map2(mod, data, modelr::rsquare),
    r2_month = map2(mod_month, data, modelr::rsquare),
    interpolate = map2(mod, data, predict),
    interpolate_monthly = map2(mod_month, data, predict)
  )

## How far back does each proxy go?
mt |>
  unnest(data) |>
  select(YY, proxy, proxy_temp_c) |>
  filter(!is.na(proxy_temp_c)) |>
  group_by(proxy) |>
  summarize(from = min(YY),
            to = max(YY)) |>
  arrange(from)

## Examine fit
fit_stats <- mt |>
  unnest(rmse:r2_month) |>
  select(proxy, tidbit, rmse:r2_month)

fit_stats |>
  select(proxy, tidbit, rmse, rmse_month) |>
  pivot_longer(cols = c(rmse, rmse_month),
               names_to = "rmse_from",
               values_to = "rmse") |>
  ggplot(aes(x = proxy, y = rmse, 
             color = rmse_from, shape = tidbit, group = tidbit)) +
  geom_point(size = 2) + 
  coord_flip() +
  scale_color_manual(values = c("goldenrod", "royalblue")) +
  theme_bw()

# r2
fit_stats |>
  select(proxy, tidbit, r2, r2_month) |>
  pivot_longer(cols = c(r2, r2_month),
               names_to = "r2_from",
               values_to = "r2") |>
  ggplot(aes(x = proxy, y = r2, 
             color = r2_from, shape = tidbit, group = tidbit)) +
  geom_point(size = 2) + 
  coord_flip() +
  scale_color_manual(values = c("goldenrod", "royalblue")) +
  theme_bw()

## Compare pred-obs
pred_obs <- mt |>
  unnest(cols = c(data, interpolate, interpolate_monthly)) |>
  ungroup() |>
  filter(!is.na(temp_c))

ggplot(pred_obs,
       aes(x = interpolate, y = temp_c)) +
  geom_point() +
  facet_grid(vars(proxy), vars(tidbit)) 

ggplot(pred_obs,
       aes(x = interpolate_monthly, y = temp_c)) +
  geom_point() +
  facet_grid(vars(proxy), vars(tidbit))

## Check bias
bias_check <- pred_obs |>
  mutate(
    bias = temp_c - interpolate,
    bias_monthly = temp_c - interpolate_monthly,
  ) |>
  select(proxy, MM, bias, bias_monthly) |>
  pivot_longer(cols = c(bias, bias_monthly),
               names_to = "bias_from",
               values_to = "bias") |>
  filter(!is.na(bias))

ggplot(bias_check,
       aes(x = MM, y = bias, fill = bias_from)) +
  geom_boxplot(position = "dodge") +
  facet_wrap(vars(proxy)) +
  scale_fill_manual(values = c("goldenrod", "royalblue")) +
  geom_hline(yintercept = 0, color = "red", lty = 2) +
  theme_bw() 


## Make interpolated timeseries
interp_data <- mt |>
  unnest(c(data, interpolate)) |>
  select(proxy, tidbit, YY, MM, temp_c, interpolate) |>
  ungroup() |>
  mutate(is_interpolated = ifelse(is.na(temp_c), TRUE, FALSE),
         temp_c_complete = ifelse(is.na(temp_c), interpolate, temp_c)) |>
  filter(proxy == "godas_15") |>
  mutate(date = ymd(paste(YY, MM, "01", sep = "-"))) |>
  filter(YY <=2020)

ggplot(interp_data,
       aes(x = date, y = temp_c_complete, 
           color = is_interpolated, group = 1)) +
  geom_point() +
  geom_line() +
  facet_wrap(vars(tidbit))

## write out
write_csv(interp_data, "Outputs/godas_15_interpolated.csv")
