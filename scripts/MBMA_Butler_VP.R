library(readxl)
library(janitor)
library(tidyverse)
library(fpp3)
library(tidyquant)
library(lubridate)

quandl_api_key("SecuBhbFjQZ4vHsBjsiE")
quandl_data <- c("USTREASURY/YIELD", "CHRIS/CME_HR1", "FRBP/ADS_VINTAGES_MOSTRECENT", "FRED/PCU327320327320", "FRED/WPU081")

# Data from Dylan -- AIA, Butler and VP data plus MBMA data
mbma_butler_vp <- read_excel("~/R/R Data/mbma_butler_vp.xlsx", 
                             col_types = c("numeric", "text", "date", 
                                           "numeric", "numeric", "numeric", 
                                           "numeric", "numeric", "numeric", 
                                           "numeric", "numeric", "numeric", 
                                           "numeric", "numeric", "numeric", 
                                           "numeric", "date", "numeric", "numeric", 
                                           "numeric", "numeric", "numeric", 
                                           "numeric")) %>% 
  clean_names()

# Look at BBNA sales

mbma_butler_vp %>% 
  filter(monthy_yr >= "2010-07-01") %>% 
  mutate(bbna = butler + vp,
         log_butler = log(butler),
         season_diff = difference(butler, 12),
         log_season_diff = difference(log(butler), 12)) %>%  # making sales stationary using log of sales then calculating seasonal difference
  ggplot(aes(monthy_yr)) +
  geom_line(aes(y = log_season_diff))

  # Data from Ed -- Standard Cost changes

standard_chg <- read_excel("data/standard_chg.xlsx", 
                           col_types = c("date", "skip", "skip", 
                                         "skip", "skip", "numeric", 
                                         "numeric")) %>% 
  mutate(month_yr = floor_date(month_yr, unit = "month"))

# use tidyquant to pull economic data from quandl
quandl_df <- quandl_data %>% 
  tq_get(get = "quandl",
         from = "2001-01-01", # oldest bbna data available to me
         collapse = "monthly")
# split out just 10 year treasury
ust_10yr <- quandl_df %>% 
  filter(symbol == "USTREASURY/YIELD") %>% 
  mutate(month_yr = floor_date(date, unit = "month")) %>% 
  select(month_yr,
         x10.yr)
# cru history
cru_history <- quandl_df %>% 
  filter(symbol == "CHRIS/CME_HR1") %>% 
  mutate(month_yr = floor_date(date, unit = "month")) %>% 
  select(month_yr,
         last) %>% 
  rename(cru = last)
# ADS Business Conditions index
ads_history <- quandl_df %>% 
  filter(symbol == "FRBP/ADS_VINTAGES_MOSTRECENT") %>% 
  mutate(month_yr = floor_date(date, unit = "month")) %>% 
  select(month_yr,
         value) %>% 
  rename(ads_bus_cond = value)
# ppi: ready-mix concrete prices
concrete_ppi <- quandl_df %>% 
  filter(symbol == "FRED/PCU327320327320") %>% 
  mutate(month_yr = floor_date(date, unit = "month")) %>% 
  select(month_yr,
         value) %>% 
  rename(concrete_ppi = value)
# ppi: lumber and wood prices
lumber_ppi <- quandl_df %>% 
  filter(symbol == "FRED/WPU081") %>% 
  mutate(month_yr = floor_date(date, unit = "month")) %>% 
  select(month_yr,
         value) %>% 
  rename(lumber_ppi = value)

model_data <- mbma_butler_vp %>% 
  mutate(bbna_dollars = butler + vp,
         lag_aia_12 = lag(architecture_billings_index, n = 12),
         monthy_yr = yearmonth(monthy_yr)) %>%  
  select(monthy_yr,
         month_yr,
         bbna_dollars,
         butler,
         vp,
         lag_aia_12) %>% 
  left_join(ust_10yr, by = c("month_yr" = "month_yr")) %>% 
  left_join(cru_history, by = c("month_yr" = "month_yr")) %>% 
  left_join(standard_chg, by = c("month_yr" = "month_yr")) %>% 
  left_join(ads_history, by = c("month_yr" = "month_yr")) %>% 
  left_join(concrete_ppi, by = c("month_yr" = "month_yr")) %>% 
  left_join(lumber_ppi, by = c("month_yr" = "month_yr")) %>% 
  select(-month_yr)

# explore ------------------------------------------
model_data %>% 
  select(1, 3, 6:8) -> model_explore1

model_explore1 %>% 
  GGally::ggpairs(columns = 2:5)

model_data %>% 
  select(1, 3, 9:13) -> model_explore2

model_explore2 %>% 
  GGally::ggpairs(columns = 2:7)

aia_explore %>% 
 as_tsibble(index = monthy_yr) %>%  
  model(tslm = TSLM(bbna_dollars ~ lag_aia_5)) -> aia_bbna_fit

# model & forecast -------------------------------------------------
training_set <- model_data %>% 
  filter(as.Date(monthy_yr) < "2019-07-01")

test_set <- model_data %>% 
  filter(as.Date(monthy_yr) >= "2019-07-01") %>% 
  as_tsibble(index = monthy_yr)

training_set %>% 
  as_tsibble(index = monthy_yr) %>% 
  model(
    tslm = TSLM(bbna_dollars ~ lag_aia_12 + x10.yr + cru + lag1_standard_chg + lag2_standard_chg + ads_bus_cond + concrete_ppi + lumber_ppi + season() + trend())
  ) -> fit_bbna_oe

report(fit_bbna_oe)

# look at the fitted values on top of actuals
augment(fit_bbna_oe) %>% 
  ggplot(aes(monthy_yr)) +
  geom_line(aes(y = bbna_dollars, color = "Data")) +
  geom_line(aes(y = .fitted, color = "Fitted")) +
  xlab("Month") + ylab(NULL) +
  ggtitle("Monthly BBNA Order Entry (Domestic, 000's of $)") +
  guides(color = guide_legend(title = NULL))

# want to see a bad model predict the future?
fc_bbna <- fit_bbna_oe %>% 
  forecast(new_data = test_set)

fc_bbna %>% 
  autoplot(model_data)

# plot the residuals
fit_bbna_oe %>% gg_tsresiduals()

# AC testing
augment(fit_bbna_oe) %>% 
  features(.resid, ljung_box, lag = 10, dof = 5)
