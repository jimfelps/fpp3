library(fpp3)
library(tidyverse)

# CHAPTER 4 & 5

## 4.5 ------------------------------------------

# scatterplot matrix showing seasonality of travel against purpose of travel

tourism_features <- tourism %>%
  features(Trips, feature_set(pkgs="feasts"))
tourism_features

tourism_features %>%
  select_at(vars(contains("season"), Purpose)) %>%
  mutate(
    seasonal_peak_year = glue::glue("Q{seasonal_peak_year+1}"),
    seasonal_trough_year = glue::glue("Q{seasonal_trough_year+1}"),
  ) %>%
  GGally::ggpairs(mapping = aes(colour=Purpose))

# 5.2 --------------------------------------------

bricks <- aus_production %>% filter(between(year(Quarter), 1970, 2004))

bricks %>% 
  ggplot(aes(Quarter, Bricks)) +
  geom_line()
# none of these work???


bricks %>% model(mean = MEAN(Bricks), naive = NAIVE(Bricks), seasonal_naive = SNAIVE(Bricks))

bricks %>% model(NAIVE(Bricks))
# RW(Bricks) is an equivalent alternative

bricks %>% model(SNAIVE(Bricks ~ lag("year")))



# Set training data from 1992 to 2006
train <- aus_production %>% filter_index("1992 Q1" ~ "2006 Q4")
# Fit the models
beer_fit <- train %>%
  model(
    Mean = MEAN(Beer),
    `Naïve` = NAIVE(Beer),
    `Seasonal naïve` = SNAIVE(Beer)
  )
# Generate forecasts for 14 quarters
beer_fc <- beer_fit %>% forecast(h=14)
# Plot forecasts against actual values
beer_fc %>%
  autoplot(train, level = NULL) +
  autolayer(filter_index(aus_production, "2007 Q1" ~ .), color = "black") +
  ggtitle("Forecasts for quarterly beer production") +
  xlab("Year") + ylab("Megalitres") +
  guides(colour=guide_legend(title="Forecast"))


# Re-index based on trading days
google_stock <- gafa_stock %>%
  filter(Symbol == "GOOG") %>%
  mutate(day = row_number()) %>%
  update_tsibble(index = day, regular = TRUE)
# Filter the year of interest
google_2015 <- google_stock %>% filter(year(Date) == 2015)
# Fit the models
google_fit <- google_2015 %>%
  model(
    Mean = MEAN(Close),
    `Naïve` = NAIVE(Close),
    Drift = NAIVE(Close ~ drift())
  )
# Produce forecasts for the 19 trading days in January 2015
google_fc <- google_fit %>% forecast(h = 19)
# A better way using a tsibble to determine the forecast horizons
google_jan_2016 <- google_stock %>%
  filter(yearmonth(Date) == yearmonth("2016 Jan"))
google_fc <- google_fit %>% forecast(google_jan_2016)
# Plot the forecasts
google_fc %>%
  autoplot(google_2015, level = NULL) +
  autolayer(google_jan_2016, Close, color='black') +
  ggtitle("Google stock (daily ending 31 Dec 2015)") +
  xlab("Day") + ylab("Closing Price (US$)") +
  guides(colour=guide_legend(title="Forecast"))

# these simple forecasting methods can sometimes be used as they provide the best forecasting method.
# based on the google stock price chart, it's apparent that these are not correct choices for forecasting.
# these simple methods are used as benchmarks for more sophisticated forecasting methods to ensure our new 
# method is better than the simple method. Why work harder than you need to?

# 5.4 ---------------------------------------------------

google_2015 %>% autoplot(Close) +
  xlab("Day") + ylab("Closing Price (US$)") +
  ggtitle("Google Stock in 2015")

aug <- google_2015 %>% model(NAIVE(Close)) %>% augment()
aug %>% autoplot(.resid) + xlab("Day") + ylab("") +
  ggtitle("Residuals from naïve method")

aug %>%
  ggplot(aes(x = .resid)) +
  geom_histogram() +
  ggtitle("Histogram of residuals")

aug %>% ACF(.resid) %>% autoplot() + ggtitle("ACF of residuals")

google_2015 %>% model(NAIVE(Close)) %>% gg_tsresiduals()

# lag=h and fitdf=K
# K = 0 bc naive model has no parameters
aug %>% features(.resid, box_pierce, lag=10, dof=0)

aug %>% features(.resid, ljung_box, lag=10, dof=0)

# use drift model on google stock price
fit <- google_2015 %>% model(RW(Close~drift()))
# pass model to tidy function to turn into tidy table. 
# drift model shows one estimated parameter, the drift coefficient
fit %>% tidy()

# apply Ljung-Box test with K = 1 to account for estimated parameter
augment(fit) %>% features(.resid, ljung_box, lag=10, dof=1)


# 5.5 ------------------------------------------------------------

#use hilo function to get tsibble with prediction intervals
google_2015 %>%
  model(NAIVE(Close)) %>%
  forecast(h = 10) %>%
  hilo() # level argument will allow different prediction intervals

# when forecast is plotted, intervals are included by default

google_2015 %>%
  model(NAIVE(Close)) %>%
  forecast(h = 10) %>%
  autoplot(google_2015)

# bootstrapped residuals

fit <- google_2015 %>%
  model(NAIVE(Close))
sim <- fit %>% generate(h = 30, times = 5, bootstrap = TRUE)
sim
# this generated 5 possible sample paths (times argument), which can be plotted...
google_2015 %>%
  ggplot(aes(x = day)) +
  geom_line(aes(y = Close)) +
  geom_line(aes(y = .sim, colour = as.factor(.rep)), data = sim) +
  ggtitle("Google closing stock price") +
  guides(col = FALSE)
# don't actually have to use the generate function as this is built into forecast function
fc <- fit %>% forecast(h = 30, bootstrap = TRUE)
fc
# plot the forecast
fc %>% autoplot(google_2015) +
  ggtitle("Google closing stock price")
# prediction interval is not normal because there is no normality assumption in bootstrapping

# now we can calculate the prediction intervals and compare to the normal distribution
google_2015 %>%
  model(NAIVE(Close)) %>%
  forecast(h = 10, bootstrap = TRUE, times = 1000) %>%
  hilo()
# they're close

# 5.7 ----------------------------------------------------

us_retail_employment <- us_employment %>%
  filter(year(Month) >= 1990, Title == "Retail Trade")
dcmp <- us_retail_employment %>%
  model(STL(Employed ~ trend(window = 7), robust=TRUE)) %>%
  components() %>%
  select(-.model)
dcmp %>%
  model(NAIVE(season_adjust)) %>%
  forecast() %>%
  autoplot(dcmp) + ylab("New orders index") +
  ggtitle("Naive forecasts of seasonally adjusted data")

fit_dcmp <- us_retail_employment %>%
  model(stlf = decomposition_model(
    STL(Employed ~ trend(window = 7), robust = TRUE),
    NAIVE(season_adjust)))

fit_dcmp %>%
  forecast() %>% 
  autoplot(us_retail_employment)

 # 5.8 --------------------------------------------------

# Time series cross-validation accuracy
google_2015_tr <- google_2015 %>%
  slice(1:(n()-1)) %>%
  stretch_tsibble(.init = 3, .step = 1)
fc <- google_2015_tr %>%
  model(RW(Close ~ drift())) %>%
  forecast(h=1)

fc %>% accuracy(google_2015)

# Residual accuracy
google_2015 %>% model(RW(Close ~ drift())) %>% accuracy()

# Forecast horizon accuracy with cross-validation
google_2015_tr <- google_2015 %>%
  slice(1:(n()-8)) %>%
  stretch_tsibble(.init = 3, .step = 1)
# forecast eight steps ahead for each set
fc <- google_2015_tr %>%
  model(RW(Close ~ drift())) %>%
  forecast(h=8) %>%
  group_by(.id) %>%
  mutate(h = row_number()) %>%
  ungroup()
# as h increases (days forecasted outside of actual values) RMSE increases, which makes sense
# because uncertainty increases
fc %>%
  accuracy(google_2015, by = "h") %>%
  ggplot(aes(x = h, y = RMSE)) + geom_point()

# Exercises --------------------------------------------

aus_econ <- global_economy %>% 
  filter(Country == "Australia") %>% 
  select(Year, Population)
# check to make sure this fits a trend
autoplot(aus_econ)

# Yep, lets do a drift model
aus_fit <- aus_econ %>% 
  model(
    RW(Population ~ drift())
  )

aus_fc <- aus_fit %>% 
  forecast(h = 10)

aus_fc %>% 
  autoplot(aus_econ)

# Australian brick production
aus_bricks <- aus_production %>% select(Quarter, Bricks)

autoplot(aus_bricks)
# seasonal data, use SNAIVE
hh_budget %>% count(Animal)
