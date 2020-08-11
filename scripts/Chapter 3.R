library(fpp3)
library(tidyverse)


## 3.2 -----------------------------------------------

us_retail_employment <- us_employment %>%
  filter(year(Month) >= 1990, Title == "Retail Trade") %>%
  select(-Series_ID)

us_retail_employment %>%
  autoplot(Employed) +
  xlab("Year") + ylab("Persons (thousands)") +
  ggtitle("Total employment in US retail")

dcmp <- us_retail_employment %>% 
  model(STL(Employed))
components(dcmp)

# trend column of dcmp follows the trend component of the employment data without
# regard for seasonal changes and random fluctuations. Essentially this is smoothing
# the overall time series to create a trend line that's easier to follow but not 
# necessarily well represented.

us_retail_employment %>%
  autoplot(Employed, color='gray') +
  autolayer(components(dcmp), trend, color='red') +
  xlab("Year") + ylab("Persons (thousands)") +
  ggtitle("Total employment in US retail")

# Plotting all components of the STL decomposition in one plot...

components(dcmp) %>% autoplot() + xlab("Year")

# 3.3 -------------------------------------------------

us_retail_employment_ma <- us_retail_employment %>%
  mutate(
    `12-MA` = slide_dbl(Employed, mean, .size = 12, .align = "cr"),
    `2x12-MA` = slide_dbl(`12-MA`, mean, .size = 2, .align = "cl")
  )

us_retail_employment_ma %>%
  autoplot(Employed, color='gray') +
  autolayer(us_retail_employment_ma, vars(`2x12-MA`), color='red') +
  xlab("Year") + ylab("Persons (thousands)") +
  ggtitle("Total employment in US retail")

# 3.4 ------------------------------------------------

# classical decomposition - additive

us_retail_employment %>% 
  model(classical_decomposition(Employed, type = "additive")) %>% 
  components() %>% 
  autoplot() + xlab("Year") +
  ggtitle("Classical additive decomposition of total US retail employment")

# classical decomposition - multiplicative

us_retail_employment %>% 
  model(classical_decomposition(Employed, type = "multiplicative")) %>% 
  components() %>% 
  autoplot() + xlab("Year") +
  ggtitle("Classical additive decomposition of total US retail employment")

# 3.5 --------------------------------------------------

# X11 decomposition

x11_dcmp <- us_retail_employment %>%
  model(x11 = feasts:::X11(Employed, type = "additive")) %>%
  components()

autoplot(x11_dcmp) + xlab("Year") +
  ggtitle("Additive X11 decomposition of US retail employment in the US")

x11_dcmp %>%
  ggplot(aes(x = Month)) +
  geom_line(aes(y = Employed, colour = "Data")) +
  geom_line(aes(y = season_adjust, colour = "Seasonally Adjusted")) +
  geom_line(aes(y = trend, colour = "Trend")) +
  xlab("Year") + ylab("Persons (thousands)") +
  ggtitle("Total employment in US retail") +
  scale_colour_manual(values=c("gray","blue","red"),
                      breaks=c("Data","Seasonally Adjusted","Trend"))

# seasonal sub-series plot

x11_dcmp %>%
  gg_subseries(seasonal)

# 3.6 ---------------------------------------------------

# SEATS decomposition

seats_dcmp <- us_retail_employment %>%
  model(seats = feasts:::SEATS(Employed)) %>%
  components()

autoplot(seats_dcmp) + xlab("Year") +
  ggtitle("SEATS decomposition of total US retail employment")

