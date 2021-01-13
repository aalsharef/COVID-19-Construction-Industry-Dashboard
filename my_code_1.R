# Load the libraries ----
library(tidyverse)
library(lubridate)
library(modeltime)
library(fpp3)
library(tidymodels)
library(timetk)
library(readxl)
library(slider)
library(here)
library(extrafont)
library(tidyverse)
library(tsibble)
library(lubridate)
library(fable)
library(smooth)
library(Mcomp)
library(tidyquant)
library(scales)
font_import()
loadfonts(device = "win")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Economic Data ----

  # GDP per capita

tq_get("A939RC0Q052SBEA",get = "economic.data") %>%
  filter(date > "2015-01-01") %>%
  arrange(desc(date))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Total Construction Spending (TTLCONS) ---- Millions of Dollars,


tq_get("TTLCONS",get = "economic.data") %>%
  filter(date > "2015-01-01") %>%
  arrange(desc(date))


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

cat_stocks <- tq_get("CAT", get = "stock.prices")

#view(cat_stocks)

cat_stocks_2 <- cat_stocks %>%
  select(date, close) %>%
  filter(date > "2015-01-01")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sp_500 <- tq_index("SP500") %>%


tq_index("SP500") %>%
  count(sector)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
tq_index("SP500") %>%
  view()

tq_get("SP500",get = "economic.data") %>%
  filter(date > "2015-01-01")


tq_get("WPS1321",get = "economic.data") %>%
  filter(date > "2015-01-01")

tq_get("WPS1321",get = "economic.data") %>%
  filter(date > "2015-01-01")

# NASDAQ Composite Index (NASDAQCOM)

tq_get("NASDAQCOM",get = "economic.data") %>%
  filter(date > "2015-01-01") %>%
  arrange(desc(date))

# Dow Jones Industrial Average

tq_get("DJIA",get = "economic.data") %>%
  filter(date > "2015-01-01") %>%
  arrange(desc(date))

# Crude Oil Prices: West Texas Intermediate (WTI) - Cushing, Oklahoma

tq_get("DCOILWTICO",get = "economic.data") %>%
  filter(date > "2015-01-01") %>%
  arrange(desc(date))

# Unemployment Rate - Seasonally Adjusted

tq_get("UNRATE",get = "economic.data") %>%
  filter(date > "2015-01-01") %>%
  arrange(desc(date))


# All Employees, Construction (USCONS) - U.S. Bureau of Labor Statistics

tq_get("USCONS",get = "economic.data") %>%
  filter(date > "2015-01-01") %>%
  arrange(desc(date))

# Covid-19 data --------

# Today's date

Sys.Date()

cov_dat <- read_csv("https://covidtracking.com/data/download/all-states-history.csv")

# select the variables

cov_dat_2 <- cov_dat %>%
  select(date,	state , positive)

# Group by data - total USA

cov_dat_3 <- cov_dat_2 %>%
  group_by(date) %>%
  summarise(cumulative = sum(positive)) %>%
  ungroup() %>%
  filter(date >= "2020-03-31") %>%
  mutate(daily = cumulative - lag(cumulative, default = first(cumulative))) %>%
  slice(-1L) %>%
  mutate(mov_avg_14 = slider::slide_dbl(daily, mean, .before=7 ,.after=6, .complete=TRUE))


# Group by data - by State

cov_dat_4 <- cov_dat_2 %>%
  group_by(date,state) %>%
  summarise(cumulative = sum(positive)) %>%
  ungroup() %>%
  filter(date >= "2020-03-31") %>%
  arrange(date, state) #%>%
  #mutate(daily = cumulative - lag(cumulative, default = first(cumulative)))

view(cov_dat_4)
view(AK)

# create df for each state ---- think about another way to filter the data
AK <- cov_dat_4 %>% filter(state == "AK") %>% mutate(daily = cumulative - lag(cumulative, default = first(cumulative))) %>%  slice(-1L)


# ggplot usa covid

plt_cov_us <- ggplot(data = cov_dat_3) +
  geom_line(aes(x = date, y = daily), color = "#3182bd", size = .5) +
  geom_line(aes(x = date, y = mov_avg_14), color = "#d53e4f", size = .5) +

  scale_color_discrete(labels = c("Daily Cases", "14 Days MA")) +

  scale_x_date(date_labels = "%m-%d-%Y", breaks = "2 months") +
  scale_y_continuous(labels = comma) +
  labs(x = "Timeline" , y = "Daily Number of Cases", caption = "Data source: COVID Tracking Project", title = "Daily Number of Positive Covid-19 Cases in the U.S. and the 14 Days Moving Average") +
  theme_bw(13) +
  theme(plot.margin = unit(c(16,16,16,16), unit = "pt"),
        text=element_text(family="Times New Roman"))


# colors <- c("Daily Covid-19 Cases" = "blue", "14 Days MA" = "red")

plt_cov_us <- ggplot(data = cov_dat_3) +
  geom_line(aes(x = date, y = daily, color = "Daily Covid-19 Cases") , size = .5) +
  geom_line(aes(x = date, y = mov_avg_14, color = "14 Days MA"), size = .8) +

  scale_x_date(date_labels = "%m-%d-%Y", breaks = "2 months") +
  scale_y_continuous(labels = comma) +
  labs(x = "Timeline" , y = "Daily Number of Cases", caption = "Data source: COVID Tracking Project", title = "Daily Number of Positive Covid-19 Cases in the U.S. and the 14 Days Moving Average (MA)", color = "") +
  scale_color_manual(values = c("Daily Covid-19 Cases" = "#3182bd", "14 Days MA" = "#d53e4f")) +
  theme_bw(15) +
  theme(plot.margin = unit(c(16,16,16,16), unit = "pt"),
        text=element_text(family="Times New Roman"),
        legend.text = element_text(size=14))

# Code to obtain the data ----

# Page 1 - Covid-19 ----

Sys.Date()

  # prep the covid-19 data from the covid tracking project

cov_dat <- read_csv("https://covidtracking.com/data/download/all-states-history.csv")

  # select the variables

cov_dat_2 <- cov_dat %>%
  select(date,	state , positive)

  # Group by data - total USA and add the moving average column from "2020-04-01"

cov_dat_3 <- cov_dat_2 %>%
  group_by(date) %>%
  summarise(cumulative = sum(positive)) %>%
  ungroup() %>%
  filter(date >= "2020-03-31") %>%
  mutate(daily = cumulative - lag(cumulative, default = first(cumulative))) %>%
  slice(-1L) %>%
  mutate(mov_avg_14 = slider::slide_dbl(daily, mean, .before=7 ,.after=6, .complete=TRUE))


  # Group by data - by State

cov_dat_4 <- cov_dat_2 %>%
  group_by(date,state) %>%
  summarise(cumulative = sum(positive)) %>%
  ungroup() %>%
  filter(date >= "2020-03-31") %>%
  arrange(date, state) #%>%
  # mutate(daily = cumulative - lag(cumulative, default = first(cumulative)))

view(cov_dat_4)

# create df for each state ---- think about another way to filter the data
AK <- cov_dat_4 %>% filter(state == "AK") %>% mutate(daily = cumulative - lag(cumulative, default = first(cumulative))) %>%  slice(-1L)


# ggplot usa covid

# plt_cov_us <- ggplot(data = cov_dat_3) +
#   geom_line(aes(x = date, y = daily), color = "#3182bd", size = .5) +
#   geom_line(aes(x = date, y = mov_avg_14), color = "#d53e4f", size = .5) +
#
#   scale_color_discrete(labels = c("Daily Cases", "14 Days MA")) +
#
#   scale_x_date(date_labels = "%m-%d-%Y", breaks = "2 months") +
#   scale_y_continuous(labels = comma) +
#   labs(x = "Timeline" , y = "Daily Number of Positive Cases", caption = "Data source: COVID Tracking Project", title = "Daily Number of Positive Covid-19 Cases in the U.S. and the 14 Days Moving Average") +
#   theme_bw(13) +
#   theme(plot.margin = unit(c(16,16,16,16), unit = "pt"),
#         text=element_text(family="Times New Roman"))


# colors <- c("Daily Covid-19 Cases" = "blue", "14 Days MA" = "red")

  # ggplot usa covid

plt_cov_us <- ggplot(data = cov_dat_3) +
  geom_line(aes(x = date, y = daily, color = "Daily Covid-19 Cases") , size = .5) +
  geom_line(aes(x = date, y = mov_avg_14, color = "14 Days MA"), size = .8) +

  scale_x_date(date_labels = "%m-%d-%Y", breaks = "2 months") +
  scale_y_continuous(labels = comma) +
  labs(x = "Timeline" , y = "Daily Number of Positive Cases", caption = "Data source: COVID Tracking Project", title = "Daily Number of Positive Covid-19 Cases in the U.S. and the 14 Days Moving Average (MA)", color = "") +
  scale_color_manual(values = c("Daily Covid-19 Cases" = "#3182bd", "14 Days MA" = "#d53e4f")) +
  theme_bw(15) +
  theme(plot.margin = unit(c(16,16,16,16), unit = "pt"),
        text=element_text(family="Times New Roman"),
        legend.text = element_text(size=14))

  # Commulative Number of cases

cov_dat_com <- cov_dat_2 %>%
  group_by(date) %>%
  summarise(cumulative = sum(positive)) %>%
  ungroup() %>%
  filter(date >= "2020-03-31")

  # Total Number of Cases to date

Comulative_num_cases <- cov_dat_com %>%
  select(cumulative) %>%
  filter(row_number()==n())


  # Plot comulative


plt_cov_us_com <- ggplot(data = cov_dat_com) +
  geom_line(aes(x = date, y = cumulative), color = "#3182bd", size = .5) +
  scale_x_date(date_labels = "%m-%d-%Y", breaks = "2 months") +
  scale_y_continuous(labels = comma) +
  labs(x = "Timeline" , y = "Cumulative  Number of Positive Cases", caption = "Data source: COVID Tracking Project", title = "Cumulative Number of Positive Covid-19 Cases in the U.S.") +
  theme_bw(13) +
  theme(plot.margin = unit(c(16,16,16,16), unit = "pt"),
        text=element_text(family="Times New Roman"))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Page 2 - Specific Construction Related Indices ----

Sys.Date()

  # All Employees, Construction (USCONS) - U.S. Bureau of Labor Statistics
    # multiplied the number by 1000
  #  Source U.S. Bureau of Labor Statistics
  # https://fred.stlouisfed.org/series/USCONS
  # Seasonally Adjusted
  # unit: Number of jobs

tq_get("USCONS",get = "economic.data") %>%
  filter(date >= "2019-01-01") %>%
  arrange(desc(date)) %>%
  mutate(Number_of_const_jobs = price*1000)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Total Construction Spending
  # https://fred.stlouisfed.org/series/TTLCONS
  # Seasonally Adjusted Annual Rate
  #  U.S. Census Bureau
  # https://www.census.gov/construction/c30/c30index.html
  # Unit: $

tq_get("TTLCONS",get = "economic.data") %>%
  filter(date >= "2019-01-01") %>%
  arrange(desc(date)) %>%
  mutate(Value_of_Const_Put_in_Place = price*1000000)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # 30-Year Fixed Rate Mortgage Average in the United States
  #
  # Not Seasonally Adjusted
  # Source: http://www.freddiemac.com/index.html
  # Source : Freddie Mac

tq_get("MORTGAGE30US",get = "economic.data") %>%
  filter(date >= "2019-01-01") %>%
  arrange(desc(date))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # PPI - Lumber
  # https://beta.bls.gov/dataViewer/view/timeseries/WPU08
  # PPI Commodity data for Lumber and wood products, not seasonally adjusted
  # Not Seasonally Adjusted
  # https://data.bls.gov/timeseries/WPS081&output_view=pct_1mth
    # another source - following the same trend (seasonally adjusted)

tq_get("WPU08",get = "economic.data") %>%
  filter(date >= "2019-01-01") %>%
  arrange(desc(date))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # PPI- Steel
  # https://beta.bls.gov/dataViewer/view/timeseries/WPU101708
  # PPI Commodity data for Metals and metal products-Cold finished steel bars and bar shapes, not seasonally adjusted
  # Not Seasonally Adjusted

tq_get("WPU101708",get = "economic.data") %>%
  filter(date >= "2019-01-01") %>%
  arrange(desc(date))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # PPI - Cement
  # PPI Commodity data for Nonmetallic mineral products-Cement, hydraulic, not seasonally adjusted
  # https://beta.bls.gov/dataViewer/view/timeseries/WPU1322
  # Not Seasonally Adjusted

tq_get("WPU1322",get = "economic.data") %>%
  filter(date >= "2019-01-01") %>%
  arrange(desc(date))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # PPI - Gypsum building materials
  # PPI industry data for Gypsum product manufacturing-Gypsum building materials, not seasonally adjusted
  # https://beta.bls.gov/dataViewer/view/timeseries/PCU3274203274201
# Not Seasonally Adjusted

tq_get("PCU3274203274201",get = "economic.data") %>%
  filter(date >= "2019-01-01") %>%
  arrange(desc(date))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # PPI - asphalt
  # https://beta.bls.gov/dataViewer/view/timeseries/PCU3241213241210131
  # Not Seasonally Adjusted
  # 	PPI industry data for Asphalt paving mixture & block manufacturing-Asphalt and tar paving mixt. (excl liquid), incl bitum. or asph. concrete, asph. paving cement, not seasonally adjusted.
  # # Not Seasonally Adjusted

  # Note, there wasn't increase in the prices. This might be attributed to the drop in oil prices or drop of income for DOTs due to relatively slow traffic and hence less income from gas taxes.

tq_get("PCU3241213241210131",get = "economic.data") %>%
  filter(date >= "2019-01-01") %>%
  arrange(desc(date))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # PPI - Aggregate
  # Treated lightweight aggregate and crushed slag, minerals and earths
  # https://beta.bls.gov/dataViewer/view/timeseries/PCU32799232799204
  # # Not Seasonally Adjusted


tq_get("PCU32799232799204",get = "economic.data") %>%
  filter(date >= "2019-01-01") %>%
  arrange(desc(date))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Page 3 - Macro Economic Indicators ----

  # Unemployment Rate - Seasonally Adjusted
  # https://fred.stlouisfed.org/series/UNRATE
  # Unit: Percent (%)

tq_get("UNRATE",get = "economic.data") %>%
  filter(date > "2019-01-01") %>%
  arrange(desc(date))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Effective Federal Funds Rate (EFFR)
  # Not Seasonally Adjusted
  # freq: Daily


tq_get("EFFR",get = "economic.data") %>%
  filter(date > "2019-01-01") %>%
  arrange(desc(date))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Consumer Price Index for All Urban Consumers: All Items in U.S. City Average
  # https://fred.stlouisfed.org/series/CPIAUCSL
  # index 1982-1984=100
  # Seasonally Adjusted

tq_get("CPIAUCSL",get = "economic.data") %>%
  filter(date > "2019-01-01") %>%
  arrange(desc(date))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Crude Oil Prices: West Texas Intermediate (WTI) - Cushing, Oklahoma
  # Not Seasonally Adjusted
  # updated: Daily

tq_get("DCOILWTICO",get = "economic.data") %>%
  filter(date > "2019-01-01") %>%
  arrange(desc(date))
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Real Gross Domestic Product (GDPC1)
  # Source: U.S. Bureau of Economic Analysis
  # https://fred.stlouisfed.org/series/GDPC1
  # Real gross domestic product is the inflation adjusted value of the goods and services produced by labor and property located in the United States.For more information see the Guide to the National Income and Product Accounts of the United States (NIPA). For more information, please visit the Bureau of Economic Analysis.
  # Note: Billions of Chained 2012 Dollars, Seasonally Adjusted Annual Rate

  # Note: Billions of Chained 2012 Dollars, Seasonally Adjusted Annual Rate

tq_get("GDPC1",get = "economic.data") %>%
  filter(date > "2019-01-01") %>%
  arrange(desc(date))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # GDP per capital
  # Seasonally Adjusted Annual Rate
  # unit $
  # freq: Quarterly


tq_get("A939RC0Q052SBEA",get = "economic.data") %>%
  filter(date > "2019-01-01") %>%
  arrange(desc(date))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Page 4 Stocks of Construction Companies ----

# Page 4A - Stock Market Indices ----

  # S&P 500 (SP500)
  # Units: Index - Not Seasonally Adjusted
  # freq: Daily

tq_get("SP500",get = "economic.data") %>%
  filter(date > "2019-01-01") %>%
  arrange(desc(date))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Dow Jones Industrial Average (DJIA)
  # Units: Index - Not Seasonally Adjusted
  # freq: Daily

tq_get("DJIA",get = "economic.data") %>%
  filter(date > "2019-01-01") %>%
  arrange(desc(date))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # NASDAQ Composite Index (NASDAQCOM)
  #Units: Index Feb 5, 1971=100,
  # Not Seasonally Adjusted - freq: daily

tq_get("NASDAQCOM",get = "economic.data") %>%
  filter(date > "2019-01-01") %>%
  arrange(desc(date))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Page 4B - Top 5 Publicly Traded Construction Companies in a U.S. Stock Market ranked by their Revenue According to ENR 2020  ----

  # Fluor Corporation
  # unit : $
tq_get("FLR", get = "stock.prices") %>%
  select(date, close) %>%
  filter(date > "2019-01-01") %>%
  arrange(desc(date))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # AECOM (ACM)
  # unit : $
tq_get("ACM", get = "stock.prices") %>%
  select(date, close) %>%
  filter(date > "2019-01-01") %>%
  arrange(desc(date))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Mcdermott International Ltd.
  # obtained from tiingo_api
  # May 11, 2018.
  #  stock ceased being listed on the NYSE on May 11, 2018.

  tiingo_api_key('1c445c8d5c44786ecb81c42a7dbf067e92e62519')

  tq_get(c("MCDIF"), get = "tiingo") %>%
  view()
  select(date, close) %>%
  arrange(desc(date)) %>%
  mutate(date = date(date))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Skanska AB (publ) (SKSBF)

tq_get("SKSBF", get = "stock.prices") %>%
  select(date, close) %>%
  filter(date > "2019-01-01") %>%
  arrange(desc(date))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # Tutor Perini Corporation (TPC)

tq_get("TPC", get = "stock.prices") %>%
  select(date, close) %>%
  filter(date > "2019-01-01") %>%
  arrange(desc(date))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  Page 4C -Related Construction Companies Stocks from Different Sectors ----

  # Capital Projects Owners
  # Chevron Corporation (CVX)
tq_get("CVX", get = "stock.prices", from = "2019-01-01") %>%
  select(date, close) %>%
  arrange(desc(date))

  # Dow Inc. (DOW)
tq_get("DOW", get = "stock.prices", from = "2019-01-01") %>%
  select(date, close) %>%
  arrange(desc(date))


  # Residential Construction Companies
    # Lennar Corporation (LEN)

tq_get("LEN", get = "stock.prices") %>%
  select(date, close) %>%
  filter(date > "2019-01-01") %>%
  arrange(desc(date))

    # D.R. Horton,  Inc. (DHI)

tq_get("DHI", get = "stock.prices") %>%
  select(date, close) %>%
  filter(date > "2019-01-01") %>%
  arrange(desc(date))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # Farm & Construction Equipment
      # Deere & Company

tq_get("DE", get = "stock.prices") %>%
  select(date, close) %>%
  filter(date > "2019-01-01") %>%
  arrange(desc(date))

    # Caterpillar Inc. (CAT)

tq_get("CAT", get = "stock.prices") %>%
  select(date, close) %>%
  filter(date > "2019-01-01") %>%
  arrange(desc(date))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #  construction and home improvement Products Companies
  # The Home Depot, Inc. (HD)

tq_get("HD", get = "stock.prices",from = "2019-01-01") %>%
  select(date, close) %>%
  arrange(desc(date))

  # Lowe's Companies, Inc. (LOW)

tq_get("LOW", get = "stock.prices",from = "2019-01-01") %>%
  select(date, close) %>%
  arrange(desc(date))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # software for  designing, executing, and operating Construction and Capital Projects

  # Bentley Systems, Incorporated (BSY)

tq_get("BSY", get = "stock.prices") %>%
  select(date, close) %>%
  filter(date > "2019-01-01") %>%
  arrange(desc(date))

  # Autodesk, Inc. (ADSK)

tq_get("ADSK", get = "stock.prices") %>%
  select(date, close) %>%
  filter(date > "2019-01-01") %>%
  arrange(desc(date))












































































