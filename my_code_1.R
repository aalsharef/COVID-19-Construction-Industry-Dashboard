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
font_import()
loadfonts(device = "win")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

cat_stocks <- tq_get("CAT", get = "stock.prices")
view(cat_stocks)

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


















