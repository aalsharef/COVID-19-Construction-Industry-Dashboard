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

# Covid-19 data --------

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




















