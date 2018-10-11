#=========================================
##First submission for Nico (finmetrics)

# This is the script I used for the code
# I just copied the code over from my README
# See README for explanation of how I did my calculations

library(tidyverse)
library(rmsfuns)
library(lubridate)
Daily_TRI <- 
  read_rds("data/Fin_Data_SA_US_NKY.rds")

#Change data name
data <- Daily_TRI
#Change column names to make for easier reading
names(data) <- c("date", "ticker", "index", "name", "cap", "sector", "industry", "country")

monthlyreturns <- 
  data %>% 
  mutate(year_month = format(date, "%Y%B")) %>% 
  group_by(year_month, ticker) %>% 
  
  # Now filter to just have the last date of each month, as we are essentially calculating 
  #"monthly" returns
  filter(date == last(date)) %>%
  ungroup() %>% 
  
  #Calculate the monthly returns by ticker
  group_by(ticker) %>% 
  mutate(monthly_returns = index/lag(index) - 1) %>% 
  mutate(monthly_returns = coalesce(monthly_returns, 0)) %>% 
  ungroup() %>% 
  
  # Next we want to add up the market cap by sector
  group_by(country, sector, date) %>%  #this makes sense - understand the order of grouping!
  mutate(sector_cap=sum(cap)) %>% 
  
  # Calculate the weight
  mutate(weight= cap/sector_cap) %>% #delete this?
  
  # Calculate the weighted indices by multiplying the TRI (index) by the weight above
  mutate(weighted_index_return = sum(monthly_returns*weight)) %>% #each ticker (company) then has a weighted index
  summarise(weighted_monthly_returns = mean(weighted_index_return))

#Sharpe ratio
Sharpe_ratios <- 
  monthlyreturns %>% 
  mutate(Sharpe = mean(weighted_monthly_returns, na.rm = T)/sd(weighted_monthly_returns, na.rm = T)) %>% 
  print(Sharpe)

#Cumulative returns
cumreturns_financials <- 
  monthlyreturns %>% 
  filter(sector %in% c("Financials")) %>% 
  group_by(country) %>% 
  mutate(monthly_cumreturns = cumprod( 1 + weighted_monthly_returns)) %>% 
  group_by(date, country) %>% 
  ungroup()

library("ggplot2")

#cumreturns_financials %>% 
# group_by(date, country) %>% 
#print(cumreturns_financials)
plot_cumreturns_financials <- 
  cumreturns_financials %>% 
  ggplot() +
  geom_line(aes(x=date, y=monthly_cumreturns, colour = country)) +
  guides(colour = T, size = F) 

#Now we do the rolling standard deviation
library("PerformanceAnalytics")
library("xts", "zoo", "tbl_xts")
library("rmsfuns")
# Now, we go ahead with the coding
xts_dailyreturns_materials <- 
  data %>%
  group_by(ticker) %>% 
  filter(sector %in% c("Materials")) %>%
  mutate(daily_returns = index/lag(index) -1) %>%
  mutate(daily_returns = coalesce(daily_returns, 0)) %>% 
  ungroup() %>%
  group_by(country, date) %>%
  summarise(mean_daily_returns = mean(daily_returns)) %>% 
  ungroup() %>% 
  tbl2xts::tbl_xts(.,cols_to_xts = "mean_daily_returns", spread_by = "country")

# Get the output
chart.RollingPerformance(xts_dailyreturns_materials, FUN = "sd", width = 4, main = "Rolling 60 day sd of the materials sector", legend.loc = "top middle")

