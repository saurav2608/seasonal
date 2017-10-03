library(tidyverse)
library(seasonal)
library(lubridate)
library(forecast)
library(ggthemes)
theme_set(theme_economist_white())
#India IIP data from below link (no API support)
#https://data.gov.in/catalog/nic-2-digit-level-and-sectoral-monthly-indices-all-india-index-industrial-production

# code 32 is white goods http://mospi.nic.in/sites/default/files/main_menu/national_industrial_classification/nic_2004_struc_2digit.pdf

iip <- read_csv("data/india_iip_05_13.csv")
month_M <- stringr::str_sub(iip$Month, 1, 3)
year_y <- stringr::str_sub(iip$Month, 6, -1)

iip$date <- lubridate::mdy(paste(month_M, "01", year_y, sep = "-"))

prod_ts <- ts(iip$`NIC 32`, 
                   start = c(year(iip$date[1]),
                             month(iip$date[1])), 
                   frequency = 12)
g <- ggplot(iip) +
  geom_line(aes(y = `NIC 32`, x = date)) +
  ggtitle("IIP Data - White Goods Production Index") +
  ylab("White Good Prod Index")
ggsave("output/ts.png", g, device = "png")

diwali_reg <- genhol(diwali, start = -60, end = 0, frequency = 12, center = "calendar")

m <- seas(prod_ts, 
          xreg  = diwali_reg, 
          regression.aictest = NULL,
          regression.usertype = "holiday",
          forecast.maxlead = 12)
summary(m)
plot(m, trend = T)
monthplot(m)
pacf(resid(m))
spectrum(diff(resid(m)))
plot(density(resid(m)))
qqnorm(resid(m))

output_df <- data.frame(date = iip$date, orig = as.numeric(original(m)), 
                        final = as.numeric(final(m)), 
                        trend = as.numeric(m$data[, "trend"]))
g1 <- ggplot(output_df, aes(x = date)) +
  geom_line(aes(y = orig, color = "blue")) +
  geom_line(aes(y = final, color = "red")) +
  geom_line(aes(y = trend, color = "gray"), linetype = 2) +
  ggtitle("Original and Seasonally Adjusted Series") +
  theme(legend.position = "none") +
  ylab("White Goods Index")
g1  
ggsave("output/ts1.png", g1, device = "png")
