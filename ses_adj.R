library(tidyverse)
library(seasonal)
library(lubridate)
car_sales <- read_csv("data/sales.csv")
car_sales$Month <- lubridate::mdy(car_sales$Month)
car_sales_ts <- ts(car_sales$Sales, 
                   start = c(year(car_sales$Month[1]),
                             month(car_sales$Month[1])), 
                   frequency = 12)

diwali_reg <- genhol(diwali, start = -29, end = 0, frequency = 12, center = "calendar")

m <- seas(car_sales_ts, 
          xreg = diwali_reg, 
          regression.aictest = NULL)
summary(m)
plot(m, trend = T)
monthplot(m)
pacf(resid(m))
spectrum(diff(resid(m)))
plot(density(resid(m)))
qqnorm(resid(m))
view(m)
m$list
plot(series(m, "series.adjoriginal"))
