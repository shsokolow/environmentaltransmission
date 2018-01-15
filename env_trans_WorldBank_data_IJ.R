# Code to download and merge World Bank Indicator datasets
# Isabel Jones
# 2017.12.06

# requires.csv of country list with ISO-3 code

install.packages("WDI")
library(WDI)
library(dplyr)
library(tidyr)

# dowload .csv list of WHO GHE countries and ISO-3 codes
countries = read.csv("WHO_GHE_Country_List.csv")
# change column name "iso3_code" to "iso3c" to match GHE
colnames(countries)[2] = "iso3c"

# use WDIsearch() to find indicator of interest
# example:
# WDIsearch("GNI")
# define and download data of interest, extra = TRUE allows for inclusion of iso-3 code and other relevant information

# begin with area, "AG.LND.TOTL.K2" to get area, iso-3 code, and latitude for single year (2015)
area <- WDI(country = "all", indicator = "AG.LND.TOTL.K2", start = 2015, end = 2015, extra = TRUE)
area <- area[,(c("country","iso3c", "AG.LND.TOTL.K2","latitude","income"))]

# merge data based on ISO-3 code
data <- left_join(countries, area, by = "iso3c")

# check to make sure all countries retained
nrow(countries)
nrow(data)

# download other indicators of interest, for specific range of years
#GDP per capita, current $US
GDPpercapita <- WDI(country = "all", indicator = "NY.GDP.PCAP.CD", start = 2000, end = 2015, extra = TRUE)

# select columns and years of interest (2000,2005,2010,2015)
GDPpercapita <-  GDPpercapita %>% 
  select(c("iso3c","year","NY.GDP.PCAP.CD")) %>% 
  filter(year %in% c("2000","2005","2010","2015"))

# merge with 'data'
data <- left_join(data,GDPpercapita)

# repeat for all indicators of interest
GDPcurrentmillions <- WDI(country = "all", indicator = "NY.GDP.MKTP.CD", start = 2000, end = 2015, extra = TRUE)
GDPcurrentmillions <-  GDPcurrentmillions %>% 
  select(c("iso3c","year","NY.GDP.MKTP.CD")) %>% 
  filter(year %in% c("2000","2005","2010","2015"))
data <- left_join(data,GDPcurrentmillions)

GNIcurrent <- WDI(country = "all", indicator = "NY.GNP.MKTP.CD", start = 2000, end = 2015, extra = TRUE)
GNIcurrent <-  GNIcurrent %>% 
  select(c("iso3c","year","NY.GNP.MKTP.CD")) %>% 
  filter(year %in% c("2000","2005","2010","2015"))
data <- left_join(data,GNIcurrent)

# if necessary, download .csv, using `write.csv( )`
