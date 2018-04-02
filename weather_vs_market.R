#As part of my brushing up of R, in this exercise, I'd like to observe the relationship between
#weather in New York City and the daily return on the S&P 500 index.
#I had heard a claim in a behavioral economics discussion that a gloomy
#weather tends to depress the stock market sentiment.
#I am using S&P500 daily return and weather data from 2010-2017.
# AWND = Average daily wind speed (miles per hour)
# PRCP = Precipitation (inches)

setwd("/Users/ujjwal/Projects/Data/Weather")

#reading the weather data; source: https://www.ncdc.noaa.gov/
weather <- read.csv("NYC_weather_2010-2017.csv")

#reading S&P500 index data into spx; source: FRED (https://fred.stlouisfed.org/)
spx <- read.csv("SP500_2010_2017.csv")

#converting date column to date data type
weather$DATE <- as.Date(weather$DATE)
spx$DATE <- as.Date(spx$DATE)

#convert SP500 data to numeric; could also be done by setting (stringsAsFactors = False) in read.csv
spx$SP500 <- as.numeric(as.character(spx$SP500)) 

#limit spx data to days when market was open in 207
spx2010_2017 <- subset(spx, spx$SP500 != "." & format(spx$DATE, "%Y") <= 2017)

#plot of SPX index in 2017
plot(x = spx2010_2017$DATE, y = spx2010_2017$SP500, type = "l", main = "SPX index in 2017")

#limit observations to days in 2017 only; vlookup weather data ()
trainData <- merge(spx2010_2017[,c("DATE", "SP500")], weather[, c("DATE", "AWND", "PRCP", "SNOW")])

#plot of SPX index with AWND, PRCP in 2017
n <- nrow(spx2010_2017)
spxDailyRtn <- ((trainData$SP500[2:n] - trainData$SP500[1:(n-1)]))/trainData$SP500[1:(n-1)]
trainData$spxDailyRtn <- c(NA, spxDailyRtn)

#Running a regression model with SPX return as dependent variable and weather as independent variables
Model1 = lm(spxDailyRtn ~ AWND+PRCP, data = trainData)
summary(Model1)


#Summary: Obviously, the adjusted R-squared is quite low since weather is not
# a key determinant of market returns. But interestingly, the PRCP variable
# is somewhat significant at the 0.05 level with p-value of 0.0495.
