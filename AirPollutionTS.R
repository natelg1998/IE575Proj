install.packages("astsa")
install.packages("changepoint")
library(forecast)
library(astsa)
library(tseries)
library(car)
library(caret)
library(changepoint)

pollutiondeaths <- read.csv("C:/Users/ntlg4/RFiles/IE575/Final Project/archive/death-rates-from-air-pollution.csv")
head(pollutiondeaths)
colSums(is.na(pollutiondeaths))
pollutiondeathsdf <- data.frame(pollutiondeaths)
names(pollutiondeathsdf) <- c("Country", "Code", "Year", "TotalAirPollutionDeath", "IndoorAirPollutionDeath",
                              "OutdoorAirPollutionDeath", "OzonePollutionDeath")

pollutiondeathsdf$Country <- as.character(pollutiondeathsdf$Country)
pollutiondeathsdf$Code <- as.character(pollutiondeathsdf$Code)

#Remove non-countries (those without a country code)
pollutiondeathsdf <- pollutiondeathsdf[!(pollutiondeathsdf$Code == ""),]
pollutiondeathsdf <- pollutiondeathsdf[!(pollutiondeathsdf$Code == "OWID_WRL"),]

pollutiondeathsdf$Country <- as.factor(pollutiondeathsdf$Country)
pollutiondeathsdf$Code <- as.factor(pollutiondeathsdf$Code)

unique(pollutiondeathsdf$Country)
str(pollutiondeathsdf)
colSums(is.na(pollutiondeathsdf))
###############################################################################
#EDA
par(mfrow = c(2,2))
overallDeathbyCountrydf <- aggregate(pollutiondeathsdf[,c(4)], by = list(pollutiondeathsdf$Year, pollutiondeathsdf$Country), sum, na.rm = T)
names(overallDeathbyCountrydf) <- c("Year", "Country", "TotalAirPollutionDeath")
overallDeathbyCountrydf
hist(overallDeathbyCountrydf$TotalAirPollutionDeath, main = "Overall Death By Air Pollution")

indoorDeathbyCountrydf <- aggregate(pollutiondeathsdf[,c(5)], by = list(pollutiondeathsdf$Year, pollutiondeathsdf$Country), sum, na.rm = T)
names(indoorDeathbyCountrydf) <- c("Year", "Country", "IndoorAirPollutionDeath")
indoorDeathbyCountrydf
hist(indoorDeathbyCountrydf$IndoorAirPollutionDeath, main = "Indoor Death By Air Pollution")

outdoorDeathbyCountrydf <- aggregate(pollutiondeathsdf[,c(6)], by = list(pollutiondeathsdf$Year, pollutiondeathsdf$Country), sum, na.rm = T)
names(outdoorDeathbyCountrydf) <- c("Year", "Country", "OutdoorAirPollutionDeath")
outdoorDeathbyCountrydf
hist(outdoorDeathbyCountrydf$OutdoorAirPollutionDeath, main = "Outdoor Death By Air Pollution")

ozoneDeathbyCountrydf <- aggregate(pollutiondeathsdf[,c(7)], by = list(pollutiondeathsdf$Year, pollutiondeathsdf$Country), sum, na.rm = T)
names(ozoneDeathbyCountrydf) <- c("Year", "Country", "OzoneAirPollutionDeath")
ozoneDeathbyCountrydf
hist(ozoneDeathbyCountrydf$OzoneAirPollutionDeath, main = "Ozone Death By Air Pollution")

######################################################################################################
#Create Time Series On Full Data
overallDeathdf <- aggregate(pollutiondeathsdf[,c(4)], by = list(pollutiondeathsdf$Year), sum, na.rm = T)
names(overallDeathdf) <- c("Year", "TotalAirPollutionDeath")

indoorDeathdf <- aggregate(pollutiondeathsdf[,c(5)], by = list(pollutiondeathsdf$Year), sum, na.rm = T)
names(indoorDeathdf) <- c("Year", "IndoorAirPollutionDeath")

outdoorDeathdf <- aggregate(pollutiondeathsdf[,c(6)], by = list(pollutiondeathsdf$Year), sum, na.rm = T)
names(outdoorDeathdf) <- c("Year", "OutdoorAirPollutionDeath")

ozoneDeathdf <- aggregate(pollutiondeathsdf[,c(7)], by = list(pollutiondeathsdf$Year), sum, na.rm = T)
names(ozoneDeathdf) <- c("Year", "OzoneAirPollutionDeath")

overalldeaths_ts <- ts(overallDeathdf$TotalAirPollutionDeath, start = min(overallDeathdf$Year), frequency = 1)
indoordeaths_ts <- ts(indoorDeathdf$IndoorAirPollutionDeath, start = min(indoorDeathdf$Year), frequency = 1)
outdoordeaths_ts <- ts(outdoorDeathdf$OutdoorAirPollutionDeath, start = min(outdoorDeathdf$Year), frequency = 1)
ozonedeaths_ts <- ts(ozoneDeathdf$OzoneAirPollutionDeath, start = min(ozoneDeathdf$Year), frequency = 1)

par(mfrow = c(2,2))
plot(overalldeaths_ts, main = "Overall Air Pollution Deaths Globally (Per 100,000)", xlab = "Year", ylab = "Number of Deaths (Per 100,000)")
plot(indoordeaths_ts, main = "Indoor Air Pollution Deaths Globally (Per 100,000)", xlab = "Year", ylab = "Number of Deaths (Per 100,000)")
plot(outdoordeaths_ts, main = "Outdoor Air Pollution Deaths Globally (Per 100,000)", xlab = "Year", ylab = "Number of Deaths (Per 100,000)")
plot(ozonedeaths_ts, main = "Ozone Air Pollution Deaths Globally (Per 100,000)", xlab = "Year", ylab = "Number of Deaths (Per 100,000)")

par(mfrow = c(1,1))
#Pre checks - we see we have annual data so no seasonality 
cycle(overalldeaths_ts)
cycle(indoordeaths_ts)
cycle(outdoordeaths_ts)
cycle(ozonedeaths_ts)

#Check for constant mean - fails
cpt.mean(overalldeaths_ts)
cpt.mean(indoordeaths_ts)
cpt.mean(outdoordeaths_ts)
cpt.mean(ozonedeaths_ts)


#Check for constant variance - passes
cpt.var(overalldeaths_ts)
cpt.var(indoordeaths_ts)
cpt.var(outdoordeaths_ts)
cpt.var(ozonedeaths_ts)

adf.test(overalldeaths_ts)
adf.test(indoordeaths_ts)
adf.test(outdoordeaths_ts)
adf.test(ozonedeaths_ts)



par(mfrow = c(1,1))
#All time series models fail Dickey-Fuller we need to stationarize our data
#1 Differencing
plot(diff(overalldeaths_ts, differences = 1), main = "Overall Deaths")
adf.test(diff(overalldeaths_ts, differences = 1))

plot(diff(indoordeaths_ts, differences = 1), main = "Indoor Deaths")
adf.test(diff(indoordeaths_ts, differences = 1))

plot(diff(outdoordeaths_ts, differences = 1), main = "Outdoor Deaths")
adf.test(diff(outdoordeaths_ts, differences = 1))
acf(diff(outdoordeaths_ts, differences = 1))
pacf(diff(outdoordeaths_ts, differences = 1))
acf2(diff(outdoordeaths_ts, differences = 1))
fitOutdoorDeaths <-  Arima(diff(outdoordeaths_ts, differences = 1), order = c(0,1,0), seasonal = F)
summary(fitOutdoorDeaths)

plot(diff(ozonedeaths_ts, differences = 1), main = "Ozone Deaths")
adf.test(diff(ozonedeaths_ts, differences = 1))

#Only outdoordeaths is passing differencing. Keep differencing
#Differencing
plot(diff(overalldeaths_ts, differences = 2), main = "Overall Deaths")
adf.test(diff(overalldeaths_ts, differences = 2))
acf(diff(overalldeaths_ts, differences = 2))
pacf(diff(overalldeaths_ts, differences = 2))
acf2(diff(overalldeaths_ts, differences = 2))
fitOverallDeath1 <- Arima(diff(overalldeaths_ts, differences = 2), order = c(0,2,0), seasonal = F)
fitOverallDeath1
summary(fitOverallDeath1)

plot(diff(indoordeaths_ts, differences = 2), main = "Indoor Deaths")
adf.test(diff(indoordeaths_ts, differences = 2))
acf(diff(indoordeaths_ts, differences = 2))
pacf(diff(indoordeaths_ts, differences = 2))
acf2(diff(indoordeaths_ts, differences = 2))
fitIndoorDeath1 <- Arima(diff(indoordeaths_ts, differences = 2), order = c(0,2,0), seasonal = F)
fitIndoorDeath1
summary(fitIndoorDeath1)

plot(diff(outdoordeaths_ts, differences = 2), main = "Outdoor Deaths")
adf.test(diff(outdoordeaths_ts, differences = 2))
acf(diff(outdoordeaths_ts, differences = 2))
pacf(diff(outdoordeaths_ts, differences = 2))
acf2(diff(outdoordeaths_ts, differences = 2))
fitoutdoorDeath1 <- Arima(diff(outdoordeaths_ts, differences = 2), order = c(0,2,1), seasonal = F)
fitoutdoorDeath1
summary(fitoutdoorDeath1)

plot(diff(ozonedeaths_ts, differences = 2), main = "Ozone Deaths")
adf.test(diff(ozonedeaths_ts, differences = 2))
acf(diff(ozonedeaths_ts, differences = 2))
pacf(diff(ozonedeaths_ts, differences = 2))
acf2(diff(ozonedeaths_ts, differences = 2))
fitOzoneDeath1 <- Arima(diff(ozonedeaths_ts, differences = 2), order = c(0,2,0), seasonal = F)
fitOzoneDeath1
summary(fitOzoneDeath1)

 
# gives us the best fitting ARIMA model with our data
#https://stackoverflow.com/questions/53099289/error-in-array-data-must-be-of-a-vector-type-was-null-in-r

fitOverallDeathTs <- auto.arima(overalldeaths_ts, seasonal = F) #ARIMA(1,2,0)
fitOverallDeathTs
summary(fitOverallDeathTs)
plot(fitOverallDeathTs)


fitIndoorDeathTs <- auto.arima(indoordeaths_ts, seasonal = F) #ARIMA(1,1,0)
fitIndoorDeathTs
summary(fitIndoorDeathTs)
plot(fitIndoorDeathTs)


fitOutdoorDeathTs <- auto.arima(outdoordeaths_ts, seasonal = F) #ARIMA(0,2,2)
fitOutdoorDeathTs
summary(fitOutdoorDeathTs)
plot(fitOutdoorDeathTs)

fitOzoneDeathTs <- auto.arima(ozonedeaths_ts, seasonal = F) #ARIMA(1,1,0)
fitOzoneDeathTs
summary(fitOzoneDeathTs)
plot(fitOzoneDeathTs)



######################################################
# Measuring fitness 
#Auto Arima Models
fitOverallDeathTs #AICc = 299.51 
fitIndoorDeathTs #AICc = 273.67
fitOutdoorDeathTs #AICc=305.38
fitOzoneDeathTs #AICc=126.76

## Take our best models and make predictions
forecast(fitOverallDeathTs, h= 10)
accuracy(forecast(fitOverallDeathTs, h= 10))

forecast(fitOutdoorDeathTs, h = 10)
accuracy(forecast(fitOverallDeathTs, h= 10))

forecast(fitIndoorDeathTs, h = 10)
accuracy(forecast(fitOverallDeathTs, h= 10))

forecast(fitOzoneDeathTs, h=10)
accuracy(forecast(fitOzoneDeathTs, h=10))



