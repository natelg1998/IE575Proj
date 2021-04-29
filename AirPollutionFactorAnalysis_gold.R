#What I have done so far is format our data into a cleaner format for you to perform whatever 
#analysis you see fit (Lin Regression, MLR, Logistic Regression, classification, etc)
library(xlsx)
library(dplyr)
library(tidyr)
library(gapminder)
wdidata <- read.xlsx("C:/Users/ntlg4/RFiles/IE575/Final Project/archive/WDI1990_2017_2.xlsx", sheetName = "Data")
wdidatadf <- data.frame(wdidata)

wdidatadf <- dplyr::select(wdidatadf, -c(4)) # Remove Series Code - unnecessary 
str(wdidatadf)


names(wdidatadf) <- c("Country", "Code", "Series", "1990", "1991", "1992", "1993", "1994", "1995", "1996", "1997", "1998",
                        "1999","2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008",
                        "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017")

#https://www.r-bloggers.com/2019/10/data-pivoting-with-tidyr/ -explanation of what I did here

pivot1 <- pivot_longer(wdidatadf, cols = c('1990','1991', '1992', '1993', '1994','1995', '1996', '1997', '1998', '1999', '2000','2001','2002','2003','2004', '2005', '2006', '2007', '2008', '2009','2010', '2011', '2012','2013', '2014', '2015', '2016', '2017'), names_to = "year")
pivot1$value <- as.character(pivot1$value)
pivot1$value <- as.numeric(pivot1$value)
typeof(pivot1$value)

pivot2 <- pivot1 %>%
  pivot_wider(
    names_from = Series,
    values_from = value
  )

wdicleandatdf <- data.frame(pivot2)
names(wdicleandatdf) <- c("Country", "Code","Year","CO2", "GDP","GDPPerCap","MortalityRateInfant","NetMigration",
                          "PopTotal","PopDens","LandArea","PM2.5","MethaneEmis","NOEmis","SF6Emis","NA")


pollutiondeaths <- read.csv("C:/Users/ntlg4/RFiles/IE575/Final Project/archive/death-rates-from-air-pollution.csv")
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

wdidataclean_df <- merge(wdicleandatdf, pollutiondeathsdf, by = c("Code", "Year"))

wdidataclean_df <- wdidataclean_df[-16] #Drop random NA column

#Rename Country Column and drop duplicate column
colnames(wdidataclean_df)[colnames(wdidataclean_df) == "Country.x"] <- "Country"
wdidataclean_df <- subset(wdidataclean_df, select = c(-Country.y))

#Reorder
wdidataclean_df <- wdidataclean_df[,c(3,1,2,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19)]

##############################3
#Tasks
#1. Clean the data frame - numerous NAs 
#2. Normalize/Scale the Data
#3. Perform your analysis

library(e1071)
colSums(is.na(wdidataclean_df))

countNAPercent <- function(x){
  NAPercent <- sum(is.na(x))/length(x)
  return(NAPercent)
}


#Calc missing values percent
apply(wdidataclean_df, 2, countNAPercent)


wdidataPreProcessdf <- wdidataclean_df

tonumeric <- c("CO2", "GDP","GDPPerCap","MortalityRateInfant","NetMigration",
            "PopTotal","PopDens","LandArea","PM2.5","MethaneEmis","NOEmis","SF6Emis")
for(i in tonumeric){
  wdidataPreProcessdf[,i] <- unlist(wdidataPreProcessdf[,i], recursive = T, use.names = T)
}
str(wdidataPreProcessdf)

#We need to do some EDA. IF you plan to a regression analysis you will need to normalize your data

#Skewness 
apply(wdidataPreProcessdf[,4:19],2, function(x) e1071::skewness(x, na.rm = T, type =2))
#Kurtosis Values
apply(wdidataPreProcessdf[,4:19],2, function(x) e1071::kurtosis(x, na.rm = T, type =2))

apply(wdidataPreProcessdf[,c(4:19)], 2, function(x) mean(x,na.rm=T))

apply(wdidataPreProcessdf[,c(4:19)], 2, function(x) sd(x,na.rm=T))

boxplot(wdidataPreProcessdf$CO2, main =  "CO2 Emissions")
boxplot(wdidataPreProcessdf$GDP, main = "GDP ($US)")
boxplot(wdidataPreProcessdf$GDPPerCap, main = "GDP Per Capita")
boxplot(wdidataPreProcessdf$MortalityRateInfant, main = "Infant Mortality Rate)")
boxplot(wdidataPreProcessdf$NetMigration, main = "Net Migration")
boxplot(wdidataPreProcessdf$PopTotal, main = "Population Total")
boxplot(wdidataPreProcessdf$PopDens, main = "Population Density")
boxplot(wdidataPreProcessdf$LandArea)
boxplot(wdidataPreProcessdf$PM2.5)
boxplot(wdidataPreProcessdf$MethaneEmis)
boxplot(wdidataPreProcessdf$NOEmis)
boxplot(wdidataPreProcessdf$SF6Emis)
boxplot(wdidataPreProcessdf$TotalAirPollutionDeath)
boxplot(wdidataPreProcessdf$IndoorAirPollutionDeath)
boxplot(wdidataPreProcessdf$OutdoorAirPollutionDeath)
boxplot(wdidataPreProcessdf$OzonePollutionDeath)

par(mfrow = c(2,4))
hist(wdidataPreProcessdf$CO2, main =  "CO2 Emissions")
hist(wdidataPreProcessdf$GDP, main = "GDP ($US)")
hist(wdidataPreProcessdf$GDPPerCap, main = "GDP Per Capita")
hist(wdidataPreProcessdf$MortalityRateInfant, main = "Infant Mortality Rate)")
hist(wdidataPreProcessdf$NetMigration, main = "Net Migration")
hist(wdidataPreProcessdf$PopTotal, main = "Population Total")
hist(wdidataPreProcessdf$PopDens, main = "Population Density")
hist(wdidataPreProcessdf$LandArea, main = "LandArea")
hist(wdidataPreProcessdf$PM2.5, main = "PM2.5")
hist(wdidataPreProcessdf$MethaneEmis, main = "Methane")
hist(wdidataPreProcessdf$NOEmis, main = "NOEmis")
hist(wdidataPreProcessdf$SF6Emis, main = "SF6")
hist(wdidataPreProcessdf$TotalAirPollutionDeath, main = "TotalAirPollution")
hist(wdidataPreProcessdf$IndoorAirPollutionDeath, main ="IndoorAirPollution")
hist(wdidataPreProcessdf$OutdoorAirPollutionDeath, main = "OutdoorAirPollution")
hist(wdidataPreProcessdf$OzonePollutionDeath, main = "OzoneAirPollution")

par(mfrow = c(1,1))
library(LaplacesDemon)

apply(wdidataPreProcessdf[,c(4:19)],2,is.multimodal)


library(caret)
library(corrplot)
wdidataPreProcess.sc <- wdidataPreProcessdf
wdidataPreProcess.sc[,c(4:19)] <- scale(wdidataPreProcessdf[,c(4:19)])
summary(wdidataPreProcess.sc)
corrplot(cor(wdidataPreProcess.sc[,c(4:19)], use = "pairwise.complete.obs"), 
         method = "circle",
         type = "full",
         order ="original",
         bg = "white",
         tl.col = "black",
         tl.cex = 1)
cor(wdidataPreProcess.sc[,c(4:19)], use = "pairwise.complete.obs")


########

apply(wdidataPreProcessdf, 2, countNAPercent)

#Drop Columns with 50% or more NA
wdidataPreProcessdf <- subset(wdidataPreProcessdf, select = c(-NetMigration,-PM2.5, -SF6Emis))

#Handle Outliers
changeNA<-function(df){
  df %>% mutate_if(is.numeric, ~replace(., . %in% boxplot.stats(.)$out, NA))
}

wdiremoveOutlier <- changeNA(wdidataPreProcessdf)

#Calc missing values percent
apply(wdiremoveOutlier, 2, countNAPercent)  

wdidataPreProcess2df <- wdiremoveOutlier

#I wouldn't do an aggregated mean because you are trying to see the relationship over the 28 year period
#and it is tough to identify a relationship doing it this way. you want as many data points as possible 

# summarizedata <- wdidataPreProcess2df %>% 
#   group_by(Country) %>%
# summarize_at(c('CO2','GDP','GDPPerCap','MortalityRateInfant','PopTotal','PopDens',
#                  'LandArea','MethaneEmis','NOEmis','TotalAirPollutionDeath'), mean, na.rm = TRUE)



#impute missing values - warning this will take a while
#gave up with mice it took forever 
#reason why i used cart 
# #https://stackoverflow.com/questions/48355250/do-imputation-in-r-when-mice-returns-error-that-system-is-computationally-singu
# library(mice)
# set.seed(43256)
# mice_imputes <- mice(wdidataPreProcess2df, m =5, maxit = 40, method = "cart")
# mice_imputes$method
# mice_imputes$imp
# mice_imputes$nmis
# summary(mice_imputes)
# imputed_data <- complete(mice_imputes,5)
# summary(imputed_data)
# wditransformed1 <- imputed_data

library(missForest)
set.seed(446578)
meanmat<-data.matrix(wdidataPreProcess2df)
meanmat_imp<-missForest(meanmat)
wditransformed1df<-data.frame(meanmat_imp$ximp)

view(wditransformed1df)

par(mfrow = c(2,5))
hist(wditransformed1df$CO2)
hist(wditransformed1df$GDP)
hist(wditransformed1df$GDPPerCap)
hist(wditransformed1df$MortalityRateInfant)
hist(wditransformed1df$PopTotal)
hist(wditransformed1df$PopDens)
hist(wditransformed1df$PopDens)
hist(wditransformed1df$LandArea)
hist(wditransformed1df$MethaneEmis)
hist(wditransformed1df$NOEmis)
hist(wditransformed1df$TotalAirPollutionDeath)
hist(wditransformed1df$IndoorAirPollutionDeath)
hist(wditransformed1df$OutdoorAirPollutionDeath)
hist(wditransformed1df$OutdoorAirPollutionDeath)

apply(wditransformed1df[,4:16],2, function(x) e1071::skewness(x, na.rm = T, type =2))

#apply(wditransformed1df,2,shapiro.test)
#https://www.tutorialspoint.com/how-to-deal-with-error-error-in-shapiro-test-sample-size-must-be-between-3-and-5000-in-r
#https://www.isixsigma.com/dictionary/anderson-darling-normality-test/
library(nortest)
apply(wditransformed1df,2,ad.test)

par(mfrow = c(1,1))
#Transformations using Box Cox
#Ignore Country, Code, Year, TotalAirPollution, IndoorAirPollution, OutdoorAirPollution, OzoneAirPollution
apply(wditransformed1df, 2, BoxCoxTrans)

CO2BoxCox <- ((wditransformed1df$CO2^0.2 -1)/0.2)
hist(CO2BoxCox, main = "CO2BoxCox")
ad.test(CO2BoxCox)
e1071::skewness(CO2BoxCox,type =2)

GDPBoxCox <- (log(wditransformed1df$GDP))
hist(GDPBoxCox, main = "GDPBoxCox")
ad.test(GDPBoxCox)
e1071::skewness(GDPBoxCox,type =2)

GDPPerCapBoxCox <- (log(wditransformed1df$GDPPerCap))
hist(GDPPerCapBoxCox, main = "GDPPerCapBoxCox")
ad.test(GDPPerCapBoxCox)
e1071::skewness(GDPPerCapBoxCox,type =2)

MortalityBoxCox <- (log(wditransformed1df$MortalityRateInfant))
hist(MortalityBoxCox, main = "MortalityInfantBoxCox")
ad.test(MortalityBoxCox)
e1071::skewness(MortalityBoxCox,type =2)


PopTotalBoxCox <- ((wditransformed1df$PopTotal^0.2 -1)/0.2)
hist(PopTotalBoxCox, main = "PopTotalBoxCox")
ad.test(PopTotalBoxCox)
e1071::skewness(PopTotalBoxCox,type =2)

PopDensBoxCox <- ((wditransformed1df$PopDens^0.3 -1)/0.2)
hist(PopDensBoxCox, main = "PopDensBoxCox")
ad.test(PopDensBoxCox)
e1071::skewness(PopDensBoxCox,type =2)

LandAreaBoxCox <- ((wditransformed1df$LandArea^0.2 -1)/0.2)
hist(LandAreaBoxCox, main = "LandAreaBoxCox")
ad.test(LandAreaBoxCox)
e1071::skewness(LandAreaBoxCox,type =2)

library(car)
#PowerTransform
powerTransform(wditransformed1df$CO2)
coef(powerTransform((wditransformed1df$CO2)))
powerCO2 <- bcPower(wditransformed1df$CO2, coef(powerTransform((wditransformed1df$CO2))))
hist(powerCO2)
ad.test(powerCO2)
e1071::skewness(powerCO2, type =2)

powerTransform(wditransformed1df$GDP)
coef(powerTransform((wditransformed1df$GDP)))
powerGDP <- bcPower(wditransformed1df$GDP, coef(powerTransform((wditransformed1df$GDP))))
hist(powerGDP)
ad.test(powerGDP)
e1071::skewness(powerGDP, type =2)

powerTransform(wditransformed1df$GDPPerCap)
coef(powerTransform((wditransformed1df$GDPPerCap)))
powerGDPPerCap <- bcPower(wditransformed1df$GDPPerCap, coef(powerTransform((wditransformed1df$GDPPerCap))))
hist(powerGDPPerCap)
ad.test(powerGDPPerCap)
e1071::skewness(powerGDPPerCap, type =2)

powerTransform(wditransformed1df$MortalityRateInfant)
coef(powerTransform((wditransformed1df$MortalityRateInfant)))
powerMortalityRate <- bcPower(wditransformed1df$MortalityRateInfant, coef(powerTransform((wditransformed1df$MortalityRateInfant))))
hist(powerMortalityRate)
ad.test(powerMortalityRate)
e1071::skewness(powerMortalityRate, type =2)

powerTransform(wditransformed1df$PopTotal)
coef(powerTransform((wditransformed1df$PopTotal)))
powerPopTotal <- bcPower(wditransformed1df$PopTotal, coef(powerTransform((wditransformed1df$PopTotal))))
hist(powerPopTotal)
ad.test(powerPopTotal)
e1071::skewness(powerPopTotal, type =2)

powerTransform(wditransformed1df$PopDens)
coef(powerTransform((wditransformed1df$PopDens)))
powerPopDens <- bcPower(wditransformed1df$PopDens, coef(powerTransform((wditransformed1df$PopDens))))
hist(powerPopDens)
ad.test(powerPopDens)
e1071::skewness(powerPopDens, type =2)

powerTransform(wditransformed1df$LandArea)
coef(powerTransform((wditransformed1df$LandArea)))
powerLandArea <- bcPower(wditransformed1df$LandArea, coef(powerTransform((wditransformed1df$LandArea))))
hist(powerLandArea)
ad.test(powerLandArea)
e1071::skewness(powerLandArea, type =2)

#Has nonpositive values need to use Yeo-Johnson
powerTransform(wditransformed1df$MethaneEmis)
coef(powerTransform((wditransformed1df$MethaneEmis)))
powerMethaneEmis <- bcPower(wditransformed1df$MethaneEmis, coef(powerTransform((wditransformed1df$MethaneEmis))))
hist(powerMethaneEmis)
ad.test(powerMethaneEmis)
e1071::skewness(powerMethaneEmis, type =2)

#Has nonpositive values need to use Yeo-Johnson
powerTransform(wditransformed1df$NOEmis)
coef(powerTransform((wditransformed1df$NOEmis)))
powerNOEmis <- bcPower(wditransformed1df$NOEmis, coef(powerTransform((wditransformed1df$NOEmis))))
hist(powerNOEmis)
ad.test(powerNOEmis)
e1071::skewness(powerNOEmis, type =2)

#Yeo-Johnson
powerTransform(wditransformed1df$MethaneEmis, family ="yjPower")
coef(powerTransform((wditransformed1df$MethaneEmis), family = "yjPower"))
powerMethaneEmis <- yjPower(wditransformed1df$MethaneEmis, coef(powerTransform((wditransformed1df$MethaneEmis), family ="yjPower")))
hist(powerMethaneEmis)
ad.test(powerMethaneEmis)
e1071::skewness(powerMethaneEmis, type =2)

powerTransform(wditransformed1df$NOEmis, family = "yjPower")
coef(powerTransform((wditransformed1df$NOEmis), family = "yjPower"))
powerNOEmis <- yjPower(wditransformed1df$NOEmis, coef(powerTransform((wditransformed1df$NOEmis), family = "yjPower")))
hist(powerNOEmis)
ad.test(powerNOEmis)
e1071::skewness(powerNOEmis, type =2)


#########################
#Build linear models with best transformed values from the Power transform
totalAirPollutionModelData <- data.frame("TotalAirPollution" = wditransformed1df$TotalAirPollutionDeath, "CO2" = powerCO2,
                                         "GDP($US)" = powerGDP, "GDPPerCap" = powerGDPPerCap, "InfantMortalityRate" = powerMortalityRate,
                                         "PopTotal" = powerPopTotal, "PopDens" = powerPopDens, "LandArea" = powerLandArea, "MethanEmis" = powerMethaneEmis,
                                         "NoEmis" = powerNOEmis)


set.seed(448673)
totalAirPollution_sampling  <- createDataPartition(totalAirPollutionModelData$TotalAirPollution, p = 0.8, list = F)
total_train <- totalAirPollutionModelData[totalAirPollution_sampling,]
total_test <- totalAirPollutionModelData[-totalAirPollution_sampling,]

totalMod <- lm(TotalAirPollution ~., data = total_train)
summary(totalMod)
lmtest::dwtest(totalMod)
mean(totalMod$residuals)
vif(totalMod)
par(mfrow =c(2,2))
plot(totalMod)

#So we are seeing very high multicollinearity with GDP with a very high VIF measure. VIF should be less than 4
#GDP also has highest p value. I am going to use the MASS library with StepAIC to get our best fit model fast

library(MASS)
stepTotalMod <- stepAIC(totalMod, direction ="backward", trace = T)

library(caret)
set.seed(1235)
trainTotalMod <-trainControl(method = "cv", number = 10)
train_mod <- train(TotalAirPollution ~ ., data = total_train,
                   method = "leapBackward",
                   tuneGrid = data.frame(nvmax = 1:10),
                   trControl = trainTotalMod)
train_mod$results
summary(train_mod$finalModel)


#Training says best model is all variables but we fail to meet the assumptions of linear regression
trainadjustMod <- lm(TotalAirPollution ~ ., data = total_train)
summary(trainadjustMod)
lmtest::dwtest(trainadjustMod)
mean(trainadjustMod$residuals)
vif(trainadjustMod)
par(mfrow =c(2,2))
plot(trainadjustMod)

pred1 <- predict(trainadjustMod, newdata = total_test)
rmse <- ModelMetrics::rmse(total_test$TotalAirPollution, pred1)
c(RMSE=rmse, R2=summary(trainadjustMod)$r.squared)
par(mfrow =c(1,1))
plot(total_test$TotalAirPollution, pred1)


#Remove GDP
trainadjustMod <- lm(TotalAirPollution ~ . -GDP..US., data = total_train)
summary(trainadjustMod)
lmtest::dwtest(trainadjustMod)
mean(trainadjustMod$residuals)
vif(trainadjustMod)
par(mfrow =c(2,2))
plot(trainadjustMod)

pred1 <- predict(trainadjustMod, newdata = total_test)
rmse <- ModelMetrics::rmse(total_test$TotalAirPollution, pred1)
c(RMSE=rmse, R2=summary(trainadjustMod)$r.squared)
par(mfrow =c(1,1))
plot(total_test$TotalAirPollution, pred1)

#Remove GDP and PopTotal review summary(train_mod$finalModel) for plucking
trainadjustMod <- lm(TotalAirPollution ~ . -GDP..US. -PopTotal, data = total_train)
summary(trainadjustMod)
lmtest::dwtest(trainadjustMod)
mean(trainadjustMod$residuals)
vif(trainadjustMod)
par(mfrow =c(2,2))
plot(trainadjustMod)

pred1 <- predict(trainadjustMod, newdata = total_test)
rmse <- ModelMetrics::rmse(total_test$TotalAirPollution, pred1)
c(RMSE=rmse, R2=summary(trainadjustMod)$r.squared)
par(mfrow =c(1,1))
plot(total_test$TotalAirPollution, pred1)

#Remove GDP, PopTotal, NOEmis review summary(train_mod$finalModel) for plucking
trainadjustMod <- lm(TotalAirPollution ~ . -GDP..US. -PopTotal -NoEmis, data = total_train)
summary(trainadjustMod)
lmtest::dwtest(trainadjustMod)
mean(trainadjustMod$residuals)
vif(trainadjustMod)
par(mfrow =c(2,2))
plot(trainadjustMod)

pred1 <- predict(trainadjustMod, newdata = total_test)
rmse <- ModelMetrics::rmse(total_test$TotalAirPollution, pred1)
c(RMSE=rmse, R2=summary(trainadjustMod)$r.squared)
par(mfrow =c(1,1))
plot(total_test$TotalAirPollution, pred1)

#Remove GDP, PopTotal, NOEmis, MethanEmis review summary(train_mod$finalModel) for plucking
trainadjustMod <- lm(TotalAirPollution ~ . -GDP..US. -PopTotal -NoEmis -MethanEmis, data = total_train)
summary(trainadjustMod)
lmtest::dwtest(trainadjustMod)
mean(trainadjustMod$residuals)
vif(trainadjustMod)
par(mfrow =c(2,2))
plot(trainadjustMod)

pred1 <- predict(trainadjustMod, newdata = total_test)
rmse <- ModelMetrics::rmse(total_test$TotalAirPollution, pred1)
c(RMSE=rmse, R2=summary(trainadjustMod)$r.squared)
par(mfrow =c(1,1))
plot(total_test$TotalAirPollution, pred1)

#Remove GDP, PopTotal, NOEmis, MethanEmis, CO2 review summary(train_mod$finalModel) for plucking
trainadjustMod <- lm(TotalAirPollution ~ . -GDP..US. -PopTotal -NoEmis -MethanEmis -CO2, data = total_train)
summary(trainadjustMod)
lmtest::dwtest(trainadjustMod)
mean(trainadjustMod$residuals)
vif(trainadjustMod)
par(mfrow =c(2,2))
plot(trainadjustMod)

pred1 <- predict(trainadjustMod, newdata = total_test)
rmse <- ModelMetrics::rmse(total_test$TotalAirPollution, pred1)
c(RMSE=rmse, R2=summary(trainadjustMod)$r.squared)
par(mfrow =c(1,1))
plot(total_test$TotalAirPollution, pred1)

#Remove GDP, PopTotal, NOEmis, MethanEmis, CO2, PopDens review summary(train_mod$finalModel) for plucking
trainadjustMod <- lm(TotalAirPollution ~ . -GDP..US. -PopTotal -NoEmis -MethanEmis -CO2 -PopDens, data = total_train)
summary(trainadjustMod)
lmtest::dwtest(trainadjustMod)
mean(trainadjustMod$residuals)
vif(trainadjustMod)
par(mfrow =c(2,2))
plot(trainadjustMod)

pred1 <- predict(trainadjustMod, newdata = total_test)
rmse <- ModelMetrics::rmse(total_test$TotalAirPollution, pred1)
c(RMSE=rmse, R2=summary(trainadjustMod)$r.squared)
par(mfrow =c(1,1))
plot(total_test$TotalAirPollution, pred1)

#####################################################################################################33
#Algorithm ideas were not giving models that fit all assumptions of regression
#Remove by VIF values

totalMod <- lm(TotalAirPollution ~., data = total_train)
summary(totalMod)
lmtest::dwtest(totalMod)
mean(totalMod$residuals)
vif(totalMod)
par(mfrow =c(2,2))
plot(totalMod)

pred2 <- predict(totalMod, newdata = total_test)
rmse <- ModelMetrics::rmse(total_test$TotalAirPollution, pred2)
c(RMSE=rmse, R2=summary(totalMod)$r.squared)
par(mfrow =c(1,1))
plot(total_test$TotalAirPollution, pred2)


#Remove Poptotal highest VIF

totalMod <- lm(TotalAirPollution ~. -PopTotal, data = total_train)
summary(totalMod)
lmtest::dwtest(totalMod)
mean(totalMod$residuals)
vif(totalMod)
par(mfrow =c(2,2))
plot(totalMod)

pred2 <- predict(totalMod, newdata = total_test)
rmse <- ModelMetrics::rmse(total_test$TotalAirPollution, pred2)
c(RMSE=rmse, R2=summary(totalMod)$r.squared)
par(mfrow =c(1,1))
plot(total_test$TotalAirPollution, pred2)


#Remove PopTotal, LandArea highest VIF
totalMod <- lm(TotalAirPollution ~. -PopTotal -LandArea, data = total_train)
summary(totalMod)
lmtest::dwtest(totalMod)
mean(totalMod$residuals)
vif(totalMod)
par(mfrow =c(2,2))
plot(totalMod)

pred2 <- predict(totalMod, newdata = total_test)
rmse <- ModelMetrics::rmse(total_test$TotalAirPollution, pred2)
c(RMSE=rmse, R2=summary(totalMod)$r.squared)
par(mfrow =c(1,1))
plot(total_test$TotalAirPollution, pred2)

#Remove PopTotal, LandArea, GDP highest VIF
totalMod <- lm(TotalAirPollution ~. -PopTotal -LandArea -GDP..US., data = total_train)
summary(totalMod)
lmtest::dwtest(totalMod)
mean(totalMod$residuals)
vif(totalMod)
par(mfrow =c(2,2))
plot(totalMod)

pred2 <- predict(totalMod, newdata = total_test)
rmse <- ModelMetrics::rmse(total_test$TotalAirPollution, pred2)
c(RMSE=rmse, R2=summary(totalMod)$r.squared)
par(mfrow =c(1,1))
plot(total_test$TotalAirPollution, pred2)

#Remove PopTotal, LandArea, GDP, GDPPerCap highest VIF
totalMod <- lm(TotalAirPollution ~. -PopTotal -LandArea -GDP..US. -GDPPerCap, data = total_train)
summary(totalMod)
lmtest::dwtest(totalMod)
mean(totalMod$residuals)
vif(totalMod)
par(mfrow =c(2,2))
plot(totalMod)

pred2 <- predict(totalMod, newdata = total_test)
rmse <- ModelMetrics::rmse(total_test$TotalAirPollution, pred2)
c(RMSE=rmse, R2=summary(totalMod)$r.squared)
par(mfrow =c(1,1))
plot(total_test$TotalAirPollution, pred2)

###########################3
#Base Step

backwardstep <- step(totalMod, direction = "backward", scope = formula(totalMod), trace =0)
backwardstep$anova
backwardstep$coefficients

totalMod <- lm(TotalAirPollution ~., data = total_train)
summary(totalMod)
lmtest::dwtest(totalMod)
mean(totalMod$residuals)
vif(totalMod)
par(mfrow =c(2,2))
plot(totalMod)

pred2 <- predict(totalMod, newdata = total_test)
rmse <- ModelMetrics::rmse(total_test$TotalAirPollution, pred2)
c(RMSE=rmse, R2=summary(totalMod)$r.squared)
par(mfrow =c(1,1))
plot(total_test$TotalAirPollution, pred2)





#Best model appears to be the base model

# final model
finalTotalMod <- lm(TotalAirPollution ~ ., data = totalAirPollutionModelData)
summary(finalTotalMod)
lmtest::dwtest(finalTotalMod)
mean(finalTotalMod$residuals)
vif(finalTotalMod)
par(mfrow =c(2,2))
plot(finalTotalMod)

#**********************************************************************
#Transform skewness using log10, if skewness is between -0.5 and 0.5, fairly symmetrical
#Transform for certain features using sqrt()
#names(totalAirPollutionModelData)
names(wditransformed1df)

skewTrans<-wditransformed1df[,c(-1,-2,-3,-14,-15,-16)]
names(skewTrans)
skewness(skewTrans$TotalAirPollution)#0.74
skewTrans$TotalAirPollution<-log10(skewTrans$TotalAirPollution)
skewness(skewTrans$TotalAirPollution)#-0.36

skewness(skewTrans$CO2)#-0.90 
skewTrans$CO2<-log10(skewTrans$CO2)
skewness(skewTrans$CO2)#-0.65


skewness(skewTrans$GDP)#1.23 
skewTrans$GDP<-log10(skewTrans$GDP)
skewness(skewTrans$GDP)#-0.32
hist(skewTrans$GDP)

skewness(skewTrans$GDPPerCap)#1.15 
skewTrans$GDPPerCap<-log10(skewTrans$GDPPerCap)
skewness(skewTrans$GDPPerCap)#-0.09

skewness(skewTrans$MortalityRateInfant)#1.04
skewTrans$MortalityRateInfant<-log10(skewTrans$MortalityRateInfant)
skewness(skewTrans$MortalityRateInfant)#-0.238

skewness(skewTrans$PopTotal)#1.08
skewTrans$PopTotal<-log10(skewTrans$PopTotal)
skewness(skewTrans$PopTotal)#-0.81

skewness(skewTrans$PopTotal)#1.08
skewTrans$PopTotal<-log10(skewTrans$PopTotal)
skewness(skewTrans$PopTotal)#-0.81

#skewTrans$PopDens<-wditransformed1df$PopDens
skewness(skewTrans$PopDens)#.97
skewTrans$PopDens<-sqrt(skewTrans$PopDens)#Using sqrt instead since the log10 increased skewness
skewness(skewTrans$PopDens)#-0.81

#skewTrans$LandArea<-wditransformed1df$LandArea
skewness(skewTrans$LandArea)#1.16
skewTrans$LandArea<-sqrt(skewTrans$LandArea)
skewness(skewTrans$LandArea)#0.397

#skewTrans$MethaneEmis<-wditransformed1df$MethaneEmis
skewness(skewTrans$MethaneEmis)#1.06
skewTrans$MethaneEmis<-sqrt(skewTrans$MethaneEmis)
skewness(skewTrans$MethaneEmis)#0.38

#skewTrans$NOEmis<-wditransformed1df$NOEmis
skewness(skewTrans$NOEmis)#1.17
skewTrans$NOEmis<-sqrt(skewTrans$NOEmis)
skewness(skewTrans$NOEmis)#0.398

names(skewTrans)
view(skewTrans)
skewTrans<-skewTrans[,c(-10)]

#See density distribution
#referenced https://www.datanovia.com/en/lessons/transform-data-to-normal-distribution-in-r/> 
install.packages("ggpubr")
library(ggpubr)
ggdensity(skewTrans, x = "PopDens", fill = "lightgray", title = "PopDens") + stat_overlay_normal_density(color = "red", linetype = "dashed") 
ggdensity(wditransformed1df, x = "PopDens", fill = "lightgray", title = "PopDens") + stat_overlay_normal_density(color = "red", linetype = "dashed")

ggdensity(skewTrans, x = "CO2", fill = "lightgray", title = "CO2") + stat_overlay_normal_density(color = "red", linetype = "dashed") 
ggdensity(wditransformed1df, x = "CO2", fill = "lightgray", title = "PopDens") + stat_overlay_normal_density(color = "red", linetype = "dashed") 

hist(skewTrans$CO2)
hist(skewTrans$PopDens)

#Eventhough the density looks normalized, the histograms say otherwise.

############################################
#Repeat the model with newly transformed data

set.seed(448673)
data_sampling  <- createDataPartition(skewTrans$TotalAirPollution, p = 0.8, list = F)
data_train <- skewTrans[data_sampling,]
data_test <- skewTrans[-data_sampling,]

dataMod <- lm(TotalAirPollution ~., data = data_train)
summary(dataMod)
lmtest::dwtest(dataMod)
mean(dataMod$residuals)
vif(dataMod)
par(mfrow =c(2,2))
plot(dataMod)

##Need to stick with the base model as this new attempt does not improve the model.
##******Copied from above
##*#Best model appears to be the base model

# final model
finalTotalMod <- lm(TotalAirPollution ~ ., data = totalAirPollutionModelData)
summary(finalTotalMod)
lmtest::dwtest(finalTotalMod)
mean(finalTotalMod$residuals)
vif(finalTotalMod)
par(mfrow =c(2,2))
plot(finalTotalMod)

