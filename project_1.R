install.packages("psych")
install.packages("purrr")
install.packages("tidyr")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("cartools")
install.packages("car")
install.packages("MASS")
install.packages("geoR")
install.packages("DataExplorer")
install.packages("corrplot")
install.packages("rcompanion")
library(corrplot)
library(purrr)
library(tidyr)
library(ggplot2)
library(psych)
library(dplyr)
library(caTools)
library(rcompanion)
library(car)
library(MASS)
library(geoR)
library(DataExplorer)

# data exploration
mortgage <-read.csv("Hours-to-Pay-Mortgage.csv")
dim(mortgage)
names(mortgage)
sapply(mortgage, class) 


sapply(Clean_Mortgage, class) 

is.na(mortgage)  #true for column 'X'

#datacleaning
#1. removing invalid column "X"
Clean_Mortgage <- mortgage[-10]
names(Clean_Mortgage)
#2. removing column number of periods as it makes no effect
Clean_Mortgage <- Clean_Mortgage[-8]
names(Clean_Mortgage)
summary(Clean_Mortgage)
attach(Clean_Mortgage)
#3.checking class of datatype
sapply(Clean_Mortgage, class)  #instead of using loop, used sapply

#4. Changing data type to numeric
Clean_Mortgage$Median.Home.Listing.Price<- as.numeric(Clean_Mortgage$Median.Home.Listing.Price)
Clean_Mortgage$X30.year.Fixed.Mortgage.Rate <- as.numeric(Clean_Mortgage$X30.year.Fixed.Mortgage.Rate)
Clean_Mortgage$Monthly.Mortgage.Payment<- as.numeric(Clean_Mortgage$Monthly.Mortgage.Payment)
Clean_Mortgage$Median.Household.Income<- as.numeric(Clean_Mortgage$Median.Household.Income)
Clean_Mortgage$Present.Value<- as.numeric(Clean_Mortgage$Present.Value)
#instead of using loop, used sapply

sapply(Clean_Mortgage, class) 



#plotting missing data
head(is.na(mortgage),10)
plot_missing(mortgage)

plot_missing(Clean_Mortgage)

#rechecking the class
sapply(Clean_Mortgage, class)
#data cleaning
Clean_Mortgage_key_val <- gather(Clean_Mortgage, predictor, Hours.per.Month.to.Afford.a.Home, -State, -City )
par(mfrow=c(4,4))
head(Clean_Mortgage_key_val)
tail(Clean_Mortgage_key_val)
#visualization
# scatterplot
 pairs.panels(Clean_Mortgage)

 #histogram
Clean_Mortgage_key_val %>%
  keep(is.numeric) %>%                                                     #keep() from purrr library will keep all the variables that satisfy predicate function(is.numeric)
  ggplot(aes(Clean_Mortgage_key_val$Hours.per.Month.to.Afford.a.Home)) +
  facet_wrap(~ Clean_Mortgage_key_val$predictor, scales = "free") +            #facetwrap is a better version of par(mfrow) - automatically selects rows and column - ggplot2 library
  geom_histogram()


#model2 key-val

Clean_Mortgage_key_val <- gather(Clean_Mortgage, predictor, 
                                 Hours.per.Month.to.Afford.a.Home, -State, -City )
par(mfrow=c(2,2))
head(Clean_Mortgage_key_val)
model = lm(Clean_Mortgage_key_val$Hours.per.Month.to.Afford.a.Home~Clean_Mortgage_key_val$predictor)
summary(model)
confint(model)
conf1<- predict(model, data=Clean_Mortgage_key_val, 
                interval = c("confidence"), level=0.95, type="response")
conf1
plot(model)
abline(model)
#model 1 modeling using multivariate linear regression
model1 = lm(Clean_Mortgage$Hours.per.Month.to.Afford.a.Home~
              Clean_Mortgage$Median.Home.Listing.Price+
              Clean_Mortgage$Median.Household.Income+
              Clean_Mortgage$Monthly.Mortgage.Payment+
              Clean_Mortgage$Present.Value+
              Clean_Mortgage$X30.year.Fixed.Mortgage.Rate)
summary(model1)
facet_wrap(~ model1, scales = "free") 
plot(model1)
points(Clean_Mortgage$Median.Home.Listing.Price+
         Clean_Mortgage$Median.Household.Income+
         Clean_Mortgage$Monthly.Mortgage.Payment+
         Clean_Mortgage$Present.Value+
         Clean_Mortgage$X30.year.Fixed.Mortgage.Rate~model1$fitted.values, pch=20)
confint(model1)

# multicollinearity
vif(model1) 
#R.Square
summary(model1)$adj.r.square
summary(model)$adj.r.square #The good news is that even when R-squared is low, low P values still indicate a real relationship between the significant predictors and the response variable.

#scaling
Df_scaled=data.frame(scale(Clean_Mortgage[3,8]))

scaled_model = lm(Df_scaled$Hours.per.Month.to.Afford.a.Home~Df_scaled$Median.Home.Listing.Price+Df_scaled$Median.Household.Income+Df_scaled$Monthly.Mortgage.Payment+Df_scaled$Present.Value+Df_scaled$X30.year.Fixed.Mortgage.Rate)
#transformation
box <- boxcox(Clean_Mortgage$Hours.per.Month.to.Afford.a.Home~Clean_Mortgage$Median.Home.Listing.Price+Clean_Mortgage$Median.Household.Income+Clean_Mortgage$Monthly.Mortgage.Payment+Clean_Mortgage$Present.Value+Clean_Mortgage$X30.year.Fixed.Mortgage.Rate)
Lambda(model)


sum((model1$fitted.values-mean(Clean_Mortgage$Hours.per.Month.to.Afford.a.Home))^2)
#SSRes
sum((model1$residuals)^2)
#SST
sum((Clean_Mortgage$Hours.per.Month.to.Afford.a.Home-mean(Clean_Mortgage$Hours.per.Month.to.Afford.a.Home))^2)
print("SST=SSRes+SSR")



#correlation

y <- Clean_Mortgage[7]
x <- Clean_Mortgage[c(3:6,8)]
corr2 <- cor(x,y)
corrplot(corr2, method = "number") #corrplotlibrary

#model3
model_clean <- lm(Hours.per.Month.to.Afford.a.Home~Median.Home.Listing.Price+Present.Value, data=Clean_Mortgage)
summary(model_clean)
par(mfrow=c(2,2))
plot(model_clean)

#transformation test
boxcox(model_clean)

#model4 

new_model<- lm(Hours.per.Month.to.Afford.a.Home~Present.Value, data=Clean_Mortgage)
summary(new_model)
par(mfrow=c(2,2))
scatter.smooth(x=Clean_Mortgage$Present.Value, y=Clean_Mortgage$Hours.per.Month.to.Afford.a.Home, 
               main="Hours ~ Present value")


par(mfrow=c(2,2))
plot(new_model)
abline(new_model)
boxcox((new_model))

#model selection
AIC(new_model)  #2nd lowest
AIC(model_clean)
AIC(model1)
AIC(model)

BIC(new_model) #lowest
BIC(model_clean)
BIC(model1)
BIC(model)

#no transformation needed on this model

box <- boxcox(Clean_Mortgage$Hours.per.Month.to.Afford.a.Home~Clean_Mortgage$Present.Value)
Clean_Mortgage$Hours.per.Month.to.Afford.a.Home <- log(Clean_Mortgage$Hours.per.Month.to.Afford.a.Home)
logmodel <- lm(Clean_Mortgage$Hours.per.Month.to.Afford.a.Home~Clean_Mortgage$Present.Value)
summary(logmodel)
plot(logmodel)

#test and train

# Create Training and Test data -
set.seed(100)  # setting seed to reproduce results of random sampling
trainingRowIndex <- sample(3:nrow(Clean_Mortgage), 0.8*nrow(Clean_Mortgage))  # row indices for training data
trainingData <- Clean_Mortgage[trainingRowIndex, ]  # model training data
testData  <- Clean_Mortgage[-trainingRowIndex, ]   # test data


# Build the model on training data -
lmMod <- lm(Hours.per.Month.to.Afford.a.Home~Present.Value+Median.Home.Listing.Price, data=trainingData)  # build the model
hourPred <- predict(lmMod, testData)  # predict hours
summary(lmMod)
actuals_preds <- data.frame(cbind(actuals=testData$Hours.per.Month.to.Afford.a.Home, predicteds=hourPred))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)  # 82.7%
head(actuals_preds)
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  
min_max_accuracy
par(mfrow=c(2,2))
plot(lmMod)

influence<- lm.influence(lmMod)$hat
summary(influence)
press <- lmMod$residuals/(influence)
summary(press)

conf1<- predict(lmMod, data=Clean_Mortgage, 
                interval = c("confidence"), level=0.95, type="response")
conf1


BIC(new_model) #lowest
BIC(model_clean)
BIC(model1)
BIC(model)
BIC(lmMod)
plot(BIC(lmMod))
BIC(logmodel)
