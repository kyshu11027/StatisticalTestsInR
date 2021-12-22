rm(list=ls());
DIR = "/Users/kyshu/Desktop/BME/BME423R/Discussion 2";
setwd(DIR);
cat("\014");
graphics.off();

latitude <- c(33, 34.5, 35, 37.5, 39, 41.8, 39, 39, 28, 33, 44.5, 40, 40.2, 42.2, 38.5,37.8, 31.2, 45.2, 39, 42.2, 43.5, 46, 32.8, 38.5, 47, 41.5, 39, 43.8, 40.2, 35, 43, 35.5, 47.5, 40.2,35.5, 44, 40.8, 41.8, 33.8, 44.8, 36, 31.5, 39.5, 44, 37.5, 47.5, 38.8, 44.5, 43)
mortality <- c(219, 160, 170,182,149,159,200,177,197,214,116,124,128,128,166,147,190,117,162,143,117,116,207,131,109,122,191,129,159,141,152,199,115,131,182,136,132,137,178,86,186,229,142,153,166,117,136,110,134)
mortalityData <- data.frame(latitude, mortality)
n = 49
b = (n*sum(latitude*mortality)-sum(latitude)*sum(mortality))/(n*sum(latitude^2) - (sum(latitude))^2)
a = (sum(mortality)*sum(latitude^2) - sum(latitude)*sum(latitude*mortality))/(n*sum(latitude^2) - sum(latitude)^2)
mortality.regression <- lm(formula = mortality ~ latitude, data = mortalityData )
summary(mortality.regression)
yhat <- a + b*latitude
mortality.var <- (1/(n-2))* sum((mortality - yhat)^2)
sx = sd(latitude)
mean.latitude = mean(latitude)
syx = sqrt(mortality.var)
sum((latitude - mean.latitude)*(mortality - mean(mortality)))
sum((latitude-mean.latitude)^2)
sum((mortality - mean(mortality))^2)

#Question 3
rm(list=ls());
DIR = "/Users/kyshu/Desktop/BME/BME423R/BME423HW03";
setwd(DIR);
cat("\014");
graphics.off();

infection.data <- read.csv("Hwk3_Hospital_Infection_data.csv", header = TRUE, sep = ",")
infection.regression <- lm(formula = Infection.Risk ~ Stay + Age + XRay, data = infection.data)

#3a: 
summary(infection.regression)
# x1 = Stay, x2 = Age, x3 = XRay
# yhat = 1.00 + 0.308x1 - 0.023x2 + 0.0197x3

#3b: To reject the null hypothesis of no relation between infection risk and any
#of the variables above, I would conduct t-tests for each of the variables. 
#The null hypothesis would be that there was no relation, so each t-test would 
#compare to 0 and the standard deviation would be the standard errors in the 
#summary of the regression.

#3c:
#Length of Stay:
  #H0: There is no relation between length of stay and risk of infection.
  #t value = 5.189
  #Pr(>|t|) < 0.001
  #There is a significant relationship between length of stay and risk of 
  #infection. (P < 0.001)
#Age:
  #H0: There is no relation between age and risk of infection.
  #t value = -0.023
  #Pr(>|t|) > 0.05
  #There is no significant relationship between age and risk of infection.
#X-Ray:
  #H0: There is no relation between number of X-Rays and risk of infection
  #t value = 0.0197
  #Pr(>|t|) < 0.001
  #There is a significant relationship between number of X-Rays and risk of
  #infection. (P < 0.001)

#3d:
confint(infection.regression, level = 0.95)
#Since the confidence intervals of the intercept and age variable contain zero,
#there is no significant relationship between age and risk of infection, and 
#there is no significant difference between the intercept and 0. This aligns
#with the previous results.

#3e:
infection.regression2 = lm(formula = Infection.Risk ~ Stay + XRay, data = infection.data)
summary(infection.regression2)
prediction = -0.150603 + 0.295845 * 12 + 0.020227 * 85
print(prediction)

#Question 4
rm(list=ls());
DIR = "/Users/kyshu/Desktop/BME/BME423R/BME423HW03";
setwd(DIR);
cat("\014");
graphics.off();

before <- c(66.79, 54.81, 58.47, 83.55, 63.25, 61.56, 86.58, 43.52, 77.54, 73.03, 53.61)
after <- c(64.55, 58.69, 57.88, 92.07, 67.33, 67.68, 91.29, 49.85, 74.39, 75.93, 56.48)
mood.data <- data.frame(before, after)

library(lsr)
pairedSamplesTTest(formula = ~ before + after, data = mood.data)

#Question 5
rm(list=ls());
DIR = "/Users/kyshu/Desktop/BME/BME423R/BME423HW03";
setwd(DIR);
cat("\014");
graphics.off();

before <- c(34, 32, 30, 31, 32, 31, 34, 33, 35, 30, 32, 31);
days.30 <- c(35, 38, 33, 29, 36, 33, 39, 33, 38, 36, 32, 30);
days.180 <- c(38, 39, 37, 36, 39, 38, 38, 35, 36, 35, 34, 35);


hematocrit <- data.frame(before, days.30, days.180)
rowVars <- rowSums((hematocrit - rowMeans(hematocrit))^2)
mean.before <- mean(before)
mean.30 <- mean(days.30)
mean.180 <- mean(days.180)
Xbar <- (mean.before + mean.30 + mean.180)/3
SSwitsubjs <- sum(rowVars)        
SStreat  <- 12*((mean.before - Xbar)^2 + (mean.30 - Xbar)^2 + (mean.180 - Xbar)^2);
SSres = SSwitsubjs - SStreat
F = (SStreat/2)/(SSres/22)
MSres = SSres/22
