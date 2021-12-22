rm(list=ls());
DIR = "/Users/kyshu/Desktop/BME/BME423R/BME423HW04";
setwd(DIR);
cat("\014");
graphics.off();

with.anesthetic <- c(135,141,145,165,167,173,178,191,244,245,256,267,268,282)
without.anesthetic <- c(129,131,138,139,142,143,144,147,155,156,163,171,192,230)
library(lsr)
wide <- data.frame(with.anesthetic, without.anesthetic)

library(survival)
library(ggplot2)
library(ggfortify)
mydata<-read.csv("drughiv.csv", sep = ",", header = TRUE)

fit<- survfit(Surv(time, delta)~drug, data = mydata, conf.type = "plain", type = "kaplan-meier")
summary(fit)

#just AZT and zalcitabine
plot(fit[1]) #plot of 
fit[1]
#median is 49 days, and the 95% CI of the probability of CD4 return is 
#0.2294<p<0.733

#AZT + zalcitabine + squinavir
plot(fit[2])
fit[2]
#median is 90 days, and the 95% CI of the probability of CD4 return is 
#0.1811<p<0.682

survdiff(Surv(time, delta)~drug, data = mydata, rho = 0)
#Chisq = 2, with p = 0.2, so there is no significant difference between the CD4 
#return functions of the two treatments 
