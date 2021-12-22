rm(list=ls());
DIR = "/Users/kyshu/Desktop/BME/BME423R/Discussion 2";
setwd(DIR);
cat("\014");
graphics.off();

library(lsr)

qt(0.05/6, 42, lower.tail = FALSE)
qt(0.05/2, 15, lower.tail = FALSE)
