rm(list=ls());
DIR = "/Users/kyshu/Desktop/BME/BME423R/Discussion 2";
setwd(DIR);
cat("\014");
graphics.off();


p1 <- pnorm(6, mean = 5, sd = 0.6, lower.tail = TRUE, log.p = FALSE) - pnorm(4.5, mean = 5, sd = 0.6, lower.tail = TRUE, log.p = FALSE)

p2 <- pnorm(0, mean = 5, sd = 0.6, lower.tail = TRUE)

p3 <- 1 - pnorm(6, mean = 5, sd = 0.6, lower.tail = TRUE)

male = c(69, 66, 70, 71, 72, 67, 70, 71, 72, 69)
female = c(65, 69, 59, 64, 62, 67, 68, 62, 64, 65);
maleFemale = c(male, female)
mydata <- matrix(maleFemale, nrow = 10, ncol = 2)
colnames(mydata) <- c("Male", "Female")

describeBy(mydata, group = )

male.var = var(male)
female.var = var(female)
male.median = median(male)
female.median = median(female)
male.range = range(male)
female.range = range(female)
male.sd = sd(male)
female.sd = sd(female)
summary(male)
summary(female)
male.SEM = male.sd/sqrt(10)
female.SEM = female.sd/sqrt(10)

qqnorm(male, main = "Male Normal Q-Q Plot")
qqnorm(female, female = "Female Normal Q-Q Plot")


rm(list=ls());
DIR = "/Users/kyshu/Desktop/BME/BME423R/BME423HW01";
setwd(DIR);
cat("\014");
graphics.off();

mypath = "BNP_data_2021.csv";
BNPdata = read.csv(mypath, header = TRUE, sep = ",")

#R-Solution

my.anova <- aov(BNP ~ Group, BNPdata)
summary(my.anova)

#vn = 2, vd = 162

qf(0.95, df1 = 2, df2 = 162)

