#Chapter 5

#Question 9

#import csv file
Boston <- read.csv("Boston.csv", header = TRUE)
head(Boston)
Boston = Boston[, 7:20]
mean(Boston$medv)

#average home value is 22.53281

sd(Boston$medv) / sqrt(length(Boston$medv))
boot.fn <- function(vector, index) {mean(vector[index])}

#Standard error is 0.4088611

(boot_results <- boot(data = Boston$medv, statistic = boot.fn, R = 1000))
boot_results_SE <- sd(boot_results$t)
round(c(mean(Boston$medv) - 2*boot_results_SE, mean(Boston$medv) + 2*boot_results_SE), 4)

#Standard error is 0.4081538

t.test(Boston$medv)

#mean of x 22.53281 

median(Boston$medv)

#estimated population median is 21.2

#CI of 95 
boot.fn <- function(vector, index) {median(vector[index])}
(boot_results <- boot(data = Boston$medv, statistic = boot.fn, R = 1000))


quantile(Boston$medv, 0.1)

#CMEDV is 12.8

boot.fn <- function(vector, index) {quantile(vector[index], 0.1)}
(boot_results <- boot(data = Boston$medv, statistic = boot.fn, R = 1000))

