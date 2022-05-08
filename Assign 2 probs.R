##We will read the College data set into R using the read.csv() function. Load the College data set into R. 
college <- read.csv("./College.csv", stringsAsFactors = TRUE)

##Looking at the data with the View() function, we can see that the first column only contains the name of each university.
#R could treat this as data but we need the names so we will use the command
##### > rownames(college) <- college[, 1] > View(college)
library(tidyverse)
rownames(college) <- college$X
college$X <- NULL
glimpse(college)

##Multiple steps
#To generate a numerical summary of the variables in the data set, use the summary() function.
summary(college)
#To create a scatterplot matrix of the first ten columns or variables in the data, use the pairs() function. Remember that you can use A[,1:10] to refer to the first ten columns of a matrix A.
pairs(college[ ,1:10])
#To create side-by-side boxplots of Outstate versus Private, use the plot() function.
theme_set(theme_light()) # setting a theme for all graphs (looks cleaner)
ggplot(college, aes(x = Private, y = Outstate, fill = Private)) + 
  geom_boxplot() + 
  scale_y_continuous(labels = scales::comma_format()) + 
  theme(legend.position = "none") + 
  labs(title = "Outstate vs Private - Boxplot")
#Bin the Top10perc variable to create a new qualitative variable called Elite. We will categorize universities into two groups based on whether or not the proportion of students from the top 10% of their high school classes exceeds 50%.
college$Elite <- factor(ifelse(college$Top10perc <= 50, "No", "Yes"))
#Use the summary function to see how many elite
summary(college$Elite)
#Plot
ggplot(college, aes(x = Elite, y = Outstate, fill = Elite)) + 
  geom_boxplot() + 
  scale_y_continuous(labels = scales::comma_format()) + 
  theme(legend.position = "none") + 
  labs(title = "Outstate vs Elite - Boxplot")
#Produce histogram
apps_1 <- ggplot(college, aes(x = Apps)) + 
  geom_histogram(bins = 30, fill = "deepskyblue3") + 
  scale_x_continuous(labels = scales::comma_format()) + 
  labs(title = "Apps - Histogram", 
       subtitle = "Bins = 30", 
       y = "Count")
apps_2 <- ggplot(college, aes(x = Apps)) + 
  geom_histogram(bins = 60, fill = "deepskyblue3") + 
  scale_x_continuous(labels = scales::comma_format(), limits = c(0, 20000)) + 
  labs(title = "Apps - Histogram", 
       subtitle = "Bins = 60, Apps range: 0 - 20,000", 
       y = "Count")
phd_1 <- ggplot(college, aes(x = PhD)) + 
  geom_histogram(bins = 15, fill = "#16A085") + 
  scale_x_continuous(labels = scales::comma_format()) + 
  labs(title = "PhD - Histogram", 
       subtitle = "Bins = 15", 
       y = "Count")
phd_2 <- ggplot(college, aes(x = PhD)) + 
  geom_histogram(bins = 40, fill = "#16A085") + 
  scale_x_continuous(labels = scales::comma_format()) + 
  labs(title = "PhD - Histogram", 
       subtitle = "Bins = 40", 
       y = "Count")
room.board_1 <- ggplot(college, aes(x = Room.Board)) + 
  geom_histogram(bins = 10, fill = "#BB8FCE") + 
  scale_x_continuous(labels = scales::comma_format()) + 
  labs(title = "Room.Board - Histogram", 
       subtitle = "Bins = 10", 
       y = "Count")
room.board_2 <- ggplot(college, aes(x = Room.Board)) + 
  geom_histogram(bins = 30, fill = "#BB8FCE") + 
  scale_x_continuous(labels = scales::comma_format()) + 
  labs(title = "Room.Board - Histogram", 
       subtitle = "Bins = 30", 
       y = "Count")
library(gridExtra)
grid.arrange(apps_1, apps_2, 
             phd_1, phd_2, 
             room.board_1, room.board_2, 
             ncol = 2, nrow = 3)



##Install libraries 
library(ISLR)
glimpse(Auto)
sum(is.na(Auto)) 

###predictors that are quantitative ones that are qualitative
#Quantitative: mpg - Miles per gallon. cylinders - Number of cylinders between 4 and 8. displacement - Engine displacement (cu. inches). horsepower - Engine horsepower. weight - Vehicle weight (lbs.). acceleration - Time to accelerate from 0 to 60 mph (sec.). year - Model year (modulo 100)
#Qualitative:origin - Origin of car (1. American, 2. European, 3. Japanese). name - Vehicle name

#The range() function is used to calculate the range of each quantitative predictor.
range_Auto <- data.frame(sapply(Auto[ ,1:7], range))
rownames(range_Auto) <- c("min:", "max:")
range_Auto

#each quantitative predictor's mean and standard deviation
sapply(Auto[ ,1:7], mean)
sapply(Auto[ ,1:7], sd)

#We can find the range, mean, and standard deviation of each predictor in the subset of data that remains by removing the 10th through 85th observations.
Auto_2 <- Auto[-c(10:85), ]
range_Auto_2 <- data.frame(sapply(Auto_2[ ,1:7], range))
rownames(range_Auto_2) <- c("min:", "max:")
range_Auto_2
sapply(Auto_2[ ,1:7], mean)
sapply(Auto_2[ ,1:7], sd)

#Using the entire data set, investigate the predictors graphically using scatterplots or other tools of your choice.
pairs(Auto[ ,1:7])
Auto$origin <- factor(Auto$origin, labels = c("American", "European", "Japanese"))
ggplot(Auto, aes(x = weight, y = acceleration)) + 
  geom_point() + 
  theme(legend.position = "none") + 
  scale_x_continuous(labels = scales::comma_format()) + 
  labs(x = "Weight", 
       y = "Acceleration", 
       title = "Correlation between weight and acceleration")
ggplot(Auto, aes(x = weight, y = acceleration, col = origin)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  facet_wrap(~ origin) + 
  theme(legend.position = "none") + 
  scale_x_continuous(labels = scales::comma_format()) + 
  labs(x = "Weight", 
       y = "Acceleration", 
       title = "Correlation between weight and acceleration, by origin")
ggplot(Auto, aes(x = year + 1900, y = displacement)) + 
  geom_jitter() + 
  theme(legend.position = "none") + 
  labs(x = "Year", 
       y = "Displacement", 
       title = "Engine Displacement (trends over time)")



#Install library 
##import the Boston data set
library(MASS)
#import the Boston data set. 
#Determine the number of rows and columns in this data set. Examine the rows and columns to see what they represent.
dim(Boston)

#pairwise scatterplots of the predictors
pairs(Boston)

#Find any  predictors related to the per capita crime rate
Boston$chas <- factor(Boston$chas)
predictor_df <- data.frame(response = character(), 
                           varname = character(), 
                           R2 = numeric(), 
                           R2_log = numeric(), 
                           R2_quad = numeric(), 
                           max_R2 = numeric(), 
                           stringsAsFactors = F)

for (i in setdiff(1:ncol(Boston), 4)) { # excluding 'chas' as it uses different eval metric
  response <- names(Boston)[i]
  predictor_info <- best_predictor(Boston, names(Boston)[i])[1, -2]
  predictor_df <- rbind(predictor_df, cbind(response, predictor_info))
}
predictor_df %>%
  arrange(desc(max_R2)) %>%
  rename(best_predictor = varname)
best_predictor(Boston, "chas")[1, ]

#Find any census tracts in Boston that appear to have unusually high crime rates. Pupil-teacher ratios, tax rates
best_predictor(Boston, "crim")
ggplot(Boston, aes(x = medv, y = crim)) + 
  geom_point(alpha = 0.5) + 
  geom_smooth(method = "lm", formula = "y ~ x + I(x^2)", col = "green") +
  labs(title = "Medv vs Crim - Scatter Plot", 
       x = "Medv", 
       y = "Crim")
ggplot(Boston, aes(x = dis, y = crim)) + 
  geom_point(alpha = 0.5) + 
  geom_smooth(method = "lm", formula = "y ~ x + I(x^2)", col = "green") +
  labs(title = "Dis vs Crim - Scatter Plot", 
       x = "Dis", 
       y = "Crim")
ggplot(Boston, aes(x = lstat, y = crim)) + 
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", formula = "y ~ x + I(x^2)", col = "green") +
  labs(title = "Lstat vs Crim - Scatter Plot",
       x = "Lstat", 
       y = "Crim")

#crime rates
ggplot(Boston, aes(x = "", y = crim)) + 
  geom_boxplot() + 
  scale_y_continuous(breaks = seq(0, 100, 10)) + 
  labs(y = "Crim", 
       title = "Crim - Boxplot") + 
  coord_flip() + 
  theme(axis.title.y = element_blank(), 
        axis.ticks.y = element_blank())

range(Boston$crim)
#Tax rates
range(Boston$tax)
ggplot(Boston, aes(x = tax)) + 
  geom_histogram(binwidth = 10, fill = "mediumseagreen") + 
  scale_y_continuous(breaks = seq(0, 200, 20)) +
  labs(title = "Tax - Histogram", 
       x = "Tax", 
       y = "Count")

#Pupil-teacher Ratio
range(Boston$ptratio)
ggplot(Boston, aes(x = ptratio)) + 
  geom_histogram(binwidth = 0.25, fill = "deepskyblue") + 
  scale_x_continuous(breaks = seq(12, 23, 0.5)) + 
  scale_y_continuous(breaks = seq(0, 200, 20)) + 
  labs(title = "Ptratio - Histogram", 
       x = "Ptratio", 
       y = "Count")


table(Boston$chas)


median(Boston$ptratio)


Boston[Boston$medv == min(Boston$medv), ]
sum(Boston$rm > 7)
Boston_gt_8rooms <- Boston[Boston$rm > 8, ]
nrow(Boston_gt_8rooms)



#Istall ISLR package
#Use auto data set
library(ISLR)
data("Auto")
attach(Auto)
head(Auto)

##To perform a simple linear regression with mpg as the response and horsepower as the predictor, use the lm() function. To print the results, use the summary() function.
fit.8a <- lm(mpg~horsepower)
summary(fit.8a)
predict(fit.8a,data.frame(horsepower=c(98)),interval = "confidence")
predict(fit.8a,data.frame(horsepower=c(98)),interval = "confidence")

#Make a graph of the response and the predictor. To display the least squares regression line, use the abline() function.
plot(horsepower,mpg, main = "mpg vs horsepower", col="red" )
abline(fit.8a, col="blue")

#To generate diagnostic plots of the least squares regression fit, use the plot() function. 
par(mfrow =c(2,2))
plot(fit.8a)
plot(hatvalues (fit.8a))
which.max (hatvalues (fit.8a))
abline(0.0225,0)


#Use the auto data set 
##Create a scatterplot matrix with all of the variables in the data set.
pairs(Auto)

#Compute the matrix of correlations between the variables using the function cor(). You will need to exclude the name variable, which is qualitative
str(Auto)
cor(Auto[-c(9)])

#To perform a multiple linear regression with mpg as the response and all other variables except name as predictors, use the lm() function.
fit.9c <- lm(mpg~.,data = Auto[-c(9)])
summary(fit.9c)

fit.9.e1 <- lm(mpg~year*weight)
summary(fit.9.e1)
fit.9.e2 <- lm(mpg~cylinders * displacement+displacement * weight, data = Auto[, 1:8])
summary(fit.9.e2)


fit.9f.1 <- lm(mpg~poly(horsepower,2))
fit.9f.2 <- lm(mpg~poly(displacement,2))
fit.9f.3 <- lm(mpg~poly(displacement,1))
par(mfrow =c(2,2))
summary(fit.9f.1)
plot(fit.9f.1)
par(mfrow =c(2,2))
summary(fit.9f.2)
plot(fit.9f.2)


#Install ISLR package
#Use carseats dataset
library(ISLR)
data("Carseats")
head(Carseats)

#Fit a multiple regression model to predict sales based on price, city, and country.
str(Carseats)
attach(Carseats)
fit.10.a1 <- lm(Sales~Price+Urban+US)
contrasts(US)
summary(fit.10.a1)
plot(Sales)


#Fit a smaller model based on your response to the previous question, using only the predictors with evidence of association with the outcome.
fit.10.e1 <- lm(Sales~Price+US)
summary(fit.10.e1)

#The models in (a) and (e) fit the data, find how they fit
anova(fit.10.a1,fit.10.e1)

#Determine the 95 percent confidence intervals for the coefficient using the model from (e) (s)
confint(fit.10.e1)

#evidence of outliers or observations with high leverage in the model from
par(mfrow=c(2,2))
plot(fit.10.e1)