#Chapter 4
## Question 13:
#import csv file
Weekly <- read.csv("Weekly.csv", header = TRUE, stringsAsFactors=T)
#Install and run library
library(ISLR)
#Using the head function to fetch first part of the data
head(Weekly)


summary(Weekly)
prop.table(table(Weekly$Direction))
#There are no obvious trends seen with the plot
pairs(Weekly[ ,-9])
#Looking at volume of shares overtime, there is an increase at 2007 and is the highest at 2009
plot(Weekly$Volume, ylab = "Shares traded (in billions)")
cor(Weekly[-9])


#Using glm we are able to find the direction the generalized linear model
glm_direction <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, 
               data = Weekly, 
               family = "binomial")
summary(glm_direction)

#In this logistic regression model, Lag2 was the only statistically significant predictor. At 5% significance level, this indicates that Lag2 is associated with Direction. 


predicted <- factor(ifelse(predict(glm_direction, type = "response") < 0.5, "Down", "Up"))
prop.table(table(predicted))

#Using all of the original predictors and a prediction threshold of 0.5, the logistic regression model correctly predicted 54 down weeks and 557 up days.


train <- Weekly[Weekly$Year <= 2008, ]
test <- Weekly[Weekly$Year > 2008, ]
glm_direction <- glm(Direction ~ Lag2, 
               data = train, 
               family = "binomial")
predicted <- factor(ifelse(predict(glm_direction, newdata = test, type = "response") < 0.5, "Down", "Up"))
prop.table(table(predicted))

#Using only Lag2 as the predictor, and then make predictions for the held-out data


train = (Weekly$Year < 2009)
glm.fit = glm(Direction ~ Lag2, data = Weekly, subset = train, family = "binomial")
summary(glm.fit)
glm.probs = predict(glm.fit, Weekly[!train, ], type = "response")
glm.pred = rep("Down", dim(Weekly[!train, ])[1])
glm.pred[glm.probs > 0.5] = "Up"
mean(glm.pred == Weekly[!train, ]$Direction)
mean(Weekly[!train, ]$Direction == "Up")

#The confusion matrix for the original logistic regression model, Part 4, and the new linear discriminant analysis model are identical


library(MASS)
lda.fit = lda(Direction ~ Lag2, data = Weekly, subset = train)
lda.fit
lda.pred = predict(lda.fit, Weekly[!train, ])
table(lda.pred$class, Weekly[!train, ]$Direction)
mean(lda.pred$class == Weekly[!train, ]$Direction)

#The corresponding model correctly predicted the market direction


qda.fit = qda(Direction ~ Lag2, data = Weekly, subset = train)
qda.fit
qda.pred = predict(qda.fit, Weekly[!train, ])
table(qda.pred$class, Weekly[!train, ]$Direction)
mean(qda.pred$class == Weekly[!train, ]$Direction)

#Based on the results of k-nearest neighbors classification with k=1, the model correctly predicted the market direction for 50% of the weeks in the held-out data. This was when using only Lag2 as the single predictor.


library(class)
train.X = data.frame(Weekly[train, ]$Lag2)
test.X = data.frame(Weekly[!train, ]$Lag2)
train.Direction = Weekly[train, ]$Direction
set.seed(1)
knn.pred = knn(train.X, test.X, train.Direction, k = 1)
table(knn.pred, Weekly[!train, ]$Direction)
mean(knn.pred == Weekly[!train, ]$Direction)


###The best overall predication accuracy was equal among logistic regression and linear discriminant analysis.

