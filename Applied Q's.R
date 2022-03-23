#Chapter 4
## Question 13:


#Chapter 5

#  Question 5:
#Install and run library
library(ISLR)
#Using the head function to fetch first part of the data
head(Default)
#Using the set.seed to initialize a pseudorandom number generator
set.seed(330)
# Part a fit logistic regression model 
#Using glm we are able to fit the generalized linear model
glm.fit = glm(default ~ income + balance, data = Default, family = "binomial")
#Produce results using the summary function
summary(glm.fit)
# Part b split/fit data set, obtain prediction of default status, and validation set error
### Using a 80/20 split for training and testing
train = sample(dim(Default)[1], 0.80*dim(Default)[1])
### Using income and balance, we are able to fit the model
glm.fit = glm(default ~ income + balance, data = Default, subset = train, family = "binomial")
### For a posterior probability greater than 0.5 predictions will be stores in glm.pred 
glm.probs = predict(glm.fit, Default[-train, ], type = "response")
glm.preds = rep("No", dim(Default)[1])
glm.preds[glm.probs > 0.5] = "Yes"
### Computing the validation set error
mean(glm.preds != Default[-train, "default"])
### A value of 0.024 was returned 
# Part c repeating the process in b 3 times
### 1st time
train = sample(dim(Default)[1], 0.80*dim(Default)[1])
glm.fit = glm(default ~ income + balance, data = Default, subset = train, family = "binomial")
glm.probs = predict(glm.fit, Default[-train, ], type = "response")
glm.preds = rep("No", dim(Default)[1])
glm.preds[glm.probs > 0.5] = "Yes"
mean(glm.preds != Default[-train, "default"])
### value of 0.0245 was obtained

### 2nd time
train = sample(dim(Default)[1], 0.80*dim(Default)[1])
glm.fit = glm(default ~ income + balance, data = Default, subset = train, family = "binomial")
glm.probs = predict(glm.fit, Default[-train, ], type = "response")
glm.preds = rep("No", dim(Default)[1])
glm.preds[glm.probs > 0.5] = "Yes"
mean(glm.preds != Default[-train, "default"])
### this time the value was 0.028

### 3rd time
train = sample(dim(Default)[1], 0.80*dim(Default)[1])
glm.fit = glm(default ~ income + balance, data = Default, subset = train, family = "binomial")
glm.probs = predict(glm.fit, Default[-train, ], type = "response")
glm.preds = rep("No", dim(Default)[1])
glm.preds[glm.probs > 0.5] = "Yes"
mean(glm.preds != Default[-train, "default"])
### The value became 0.0265
1 - mean(Default[-train, "default"] == "No")
### Differences is seen. Average test error is 0.031 

# Part D Adding dummy variable for student

with.student = rep(0, 50)
without.student = rep(0, 50)
for (i in 1:50){
  train = sample(dim(Default)[1], 0.80*dim(Default)[1])
  with.student.fit = glm(default ~ ., data = Default, subset = train, family = "binomial")
  without.student.fit = glm(default ~ income + balance, data = Default, subset = train, family = "binomial")
  with.student.probs = predict(with.student.fit, Default[-train, ], type = "response")
  without.student.probs = predict(without.student.fit, Default[-train, ], type = "response")
  with.student.preds = rep("No", dim(Default)[1])
  without.student.preds = rep("No", dim(Default)[1])
  with.student.preds[with.student.probs > 0.5] = "Yes"
  without.student.preds[without.student.probs > 0.5] = "Yes"
  with.student[i] = mean(with.student.preds != Default[-train, "default"])
  without.student[i] = mean(without.student.preds != Default[-train, "default"])
}
difference = with.student - without.student
errors = data.frame(with.student, without.student, difference)
mean(errors$difference)
