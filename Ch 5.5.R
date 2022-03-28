#Chapter 5

#  Question 5:
library(ISLR)
#Using the head function to fetch first part of the data
head(Default)
#Using the set.seed to initialize a pseudorandom number generator
set.seed(330)


#Using glm we are able to fit the generalized linear model
glm.fit = glm(default ~ income + balance, data = Default, family = "binomial")
#Produce results using the summary function
summary(glm.fit)

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


### 1st time
train = sample(dim(Default)[1], 0.80*dim(Default)[1])
glm.fit = glm(default ~ income + balance, data = Default, subset = train, family = "binomial")
glm.probs = predict(glm.fit, Default[-train, ], type = "response")
glm.preds = rep("No", dim(Default)[1])
glm.preds[glm.probs > 0.5] = "Yes"
mean(glm.preds != Default[-train, "default"])


### 2nd time
train = sample(dim(Default)[1], 0.80*dim(Default)[1])
glm.fit = glm(default ~ income + balance, data = Default, subset = train, family = "binomial")
glm.probs = predict(glm.fit, Default[-train, ], type = "response")
glm.preds = rep("No", dim(Default)[1])
glm.preds[glm.probs > 0.5] = "Yes"
mean(glm.preds != Default[-train, "default"])


### 3rd time
train = sample(dim(Default)[1], 0.80*dim(Default)[1])
glm.fit = glm(default ~ income + balance, data = Default, subset = train, family = "binomial")
glm.probs = predict(glm.fit, Default[-train, ], type = "response")
glm.preds = rep("No", dim(Default)[1])
glm.preds[glm.probs > 0.5] = "Yes"
mean(glm.preds != Default[-train, "default"])

1 - mean(Default[-train, "default"] == "No")
### Differences is seen each time. 


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

