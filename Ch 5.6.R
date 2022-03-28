#Chapter 5


#Question 6

library(boot)
set.seed(312)
glm.fit = glm(default ~ income + balance, data = Default, family = "binomial")
summary(glm.fit)$coefficients[, 2]


boot.fn = function(data){
  coefs = coef(glm(default ~ income + balance, data = data, 
                   family = "binomial"))[c("income", "balance")]
  return(coefs)}
boot.fn(Default)


set.seed(101)
boot(Default, boot.fn, 1000)

#One of the assumptions of Logistic regression is that the data is independent and identically distributed. Because of this and the high mortality rate, the only valid strategy beyond observation would be a randomized trial and this would never be possible given
