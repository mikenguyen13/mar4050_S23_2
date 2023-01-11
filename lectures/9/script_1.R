library(randomNames)
library(tidyverse)


set.seed(123)
n = 100
names = randomNames(n)
satisfaction = sample(c(0,1,2,3,4,5), replace = T, size = n)
housing = sample(c(0, 1), replace = TRUE, size = n) # 0 = On-campus, 1 = Off-campus
delivery = sample(c(0,1), replace = T, size = n) # 0 = used, 1 = never used
gender = sample(c(0, 1), replace = TRUE, size = n) # 0 = Male, 1 = Female
height = rnorm(n, mean = 6, sd = 0.2)

df = data.frame(names, satisfaction, housing, delivery, gender, height)

# create mode function 
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

mean(satisfaction)
median(satisfaction)
getmode(satisfaction)


median(gender)

median(delivery)


boxplot()
hist()

