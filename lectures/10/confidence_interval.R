# visualization
# mean CI

n.draw = 100
mu = 12 # the true mean 
n = 100000
SD = 1.5 # the true standard deviation 


draws = matrix(rnorm(n.draw * n , mu, SD), n)

get.conf.int = function(x) t.test(x)$conf.int # we want to get the confidence interval for each sample
conf.int = apply(draws, 2, get.conf.int)
sum(conf.int[1, ] <= mu & conf.int[2, ] >= mu)

plot(
  
  range(conf.int),
  c(0, 1 + n.draw),
  type = "n",
  xlab = "mean tail length",
  ylab = "sample run"
)
for (i in 1:n.draw) lines(conf.int[, i], rep(i, 2), lwd = 2)
abline(v = 12, lwd = 2, lty = 2)


