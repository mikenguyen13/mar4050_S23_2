# Hypothesis test

N=20 #just chosen arbitrarily
samp=rnorm(N)
myTest=t.test(samp)
tcrit=qt(0.025, df=(N-1))

dum=seq(-3.5, 3.5, length=10^4)#For the plot

plot(dum, dt(dum, df=(N-1)), type='l', xlab='t', ylab='f(t)')
abline(v=myTest$statistic, lty=2)
abline(v=tcrit, col='red', lty=2)
abline(v=-tcrit, col='red', lty=2)


# Alternatively

library(MASS)
h=na.omit(survey$Height)
pop.mean=mean(h)
h.sample = sample(h,30)
t.test(h.sample,mu=pop.mean)

library(gginference)
ggttest(t.test(h.sample,mu=pop.mean))



# Alternatively 

library(MASS)
library(ggplot2)

h = na.omit(survey$Height)
pop.mean = mean(h)

n_reps = 20
sample_size = 30
res_list = list()

for (i in 1:n_reps) {
  h.sample = sample(h, sample_size)
  res_list[[i]] = t.test(h.sample, mu=pop.mean)
}

dat = data.frame(id=seq(length(res_list)),
                 estimate=sapply(res_list, function(x) x$estimate),
                 conf_int_lower=sapply(res_list, function(x) x$conf.int[1]),
                 conf_int_upper=sapply(res_list, function(x) x$conf.int[2]))

p = ggplot(data=dat, aes(x=estimate, y=id)) +
  geom_vline(xintercept=pop.mean, color="red", linetype=2) +
  geom_point(color="grey30") +
  geom_errorbarh(aes(xmin=conf_int_lower, xmax=conf_int_upper), 
                 color="grey30", height=0.4)
p

# ggsave("CI_plot.png", plot=p, height=4, width=6, units="in", dpi=150)





# one sample t-test


set.seed(0)
treeVolume <- c(rnorm(75, mean = 36500, sd = 2000))
t.test(treeVolume, mu = 39000) # Ho: mu = 39000

###

set.seed(0)

ClevelandSpending <- rnorm(50, mean = 250, sd = 75)
NYSpending <- rnorm(50, mean = 300, sd = 80)

# export data to excel
# rio::export(cbind(ClevelandSpending, NYSpending), file.path(getwd(),"lectures","10","independent_t.xlsx"))

t.test(ClevelandSpending, NYSpending, var.equal = TRUE)

# similarly

spending <- c(ClevelandSpending, NYSpending)
city <- c(rep("Cleveland", 50), rep("New York", 50))

t.test(spending ~ city, var.equal = TRUE)

# what if we have different variance for the two variables 

t.test(ClevelandSpending, NYSpending, var.equal = FALSE)

var.test(ClevelandSpending, NYSpending)



# Visualization 

library(ggplot2)

#Sample data
data = data.frame(spending = c(ClevelandSpending, NYSpending),
                  state = c(rep("Clevaland", length(ClevelandSpending)), rep("NY", length(NYSpending))))

data = cbind(ClevelandSpending, NYSpending) %>% view()
  

#Plot.
ggplot(data, aes(x = spending, fill = state)) + geom_density(alpha = 0.5)



# Plot weight by group and color by group

library("ggpubr")
ggboxplot(data, x = "state", y = "spending", 
          color = "state", palette = c("#00AFBB", "#E7B800"),
          ylab = "Weight", xlab = "Groups")

