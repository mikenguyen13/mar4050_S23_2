# Advertising causes sales to increase
set.seed(1)
ad = rnorm(100, mean = 0, sd = 1)
sales = ad*0.7 + rnorm(100, mean = 0, sd = 1)
cor(ad, sales)
plot(ad, sales)

# Sales causes the advertising to increase (reverse causation)
set.seed(1)
sales = rnorm(100, mean = 0 , sd = 1)
ad = sales*0.7 + rnorm(100, mean = 0, sd = 1)
cor(ad, sales)
plot(ad, sales)


# Outdoor temp causes both an increase in advertising and sales. Hence, spurious correlation
set.seed(1)
outdoor = rnorm(100, mean = 75, sd = 5)

ad = outdoor*0.3 + rnorm(100, mean = 0, sd = 1)

sales = outdoor*0.2 + ad*0.01 +  rnorm(100, mean = 0, sd = 1)

cor(sales, ad)

plot(sales, ad)