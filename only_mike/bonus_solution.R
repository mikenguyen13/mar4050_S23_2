var.test(satisfaction ~ gender, df, alternative = "two.sided")

# p = 0.1964 which is greater than 0.05, there is no difference in the two samples' variances

t.test(satisfaction ~ gender, df )

# p is signficant, there is a difference

var.test(satisfaction ~ social_media, df)

# no difference in F-test

t.test(satisfaction ~ social_media, df)

# there is a difference in t-test