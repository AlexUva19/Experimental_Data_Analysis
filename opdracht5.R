# 
# Experimental Data Analysis exercise 5 
# Question 5

# A

# The data is not paired because two measures come from two different experimental units 
chickens <- chickwts

chickens_meat <- chickens[49:59, 1]
chickens_sunflower <- chickens[37:48, 1]
hist(chickens_meat); hist(chickens_sunflower); boxplot(chickens_meat,chickens_sunflower)


test_1 <- t.test(chickens_meat,chickens_sunflower)
# print(test_1) -> result of paired t is statistically significant difference in the means

test_2 <- wilcox.test(chickens_meat,chickens_sunflower)
# print(test_2) -> result of mann whitnet is no statical significant difference in the medians

test_3 <-  ks.test(chickens_meat,chickens_sunflower)
# print(test_3) -> according to this two sided test, the means are different based on the vertical distance


# B

lm <- lm(weight ~ as.factor(feed), data = chickens)
summary(lm)
anova(lm)

# From the summary and anova we can see that there is a significant influence of types of food on chicken weights

# the summary show that: (note it is in alphabetic order)
# these are the expectations ? better way to extract these
casein <- 323.583
horsebean <- -163.383 + 323.583
linsead <- -104.833 +  323.583
meatmeal <- -46.674 +  323.583
soybean <- -77.155 + 323.583
sunflower <- -5.333  + 323.583


# C
# we do not have many data points so we combine them into one plot
# it looks like the errors are normally distributed
# put them in a dataframe and check the variances

par(mfrow=c(1,1)); qqnorm(residuals(lm))


#D 

attach(chickens) ; kruskal.test(weight ,feed)
anova(lm)

# It seems that both methods yield no real difference in the results. This is probably because 
# the sample are points are normally distributed and have the same variance (can be seen from qq plot)







# For making it nicer: put everything in a datafrmae Create list for all different kinds of food, make dataframe of them later 
# 
# chickens_meat <- chickens[49:59, 1]
# chickens_sunflower <- chickens[37:48, 1]
# chickens_horsebean <- chickens[1:10, 1]
# chickens_horsebean <- chickens[11:22, 1]
# chickens_horsebean <- chickens[23:36, 1]
# chickens_meatmeal <- chickens[49:59, 1]
# chickens_casein <- chickens[60:71, 1]
# 
# mean(chickens_meat)
# mean(chickens_sunflower)



