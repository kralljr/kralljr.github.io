#  Week 7: linear regression

# Set working directory
#setwd("~/Dropbox/IntrotoREpi/data/")

# Load libraries
library(dplyr)


###########################
# Data cleaning

# Read in data
ga_crime <- read.csv("georgia_crime.csv", skip = 5, stringsAsFactors = F)
head(ga_crime)
tail(ga_crime)

# Clean data
ga_crime <- filter(ga_crime, State != "")
ga_crime <- select(ga_crime, -X)
ga_crime <- filter(ga_crime, Murder.and.nonnegligent.manslaughter.rate > 0)
ga_crime <- filter(ga_crime, Population <= 150000)

# Look at the data
glimpse(ga_crime)

#Rename variables for easier calling
ga_crime <- rename(ga_crime, 
  murder = Murder.and.nonnegligent.manslaughter.rate, pop = Population, 
  robbery = Robbery.rate)
glimpse(ga_crime)









###########################
# Exploratory data analysis
#install.packages("epiR")
library(epiR)
summary(ga_crime$murder)
epi.descriptives(ga_crime$murder)
epi.descriptives(ga_crime$pop)

#Any missing data?
table(complete.cases(ga_crime))
head(complete.cases(ga_crime))




#Exploratory data analysis

# Check normality assumption of outcome (murder rate)
hist(ga_crime$murder, xlab = "Murder rate per 100,000",
  main = "Histogram of murder rate")

# Check linearity assumption
lpop1 <- lowess(ga_crime$pop, ga_crime$murder)
plot(ga_crime$pop, ga_crime$murder, xlab = "Population", 
  ylab = "Murder rate (per 100,000)", 
  main = "Murder rate vs. population")
lines(x = lpop1$x, y = lpop1$y, col = "red")

# Transform murder rate and population rate
ga_crime <- mutate(ga_crime, lmurder = log(murder), lpop = log(pop))


# Re-check assumptions
# Re-check normality assumption

hist(ga_crime$lmurder, xlab = "Log murder rate per 100,000",
     main = "Histogram of log murder rate")

# Re-check linearity
lpop1 <- lowess(ga_crime$lpop, ga_crime$lmurder)
plot(ga_crime$lpop, ga_crime$lmurder, xlab = "Log population", 
  ylab = "Log murder rate", main = "Log murder rate vs. log population")
lines(x = lpop1$x, y = lpop1$y, col = "red")









###########################
# Simple linear regression

# To fit a linear model, we use the `lm` command.
#- `y ~ x` is the formula where `y` is the outcome and `x` is the predictor
#- `data` specifies which dataset you want R to use
#- Could also write `lm(dataset$y ~ dataset$x)`
#lm(y ~ x, data = dataset)


# Do Georgia agencies that serve a larger population have a higher murder rate?
lm_murder <- lm(lmurder ~ lpop, data = ga_crime)
lm_murder

# The resulting object from a `lm` call is of the class `lm`
class(lm_murder)
names(lm_murder)
lm_murder$coefficients

#We can use `summary(lm)` to obtain more information about the regression
s_lm_murder <- summary(lm_murder)
s_lm_murder
names(s_lm_murder)
s_lm_murder$coef


#- The slope of -0.70 means that log murder rate decreases by -0.70 when log 
#population increases by 1.
# - The p-value corresponding to the slope is $<0.05$, therefore we reject the 
# null hypothesis that the change in log murder rate when the log population 
# changes is different than 0.  In other words, we found that in 2012, 
# log population and log murder rate are associated in Georgia agencies.


# The intercept is 9.35, which is interpreted as the log murder rate when 
# population is zero.

# The p-value corresponding to the intercept is $<0.05$, therefore we reject 
# the null hypothesis that the mean log murder rate is 0 when the log population is 0.

#- Often, not a reasonable test to perform
#- Smallest population observed is 10,388 people (Jesup City), 
#which corresponds to 9.2 in log population.
ga_crime[which.min(ga_crime$pop), c("Agency", "pop", "lpop")]



#There is a lot of information contained in the `summary(lm)` output
names(s_lm_murder)
# Residual standard deviation
s_lm_murder$sigma


# In-class exercise 1:
# Find the r-squared for the regression of log murder rate on log population
s_lm_murder$r.squared
s_lm_murder$r.s
summary(lm_murder)$r.squared











###########################
# Model checking

# Histogram of residuals
hist(lm_murder$resid, main = "Histogram of residuals", xlab = "Residuals")

# Residuals vs. predictor (population)
scatter.smooth(ga_crime$lpop, lm_murder$resid, main = "Residuals vs. log population", 
               xlab = "log Population")
abline(h = 0, col = "red")


# Same scatterplot as earlier
lpop1 <- lowess(ga_crime$lpop, ga_crime$lmurder)
plot(ga_crime$lpop, ga_crime$lmurder, xlab = "Log population", 
  ylab = "Log murder rate", main = "Log murder rate vs. log population")
lines(lpop1, col = "red")

#- Add the line of best fit
abline(a = lm_murder$coef[1], b = lm_murder$coef[2], col = "blue")

#If we plot an `lm` object using `plot(lm)`, we can get some useful information
plot(lm_murder)







###########################
#Multiple linear regression


# Is log population associated with log murder rate, controlling for 
# robbery rate?

# Look at scatter plots with robbery rate
scatter.smooth(ga_crime$lpop, ga_crime$rob, xlab = "Log population", 
  ylab = "Robbery rate (per 100,000)", main = "Robbery rate vs. log population", 
  lpars = list(col = "red"))
scatter.smooth(ga_crime$rob, ga_crime$lmurder, 
  xlab = "Robbery rate (per 100,000)", ylab = "Log murder rate", 
  main = "Log murder rate vs. robbery rate", lpars = list(col = "red"))



# Is population associated with murder rate, after controlling for 
# robbery rate?
lm_murder <- lm(lmurder ~ lpop + robbery, data = ga_crime)
lm_murder


# In-class exercise 2:
# Extract the estimated coefficients, standard errors, and p-values
# from the multiple linear regression
lm_murder$coef
summary(lm_murder)$coef[, c(1, 2, 4)]
scoef <- summary(lm_murder)$coef
scoef <- data.frame(scoef)
select(scoef, Estimate)









###########################
# Interpreting coefficients

#(see sides)











###########################
# Multiple linear regression using categorized population
ga_crime <- mutate(ga_crime, popcat = ifelse(pop < 15000, "small", 
    ifelse(between(pop, 15000,  30000), "medium", "large")))
table(ga_crime$popcat)


# Plot data
scatter.smooth(ga_crime$pop, ga_crime$lmurder, xlab = "Population (2012)", 
  ylab = "Log murder rate", 
  main = "Log murder rate vs. population in GA in 2012", 
  lpars = list(col = "red"))
abline(v = 15000, col = "blue")
abline(v = 30000, col = "blue")

# good cutoffs on the log scale
scatter.smooth(ga_crime$lpop, ga_crime$lmurder, xlab = "Log population", 
  ylab = "Log murder rate", 
  main = "Log murder rate vs. log population in GA in 2012", 
  lpars = list(col = "red"))
abline(v = log(15000), col = "blue")
abline(v = log(30000), col = "blue")





# What type of variable is `popcat`?
class(ga_crime$popcat)
#In `lm`, R automatically translates string or character variables into factors 
lm_murder <- lm(lmurder ~ popcat, data = ga_crime)
lm_murder

summary(lm_murder)$coef


# In class exercise 3:  Interpreting regression output
#- What is the reference category?
#- What is `popcatmedium`?
#- What is `popcatsmall`?
#- What is the mean log murder rate for large agencies?
#- What is the mean log murder rate for small agencies?

lm_murder$coef[1] + lm_murder$coef[3]







#Changing the reference category
# What are the levels?
ga_crime$popcat <- factor(ga_crime$popcat)
class(ga_crime$popcat)
levels(ga_crime$popcat)
# Change the levels
ga_crime$popcat <- factor(ga_crime$popcat, levels = c("small", "medium",
  "large"))
lm_murder <- lm(lmurder ~ popcat, data = ga_crime)
lm_murder


# In-class exercise 4: Rerun the above regression, changing the reference 
# category to medium
ga_crime$popcat <- factor(ga_crime$popcat, levels = c("medium", "small", 
   "large"))
lm_murder <- lm(lmurder ~ popcat, data = ga_crime)
lm_murder









###########################
# Contrasts

#Contrasts can be used to determine sums of regression output.
#- What is the mean log murder rate for medium agencies?
#- What is the difference in log murder rate between medium and small agencies?
#We can use the `contrast` function in the `contrast` package to get the 
# mean in each group.

#contrast(lm object, a = list(name of variable, all levels of variable))

#install.packages("contrast")
library(contrast)
levels(ga_crime$popcat)
contrast(lm_murder, a = list(popcat = levels(ga_crime$popcat)))

# using lm results
lm_murder$coef
lm_murder$coef[1] + lm_murder$coef[2]
lm_murder$coef[1] + lm_murder$coef[3]



#Suppose we want to test to see whether medium or large cities differ 
#in murder rates compared with small cities.

# Sample code
#contrast(lm object, a = list(name of variable, all levels of variable),
#b = list(name of variable, reference category))

contrast(lm_murder, a = list(popcat = c("small", "medium", "large")), 
  b = list(popcat = c("small")))

# using lm results
lm_murder$coef
lm_murder$coef[1] - (lm_murder$coef[1] + lm_murder$coef[2])
(lm_murder$coef[1] + lm_murder$coef[3]) - (lm_murder$coef[1] + lm_murder$coef[2])
















###########################
#Analysis of Variance (ANOVA)

#Because ANOVA is the same as fitting a multiple linear regression model 
#with one categorical predictor, we use the `anova` function on an `lm` object.
# Sample code:
# anova(linear regression model)
lm_murder <- lm(lmurder ~ popcat, data = ga_crime)
anova_murder <- anova(lm_murder)
anova_murder
names(anova_murder)




#P-value for global F-test
pval <- anova_murder$Pr[1]
pval


