#  Week 7: Regression extentions

# Set working directory
#setwd("~/Dropbox/IntrotoREpi/data/")

# Load libraries
library(dplyr)


# Read in data from week 5 on lecture site
vlbw <- read.csv("vlbw.csv")









######################
# Exploratory data analysis

# Histogram of birthweight
hist(vlbw$bwt, main = "Histogram of birthweight", xlab = "Birthweight (grams)")

# Scatterplot of gesation and birthweight
scatter.smooth(vlbw$gest, vlbw$bwt, xlab = "Gestation (weeks)", 
  ylab = "Birthweight (grams)", main = "Birthweight vs. Gestation")

# Is gestational age associated with birthweight
# Review from last class
lm_gest <- lm(bwt ~ gest, data = vlbw)
summary(lm_gest)$coef

# Is infant status (maybe a proxy for health) associated with birthweight?
lm_dead <- lm(bwt ~ dead, data = vlbw)
summary(lm_dead)$coef

# Does the association between gestation and birthweight differ by survival?
# - Were infants who died smaller in birthweight for their gestational age

# Two datasets; dead, alive
vlbw_d <- filter(vlbw, dead == 1)
vlbw_a <- filter(vlbw, dead == 0)

# look at the data
plot(vlbw_d$gest, vlbw_d$bwt, col = "red", xlab = "Gestation (weeks)", 
     ylab = "Birthweight (grams)", main = "Birthweight vs. Gestation",
     ylim = c(380, 1600))
points(vlbw_a$gest, vlbw_a$bwt, col = "blue")
legend("topleft", legend = c("Dead", "Alive"), pch = 1, col = c("red", "blue"))












######################
# Interactions


# Add interactions to see if association differs
# - Use * for interactions and main effects
# - One categorical variable, one continuous covariate is same as
# ANalysis of COVariance
lm_gest_d <- lm(bwt ~ gest * dead, data = vlbw)
lm_gest_d 
anova(lm_gest_d)

# Only interaction, no main effects
lm(bwt ~ gest : dead, data = vlbw)



# In-class exercise 1:
# Does the association between gestational age and birthweight differ
# by alive status? 
summary(lm_gest_d )$coef[4, 4]




# Does the association between survival status and birthweight differ by 
# pneumothorax?
# - Two categorical covariates
# - Same as two-way ANOVA
summary(lm(bwt ~ pneumo + dead, data = vlbw))
lm_pneu_d <- lm(bwt ~ pneumo * dead, data = vlbw)
lm_pneu_d
anova(lm_pneu_d)











######################
# Splines
# Should we be fitting two lines?
scatter.smooth(vlbw$gest, vlbw$bwt, xlab = "Gestation (weeks)", 
  ylab = "Birthweight (grams)", main = "Birthweight vs. Gestation")
abline(v = 31, col = "red")


# Create a new variable
vlbw <- mutate(vlbw, spl_gest = ifelse(gest >= 31, gest - 31, 0))
vlbw_spl <- lm(bwt ~ gest + spl_gest, data = vlbw)
summary(vlbw_spl)


plot(vlbw$gest, vlbw$bwt, xlab = "Gestation (weeks)", 
    ylab = "Birthweight (grams)", main = "Birthweight vs. Gestation")
coef1 <- vlbw_spl$coef
# Add line before 31
abline(a = coef1[1], b = coef1[2], col = "blue")
# compute new intercept
y_break <- coef1[1] + 31 * coef1[2]
new_int <- y_break -  31 * (coef1[2] + coef1[3])
# Add line after 31
abline(a = new_int, b = coef1[2] + coef1[3], col = "green")



# In-class exercise 2:
# How much more variability in birthweight is explained by the spline model
# compared to the model with only gestational age?
s1 <- summary(vlbw_spl)
names(s1)
s1$r.squared
summary(lm_gest)$r.squared




# Revisiting earlier interaction between gestation and status
# After accounting for non-linear trend in gestation, no association!
lm_gest_spl2 <- lm(bwt ~ gest * dead + dead * spl_gest, data = vlbw)
summary(lm_gest_spl2)$coef










######################
# Logistic regression
# Is gestational age associated with survival status?
glm_d <- glm(dead ~ gest, data = vlbw, family = "binomial")
glm_d
summary(glm_d)$coef

# The log odds of death associated with a one week increase in gestational 
# age is -0.72, 
# The log odds ratio of death comparing infants who differ by one week 
# in gesational age is -0.72

exp(log(5))

exp(glm_d$coef[2])
# The odds ratio of death comparing infants who differ by one week 
# in gestational age is 0.49.


# glm can also be used to fit linear regression
lm_gest
glm(bwt ~ gest, data = vlbw)



# In-class exercise 3:
# What is the odds ratio of death comparing infants with pneumothorax to 
# infants without pneumothorax adjusted for birthweight and gestational age?
g1 <- glm(dead ~ pneumo + bwt + gest, data = vlbw, family = "binomial")
exp(g1$coef[2])
summary(g1)$coef

#Log-linear or Poisson models
# glm(y ~ x, data = dat, family = "poisson")


######################
# Survival data in R


###
# Survival data and Kaplan-Meier curves
# Load package and data
library(survival)
data(ovarian)
# Create survival data
surv_cancer <- Surv(ovarian$futime, ovarian$fustat)
# Plot Kaplan-Meier curves for two chemotherapy treatments
plot(survfit(surv_cancer ~ ovarian$rx), 
     main = "Kaplan-Meier Survival curves", 
     xlab = "Days", ylab = "Proportion surviving", 
     col = c(1, 2))
legend(c("topright"), legend = c("Trt 1", "Trt 2"), 
       col= c(1, 2), lty = 1)



###
# Hunger Games survival
# http://www.bdkeller.com/writing/hunger-games-survival-analysis/
# "Hunger games survival analysis: Do "career" tributes survive longer?
hunger <- read.csv("hungergames.csv", stringsAsFactors = F)
surv_hunger <- Surv(time = hunger$survival_days,event = rep(1, nrow(hunger)))
plot(survfit(surv_hunger ~ hunger$career), 
     main = "74th annual Hunger Games - survival estimates", 
     xlab = "Days", ylab = "Proportion surviving", col = c(1, 2))
legend(c("topright"), legend = c("Career tribute", "Not career tribute"), 
       col= c(1, 2), lty = 1)




###
# Cox proportional hazards model
# Is treatment group associated with survival in ovarian cancer patients?
head(ovarian)
ovarian$rx <- factor(ovarian$rx, levels = c(1, 2), labels = c("Trt1", "Trt2"))
coxph(surv_cancer ~ rx, data = ovarian)


