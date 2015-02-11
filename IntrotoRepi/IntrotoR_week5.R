# Lecture 5

# Read in data
# Data: http://biostat.mc.vanderbilt.edu/wiki/pub/Main/DataSets/vlbw.html
vlbw <- read.csv("vlbw.csv", stringsAsFactors = F)
# Subset data to be more manageable for class
vlbw <- vlbw[complete.cases(vlbw), c("bwt", "gest", "sex", "twn", "pneumo")]




#####################
# One sample t-tests
#####################
# Graphically display age:
hist(vlbw$gest, xlab = "Gestational age", main = "Histogram of 
  gestational age")

# Perform a t-test for
# H0: mu = 39
# H1: mu != 39
# at alpha = 0.05
t_age <- t.test(x = vlbw$gest, mu = 39)
#Print results
t_age
# Check names
names(t_age)
# Extract p-value
pval <- t_age$p.value





#####################
# Two sample t-tests
#####################
# Does mean gestational age differ between male and female low birthweight infants?
# First, subset gestation by male and female
age_female <- vlbw$gest[vlbw$sex == "female"]
age_male <- vlbw$gest[vlbw$sex == "male"]
# Perform two sample t-test for
# H0: mu1 = mu2
# H1: mu1 != mu2
t_agesex <- t.test(age_female, age_male)


# In-class question:
# Perform a t-test for
# H0: mu1 = mu2
# H1: mu1 < mu2




#####################
# One sample test of proportion
#####################
# First make a table of values
table_pneumo <- table(vlbw$pneumo)
table_pneumo
table_pneumo <- matrix(c(127, 518), ncol = 2)

#Is the proportion of pneumothorax in low birthweight infants different than 
# 6.3%?
prop.test(table_pneumo, p = 0.063)





#####################
# Two sample test of proportion
#####################
# Get a table of twin status by pneumothorax
table_pneumo <- table(twin = vlbw$twn, pneumo = vlbw$pneumo)
table_pneumo
# Reformat results
table_pneumo <- matrix(c(95, 32, 415, 102), ncol = 2)
colnames(table_pneumo) <- c("Pneumo", "No pneumo")
rownames(table_pneumo) <- c("Not twin", "Twin")


# In-class question:
# Test whether the proportion of pneumothorax differs between twins and singleton
  #births






#####################
# Chi-squared test
#####################
# Is sex associated with being a twin in low birthweight infants?
chsq_surgery <- chisq.test(vlbw$sex, vlbw$twn)

# In-class question:
# Extract out the p-value from the chi-squared test








#####################
# Relative risk/ risk ratio and odds ratio
#####################
epi_pneumo <- epitab(table_pneumo, method = "riskratio")
epi_pneumo


# In-class question:
# Extract out the odds ratio and its corresponding confidence interval









#####################
# Power calculations
#####################
# Compute power when 
# - The difference in means is 0.1
# - The standard deviation is 1
# - We want 90% power
power.t.test(delta = 0.1, power = 0.9, type = "two.sample", alternative = 
  "two.sided")


# In-class question:
#What would our power to detect a difference between two means if
#- The difference in means is 15
#- The standard deviation is 10
#- The sample size in each group is 10
