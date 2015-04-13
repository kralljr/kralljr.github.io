# Solution to HW3



#####
# Part 1.

## @knitr plot1

# Load libraries
library(dplyr)
library(ggplot2)

# Read in data
#s_samp <- read.csv("support_sample.csv")
# create plot
support <- read.csv("support2.csv", stringsAsFactors = F)
support <- mutate(support, lcharge = log(charges))
g1 <- ggplot(data = support, aes(x = age, y = lcharge)) +
    # Add points
    geom_point() + stat_smooth() + xlab("Age (years)") +
    ylab("Charges (log dollars)") + 
    ggtitle("Association between hospital charges and age")

g1





#####
# Part 2


# Read in support dataset
## @knitr plot2
support <- read.csv("support2.csv", stringsAsFactors = F)


# Load libraries
library(dplyr)
library(ggplot2)



# 2a. Estimate univariate odds ratios

# Which variables
vars <- c("age", "sex", "edu", "scoma", "num.co", "diabetes")
# Setup output
uOR <- matrix(nrow = length(vars), ncol = 3)

# For each variable
for(i in 1 : length(vars)) {
    # Get variable from support data
    var1 <- support[, vars[i]]
    
    # Run simple regression
    glm1 <- glm(hospdead ~ var1, data = support, family = "binomial")
    
    # Get odds ratio
    or <- exp(glm1$coef[2])
    
    # Get 95% CI for odds ratio
    confint1 <- exp(confint(glm1)[2, ])
    
    # Save output
    uOR[i, ] <- c(or, confint1)
}




# 2b. Estimate multivariate odds ratios

# create equation
equation1 <- paste("hospdead ~", paste(vars, collapse = " + "))
# regression model
glmm <- glm(eval(equation1), data = support, family = "binomial")
# find OR
mOR <- exp(glmm$coef[-1])
# CI
cimOR <- exp(confint(glmm)[-1, ])
mOR <- cbind(mOR, cimOR)


# 2c. Create data frame

# Clean mOR
mOR <- data.frame(mOR)
# Add variables and Type
mOR <- mutate(mOR, Variable = vars, Type = "Multivariate")
# Rename columns
mOR <- rename(mOR, OR = mOR, LB = X2.5.., UB = X97.5..)


# Clean uOR
colnames(uOR) <- c("OR", "LB", "UB")
uOR <- data.frame(uOR)
# Add variables and Type
uOR <- mutate(uOR, Variable = vars, Type = "Univariate")


# Join together
death_OR <- full_join(uOR, mOR)


# Re-label variable
death_OR$Variable <- factor(death_OR$Variable, levels = vars, 
  labels = c("Age", "Male sex", "Education", "Coma score", "Comorbidities", 
  "Diabetes"))
# Re-order Type
death_OR$Type <- factor(death_OR$Type, levels = c("Univariate", "Multivariate"))


# Part 3
# Specify colors
library(ggplot2)
cols <- c("slateblue", "darkred")
size1 <- 18

# Plot
g1 <- ggplot(data = death_OR, aes(x = Type, y = OR, colour = Type)) +
  # Add ORs and confidence intervals
  geom_point(size = 3, shape = 20) + 
  geom_errorbar(aes(ymin = LB, ymax = UB, colour = Type), width = 0) +
  # Change plotting colors
  scale_color_manual(values = cols, name = "") +   
  # Add axis labels
  ylab("Odds ratio") +
  xlab("") +
  # Add title
  ggtitle("Covariates associated with in-hospital death") +    
    
  # Get rid of grey background
  theme_bw() +
    
  # Change size of labels
  theme(axis.text.y = element_text(size = size1),
    # Angle x axis labels
    axis.text.x = element_text(size = size1, angle = 20, hjust = 1, 
    vjust = 1), legend.text = element_text(size = size1),
    axis.title = element_text(size = size1),
    # Remove legend
    legend.position = "none", 
    plot.title = element_text(size = size1),
    strip.text = element_text(size = size1)) + 
  # Add horizontal line at 1
  geom_hline(aes(yintercept = 1), colour = "grey50", linetype = "dashed") 
# Add faceting by type with free axes and 2 columns
g1 + facet_wrap(~ Variable, scales = "free", ncol = 2)



