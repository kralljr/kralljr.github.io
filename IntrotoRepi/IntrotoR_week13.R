


# Writing R functions


# - `lm` for linear regression
# - `summary` to get summary statistics of a vector
# - `print` to print results
# 
# But we can also write our own functions.  Why?
# - Something you write in R over and over again
# - Something you want someone else to use easily
# - Makes your code cleaner

# Syntax:
# my_function <- function(argument1, argument2, ...) {
#     things my_function does
# }



# Function to convert from Celsius to Fahrenheit

make_fah <- function(celsius) {
  fahrenheit <- celsius * 1.8  + 32
  return(fahrenheit)
}


# new object in R
make_fah

# Try it out:
make_fah(30)



# Last object is returned
make_fah <- function(celsius) {
  celsius * 1.8  + 32
}

#run the function
make_fah(30)


# Functions can be vectorized.  
cels_temp <- seq(0, 30, length = 10)
cels_temp
make_fah(cels_temp)




# Now for a matrix
cels_temp <- matrix(sample(seq(0, 30), 20), nrow = 4)
rownames(cels_temp) <- paste0("Day", seq(1, 4))
colnames(cels_temp) <- paste0("City", seq(1, 5))
cels_temp
make_fah(cels_temp)



# Syntax:
#    Like in if/then/else statements, indent lines inside of 
#    functions.  Also, spaces before and after assignment operator
#    and before bracket {




# We can also control the output


make_fah <- function(celsius) {
  fahrenheit <- celsius * 1.8  + 32
  cat("The temperature in fahrenheit is", fahrenheit, "\n")
}
make_fah(30)








######
# Creating a function to generate the output we want

# setwd("~/Dropbox/IntrotoREpi/data")
vlbw <- read.csv("vlbw.csv")

head(vlbw)

#OR of dead comparing pneumo to not
ps <- tapply(vlbw$dead, vlbw$pneumo, mean, na.rm = T)
#proportion of dead in pneumo and non-pneumo
p_1 <- ps[2]
p_2 <- ps[1]
#odds of death in pneumo and non-pneumo
odds_1 <- p_1 / (1 - p_1)
odds_2 <- p_2 / (1 - p_2)
odds_1 / odds_2



#In-class exercise 1: Write a function (orfun) to estimate the odds ratio
# from two vectors.  Assume x and y are 0 for unexposed/disease free, 
# 1 for exposed/diseased

orfun <- function(x, y) {
    #OR of dead comparing pneumo to not
    ps <- tapply(y, x, mean, na.rm = T)
    #proportion of dead in pneumo and non-pneumo
    p_1 <- ps[2]
    p_2 <- ps[1]
    #odds of death in pneumo and non-pneumo
    odds_1 <- p_1 / (1 - p_1)
    odds_2 <- p_2 / (1 - p_2)
    odds_1 / odds_2
    
}

orfun(vlbw$pneumo, vlbw$dead)
orfun(vlbw$twn, vlbw$dead)
orfun(vlbw$sex, vlbw$dead)


library(epitools)
epitab(vlbw$dead, vlbw$pneumo)
epitab(vlbw$pneumo, vlbw$dead)
epitab(vlbw$twn, vlbw$dead)






#We can also incorporate if/then/else statements
# - This function outputs a sentence if the input has length 1
# - This function outputs the temperature only if the input has a
#   length greater than 1
# - We should comment our function



# Function to compute temperature in fahrenheit from temperature in 
#  celsius
# celsius is vector or matrix of temperatures in celsius
# Output is either a sentence result or the vector/matrix result
make_fah <- function(celsius) {
    #compute the temperature in celsius
    fahrenheit <- celsius * 1.8  + 32
    
    #if the input has length 1, print sentence
    if(length(celsius) == 1) {
        cat("The temperature in fahrenheit is", fahrenheit, "\n")
        
    #if input has length greater than 1, print temperature only
    }else{
        fahrenheit
    }
}


make_fah(30)
make_fah(cels_temp)





#We can also include more details in the output:
    
make_fah <- function(celsius) {
    #compute the temperature in celsius
    fahrenheit <- celsius * 1.8  + 32
    
    #if the input has length 1, print temp only
    if(length(celsius) == 1) {
        output <- fahrenheit
    #if the input has length greater than 1, print temp and summary
    }else{
        sum_celsius <- summary(celsius)
        sum_fah <- summary(fahrenheit)
        output <- list(fahrenheit = fahrenheit, summary_f = sum_fah, 
          summary_c = sum_celsius)
    }
    
    return(output)
}


make_f_1 <- make_fah(cels_temp)
names(make_f_1)
class(make_f_1)
make_f_1
make_fah(30)








# We can use the `cat` function to print messages along the way
make_fah <- function(celsius) {
    #compute the temperature in celsius
    fahrenheit <- celsius * 1.8  + 32
    
    #if the input has length 1, print temp
    if(length(celsius) == 1) {
        cat("The temperature in fahrenheit is", fahrenheit, "\n")
        output <- fahrenheit
    
    #if the input has length greater than 1, print mean temp
    }else{
        cat("The mean temperature in fahrenheit is", 
	    mean(fahrenheit), "\n")
        sum_celsius <- summary(celsius)
        sum_fah <- summary(fahrenheit)
        output <- list(fahrenheit = fahrenheit, summary_f = sum_fah, 
                       summary_c = sum_celsius)
    }
    
    return(output)
}

mf <- make_fah(cels_temp)
mf





# We can use the `warnings` function to print warnings
# and the message function to print message
make_fah <- function(celsius) {
    #compute the temperature in celsius
    fahrenheit <- celsius * 1.8  + 32
    
    #if the input is outside the extreme temperatures in Atlanta, 
    #  print warning
    if(min(celsius) < -23 | max(celsius) > 41.4) {
        warning("Exceeds current record temperature in Atlanta")
    }
    message("Input length=", length(celsius), "\n")
    
    sum_celsius <- summary(celsius)
    sum_fah <- summary(fahrenheit)
    output <- list(fahrenheit = fahrenheit, summary_f = sum_fah, 
                   summary_c = sum_celsius)
    
    return(output)
}

mf <- make_fah(c(30, 35, 40))
mf
mf <- make_fah(c(-1000, sample(seq(0, 40), 100, replace = T)))
mf



# We can use the `stop` function to stop the function based on some 
#  condition
# For example, stop if encounter an error
make_fah <- function(celsius) {
    #compute the temperature in celsius
    fahrenheit <- celsius * 1.8  + 32
    
    #if the input is outside the extreme temperatures in Atlanta,    
    # stop and print error
    if(min(celsius) < -23 | max(celsius) > 41.4) {
        stop("Exceeds current record temperature in Atlanta")
    }
    message("Input length=", length(celsius), "\n")
    
    sum_celsius <- summary(celsius)
    sum_fah <- summary(fahrenheit)
    output <- list(fahrenheit = fahrenheit, summary_f = sum_fah, 
                   summary_c = sum_celsius)
    
    return(output)
}

mf2 <- make_fah(c(-1000, sample(seq(0, 40), 100, replace = T)))
mf2








# We could write a function to control the output of GLMs
# setwd to data
diab <- read.csv("diabetes.csv")

# load libraries
library(dplyr)
library(ggplot2)

# dat is dataset (e.g. diabetes)
# vars is vector of covariate names (character)
# outcome is outcome (character)
glmOR <- function(dat, vars, outcome) {
    
    # Set up outcome matrix
    or <- matrix(nrow = length(vars), ncol = 3)
    for(i in 1 : length(vars)) {
        # Run univariate regression for variable i
	    coef1 <- glm(dat[, outcome] ~ dat[, vars[i]], data = dat, 
                     family = "binomial")
        # Organiza output
        lbub <- exp(confint(coef1)[2, ])
        or[i, ] <- c(exp(coef1$coef[2]), lbub)
        
    }
    # Add in variable
    or <- data.frame(vars, or)
    # Name output
    colnames(or) <- c("variable", "or", "lb", "ub")
    # Add type to univariate results
    or <- mutate(or, type = "univariate")

    return(or)
}

glmOR(diab, c("chol", "hdl", "weight"), "diab1")
glmOR(diab, c("chol", "hdl", "weight", "waist", "height"), "diab1")


outcome <- "diab1"
vars <- c("chol", "hdl", "weight", "waist", "height")
glmOR(diab, vars, outcome)





# What if the function doesn't run??
traceback()
debug(glmOR)
glmOR(diab, vars, outcome)
# press enter to proceed to next step
# press c to continue through loop
# press Q to quit
undebug(glmOR)





# Plotting function

# ORdat is dataframe of variable, or, lb, ub, type
# outcome is label for outcome of interest
# cols is wanted colors
# size is text size
gfun <- function(ORdat, outcome, cols, size1) {
    
   g1 <- ggplot(data = ORdat, aes(x = type, y = or, colour = type)) +
        # Add ORs and confidence intervals
        geom_point(size = 3, shape = 20) + 
        geom_errorbar(aes(ymin = lb, ymax = ub, colour = type), 
           width = 0) +
        # Change plotting colors
        scale_color_manual(values = cols, name = "") +   
        # Add axis labels
        ylab("Odds ratio") +
        xlab("") +
        # Add title
        ggtitle(paste("Covariates associated with", outcome)) +    
        
        # Get rid of grey background
        theme_bw() +
        
        # Change size of labels
        theme(axis.text.y = element_text(size = size1),
              # Angle x axis labels
              axis.text.x = element_text(size = size1, angle = 20, 
		hjust = 1, 
              vjust = 1), legend.text = element_text(size = size1),
              axis.title = element_text(size = size1),
              # Remove legend
              legend.position = "none", 
              plot.title = element_text(size = size1),
              strip.text = element_text(size = size1)) + 
        # Add horizontal line at 1
        geom_hline(aes(yintercept = 1), colour = "grey50", 
           linetype = "dashed") 
    
    # Add faceting by type with free axes and 2 columns
    g1 + facet_wrap(~ variable, scales = "free", ncol = 2)
}



# First get data
dat1 <- glmOR(diab, vars, outcome)
# Try plot fun
gfun(ORdat = dat1, outcome = "Diabetes", cols = "red", 
     size1 = 18)
gfun(ORdat = dat1, outcome = "diab1", cols = "purple", 
     size1 = 6)








# In-class exercise 2: Revise the previous function to include an 
#  argument for the horizontal line color.
gfun <- function(ORdat, outcome, cols, size1, colhline) {
    
    g1 <- ggplot(data = ORdat, aes(x = type, y = or, colour = type)) +
        # Add ORs and confidence intervals
        geom_point(size = 3, shape = 20) + 
        geom_errorbar(aes(ymin = lb, ymax = ub, colour = type), 
                      width = 0) +
        # Change plotting colors
        scale_color_manual(values = cols, name = "") +   
        # Add axis labels
        ylab("Odds ratio") +
        xlab("") +
        # Add title
        ggtitle(paste("Covariates associated with", outcome)) +    
        
        # Get rid of grey background
        theme_bw() +
        
        # Change size of labels
        theme(axis.text.y = element_text(size = size1),
              # Angle x axis labels
              axis.text.x = element_text(size = size1, angle = 20, 
                                         hjust = 1, 
                                         vjust = 1), legend.text = element_text(size = size1),
              axis.title = element_text(size = size1),
              # Remove legend
              legend.position = "none", 
              plot.title = element_text(size = size1),
              strip.text = element_text(size = size1)) + 
        # Add horizontal line at 1
        geom_hline(aes(yintercept = 1), colour = colhline, 
                   linetype = "dashed") 
    
    # Add faceting by type with free axes and 2 columns
    g1 + facet_wrap(~ variable, scales = "free", ncol = 2)
}


gfun(ORdat = dat1, outcome = "diab1", cols = "purple", 
     size1 = 6, colhline = "orange")














# plot defaults
gfun(dat1, "Diabetes")


#########
# Plot defaults
gfun <- function(ORdat, outcome, cols = c("purple", "orange"), size1 = 18) {
    
    g1 <- ggplot(data = ORdat, aes(x = type, y = or, colour = type)) +
        # Add ORs and confidence intervals
        geom_point(size = 3, shape = 20) + 
        geom_errorbar(aes(ymin = lb, ymax = ub, colour = type), width = 0) +
        # Change plotting colors
        scale_color_manual(values = cols, name = "") +   
        # Add axis labels
        ylab("Odds ratio") +
        xlab("") +
        # Add title
        ggtitle(paste("Covariates associated with", outcome)) +    
        
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
    g1 + facet_wrap(~ variable, scales = "free", ncol = 2)
}


gfun(dat1, "Diabetes", cols = "red")





# Customizing through arguments
gfun <- function(ORdat, outcome, cols = c("purple", "orange"), size1 = 18, 
  legend = F) {
    
    g1 <- ggplot(data = ORdat, aes(x = type, y = or, colour = type)) +
        # Add ORs and confidence intervals
        geom_point(size = 3, shape = 20) + 
        geom_errorbar(aes(ymin = lb, ymax = ub, colour = type), width = 0) +
        # Change plotting colors
        scale_color_manual(values = cols, name = "") +   
        # Add axis labels
        ylab("Odds ratio") +
        xlab("") +
        # Add title
        ggtitle(paste("Covariates associated with", outcome)) +    
        
        # Get rid of grey background
        theme_bw() +
        
        # Change size of labels
        theme(axis.text.y = element_text(size = size1),
              # Angle x axis labels
              axis.text.x = element_text(size = size1, angle = 20, hjust = 1, 
               vjust = 1), legend.text = element_text(size = size1),
              axis.title = element_text(size = size1),
              plot.title = element_text(size = size1),
              strip.text = element_text(size = size1)) + 
        # Add horizontal line at 1
        geom_hline(aes(yintercept = 1), colour = "grey50", linetype = "dashed") 
    
    # if legend = F, remove legend
    if(!legend) {
        # Remove legend
        g1 <- g1 + theme(legend.position = "none")
    }
    
    # Add faceting by type with free axes and 2 columns
    g1 + facet_wrap(~ variable, scales = "free", ncol = 2)
}



gfun(dat1, "Diabetes")
gfun(dat1, "Diabetes", legend = T)
