####
####
# Jenna Krall, PhD
# Homework 1
# 560R
####
####

# Question 1
# Part 1

# COMMENT OUT OR REMOVE SETWD
# setwd("~/Dropbox/IntrotoRepi")

# First open dataset
data(iris)

# Part 2

# Find mean sepal length
m_slength <- mean(iris$Sepal.Length)

# Part 3

# Create new variable
sepal_g4 <- vector(length = length(iris$Sepal.Length))
sepal_g4[iris$Sepal.Length > 4] <- TRUE
sepal_g4[iris$Sepal.Length <= 4] <- FALSE

# Part 4

# Make a table of new variable
table_g4 <- table(sepal_g4)

# Part 5

# Make a plot
plot(Sepal.Width ~ Sepal.Length, data = iris, main = "Sepal width vs. 
  Sepal length", xlab = "Sepal length", ylab = "Sepal width")
