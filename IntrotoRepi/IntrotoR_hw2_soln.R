### 
## @knitr table1
#setwd("~/Dropbox/IntrotoREpi/homework/homework2/")
library(dplyr)

# Part 1
#1
# Define the age of the kid
# age_kid <- 5
age_kid <- round(age_kid)
# Between 1-2
if(age_kid <= 2 & age_kid >=1) {
    sleep_duration <- "11-14 hours"
# Between 3 and 5
}else if(age_kid <= 5 & age_kid >= 3) {
    sleep_duration <- "10-13 hours"
# Between 6 and 13
}else if(age_kid <= 13 & age_kid >= 6) {
    sleep_duration <- "9-11 hours"
# Catchall
}else{
    sleep_duration <- "not defined"
}


# # Part 2
#1. Read in the data for males and females
diaf <- read.csv("diabetes_female.csv", stringsAsFactors = F)
diam <- read.csv("diabetes_male.csv", stringsAsFactors = F)


#2. Merge together the datasets
diabetes <- full_join(diaf, diam)


#3. Select columns
diab_sel <- select(diabetes, chol, hdl, age, height, weight)
# Use apply to compute means
total_means <- apply(diab_sel, 2, mean, na.rm = T)


#4. Use mutate to add diabetes variable
# Create diabetes variable
diab <- vector(length = nrow(diabetes))
diab <- ifelse(diabetes$glyhb > 7, 1, 0)
# Add diabetes
diabetes <- mutate(diabetes, diab1 = diab)


#5. Create grouped dataset
diab_grouped <- group_by(diabetes, diab1)


#6. Compute means for each variable
s1 <- summarise_each(diab_grouped, funs(mean(., na.rm = T)), chol, hdl, age,
  height, weight)
# Reformat table
s1 <- s1[-3, ]
s1 <- t(s1)
# Rename columns
colnames(s1) <- c("Mean_not_diabetic", "Mean_diabetic")
s1 <- s1[-1, ]
s1 <- data.frame(s1)
# Add new variable
s1 <- mutate(s1, Variable = rownames(s1))


#7. Compute length for each variable
l1 <- summarise_each(diab_grouped, funs(length(which(!is.na(.)))), chol,
  hdl, age, height, weight)
# Reformat table
l1 <- l1[-3, ]
l1 <- t(l1)
# Rename columns
colnames(l1) <- c("N_not_diabetic", "N_diabetic")
l1 <- l1[-1, ]
l1 <- data.frame(l1)


#7. Add total means, and sample sizes
s1 <- mutate(s1, Mean = total_means, N_not_diabetic = l1$N_not_diabetic, 
  N_diabetic = l1$N_diabetic)


#8. Use a for loop for t.test
vars1 <- c("chol", "hdl", "age", "height", "weight")

#Which diabetics/not
wh1 <- which(diabetes$diab1 == 1)
wh0 <- which(diabetes$diab1 == 0)

#Initialize output
lb <- vector()
ub <- vector()
pval <- vector()
Meandiff <- vector()

# For each variable
for(i in 1 : 5) {
	# Compute t.test
    temp <- t.test(diabetes[wh1, vars1[i]], diabetes[wh0, vars1[i]])
    # Get means
    m1 <- mean(diabetes[wh1, vars1[i]], na.rm = T)
    m2 <- mean(diabetes[wh0, vars1[i]], na.rm = T)
    
   	# Get mean difference
    Meandiff[i] <- m1 - m2
    # Get confidence interval
    lb[i] <- temp$conf.int[1]
    ub[i] <- temp$conf.int[2]
    # p-value
    pval[i] <- temp$p.value
}

# Add new variables
s1 <- mutate(s1, Meandiff = Meandiff, Meandiff_LB = lb, Meandiff_UB = ub,  
  Meandiff_Pval = pval)
# Rearrange final table
tab_diab <- s1[, c(3, 4, 6, 2, 5, 1, 7 : 10)]




###
# For pdf
# mean_diff2 <- tab_diab
# for(i in 2 : ncol(mean_diff2)) {
	# mean_diff2[, i] <- "--"
# }
# mean_diff2
