# Comment out working directory
#setwd("~/Dropbox/IntrotoRepi/homework/homework1")

# Part 1:
#####
#a
# Read data into R
# Use stringsAsFactors = F if data are messy
fev <- read.csv("FEV.csv", stringsAsFactors = F)




#####
#b. 
# Find number of rows and columns and name vector
info_fev <- c(nrow(fev), ncol(fev))
names(info_fev) <- c("Nobs", "Nvar")





#####
#c. 
# Initialize output matrix with row and column names 
summ_df <- matrix(nrow = 3, ncol = 6)
colnames(summ_df) <- c("Nobs", "Min", "Max", "Mean", "SD", "Nmiss")
rownames(summ_df) <- c("age", "FEV", "height")

# For each column get relevent informatiion
summ_df[, "Nmiss"] <- c(length(which(is.na(fev$age))), 
  length(which(is.na(fev$fev))), length(which(is.na(fev$height))))
summ_df[, "Nobs"] <- c(length(which(!is.na(fev$age))), 
  length(which(!is.na(fev$fev))), length(which(!is.na(fev$height))))
summ_df[, "Min"] <- round(c(min(fev$age), min(fev$fev), min(fev$height)), 2)
summ_df[, "Max"] <- round(c(max(fev$age), max(fev$fev), max(fev$height)), 2)
summ_df[, "Mean"] <- round(c(mean(fev$age), mean(fev$fev), mean(fev$height)), 
  2)
summ_df[, "SD"] <- round(c(sd(fev$age), sd(fev$fev), sd(fev$height)), 2)

# Change to data.frame so can have different data types
summ_df <- data.frame(summ_df)
# Ensure that counts are integers
summ_df[, "Nmiss"] <- as.integer(summ_df[, "Nmiss"])
summ_df[, "Nobs"] <- as.integer(summ_df[, "Nobs"])





#####
#d
# Create a table of smokers by sex
tab_sex_smoke <- table(fev$sex, fev$smoke)

# Compute proportions
sums_smoke <- table(fev$sex)
tab_sex_smoke[, 1] <- tab_sex_smoke[, 1] / sums_smoke
tab_sex_smoke[, 2] <- tab_sex_smoke[, 2] / sums_smoke

# Compute percentage and round
tab_sex_smoke <- round(tab_sex_smoke * 100, 1)



#####
#e

# Create age category variable
agecat <- vector()
agecat[fev$age < 9] <- "< 9"
agecat[fev$age >= 9 & fev$age <= 13] <- "9 - 13"
agecat[fev$age > 13] <- "> 13"

# Compute mean for each group
mean_fev <- matrix(nrow = 3, ncol = 2)
mean_fev[1, 1] <- mean(fev$fev[agecat == "< 9" & 
  fev$smoke == "current smoker"])
mean_fev[2, 1] <- mean(fev$fev[agecat == "9 - 13" & 
  fev$smoke == "current smoker"])
mean_fev[3, 1] <- mean(fev$fev[agecat == "> 13" & 
  fev$smoke == "current smoker"])
mean_fev[1, 2] <- mean(fev$fev[agecat == "< 9" & 
  fev$smoke == "non-current smoker"])
mean_fev[2, 2] <- mean(fev$fev[agecat == "9 - 13" & 
  fev$smoke == "non-current smoker"])
mean_fev[3, 2] <- mean(fev$fev[agecat == "> 13" & 
  fev$smoke == "non-current smoker"])

# Add names
rownames(mean_fev) <- c("Under 9", "9 - 13", "Over 13")
colnames(mean_fev) <- c("Current smoker", "Non-current smoker")


mean_fev <- round(mean_fev, 2)




#####
#f. 
# Subset FEV data
fev_smoke <- fev[fev$smoke == "current smoker" & fev$fev < 2.5 & 
    fev$sex == "female", ]
# Order data by age then height
fev_smoke <- fev_smoke[order(fev_smoke$age, fev_smoke$height), ]






#####
#Part 2.
#####
# plot layout
mat1 <- rbind(c(1, 2), 3)
layout(mat1)
mar.default <- c(5.1, 4.1, 4.1, 2.1)




#####
#plot 1

# Increase bottom margins
par(mar = mar.default + c(5, 0, 0, 0))

# Create color vector
cols <- vector()
cols[fev$sex == "female"] <- "red"
cols[fev$sex == "male"] <- "blue"


# Create scatterplot
plot(fev$fev ~ fev$age, col = cols, pch = 16, xlab = "", ylab = "FEV (liters)", 
  main = "Changes in FEV with age")
# Add legend
legend("topleft", legend = c("female", "male"), col = c("red", "blue"), 
  pch = 16)
# Add x label
mtext("Age (years)", side = 1, line = 4)

# Add lowess plot for females
fem <- fev[fev$sex == "female", ]
lfem <- lowess(x = fem$age, y = fem$fev)
lines(lfem$x, lfem$y, col = "red")

# Add lowess plot for males
mal <- fev[fev$sex == "male", ]
lmal <- lowess(x = mal$age, y = mal$fev)
lines(lmal$x, lmal$y, col = "blue")





#####
#plot 2
# Make agecat variable into a factor
agecat <- factor(agecat, levels = c("< 9", "9 - 13", "> 13"), 
	labels = c("Under 9", "9 - 13", "Over 13"))
	
# Compute proportion of current smokers by age
tab_sm_age <- table(fev$smoke, agecat)
tab_age <- table(agecat)
tab_plot <- tab_sm_age[1, ] / tab_age

# Create plot
barplot(tab_plot, col = "green", ylab = "Proportion current smokers", 
  xlab = "", las = 2, yaxt = "n", main = "Smokers by age category", 
  ylim = c(0, 1))

# Change y-axis
axis(2, at = c(0, 0.5, 1))
# Add x label
mtext("Age (years)", side = 1, line = 4)




#####

#plot 3
# Increase margin on right
par(mar = mar.default + c(0, 0, 0, 8))

# Create color variable
cols <- vector(, length = length(agecat))
cols[which(agecat == "Under 9")] <- 1
cols[which(agecat == "9 - 13")] <- "red"
cols[which(agecat == "Over 13")] <- "green"


# Create plotting symbol variable
pchs <- vector(, length = length(agecat))
pchs[which(agecat == "Under 9")] <- 1
pchs[which(agecat == "9 - 13")] <- 2
pchs[which(agecat == "Over 13")] <- 3

# Create scatterplot
plot(fev$height, fev$fev, xlab = "Height (inches)", main = "Changes in FEV 
  with height by age category", ylab = "FEV (liters)", pch = pchs, col = cols, 
  ylim = c(0, 8))
# Add legend
legend("topleft", legend = levels(agecat), pch = c(1, 2, 3), col = c(1, 2, 3),
  bty = "n", title = "Age category")

# Compute mean FEV for each age category
ml9 <- mean(fev$fev[agecat == "Under 9"])
m913 <- mean(fev$fev[agecat == "9 - 13"])
mo13 <- mean(fev$fev[agecat == "Over 13"])

# Add lines for each mean
abline(h = ml9, col = 1, lty = 2)
abline(h = m913, col = "red", lty = 2)
abline(h = mo13, col = "green", lty = 2)

# Add text for each line
mtext("Mean FEV under 9", at = c(75, ml9), side = 4, las = 2)
mtext("Mean FEV 9 - 13", at = c(75, m913), side = 4, las = 2, col = "red")
mtext("Mean FEV over 13", at = c(75, mo13), side = 4, las = 2, col = "green")



####
# Bonus code
# Subset height for age category
height_l9 <- fev$height[agecat == "Under 9"]
height_913 <- fev$height[agecat == "9 - 13"]
height_o13 <- fev$height[agecat == "Over 13"]

# Add segments for each age group spanning min/max height
segments(y0 = ml9, x0 = min(height_l9), x1 = max(height_l9), col = 1, lty = 2)
segments(y0 = m913, x0 = min(height_913), x1 = max(height_913), col = 2, 
  lty = 2)
 segments(y0 = mo13, x0 = min(height_o13), x1 = max(height_o13), col = 3, 
  lty = 2)
