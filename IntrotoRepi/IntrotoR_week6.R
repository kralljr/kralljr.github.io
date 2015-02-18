#  Week 6: dplyr
#Focus today on tools to better
# - Clean data by removing and adding rows/columns
# - Summarizing data
# - Merging data
# - Transforming data

# First load libraries
library(dplyr)
library(tidyr)

# Read in very low birthweight data
vlbw_all <- read.csv("vlbw.csv", stringsAsFactors = F)
# Rename first column
colnames(vlbw_all)[1] <- "id"
# Restrict columns
vlbw <- vlbw_all[, c("id", "bwt", "gest", "sex", "twn", "pneumo")]






############
# Data frame tbl
############
vlbw
# Data frame tbl
vlbw <- tbl_df(vlbw)
vlbw


####
# Learning about our data set
# New!
glimpse(vlbw)
# Old (base R)
str(vlbw)


# Number of unique values
n_distinct(vlbw$twn)
table(vlbw$twn, exclude = NULL)



# Removing duplicate rows
dim(vlbw)
# Add duplicated rows
vlbw_dup <- rbind(vlbw, vlbw)
dim(vlbw_dup)

vlbw_dup <- distinct(vlbw_dup)
all.equal(vlbw_dup, vlbw)







############
# Data manipulation
############

####
# Subsetting columns using `select`
vlbw_3 <- select(vlbw, bwt, gest)

# Same way using base R
vlbw_2 <- vlbw[, c("bwt", "gest")]

all.equal(vlbw_2, vlbw_3)


# In-class exercise 1:
# Select the first 5 columns of vlbw_all





####
# Removing columns using `select`
vlbw_3 <- select(vlbw, -bwt, -gest) 

#using base R
vlbw_2 <- vlbw[, -which(colnames(vlbw) %in% c("bwt", "gest"))]
all.equal(vlbw_2, vlbw_3)




####
# Subsetting rows by condition using `filter`
vlbw_3 <- filter(vlbw, gest >= 39) 

#using base R
vlbw_2 <- vlbw[vlbw$gest >= 39, ]


# In-class exercise 2:
# Subset the data to female twins with pneumothorax




####
# Subsetting rows by position using `slice`
vlbw_3 <- slice(vlbw, 1 : 5)

#using base R
vlbw_2 <- vlbw[1 : 5, ]
all.equal(vlbw_2, vlbw_3)







####
# Renaming variables
head(select(vlbw, 2 : 4))
vlbw_2 <- rename(vlbw, Sex = sex)

#using base R
colnames(vlbw_2)[colnames(vlbw_2) == "Sex"] <- "sex"




####
# Adding new variables to our data frame using mutate

# In base R, we might do this
full_term <- 1 * (vlbw$gest >= 39)
table(full_term, exclude = NULL)
vlbw_2 <- data.frame(vlbw, full_term)
head(vlbw_2)


# In dplyr,
vlbw_2 <- mutate(vlbw, full_term = 1 * (vlbw$gest >= 39))
vlbw_2

all.equal(full_term, vlbw_2$full_term)


####
# Can also drop variables with mutate
vlbw_3 <- mutate(vlbw, sex = NULL)
vlbw_3

# Using base R
vlbw_2 <- vlbw[, -which(colnames(vlbw) == "sex")]



# Can easily add/drop variables with transmute
vlbw_2 <- transmute(vlbw, full_term = 1 * (vlbw$gest >= 39))
vlbw_2









############
# Rearranging data
############




####
# Using `arrange` to sort your data
vlbw_3 <- arrange(vlbw, gest)
vlbw_3

vlbw_2 <- vlbw[order(vlbw$gest), ]
vlbw_2

all.equal(vlbw_2, vlbw_3)


# In-class exercise 3:
# Sort the data by birthweight, gestation, and sex






#####
# Grouping

#When we use factors in base R
#- We have to continually tell R we want to classify by those variables
#- Example: If we want mean BMI by age category and later we want to compute 
# mean height by age category, we have to subset our data again.

# Base R:
tapply(BMI, agecat, mean)
tapply(height, agecat, mean)


#`group_by` in `dplyr` remembers what variables you want to group by
gb_sextwn <- group_by(vlbw, sex, twn)
gb_sextwn


####
# Useful group functions
# What groups?
groups(gb_sextwn)
# Size of each group
group_size(gb_sextwn)
tally(gb_sextwn)
# Number of groups
n_groups(gb_sextwn)




# To ungroup:
ungroup(gb_sextwn)





############
# Summarizing data
############


# In base R, we might do this to get the mean gestational age 
# by sex and twin status
table(vlbw$sex, vlbw$twn, exclude = NULL)
mean(vlbw$gest[vlbw$sex == "female" & vlbw$twn == 1], na.rm = T)
mean(vlbw$gest[vlbw$sex == "female" & vlbw$twn == 0], na.rm = T)
mean(vlbw$gest[vlbw$sex == "male" & vlbw$twn == 1], na.rm = T)
mean(vlbw$gest[vlbw$sex == "male" & vlbw$twn == 0], na.rm = T)
mean(vlbw$gest[is.na(vlbw$sex) & vlbw$twn == 0], na.rm = T)
mean(vlbw$gest[is.na(vlbw$sex) & is.na(vlbw$twn)], na.rm = T)


# Using dplyr,
summarise(gb_sextwn, mean(gest, na.rm = T))

# We can also summarize multiple variables
summarise_each(gb_sextwn, funs(mean), gest, bwt)

# We can add arguments to our functions:
summarise_each(gb_sextwn, funs(mean(., na.rm = T)), gest, bwt)


# In-class exercise 4:
# Compute the 20th percentile of gestational age by sex and twin status












############
# Merging and transforming data
############

# Suppose we have some new data
weight1 <- vlbw$bwt + 100 
weight2 <- vlbw$bwt + 200 
weights <- data.frame(seq(1, nrow(vlbw)), weight1, weight2)
colnames(weights) <- c("id", "weight_1", "weight_2")
weights <- tbl_df(weights)
weights


# Merge with out data
# left_join(x = data1, y = data2, by = what variable to merge by?)
m_vlbw <- left_join(vlbw, weights, by = "id")
m_vlbw


# What if column names are not the same
colnames(weights)[1] <- "infantID"
weights
left_join(vlbw, weights, by = "id")

m_vlbw <- left_join(vlbw, weights, by = c("id" = "infantID"))
m_vlbw


# What if some IDs are not in dataset
# Remove  the data for IDs 35-671
vlbw_2 <- filter(vlbw, id < 35)

n_distinct(vlbw_2$id)
n_distinct(vlbw$id)

# Now merge-- missing observations?
m_vlbw <- left_join(vlbw, weights, by = c("id" = "infantID"))
n_distinct(m_vlbw$id)

m_vlbw <- full_join(vlbw, weights, by = c("id" = "infantID"))
n_distinct(m_vlbw$id)


####
#Now, suppose we have
#- IDs 1-34 in `vlbw` data
#- IDs 20-40 in `weights` data
#and we only want to keep IDs with `weights` data
# Right join
weights_2 <- filter(weights, infantID %in% seq(20, 40))
m_vlbw <- right_join(vlbw_2, weights_2, by = c("id" = "infantID"))
n_distinct(m_vlbw$id)

# Inner join
m_vlbw <- inner_join(vlbw_2, weights_2, by = c("id" = "infantID"))
n_distinct(m_vlbw$id)





#These data are in what we call "wide form".  
#   - Each id corresponds to one row in the dataset
#   - Columns represent the same variable over time (weights)
# Let's transform our dataset to "long form", so that each row represents 
# a unique observation.  We will use the `tidyr` package 
# gather(data, key = timevar, value = valuevar, what switches)
long_data <- gather(m_vlbw, key = time, value = weight, weight_1 : weight_2)
long_data


# We can also get the data back into "long form", by spreading the dataset
# spread(data, key = timevar, value = valuevar)
wide_data <- spread(long_data, key = time, value = weight)
wide_data



#### 
# Sidenote: sourcing external code

#You might have 
#    - One .R file to clean and merge datasets
#    - One .R file to create output including tables and figures

#If we want to separate the two files, we can call the second from the 
# first using the `source` function.
source("data_clean.R")



# Review data wrangling cheatsheet