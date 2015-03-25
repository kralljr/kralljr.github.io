#  Week 10: More ggplot2 and colors
# Goals:
    # 1. Theme elements
    # 2. Legends
    # 3. Better colors
    # 4. Manipulating datasets for ggplot


# Set working directory
#setwd("~/Dropbox/IntrotoREpi/data/")


# load/install relevant libraries
#install.packages("RColorBrewer")
#install.packages("reshape2)
library(dplyr)
library(tidyr)
library(reshape2)
library(ggplot2)
library(RColorBrewer)
library(gridExtra)


# load data
data(airquality)





##################
# Review

# Recall plot of airquality data from last week:
g1 <- ggplot(data = airquality, aes(x = Temp, y = Ozone)) + geom_point() + 
    ggtitle("Temperature vs. Ozone") + xlab("Temperature") + ylab("Ozone")
g1



# Recall plot of airquality datafrom last week:
ggplot(data = airquality, aes(x = Temp, y = Ozone)) + 
    geom_point(size = 10, color = "red") + 
    ggtitle("Temperature vs. Ozone") + xlab("Temperature") + ylab("Ozone")



##################
# Theme elements
# These control sizing, color etc. for things like axis labels, titles

# Syntax:
# theme(thing we want to change = theme attribute(attribute assigning))

# Changing the size of all titles
g1 +  theme(title = element_text(size = 25))


# Changing the size of one title
g1 + theme(axis.title.x = element_text(size = 25))
g1 + theme(plot.title = element_text(size = 25))


# Changing the size of text and axes labels simultaneously
g1 + theme(text = element_text(size = 25))

# Changing the size of one axis
g1 + theme(axis.text.y = element_text(size = 25))



# Changing the size of lines (including tick marks)
g1
g1 + theme(line = element_line(size = 0.1))
g1 + theme(axis.ticks.y = element_line(size = 2))



# Getting rid of grey background
g1 + theme_bw()




# Removing things (e.g. text, titles, etc.) from your plot
g1 + theme(axis.text = element_blank())





# In-class exercise 1:
# Create a plot of ozone vs. temperature with no tick marks and no grid




#######
# Legends
airquality <- mutate(airquality, Month = factor(Month))
g1 <- ggplot(data = airquality, aes(x = Temp, y = Ozone, colour = Month)) + 
    geom_point() + ggtitle("Temperature vs. Ozone") + xlab("Temperature") + 
    ylab("Ozone")
g1



# Move legend
g1 + theme(legend.position = "top")


# Change title of legend
g1 + theme(legend.title = element_blank(), legend.position = "top")



# But those colors are ugly!  I don't know what those numbers are!
labs1 <- c("May", "June", "July", "August", "September")
cols <- seq(1, 5)
g1 + scale_color_manual(labels = labs1, name = "", values = cols)









########
# Better colors using R Color Brewer
cols <- brewer.pal(5, "Dark2")
g1 + scale_color_manual(labels = labs1, values = cols)

### ggplot 2 has this built in!
g1 + scale_colour_brewer(palette = "Dark2")



# Continuous wind speed
# Specify sequential palette 
cols <- brewer.pal(9, "Oranges")
# Usual plot with color for wind speed
g2 <- ggplot(data = airquality, aes(x = Temp, y = Ozone, colour = Wind)) + geom_point() + 
    ggtitle("Temperature vs. Ozone") + xlab("Temperature") + ylab("Ozone")
g2
# Change colors specifying low and high
g2 + scale_color_gradient(low = cols[1], high = cols[9]) + theme_bw()

# Using ggplot2 directly (no color bar)
g2 + scale_color_distiller(palette = "Oranges")








# Everything at once:

# Initialize plot
ggplot(data = airquality, aes(x = Temp, y = Ozone, colour = Month)) + 
    # Add points with shape = 2
    geom_point(shape = 2) + 
    # Add titles
    ggtitle("Temperature vs. Ozone") + xlab("Temperature") + ylab("Ozone") +
    # Change colours, labels for legend
    scale_color_brewer(labels = labs1, palette = "Set1") +
    # Remove title for legend, move legend to top
    theme(legend.title = element_blank(), legend.position = "top") +
    # Change size of all text
    theme(text = element_text(size = 25))











######
# Manipulating datasets for ggplot: "tidy data"
# 1. Anything you want to "facet" by should be in one variable (multiple rows)
# 2. Anything you want to color, shape, etc by should be in multiple columns
data(Titanic)
mtitanic <- melt(Titanic)
head(mtitanic)

ggplot(data = mtitanic, aes(x = Class, y = value, fill = Survived)) + 
    geom_bar(stat = "identity") + 
    facet_grid(Sex ~ Age, scales = "free")


# Same plot if data aren't formatted nicely
wide <- spread(mtitanic, Sex, value)
head(wide)

# Now need to make separate plots for males and females
# You need to write more code!
gmale <- ggplot(data = wide, aes(x = Class, y = Male, fill = Survived)) + 
    geom_bar(stat = "identity") + 
    facet_grid(~ Age, scales = "free")
gfemale <- ggplot(data = wide, aes(x = Class, y = Female, fill = Survived)) + 
    geom_bar(stat = "identity") + 
    facet_grid(~ Age, scales = "free")
grid.arrange(gmale, gfemale)





# If variable is a column (instead of columns for each of age, chol, etc), 
# can use ggplot without alteration
load("OR_df.RData")
head(OR_df)

# Set up data, x/y/ color
ggplot(OR_df, aes(x = Variable, y = OR, color = Variable)) +
    #Add points for ORs
    geom_point(size = 3, shape = 20) +
    #Add error bars
    geom_errorbar(aes(ymin = LB, ymax = UB), width = 0.3) +
    #Add main and axes titles
    ggtitle("Associations between covariates and diabetes") +
    ylab("Odds ratio") + xlab("Covariates")



head(airquality)
g1 <- ggplot(airquality, aes(x = Ozone, fill = Month)) + geom_histogram() 
g2 <- ggplot(airquality, aes(x = Solar.R, fill = Month)) + geom_histogram() 
g3 <- ggplot(airquality, aes(x = Wind, fill = Month)) + geom_histogram() 
g4 <- ggplot(airquality, aes(x = Temp, fill = Month)) + geom_histogram() 
grid.arrange(g1, g2, g3, g4)

# In-class exercise 2:
# Set up a dataset called gaq to plot histograms of each variable 
# (ozone, solar, wind, temperature)
# Hint: use gather function and specify arguments:
# 1. data, 2. name of column for variables, 3. name of column for values, 
# 4. what variables do you want to gather (or not gather)
# gaq <- 

ggplot(gaq, aes(x = value, fill = Month)) + geom_histogram() + 
    facet_wrap( ~ variable, scales = "free")
