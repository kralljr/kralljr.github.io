#  Week 9: ggplot2
#Focus today on tools to 
# - Create nice looking simple scatter plots
# - Create more complicated scatter plots
# - Create plots confidence intervals 
# - Create publication appropriate plots 


# First load libraries
#install.packages("ggplot2")
library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)


#look at data
head(airquality)


#qplot
qplot(Ozone,data=airquality)


qplot(Ozone,data=airquality, binwidth=15)


qplot(Temp, Ozone, data=airquality)


#check missingness 
summary(airquality$Temp)
summary(airquality$Ozone)


#geom
qplot(Temp, Ozone, data=airquality,geom=c("point"))

qplot(Temp, Ozone, data=airquality,geom=c("smooth"))

qplot(Temp, Ozone, data=airquality,geom=c("point", "smooth"))


#fitted line
qplot(Temp, Ozone, data=airquality,geom=c("point", "smooth"), method = lm)


#3rd factor
qplot(Temp,Ozone,data=airquality,color=Month)


#4th factor
qplot(Temp,Ozone,data=airquality,color=Month, size=Solar.R)


#translucent points
qplot(Temp,Ozone,data=airquality,color=Month, size=Solar.R, alpha = I(0.7))
qplot(Temp,Ozone,data=airquality,color=Month, size=Solar.R, alpha = I(0.3))


#titles and axes
qplot(Temp,Ozone,data=airquality,color=Month, size=Solar.R, alpha = I(0.7),  
      xlab = "Maximum Temperature (F)", ylab = "Ozone (ppb)",
      main = "Ozone and Maximum Temperature by Month and Solar Radiation")




#ggplot basic formula
ggplot(airquality, aes(x=Temp, y=Ozone)) + geom_point()


#compare
qplot(Temp, Ozone, data=airquality, main="Qplot")
plot(airquality$Temp, airquality$Ozone, pch = 16)


#color
a <- ggplot(airquality, aes(x=Temp, y=Ozone, color = Solar.R)) + geom_point(size=4, shape=20)
a


#title and lables
a <- ggplot(airquality, aes(x=Temp, y=Ozone, color = Solar.R)) + geom_point(size=4, shape=20) + 
  ylab("Ozone (ppb)") + xlab("Temperature (F)")  + ggtitle("Ozone by Temperature")
a



#facets
a + facet_wrap(~Month) + ggtitle("Ozone by Temp and Month")


#better facet labels
airquality$Month <- factor(airquality$Month, levels = seq(5, 9), labels = c("May", 
              "June", "July", "August", "September"))


a <- ggplot(airquality, aes(x=Temp, y=Ozone, color = Solar.R)) + geom_point(size=3, shape=20)

a + facet_wrap(~Month) + ggtitle("Ozone by Temp and Month")

#grid facets
airquality <- mutate(airquality, windcat = cut(Wind, breaks = 4))
a <- ggplot(airquality, aes(x=Temp, y=Ozone, color = Solar.R)) + 
      geom_point(size=3, shape=20)

a + facet_grid(Month ~ windcat)  



#colors in ggplot
cols <- c('#1B9E77', '#D95F02', '#7570B3', '#E7298A', '#66A61E')
a <- ggplot(airquality, aes(x=Temp, y=Ozone, color = Month)) + geom_point(size=4, shape=20) + 
      scale_color_manual(values = c(cols))
a
#this works bc we made Month a factor above

#continuous color scale
a <- ggplot(airquality, aes(x=Temp, y=Ozone, color = Solar.R)) + 
    geom_point(size=3, shape=20) + scale_color_gradient(low = "#FF8C00", high = "#68228B") 
a


#setting up the data for confidence intervals
aq_month <- group_by(airquality, Month)
means <- summarise_each(aq_month, funs(mean(., na.rm = T)), Ozone, Temp)
means <- gather(means, "Variable", "Mean", 2 : 3)

#can combine both functions into one
sds <- gather(summarise_each(aq_month, funs(sd(., na.rm = T)), Ozone, Temp), "Variable", "SD", 2 : 3)
nobs <- gather(summarise_each(aq_month, funs(length(which(!is.na(.)))), Ozone, Temp), "Variable", "N_obs", 2 : 3)

#use full_join to get dataset
CIdat <- full_join(means, sds)
CIdat <- full_join(CIdat, nobs)

#Can use subsequent variables you create using mutate!
CIdat <- mutate(CIdat, Tstat = qt(0.975, N_obs-1), ME = (Tstat*SD/sqrt(N_obs)), 
                LB = (Mean - ME), UB = (Mean + ME))

CI_ozone <- filter(CIdat, Variable == "Ozone")


#plotting confidence intervals
b <- ggplot(CI_ozone, aes(y= Mean, x=Month, color=Month)) + geom_point(size=3, shape=20) + 
  geom_errorbar(aes(ymin=LB, ymax=UB), width=0.3) +
  ggtitle("Mean ozone by month") + ylab("Ozone (ppb)") + xlab("Month") 
b

#removing a legend
b <- b + theme(legend.position = "none")

b



#printing plots for publication
png(filename="/Users/brooke/Desktop/AQ.png", width=1200)
grid1 <- grid.arrange(b,c, ncol=2, main=textGrob("Ozone and Daily Temperature",gp=gpar(fontsize=25,font=1)))
dev.off()



