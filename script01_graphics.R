### base graphics

# load ibrary with pre-installed datasets
library(datasets)
# show first lines of the data
head(airquality)

## Histogram
hist(airquality$Temp)

# ...add labels
hist(airquality$Temp,
     xlab = "Temperatur (Grad Fahrenheit)",
     ylab = "Häufigkeit",
     main = "Temperaturen in New York City - 1.5. bis 30.9.1073")

# ...change y axis into density 
hist(airquality$Temp,
     xlab = "Temperatur (Grad Fahrenheit)",
     ylab = "Häufigkeit",
     main = "Temperaturen in New York City - 1.5. bis 30.9.1073",
     prob = TRUE)

# add density line
lines(density(airquality$Temp), lty = "dotted", col = "red", lwd = 2)


## Barplot
library(MASS)
head(Cars93)

# compute frequencies
table(Cars93$Type)

# create barplot
barplot(table(Cars93$Type))

# extend line along the y axis from 20 to 25
barplot(table(Cars93$Type), ylim = c(0, 25))

# change space betwee bars
barplot(table(Cars93$Type), ylim = c(0, 25), space = 0.5)

## --> Task: Add labels to the x axis and y axis (cf. above)!


## Filtering data
library(dplyr)
USA.Cars93 <- filter(Cars93, Origin == "USA")  

# ## --> Tasks:
# Recreate the barplot for USA.Cars93!
# Create a new data set for non-USA cars and do the same! 
# Compare both barplots!
# Try to create a barplot for the variable Cylinders!  


## Grouped barplot
# library(datasets)
studentinnen <- HairEyeColor[,,2]
studentinnen
# note: this is already a frequency table => no need to call table() here!

# transpose table
t(studentinnen)

# create grouped barplot
# first define colours:
# http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf
farben <- c("brown", "blue", "chocolate", "green")

barplot(t(studentinnen), beside = TRUE,
        ylim = c(0,70),
        xlab = "Haarfarbe", ylab = "Häufigkeit der Augenfarbe",
        col = farben
        )

# add legend
legend("top", rownames(t(studentinnen)), cex = 0.8, fill = farben, title = "Augenfarbe")


# analyze the dependency
# mosaicplot
mosaicplot(studentinnen, col = farben)

# Chi squared 
chisq.test(studentinnen)

x2 <- chisq.test(studentinnen)
x2$observed
x2$expected # table of expected frequencies
x2$statistic
x2$p.value

# Cramer's V:
sqrt((x2$statistic / sum(studentinnen)) / min(dim(studentinnen)-1)) # by hand
#install.packages("questionr")
library(questionr)
cramer.v(studentinnen)

## Scatterplot
# library(datasets)
plot(airquality$Wind, airquality$Temp,
     pch = 16,                             # choose symbol
     xlab = "Windgeschwindigkeit (MPH)",
     ylab = "Temertatur (Fahrenheit)")
     

## Scatterplot matrix
# select only three variables from data
ozone.temp.wind <- subset(airquality, select = c(Ozone, Temp, Wind))
colnames(ozone.temp.wind) <- c("Ozon", "Temperatur", "Wind")
head(ozone.temp.wind)

# create matrix of pairwise scatterplots
pairs(ozone.temp.wind)
## --> Task: Describe the dependencies between ozone, temperature and wind! 
##           Which of them are strong and which are weak?

# compute correlation matrix
cor(ozone.temp.wind)
## --> Task: What does NA mean in the first table?  
##           ...Take a look at the data and the variable Ozon. What do you notice?

cor(ozone.temp.wind, use = "pairwise.complete.obs")

# visualize correlation matrix
# install.packages("corrplot")
library(corrplot)
corrplot(cor(ozone.temp.wind, use = "pairwise.complete.obs"))


## boxplot
boxplot(Temp ~ Month, data = airquality)

# nicer axis labels
boxplot(Temp ~ Month, data = airquality, xaxt = "n")
axis(1, at = 1:5, labels = c("Mai", "Jun", "Jul", "Aug", "Sep"))



### ggplot2
library(ggplot2)

# create 'base graph layout'
ggplot(airquality, aes(x = Temp))

# ...add histogram
ggplot(airquality, aes(x = Temp)) + 
  geom_histogram()

# general syntax:
# ggplot(data, aes()) + 
#   geom_xxx() +          # type of the plot
#   theme_xxx() +         # change grapic layout style
#   labs()                # add labels

# ...modify bin width of histogram
ggplot(airquality, aes(x = Temp)) + 
  geom_histogram(binwidth = 5)

# ...change colors
ggplot(airquality, aes(x = Temp)) + 
  geom_histogram(binwidth = 5, color = "black", fill = "grey80")

# ...theme for white background  and labels
ggplot(airquality, aes(x = Temp)) + 
  geom_histogram(binwidth = 5, color = "black", fill = "grey80") +
  theme_bw() +
  labs(x = "Temperatur (Fahrenheit)",
       y = "Häufigkeit", 
       title = "Airquality Data")

# barplots
# library(MASS)
ggplot(Cars93, aes(x = Type)) +
  geom_bar() +                       # barplot
  labs(x = "Fahrzeugtyp", y = "Häufigkeit")


# select table with only two variables
type.origin <- subset(Cars93, select = c(Type, Origin))

# stacked bars                    ...+ map variable to colour
ggplot(type.origin, aes(x = Type, fill = Origin)) +
  geom_bar()

# grouped bars
ggplot(type.origin, aes(x = Type, fill = Origin)) +
  geom_bar(position = "dodge")

ggplot(type.origin, aes(x = Type, fill = Origin)) +
  geom_bar(position = "dodge", color = "black")


# ...grouped bars based if the data are already aggregated in frequency tables
# ...cf. above...
# library(datasets)
# studentinnen <- HairEyeColor[,,2]
# create a data frame from the table
studentinnen.df <- as.data.frame(studentinnen)
studentinnen.df 

ggplot(studentinnen.df, aes(x=Hair, y = Freq, fill = Eye)) +
  geom_bar(position = "dodge", stat = "identity")  

# ...add colour scale (from black to grey)
ggplot(studentinnen.df, aes(x=Hair, y = Freq, fill = Eye)) +
  geom_bar(position = "dodge", stat = "identity") + 
  scale_fill_grey(start = 0, end = 1)


## scatterplot
ggplot(airquality, aes(x = Wind, y = Temp)) +
  geom_point()

# remove missing values
# istall.packages("tidyr")
library(tidyr)
aq.wo.na <- drop_na(airquality)

# create new variable: high/low ozone
med.oz <- median(aq.wo.na$Ozone)
aq.wo.na$Ozon.Level <- ifelse(aq.wo.na$Ozone <= med.oz, "niedrig", "hoch")
aq.wo.na

# map variable to colour
ggplot(aq.wo.na, aes(x = Wind, y = Temp, col = Ozon.Level)) +
  geom_point(size = 2.5) + 
  scale_color_grey(start = 0, end = 1)

# scatterplot matrix
aq.subset <- subset(aq.wo.na, select = c(Ozone, Wind, Temp, Solar.R))
# install.packages("GGally")
library(GGally)
ggpairs(aq.subset)

# 3D Plot
# install.packages("scatterplot3d")
library(scatterplot3d)
scatterplot3d(aq.wo.na$Temp ~ aq.wo.na$Wind + aq.wo.na$Ozone, pch = 19)

# boxplot
ggplot(airquality, aes(x=as.factor(Month), y = Temp)) +
  geom_boxplot() 

# ...add points to plots and label axes
ggplot(airquality, aes(x=as.factor(Month), y = Temp)) +
  geom_boxplot() + 
  geom_point() +
  labs(x = "Monat", y = "Temperatur") +
  scale_x_discrete(labels = c("Mai","Jun","Jul","Aug","Sep"))
