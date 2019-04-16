##
## Housing Price Project
##

# Primary Research Questions
# How well do the home characterisitics explain sale price?
# What factors increase the sale price of a home?
# Does the variability of sale price increase with the size of the home (as given by living area)?
# What is your predicted/appraised sale price for the homes in the dataset that do not have a sale price?
  

library(ggplot2)

housing <- read.csv("https://mheaton.byu.edu/Courses/Stat469/Topics/3%20-%20SpatialCorrelation/3%20-%20Project/Data/HousingPrices.csv")

ggplot(data = housing, aes(x = Lon, y = Lat, color = Price)) + geom_point() + scale_color_distiller(palette = "RdBu", na.value = "white")

# most important and informative plot: plot of the residuals 