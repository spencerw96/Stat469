##
## Housing Price Project
##

library(ggplot2)

housing <- read.csv("https://mheaton.byu.edu/Courses/Stat469/Topics/3%20-%20SpatialCorrelation/3%20-%20Project/Data/HousingPrices.csv")

ggplot(data = housing, aes(x = Lon, y = Lat, color = Price)) + geom_point() + scale_color_distiller(palette = "RdBu", na.value = "white")
