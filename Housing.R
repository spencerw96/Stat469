##
## Housing Price Project
##

# Primary Research Questions
# How well do the home characterisitics explain sale price?
# What factors increase the sale price of a home?
# Does the variability of sale price increase with the size of the home (as given by living area)?
# What is your predicted/appraised sale price for the homes in the dataset that do not have a sale price?
  

library(ggplot2)
library(geoR)
library(tidyverse)
library(nlme)
source('https://raw.githubusercontent.com/MJHeaton/glstools/master/stdres.gls.R')
source("https://raw.githubusercontent.com/MJHeaton/glstools/master/predictgls.R")
library(lmtest)
library(MASS)
library(scales)
library(car)

house <- read.csv("https://mheaton.byu.edu/Courses/Stat469/Topics/3%20-%20SpatialCorrelation/3%20-%20Project/Data/HousingPrices.csv")
house$House.Style <- as.factor(house$House.Style)

h_true <- house %>% filter(!is.na(Price))
h_pred <- house %>% filter(is.na(Price))

ggplot(data = h_true, aes(x = Lon, y = Lat, color = Price)) + xlab("Longitude") + ylab("Latitude") +
  geom_point() + scale_color_distiller(palette = "Spectral", na.value = "white", labels = comma) +
  ggtitle("Housing Prices by Location")
# for linearity
# pairs(house[c(1, 2, 4, 6, 8:11)])
boxplot(Price~House.Style, data=h_true)

#check assumptions and effectiveness w/normal linear model
home_lm <- lm(Price~., data=house)
par(mfrow=c(1,1))
res <- resid(home_lm)
fit <- fitted(home_lm)

#check spatial correlation
ggplot(data = h_true, aes(x = Lon, y = Lat, color = res)) + 
  geom_point() + scale_color_distiller(palette = "Spectral", na.value = "white")
vario <- variog(coords=h_true[,2:3], data=resid(home_lm))
plot(vario)
#there is apparently spatial correlation, consider this in future model
#normality
hist(res)
#equal variance
qplot(fit, res, geom = "point") + geom_hline(yintercept = 0, col = "red") + 
  xlab("Fitted Values") + ylab("Residuals") + ggtitle("Fitted Values v. Residuals") +
  scale_x_continuous(labels = comma)
#is there heteroskedasticity?
#linearity
avPlots(home_lm)
#build a gls model that accounts for heteroskedasticity and spatial correlation
#the variables that appear to affect homoskedasticity are Gr.Liv.Area, Full.Bath, and Garage.Cars
#when going through these in the model, Gr.Liv.Area appears to account for all of it on its own, which makes 
#sense because garage size and number of bathrooms will certainly be related to house size 
het_gls <- gls(model = Price~., data = h_true, weights = varExp(form=~Gr.Liv.Area), method = "ML")
std_res <- resid(het_gls, type = "pearson")
gls_fit <- fitted(het_gls)
plot(gls_fit, std_res)
hist(std_res)

# exp_gls <- gls(model=Price~., data=h_true, correlation=corExp(form=~Lon+Lat, nugget=TRUE), 
#    weights=varExp(form=~Gr.Liv.Area), method="ML")

# sph_gls <- gls(model=Price~., data=h_true, correlation=corSpher(form=~Lon+Lat, nugget=TRUE), 
#               weights=varExp(form=~Gr.Liv.Area), method="ML")

gaus_gls <- gls(model=Price~., data=h_true, correlation=corGaus(form=~Lon+Lat, nugget=TRUE), 
               weights=varExp(form=~Gr.Liv.Area), method="ML")

#AIC(exp_gls)
#AIC(sph_gls)
AIC(gaus_gls) #returns the smallest value

home_gls <- gaus_gls
gaus_fit <- fitted(home_gls)
#check spatial correlation 
dec_res <- stdres.gls(gaus_gls)
dec_var <- variog(coords=h_true[,2:3], data=dec_res)
plot(dec_var) 
#maybe not a perfect variogram, but it looks much better (INDEPENDENCE)
ggplot(data = h_true, aes(x = Lon, y = Lat, color = dec_res)) + geom_point() + 
  scale_color_distiller(palette = "Spectral", na.value = "white") +
  xlab("Longitude") + ylab("Latitude")
#this map looks good, the residuals appear to be random, indepenence met

#linearity
avPlots(home_lm)

qplot(gaus_fit, dec_res) + geom_point() + geom_hline(yintercept = 0, col = "red") + 
  xlab("Fitted Values") + ylab("Residuals") + ggtitle("Fitted Values v. Residuals") +
  scale_x_continuous(labels = comma)
#equal variance looks good for homoskedasticity

qplot(dec_res, geom = "histogram", bins = 10) + xlab("Residuals") + ylab("Frequency")
#looks normal
#we've got our model!
summary(home_gls)

#####################################
### answer the research questions ###
#####################################

### How well do the home characteristics explain sale price?
# pseudo r^2 (cor(actual, predicted))^2
h <- h_true[,-1]
preds_h <- predictgls(home_gls, newdframe = h) 
(cor(h_true[,1], preds_h[,12]))^2

### What factors increase the sale price of a home?
summary(home_gls)
confint(home_gls)

### Does the variability of sale price increase with the size of the home (as given by living area)?
bptest(home_lm)
ks.test(res, "pnorm")

### What is your predicted/appraised sale price for the homes in the dataset that do not have a sale price?
# create predicted values on dataframe with missing price values
preds.fit <- predictgls(home_gls, newdframe = h_pred)
predictedvals <- preds.fit[,12]
