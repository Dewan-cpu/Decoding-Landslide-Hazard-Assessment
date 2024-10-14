#==================================================================================
# R code for landslide susceptibility modeling using Generalized Additive Model (GAM)
# Last modified:  October 2024
# The code is distributed "as is", WITH NO WARRANTY whatsoever!
#==================================================================================

## INITIAL SETTINGS
--------------------
# Install necessary libraries & load packages

list.packages = c("sf", "tidyverse", "pROC", "mapview", "mgcv", "sperrorest", "paletteer")
new.packages = list.packages[!(list.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# loading packages
# this command will load the previously install packages
lapply(list.packages, require, character.only=T)
remove(list.packages, new.packages)

## Setting up directory
------------------------

setwd("C:/Users/Dewan/Desktop/Testrun/Testrun")

# LOADING DATA 
Inventory = sf::st_read("C:/Users/Dewan/Desktop/Testrun/Testrun/Inventory2018_2020combined.gpkg")
Inventory2018 = sf::st_read("C:/Users/Dewan/Desktop/Testrun/Testrun/Inventory2018.gpkg")
Boundary = sf::st_read("C:/Users/Dewan/Desktop/Testrun/Testrun/campboundary46n.gpkg")

# load mapping units
SU = sf::st_read("C:/Users/Dewan/Desktop/Testrun/Testrun/Slopeunitstheend.gpkg")

## visualize data
-----------------
# mapview will allow to visualize the available data in an interactive interface.
# the basemap can be switched to topographic maps and satellite imagery 


library(mapview)

mapout <- mapview(Inventory, col.regions = "blue") +
  mapview(SU, color = "black", alpha.regions=0) +
  mapview(Boundary, color = "magenta", alpha.regions=0, lwd=2)
 


mapview(Inventory2018, col.regions = "blue") +
  mapview(SU, color = "black", alpha.regions=0) +
  mapview(Boundary, color = "magenta", alpha.regions=0, lwd=2)

# If failed to view map, follow the below snippet & save as webpage
# stable slope units without landslide are represented with "0"
# unstable slope units with landslides are represented with "1"

mapview(SU, zcol="Landslid_2", col.regions=c("dodgerblue1", "firebrick1")) +
  mapview(SU, zcol="Landslid_1", col.regions=c("dodgerblue4", "firebrick1")) 

mapview(SU, zcol="Landslide2", col.regions=c("dodgerblue1", "firebrick1")) +
mapview(SU, zcol="LandslideC", col.regions=c("dodgerblue1", "firebrick1"))

## Exploratory data analysis
-----------------------------
# histograms and boxplots
hist(SU$SlopeM, breaks = 100, xlab="", ylab="Frequency", main = "Average slope (°)")
boxplot(SU$SlopeM ~ SU$Landslid_2, col = c("dodgerblue1", "firebrick1"), main = "Average slope (°)", xlab="", ylab="")
boxplot(SU$AspectM ~ SU$Landslid_2, col = c("dodgerblue1", "firebrick1"), main = "Average Aspect (°)", xlab="", ylab="")
boxplot(SU$RelRelfM ~ SU$Landslid_2, col = c("dodgerblue1", "firebrick1"), main = "Relative relief", xlab="", ylab="")
boxplot(SU$Rain2013_5 ~ SU$Landslid_2, col = c("dodgerblue1", "firebrick1"), main = "Average Rainfall (mm)", xlab="", ylab="")
# Just for fun
boxplot(SU$SoilType ~ SU$Landslid_2, col = c("dodgerblue1", "firebrick1"), main = "Soil Type", xlab="", ylab="")
boxplot(SU$LULC201720 ~ SU$Landslid_2, col = c("dodgerblue1", "firebrick1"), main = "Landcover Change", xlab="", ylab="")
# we can also check patterns using the Probability Density function for both stable and unstable slope units
plot(density(SU$SlopeM[SU$Landslid_1==2]), col = "blue", "Average slope (°)", lwd = 2)
plot(density(SU$SlopeM[SU$Landslid_1==1]), col = "blue", "Average slope (°)", lwd = 2)

## MODELING
--------------
# Statistical modeling using 
# Generalized Additive Modes (GAMs) in the package mgcv https://cran.r-project.org/web/packages/mgcv/index.html
# Frequentist framework

# before we start let us have a look at the total of events in each inventory
nrow(Inventory)

# model fit
# formula
# bs controls the smooth types to be use in the model and "tp" is used by default.
# k-1 or (k) sets the upper limit on the degrees of freedom associated with a smooth. 
# for more details go to https://cran.r-project.org/web/packages/mgcv/mgcv.pdf or
# (Wood, 2017) https://www.taylorfrancis.com/books/mono/10.1201/9781315370279/generalized-additive-models-simon-wood

#//////////////////////////////////////
## Model for 2021(Monsoon) and prior
#//////////////////////////////////////

formula_5000 = Landslid_2 ~
  s(SlopeM, bs="tp", k = 26) + #s() is used to declare the non-linear properties
  s(SlopeR, bs="tp", k = 26)+
  s(SlopeSTD, bs="tp", k = 26)+
  s(SoildepthR, k = 26)+
  s(SoildepthM, k = 26)+
  s(SoilDepthS, k = 26)+
  s(AspectM, k = 26)+
  s(AspectSTD, k = 26)+
  s(PlanCurvR, k = 26)+
  s(PlanCurvM, k = 26)+
  s(ProfCurvR, k = 26)+
  s(ProfCurvM,k = 26)+
  s(RelRelfR, k = 26)+
  s(RelRelfM, k = 26)+
  s(TWIM, k = 26)+
  s(TWIR, k = 26)+
  s(TWISTD, k = 26)+
  s(DistRR, k = 26)+
  s(DistRM, k = 26)+
  s(Rain201321, k = 26)+
  s(Rain2013_4, k = 26)+
  s(SPIR, k = 26)+
  s(SPIM, k = 26)+
  s(SPISTD, k = 26)+
  LULC201720+
  NDVI201720+
  SoilType

# Fit the model
start.time = Sys.time()
mod_5000 = mgcv::gam(formula_5000, family = binomial, method="REML", data = SU)
end.time = Sys.time()
print(end.time-start.time)
# Residuals vs Fitted plot
plot(mod_5000$fitted.values, residuals(mod_5000), 
     xlab = "Fitted values", ylab = "Residuals", 
     main = "Residuals vs Fitted Values", pch = 20, col = "blue")
abline(h = 0, lty = 2, col = "red")

# QQ Plot for residuals
qqnorm(residuals(mod_5000), main = "Normal Q-Q Plot of Residuals")
qqline(residuals(mod_5000), col = "red", lty = 2)

# Histogram of residuals
hist(residuals(mod_5000), 
     breaks = 30, 
     main = "Histogram of Residuals", 
     xlab = "Residuals", col = "lightblue", border = "black")
# QQ plot with gratia
qq_plot(mod_5000_a)
qq_plot(mod_5000)
# Histogram with gratia
appraise(mod_5000)

# summary will give an overview of the fit
summary(mod_5000)
library(Metrics)
bias(mod_5000$fitted.values, (SU$Landslid_2))
ae(mod_5000$fitted.values, (SU$Landslid_2))
mae(mod_5000$fitted.values, (SU$Landslid_2))
rmse(mod_5000$fitted.values, (SU$Landslid_2))
cor(mod_5000$fitted.values, (SU$Landslid_2), method = "pearson")

# partial effects
plot(mod_5000, select=1, trans=plogis, shade=T, ylab="") # you can change the select to explore the other nonlinear effects

plot(mod_5000, trans=plogis, pages=1, all.terms=T, shade=T, ylab="")

windows()
plot(mod_5000,shade = TRUE, shade.col = "blue", ylim = c(-10,10))

plot(mod_5000,shade = TRUE, shade.col = "blue", select = 32)
dev.off()

## Visualization 3
-------------------
# Draw the plot for the specific term and add theme
plotss <- draw(mod_5000, select = "s(SlopeSTD)") +
  ylim(c(-10, 10)) +
  theme(axis.text = element_text(size = 25))
plotss

## Fitting performance:goodness of fit
---------------------------------------
# to create a new column in your dataset where the probability is stored
library(pROC)
SU$probability = as.numeric(predict(mod_5000, type="response", newdata=SU)) 
myroc_5000 = roc(response=SU$Landslid_2, predictor=SU$probability, auc=T) 
par(pty="s")
plot(myroc_5000, main = round(myroc_5000$auc, 5))
print(myroc_5000$auc)


## validation 
---------------
# 10-fold cross-validation
# we divide the dataset into 10 equal parts. We will use 9 of those parts to fit our model and validate over the remaining one
# in a interactive way
# partitions
# the library only works with centroids, but this step is merely for visualization
# what we only need is to defined the partitions
# Loading relevant packages

library(dplyr)
library(tidyverse)

centroids = dplyr::select(SU, CoordX_1, CoordY_1) %>% sf::st_drop_geometry()
partition = partition_cv(SU, nfold = 10, repetition = 1, seed1 = 123) 
plot(partition, centroids, coords = c("CoordX_1", "CoordY_1"), cex = 0.01, pch = 19)

# settings for the loop
# this loop will fit the model over 9 of the defined folds and will predict over the remaining fold
fold = (1:10)
SU$prediction = NA
SU_myroc = c(NA)
df = SU
roc = list()

# loop
for (i in fold){
  id.holdout = partition[[1]][[i]]$test
  df_train = SU[-id.holdout,]
  df_test = SU[id.holdout, ]
  fit = mgcv::gam(formula_5000, data=df_train, family=binomial, method="REML")
  SU$prediction[id.holdout] = as.numeric(predict(fit, type="response", newdata=df_test))
  roc[[i]] = roc(response=df_test$Landslid_2, predictor=SU$prediction[id.holdout], auc=T)
  SU_myroc[i] = as.numeric(unlist(roc[[i]][9]))
}

# Let's check the predictive performance over the 10 testing folds
boxplot(SU_myroc)
round(SU_myroc,5)
mean(SU_myroc)
median(SU_myroc)

# we can plot the 10 roc curves, one generated for every testing fold

par(pty="s")
plot(roc[[1]]); plot(roc[[2]], add=T); plot(roc[[3]], add=T); plot(roc[[4]], add=T); plot(roc[[5]], add=T)
plot(roc[[6]], add=T); plot(roc[[7]], add=T); plot(roc[[8]], add=T); plot(roc[[9]], add=T); plot(roc[[10]], add=T)


## VISUALIZATION 
------------------
# to visualize the results of the fit and the predictions
# we can adjust the strect and type of palette according to your interests
mapview(SU, zcol="probability", col.regions=paletteer::paletteer_d("RColorBrewer::RdYlGn", direction=-1))+
  mapview(SU, zcol="prediction", col.regions=paletteer::paletteer_d("RColorBrewer::RdYlGn", direction=-1))+
  mapview(SU, zcol="Landslid_1", col.regions=c("dodgerblue1", "firebrick1"))

mapview(SU, zcol="probability", col.regions=paletteer::paletteer_d("RColorBrewer::RdYlGn", direction=-1), at=seq(0, 1, by=0.2), layer.name="Landslide Susceptibility 2021")

##Export Probability & Prediction
----------------------------------
write.csv(SU$probability, file="probability2021.csv", row.names= FALSE)

write.csv(SU$prediction, file="prediction2020.csv", row.names= FALSE)



#//////////////////////////////////////
## Model for 2018 (Monsoon) and prior
#///////////////////////////////////////

formula_5000_a = Landslide2 ~
  s(SlopeM, bs="tp", k = 24) + #s() is used to declare the non-linear properties
  s(SlopeR, bs="tp", k = 24)+
  s(SlopeSTD, bs="tp", k = 24)+
  s(SoildepthR, k = 24)+
  s(SoildepthM, k = 24)+
  s(SoilDepthS, k = 24)+
  s(AspectM, k = 24)+
  s(AspectSTD, k = 24)+
  s(PlanCurvR, k = 24)+
  s(PlanCurvM, k = 24)+
  s(ProfCurvR, k = 24)+
  s(ProfCurvM,k = 24)+
  s(RelRelfR, k = 24)+
  s(RelRelfM, k = 24)+
  s(TWIM, k = 24)+
  s(TWIR, k = 24)+
  s(TWISTD, k = 24)+
  s(Rain201317, k = 24)+
  s(Rain2013_1, k = 24)+
  s(SPIR, k = 24)+
  s(SPIM, k = 24)+
  s(SPISTD, k = 24)+
  LULC2017_1+
  NDVI2017_1+
  SoilType
  

# Fit the Model
mod_5000_a = mgcv::gam(formula_5000_a, family = binomial, method="REML", data = SU)

summary(mod_5000_a) 
gam.check(mod_5000_a)

# Fit the model with more iterations if it fails to converge
mod_5000_a = mgcv::gam(formula_5000_a, family = binomial, method="REML", data = SU, control = list(maxit = 500))

# Goodness of fit
library(Metrics)
bias(mod_5000_a$fitted.values, (SU$Landslid_2))
ae(mod_5000_a$fitted.values, (SU$Landslid_2))
#plot(density(ae(Fit.size$fitted.values, log(data4fit$ExpandedSlide))))
mae(mod_5000_a$fitted.values, (SU$Landslid_2))
rmse(mod_5000_a$fitted.values, (SU$Landslid_2))
cor(mod_5000_a$fitted.values, (SU$Landslid_2), method = "pearson")

# partial effects
plot(mod_5000_a, select=1, trans=plogis, shade=T, ylab="") # you can change the select to explore the other nonlinear effects


plot(mod_5000_a, trans=plogis, pages=1, all.terms=T, shade=T, ylab="")

# Another way
windows()
plot(mod_5000_a,shade = TRUE, shade.col = "blue", ylim = c(-10,10))
plot(mod_5000_a,shade = TRUE, shade.col = "blue", select = 32)
dev.off()

summary(mod_5000_a)

## Visualization 3
-------------------
# Draw the plot for the specific term and add theme
plotsm <- draw(mod_5000_a, select = "s(SlopeSTD)") +
  ylim(c(-10, 10)) +
  theme(axis.text = element_text(size = 25))
plotsm


## fitting performance
-----------------------
# to create a new column in your dataset where the probability is stored
SU$probability_a = as.numeric(predict(mod_5000_a, type="response", newdata=SU)) 
myroc_5000_a = roc(response=SU$Landslide2, predictor=SU$probability_a, auc=T)           
plot(myroc_5000_a, main = round(myroc_5000_a$auc, 5))
print(myroc_5000_a$auc)

## validation
--------------
# 10-fold cross-validation
# we divide the dataset into 10 equal parts. We will use 9 of those parts to fit our model and validate over the remaining one
# in a interactive way

# partitions
# the library only works with centroids, but this step is merely for visualization
# what we only need is to defined the partitions
centroids = dplyr::select(SU, CoordX_1, CoordY_1) %>% sf::st_drop_geometry()
partition = partition_cv(SU, nfold = 10, repetition = 1, seed1 = 123) 
plot(partition, centroids, coords = c("CoordX_1", "CoordY_1"), cex = 0.01, pch = 19)

# settings for the loop
# this loop will fit the model over 9 of the defined folds and will predict over the remaining fold
fold = (1:10)
SU$prediction_a = NA
SU_myroc_a = c(NA)
df_a = SU
roc_a = list()

# loop
for (i in fold){
  id.holdout = partition[[1]][[i]]$test
  df_a_train = SU[-id.holdout,]
  df_a_test = SU[id.holdout, ]
  fit_a = mgcv::gam(formula_5000_a, data=df_a_train, family=binomial, method="REML")
  SU$prediction_a[id.holdout] = as.numeric(predict(fit, type="response", newdata=df_a_test))
  roc_a[[i]] = roc(response=df_a_test$Landslide2, predictor=SU$prediction_a[id.holdout], auc=T)
  SU_myroc_a[i] = as.numeric(unlist(roc_a[[i]][9]))
}

# let us check the predictive performance over the 10 testing folds
boxplot(SU_myroc_a)
round(SU_myroc_a,5)
mean(SU_myroc_a)
median(SU_myroc_a)

# we can plot the 10 roc curves, one generated for every testing fold

plot(roc_a[[1]]); plot(roc_a[[2]], add=T); plot(roc_a[[3]], add=T); plot(roc_a[[4]], add=T); plot(roc_a[[5]], add=T)
plot(roc_a[[6]], add=T); plot(roc_a[[7]], add=T); plot(roc_a[[8]], add=T); plot(roc_a[[9]], add=T); plot(roc_a[[10]], add=T)


# VISUALIZATION 
----------------
# to visualize the results of the fit and the predictions
# you can adjust the strect and type of palette according to your interests
mapview(SU, zcol="probability_a", col.regions=paletteer::paletteer_d("RColorBrewer::RdYlGn", direction=-1))+
  mapview(SU, zcol="prediction_a", col.regions=paletteer::paletteer_d("RColorBrewer::RdYlGn", direction=-1))+
  mapview(SU, zcol="Landslid_1", col.regions=c("dodgerblue1", "firebrick1"))

mapview(SU, zcol="probability_a", col.regions=paletteer::paletteer_d("RColorBrewer::RdYlGn", direction=-1), at=seq(0, 1, by=0.2), layer.name="Landslide Susceptibility 2018")


##Export Probability & Prediction
----------------------------------

write.csv(SU$probability_a, file="probability2018.csv", row.names= FALSE)

write.csv(SU$prediction_a, file="prediction2018.csv", row.names= FALSE)


# "All models are wrong, but some are useful" George E.P.Box

## Additional Visualization
----------------------------

#install.packages(gratia)
install.packages("gratia")
library(gratia)
#install.packfages("ggpmisc")
library(ggpmisc)
library(ggplot2)

dev.off()  # Clear the current graphics device


## Visualization 1
draw(mod_5000,select = "s(SlopeM)")
#
plot <- draw(mod_5000,select = "s(SlopeM)")
 
plot + theme(axis.text = element_text(size = 20))
plota <- draw(mod_5000_a,select = "s(SlopeM)")
plota + theme(axis.text = element_text(size = 30))

##
plot + plota + patchwork::plot_layout(ncol = 2)

## Visualization 2
plotsm <- draw(mod_5000_a,select = "s(SlopeM)") + theme(axis.text = element_text(size = 30))
plotsm
class(mod_5000)

## Visualization 3
# Draw the plot for the specific term and add theme
plotsm <- draw(mod_5000_a, select = "s(SlopeR)") +
  ylim(c(-10, 10)) +
  theme(axis.text = element_text(size = 25))
plotsm


#White Background
plotsm <- draw(mod_5000, select = "s(SlopeM)") +
  ylim(c(-10, 10)) +
  theme_minimal() +  # Set the background to white
  theme(
    axis.text = element_text(size = 30),
    panel.background = element_rect(fill = "white"),  # Ensure panel background is white
    plot.background = element_rect(fill = "white")    # Ensure plot background is white
  )
plotsm

plotsr <- draw(mod_5000, select = "s(SlopeR)") +
  ylim(c(-10, 10)) +
  theme_minimal() +  # Set the background to white
  theme(
    axis.text = element_text(size = 30),
    panel.background = element_rect(fill = "white"),  # Ensure panel background is white
    plot.background = element_rect(fill = "white")    # Ensure plot background is white
  )
plotsr

plotst <- draw(mod_5000, select = "s(SlopeSTD)") +
  ylim(c(-10, 10)) +
  theme_minimal() +  # Set the background to white
  theme(
    axis.text = element_text(size = 30),
    panel.background = element_rect(fill = "white"),  # Ensure panel background is white
    plot.background = element_rect(fill = "white")    # Ensure plot background is white
  )
plotst


plotsm + plotsr +plotst + patchwork::plot_layout(ncol = 3)

## Visualization 4
plotsm18 <- draw(mod_5000_a,select = "s(SlopeM)")
plotsm18 + theme(axis.text = element_text(size = 30))


plotsr18 <- draw(mod_5000_a,select = "s(SlopeR)")
plotsr18 + theme(axis.text = element_text(size = 30))

plotst18 <- draw(mod_5000_a,select = "s(SlopeSTD)")
plotst18 + theme(axis.text = element_text(size = 30))

plotsm18 + plotsr18 +plotst18 + patchwork::plot_layout(ncol = 3)


----------
## Visualization 5
# install.packages(gratia)
#install.packages("gratia")
library(gratia)
comp<-compare_smooths(mod_5000,mod_5000_a)
draw(comp)
?draw
-----------------

