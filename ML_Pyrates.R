#==================================================================================
# R code for landslide susceptibility modeling using Machine Learning Algorithm
# Last modified:  October 2024
# The code is distributed "as is", WITH NO WARRANTY whatsoever!
#==================================================================================

## Installing/importing required libraries/packages
---------------------------------------------------

#install.packages("tidyverse")
library(tidyverse)
# Key Libraries
library(lattice)
library(ggplot2)
#install.packages("caret")
library(caret)

## Data Import
---------------

setwd("C:/Users/Dewan/Desktop/Slope Units/R") 
data <-  read.csv("C:/Users/Dewan/Desktop/Slope Units/R/Theend17082023csv.csv", header = T)


## Splitting data into training and testing
--------------------------------------------
# Use 60% of data set as training set and remaining 40% as testing set

sample <- sample(c(TRUE, FALSE), nrow(data), replace=TRUE, prob=c(0.6,0.4))
train  <- data[sample, ]
test   <- data[!sample, ]

## Dimension of data
---------------------
dim(train)
dim(test)

## Scale the data for standardization
-------------------------------------

#train_sorted<-train[,-c(1,2,4,5,6,7,8,9,12,18,19,24,27,30,37,38,39,40,41,44,45)]
# Finding the maximum and minimum values except the first column
maxs <- apply(train [c(-1)], 2, max)
mins <- apply(train[c(-1)], 2, min)
# Scaling the data between 0 and 1
scaled_train <- as.data.frame(scale(train[c(-1)], center = mins, scale = maxs - mins))
# Combining the target column
scaled_train=cbind(scaled_train,train$Landslide)
# Renaming the column
colnames(scaled_train)[28]<-"Slide2020"

##Fit the Model
----------------
ctrl <- trainControl(method="cv", summaryFunction=twoClassSummary, classProbs=T,savePredictions = TRUE)


## Model Training
------------------

# Random Forest(rf)
tuneGrid.rf <- expand.grid(mtry = c(2, 4, 6, 8))
fit.rf <- train(Slide2020~., data=scaled_train, method="rf", metric="ROC", tuneGrid=tuneGrid.rf, trControl=ctrl)
# Support Vector Machine (svmRadial)
tuneGrid.svm <- expand.grid(C = c(0.1, 1, 10), sigma = c(0.01, 0.05, 0.1))
fit.svm <- train(Slide2020~., data=scaled_train, method="svmRadial", metric="ROC", tuneGrid=tuneGrid.svm, trControl=ctrl)
# Neural Network(nnet)
tuneGrid.nnet <- expand.grid(size = c(3, 5, 7), decay = c(0.001, 0.1))
fit.nnet <- train(Slide2020~., data=scaled_train, method="nnet", metric="ROC", tuneGrid=tuneGrid.nnet, trace=FALSE, trControl=ctrl, maxit=100)
# XGBoost(xgbLinear)
fit.XGB<- train(Slide2020~., data=scaled_train, method="xgbLinear", metric="ROC", trControl=ctrl)
# Linear Discriminant Analysis(lda)
fit.lda <- train(Slide2020~., data=scaled_train, method="lda", metric="ROC", trControl=ctrl)

# Also better to know deafualt set up. Not to run 
fit.rf <- train(Slide2020~., data=scaled_train, method="rf", metric="ROC",trControl=ctrl)                                         
fit.svm <- train(Slide2020~., data=scaled_train, method="svmRadial", metric="ROC", trControl=ctrl)
fit.lda <- train(Slide2020~., data=scaled_train, method="lda", metric="ROC", trControl=ctrl)
#fit.cart<- train(Training~., data=scaled_train, method="rpart", metric="ROC", trControl=ctrl)
fit.nnet<- train(Slide2020~., data=scaled_train, method="nnet",trace=FALSE,metric="ROC", trControl=ctrl)
fit.XGB<- train(Slide2020~., data=scaled_train, method="xgbLinear", metric="ROC", trControl=ctrl)
#fit.Boost<- train(Slide2020~., data=scaled_train, method="adaboost", metric="ROC", trControl=ctrl)

## Train Model Performance
---------------------------
# Loading relevant packages
install.packages("MLeval")
library(MLeval)
#res<-evalm(list(fit.cart,fit.rf,fit.svm,fit.nnet,fit.lda),gnames = c("cart","rf","svm","nnet","lda"),rlinethick=1,fsize=8)
res<-evalm(list(fit.rf),gnames = c("rf"),rlinethick=1,fsize=8)
res<-evalm(list(fit.svm),gnames = c("svm"),rlinethick=1,fsize=8)
res<-evalm(list(fit.lda),gnames = c("lda"),rlinethick=1,fsize=8)
res<-evalm(list(fit.nnet),gnames = c("nnet"),rlinethick=1,fsize=8)
res<-evalm(list(fit.XGB),gnames = c("xgbLinear"),rlinethick=1,fsize=8)
res <- evalm(list(fit.rf, fit.svm, fit.nnet, fit.lda, fit.XGB),
             gnames = c("rf", "svm", "nnet", "lda", "xgbLinear"),
             rlinethick = 1, fsize = 8)
# View the optimal results
res$optres
# Covariate importance
varImp(fit.rf)
plot(varImp(fit.rf))
plot(varImp(fit.nnet))

#Optional: Confusion Matrix
#confusionMatrix.train(fit.cart)
confusionMatrix.train(fit.rf)
confusionMatrix.train(fit.svm)
confusionMatrix.train(fit.nnet)
confusionMatrix.train(fit.lda)


## Model Testing
-----------------

#test_sorted<-test[,-c(1,2,4,5,6,7,8,9,12,18,19,24,27,30,37,38,39,40,41,44,45)]
maxs <- apply(test [c(-1)], 2, max)
mins <- apply(test[c(-1)], 2, min)
scaled_test <- as.data.frame(scale(test[c(-1)], center = mins, scale = maxs - mins))

scaled_test=cbind(scaled_test,test$Landslide)
colnames(scaled_test)[28]<-"Slide2020"


## Prediction using Testing Data
---------------------------------

# Importing relevant packages
library(ROCR)
library(pROC)
#Probabilistic preditions
p_rf_test <- predict(fit.rf, scaled_test, type = "prob")
p_svm_test <- predict(fit.svm, scaled_test, type = "prob")
p_nnet_test <- predict(fit.nnet, scaled_test, type = "prob")
p_lda_test <- predict(fit.lda, scaled_test, type = "prob")
p_xgb_test <- predict(fit.XGB, scaled_test, type = "prob")


# Predictions for the test data
p_rf_test <- predict(fit.rf, scaled_test, type = "prob")
p_svm_test <- predict(fit.svm, scaled_test, type = "prob")
p_nnet_test <- predict(fit.nnet, scaled_test, type = "prob")
p_lda_test <- predict(fit.lda, scaled_test, type = "prob")
p_xgb_test <- predict(fit.XGB, scaled_test, type = "prob")

# Plot ROC curves for each model and extract AUC
roc_rf <- roc(scaled_test$Slide2020, p_rf_test[,"YES"], plot=TRUE, col="green", lwd=3, legacy.axes=TRUE, main="ROC Curves")
roc_nnet <- roc(scaled_test$Slide2020, p_nnet_test[,"YES"], plot=TRUE, col="blue", lwd=3, legacy.axes=TRUE, add=TRUE)
roc_svm <- roc(scaled_test$Slide2020, p_svm_test[,"YES"], plot=TRUE, col="purple", lwd=3, legacy.axes=TRUE, add=TRUE)
roc_lda <- roc(scaled_test$Slide2020, p_lda_test[,"YES"], plot=TRUE, col="red", lwd=3, legacy.axes=TRUE, add=TRUE)
roc_xgb <- roc(scaled_test$Slide2020, p_xgb_test[,"YES"], plot=TRUE, col="black", lwd=3, legacy.axes=TRUE, add=TRUE)

# Dynamically generate AUC values
auc_rf <- auc(roc_rf)
auc_nnet <- auc(roc_nnet)
auc_svm <- auc(roc_svm)
auc_lda <- auc(roc_lda)
auc_xgb <- auc(roc_xgb)

# Plot an ROC curve (or any other plot first)
par(pty="s")

plot(roc_nnet, col="blue", lwd=3, legacy.axes=TRUE, main="ROC Curves")

# Add other ROC curves to the same plot
plot(roc_rf, col="green", lwd=3, add=TRUE)
plot(roc_lda, col="red", lwd=3, add=TRUE)
plot(roc_svm, col="black", lwd=3, add=TRUE)
plot(roc_xgb, col="purple", lwd=3, add=TRUE)

# Add a legend with dynamically calculated AUC values
legend("bottomright", legend=c(
  paste("NNET_AUC", round(auc_nnet, 2)),
  paste("RF_AUC", round(auc_rf, 2)),
  paste("LDA_AUC", round(auc_lda, 2)),
  paste("SVM_AUC", round(auc_svm, 2)),
  paste("XGB_AUC", round(auc_xgb, 2))),
  col=c("blue","green","red","black","purple"),
  lty=1, lwd=4, cex=0.8)

## Produce Landslide Susceptibility map using training model results and raster layers data
-------------------------------------------------------------------------------------------
# Import necessary libraries
install.packages("raster")
install.packages("rgdal")
# Install terra package (replaces rgdal and raster)
install.packages("terra")
# Load necessary libraries
library(terra)
library(raster)
library(rgdal)

#Load the Raster data
# To check file exists
file.exists("C:/Users/Dewan/Desktop/Slope Units/R/MLFactor_test/ElevationM.tif")

ElevationR = raster("C:/Users/Dewan/Desktop/Slope Units/R/MLFactor_test/ElevationR.tif")
ElevationM = raster("C:/Users/Dewan/Desktop/Slope Units/R/MLFactor_test/ElevationM.tif")  
SlopeR= raster("C:/Users/Dewan/Desktop/Slope Units/R/MLFactor_test/SlopeR.tif")
SlopeM= raster("C:/Users/Dewan/Desktop/Slope Units/R/MLFactor_test/SlopeM.tif")
SlopeSTD= raster("C:/Users/Dewan/Desktop/Slope Units/R/MLFactor_test/SlopeSTD.tif")
SoildepthR= raster("C:/Users/Dewan/Desktop/Slope Units/R/MLFactor_test/SoildepthR.tif")
SoildepthM= raster("C:/Users/Dewan/Desktop/Slope Units/R/MLFactor_test/SoildepthM.tif")
SoilDepthSTD= raster("C:/Users/Dewan/Desktop/Slope Units/R/MLFactor_test/SoilDepthSTD.tif")
AspectM= raster("C:/Users/Dewan/Desktop/Slope Units/R/MLFactor_test/AspectM.tif")
AspectSTD= raster("C:/Users/Dewan/Desktop/Slope Units/R/MLFactor_test/AspectSTD.tif")
PlanCurvM= raster("C:/Users/Dewan/Desktop/Slope Units/R/MLFactor_test/PlanCurvM.tif")
PlanCurvR= raster("C:/Users/Dewan/Desktop/Slope Units/R/MLFactor_test/PlanCurvR.tif")
ProfCurvM= raster("C:/Users/Dewan/Desktop/Slope Units/R/MLFactor_test/ProfCurvM.tif")
ProfCurvR= raster("C:/Users/Dewan/Desktop/Slope Units/R/MLFactor_test/ProfCurvR.tif")
RelRelfM= raster("C:/Users/Dewan/Desktop/Slope Units/R/MLFactor_test/RelRelfM.tif")
RelRelfR= raster("C:/Users/Dewan/Desktop/Slope Units/R/MLFactor_test/RelRelfR.tif")
TWIM= raster("C:/Users/Dewan/Desktop/Slope Units/R/MLFactor_test/TWIM.tif")
TWIR= raster("C:/Users/Dewan/Desktop/Slope Units/R/MLFactor_test/TWIR.tif")
TWISTD= raster("C:/Users/Dewan/Desktop/Slope Units/R/MLFactor_test/TWISTD.tif")
SoilType= raster("C:/Users/Dewan/Desktop/Slope Units/R/MLFactor_test/SoilType.tif")
DistRM= raster("C:/Users/Dewan/Desktop/Slope Units/R/MLFactor_test/DistRM.tif")
DistRR= raster("C:/Users/Dewan/Desktop/Slope Units/R/MLFactor_test/DistRR.tif")
Rain201321R= raster("C:/Users/Dewan/Desktop/Slope Units/R/MLFactor_test/Rain201321R.tif")
Rain201321MAX= raster("C:/Users/Dewan/Desktop/Slope Units/R/MLFactor_test/Rain201321MAX.tif")
LULC20172021= raster("C:/Users/Dewan/Desktop/Slope Units/R/MLFactor_test/LULC20172021.tif")
NDVI20172021= raster("C:/Users/Dewan/Desktop/Slope Units/R/MLFactor_test/NDVI20172021.tif")
SPIR= raster("C:/Users/Dewan/Desktop/Slope Units/R/MLFactor_test/SPIR.tif")
SPIM= raster("C:/Users/Dewan/Desktop/Slope Units/R/MLFactor_test/SPIM.tif")
SPISTD= raster("C:/Users/Dewan/Desktop/Slope Units/R/MLFactor_test/SPISTD.tif")

# check attributes and projection and extent
extent(ElevationM)
extent(ElevationR)
extent(SlopeM)
extent(SlopeR)
extent(SlopeSTD)
extent(SoildepthM)
extent(SoildepthR)
extent(AspectM)
extent(AspectSTD)
extent(PlanCurvM)
extent(PlanCurvR)
extent(ProfCurvM)
extent(ProfCurvR)
extent(RelRelfM)
extent(RelRelfR)
extent(TWIM)
extent(TWIR)
extent(TWISTD)
extent(SoilType)
extent(DistRM)
extent(DistRR)
extent(Rain201321MAX)
extent(Rain201321R)
extent(SPIR)
extent(SPIM)
extent(SPISTD)
extent(NDVI20172021)
extent(LULC20172021)
extent(SoilDepthSTD)

# If different extent, then resample them using the smallest area
DistRR_a <- resample(DistRR,ElevationM, resample='bilinear')
DistRM_a <- resample(DistRM,ElevationM, resample='bilinear')
SoilType_a <- resample(SoilType,ElevationM, resample='bilinear')
TWIM_a <- resample(TWIM,ElevationM, resample='bilinear')
TWIR_a <- resample(TWIR,ElevationM, resample='bilinear')
TWISTD_a <- resample(TWISTD,ElevationM, resample='bilinear')
RelRelfM_a <- resample(RelRelfM,ElevationM, resample='bilinear')
RelRelfR_a <- resample(RelRelfR,ElevationM, resample='bilinear')
NDVI20172021_a <- resample(NDVI20172021,ElevationM, resample='bilinear')
LULC20172021_a <- resample(LULC20172021,ElevationM, resample='bilinear')
#landuse_diff_r <- resample(landuse_diff,elevation, resample='bilinear') 
#geology_r <- resample(geology,elevation, resample='bilinear') 
#waterbodies_r <- resample(waterbodies,elevation, resample='bilinear') 

# check the new extent
extent(RelRelfM_a)

# write to a new geotiff file
writeRaster(DistRR_a,filename="resampled/DistRR.tif", format="GTiff", overwrite=TRUE) 
writeRaster(DistRM_a,filename="resampled/DistRM.tif", format="GTiff", overwrite=TRUE)
writeRaster(SoilType_a,filename="resampled/SoilType.tif", format="GTiff", overwrite=TRUE)
writeRaster(TWIM_a,filename="resampled/TWIM.tif", format="GTiff", overwrite=TRUE)
writeRaster(TWIR_a,filename="resampled/TWIR.tif", format="GTiff", overwrite=TRUE)
writeRaster(TWISTD_a,filename="resampled/TWISTD.tif", format="GTiff", overwrite=TRUE)
writeRaster(RelRelfM_a,filename="resampled/RelRelfM.tif", format="GTiff", overwrite=TRUE)
writeRaster(RelRelfR_a,filename="resampled/RelRelfR.tif", format="GTiff", overwrite=TRUE)
writeRaster(NDVI20172021_a,filename="resampled/NDVI20172021.tif", format="GTiff", overwrite=TRUE)
writeRaster(LULC20172021_a,filename="resampled/LULC20172021.tif", format="GTiff", overwrite=TRUE)
#writeRaster(waterbodies_r,filename="resampled/waterbodies.tif", format="GTiff", overwrite=TRUE)

## stack multiple raster files
Stack_List= list.files(path = "resampled/",pattern = "tif$", full.names = TRUE)

Rasters=stack(Stack_List)
names(Rasters)

#library(ggplot2)
#library(grDevices)
#library(sp)
#library(raster)
library(rasterVis)
#library(ROCR)
#library(rgdal)
#library(caTools)
#library(gtools)
#library(MASS)

# scale the raster stack data
scaled_raster<-scale(Rasters,center = TRUE,scale = TRUE)
# Downscale the resolution of the raster
New<-aggregate(scaled_raster,10)

## predict the map
-----------------------
svm1<-1-raster::predict(New,fit.svm,type="prob")
plot(svm1,col=terrain.colors(100))


lda<-1-raster::predict(New,fit.lda,type="prob")
plot(lda,col=terrain.colors(100))

#cart<-1-raster::predict(New,fit.cart,type="prob")
#plot(cart,col=terrain.colors(100))

rf<-1-raster::predict(New,fit.rf,type="prob")
plot(rf,col=terrain.colors(100))

nnet<-1-raster::predict(New,fit.nnet,type="prob")
plot(nnet)


xgb<-1-raster::predict(New,fit.XGB,type="prob")
plot(xgb)

#save the map
-------------
proj4string(rf)=CRS( "+proj=utm +zone=46 +datum=WGS84 +units=m +no_defs")
writeRaster(rf,filename = "final prediction map/rf.tif", format="GTiff", overwrite=TRUE)

proj4string(nnet)=CRS( "+proj=utm +zone=46 +datum=WGS84 +units=m +no_defs")
writeRaster(nnet,filename = "final prediction map/nnet.tif", format="GTiff", overwrite=TRUE)

proj4string(lda)=CRS( "+proj=utm +zone=46 +datum=WGS84 +units=m +no_defs")
writeRaster(lda,filename = "final prediction map/lda.tif", format="GTiff", overwrite=TRUE)
