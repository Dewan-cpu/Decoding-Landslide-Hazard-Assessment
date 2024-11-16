## Load Library
library(mgcv)

##############################
### Set the work directory ###
##############################
setwd("C:/Users/Dewan/Desktop/Slope Units/R")
data4fit = read.delim("Theend17082023 - Copy.txt", 
                      sep = "\t",
                      header = T)

################################################
### Let's explore the covariate information ###
################################################

smoothScatter(data4fit$CoordX,data4fit$CoordY)
plot(density(data4fit$SlopeM))

####################################################
### Let's define the structure of our GAM model ### 
####################################################
num.baseFun = 26
Formula.size = log(ExpandedSL2020) ~ 
  s(SlopeR, k = num.baseFun) +
  s(SlopeM, k = num.baseFun) +
  s(SlopeSTD, k = num.baseFun) +
  s(SoildepthR, k = num.baseFun) +
  s(SoildepthM, k = num.baseFun) +
  s(SoilDepthSTD, k = num.baseFun) +
  s(AspectM, k = num.baseFun) +
  s(AspectSTD, k = num.baseFun) +
  s(PlanCurvM, k = num.baseFun) +
  s(PlanCurvR, k = num.baseFun)+
  s(ProfCurvM, k = num.baseFun) +
  s(ProfCurvR, k = num.baseFun)+
  s(RelRelfM, k = num.baseFun) +
  s(RelRelfR, k = num.baseFun)+
  s(TWIM, k = num.baseFun) +
  s(TWIR, k = num.baseFun) +
  s(TWISTD, k = num.baseFun) +
  s(DistRR, k = num.baseFun) +
  s(DistRM, k = num.baseFun) +
  s(SPIR, k = num.baseFun) +
  s(SPIM, k = num.baseFun) +
  s(SPISTD, k = num.baseFun) +
  s(Rain201321MAX, k = num.baseFun) +
  s(Rain201321R, k = num.baseFun) +
  as.factor(SoilType) +
  as.factor(LULC20172021) +
  as.factor(NDVI20172021) 


##############################################################
### Let's run the GAM model following the structure above ### 
##############################################################
Fit.size = mgcv::gam(Formula.size, family = "gaussian", data = data4fit)

########################################################################
### Explore the correlation between observed and estimated landslide areas ### 
########################################################################

### Here's a representation 
library(ggplot2)
data.fitplot = data.frame(obs = log(data4fit$ExpandedSL2020), fit = Fit.size$fitted.values)
data.lineplot = data.frame(linx = c(0,15), liny = c(0,15))
#windows()

ggplot(data.fitplot, aes(x=obs, y=fit)) + xlim(0, 9) + ylim(0, 9)+
  labs(x=expression("Observed"~"["*"log"*"("*"m"^"2"*")"*"]"), 
       y=expression("Fitted"~"["*"log"*"("*"m"^"2"*")"*"]"))+
  geom_bin2d(bins = 100) + 
  scale_fill_continuous(type = "viridis") + theme_bw() +
  theme(axis.text=element_text(size=16, family = "serif", color = "black"), 
        axis.title=element_text(size=20,face="bold", family = "serif"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.border = element_rect( colour = "black",size = 0.8),
        legend.position = c(0.91,0.16),
        legend.text = element_text(size=12, family = "serif", color = "black"),
        legend.title = element_text(size=16, family = "serif", color = "black"))+ 
  geom_abline(intercept = 0, slope = 1, colour="red", linetype="dashed",size=1)+
  labs(fill='Count')



# QQ plot with gratia
library(gratia)

qq_plot(Fit.size)

# Histogram of residuals
hist(residuals(Fit.size), 
     breaks = 25, 
     main = "Histogram of Residuals", 
     xlab = "Residuals", col = "lightblue", border = "black")

# Histogram with gratia
appraise(Fit.size)


