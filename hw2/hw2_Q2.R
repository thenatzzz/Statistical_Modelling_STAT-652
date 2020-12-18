help("airquality")

AQ_original = airquality
head(AQ_original,5)

## HW 2 STAT-652 (Lecture 2c)

# Air quality data
############## 1. #################  

# X= Solar.R, Wind, Temp
# Y = Ozone
AQ = AQ_original[,1:4]
head(AQ)


pairs(AQ)

# 1.a)
# ctrl+shift+C to comment code
# par(mfrow=c(3,1))
# par(mar=c(1,1,1,1))

plot_SolarOzone=plot(AQ$Solar.R,AQ$Ozone,
                     main="Air Quality",
                     xlab="Solar radiation (lang)", 
                     ylab="Ozone (ppb)",
                     cex.lab=1.2)
plot_WindOzone=plot(AQ$Wind,AQ$Ozone,
                     main="Air Quality",
                     xlab="Average wind speed (mph)", 
                     ylab="Ozone (ppb )",
                     cex.lab=1.2)
plot_TempOzone=plot(AQ$Temp,AQ$Ozone,
                     main="Air Quality",
                     xlab="Temperature (F)", 
                     ylab="Ozone (ppb)",
                     cex.lab=1.2)
# Restore graphic parameter
par(mfrow=c(1, 1))

# SAVE aspect width: 800, height: 600

################## 2. ##############################
# X against Y 
attach(AQ)
###2.a report 3 slopes and t-values in a table
lm_SolarOzone = lm(Ozone ~ Solar.R, data = AQ)
summary(lm_SolarOzone)

lm_WindOzone = lm(Ozone ~ Wind, data = AQ)
summary(lm_WindOzone)

lm_TempOzone = lm(Ozone ~ Temp, data = AQ)
summary(lm_TempOzone)

###2.b
# par(mar=c(1,1,1,1))
# with(AQ,plot(Solar.R, Ozone))
with(AQ,plot_SolarOzone)
abline(lm_SolarOzone)

with(AQ,plot_WindOzone)
abline(lm_WindOzone)

with(AQ,plot_TempOzone)
abline(lm_TempOzone)

### 3.
# 3D plot xy=Temp,Wind z= Ozone

#
#The list of packages to be loaded. I already have ggplot2 package in my R environment and I need to install rgl package. This code should work fine for both cases.
#
list.of.packages <- c("rgl","ggplot2","knitr","rglwidget")
#
#You should be able to simply reuse the following lines of code as is
#
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
#
if(length(new.packages)) install.packages(new.packages)
#
# By now we have installed the requisite packages. Time to load them .
#
lapply(list.of.packages,function(x){library(x,character.only=TRUE)})
knit_hooks$set(webgl = hook_webgl)

plot3d(AQ$Wind,
       AQ$Temp,
       AQ$Ozone,
       xlab = "Wind", 
       ylab = "Temp", 
       zlab = "Ozone")

### 4.
##4.a)
model_3D <- lm(Ozone ~   Wind + Temp, data = AQ)
summary(model_3D)
##4.b)
plot3d(AQ$Wind,AQ$Temp,AQ$Ozone, type="s", size=0.5, lit=FALSE)

# 
# AQ
# sum(is.na(AQ$Wind))
# sum(is.na(AQ$Temp))
# sum(is.na(AQ$Ozone))
# length(AQ$Ozone)
# 
AQ_dropNaN_Ozone=AQ[complete.cases(AQ$Ozone), ]
# sum(is.na(AQ_dropNaN_Ozone$Ozone))
# sum(is.na(AQ_dropNaN_Ozone$Wind))
# sum(is.na(AQ_dropNaN_Ozone$Temp))
length(AQ_dropNaN_Ozone$Ozone)


install.packages("scatterplot3d")
library(scatterplot3d) # This library will allow us to draw 3d plot

####### STYLE 1 ###########################################
# fit a linear model using lm model and draw a regression plane
plot3d <- scatterplot3d(AQ$Wind,AQ$Temp,AQ$Ozone,
                        angle=55, scale.y=0.7, pch=16, color ="red", main ="Regression Plane")
my.lm<- lm(Ozone ~ Wind +Temp ,data=AQ)
plot3d$plane3d(my.lm, lty.box = "dotted")

####### STYLE 2 ###########################################
# scatterplot
s3d <- scatterplot3d(AQ$Wind,AQ$Temp,AQ$Ozone, pch = 19, type = "p", color = "darkgrey",
                     main = "Regression Plane", grid = TRUE, box = FALSE,  
                     mar = c(2.5, 2.5, 2, 1.5), angle = 55)
# regression plane
s3d$plane3d(my.lm, draw_polygon = TRUE, draw_lines = TRUE, 
            polygon_args = list(col = rgb(.1, .2, .7, .5)))
# overlay positive residuals
wh <- resid(my.lm) > 0
s3d$points3d(AQ$Wind[wh], AQ$Temp[wh], AQ$Ozone[wh], pch = 19)


####### STYLE 3 BEST###########################################

fit1 <- lm(Ozone~Wind+Temp, data = AQ_dropNaN_Ozone)
sp <- scatterplot3d::scatterplot3d(AQ_dropNaN_Ozone$Wind, 
                                   AQ_dropNaN_Ozone$Temp, 
                                   AQ_dropNaN_Ozone$Ozone, 
                                   angle = 45)
sp$plane3d(fit1, lty.box = "solid")#, draw_polygon = TRUE,draw_lines = FALSE,
            # polygon_args = list(col = rgb(.1, .2, .7, .5)) ) # Fill color

orig <- sp$xyz.convert(AQ_dropNaN_Ozone$Wind, 
                       AQ_dropNaN_Ozone$Temp, 
                       AQ_dropNaN_Ozone$Ozone)
plane <- sp$xyz.convert(AQ_dropNaN_Ozone$Wind, 
                        AQ_dropNaN_Ozone$Temp,  fitted(fit1))

i.negpos <- 1 + (resid(fit1) > 0)
segments(orig$x, orig$y, plane$x, plane$y,
         col = c("blue", "red")[i.negpos], 
         lty = 1) # (2:1)[i.negpos]
# install.packages("FactoClass")
sp <- FactoClass::addgrids3d(AQ_dropNaN_Ozone$Wind, 
                             AQ_dropNaN_Ozone$Temp, 
                             AQ_dropNaN_Ozone$Ozone,
                             angle = 45,
                             grid = c("xy", "xz", "yz"))

rgl::plot3d(AQ_dropNaN_Ozone$Wind, 
            AQ_dropNaN_Ozone$Temp, 
            AQ_dropNaN_Ozone$Ozone, type = "p", 
            xlab = "Wind", 
            ylab = "Temp", 
            zlab = "Ozone", site = 5, lwd = 15)
rgl::planes3d(fit1$coefficients["Wind"], 
              fit1$coefficients["Temp"], -1, 
              fit1$coefficients["(Intercept)"], alpha = 0.3, front = "line")
rgl::segments3d(rep(AQ_dropNaN_Ozone$Wind, each = 2),
                rep(AQ_dropNaN_Ozone$Temp, each = 2),
                matrix(t(cbind(AQ_dropNaN_Ozone$Ozone, predict(fit1))), nc = 1),
                col = c("red", "red")[i.negpos],
                lty = 1) # (2:1)[i.negpos]


####### STYLE 4###########################################
# scatterplot
s3d <- scatterplot3d(AQ_dropNaN_Ozone$Wind, 
                     AQ_dropNaN_Ozone$Temp, 
                     AQ_dropNaN_Ozone$Ozone, pch = 19, type = "p", color = "darkgrey",
                     main = "Regression Plane", grid = TRUE, box = FALSE,  
                     mar = c(2.5, 2.5, 2, 1.5), angle = 55)

# compute locations of segments
orig     <- s3d$xyz.convert(AQ_dropNaN_Ozone$Wind, 
                            AQ_dropNaN_Ozone$Temp, 
                            AQ_dropNaN_Ozone$Ozone)
plane    <- s3d$xyz.convert(AQ_dropNaN_Ozone$Wind, 
                            AQ_dropNaN_Ozone$Temp, 
                             fitted(fit1))
i.negpos <- 1 + (resid(fit1) > 0) # which residuals are above the plane?

# draw residual distances to regression plane
segments(orig$x, orig$y, plane$x, plane$y, col = "red", lty = c(2, 1)[i.negpos], 
         lwd = 1.5)

# draw the regression plane
s3d$plane3d(fit1, draw_polygon = TRUE, draw_lines = TRUE, 
            polygon_args = list(col = rgb(0.7, 0.7, 0.7, 0.7)))

# redraw positive residuals and segments above the plane
wh <- resid(fit1) > 0
segments(orig$x[wh], orig$y[wh], plane$x[wh], plane$y[wh], col = "red", lty = 1, lwd = 1.5)
s3d$points3d(AQ_dropNaN_Ozone$Wind[wh], 
             AQ_dropNaN_Ozone$Temp[wh], 
             AQ_dropNaN_Ozone$Ozone[wh], pch = 19)

# REF: https://stackoverflow.com/questions/47344850/scatterplot3d-regression-plane-with-residuals



# CLEAN UP #################################################

# Clear packages
detach("package:datasets", unload = TRUE)  # For base

# Clear plots
dev.off()  # But only if there IS a plot

# Clear console
cat("\014")  # ctrl+L
