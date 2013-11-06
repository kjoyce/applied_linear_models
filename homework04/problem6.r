rm(list = ls())
library(faraway) 
library(leaps)
library(MPV)
data(divusa)

# a
subs = regsubsets(divorce~.,data=divusa,method="backward")
(rs = summary(subs))

# b-d
n <- length(divusa$divorce); p <- 2:ncol(divusa)           # Sets n=50, p=(2,3,4,5,6,7)
(aic <- n*log(rs$rss/n)+2*p)      # AIC statistic for the 6 models
(rs$adjr2)                  # Adjusted R-squared for the 6 models
(rs$cp)                     # Cp-statistic for the 6 models

tab = cbind(aic=aic,adjr2=rs$adjr2,cp=rs$cp,p=2:7)
tab = tab[6:1,]

stab = matrix(sprintf("%.4g &",as.matrix(tab[,1:3])),nrow=nrow(tab)) 
stab = cbind(stab,sprintf("%.4g \\\\ \n",tab[,4]))
cat(t(stab))
