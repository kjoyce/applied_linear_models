mammals <- read.csv("mammals.txt")
attach(mammals)
lbrain <- log(brain); lbody <- log(body)
lgest <- log(gestatio); llitter <- log(litter)
pairs(cbind(lbrain,lbody,lgest,llitter))
cor(cbind(lbrain,lbody,lgest,llitter))

n <- length(lbrain)                          # Sample size
reg.out <- lm(lbrain~lbody+lgest+llitter)    # Fits the regression model
p <- length(reg.out$coef)                    # Number of parameters
tstar <- qt(0.975,n-p)                       # 95% t-star value for n-p df
(bhat <- reg.out$coef)                       # Model parameter estimates
(seb <- summary(reg.out)$coef[,2])           # Standard errors of betahats
(ci <- cbind(bhat-tstar*seb,bhat+tstar*seb)) # 95% confidence intervals

confint(reg.out)                             # Fast way to get 95% CIs

alpha <- 0.05                                # Sets the desired alpha-level
g <- p                                       # Number of CIs computed
(bonflevel <- 1-alpha/g)                     # Computes the Bonferroni level
confint(reg.out,level=bonflevel)             # Bonferroni joint CIs

library(ellipse)                             # Loads the ellipse library
plot(ellipse(reg.out,c(2,3)),type="l",lwd=2, # Plots the conf. region
  cex.axis=1.5,xlab="Beta1 (Log Body Size)",
  ylab="Beta2 (Log Gestation)",cex.lab=1.6,mgp=c(2.7,1,0))
points(coef(reg.out)[2],coef(reg.out)[3],    # Plots the regression estimates
  pch=16,cex=2)                              #   in the center of ellipse
abline(v=confint(reg.out)[2,],lty=2)         # Individual CI for beta1
abline(h=confint(reg.out)[3,],lty=2)         # Individual CI for beta2
abline(v=confint(reg.out,level=              # Bonferroni-corrected joint
  bonflevel)[2,],lty=3)                      #   CI for beta1
abline(h=confint(reg.out,level=              # Bonferroni-corrected joint
  bonflevel)[3,],lty=3)                      #   CI for beta2
title("Joint Confidence Region for Beta1, Beta2",cex.main=1.6)

# Confidence and Prediction Interval for Log(Brain Weight)
# ======================================================
x0 <- c(1,log(8.5),log(63),log(4))         # Vector of predictor values
(y0 <- t(x0)%*%coef(reg.out))              # Predicted y for x0
tstar <- qt(0.975,n-p)                     # 95% t-star value for n-p df
X <- model.matrix(reg.out)                 # Constructs the X-matrix
(xtxi <- solve(t(X)%*%X))                  # (X'X)-inverse matrix
mse <- summary(reg.out)$sigma^2            # Computes the model MSE
(seci <- sqrt(mse*t(x0)%*%xtxi%*%x0))      # SE of y0hat for CI
(ci <- cbind(y0-tstar*seci,y0+tstar*seci)) # 95% CI for E(y0)
(sepi <- sqrt(mse*(1+t(x0)%*%xtxi%*%x0)))  # SE of y0hat for PI
(pi <- cbind(y0-tstar*sepi,y0+tstar*sepi)) # 95% CI for y0hat 

x0 <- data.frame(lbody=log(8.5),           # Creates a data frame of the
  lgest=log(63),llitter=log(4))            #   predictor values
predict(reg.out,x0,interval="confidence")  # 95% CI for E(y0)
predict(reg.out,x0,interval="prediction")  # 95% PI for y0hat

x02 <- mean(lgest)                         # Set x2 at mean log(gestation)
x03 <- mean(llitter)                       # Set x3 at mean log(litter)
x01 <- seq(-4,8,.01)                       # Set x1 from -4 to 8 by .01
x0 <- data.frame(lbody=x01,lgest=mean(     # Data frame for predictor
  lgest),llitter=mean(llitter))            #   vector of values
cipred <- predict(reg.out,x0,se=T,         # 95% CIs for each value of
  interval="confidence")                   #   log body size
pipred <- predict(reg.out,x0,se=T,         # 95% PIs for each value of
  interval="prediction")                   #   log body size
matplot(x01,cipred$fit,lty=c(1,2,2),type=  # Matrix column plot of conf.
  "l",xlab="Log Body Size",ylab=           #   bands with bands of line
  "Log Brain Weight",cex.lab=1.6,cex.axis= #   type 2.
  1.5,mgp=c(2.7,1,0),col=c(1,1,1),lwd=2)
matplot(x01,pipred$fit,lty=c(1,3,3),       # Adds predcition bands (add=T)
  type="l",col=c(1,1,1),lwd=2,add=T)       #   to existing plot
points(lbody,lbrain,pch=16,cex=1.3)        # Plots the brain and body pts.
legend(2,2,legend=c("Predicted LBS","Confidence Bands","Prediction Bands"),
  lty=c(1,2,3),lwd=c(2,2,2),col=c(1,1,1),cex=1.3)
rug(lbody)                                 # Distribution of log(body)

