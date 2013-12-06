age <- c(15,26,10,9,15,20,18,11,8,20,  # Defines a vector "age" with the ages at which
  7,9,10,11,11,10,12,42,17,11,10)      #   the first word was spoken.
score <- c(95,71,83,91,102,87,93,100,  # Defines a vector "score" with the Gesell
  104,94,113,96,83,84,102,100,105,57,  #   adaptive scores.
  121,86,100)
plot(age,score,pch=16,cex=2,xlab=      # Makes a scatterplot of score vs. age, with
  "Age at First Word (Months)",ylab=   #   axis labels and character size 2.
  "Gesell Adaptive Score",cex.axis=1.5,
  mgp=c(2.7,1,0),cex.lab=1.6,
  main="Gesell Adaptive Score vs. Age at First Word",cex.main=1.6)
reg <- lm(score ~ age)                 # Stores regression of score on age in "reg".
abline(reg,lwd=2)                      # Plots the regression line.
reg2 <- lm(score[-18] ~ age[-18])      # Runs regression omitting 18th case.
abline(reg2,lwd=2)                     # Plots the resulting regression line.
reg3 <- lm(score[-19] ~ age[-19])      # Runs regression omitting 19th case.
abline(reg3,lwd=2)                     # Plots the resulting regression line.

plot(reg$fitted,reg$resid,pch=16,      # Makes a residual plot.
  xlab="Predicted Scores",cex=2.0,
  ylab="Residuals",cex.axis=1.5,
  cex.lab=1.6,mgp=c(2.7,1,0),
  main="Residual Plot for Gesell Data",cex.main=1.8)
abline(h=0,lwd=2,lty=2)                # Plots the zero-line.

reg.inf <- influence(reg)              # Computes influence statistics
(round(reg.inf$hat,digits=3))          # Prints the model leverages
(sum(reg.inf$hat))                     # Sums of the model leverages
sigma <- summary(reg)$sigma            # Residual SE
seres <- sigma*sqrt(1-reg.inf$hat)     # SE of the residuals
standres <- reg$resid/seres            # Standardized residuals
X <- cbind(1,age)                      # X-matrix
hi <- diag(X%*%solve(t(X)%*%X)%*%t(X)) # Computing leverages from hat matrix

cases <- 1:21                          # Labels the cases from 1-21
library(faraway)                       # Loads the faraway library
halfnorm(lm.influence(reg)$hat,labs=   # Half-normal plot of leverages to look
  cases,ylab="Leverages",cex.lab=1.6,  #   for influential points
  cex.axis=1.5,mgp=c(2.7,1,0),main=
  "Half-Normal Plot of Leverages",cex.main=1.8)

jack <- rstudent(reg)                  # Studentized deleted residuals
jack[which.max(abs(jack))]             # Finds the max Studentized deleted resid
n <- 21; p <- 2                        # Sample size, number of parameters
qt(1-.05/(2*n),n-p)                    # Critical t-value at .05

# SAT Example
# ===========
sat <- read.csv("Data/satscore.txt")
satnew <- data.frame(sat[,c(1,2)],logtak=log(sat$takers),
  sat[,4:8])
reg.out <- lm(sat~logtak+years+expend,data=satnew)
reg.inf <- influence(reg.out)
sigma <- summary(reg.out)$sigma
standres <- resid(reg.out)/(sigma*sqrt(1-reg.inf$hat))
studdelres <- rstudent(reg.out)
par(mfrow=c(3,3))
plot(satnew$years,standres,xlab="Years",ylab="Standardized Residuals",
  pch=16,cex=1.3,main="Standardized Residuals vs. Years",cex.main=1.0)
plot(satnew$expend,standres,xlab="Expenditures",ylab="Standardized Residuals",
  pch=16,cex=1.3,main="Standardized Residuals vs. Expend",cex.main=1.0)
plot(satnew$logtak,standres,xlab="Log(% Takers)",ylab="Standardized Residuals",
  pch=16,cex=1.3,main="Standardized Residuals vs. LogTak",cex.main=1.0)
plot(satnew$years,studdelres,xlab="Years",ylab="Studentized Del. Residuals",
  pch=16,cex=1.3,main="Stud.Del. Residuals vs. Years",cex.main=1.0)
plot(satnew$expend,studdelres,xlab="Expenditures",ylab="Studentized Del. Residuals",
  pch=16,cex=1.3,main="Stud.Del. Residuals vs. Expend",cex.main=1.0)
plot(satnew$logtak,studdelres,xlab="Log(% Takers)",ylab="Studentized Del. Residuals",
  pch=16,cex=1.3,main="Stud.Del. Residuals vs. LogTak",cex.main=1.0)
plot(reg.out$fitted,standres,xlab="Predicted Values",ylab="Standardized Residuals",
  pch=16,cex=1.3,main="Residual Plot",cex.main=1.0)
plot(0,0,axes=F,xlab="",ylab="",pch=" ")
plot(reg.out$fitted,studdelres,xlab="Predicted Values",ylab="Studentized Del. Residuals",
  pch=16,cex=1.3,main="Residual Plot",cex.main=1.0)

