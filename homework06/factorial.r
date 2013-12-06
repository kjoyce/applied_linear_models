# Teaching Style Example - Factorial Design Notes
# ===============================================
scores <- c(60,55,40,50,45,30)
teachnames <- c("Lecture","Activity","Self")
studnames <- c("Graduate","Undergraduate")
teach <- rep(teachnames,2)
student <- rep(studnames,each=3)
interaction.plot(teach,student,scores,legend=F,xlab="Teaching Style",
  ylab="Mean Improvement Score",cex.lab=1.6,cex.axis=1.5,lwd=2,
  main="Interaction Plot",cex.main=1.8)
leg.names <- c("Undergraduate","Graduate")
legend(1,39,leg.names,lty=c(1,8),lwd=2,cex=1.5)

interaction.plot(student,teach,scores,legend=F,xlab="Student Type",
  ylab="Mean Improvement Score",cex.lab=1.6,cex.axis=1.5,lwd=2,
  main="Interaction Plot",cex.main=1.8)
leg.names <- c("Lecture","Activity","Self Study")
legend(1.2,47,leg.names,lty=c(2,3,1),lwd=2,cex=1.5)

scores[4] <- 30
interaction.plot(teach,student,scores,legend=F,xlab="Teaching Style",
  ylab="Mean Improvement Score",cex.lab=1.6,cex.axis=1.5,lwd=2,
  main="Interaction Plot",cex.main=1.8)
leg.names <- c("Undergraduate","Graduate")
legend(1.4,47,leg.names,lty=c(1,8),lwd=2,cex=1.5)

interaction.plot(student,teach,scores,legend=F,xlab="Student Type",
  ylab="Mean Improvement Score",cex.lab=1.6,cex.axis=1.5,lwd=2,
  main="Interaction Plot",cex.main=1.8)
leg.names <- c("Lecture","Activity","Self Study")
legend(1.6,60,leg.names,lty=c(2,3,1),lwd=2,cex=1.5)

scores <- c(58,51,43,52,46,30)
interaction.plot(teach,student,scores,legend=F,xlab="Teaching Style",
  ylab="Mean Improvement Score",cex.lab=1.6,cex.axis=1.5,lwd=2,
  main="Interaction Plot",cex.main=1.8)
leg.names <- c("Undergraduate","Graduate")
legend(1.0,40,leg.names,lty=c(1,8),lwd=2,cex=1.5)

teach <- factor(teach,levels=teachnames)
student <- factor(student,levels=studnames)
study <- data.frame(scores,teach,student)
reg1 <- lm(scores~teach*student,data=study)
summary(reg1)

reg2 <- lm(scores~teach+student,data=study)
summary(reg2)
teachcoefs <- rep(c(0,coef(reg2)[2:3]),2)
studcoefs <- rep(c(0,coef(reg2)[4]),each=3)
reg2b <- update(reg2,.~.+I(teachcoefs*studcoefs))
anova(reg2b)

anova(reg2)


# Orange Tree Example - Factorial Design notes
# ============================================
diam <- c(6.2,6.9,7.3,7.1,7.4,7.5,7.6,7.2,7.4,7.2,7.5,7.2,
          7.4,7.0,7.6,7.4,7.8,7.3,7.6,7.5,7.8,7.4,7.0,6.9,
          6.3,6.7,6.1,7.3,7.5,7.2,7.2,7.3,7.0,6.8,6.6,6.4)
pH <- as.factor(rep(rep(c(4,5,6,7),rep(3,4)),3))
calc <- as.factor(rep(c(100,200,300),rep(12,3)))
orange <- data.frame(diam=diam,pH=pH,calc=calc)

interaction.plot(calc,pH,diam,lwd=2,xlab="Calcium Level (lbs per acre)",
  ylab="Trunk Diameter Increase",cex.lab=1.6,cex.axis=1.5,cex=1.5,
  main="Interaction Plot",cex.main=1.8,legend=F)
legend(1.5,6.9,legend=c("4.0","5.0","6.0","7.0"),title="Soil pH",
  cex=1.5,lty=c(4,3,8,1),lwd=c(2,2,2,2))

reg1 <- lm(diam~calc*pH)
anova(reg1)

qqnorm(resid(reg1))
plot(fitted(reg1),resid(reg1))

TukeyHSD(aov(diam~calc+pH))


