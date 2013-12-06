data(warpbreaks)
attach(warpbreaks)
interaction.plot(tension,wool,breaks,xlab="Loom Tension",
  ylab="No. Breaks",cex.lab=1.6,cex.axis=1.5,lwd=2,
  main="Interaction Plot",cex.main=1.8)
dev.copy(pdf,"wool_interaction1.pdf")
dev.off()
interaction.plot(wool,tension,breaks,xlab="Loom Tension",
  ylab="No. Breaks",cex.lab=1.6,cex.axis=1.5,lwd=2,
  main="Interaction Plot",cex.main=1.8)
dev.copy(pdf,"wool_interaction2.pdf")
dev.off()
#leg.names <- c("Undergraduate","Graduate")
#legend(1,39,leg.names,lty=c(1,8),lwd=2,cex=1.5)

reg = lm(breaks ~ tension*wool) # fit two way anova
anova(reg)			# report the results
qqnorm(resid(reg))		# qqplot to check normality
plot(fitted(reg),resid(reg))    # check var. homogeneity
plot(fitted(reg),resid(reg),xlab="Predicted No. Breaks",ylab="Residuals",cex.lab=1.6,cex.axis=1.5,pch=16,cex=1.3,mgp=c(2.7,1,0),main="Residual Plot",cex.main=1.8)	  # make a pretty one for the report
dev.copy(pdf,"wool_resid1.pdf") 
dev.off()
reg2 = lm(log(breaks) ~ tension*wool)
plot(fitted(reg2),resid(reg2),xlab="Predicted log No. Breaks",ylab="Residuals",cex.lab=1.6,cex.axis=1.5,pch=16,cex=1.3,mgp=c(2.7,1,0),main="Residual Plot",cex.main=1.8)	  # make a pretty one for the report
dev.copy(pdf,"wool_resid2.pdf") 
dev.off()
interaction.plot(tension,wool,log(breaks),xlab="Loom Tension",
  ylab="log No. Breaks",cex.lab=1.6,cex.axis=1.5,lwd=2,
  main="Interaction Plot",cex.main=1.8)
dev.copy(pdf,"wool_interaction3.pdf")
dev.off()

aov(reg2)
summary(reg2)
TukeyHSD(aov(reg2))
