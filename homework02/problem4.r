library(faraway)
data(prostate)
attach(prostate)

par(mfrow=c(1,2))
plot(lpsa,lcavol,xlab='lpsa',ylab='lcavol')
abline(lm(lcavol ~ lpsa))
plot(lcavol,lpsa,xlab='lcavol',ylab='lpsa')
abline(lm(lpsa ~ lcavol))

dev.copy(pdf,'two_scatter_plots.pdf',height=4,width=8)
dev.off()


dev.new()
plot(lpsa,lcavol,xlab='lpsa',ylab='lcavol')
abline(lm(lcavol ~ lpsa))
beta = summary(lm(lpsa ~ lcavol))$coefficients[,1]
abline(-beta[1]/beta[2],1/beta[2],lty=8)
legend(0,3.5,c("lcavol ~ lpsa","lpsa ~ lcavol"), lty=c(1,8))

sprintf("(%.3f, %.3f)", mean(lpsa), mean(lcavol)) 

