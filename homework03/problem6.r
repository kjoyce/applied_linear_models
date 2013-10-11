shipment = read.csv('shipment.txt')
attach(shipment)
mdl = lm(time~drums+weight)
X = model.matrix(mdl)
t(X)%*%X 

X <- model.matrix(mdl)
solve(t(X)%*%X)*summary(mdl)$sigma^2 

