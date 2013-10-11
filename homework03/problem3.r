data(sat)
attach(sat)

# a
mdl = lm(total~expend+ratio+salary)
summary(mdl)

#b
mdl2 = lm(total~expend+ratio+salary+takers)
summary(mdl2)
anova(mdl2,mdl)
