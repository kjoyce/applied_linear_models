rm(list = ls())
inf_table = data.frame(read.csv("influence_problem.csv",header=T)) 
n = 55
p = 5
tstar = qt(1-.025/n,n-p-1)

h_idx = inf_table$h_i >= 2*p/n
c_idx = inf_table$Cook.s.D >= 4/n
tr_idx = abs(inf_table$t_i) >= tstar

stab = matrix(sprintf("%.3f & ",as.matrix(inf_table[,2:5])),nrow=nrow(inf_table))
stab[h_idx,1] = sprintf( "\\a{%.3f} & ", inf_table[h_idx,2] )
stab[tr_idx,2:3] = sprintf( "\\a{%.3f} & ", inf_table[tr_idx,3:4] )
stab[c_idx,4] = sprintf( "\\a{%.3f} & ", inf_table[c_idx,5] )
stab[,4] = sprintf("%s\n",stab[,4])
cat(t(stab[1:28,]))
cat("----------------------\n")
cat(t(stab[29:55,]))

cat(sprintf("%.3f & ",c( 2*p/n, 4/n, tstar) ))
cat(sprintf("%.3f & ",c( 2*p/n, 4/n, tstar) ))
cat("\n")
