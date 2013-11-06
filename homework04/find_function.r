find_leverage = function(reg.out) {
  n = length(reg.out$residuals)   
  p = n - reg.out$df.residual 
  lev = influence(reg.out)$hat
  return(list(vals=lev,idx=lev >= 2*p/n))
}

find_outlier = function(reg.out) {
  n = length(reg.out$residuals)   
  p = n - reg.out$df.residual 
  tstar = qt(1-.025/n,n-p-1)
  jack = rstudent(reg.out)
  return(list(vals=jack,idx=(abs(jack) >= tstar)))
}

find_cooksd = function(reg.out) {
  cd = cooks.distance(reg.out)
  return(list(vals=cd,idx=(cd >= 4/length(reg.out$residuals))))
}

find_dffits = function(reg.out) {
  dff = dffits(reg.out)
  n = length(reg.out$residuals)   
  p = n - reg.out$df.residual 
  return(list(vals=dff,idx=(abs(dff) >= 2*sqrt(p/n))))
}

find_dfbetas = function(reg.out) {
  dfb = dfbetas(reg.out)
  n = length(reg.out$residuals)   
  idx = (rowSums(abs(dfb) >= 2/sqrt(n)) > 0)
  return(list(vals=dfb,idx=idx,ijdx=abs(dfb) >= 2/sqrt(n)))
}

print_latex_column = function(vals,highlight_idx) {
  strs = sprintf("%.3f \n",vals)
  strs[highlight_idx] = sprintf( "\\a{%.3f }\n", vals[highlight_idx] )
  cat(strs)
}

get_cutoffs = function(reg.out) {
  n = length(reg.out$residuals)
  p = n - reg.out$df.residual 
  tstar = qt(1-.025/n,n-p-1)
  return(list( tstar=tstar, lev=2*p/n, cd=4/n, dff=2*sqrt(p/n), dfb= 2/sqrt(n)))
}

