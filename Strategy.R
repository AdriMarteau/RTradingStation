InitStrat <- function()
{
  n=length(Cl(X_))
  Strat.A <<- xts(matrix(0,nc=1,nr=n),index(X_))
  Strat.B <<- xts(matrix(0,nc=1,nr=n),index(X_))
  Strat.M <<- xts(matrix(0,nc=1,nr=n),index(X_))
  
  a=1-(1-Strat.param.sm.lvl)^(1/Strat.param.sm.l)
  sig=(Strat.param.r)/(2+Strat.param.r)
  smoothing = c(0:(Strat.param.sm.l-1))
  smoothing = (1-a)^smoothing
  smoothing = smoothing*a
  
  for(i in (Strat.param.sm.l+1):n)
  {
    Strat.M[i] <<- smoothing %*% Spot_[((i-Strat.param.sm.l):(i-1))] 
  }
  Strat.A <<- Strat.M*(1-sig)
  Strat.B <<- Strat.M*(1+sig)
  
}
