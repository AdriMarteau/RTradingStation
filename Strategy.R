InitStrat <- function() {do.call(paste("Strat",Strategy,sep=""),list())}

StratEMA <- function()
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
    { Strat.M[i] <<- smoothing %*% Spot_[((i-Strat.param.sm.l):(i-1))] / Strat.param.sm.lvl }
  Strat.A <<- Strat.M*(1-sig); Strat.B <<- Strat.M*(1+sig)
}

StratAL <- function() #Adaptative limits
{
  n=length(Cl(X_))
  
  a=1-(1-Strat.param.sm.lvl)^(1/Strat.param.sm.l)
  sig=(Strat.param.r)/(2+Strat.param.r)
  smoothing = c(0:(Strat.param.sm.l-1))
  smoothing = (1-a)^smoothing
  smoothing = smoothing*a
  
  for(i in (Strat.param.sm.l+1):n) 
    { Strat.M[i] <<- smoothing %*% Spot_[((i-Strat.param.sm.l):(i-1))] / Strat.param.sm.lvl }
 
  l=length(Spot_[paste("",StartDate,sep="/")])
  m=mean(Spot_[(l-Strat.param.al.l+1):l])
  sig=(Strat.param.r)/(2+Strat.param.r)
  A = matrix(m*(1-sig),nc=1,nr=n)
  B = matrix(m*(1+sig),nc=1,nr=n)
  for(i in (l+1):(n-1))
  {
    S=Strat.M[i]
    if((A[i]-S)/S > Strat.param.al.lvl.d)         #A is too high
      {A[(i+1):n] <- as.double(A[i]*(1-Strat.param.al.r.d))}
    if((S-A[i])/S > Strat.param.al.lvl.u)         #A is too low 
      {A[(i+1):n] <- as.double(A[i]*(1+Strat.param.al.r.u))}
    if((B[i]-S)/S > Strat.param.al.lvl.u)         #B is too high 
      {B[(i+1):n] <- as.double(B[i]*(1-Strat.param.al.r.u))}
    if((S-B[i])/S > Strat.param.al.lvl.d)         #B is too low 
      {B[(i+1):n] <- as.double(B[i]*(1+Strat.param.al.r.d))}
  }
  Strat.A <<- xts(A,index(X_));  Strat.B <<- xts(B,index(X_))
}
