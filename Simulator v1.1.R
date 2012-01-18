ExecStrat <- function(Ticker, StartDate, EndDate, Capital=Inf, ShortSell=F, Source="yahoo")
{
  X_ <<- getSymbols(Ticker,src=Source,auto.assign=F)
  Spot_ <<- (Hi(X_)+Lo(X_))/2
  
  InitStrat()
  
  X=X_[paste(StartDate,EndDate,sep="/")]
  Spot=(Hi(X)+Lo(X))/2
  A=Strat.A[paste(StartDate,EndDate,sep="/")]
  B=Strat.B[paste(StartDate,EndDate,sep="/")]
  
  n=length(Cl(X))
  Ret=xts(matrix(0,nc=6,nr=n),index(X))
  TotPnL=0
  QShare=0
  for(i in 1:n)
  {
    qB=0; qS=0
    # Determine the price (convention)
    S=as.double(Spot[i])
    # Maximum tradable qty
    qBmax=max(0,floor((Capital+TotPnL)/S))
    if(ShortSell) qSmax=Inf else qSmax=QShare
    
    # Compute the quantities to buy and sell
    if(S<=A[i]) qB=min(qBmax , ceiling(10*(A[i]-S)/A[i])) #runif(1)
    if(S>=B[i]) qS=min(qSmax, ceiling(10*(S-B[i])/B[i])) #runif(1)
    
    QShare=QShare-qS+qB
    
    PnL=S*(qS-qB)
    TotPnL=PnL+TotPnL
    
    Ret[i,1]=PnL
    Ret[i,2]=TotPnL
    Ret[i,3]=qB
    Ret[i,4]=qS
    Ret[i,5]=QShare
    Ret[i,6]=QShare*S
  }
  Ret
}

#Ret=c(Ret,xts(tmp,index(X[i,0]))) #tmp=matrix(c(PnL,TotPnL,qB,qS),nc=4)