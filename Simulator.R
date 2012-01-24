require("quantmod")

ExecStrat <- function(Ticker, StartDate, EndDate, Capital=Inf, ShortSell=F, Div=F, Source="yahoo")
{
  portfolio.reset()
  X_ <<- getSymbols(Ticker,src=Source,auto.assign=F)
  D_ <<- getDividends(Ticker,auto.assign = F)
  Spot_ <<- (Hi(X_)+Lo(X_))/2
  
  InitStrat()
  
  X=X_[paste(StartDate,EndDate,sep="/")]
  D=D_[paste(StartDate,EndDate,sep="/")]
 
  Spot=(Hi(X)+Lo(X))/2
  A=Strat.A[paste(StartDate,EndDate,sep="/")]
  B=Strat.B[paste(StartDate,EndDate,sep="/")]
  
  n=length(Cl(X))
  Ret<<-xts(matrix(0,nc=7,nr=n),index(X))
  TotPnL=0;
  QShare=0;
  for(i in 1:n)
  {
    
    date=index(Spot[i])
    # Determine the price (convention)
    S=as.double(Spot[i])
    # Maximum tradable qty TODO > write it in "portfolio.r"
    qBmax=max(0,floor((Capital+TotPnL)/S))
    if(ShortSell) qSmax=Inf else qSmax=0
    
    tmp=TakePosition(S,A[i],B[i],qBmax,qSmax)
    qB=tmp[1];qS=tmp[2]
    PnL=tmp[3]+tmp[4]
    if(Div && length(D[date])>0) {PnL=PnL+D[date]*QShare}
    
    Ret[i,1]=PnL
    TotPnL=sum(Ret[,1]);
    Ret[i,2]=TotPnL
    Ret[i,3]=qB
    
    Ret[i,4]=qS
    QShare=portfolio.totalQuant();
    Ret[i,5]=QShare
    Ret[i,6]=QShare*S
    Ret[i,7]=Ret[i,2]+Ret[i,6]
    
  }
  #Ret[,2]=cumsum(Ret[,1])
  Ret
}

#Ret=c(Ret,xts(tmp,index(X[i,0]))) #tmp=matrix(c(PnL,TotPnL,qB,qS),nc=4)