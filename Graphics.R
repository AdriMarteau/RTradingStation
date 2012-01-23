Display <- function()
{
  title=paste("Backtest Strategy on ",Ticker,sep="")
  window_s=0.1

  Cotation=cbind(Spot_,Strat.A,Strat.B)[paste(StartDate,EndDate,sep="/")]
  chartSeries(Cotation,type='l',name=title,yrange=c(min(Cotation)*(1-window_s),max(Cotation)*(1+window_s)))
  plot(addTA(Strat.A,type='l',col='red',legend="",on=1))
  plot(addTA(Strat.B,type='l',col='white',legend="",on=1))

  plot(addTA((Ret[,6]+Ret[,2]),type='l',col='blue',legend=""))
  plot(addTA(Ret[,c(3,4)],type=c('h','h'),col=c('blue','red'),legend=""))
  plot(addTA(Ret[,5],type='h',legend=""))
}