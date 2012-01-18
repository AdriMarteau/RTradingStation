Ticker= "GS" #"MSFT"
Capital=Inf
ShortSell=F
StartDate="2009-01-01"
EndDate="2011-01-31"

Strat.param.sm.l <<- 100
Strat.param.sm.lvl <<- 0.99
Strat.param.r <<- 0.01


title=paste("Backtest Strategy on ",Ticker,sep="")
window_s=0.1

Ret=ExecStrat(Ticker, StartDate, EndDate, Capital, ShortSell)
Cotation=cbind(Spot_,Strat.A,Strat.B)[paste(StartDate,EndDate,sep="/")]
chartSeries(Cotation,type='l',name=title,yrange=c(min(Cotation)*(1-window_s),max(Cotation)*(1+window_s)))
plot(addTA(Strat.A,type='l',col='white',legend="",on=1))
plot(addTA(Strat.B,type='l',col='white',legend="",on=1))

plot(addTA((Ret[,6]+Ret[,2]),type='l',col='blue',legend=""))
plot(addTA(Ret[,c(3,4)],type=c('h','h'),col=c('blue','red'),legend=""))
plot(addTA(Ret[,5],type='h',legend=""))

print("DONE")