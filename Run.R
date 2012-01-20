Ticker    <<- "MSFT"
Capital   <<- Inf
ShortSell <<- F
Dividend  <<- T
StartDate <<- "2010-01-01"
EndDate   <<- ""

Strategy  <<-"AL" #EMA

#SMOOTHING PARAMS
 Strat.param.sm.l   <<- 100
 Strat.param.sm.lvl <<- 0.7 #0.99

#ADAPTATION PARAM
 Strat.param.al.l     <<- 20   ## Starting value
 Strat.param.al.lvl.u <<- 0.4   ## Trigger lvl when spot too out (up)
 Strat.param.al.r.u   <<- 0.1   ## Correction factor (up)
 Strat.param.al.lvl.d <<- 0.2   ## Trigger lvl when band too wide (down)
 Strat.param.al.r.d   <<- 0.5   ## Correction factor (down)

#WIDTH PARAM
 Strat.param.r      <<- 0.1


title=paste("Backtest Strategy on ",Ticker,sep="")
window_s=0.1

Ret=ExecStrat(Ticker, StartDate, EndDate, Capital, ShortSell, Dividend)
Cotation=cbind(Spot_,Strat.A,Strat.B)[paste(StartDate,EndDate,sep="/")]
chartSeries(Cotation,type='l',name=title,yrange=c(min(Cotation)*(1-window_s),max(Cotation)*(1+window_s)))
plot(addTA(Strat.A,type='l',col='red',legend="",on=1))
plot(addTA(Strat.B,type='l',col='white',legend="",on=1))

plot(addTA((Ret[,6]+Ret[,2]),type='l',col='blue',legend=""))
plot(addTA(Ret[,c(3,4)],type=c('h','h'),col=c('blue','red'),legend=""))
plot(addTA(Ret[,5],type='h',legend=""))

print("DONE")