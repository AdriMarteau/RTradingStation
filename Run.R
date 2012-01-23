Ticker    <<- "MSFT"
Capital   <<- Inf
ShortSell <<- F
Dividend  <<- F
StartDate <<- "2008-01-01"
EndDate   <<- "2008-02-01"

Strategy  <<-"AL" #EMA

#SMOOTHING PARAMS
 Strat.param.sm.l   <<- 30
 Strat.param.sm.lvl <<- 0.7 #0.99

#ADAPTATION PARAM
 Strat.param.al.l     <<- 20   ## Starting value
 Strat.param.al.lvl.u <<- 0.4   ## Trigger lvl when spot too out (up)
 Strat.param.al.r.u   <<- 0.1   ## Correction factor (up)
 Strat.param.al.lvl.d <<- 0.2   ## Trigger lvl when band too wide (down)
 Strat.param.al.r.d   <<- 0.5   ## Correction factor (down)

#WIDTH PARAM
 Strat.param.r      <<- 0.4

##----------------------------------#
# RUN SIMUL.
Ret <<- ExecStrat(Ticker, StartDate, EndDate, Capital, ShortSell, Dividend)
 
Display()
