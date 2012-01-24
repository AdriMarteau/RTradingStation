Ticker    <<- "MSFT"
Capital   <<- 500
ShortSell <<- F
Dividend  <<- T
StartDate <<- "2009-01-01"
EndDate   <<- ""

Strategy  <<-"AL2" # EMA AL

#SMOOTHING PARAMS FOR EMA AND AL[i]
 Strat.param.sm.l   <<- 10
 Strat.param.sm.lvl <<- 0.7 

#ADAPTATION PARAM FOR AL1
 Strat.param.al.l     <<- 5     ## Starting value
 Strat.param.al.lvl.u <<- 0.1   ## Trigger lvl when spot too out (up)
 Strat.param.al.r.u   <<- 0.07  ## Correction factor (up)
 Strat.param.al.lvl.d <<- 0.05  ## Trigger lvl when band too wide (down)
 Strat.param.al.r.d   <<- 0.04  ## Correction factor (down)

#ADAPTATION PARAM FOR AL2
 Strat.param.al.l     <<- 5     ## Starting value
 Strat.param.al.lvl   <<- 0.1   ## Trigger lvl

#WIDTH PARAM
 Strat.param.r      <<- 0.1

##----------------------------------#
# RUN SIMUL.
Ret <<- ExecStrat(Ticker, StartDate, EndDate, Capital, ShortSell, Dividend)
 
Display()
