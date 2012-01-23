
portfolio.totalQuant <- function(){ sum(portfolio.pos$quant) }
portfolio.reset <-function() {portfolio.pos <<- list(quant=NULL,buyP=NULL,sellP=NULL)}

portfolio.buy <- function(quantity,buyPrice,sellPrice)
{
  x=c(quantity,buyPrice,sellPrice)
  for( i in 1:length(portfolio.pos))  portfolio.pos[[i]]<<-append(portfolio.pos[[i]],x[i])
  return(quantity*buyPrice)
}

# do not handle shortsell yet...
portfolio.sell <- function(Price, Quant=Inf) 
{
  QS=0
  val=0
  x=which(portfolio.pos$sellP<=Price)
  if(length(x)==0) return(val);
  for(i in x)
  {
    if(Quant>portfolio.pos$quant[i])
    {
      val=val+portfolio.pos$quant[i]*Price;
      Quant=Quant-portfolio.pos$quant[i];
      QS=QS+portfolio.pos$quant[i];
      portfolio.pos$quant[i]<<- 0;
    }
    else 
    {
      val=val+Quant*Price;
      portfolio.pos$quant[i]<<- portfolio.pos$quant[i]-Quant;
      QS=QS+Quant;
    }
  }
  x=which(portfolio.pos$quant==0);
  if(length(x)>0){ for(j in 1:3) portfolio.pos[[j]]<<-portfolio.pos[[j]][-x]}
  return(c(val,QS));
}

TakePosition <- function(PSpot,Limit.A,Limit.B,qBmax,qSmax)
{
  qB=0; qS=Inf;
  if(PSpot<=Limit.A) qB=min(qBmax , ceiling(10*(Limit.A-PSpot)/Limit.A))
  #if(PSpot>=Limit.B) qS=min(qSmax , ceiling(10*(PSpot-Limit.B)/Limit.B))
  vB=-portfolio.buy(qB,PSpot,Limit.B)
  tmp=portfolio.sell(PSpot, qS)
  vS=tmp[1]
  qSeff=tmp[2]
  return(c(qB,qSeff,vB,vS))
}