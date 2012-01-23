
portfolio.totalQuant <- function(){ sum(portfolio.pos$quant) }
portfolio.reset <-function() {portfolio.pos <<- list(quant=NULL,buyP=NULL,sellP=NULL)}

portfolio.buy_old  <- function(quantity, buyPrice, sellPrice)
{
  val=0;QB=0
  x=which(portfolio.pos$buyP>=buyPrice)
  x=which(portfolio.pos$quant[x]<0)
  if(length(x)>0)
  {
    for(i in x)
    {
      if(quantity<=-portfolio.pos$quant[i])
      {
        QB=QB+quantity
        val=quantity*buyPrice
        portfolio.pos$quant[i]<<-portfolio.pos$quant[i]+quantity
        quantity=0
      }
      else
      {
        val=-portfolio.pos$quant[i]*buyPrice
        QB=QB-portfolio.pos$quant[i]
        portfolio.pos$quant[i]<<-0
        quantity=quantity+portfolio.pos$quant[i]
      }
    }
  }
  
  if(quantity>0)
  {
    t=c(quantity,buyPrice,sellPrice)
    for( i in 1:length(portfolio.pos))  portfolio.pos[[i]]<<-append(portfolio.pos[[i]],t[i])
    val=val+quantity*buyPrice
    QB=QB+quantity
  }
  portfolio.clean()
  return(c(val,QB)) 
}

portfolio.buy  <- function(quantity, buyPrice, sellPrice)
{
  val=0;QB=0
  x1=which(portfolio.pos$buyP>=buyPrice)
  x2=which(portfolio.pos$quant<0)
  x=intersect(x1,x2)
  if(length(x)>0)
  {
    for(i in x)
    {
      val=val-portfolio.pos$quant[i]*buyPrice
      QB=QB-portfolio.pos$quant[i]
      portfolio.pos$quant[i]<<-0        
    }
  }
  
  if(quantity>0)
  {
    t=c(quantity,buyPrice,sellPrice)
    for( i in 1:length(portfolio.pos))  portfolio.pos[[i]]<<-append(portfolio.pos[[i]],t[i])
    val=val+quantity*buyPrice
    QB=QB+quantity
  }
  portfolio.clean()
  return(c(val,QB)) 
}

portfolio.sell_old <- function(quantity, buyPrice, sellPrice) 
{
  QS=0
  val=0
  x=which(portfolio.pos$sellP<=sellPrice)
  x=which(portfolio.pos$quant[x]>0)
  if(length(x)>0)
  {
    for(i in x)
    {
      if(quantity>portfolio.pos$quant[i])
      {
        val=val+portfolio.pos$quant[i]*sellPrice;
        quantity=quantity-portfolio.pos$quant[i];
        QS=QS+portfolio.pos$quant[i];
        portfolio.pos$quant[i]<<- 0;
      }
      else 
      {
        val=val+quantity*sellPrice;
        portfolio.pos$quant[i]<<- portfolio.pos$quant[i]-quantity;
        quantity=0
        QS=QS+quantity;
      }
    }
  }
  if(quantity>0) 
  {
    t=c(-quantity,buyPrice,sellPrice)
    for( i in 1:length(portfolio.pos))  portfolio.pos[[i]]<<-append(portfolio.pos[[i]],t[i])
    val=val+quantity*buyPrice
    QS=QS+quantity
  }
  portfolio.clean()
  return(c(val,QS));
}

portfolio.sell <- function(quantity, buyPrice, sellPrice) 
{
  val=0;QS=0
  x1=which(portfolio.pos$sellP<=sellPrice)
  x2=which(portfolio.pos$quant>0)
  x=intersect(x1,x2)
  if(length(x)>0)
  {
    for(i in x)
    {
      val=val+portfolio.pos$quant[i]*sellPrice;
      QS=QS+portfolio.pos$quant[i];
      portfolio.pos$quant[i]<<- 0;
    }
  }
  if(quantity>0) 
  {
    t=c(-quantity,buyPrice,sellPrice)
    for( i in 1:length(portfolio.pos))  portfolio.pos[[i]]<<-append(portfolio.pos[[i]],t[i])
    val=val+quantity*buyPrice
    QS=QS+quantity
  }
  
  portfolio.clean() 
  return(c(as.double(val),as.numeric(QS)));
}

portfolio.clean <- function() {
  x=which(portfolio.pos$quant==0);
  if(length(x)>0){ for(j in 1:3) portfolio.pos[[j]]<<-portfolio.pos[[j]][-x]}
 }

TakePosition <- function(PSpot,Limit.A,Limit.B,qBmax,qSmax)
{
  qB=0; qS=0;
  
  if(PSpot<=Limit.A) qB=min(qBmax , ceiling(10*(Limit.A-PSpot)/Limit.A))
  if(PSpot>=Limit.B) qS=min(qSmax , ceiling(10*(PSpot-Limit.B)/Limit.B))
      
  tmp=portfolio.buy (qB,PSpot,Limit.B)
  
  vB=-tmp[1]
  qB=tmp[2]
  tmp_=portfolio.sell(qS,Limit.A,PSpot)
  vS=tmp_[1]
  qSeff=tmp_[2]
  return(c(qB,qSeff,vB,vS))
}