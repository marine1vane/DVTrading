library(quantmod)

#Building the data frame and xts to show dividends, splits and technical indicators
getSymbols(c("AMZN"))  #need to improve this line to pull form google
Playground <- data.frame(AMZN)
Playground$date <- as.Date(row.names(Playground))
Playground$wday <- as.POSIXlt(Playground$date)$wday #day of the week
Playground$yday <- as.POSIXlt(Playground$date)$mday #day of the month
Playground$mon <- as.POSIXlt(Playground$date)$mon #month of the year
Playground$RSI <- RSI(Playground$AMZN.Adjusted, n = 5, maType="EMA") #can add Moving Average Type with maType = 
Playground$MACD <- MACD(AMZN, nFast = 12, nSlow = 26, nSig = 9)
Playground <- na.trim(Playground)

Playground$Div <- getDividends('AMZN', from = "2007-01-01", to = Sys.Date(), src = "google", auto.assign = FALSE)
Playground$Div <- getSplits('AMZN', from = "2007-01-01", to = Sys.Date(), src = "google", auto.assign = FALSE)

#same as above?
rrv.RSI <- RSI(AMZN$AMZN.Adjusted, n = 5)
rrv.MACD <- MACD(AMZN, nFast = 12, nSlow = 26, nSig = 9)


Playground$SellSignal <- ifelse(Playground$RSI > 70 & Playground$MACD > 0, "Sell", "Hold")
Playground$BuySignal <- ifelse(Playground$RSI < 30 & Playground$MACD < 0, "Buy", "Hold")
Playground$Position <- 0

for (i in 2:nrow(Playground)) {
  if(!is.null(Playground$SellSignal)){
    if(Playground$SellSignal=="Sell" & Playground$Position==1000){
      Playground$Cash[i+1] == Playground$AMZN.Adjusted[i+1]*1000
      for (y in i+1:nrow(Playground)) {Playground$Position[y]==0}
      
    }
  }
  if(!is.null(Playground$BuySignal)){
    if(Playground$BuySignal=="Buy" & Playground$Position==0){
      Playground$Cash[i+1] == Playground$AMZN.Adjusted[i+1]*-1000
      for (y in i+1:nrow(Playground)) {Playground$Position[y]==1000}
  }
  }
}



for (i in 2:nrow(Playground)) {
  if(!is.null(Playground$SellSignal[i-1])){
    if(Playground$SellSignal[i-1]=="Sell" & Playground$Position[i-1]==1000){
      Playground$Cash[i] <- Playground$AMZN.Adjusted[i]*1000
      Playground$Position <- -1000
    }
  }
  if(!is.null(Playground$BuySignal[i-1])){
    if(Playground$BuySignal[i-1]=="Buy" & Playground$Position[i-1]==0){
      Playground$Cash[i] <- Playground$AMZN.Adjusted[i]*-1000
      Playground$Position <- 1000
    }
  }
}

for (i in 2:nrow(Playground)) {
  if(!is.null(Playground$SellSignal[i-1])){
    if(Playground$SellSignal[i-1]=="Sell" & Playground$Position[i-1]==1000){
      Playground$Cash[i] <- Playground$AMZN.Adjusted[i]*1000
      for (y in i:nrow(Playground)) {Playground$Position[y]<-0}
    }
  }
  if(!is.null(Playground$BuySignal[i-1])){
    if(Playground$BuySignal[i-1]=="Buy" & Playground$Position[i-1]==0){
      Playground$Cash[i] <- Playground$AMZN.Adjusted[i]*-1000
      for (y in i:nrow(Playground)) {Playground$Position[y]<-1000}
    }
  }
}


for (i in 2:nrow(Playground)) {
  Playground$Position <- Playground$Position[i] +10
}
