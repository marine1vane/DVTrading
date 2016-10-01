library(quantmod)

#Building the data frame and xts to show dividends, splits and technical indicators

SymbolsList <- c("T", "AAPL")
getSymbols(SymbolsList)  #need to improve this line to pull from google


Playground <- data.frame(T, AAPL)
Playground$date <- as.Date(row.names(Playground))
Playground$wday <- as.POSIXlt(Playground$date)$wday #day of the week
Playground$yday <- as.POSIXlt(Playground$date)$mday #day of the month
Playground$mon <- as.POSIXlt(Playground$date)$mon #month of the year
Playground$T.RSI <- RSI(Playground$T.Adjusted, n = 13, maType="EMA") # n is an analytic number determined by Duke in an ad hoc fashion.
Playground$AAPL.RSI <- RSI(Playground$AAPL.Adjusted, n = 13, maType="EMA")
    #can add Moving Average Type with maType = ???
#Playground$MACD <- MACD(T, nFast = 12, nSlow = 26, nSig = 9)
Playground <- na.trim(Playground)
  
getDividends(T, from = "2007-01-01", to = Sys.Date(), src = "google", auto.assign = FALSE)
getSplits(T, from = "2007-01-01", to = Sys.Date(), src = "google", auto.assign = FALSE)

#Problem, this is feeding a two element frame into SellSignal and BuySignal, 
#i wonder if it is individually scoreing RSI and MACD
Playground$SellSignal <- ifelse(Playground$RSI > 70, "Sell", "Hold")
Playground$BuySignal <- ifelse(Playground$RSI < 30, "Buy", "Hold")
Playground$Position <- 0
Playground$Cash <- 0
Commission <- 9

for (i in 2:nrow(Playground)) {
  if(!is.null(Playground$SellSignal[i-1])){
    if(Playground$SellSignal[i-1]=="Sell" & Playground$Position[i-1]==1000){
      Playground$Cash[i] <- Playground$T.Adjusted[i]*1000-Commission
      for (y in i:nrow(Playground)) {Playground$Position[y]<-0}
    }
  }
  if(!is.null(Playground$BuySignal[i-1])){
    if(Playground$BuySignal[i-1]=="Buy" & Playground$Position[i-1]==0){
      Playground$Cash[i] <- Playground$T.Adjusted[i]*-1000-Commission
      for (y in i:nrow(Playground)) {Playground$Position[y]<-1000}
    }
  }
}
if(Playground$Position[i]==1000){Playground$Cash[i] <- Playground$T.Adjusted[i]*1000} #gets value if holding on last period
sum(Playground$Cash)  #Strategy Score
Playground$T.Adjusted[i] * 1000 - Playground$T.Adjusted[1] * 1000 #Buy and Hold Score


