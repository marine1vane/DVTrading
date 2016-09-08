ticker <- ABBV 
Stock_Regression <- function(ticker){
  RRV.model <- lm( ticker[,6] ~ Lag(ticker[,6]) * Lag(ticker[,5]))
  pred <- predict(RRV.model)
  #last(pred)
  #duffBear <- diff(pred-ticker[,6], rm.na=T)
  #sd(na.omit(duffBear))
  #subset(duffBear, duffBear[,1]>.5)
  returny <- (last(pred)-last(ticker[,6]))/last(ticker[,6])
  #Greater than means the regression prediction is positive, less than means the prediction is negative
  return(returny)
}

Money_Flow <- function(ticker) {
  #Divergence between MFI and price can be indicative of a reversal. 
  #In addition, values above/below 80/20 indicate market tops/bottoms
  indicator2 <- MFI(c(ticker[,2],ticker[,3],ticker[,4]), ticker[,5], n = 13) 
  return(last(indicator2))
}

Overbought_Oversold <- function(ticker) {
  indicator1 <- RSI(ticker[,6], n = 13, maType = "EMA")
  #indicator2 <- MACD(ticker, nFast = 12, nSlow = 26, nSig = 9)
  #signalSell <- ifelse(indicator1 > 70, "Sell", "Hold")
  #signalBuy <- ifelse(indicator1 < 30, "Buy", "Hold")
  return(last(indicator1))
}
