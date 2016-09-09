library(quantmod)
library(stringi)
#Building the data frame and xts to show dividends, splits and technical indicators
getSymbols(c("T", "KO", "SYY", "ED", "GIS", "PG", "SO", "K", "KMB",
             "VZ", "MCD", "SPB", "COF", "AMZN", "BUD", "AAPL", "XOM", 
             "CVX", "GE", "GS", "HD", "IBM", "INTC", "MMM", "JPM", "NKE",
             "JNJ", "PFE", "UNH", "DIS", "V", "WMT", "WDC", "F"), src = 'yahoo', from = "2015-01-01")

getSymbols(c("ABBV", "AA", "SAN", "BX", "CAT", "IBM"
             ), src = 'yahoo', from = "2015-01-01")

getSymbols("DAL", src='yahoo')
getSymbols.google("KO", from = "2015-01-01", to = Sys.Date())
getSymbols(c("T", "KO", "SYY", "ED", "GIS", "PG", "SO", "K", "KMB",
             "VZ", "MCD", "SPB", "COF", "AMZN", "BUD", "AAPL", "XOM", 
             "CVX", "GE", "GS", "HD", "IBM", "INTC", "MMM", "JPM", "NKE",
             "JNJ", "PFE", "UNH", "DIS", "V", "WMT", "WDC"), src = 'google', from = "2015-01-01", to = Sys.Date())


#production version of function
BuyorSell <- function(ticker) {
  indicator1 <- RSI(ticker[,6], n = 13, maType = "EMA")
  indicator2 <- MACD(ticker, nFast = 12, nSlow = 26, nSig = 9)
  signalSell <- ifelse(indicator1 > 70, "Sell", "Hold")
  signalBuy <- ifelse(indicator1 < 30, "Buy", "Hold")

#  if (last(signalSell) != "Hold") {signal1 <- ("Sell")}
#  if (last(signalBuy) != "Hold") {signal1 <- ("Buy")}
#  if (last(signalBuy) = "Hold" & last(signalSell) = "Hold") {signal1 <- "Hold")}
  return(c(last(signalSell), last(signalBuy)))
}
BuyorSellGoogle <- function(ticker) {
  #Things I am trying to collect
  #Book Value
  #Historic PE Multiple
  #RSI
  #ChakinMoneyFlow
  #acid Test = (current assets-inventory)/current liabilities  | should equal 1 or higher
  #Profitability (most useful to compare within industry groups)
  #Net Net = (current assets-current liabilities-long term debt)/number of shares outstanding | <.33
  
  getSymbols(ticker, src='yahoo')
  getFinancials("AAPL")
  getQuote('AAPL',what='Book Value')
  standardQuote()
  viewFin(x, type = "BS", period = "A", subset = "Last 4 years")
  indicator1 <- MACD(ticker, nFast = 12, nSlow = 26, nSig = 9)
  signalSell <- ifelse(indicator1 > 0.8, "Sell", "Hold")
  signalBuy <- ifelse(indicator1 < -0.8, "Buy", "Hold")
  signal <- "Hold"
  if (signalBuy[1] = "Buy" & signalBuy[2] = "Buy") {signal <- "Buy"}
  #  if (last(signalSell) != "Hold") {signal1 <- ("Sell")}
  #  if (last(signalBuy) != "Hold") {signal1 <- ("Buy")}
  #  if (last(signalBuy) = "Hold" & last(signalSell) = "Hold") {signal1 <- "Hold")}
  return(c(last(signalSell), last(signalBuy)))
}


BuyorSell(T)
BuyorSell(KO)
BuyorSell(SYY)
BuyorSell(ED)
BuyorSell(GIS)
BuyorSell(PG)
BuyorSell(SO)
BuyorSell(K)
BuyorSell(KMB)
BuyorSell(VZ)
BuyorSell(MCD)
BuyorSell(SPB)
BuyorSell(COF)
BuyorSell(AMZN)
BuyorSell(BUD)
BuyorSell(AAPL)
BuyorSell(XOM)
BuyorSell(CVX)
BuyorSell(GE)
BuyorSell(GS)
BuyorSell(HD)
BuyorSell(IBM)
BuyorSell(INTC)
BuyorSell(MMM)
BuyorSell(JPM)
BuyorSell(NKE)
BuyorSell(JNJ)
BuyorSell(PFE)
BuyorSell(UNH)
BuyorSell(DIS)
BuyorSell(V)
BuyorSell(WMT)
BuyorSell(WDC)
BuyorSell(F)

BuyorSell(AA)
BuyorSell(ABBV)
BuyorSell(BX)
BuyorSell(CAT)
BuyorSell(IBM)
BuyorSell(SAN)

getSymbols(c("F", "AMZN", "COF", "VZ"
             ), src = 'yahoo', from = "2015-01-01")

BuyorSell(F)
BuyorSell(AMZN)
BuyorSell(COF)
BuyorSell(VZ)

BuyorSellGoogle(T)
BuyorSellGoogle(KO)
BuyorSellGoogle(SYY)
BuyorSellGoogle(ED)
BuyorSellGoogle(GIS)
BuyorSellGoogle(PG)
BuyorSellGoogle(SO)
BuyorSellGoogle(K)
BuyorSellGoogle(KMB)
BuyorSellGoogle(VZ)
BuyorSellGoogle(MCD)
BuyorSellGoogle(SPB)
BuyorSellGoogle(COF)
BuyorSellGoogle(AMZN)
BuyorSellGoogle(BUD)
BuyorSellGoogle(AAPL)
BuyorSellGoogle(XOM)
BuyorSellGoogle(CVX)
BuyorSellGoogle(GE)
BuyorSellGoogle(GS)
BuyorSellGoogle(HD)
BuyorSellGoogle(IBM)
BuyorSellGoogle(INTC)
BuyorSellGoogle(MMM)
BuyorSellGoogle(JPM)
BuyorSellGoogle(NKE)
BuyorSellGoogle(JNJ)
BuyorSellGoogle(PFE)
BuyorSellGoogle(UNH)
BuyorSellGoogle(DIS)
BuyorSellGoogle(V)
BuyorSellGoogle(WMT)
play <- BuyorSellGoogle(WDC)

exportFrame <- C(BuyorSell(T), BuyorSell(KO))
DM.ABBV <- cbind()

source('~/GitHub/DVTrading2/Sentiment_Score_Function.R')
source('~/GitHub/DVTrading2/Regression_Function.R')
source('~/GitHub/DVTrading2/Machine_Learning_Function.R')
ticker <- ABBV
DM.ABBV <- cbind(Stock_Machine_Learning(ticker), 
                 Stock_Sentiment('NYSE:ABBV'), 
                 Stock_Regression(ticker), 
                 Overbought_Oversold(ticker), 
                 Money_Flow(ticker))


#experimental change to function
BuyorSell <- function(tickerframe, tickerString) {
  indicator1 <- RSI(ticker[,6], n = 5, maType = "EMA")
  indicator2 <- MACD(ticker, nFast = 12, nSlow = 26, nSig = 9)
  signalSell <- ifelse(indicator1 > 70, 1, 0)
  signalBuy <- ifelse(indicator1 < 30, 1, 0)
  
  if (last(signalSell) != 0) {return("Sell")}
  if (last(signalBuy) != 0) {return "Buy"}
  if (last(signalBuy) = 0 & last(signalSell) = 0) {return "Hold"}
}