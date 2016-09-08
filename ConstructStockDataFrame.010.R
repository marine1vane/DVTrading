library(quantmod)

#Building the data frame and xts to show dividends, splits and technical indicators
getSymbols(c("IBM"))  #need to improve this line to pull form google
Playground <- data.frame(IBM)
Playground$date <- as.Date(row.names(Playground))
Playground$RSI <- RSI(Playground$IBM.Adjusted, n = 13, maType="EMA") #can add Moving Average Type with maType = 
#Playground$MACD <- MACD(IBM, nFast = 12, nSlow = 26, nSig = 9)$signal
#Playground <- na.trim(Playground)

#How come this is not working to make my dataframe include dividends!?!
Dividend <- getDividends('IBM', from = "2007-01-01", to = Sys.Date(), src = "google", auto.assign = FALSE)
Playground$Dividend <- getDividends('IBM', from = "2007-01-01", to = Sys.Date(), src = "google", auto.assign = FALSE)
#---------------------

Playground$Splits <- getSplits('IBM', from = "2007-01-01", to = Sys.Date(), src = "google", auto.assign = FALSE)

#Problem, this is feeding a two element frame into SellSignal and BuySignal, 
#i wonder if it is individually scoreing RSI and MACD
Playground$RSI <- RSI(Playground$IBM.Adjusted, n = 13, maType="EMA") #for optimizing the n
Playground$SellSignal <- ifelse(Playground$RSI > 70, TRUE, FALSE)
Playground$BuySignal <- ifelse(Playground$RSI < 30, TRUE, FALSE)
Playground$Position <- 0
Playground$Cash <- 0
Playground$lagLow <- 0
Playground$lagHigh <- 0
Playground$lagVolume <- 0
Playground$lagClose <- 0
Commission <- 9

#BEGIN FOR LOOP WITH STRANGE PROBLEM

for (i in 2:nrow(Playground)) {
  if(!is.null(Playground$SellSignal[i-1]))
    {
    if((Playground$SellSignal[i-1]==TRUE) && 
       (Playground$Position[i-1]==1000))
    {
      Playground$Cash[i] <- Playground$IBM.Adjusted[i]*1000-Commission #Make the trade
      for (y in i:nrow(Playground)) #Fill all future rows with a cleared position
        {
        Playground$Position[y]<-0
        }       
    }
  }
  if(!is.null(Playground$BuySignal[i-1]))
    {
    if((Playground$BuySignal[i-1]==TRUE) && 
       (Playground$Position[i-1]==0))
    {
      Playground$Cash[i] <- Playground$IBM.Adjusted[i]*-1000-Commission #Make the trade
      for (y in i:nrow(Playground)) #Fill all future rows with a holding position
        {
        Playground$Position[y]<-1000
        }
    }
  }
  #This is a really sloppy way to do it but I don't know how to make my linear model work otherwise
  Playground$lagLow[i] <- Playground$IBM.Low[i-1]
  Playground$lagHigh[i] <- Playground$IBM.High[i-1]
  Playground$lagClose[i] <- Playground$IBM.Close[i-1]
  Playground$lagVolume[i] <- Playground$IBM.Volume[i-1]
}

if(Playground$Position[i]==1000){Playground$Cash[i] <- Playground$IBM.Adjusted[i]*1000-Commission} #gets value if holding on last period
sum(Playground$Cash)  #Strategy Score

#Buy and Hold Score
sum(Playground$Div, na.rm=TRUE)
Playground$IBM.Adjusted[i] * 1000 - Playground$IBM.Adjusted[1] * 1000-Commission 


#Add in the dividends

#use linear modeling with dplyr to place a limit buy or limit sell
library(dplyr)

ticker.lm <- lm(T.Low ~ lagLow + lagHigh + lagClose + lagVolume, data = Playground)
summary.lm(ticker.lm)
mutate(Playground, predictor = predict.lm(ticker.lm))
mutate(Playground, StupidLag = lag(T.Adujusted))
write.csv(Playground, file = "Stock2")
write.csv(predict.lm(ticker.lm), file = "StockLow")
ticker.lm <- lm(T.High ~ lagLow + lagHigh + lagClose + lagVolume, data = Playground)
write.csv(predict.lm(ticker.lm), file = "StockHigh")
getwd()
