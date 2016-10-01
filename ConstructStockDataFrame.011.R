library(quantmod)

#Building the data frame and xts to show dividends, splits and technical indicators
getSymbols(c("IBM"))  #need to improve this line to pull form google
Playground <- data.frame(T, KO, SYY, ED, GIS, PG, SO, K, KMB,
                         VZ, MCD, SPB, COF, AMZN, BUD, AAPL, XOM, 
                         CVX, GE, GS, HD, IBM, INTC, MMM, JPM, NKE,
                         JNJ, PFE, UNH, DIS, V, WMT, WDC, F, SAN, BX)

Playground <- data.frame(IBM, VZ, COF, JNJ)
Playground$date <- as.Date(row.names(Playground))
Playground$IBM.RSI <- RSI(Playground$IBM.Adjusted, n = 13, maType="EMA")
Playground$VZ.RSI <- RSI(Playground$VZ.Adjusted, n = 13, maType="EMA")
Playground$COF.RSI <- RSI(Playground$COF.Adjusted, n = 13, maType="EMA")
Playground$JNJ.RSI <- RSI(Playground$JNJ.Adjusted, n = 13, maType="EMA")
#Playground <- na.trim(Playground)

#How come this is not working to make my dataframe include dividends!?!
#Dividend <- getDividends('IBM', from = "2007-01-01", to = Sys.Date(), src = "google", auto.assign = FALSE)
#Playground$Dividend <- getDividends('IBM', from = "2007-01-01", to = Sys.Date(), src = "google", auto.assign = FALSE)
#---------------------

#Playground$Splits <- getSplits('IBM', from = "2007-01-01", to = Sys.Date(), src = "google", auto.assign = FALSE)

Playground$IBM.SellSignal <- ifelse(Playground$IBM.RSI > 70, TRUE, FALSE)
Playground$IBM.BuySignal <- ifelse(Playground$IBM.RSI < 30, TRUE, FALSE)
Playground$IBM.Position <- 0 #No longer needed
Playground$VZ.SellSignal <- ifelse(Playground$VZ.RSI > 70, TRUE, FALSE)
Playground$VZ.BuySignal <- ifelse(Playground$VZ.RSI < 30, TRUE, FALSE)
Playground$VZ.Position <- 0 #No longer needed
Playground$COF.SellSignal <- ifelse(Playground$COF.RSI > 70, TRUE, FALSE)
Playground$COF.BuySignal <- ifelse(Playground$COF.RSI < 30, TRUE, FALSE)
Playground$COF.Position <- 0 #No longer needed
Playground$JNJ.SellSignal <- ifelse(Playground$JNJ.RSI > 70, TRUE, FALSE)
Playground$JNJ.BuySignal <- ifelse(Playground$JNJ.RSI < 30, TRUE, FALSE)
Playground$JNJ.Position <- 0 #No longer needed
Playground$Position <- 0 #No longer needed
Playground$Cash <- 500000 #No longer needed
Playground$lagLow <- 0
Playground$lagHigh <- 0
Playground$lagVolume <- 0
Playground$lagClose <- 0
Commission <- 9
Portfolio <- data.frame(IBM = 0, VZ = 0, COF = 0, JNJ = 0, Cash=500000)
#BEGIN FOR LOOP WITH STRANGE PROBLEM

for (i in 2:nrow(Playground)) {
  if(!is.na(Playground$JNJ.SellSignal[i-1]))
  {
    if((Playground$JNJ.SellSignal[i-1]==TRUE) && 
       Portfolio$JNJ!=0)
      
    {
      #Make the trade
      Portfolio$JNJ<-0
      Portfolio$Cash <- Portfolio$Cash + Playground$JNJ.Adjusted[i]*Portfolio$JNJ - Commission    
    }
  }
  if(!is.na(Playground$JNJ.BuySignal[i-1]))
  {
    if((Playground$JNJ.BuySignal[i-1]==TRUE) && 
       (Portfolio$JNJ == 0) &&
       (Portfolio$Cash > Playground$JNJ.Adjusted[i]*1000+Commission))
    {
      #Make the trade
      Portfolio$JNJ<-1000
      Portfolio$Cash <- Portfolio$Cash-Playground$JNJ.Adjusted[i]*1000-Commission
    }
  }
  if(!is.na(Playground$IBM.SellSignal[i-1]))
  {
    if((Playground$IBM.SellSignal[i-1]==TRUE) && 
       Portfolio$IBM!=0)
      
    {
      #Make the trade
      Portfolio$IBM<-0
      Portfolio$Cash <- Portfolio$Cash + Playground$IBM.Adjusted[i]*Portfolio$IBM - Commission    
    }
  }
  if(!is.na(Playground$IBM.BuySignal[i-1]))
  {
    if((Playground$IBM.BuySignal[i-1]==TRUE) && 
       (Portfolio$IBM == 0) &&
       (Portfolio$Cash > Playground$IBM.Adjusted[i]*1000+Commission))
    {
      #Make the trade
      Portfolio$IBM<-1000
      Portfolio$Cash <- Portfolio$Cash-Playground$IBM.Adjusted[i]*1000-Commission
    }
  }
  if(!is.na(Playground$VZ.SellSignal[i-1]))
  {
    if((Playground$VZ.SellSignal[i-1]==TRUE) && 
       Portfolio$VZ!=0)      
    {
      #Make the trade
      Portfolio$VZ<-0
      Portfolio$Cash <- Portfolio$Cash + Playground$VZ.Adjusted[i]*Portfolio$VZ - Commission    
    }
  }
  if(!is.na(Playground$VZ.BuySignal[i-1]))
  {
    if((Playground$VZ.BuySignal[i-1]==TRUE) && 
       (Portfolio$VZ == 0) &&
       (Portfolio$Cash > Playground$VZ.Adjusted[i]*1000+Commission))
    {
      #Make the trade     
      Portfolio$VZ<-1000
      Portfolio$Cash <- Portfolio$Cash-Playground$VZ.Adjusted[i]*1000-Commission 
    }
  }
  if(!is.na(Playground$COF.SellSignal[i-1]))
  {
    if((Playground$COF.SellSignal[i-1]==TRUE) && 
       Portfolio$COF!=0)      
    {
      #Make the trade
      Portfolio$COF<-0
      Portfolio$Cash <- Portfolio$Cash + Playground$COF.Adjusted[i]*Portfolio$COF - Commission    
    }
  }
  if(!is.na(Playground$COF.BuySignal[i-1]))
  {
    if((Playground$COF.BuySignal[i-1]==TRUE) && 
       (Portfolio$COF == 0) &&
       (Portfolio$Cash > Playground$COF.Adjusted[i]*1000+Commission))
    {
      #Make the trade     
      Portfolio$COF<-1000
      Portfolio$Cash <- Portfolio$Cash-Playground$COF.Adjusted[i]*1000-Commission 
    }
  }
  
  #This is a really sloppy way to do it but I don't know how to make my linear model work otherwise
  Playground$lagLow[i] <- Playground$IBM.Low[i-1]
  Playground$lagHigh[i] <- Playground$IBM.High[i-1]
  Playground$lagClose[i] <- Playground$IBM.Close[i-1]
  Playground$lagVolume[i] <- Playground$IBM.Volume[i-1]
}

Playground$Cash
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
