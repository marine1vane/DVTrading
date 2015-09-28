#Parts to Build

library(tm.plugin.webmining)
require ggplot2
require dplyr
require quantmod
require twitteR

#Cyclical Cycle Detection
#Define periodicity dynamically
#Define latitude and longitude of the cycles
ggplot(stock, aes(Day, daily)) + geom_point() + facet_grid(,~weekday)

#Sentiment Analyzer
#Mine the web
#Define the libraries
#Google Finanace
#Google News
#NY Times
#Reuters
#Yahoo Finance
#Joshua.m.ulrich@gmail.com make BEA and Census API's for credibility and then contact him to look at your code, he works for IBrokers so be careful about intellectual property theft
#Not on CRAN Rlinkedin, Rfacebook, RGoogleTrends



#Technical Analyzer
#Look at traditional fundamentals
#P/E, dividends,




#Indentify traditional overbought/oversold levels
#Program the buy and sell signals
#Answer what the lead is on price and how strongly 
#-----------
#ABI = Absolute Breadth Index = |ADV-DEC|
#advanceing issues -  declining isses. Always positive. Higher Values indicate volitility
#RSI is a leading indicator
#zero to one
#overbought crosses .20 SELL signal moves from overbought to .50
#oversold crosses .80 BUY signal moves from oversold to .50
#look for divergence with price to indicate the end of a trend
#How many period RSI?  Try several values starting with 5 and ending with 14 on your optimization.  
#Is there a good way to assign this dynamically?
#Measure comparison in the Rate of Change (ROC) and the twitter posts
#Don't buy the RSI if the momentum is hugely negative
#Banded Oscillator,  CCI below 100 coming up is a BUY signal
#CCI above 100 coming down is a SELL signal
#When EMA crosses MACD, when crosses below SELL signal, when crosses above BUY signal
#when trending up look for oversold conditions only. The trend is your friend, you can identify this with a positive MACD 120 days
#Chaikin Money Flow >.25 (BUY) <-.25 (SELL); if Price is going up and <0 probable reversal
macd <- MACD(ticker,[,"Close"])
  ifelse(lag(macd$signal)>0) & macd$signal<0,"Buy", ifelse(lag(macd$signal)<0 & macd$signal > 0, "Sell","Hold")
Buy <- ifelse(pred>0.5,1,0)
Sell <- ifelse(pred<0.3,1,0) #for the true part input a position change and replicate the cost in the algorithm

positions <- function(stocks) {
  if(stockcondition > 0.8) {
    comp <- buy 100 shares
  }
  else if (stock condition < 0.3) {
    comp <- sell 100 shares
  }
  require(compiler, queitly=T)
  return(comp)
}
ccEval <- cmpfun(positions)

for(x in rownames:...) {
  
}

#Group stocks according to industry segment
#Identify best of and worst of breed stocks
#Is there a tendancy for them to pull together? Does one lead the others?
#  AAPL -> Technology
#  MSFT -> Technology
#  IBM -> Technology
#  CVX -> Oil
#  EXM -> Oil
#  VZ -> Telecom
#  T -> Telecom
  
#GEOcode yelp and twitter data to discover regional macroeconomic trends 
#against store locations for retail stocks to see if the stock could beat earnings


#Regression and or machine learning approach
#Not really sure yet, but ideas abound

#Operations Research and Back Testing
#Determine the size and quantity of profit
#Sanity Check the stocks




#Algorithmic Trades
#actually pass the trades to the platform electronically so you don't have to watch it.
#Limit Buy Order
#Limit Sell Order
#Stop Loss Order
#indicoio: can we run machine learning against the sentiment and buy signals to tell us which combinations are the most relevent?



#Start a BCD to avoid corporate taxes and pass 90% net income to investors
#70% holdings U.S. private companies diversified in specific ways
#Strategy trade alongside private equity
#Targets for acquisition, Thinknum, Psychesignal, iSentiment
