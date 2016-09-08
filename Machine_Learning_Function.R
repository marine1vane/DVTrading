#library(quantmod)
#getSymbols("MSFT")

Stock_Machine_Learning <- function(ticker){
  ticker <- cbind(ticker, Lag(ticker[,6]))
  library(xgboost)
  predictorNames <- names(ticker)[names(ticker) != 'Lag.1'] #selects everything except the outcome
  
  train <-ticker[3:nrow(ticker)-1,]
  test <- ticker[nrow(ticker),]
  
  bst <- xgboost(data = as.matrix(train[,predictorNames]),
                 label = train$Lag.1,
                 missing = NaN,
                 verbose=0,
                 eta = 0.1,
                 gamma = 50, 
                 nround = 50,
                 subsample = 8.6,
                 objective="reg:linear")
  
  predictions <- predict(bst, as.matrix(test[,predictorNames]), outputmargin=TRUE)
  returny <- (predictions-ticker$Lag.1[nrow(ticker)])/ticker$Lag.1[nrow(ticker)]
  #between 0.02 and -0.02 is hold >.02 is buy <-.02 is sell
  return(returny)
}