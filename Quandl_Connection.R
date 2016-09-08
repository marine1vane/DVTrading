library(Quandl)

mydata = Quandl("OPEC/ORB")
mydata = Quandl("FRED/GDP", collapse="annual")
mydata = Quandl("FRED/GDP", transform="rdiff")
mydata = Quandl("FRED/GDP", start_date="2001-12-31", end_date="2005-12-31")
Quandl.api_key("_8ZxvPfqX66g3n7ps55h") #marine1vane@gmail.com account
Quandl.search("Futures")



Zacks <- Quandl.dataset.get("ZAR//FF", list(rows=5))
Zacks <- Quandl.dataset.get("ZAR//FF", list(rows=5)) #need to figure how to alter the parameters
quandldata <- quandl.api(path="datasets/NSE/OIL", http="GET")
head(quandldata)
Quandl.database.bulk_download_to_file("NSE/OIL", "./NSE.zip") #not working
Zacks = Quandl("ZAR//FF")

