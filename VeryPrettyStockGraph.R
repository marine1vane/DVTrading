library(quantmod)

# display a simple bar chart
getSymbols(c("AMZN"))
barChart(AMZN,theme='white.mono',bar.type='hlc')

# display a complex chart
getSymbols(c("^GSPC")) #snp 500
chartSeries(GSPC, subset='last 3 months')
addBBands(n = 20, sd = 2, ma = "SMA", draw = 'bands', on = -1)
addADX(n = 14, maType="EMA", wilder=TRUE)
addSMA(n = 10, on = 1, with.col = Cl, overlay = TRUE, col = "brown")
addMomentum()
# Pretty Beast Chart


returny <- allReturns(AMZN)
head(returny)
sum(returny$daily, na.rm=T)
sum(returny$weekly, na.rm=T)
sum(returny$monthly, na.rm=T)
sum(returny$quarterly, na.rm=T)
sum(returny$yearly, na.rm=T)