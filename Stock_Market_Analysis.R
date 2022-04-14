######################################
# LUXOR STRATEGY MAIN SETUP
######################################
#install.packages("readxl")
library("readxl")

filepath <- "C:\\Users\\cagri\\OneDrive\\Masaüstü\\Stock Market Optimization\\BIST_100.xlsx"
data <- read_excel(filepath)
#install.packages("data.table")
library(data.table)
data <- as.data.frame(data)
rownames(data) <- c(data[,1])
data <- data[,-1]
data <- as.xts(data)
stock("data", currency = "TRY")
#install.packages("devtools")
require(devtools)
#install_github("braverock/blotter") # dependency
#install_github("braverock/quantstrat")

require(quantstrat)
Sys.setenv(TZ="UTC")

# If you previously run the same strategy: 
# You should first remove old strategy/order book/account/portfolio objects 
rm.strat(strategy.st)
rm.strat(portfolio.st)
rm.strat(account.st)
if (!exists('.blotter')) .blotter <- new.env()
if (!exists('.strategy')) .strategy <- new.env() 



### FinancialInstrument
currency(c('TRY', 'USD'))
exchange_rate('TRYUSD', tick_size=0.0001)

### quantmod
initDate = '2017-03-10'
.from=initDate
.to='2022-04-08'
#getSymbols.FI(Symbols='XU100.IS',
#              dir=system.file('extdata',package='quantstrat'),
 #             # dir='~/R/OHLC',
  #            from=.from, to=.to)

symbols <- c("XU100.IS")
getSymbols(symbols)
### blotter
strategy.st = 'luxor'
portfolio.st = 'forex'
account.st = 'IB'

initPortf(portfolio.st, symbols="data", initDate=initDate)
initAcct(account.st, portfolios=portfolio.st, initDate=initDate)

### quantstrat
initOrders(portfolio.st, initDate=initDate)

### define strategy
strategy(strategy.st, store=TRUE)
summary(get.strategy(strategy.st))

### indicators
.fast = 10
.slow = 30
add.indicator(strategy.st, 
              name = "SMA",
              arguments = list(
                x = quote(Cl(data)[,1]),
                n = .fast
              ),
              label="nFast"
)
add.indicator(strategy.st, 
              name="SMA",
              arguments = list(
                x = quote(Cl(data)[,1]),
                n = .slow
              ),
              label="nSlow"
)

summary(get.strategy(strategy.st))

### signals
add.signal(strategy.st, name='sigCrossover',
           arguments = list(
             columns=c("nFast","nSlow"),
             relationship="gte"
           ),
           label='long'
)
add.signal(strategy.st, name='sigCrossover',
           arguments = list(
             columns=c("nFast","nSlow"),
             relationship="lt"
           ),
           label='short'
)

summary(get.strategy(strategy.st))

### rules
.orderqty = 100000
.threshold = 0.0005
.txnfees = -6 # round-trip fee
add.rule(strategy.st, name='ruleSignal',
         arguments=list(sigcol='long' , 
                        sigval=TRUE,
                        orderside='long' ,
                        ordertype='stoplimit', 
                        prefer='High', 
                        threshold=.threshold,
                        orderqty=+.orderqty,
                        replace=FALSE
         ),
         type='enter',
         label='EnterLONG'
)
add.rule(strategy.st, name='ruleSignal',
         arguments=list(sigcol='short', sigval=TRUE,
                        orderside='long' ,
                        ordertype='market',
                        orderqty='all',
                        TxnFees=.txnfees,
                        replace=TRUE
         ),
         type='exit',
         label='Exit2SHORT'
)
add.rule(strategy.st, name='ruleSignal',
         arguments=list(sigcol='short', 
                        sigval=TRUE,
                        orderside='short',
                        ordertype='stoplimit', prefer='Low', threshold=-.threshold,
                        orderqty=-.orderqty,
                        replace=FALSE
         ),
         type='enter',
         label='EnterSHORT'
)
add.rule(strategy.st, name='ruleSignal',
         arguments=list(sigcol='long' , sigval=TRUE,
                        orderside='short',
                        ordertype='market',
                        orderqty='all',
                        TxnFees=.txnfees,
                        replace=TRUE
         ),
         type='exit',
         label='Exit2LONG'
)


summary(get.strategy(strategy.st))


###############################################################################
# Apply strategy
applyStrategy(strategy.st, portfolio.st)

# Updates

updatePortf(portfolio.st)
updateAcct(account.st)
updateEndEq(account.st)

###############################################################################
# Analyze indicators, signals, orders, txns
View(mktdata)
View(getOrderBook(portfolio.st)[[portfolio.st]]$data)
View(t(tradeStats(portfolio.st, 'data')))
View(perTradeStats(portfolio.st))

# MFE and MAE charts
chart.ME(portfolio.st, "data", scale='percent', type='MAE')
chart.ME(portfolio.st, 'data', scale='percent', type='MFE')

# Analyze portfolio object
myPort <- getPortfolio(portfolio.st)
names(myPort)

names(myPort$symbols)
names(myPort$symbols$data)
head(myPort$symbols$data$txn)
head(myPort$symbols$data$posPL.USD)
head(myPort$symbols$data$posPL)

names(myPort$summary)

library(lattice)
plot(xyplot(myPort$summary,xlab="",type="h",col=4))

###############################################################################
# Perf chart and equity
chart.Posn(portfolio.st, "data")

Eq<-getAccount(account.st)$summary[,"End.Eq"]
plot(as.zoo(Eq))
###############################################################################
# save the strategy in an .RData object for later retrieval
save.strategy(strategy.st)
