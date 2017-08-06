library(quantstrat)

# function : Set up strategy in quantstrat
strategy_setup <- function()
{
  rm.strat(strat.st)
  
  currency("USD")
  stock(symbol, currency="USD", multiplier=1)
  
  initPortf(name = portfolio.st, symbols = symbol, initDate = initDate, currency = 'USD')
  initAcct(name = account.st, portfolios = portfolio.st, initDate=initDate, initEq=initEq)
  initOrders(portfolio = portfolio.st, initDate = initDate)
  strategy(name = strat.st, store=TRUE)
  
  add.indicator(strategy = strat.st, name = "RSI", 
                arguments = list(price = quote(Cl(get(symbol))), 
                                 n=.n, maType=.maType),
                label='RSI')
  
  # There are two signals:
  # The first is when fast SMA crosses above the slow SMA curve
  add.signal(strategy = strat.st, name = 'sigThreshold', 
             arguments = list(threshold=.rsiOb, column='RSI', relationship='gte', cross=TRUE),
             label='RSI.gte.rsiOb')
  add.signal(strategy = strat.st, name = "sigThreshold", 
             arguments = list(threshold=.rsiOs, column='RSI', relationship='lt', cross=TRUE),
             label='RSI.lt.rsiOs')
  
  # The first rule is to enter when the stock is oversold (i.e RSI is less than .rsiOs)
  add.rule(strat.st, name='ruleSignal', 
           arguments = list(sigcol='RSI.lt.rsiOs', sigval=TRUE, 
                            replace=FALSE,
                            orderside='long',
                            ordertype='market',  
                            orderqty=.nShs, 
                            orderset='ocolong', 
                            TxnFees = 0), type = 'enter',  label='LongEntry')
  
  
  # The second rule is to exit when the stock is overbought (i.e RSI is greater than or equal to .rsiOb)
  add.rule(strategy = strat.st, name='ruleSignal', 
           arguments = list(sigcol='RSI.gte.rsiOb', sigval=TRUE, orderqty='all', 
                            ordertype='market', orderside='long', pricemethod='market',replace=FALSE,
                            TxnFees = 0), type = 'exit', path.dep = TRUE, label='LongExit')
  
  
  add.rule(strat.st, name='ruleSignal',
           arguments = list(sigcol='RSI.lt.rsiOs', sigval=TRUE,
                            replace=TRUE,
                            orderside='long',
                            ordertype='stoplimit',
                            tmult=TRUE,
                            threshold= quote( stopLoss),
                            orderqty='all',
                            orderset='ocolong'),
           type = 'chain', parent='LongEntry',label='StopLoss', 
           enabled = FALSE)
  
}

# Function to set up optimization
optimization_setup <- function()
{
  add.distribution(strategy = strat.st,
                   paramset.label = "OPT_RSI",
                   component.type = "signal",
                   component.label = "RSI.gte.rsiOb",
                   variable = list( threshold = .rsiObRange ),
                   label = "OverBought"
  )
  
  add.distribution(strategy = strat.st,
                   paramset.label = "OPT_RSI",
                   component.type = "signal",
                   component.label = "RSI.lt.rsiOs",
                   variable = list( threshold = .rsiOsRange ),
                   label = "OverSold"
  )
}

#-----------------------------------------------------------------------------------
# Initailization part

# Retrieve daily data for Vanguard REIT Index ETF (ticker VNQ) from 2004-10-01 through 2017-05-26
symbol = "VNQ"
currency("USD")
stock(symbol, currency="USD", multiplier=1)

# etfData <- 
initDate <- '2004-09-30'
startDate <- '2004-10-01'
endDate <- '2017-05-26'

Sys.setenv(TZ="UTC")
getSymbols(symbol,from=startDate, to=endDate,index.class="POSIXct", adjust=TRUE)
assign(symbol,get(symbol))

#------------------------------------------------------------
# Part (a)

initEq <- 0               # 1000000
.nShs <- 1000             # number of shares to be bought at each entry

# Strategy, portfolio, and account names
strat.st <- "rsi"
portfolio.st <- "rsi"
account.st <- "rsi"
suppressWarnings(rm.strat(strat.st)) # reset

.n = 14                   # Number of days in moving average
.maType = "SMA"           # Type of moving average
.rsiOb = 60               # The Overbought RSI value (in %)
.rsiOs = 40               # The Oversold RSI value (in %)
stopLoss <- 0.05

strategy_setup()
applyStrategy(strategy = strat.st, portfolios = portfolio.st)

# Analysis
updatePortf(Portfolio = portfolio.st)
updateAcct(name = account.st)
updateEndEq(Account = account.st)

chart.Posn(Portfolio = portfolio.st, Symbol = symbol, TA = "add_RSI(n = .n, wilder=FALSE);")

# A plot of MAE
chart.ME(Portfolio = portfolio.st, Symbol = symbol, type = 'MAE', scale = 'percent')

tstats1 <- tradeStats(Portfolios = portfolio.st)
t(tstats1)
tstats1["Max.Drawdown"]
tstats1["Net.Trading.PL"]

# #--------------------------------------------------------
# part (b)
# stopLoss = 8%
stopLoss <- 0.08
strategy_setup()
enable.rule(strategy = strat.st, type="chain",label="StopLoss")

applyStrategy(strategy = strat.st, portfolios = portfolio.st)
updatePortf(Portfolio = portfolio.st)
updateAcct(name = account.st)
updateEndEq(Account = account.st)

chart.Posn(Portfolio = portfolio.st, Symbol = symbol, TA = "add_RSI(n = .n, wilder=FALSE);")

chart.ME(Portfolio = portfolio.st, Symbol = symbol, type = 'MAE', scale = 'percent')

tstats2 <- tradeStats(Portfolios = portfolio.st)
t(tstats2)

print("Before stoploss: ")
tstats1["Max.Drawdown"]
tstats1["Net.Trading.PL"]
print("After stoploss: ")
tstats2["Max.Drawdown"]
tstats2["Net.Trading.PL"]

# ---------------------------------------------------------------
# part (c)

.rsiObRange <- seq(50, 75, by=5)
.rsiOsRange <- seq(20, 40, by=5)

.rsiOb <- .rsiObRange[1]
.rsiOs <- .rsiOsRange[1]

strategy_setup()
optimization_setup()
results <- apply.paramset(strategy.st = strat.st, paramset.label = "OPT_RSI",
                          portfolio.st=portfolio.st, account.st=account.st, nsamples=0)

# Generate heat map 
z <- tapply(X=results$tradeStats$Profit.To.Max.Draw,
            INDEX=list(results$tradeStats$OverBought,results$tradeStats$OverSold), FUN=mean)

x <- as.numeric(rownames(z))
y <- as.numeric(colnames(z))

filled.contour(x=x,y=y,z=z,color = heat.colors, xlab="RSI Overbought",ylab="RSI OverSold")
myTitle <- paste0("Profit/MaxDrawdown: ", symbol)
title(myTitle)

# gives local maximum at 70,30
# A better stable maximum found at 55,35

# Zoom into this new region
.rsiObRange <- seq(40, 65, by=5)
.rsiOsRange <- seq(20, 40, by=5)

.rsiOb <- .rsiObRange[1]
.rsiOs <- .rsiOsRange[1]

strategy_setup()
optimization_setup()
results <- apply.paramset(strategy.st = strat.st, paramset.label = "OPT_RSI",
                          portfolio.st=portfolio.st, account.st=account.st, nsamples=0)

# Generate heat map 
z <- tapply(X=results$tradeStats$Profit.To.Max.Draw,
            INDEX=list(results$tradeStats$OverBought,results$tradeStats$OverSold), FUN=mean)

x <- as.numeric(rownames(z))
y <- as.numeric(colnames(z))

filled.contour(x=x,y=y,z=z,color = heat.colors, xlab="RSI Overbought",ylab="RSI OverSold")
myTitle <- paste0("Profit/MaxDrawdown: ", symbol)
title(myTitle)
#----------------------------------------------------------------
# part (d)

# 70,30
stopLoss <- 0.08
.rsiOb = 70               
.rsiOs = 30 
strategy_setup()
enable.rule(strategy = strat.st, type="chain",label="StopLoss")

applyStrategy(strategy = strat.st, portfolios = portfolio.st)
updatePortf(Portfolio = portfolio.st)
updateAcct(name = account.st)
updateEndEq(Account = account.st)

chart.Posn(Portfolio = portfolio.st, Symbol = symbol, TA = "add_RSI(n = .n, wilder=FALSE);")

chart.ME(Portfolio = portfolio.st, Symbol = symbol, type = 'MAE', scale = 'percent')

tstats.70.30 <- tradeStats(Portfolios = portfolio.st)
t(tstats.70.30)

.rsiOb = 55               
.rsiOs = 35 
strategy_setup()
enable.rule(strategy = strat.st, type="chain",label="StopLoss")

applyStrategy(strategy = strat.st, portfolios = portfolio.st)
updatePortf(Portfolio = portfolio.st)
updateAcct(name = account.st)
updateEndEq(Account = account.st)

chart.Posn(Portfolio = portfolio.st, Symbol = symbol, TA = "add_RSI(n = .n, wilder=FALSE);")

chart.ME(Portfolio = portfolio.st, Symbol = symbol, type = 'MAE', scale = 'percent')

tstats.55.35 <- tradeStats(Portfolios = portfolio.st)
t(tstats.55.35)

print("at 70,30: ")
tstats.70.30["Max.Drawdown"]
tstats.70.30["Net.Trading.PL"]
print("at 55,35: ")
tstats.55.35["Max.Drawdown"]
tstats.55.35["Net.Trading.PL"]
# (iv) A description of which parameter pair yields the better result
# (v) A description of any downsides to the overall optimizing result
#----------------------------------------------------------------
# part (e)
stopLoss = 0.08
.rsiOb = 55               
.rsiOs = 35 
strategy_setup()
enable.rule(strategy = strat.st, type="chain",label="StopLoss")

applyStrategy(strategy = strat.st, portfolios = portfolio.st)
updatePortf(Portfolio = portfolio.st)
updateAcct(name = account.st)
updateEndEq(Account = account.st)

txns <- as.xts(getTxns(Portfolio = portfolio.st, Symbol = symbol))
pts <- perTradeStats(Portfolio = portfolio.st)
tstats <- tradeStats(Portfolios = portfolio.st)

rsiSim <- mcsim(Portfolio = portfolio.st, Account = account.st, n = 1000,
               replacement = TRUE, use = 'txns')   

summ <-summary(rsiSim)
summ
print(paste("Mean P/L  :" ,round(summ[1,1],2),
            " Lower CI ", round(summ[1,4],2),
            " Upper CI ", round(summ[1,5],2) ))

print(paste("Mean Max Drawdown :" ,round(summ[4,1],2),
            " Lower CI ", round(summ[4,4],2),
            " Upper CI ", round(summ[4,5],2) ))

plot(rsiSim, normalize=FALSE)

hist(rsiSim, normalize = FALSE)
#------------------------------------------------------------------
# part (f)
.rsiObRange <- seq(50, 80, by=5)
.rsiOsRange <- seq(20, 40, by=5)

strategy_setup()
optimization_setup()

results <- walk.forward(
  strategy.st=strat.st,
  paramset.label='OPT_RSI',
  portfolio.st=portfolio.st,
  account.st=account.st,
  period='years',
  k.training=4,
  k.testing=1,
  nsamples=0,
  audit.prefix='wfa',
  anchored=FALSE,
  verbose=TRUE
)

PerformanceAnalytics:::textplot(t(tradeStats(portfolio.st)))
txns <- getTxns(portfolio.st,symbol)
txns$Net.Txn.Realized.PL <- round(txns$Net.Txn.Realized.PL)
PerformanceAnalytics:::textplot(head(txns))
PerformanceAnalytics:::textplot(tail(txns))

plot(getPortfolio(portfolio.st)$summary$Net.Trading.PL,minor.ticks=FALSE,type="h",col=4)
chart.Posn(portfolio.st,symbol)

#chart.forward("wfa.results.RData")
