rm(list = ls())

library(mpo)
library(PortfolioAnalytics)
library(ROI)
library(ROI.plugin.quadprog)
library(ROI.plugin.glpk)
library(lattice)

Perf <- function(Retn, wts=NULL)
{
  rf = smallcapW[,"WeekRiskFree"]
  arg.list1 = list(Return.cumulative=list(), #
                   Return.annualized = list(), #
                   Return.annualized = list(geometric = F), #
                   maxDrawdown = list(), #
                   sd.annualized = list(), #
                   etl = list(),    #
                   VaR=list()
  )
  perf.tbl1 = table.Performance(Retn, metrics = c("Return.cumulative",
                                                  "Return.annualized",
                                                  "Return.annualized",
                                                  "maxDrawdown",
                                                  "sd.annualized",
                                                  "etl",
                                                  "VaR"),
                                metricsNames = c("Return.cumulative",
                                                 "Return.annualized",
                                                 "Return.annualized (geometric mean return)",
                                                 "maxDrawdown",
                                                 "sd.annualized",
                                                 "Expected Tail Loss(ETL)",
                                                 "Modified VaR"
                                ),
                                arg.list = arg.list1, interactive = FALSE)$resultingtable
  
  arg.list2 = list(sharpeRatio = list(annualize = F),  #
                   sharpeRatio = list(annualize = T), #
                   sharpeRatio = list(annualize = T, geometric = TRUE),
                   SortinoRatio = list(), #
                   starrRatio = list(alpha=0.05)
  )
  perf.tbl2 = table.Performance(Retn-rf, metrics = c("sharpeRatio", 
                                                     "sharpeRatio", 
                                                     "sharpeRatio",
                                                     "SortinoRatio",
                                                     "starrRatio"),
                                metricsNames = c("Sharp Ratio (SR)", 
                                                 "SRannual(geometric = F)", 
                                                 "SRannual(geometric = T)",
                                                 "SortinoRatio",
                                                 "starrRatio"
                                ),
                                arg.list = arg.list2, interactive = FALSE)$resultingtable
  perf.tbl= rbind(perf.tbl1,perf.tbl2)
  
  if(is.null(wts)) {
    Avg_TO = "NA"
    Avg_DIV = "NA"
  } else {
    TO =TO(na.omit(wts))
    Avg_TO = mean(TO)
    DIV = DIV(na.omit(wts))
    Avg_DIV = mean(DIV)
  }
  p = rbind(perf.tbl,Avg_TO,Avg_DIV)
  rownames(p)[13:14] = c("TO","DIV")
  return(p)
}

# Weekly returns Jan-1997 through Dec-2010
MARKET = smallcapW[,"Weekvwretd"]
returns = smallcapW[,1:20]

funds = colnames(returns)
pspec = portfolio.spec(assets=funds)
pspec.fi = add.constraint(pspec, type="full_investment")

#----------------------------------------------------------------------------------
# portfolio 1 specification
#----------------------------------------------------------------------------------
rebalance_1 = "weeks"
train_period_1 = 104
cra_1 = 0.015

pspec.Lo = add.constraint(pspec.fi, type="long_only")
pspec.gmvLo = add.objective(pspec.Lo, type="risk", name="var")
pspec.gmvLo.cra = add.objective(pspec.gmvLo, type="weight_concentration", name="HHI", conc_aversion=cra_1)

pspec1 = pspec.gmvLo.cra
heading.port1 = paste("GMV LO with CRA",cra_1,rebalance_1,train_period_1)

#----------------------------------------------------------------------------------
# portfolio 2 specification
#----------------------------------------------------------------------------------
rebalance_2 = "weeks"
train_period_2 = 104
cra_2 = 0.015
l_box_2 = -.01
u_box_2 = .25

pspec.Box = add.constraint(pspec.fi,type="box",min=l_box_2, max=u_box_2)
pspec.gmvBox = add.objective(pspec.Box, type="risk", name="var")
pspec.gmvBox.cra = add.objective(pspec.gmvBox, type="weight_concentration", name="HHI", conc_aversion=cra_2)

pspec2 = pspec.gmvBox.cra
heading.port2 = paste("GMV Box with CRA",cra_2,rebalance_2,train_period_2)

#----------------------------------------------------------------------------------
# Rebalancing / Optimization for both of the portfolio
#----------------------------------------------------------------------------------
bt1 <- optimize.portfolio.rebalancing(returns, pspec1,
                                          optimize_method="quadprog",
                                          rebalance_on=rebalance_1,
                                          training_period=train_period_1,
                                          rolling_window=train_period_1)
wts1 = extractWeights(bt1)
ret1 = Return.rebalancing(returns,wts1)

#---------------------------------------------------------------------------------
bt2 <- optimize.portfolio.rebalancing(returns, pspec2,
                                           optimize_method="quadprog",
                                           rebalance_on=rebalance_2,
                                           training_period=train_period_2,
                                           rolling_window=train_period_2)

wts2 = extractWeights(bt2)
ret2 = Return.rebalancing(returns, wts2)
#----------------------------------------------------------------------------------
# Performance measure of portfolios
#----------------------------------------------------------------------------------
tblP = NULL
tblP = cbind(Perf(MARKET),Perf(ret1, wts1),Perf(ret2, wts2) )
names(tblP) = c("MARKET",heading.port1,heading.port2)

tblP
write.table("Portfolios of smallcapW first 20 stocks (Jan 1997-Dec 2010) ", file="part1.xls", row.name=FALSE, col.names=FALSE, sep="\t", append=FALSE)
colhead = c("-",colnames(tblP))
write.table(t(colhead), file="part1.xls", row.name=FALSE, col.names=FALSE, sep="\t", append=TRUE)
write.table(tblP, file="part1.xls", row.name=TRUE, col.names=FALSE, sep="\t", append=TRUE)


#----------------------------------------------------------------------------------
# Return Analysis / Charts
#----------------------------------------------------------------------------------
# Combine returns of 2 portfolio
ret.comb <- na.omit(merge(MARKET, ret1, ret2, all=F))
names(ret.comb) = c("MARKET", heading.port1, heading.port2)

tsRainbow <- rainbow(9)
charts.PerformanceSummary(ret.comb, wealth.index = T, lty = 1, lwd=c(1,1,1), colorset = tsRainbow,
                          cex.legend = 1.2,cex.axis = 1.3)

#----------------------------------------------------------------------------------
#  TO and DIV Charts
#----------------------------------------------------------------------------------
# Calculate DIV values for portfolios
DIV_GMV_LO_CRA = DIV(wts1)
DIV_GMV_BOX_CRA =DIV(wts2)

DIV.comb=na.omit(merge(DIV_GMV_LO_CRA, DIV_GMV_BOX_CRA, all=F))
xyplot(DIV.comb,scales=list(y="same"),main="The DIV values for 2 best portfolios")

  
# Calculate the TO values for portfolios
TO_GMV_LO_CRA = TO(wts1)
TO_GMV_BOX_CRA=TO(wts2)

TO.comb=na.omit(merge(TO_GMV_LO_CRA, TO_GMV_BOX_CRA, all=F))
xyplot(TO.comb,scales=list(y="same"),main="The TO values of 2 best portfolios")


