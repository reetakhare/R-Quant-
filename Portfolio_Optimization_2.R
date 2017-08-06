rm(list=ls())

library(mpo)
library(PortfolioAnalytics)
library(ROI)
library(ROI.plugin.quadprog)
library(ROI.plugin.glpk)
library(factorAnalytics)
library(lattice)

setwd("/Users/reetakhare/Desktop/CFRM/CFRM-543/Homework/Project")
retAll = read.zoo("stocks145bonds7.csv", sep=",",header = T,format = "%m/%d/%Y")

SP500 = as.xts(retAll[,157])
names(SP500) = "SP500"
returns = retAll[,1:145]
rf = as.xts(retAll[,153])  # rf is needed for performance function

#-----------------------------------------------------------------
# Functions definition
#----------------------------------------------------------------
custom.portfolio.moments.k23 = function(R, portfolio) {
  momentargs = list()
  momentargs$mu  =  matrix(as.vector(apply(R,2,"mean")), ncol = 1)
  momentargs$sigma  =  fitSfm(R,k=23)$Omega
  momentargs$m3 = matrix(0, nrow=ncol(R), ncol=ncol(R)^2)
  momentargs$m4 = matrix(0, nrow=ncol(R), ncol=ncol(R)^3)
  return(momentargs)
}

custom.portfolio.moments.k38 = function(R, portfolio) {
  momentargs = list()
  momentargs$mu  =  matrix(as.vector(apply(R,2,"mean")), ncol = 1)
  momentargs$sigma  =  fitSfm(R,k=38)$Omega
  momentargs$m3 = matrix(0, nrow=ncol(R), ncol=ncol(R)^2)
  momentargs$m4 = matrix(0, nrow=ncol(R), ncol=ncol(R)^3)
  return(momentargs)
}

Perf <- function(Retn, wts=NULL)
{
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


#----------------------------------------------------------------
# Principal component Analysis
#---------------------------------------------------------------- 

pc = prcomp(returns)
summary(pc)
tot.var <- sum(pc$sdev^2)
barplot(pc$sdev^2,col=4,ylim=c(0,max(pc$sdev^2)*1.1),
        names.arg=paste("PC",1:145,sep=""), ylab="variance")

#----------------------------------------------------------------
# Portfolio Analysis
#---------------------------------------------------------------- 

fundNames = names(returns)
pspec = portfolio.spec(assets=fundNames)
pspec.fi = add.constraint(pspec, type="full_investment")

#----------------------------------------------------------------------------------
# portfolio 1 specification
#----------------------------------------------------------------------------------
 
lambda_1 = 7
cra_1 = 0.01

pspec1 = add.constraint(pspec.fi, type="long_only")
pspec1 = add.objective(pspec1, type="quadratic_utility",risk_aversion = lambda_1)

heading.port1 = paste("QU LO lambda=7 k=38")

#----------------------------------------------------------------------------------
# portfolio 2 specification
#----------------------------------------------------------------------------------
lambda_2 = 20
cra_2 = 0.015

pspec2 = add.constraint(pspec.fi, type="long_only")
pspec2 = add.objective(pspec2, type="risk", name="var")
pspec2 = add.objective(pspec2, type="weight_concentration", name="HHI", conc_aversion=cra_2)

heading.port2 = paste("GMV LO with CRA",cra_2,"lambda",lambda_2,"k=23")
#----------------------------------------------------------------------------------
# Rebalancing / Optimization for both of the portfolio
#----------------------------------------------------------------------------------
bt1 <- optimize.portfolio.rebalancing(returns, pspec1,
                                      optimize_method="quadprog",
                                      momentFUN = "custom.portfolio.moments.k38",
                                      rebalance_on="months",training_period=60,rolling_window=60)
wts1 = extractWeights(bt1)
ret1 = Return.rebalancing(returns,na.omit(wts1))

#---------------------------------------------------------------------------------
bt2 <- optimize.portfolio.rebalancing(returns, pspec2,
                                      optimize_method="quadprog",
                                      momentFUN = "custom.portfolio.moments.k23",
                                      rebalance_on="months",training_period=60,rolling_window=60)

wts2 = extractWeights(bt2)
ret2 = Return.rebalancing(returns, na.omit(wts2))

#----------------------------------------------------------------------------------
# Performance measure of portfolios
#----------------------------------------------------------------------------------
tblP = NULL
tblP = cbind(Perf(SP500),Perf(ret1, wts1),Perf(ret2, wts2) )
names(tblP) = c("SP500",heading.port1,heading.port2)

tblP
write.table("Portfolios of 145 assets (Jan 1995-Dec 2014) ", file="part2.xls", row.name=FALSE, col.names=FALSE, sep="\t", append=FALSE)
colhead = c("-",colnames(tblP))
write.table(t(colhead), file="part2.xls", row.name=FALSE, col.names=FALSE, sep="\t", append=TRUE)
write.table(tblP, file="part2.xls", row.name=TRUE, col.names=FALSE, sep="\t", append=TRUE)

#----------------------------------------------------------------------------------
# Return Analysis / Charts
#----------------------------------------------------------------------------------
# Combine returns of 2 portfolio
ret.comb <- na.omit(merge(SP500, ret1, ret2, all=F))
names(ret.comb) = c("MARKET", heading.port1, heading.port2)

tsRainbow <- rainbow(9)
charts.PerformanceSummary(ret.comb, wealth.index = T, lty = 1, lwd=1, colorset = tsRainbow,
                          cex.legend = 1.2,cex.axis = 1.3,
                          main = "145 STOCKS 1999-2014 various combination of constraints vs SP500")
#----------------------------------------------------------------------------------
#  TO and DIV Charts
#----------------------------------------------------------------------------------
# Calculate DIV values for portfolios
DIV_QU_LO = DIV(wts1)
DIV_GMV_LO_CRA =DIV(wts2)

DIV.comb=na.omit(merge(DIV_QU_LO, DIV_GMV_LO_CRA, all=F))
xyplot(DIV.comb,scales=list(y="same"),main="The DIV values for 2 best portfolios")


# Calculate the TO values for portfolios
TO_QU_LO = TO(wts1)
TO_GMV_LO_CRA=TO(wts2)

TO.comb=na.omit(merge(TO_QU_LO, TO_GMV_LO_CRA, all=F))
xyplot(TO.comb,scales=list(y="same"),main="The TO values of 2 best portfolios")
