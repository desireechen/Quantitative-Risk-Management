# install.packages("xlsx")
library(xlsx)
data <- read.xlsx("US Stocks.xlsx", sheetName = "Stock Data")
data$date <- as.Date(as.character(data$date), "%Y%m%d")
# Separate the stocks and take only the close prices of the stocks.
IBM <- data[which(data$TICKER == "IBM"), c(1,5)]
AAPL <- data[which(data$TICKER == "AAPL"), c(1,5)]
CVS <- data[which(data$TICKER == "CVS"), c(1,5)]
WMT <- data[which(data$TICKER == "WMT"), c(1,5)]
# Extract the dates for each half of the data.
date1 <- IBM[1:1259,]$date # ROBUSTNESS: to change this to IBM[1260:2517,]$date
date2 <- IBM[1260:2517,]$date # ROBUSTNESS: to change this to IBM[1:1259,]$date
# Rename the columns.
colnames(IBM)[2] <- "IBM"
colnames(AAPL)[2] <- "AAPL"
colnames(CVS)[2] <- "CVS"
colnames(WMT)[2] <- "WMT"
# Merge the prices to form a dataframe of prices of the 4 stocks.
df1 <- merge.data.frame(IBM, AAPL, by = "date")
df2 <- merge.data.frame(CVS, WMT, by = "date")
prices.df <- merge.data.frame(df1, df2, by = "date")
# The stocks are allocated equally in the portfolio. Get the daily average of the prices. 
prices.df$portfolio <- rowMeans(prices.df[,2:5])
# install.packages("xts")
library(xts)
# Make the dataframe an extensible time series, so that I can get dailyReturn later.
prices <- xts(prices.df, order.by = prices.df[,1])[,-1]
# Take the relevant column of each stock from the extensible time series.
IBM <- prices[,1]
AAPL <- prices[,2]
CVS <- prices[,3]
WMT <- prices[,4]
# Make numeric. IBM$Data changes from character to numeric.
storage.mode(IBM) <- "numeric"
storage.mode(AAPL) <- "numeric"
storage.mode(CVS) <- "numeric"
storage.mode(WMT) <- "numeric"
# install.packages("quantmod")
library(quantmod)
# Get daily returns of each of the 4 stocks.
IBM_daily_return <- dailyReturn(IBM) 
AAPL_daily_return <- dailyReturn(AAPL)
CVS_daily_return <- dailyReturn(CVS)
WMT_daily_return <- dailyReturn(WMT)
# Place all the daily returns of the 4stocks in a dataframe.
returns <- data.frame(IBM_daily_return, AAPL_daily_return, 
                      CVS_daily_return, WMT_daily_return)
names(returns) <- c("IBM","AAPL","CVS","WMT")
fitted.half <- returns[1:1259,] # ROBUSTNESS: to change this to returns[1260:2517,]
actual.half <- returns[1260:2517,] # ROBUSTNESS: to change this to returns[1:1259,]
########## Problem 1.2
# install.packages("rugarch")
library(rugarch)
# Creating a univariate GARCH specification object prior to fitting
# garchOrder is the ARCH(q) and GARCH(p) orders
# armaOrder is the autoregressive and moving average orders.
# distribution.model can be norm or std.
# Code below will create a uGARCHspec object to be used in the multispec and ugarchfit functions later.
spec.uni <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1), 
                                         submodel = NULL, external.regressors = NULL, 
                                         variance.targeting = FALSE),
                   mean.model = list(armaOrder = c(1, 0), external.regressors = NULL, 
                                     distribution.model = "norm", start.pars = list(), 
                                     fixed.pars = list()))
# Creating multiple univariate GARCH specification objects prior to fitting
# Replicate 4 uGARCHspec objects; one for each stock.
# Code below will create a uGARCHmultispec object to be used in the dccspec function later.
uspec = multispec(replicate(4, spec.uni))
# install.packages("rmgarch")
library(rmgarch)
# Creating a DCC-GARCH specification object prior to fitting
# Code below will create a DCCspec object to be used in the dccfit function later.
spec.multi = dccspec(uspec = uspec, dccOrder = c(1, 1), distribution = 'mvnorm')
# Creating a DCC-GARCH fit object over the full model and half model
# Code below will create a DCCfit object to be used in the dccsim function later.
full_model <- dccfit(spec = spec.multi, data = returns)
full_model
half_model <- dccfit(spec = spec.multi, data = fitted.half)
half_model
half_model@mfit$coef
# Plot DCC conditional mean
plot(half_model, which = 1, series = c(1,2)) # for IBM and AAPL stocks
plot(half_model, which = 1, series = c(3,4)) # for CVS and WMT stocks
# Plot DCC conditional sigma vs Absolute returns
plot(half_model, which = 2, series = c(1,2))
plot(half_model, which = 2, series = c(3,4))
# which=3 plots DCC conditional covariance, which=4 plots DCC conditional correlation
# which=5 plots DW portfolio with 1% VaR limits based on conditional weighted density
########## Problem 1.3
# Simulation
NSimulations = 1258 # ROBUSTNESS: This number needs to be changed to 1259 when we use 2nd half to fit.
# Creating a DCC-GARCH simulation object
set.seed(123)
simulated <- dccsim(half_model, n.sim = NSimulations, n.start = 0, 
                    m.sim = 1, startMethod = "unconditional")
simulated.mean <- simulated@msim$simX
simulated.mean <- as.data.frame(simulated.mean)
simulated.covariance <- simulated@msim$simH[[1]] # This is 4 by 4 by 1258.
# Create dataframes to store VaRs from simulated means and covariances.
var95 <- data.frame(ibm_var=double(),aapl_var=double(),cvs_var=double(),wmt_var=double())
var99 <- data.frame(ibm_var=double(),aapl_var=double(),cvs_var=double(),wmt_var=double())
z95 = -1.645
z99 = -2.326
for(x in 1:NSimulations)
{
  var95[x,1] <- simulated.mean[x,1] + z95*(simulated.covariance[1,1,x]^0.5)
  var95[x,2] <- simulated.mean[x,2] + z95*(simulated.covariance[2,2,x]^0.5)
  var95[x,3] <- simulated.mean[x,3] + z95*(simulated.covariance[3,3,x]^0.5)
  var95[x,4] <- simulated.mean[x,4] + z95*(simulated.covariance[4,4,x]^0.5)
  var99[x,1] <- simulated.mean[x,1] + z99*(simulated.covariance[1,1,x]^0.5)
  var99[x,2] <- simulated.mean[x,2] + z99*(simulated.covariance[2,2,x]^0.5)
  var99[x,3] <- simulated.mean[x,3] + z99*(simulated.covariance[3,3,x]^0.5)
  var99[x,4] <- simulated.mean[x,4] + z99*(simulated.covariance[4,4,x]^0.5)
}
# Checking if matrix are positive definite
# install.packages("Matrix")
# library(Matrix)
# install.packages("fBasics")
library(fBasics)
positive_matrix <- data.frame()
for (i in 1:NSimulations)   {
  A <- simulated.covariance[,,i]
  positive_matrix[i,1]<-isPositiveDefinite(A)
}
length(which(positive_matrix[,1]=="FALSE")) # length zero means that all matrix are positive definite
# Visualization
# Combine actual returns with simulated VaRs
combined <- cbind(actual.half,var95)
combined["index"] <- date2
# Actual Returns and Simulated 95%VaR for all 4 stocks
library(reshape2)
melted = melt(combined, id.vars="index")
library(ggplot2)
# if ggplot has error message, dev.off()
ggplot(data=melted, aes(x=index, y=value, group=variable,colour=variable)) + geom_line() + 
  xlab("Time") + ylab("Returns") + ggtitle("Actual Returns and 95% VaR")
# 95% VaRs of all 4 stocks
var95["index"] <- date2
melted95 = melt(var95, id.vars="index")
ggplot(data=melted95, aes(x=index, y=value, group=variable,colour=variable)) + 
  geom_line() + xlab("Time") + ylab("VaR") + ggtitle("95% VaR")
# 99% VaRs of all 4 stocks
var99["index"] <- date2
melted99 = melt(var99, id.vars="index")
ggplot(data=melted99, aes(x=index, y=value, group=variable,colour=variable)) + 
  geom_line() + xlab("Time") + ylab("VaR") + ggtitle("99% VaR")
# IBM actual returns, simulated 95% and 99% VaRs
IBM.comparison <- data.frame(IBM_Actual=actual.half$IBM, IBM_simulated_var95=var95$ibm_var, 
                     IBM_simulated_var99=var99$ibm_var)
IBM.comparison["index"] <- date2
melted.ibm = melt(IBM.comparison, id.vars="index")
ggplot(data=melted.ibm, aes(x=index, y=value, group=variable,colour=variable)) + 
  geom_line() + ggtitle("IBM Actual Return VS Simulated VaRs")
# AAPL actual returns, simulated 95% and 99% VaRs
AAPL.comparison <- data.frame(AAPL_Actual=actual.half$AAPL, AAPL_simulated_var95=var95$aapl_var, 
                             AAPL_simulated_var99=var99$aapl_var)
AAPL.comparison["index"] <- date2
melted.aapl = melt(AAPL.comparison, id.vars="index")
ggplot(data=melted.aapl, aes(x=index, y=value, group=variable,colour=variable)) + 
  geom_line() + ggtitle("AAPL Actual Return VS Simulated VaRs")
# CVS actual returns, simulated 95% and 99% VaRs
CVS.comparison <- data.frame(CVS_Actual=actual.half$CVS, CVS_simulated_var95=var95$cvs_var, 
                              CVS_simulated_var99=var99$cvs_var)
CVS.comparison["index"] <- date2
melted.cvs = melt(CVS.comparison, id.vars="index")
ggplot(data=melted.cvs, aes(x=index, y=value, group=variable,colour=variable)) + 
  geom_line() + ggtitle("CVS Actual Return VS Simulated VaRs")
# WMT actual returns, simulated 95% and 99% VaRs
WMT.comparison <- data.frame(WMT_Actual=actual.half$WMT, WMT_simulated_var95=var95$wmt_var, 
                             WMT_simulated_var99=var99$wmt_var)
WMT.comparison["index"] <- date2
melted.wmt = melt(WMT.comparison, id.vars="index")
ggplot(data=melted.wmt, aes(x=index, y=value, group=variable,colour=variable)) + 
  geom_line() + ggtitle("WMT Actual Return VS Simulated VaRs")
# Violation
IBM.comparison["exceed_var95"] = IBM.comparison$IBM_Actual < IBM.comparison$IBM_simulated_var95
IBM.comparison["exceed_var99"] = IBM.comparison$IBM_Actual < IBM.comparison$IBM_simulated_var99
# Percentage of violation
mean(IBM.comparison$exceed_var95)
mean(IBM.comparison$exceed_var99)
length(which(IBM.comparison$exceed_var95 == "TRUE"))
binom.test(c(length(which(IBM.comparison$exceed_var95 == "FALSE")), 
             length(which(IBM.comparison$exceed_var95=="TRUE"))), p = 0.95) # smaller p value is better
length(which(IBM.comparison$exceed_var99 == "TRUE"))
binom.test(c(length(which(IBM.comparison$exceed_var99 == "FALSE")), 
             length(which(IBM.comparison$exceed_var99 == "TRUE"))), p = 0.99)
AAPL.comparison["exceed_var95"] = AAPL.comparison$AAPL_Actual < AAPL.comparison$AAPL_simulated_var95
AAPL.comparison["exceed_var99"] = AAPL.comparison$AAPL_Actual < AAPL.comparison$AAPL_simulated_var99
mean(AAPL.comparison$exceed_var95)
mean(AAPL.comparison$exceed_var99)
length(which(AAPL.comparison$exceed_var95 == "TRUE"))
binom.test(c(length(which(AAPL.comparison$exceed_var95 == "FALSE")), 
             length(which(AAPL.comparison$exceed_var95=="TRUE"))), p = 0.95)
length(which(AAPL.comparison$exceed_var99 == "TRUE"))
binom.test(c(length(which(AAPL.comparison$exceed_var99 == "FALSE")), 
             length(which(AAPL.comparison$exceed_var99 == "TRUE"))), p = 0.99)
CVS.comparison["exceed_var95"] = CVS.comparison$CVS_Actual < CVS.comparison$CVS_simulated_var95
CVS.comparison["exceed_var99"] = CVS.comparison$CVS_Actual < CVS.comparison$CVS_simulated_var99
mean(CVS.comparison$exceed_var95)
mean(CVS.comparison$exceed_var99)
length(which(CVS.comparison$exceed_var95 == "TRUE"))
binom.test(c(length(which(CVS.comparison$exceed_var95 == "FALSE")), 
             length(which(CVS.comparison$exceed_var95=="TRUE"))), p = 0.95)
length(which(CVS.comparison$exceed_var99 == "TRUE"))
binom.test(c(length(which(CVS.comparison$exceed_var99 == "FALSE")), 
             length(which(CVS.comparison$exceed_var99 == "TRUE"))), p = 0.99)
WMT.comparison["exceed_var95"] = WMT.comparison$WMT_Actual < WMT.comparison$WMT_simulated_var95
WMT.comparison["exceed_var99"] = WMT.comparison$WMT_Actual < WMT.comparison$WMT_simulated_var99
mean(WMT.comparison$exceed_var95)
mean(WMT.comparison$exceed_var99)
length(which(WMT.comparison$exceed_var95 == "TRUE"))
binom.test(c(length(which(WMT.comparison$exceed_var95 == "FALSE")), 
             length(which(WMT.comparison$exceed_var95=="TRUE"))), p = 0.95)
length(which(WMT.comparison$exceed_var99 == "TRUE"))
binom.test(c(length(which(WMT.comparison$exceed_var99 == "FALSE")), 
             length(which(WMT.comparison$exceed_var99 == "TRUE"))), p = 0.99)
########## Problem 1.4
# Univariate portfolio by taking the daily average of the returns of the 4 stocks
returns["portfolio"] <- rowMeans(returns)
fitted.half["portfolio"] <- rowMeans(fitted.half)
actual.half["portfolio"] <- rowMeans(actual.half)
# Fit a univariate garch model
# Code below will create a uGARCHfit object to be used in the ugarchsim function later.
uni.fit <- ugarchfit(spec = spec.uni, data = fitted.half$portfolio , solver.control = list(trace=1))
uni.fit
plot(uni.fit,which="all")
# Simulating from a univariate GARCH model
set.seed(123)
uni.sim = ugarchsim(uni.fit, n.sim = NSimulations, m.sim = 1, startMethod = 'unconditional', rseed = 10)
portfolio.mean <- uni.sim@simulation$seriesSim
portfolio.sd <- uni.sim@simulation$sigmaSim
portfolio.var95 <- portfolio.mean + z95*portfolio.sd
portfolio.var99 <- portfolio.mean + z99*portfolio.sd
portfolio.uni <- data.frame(portfolio_Actual=actual.half$portfolio, 
                                  portfolio_simulated_var95=portfolio.var95, 
                                  portfolio_simulated_var99=portfolio.var99)
portfolio.uni["index"] <- date2 
melted.portfolio.uni = melt(portfolio.uni, id.vars="index")
ggplot(data=melted.portfolio.uni, aes(x=index, y=value, group=variable,colour=variable)) + 
  geom_line() + ggtitle("Univariate Portfolio Actual Return VS Simulated VaRs")
portfolio.uni["exceed_var95"] = portfolio.uni$portfolio_Actual < portfolio.uni$portfolio_simulated_var95
portfolio.uni["exceed_var99"] = portfolio.uni$portfolio_Actual < portfolio.uni$portfolio_simulated_var99
mean(portfolio.uni$exceed_var95)
mean(portfolio.uni$exceed_var99)
length(which(portfolio.uni$exceed_var95 == "TRUE"))
binom.test(c(length(which(portfolio.uni$exceed_var95 == "FALSE")), 
             length(which(portfolio.uni$exceed_var95=="TRUE"))), p = 0.95)
length(which(portfolio.uni$exceed_var99 == "TRUE"))
binom.test(c(length(which(portfolio.uni$exceed_var99 == "FALSE")), 
             length(which(portfolio.uni$exceed_var99=="TRUE"))), p = 0.99)
# Multivariate portfolio by taking the daily average of the VaRs of the 4 stocks
# Just taking the first 4 columns of the VaR dataframes
var95 <- var95[,1:4]
var99 <- var99[,1:4]
var95["average"] <- rowMeans(var95)
var99["average"] <- rowMeans(var99)
portfolio.multi <- data.frame(portfolio_Actual=actual.half$portfolio, 
                              portfolio_simulated_var95=var95$average, 
                              portfolio_simulated_var99=var99$average)
portfolio.multi["index"] <- date2 
melted.portfolio.multi = melt(portfolio.multi, id.vars="index")
ggplot(data=melted.portfolio.multi, aes(x=index, y=value, group=variable,colour=variable)) + 
  geom_line() + ggtitle("Multivariate Portfolio Actual Return VS Simulated VaRs")
portfolio.multi["exceed_var95"] = portfolio.multi$portfolio_Actual < portfolio.multi$portfolio_simulated_var95
portfolio.multi["exceed_var99"] = portfolio.multi$portfolio_Actual < portfolio.multi$portfolio_simulated_var99
mean(portfolio.multi$exceed_var95)
mean(portfolio.multi$exceed_var99)
length(which(portfolio.multi$exceed_var95 == "TRUE"))
binom.test(c(length(which(portfolio.multi$exceed_var95 == "FALSE")), 
             length(which(portfolio.multi$exceed_var95=="TRUE"))), p =0.95)
length(which(portfolio.multi$exceed_var99 == "TRUE"))
binom.test(c(length(which(portfolio.multi$exceed_var99 == "FALSE")), 
             length(which(portfolio.multi$exceed_var99=="TRUE"))), p =0.99)
########## Problem 2 on portfolio optimization
# install.packages("quadprog")
library(quadprog)
set.seed(123)
eff.frontier <- function (returns,cov, short="no", max.allocation=NULL, 
                          risk.premium.up=.5, risk.increment=.005){
  # return argument should be a m x n matrix with one column per security
  # short argument is whether short-selling is allowed; default is no (short selling prohibited)
  # max.allocation is the maximum % allowed for any one security (reduces concentration)
  # risk.premium.up is the upper limit of the risk premium modeled (see for loop below)
  # risk.increment is the increment (by) value used in the for loop
  covariance <- cov
  print(covariance)
  n <- ncol(covariance)
  # Create initial Amat and bvec assuming only equality constraint (short-selling is allowed, no allocation constraints)
  Amat <- matrix (1, nrow=n)
  bvec <- 1
  meq <- 1
  # Then modify the Amat and bvec if short-selling is prohibited
  if(short=="no"){
    Amat <- cbind(1, diag(n))
    bvec <- c(bvec, rep(0, n))
  }
  # And modify Amat and bvec if a max allocation (concentration) is specified
  if(!is.null(max.allocation)){
    if(max.allocation > 1 | max.allocation <0){
      stop("max.allocation must be greater than 0 and less than 1")
    }
    if(max.allocation * n < 1){
      stop("Need to set max.allocation higher; not enough assets to add to 1")
    }
    Amat <- cbind(Amat, -diag(n))
    bvec <- c(bvec, rep(-max.allocation, n))
  }
  # Calculate the number of loops based on how high to vary the risk premium and by what increment
  loops <- risk.premium.up / risk.increment + 1
  loop <- 1
  # Initialize a matrix to contain allocation and statistics
  # This is not necessary, but speeds up processing and uses less memory
  eff <- matrix(nrow=loops, ncol=n+3)
  # Now I need to give the matrix column names
  colnames(eff) <- c(colnames(returns), "Std.Dev", "Exp.Return", "sharpe")
  # Loop through the quadratic program solver
  for (i in seq(from=0, to=risk.premium.up, by=risk.increment)){
    dvec <- colMeans(returns) * i # This moves the solution up along the efficient frontier
    sol <- solve.QP(covariance, dvec=dvec, Amat=Amat, bvec=bvec, meq=meq)
    eff[loop,"Std.Dev"] <- sqrt(sum(sol$solution *colSums((covariance * sol$solution))))
    eff[loop,"Exp.Return"] <- as.numeric(sol$solution %*% colMeans(returns))
    eff[loop,"sharpe"] <- eff[loop,"Exp.Return"] / eff[loop,"Std.Dev"]
    eff[loop,1:n] <- sol$solution
    loop <- loop+1
  }
  return(as.data.frame(eff))
}
optimal.weight=data.frame(X1=double(),X2=double(),X3=double(),X4=double(),
                          Std.Dev=double(),Exp.Return=double(),sharpe=double())
# Run the eff.frontier function based on no short and 100% alloc. restrictions for all simulations
set.seed(123)
for (x in 1:NSimulations){
  eff <- eff.frontier(returns=simulated.mean[x,],cov=simulated.covariance[1:4,1:4,x],
                      short="no", max.allocation=1, risk.premium.up=1, risk.increment=.001)
  # Find the optimal portfolio
  eff.optimal.point <- eff[eff$sharpe==max(eff$sharpe),]
  optimal.weight[x,] <- eff.optimal.point
}
# Get the mean of all the simulated optimal weights for each stock
ibm.opt <- mean(optimal.weight$X1)
apple.opt <- mean(optimal.weight$X2)
cvs.opt <- mean(optimal.weight$X3)
wmt.opt <- mean(optimal.weight$X4)
ibm.opt
apple.opt
cvs.opt
wmt.opt
# Portfolio return is actual return of each stock multiplied by the optimal weight of the stock
portfolio.opt = actual.half$IBM*ibm.opt + actual.half$AAPL*apple.opt 
              + actual.half$CVS*cvs.opt + actual.half$WMT*wmt.opt
mean(portfolio.opt)
sd(portfolio.opt)
quantile(portfolio.opt,c(0.01,0.05))
# Compared to the benchmark of equal allocation for all 4 stocks.
benchmark <- rowMeans(actual.half)
mean(benchmark)
sd(benchmark)
quantile(benchmark,c(0.01,0.05))