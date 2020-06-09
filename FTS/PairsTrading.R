library(quantmod)
library(tseries)
library(xts)
library(urca)

#Use linear regression to get the hedge ratio and spread.
CalculateSpread <- function(price.pair, method = lm)
{
  #  x <- log(price.pair)
  x <- price.pair
  reg <- method(x[,1] ~ x[,2])
  hedge.ratio <- as.numeric(reg$coef[2])
  premium     <- as.numeric(reg$coef[1])
  spread      <- x[,1] - (hedge.ratio * x[,2] + premium)
  
  return(list(spread = spread, hedge.ratio = hedge.ratio, premium = premium))
}

#Return wether spread is stationary or not using PP test and ADF test
StationaryCheck <- function(spread, threshold=0.05)
{
  Is.passed.PP.test  <- PP.test(as.numeric(spread))$p.value <= threshold
  Is.passed.adf.test <- adf.test(as.numeric(spread))$p.value <= threshold
  c(PP.test = Is.passed.PP.test, adf.test = Is.passed.adf.test)
}

#Use Johansen-Procedure Test to check whether the two time series are co-integrated
CoIntegration <- function(price.pair)
{
  x <- price.pair
  m1=ar(x)
  return(ca.jo(x,K=max(m1$order,2)))
}

#Estimate the parameter on the trading window
EstimateParameter <- function(price.pair, period, method = lm)
{
  Applied <- function(price.pair)
  {
    reg <- CalculateSpread(price.pair, method)
    c(spread = as.numeric(last(reg$spread)), hedge.ratio = reg$hedge.ratio, premium = reg$premium)
  }
  as.xts(rollapplyr(price.pair, period, Applied, by.column = FALSE))
}


#Plot the maxdrawdown of the time series
PlotMaxDrawDown <- function(x)
{
  mdd=maxdrawdown(x)
  plot(x)
  segments(time(x)[mdd$from], x[mdd$from],time(x)[mdd$to], x[mdd$from], col="grey")
  segments(time(x)[mdd$from], x[mdd$to],time(x)[mdd$to], x[mdd$to], col="grey")
  mid <- time(x)[(mdd$from + mdd$to)/2]
  arrows(mid, x[mdd$from], mid, x[mdd$to], col="red", length = 0.16)
  #return(mdd$maxdrawdown)
}

#Use iteration to find the Optimized parameter for trading
OptimizeParameter <- function(stock.pairs,a,b){
  V=matrix(0,a,b)
  for (i in 1:a){
    for (j in 0:b){
      phi=j/10
      tradingwindow=i*10
      parame=EstimateParameter(stock.pairs, period = tradingwindow)
      entry=min(mean(parame$spread[!is.na(parame$spread)][1:tradingwindow])+phi*sqrt(var(parame$spread[!is.na(parame$spread)][1:tradingwindow])),max(parame$spread[tradingwindow:length(parame$spread)]))
      exit=min(mean(parame$spread[!is.na(parame$spread)][1:tradingwindow])-phi*sqrt(var(parame$spread[!is.na(parame$spread)][1:tradingwindow])),max(parame$spread[tradingwindow:length(parame$spread)]))
      sig=ststSimple(parame$spread,as.numeric(entry),as.numeric(exit))
      returns.pairtrading <- Return(stock.pairs, lag(sig), lag(parame$hedge.ratio))
      return=cumprod(1 + returns.pairtrading)
      V[i,j]=return[as.numeric(length(return[,1])),1]
    }
  }
  return(V)
}

#Generate trading signal with entry and spread
ststSimple <- function(spread, spread.entry, spread.exit)
{
  signal <- ifelse(spread >=   spread.entry, -1, NA)
  signal <- ifelse(spread <=   spread.exit,  1, signal)
  #return(signal)
  return(na.locf(signal))
}

#Transfer the hedge ratio to the weight of the portfolio
HedgeRatio2Weight <- function(hedge.ratio)
{
  hedge.ratio <- abs(hedge.ratio) * (-1)
  #
  normalization.factor <- 1 / (1 + abs(hedge.ratio))
  return(cbind(1 * normalization.factor, hedge.ratio * normalization.factor))
}

#Generating the return for the total period
Return <- function(price.pair, signal.lagged, hedge.ratio.lagged)
{
  #
  signal      <- as.xts(na.omit(cbind(signal.lagged, -1*(signal.lagged))))
  return.pair <- as.xts(na.omit(.return(price.pair, type = "discrete")))
  weight.pair <- as.xts(na.omit(HedgeRatio2Weight(hedge.ratio.lagged)))
  #
  #names(return.pair) <- names(price.pair)
  #names(signal)      <- names(price.pair)
  #names(weight.pair) <- names(price.pair) 
  #as.xts(apply(signal * weight.pair * return.pair, 1, sum) * leverage)
  x <-          as.xts(apply(merge(signal[, 1], weight.pair[, 1], return.pair[, 1], all = FALSE), 1, prod))
  x <- merge(x, as.xts(apply(merge(signal[, 2], weight.pair[, 2], return.pair[, 2], all = FALSE), 1, prod)))
  
  if(!length(dim(x))){
    xts(rep(NA, nrow(price.pair)), order.by = index(price.pair))
  }else{
    xts(rowSums(x), order.by = index(x))
  }
}

#used in generating returns
.return <- function(x, type = c("continuous", "discrete"), na.pad = TRUE) 
{
  type <- match.arg(type)
  if (type == "discrete") {
    result <- x/lag(x, na.pad = na.pad) - 1
  }else if (type == "continuous") {
    result <- diff(log(x), na.pad = na.pad)
  }
  return(result)
}

#generate the returns for pairs trading
TradeSimple <- function(stock.pairs,phi,tradingwindow){
  parame=EstimateParameter(stock.pairs, period = tradingwindow)
  entry=min(mean(parame$spread[!is.na(parame$spread)][1:tradingwindow])+phi*sqrt(var(parame$spread[!is.na(parame$spread)][1:tradingwindow])),max(parame$spread[tradingwindow:length(parame$spread)]))
  exit=min(mean(parame$spread[!is.na(parame$spread)][1:tradingwindow])-phi*sqrt(var(parame$spread[!is.na(parame$spread)][1:tradingwindow])),max(parame$spread[tradingwindow:length(parame$spread)]))
  sig=ststSimple(parame$spread,as.numeric(entry),as.numeric(exit))
  returns.pairtrading <- Return(stock.pairs, lag(sig), lag(parame$hedge.ratio))
  return=100*cumprod(1 + returns.pairtrading)
}

#Improved signal
WSimpleWithTakeProfit <- function(spread, spread.entry, spread.exit, spread.take.profit,tradingwindow)
{
  signal <- ifelse(spread[tradingwindow:length(spread)] >=  spread.entry, -1, 0)
  signal <- ifelse(spread[tradingwindow:length(spread)] <=  spread.exit,  1, signal)
  
  take.profit.upper <- 1/2*(spread.entry+spread.exit)+abs(spread.take.profit)
  take.profit.lower <- 1/2*(spread.entry+spread.exit)-abs(spread.take.profit)
  
  #Hit take.profit line : 0
  #other case : continue previous position
  for(i in tradingwindow:nrow(signal))
  {
    if(signal[i] == 0){
      if(signal[i - 1] == 1){
        if(as.numeric(spread[i]) >= take.profit.lower){
          signal[i] <- 0
        }else{
          signal[i] <- signal[i - 1]
        }
      }else if(signal[i - 1] == -1){
        if(as.numeric(spread[i]) <= take.profit.upper){
          signal[i] <- 0
        }else{
          signal[i] <- signal[i - 1]          
        }
      }
    }
  }
  return(signal)
}

#generate the returns for pairs trading
TradeSimpleProfit <- function(stock.pairs,phi,lamda,tradingwindow){
  parame=EstimateParameter(stock.pairs, period = tradingwindow)
  u=mean(parame$spread[!is.na(parame$spread)][1:tradingwindow])
  sigma=sqrt(var(parame$spread[!is.na(parame$spread)][1:tradingwindow]))
  entry=min(u+phi*sigma,max(parame$spread[tradingwindow:length(parame$spread)]))
  exit=min(u-phi*sigma,max(parame$spread[tradingwindow:length(parame$spread)]))
  profit=lamda*sigma
  sig=WSimpleWithTakeProfit(parame$spread,as.numeric(entry),as.numeric(exit),as.numeric(profit),tradingwindow)
  returns.pairtrading <- Return(stock.pairs, lag(sig), lag(parame$hedge.ratio))
  return=100*cumprod(1 + returns.pairtrading)
}

#Use iteration to find the Optimized parameter for trading with signal=profit
OptimizeParameterProfit <- function(stock.pairs,b,c){
  V=array(0,dim=c(b,c))
  for (j in 0:b){
    for (k in 0:c){
      tradingwindow=40
      phi=j/10
      lamda=k/10
      parame=EstimateParameter(stock.pairs, period = tradingwindow)
      u=mean(parame$spread[!is.na(parame$spread)][1:tradingwindow])
      sigma=sqrt(var(parame$spread[!is.na(parame$spread)][1:tradingwindow]))
      entry=min(u+phi*sigma,max(parame$spread[tradingwindow:length(parame$spread)]))
      exit=min(u-phi*sigma,max(parame$spread[tradingwindow:length(parame$spread)]))
      profit=lamda*sigma
      sig=WSimpleWithTakeProfit(parame$spread,as.numeric(entry),as.numeric(exit),as.numeric(profit),tradingwindow)
      returns.pairtrading <- Return(stock.pairs, lag(sig), lag(parame$hedge.ratio))
      return=cumprod(1 + returns.pairtrading)
      V[j,k]=return[as.numeric(length(return[,1])),1]
    }
  }
  return(V)
}

#generate the returns for pairs trading
TradeSimpleMax <- function(stock.pairs,phi,lamda,tradingwindow){
  parame=EstimateParameter(stock.pairs, period = tradingwindow)
  u=mean(parame$spread[!is.na(parame$spread)][1:tradingwindow])
  sigma=sqrt(var(parame$spread[!is.na(parame$spread)][1:tradingwindow]))
  entry=min(u+phi*sigma,max(parame$spread[tradingwindow:length(parame$spread)]))
  exit=min(u-phi*sigma,max(parame$spread[tradingwindow:length(parame$spread)]))
  upbound=u+lamda*sigma
  lowbound=u-lamda*sigma
  sig=withmaxSimple(parame$spread,as.numeric(entry),as.numeric(exit),as.numeric(upbound),as.numeric(lowbound))
  returns.pairtrading <- Return(stock.pairs, lag(sig), lag(parame$hedge.ratio))
  return=100*cumprod(1 + returns.pairtrading)
}

#Generate trading signal with entry and spread
withmaxSimple <- function(spread, entry, exit,upbound,lowbound)
{
  signal <- ifelse(spread >= upbound, 0, NA)
  signal <- ifelse(spread <= lowbound, 0, signal)
  signal <- ifelse(spread >= entry & spread <= upbound,  -1, signal)
  signal <- ifelse(spread <= exit & spread >= lowbound,  1, signal)
  #return(signal)
  return(na.locf(signal))
}

#Use iteration to find the Optimized parameter for trading
OptimizeParameterMax <- function(stock.pairs,b,c){
  V=array(0,dim=c(b,c))
  for (j in 0:b){
    for (k in j:c){
      tradingwindow=40
      phi=j/10
      lamda=k/10
      parame=EstimateParameter(stock.pairs, period = tradingwindow)
      u=mean(parame$spread[!is.na(parame$spread)][1:tradingwindow])
      sigma=sqrt(var(parame$spread[!is.na(parame$spread)][1:tradingwindow]))
      entry=min(u+phi*sigma,max(parame$spread[tradingwindow:length(parame$spread)]))
      exit=min(u-phi*sigma,max(parame$spread[tradingwindow:length(parame$spread)]))
      upbound=u+lamda*sigma
      lowbound=u-lamda*sigma
      sig=withmaxSimple(parame$spread,as.numeric(entry),as.numeric(exit),as.numeric(upbound),as.numeric(lowbound))
      returns.pairtrading <- Return(stock.pairs, lag(sig), lag(parame$hedge.ratio))
      return=cumprod(1 + returns.pairtrading)
      V[j,k]=return[as.numeric(length(return[,1])),1]
    }
  }
  return(V)
}

#combine stock
xtc <- function(XTS.obj, start_date = "2016-01-04"){
  dts <- index(XTS.obj)
  strt <- which(dts == start_date)
  if(length(strt) == 0){
    out <- 0
  } else {
    out <- XTS.obj[-c(1:(strt - 1))]
  }
  out
}

#get
GetStockPair <- function(all_stocks){
  stocks_trunc <- lapply(all_stocks, xtc)
  stock_data   <- as.matrix(Reduce("cbind", stocks_trunc))
  return(stock_data)
}

#Generating the return for the total period
ReturnThree <- function(price.pair, signal.lagged, hedge.ratio1.lagged,hedge.ratio2.lagged)
{
  #
  signal      <- as.xts(na.omit(cbind(signal.lagged, -1*(signal.lagged))))
  return.pair <- as.xts(na.omit(.return(price.pair, type = "discrete")))
  weight.pair <- as.xts(na.omit(HedgeRatio2WeightThree(hedge.ratio1.lagged,hedge.ratio2.lagged)))
  #
  #names(return.pair) <- names(price.pair)
  #names(signal)      <- names(price.pair)
  #names(weight.pair) <- names(price.pair) 
  #as.xts(apply(signal * weight.pair * return.pair, 1, sum) * leverage)
  x <-          as.xts(apply(merge(signal[, 1], weight.pair[, 1], return.pair[, 1], all = FALSE), 1, prod))
  x <- merge(x, as.xts(apply(merge(signal[, 2], weight.pair[, 2], return.pair[, 2], all = FALSE), 1, prod)))
  
  if(!length(dim(x))){
    xts(rep(NA, nrow(price.pair)), order.by = index(price.pair))
  }else{
    xts(rowSums(x), order.by = index(x))
  }
}

#Transfer the hedge ratio to the weight of the portfolio
HedgeRatio2WeightThree <- function(hedge.ratio1,hedge.ratio2)
{
  hedge.ratio1 <- abs(hedge.ratio1) * (-1)
  hedge.ratio2 <- abs(hedge.ratio2) * (-1)
  #
  normalization.factor <- 1 / (1 + abs(hedge.ratio1)+abs(hedge.ratio2))
  return(cbind(1 * normalization.factor, hedge.ratio1 * normalization.factor,hedge.ratio2*normalization.factor))
}

EstimateParameterThree <- function(price.pair, period, method = lm)
{
  Applied <- function(price.pair)
  {
    reg <- CalculateSpreadThree(price.pair, method)
    c(spread = as.numeric(last(reg$spread)), hedge.ratio = reg$hedge.ratio, premium = reg$premium)
  }
  as.xts(rollapplyr(price.pair, period, Applied, by.column = FALSE))
}

CalculateSpreadThree <- function(price.pair, method = lm)
{
  #  x <- log(price.pair)
  x <- price.pair
  reg <- method(as.numeric(x[,1]) ~ x[,2]+x[,3])
  hedge.ratio1 <- as.numeric(reg$coef[2])
  hedge.ratio2 <- as.numeric(reg$coef[3])
  premium     <- as.numeric(reg$coef[1])
  spread      <- x[,1] - (hedge.ratio1 * x[,2] +hedge.ratio2 * x[,3]+ premium)
  
  return(list(spread = spread, hedge.ratio =cbind(hedge.ratio1=hedge.ratio1,hedge.ratio2=hedge.ratio2), premium = premium))
}

###############
#data
###############

#load the data
BRK.A=read.table("BRK.A.csv",sep=',',header=T)
BRK.B=read.table("BRK.B.csv",sep=',',header=T)
BRKA=BRK.A$close
BRKB=BRK.B$close

#split into training and testing
brka1train=BRKA[1:126]
brka1test=BRKA[127:252]
brkb1train=BRKB[1:126]
brkb1test=BRKB[127:252]

#generate pairs
stock.train=xts(cbind(brka1train,brkb1train),order.by = as.Date(BRK.A$date[1:126]))
stock.test=xts(cbind(brka1test,brkb1test),order.by = as.Date(BRK.A$date[127:252]))
stock.brk=xts(cbind(BRKA,BRKB),order.by = as.Date(BRK.A$date))
stock.pairs=stock.brk
spr=CalculateSpread(stock.pairs, method = lm)
spr$hedge.ratio

#check for stationary
StationaryCheck(spr$spread,0.05)

#check for co integration
coi=CoIntegration(stock.pairs)
summary(coi)

###Just have a look on the whole set.

#Estimate parameter
phi=0.7
tradingwindow=40
parame=EstimateParameter(stock.pairs, period = tradingwindow)
plot(parame$spread)
entry=mean(parame$spread[!is.na(parame$spread)][1:tradingwindow])+phi*sqrt(var(parame$spread[!is.na(parame$spread)][1:tradingwindow]))
exit=mean(parame$spread[!is.na(parame$spread)][1:tradingwindow])-phi*sqrt(var(parame$spread[!is.na(parame$spread)][1:tradingwindow]))
sig=ststSimple(parame$spread,as.numeric(entry),as.numeric(exit))

#Performance of pair trading
returns.pairtrading <- Return(stock.pairs, lag(sig), lag(parame$hedge.ratio))
value=100 * cumprod(1 + returns.pairtrading)
plot(value)
value[length(value)]

#Check for the max drawdown as risk analysis
PlotMaxDrawDown(value)
maxdrawdown(value)$maxdrawdown

###############
#model 1
###############

#Take a lot of time to run
OptimizeBRK=OptimizeParameter(stock.pairs,6,10)#0.7,40
OptimizeTrain=OptimizeParameter(stock.train,6,10)#0.7,40
OptimizeTest=OptimizeParameter(stock.test,6,10)#0.5,40

#Use the training data to get the parameter
trainOpt=TradeSimple(stock.train,0.7,40)
plot(trainOpt)
trainOpt[length(trainOpt)]
maxdrawdown(trainOpt)$maxdrawdown
PlotMaxDrawDown(trainOpt)

#With the same parameter for test, performance on testing period
testPre=TradeSimple(stock.test,0.7,40)
plot(testPre)
testPre[length(testPre)]
maxdrawdown(testPre)$maxdrawdown
PlotMaxDrawDown(testPre)

#Compare with the optimized
testOpt=TradeSimple(stock.test,0.5,40)
plot(testOpt)
testOpt[length(testOpt)]
maxdrawdown(testOpt)$maxdrawdown
PlotMaxDrawDown(testOpt)

###############
#model 2
###############

TrainProfitOptimize=OptimizeParameterProfit(stock.train,10,10)
TestProfitOptimize=OptimizeParameterProfit(stock.test,10,10)

#New Risk management strategy
trainprofit=TradeSimpleProfit(stock.train,0.3,0.2,40)
trainprofit[length(trainprofit)]
maxdrawdown(trainprofit)$maxdrawdown
plot(trainprofit)
PlotMaxDrawDown(trainprofit)

#Result on the testing 
testprofit=TradeSimpleProfit(stock.test,0.3,0.2,40)
testprofit[length(testprofit)]
maxdrawdown(testprofit)$maxdrawdown
plot(testprofit)
PlotMaxDrawDown(testprofit)

#Result of the optimized
testprofitopt=TradeSimpleProfit(stock.test,0.5,0.2,40)
testprofitopt[length(testprofitopt)]
maxdrawdown(testprofitopt)$maxdrawdown
plot(testprofitopt)
PlotMaxDrawDown(testprofitopt)

###############
#model 3
###############

#Optimize the parameter
TrainMaxOpt=OptimizeParameterMax(stock.train,10,10)
TestMaxOpt=OptimizeParameterMax(stock.test,10,10)

#Train set performance
trainmax=TradeSimpleMax(stock.train,0.7,1.0,40)
trainmax[length(trainmax)]
maxdrawdown(trainmax)$maxdrawdown
plot(trainmax)
PlotMaxDrawDown(trainmax)

#Test set performance
testmax=TradeSimpleMax(stock.test,0.7,1.0,40)
testmax[length(testmax)]
maxdrawdown(testmax)$maxdrawdown
plot(testmax)
PlotMaxDrawDown(testmax)

###############
#Risk part
###############

#risk analysis in 2008
BRK.A_08=read.table("BRK.A_08.csv",sep=',',header=T)
BRK.B_08=read.table("BRK.B_08.csv",sep=',',header=T)
BRKA_08=BRK.A_08$close
BRKB_08=BRK.B_08$close

brka1train_08=BRKA_08[1:126]
brka1test_08=BRKA_08[127:253]
brkb1train_08=BRKB_08[1:126]
brkb1test_08=BRKB_08[127:253]

stock.train_08=xts(cbind(brka1train_08,brkb1train_08),order.by = as.Date(BRK.A_08$date[1:126]))
stock.test_08=xts(cbind(brka1test_08,brkb1test_08),order.by = as.Date(BRK.A_08$date[127:253]))
stock.brk_08=xts(cbind(BRKA_08,BRKB_08),order.by = as.Date(BRK.A_08$date))
stock.pairs_08=stock.brk_08

spr_08=CalculateSpread(stock.pairs, method = lm)
spr_08$hedge.ratio

#check for stationary
StationaryCheck(spr_08$spread,0.05)

#check for co integration
coi_08=CoIntegration(stock.pairs_08)
summary(coi_08)

#Optimize fOr model 1
OptimizeTrain_08=OptimizeParameter(stock.train_08,6,10)#0.9,40
OptimizeTest_08=OptimizeParameter(stock.test_08,6,10)#1.0,40

#Train
trainOpt_08=TradeSimple(stock.train_08,0.9,40)
plot(trainOpt_08)
trainOpt_08[length(trainOpt_08)]
maxdrawdown(trainOpt_08)$maxdrawdown
PlotMaxDrawDown(trainOpt_08)

#Test
testPre_08=TradeSimple(stock.test_08,0.9,40)
plot(testPre_08)
testPre_08[length(testPre_08)]
maxdrawdown(testPre_08)$maxdrawdown
PlotMaxDrawDown(testPre_08)

#Optimize fOr model 2
TrainProfitOptimize_08=OptimizeParameterProfit(stock.train_08,10,10)#1.0,0.3
#TestProfitOptimize_08=OptimizeParameterProfit(stock.test_08,10,10)

#Train
trainprofit_08=TradeSimpleProfit(stock.train_08,1.0,0.3,40)
trainprofit_08[length(trainprofit_08)]
maxdrawdown(trainprofit)$maxdrawdown
plot(trainprofit_08)
PlotMaxDrawDown(trainprofit_08)

#Test
testprofit_08=TradeSimpleProfit(stock.test_08,1.0,0.2,40)
testprofit_08[length(testprofit_08)]
maxdrawdown(testprofit_08)$maxdrawdown
plot(testprofit_08)
PlotMaxDrawDown(testprofit_08)

#Optimize fOr model 3
TrainMaxOpt_08=OptimizeParameterMax(stock.train_08,10,10)#0.5,0.6,40

#Train
trainmax_08=TradeSimpleMax(stock.train_08,0.5,0.6,40)
trainmax_08[length(trainmax_08)]
maxdrawdown(trainmax_08)$maxdrawdown
plot(trainmax_08)
PlotMaxDrawDown(trainmax_08)

#Test
testmax_08=TradeSimpleMax(stock.test_08,0.5,0.6,40)
testmax_08[length(testmax_08)]
maxdrawdown(testmax_08)$maxdrawdown
plot(testmax_08)
PlotMaxDrawDown(testmax_08)

###############
#Three stock part
###############

library(quantmod)

#Generate the three pairs.
getSymbols("SPY",from = '2019-01-02',to = "2019-12-31",warnings = FALSE,auto.assign = TRUE)  
getSymbols("IVV",from = '2019-01-02',to = "2019-12-31",warnings = FALSE,auto.assign = TRUE) 
getSymbols("VOO",from = '2019-01-02',to = "2019-12-31",warnings = FALSE,auto.assign = TRUE) 
stock.three=as.xts(cbind(unclass(SPY$SPY.Adjusted), unclass(IVV$IVV.Adjusted), unclass(VOO$VOO.Adjusted)),order.by=index(VOO))

jotest=ca.jo(stock.three, type="trace", K=2, ecdet="none", spec="longrun")
summary(jotest)

spreadd=CalculateSpreadThree(stock.three)

phi=0.85
window=50
paramee=EstimateParameterThree(stock.three,period = window)
mu=as.numeric(mean(paramee$spread[!is.na(paramee$spread)][1:window]))
sigma=as.numeric(sqrt(var(paramee$spread[!is.na(paramee$spread)][1:window])))
sigg=ststSimple(paramee$spread,mu+phi*sigma,mu-phi*sigma)
returnss.pairtrading <- ReturnThree(stock.three, lag(sigg), lag(paramee$hedge.ratio1),lag(paramee$hedge.ratio2))
returns=100*cumprod(1 + returnss.pairtrading)
returns[length(returns)]
plot(returns)
PlotMaxDrawDown(returns)
maxdrawdown(returns)$maxdrawdown