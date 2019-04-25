#Final Project
#Portfolio Optimization in R
#Donghao (Leonardo) Guo, Will Noone



#library for portfolio optimization
library(fPortfolio)
#library for data collection 
library(quantmod)
library(fImport)




##Data Collection and Preprocessing

#CREATE FUNCTION TO SCRAP YAHOO FINANCE FOR PRICE & VOLUME DATA
yahooDownload <- function(name, units=name, from=Sys.Date()-366*5, to=Sys.Date()) 
{
  #COMPOSED DOWNLOAD URL:
  fromPosix <- as.POSIXlt(from)
  toPosix <- as.POSIXlt(to)
  URL <- composeURL(
    "chart.yahoo.com/table.csv?",
    "a=", fromPosix$mon,
    "&b=", fromPosix$mday,
    "&c=", fromPosix$year + 1900,
    "&d=", toPosix$mon,
    "&e=", toPosix$mday,
    "&f=", toPosix$year + 1900,
    "&g=d&q=q&y=0&s=", name,
    "&x=.csv")
  
  # Download the Data:
  download <- read.csv(URL)
  
  # Convert to timeSeries:
  data <- as.matrix(download[NROW(download):1, -1])
  charvec <- rev(format(strptime(download[, 1], format = "%F")))
  units <- paste(units, c("O", "H", "L", "C", "V", "A"), sep = ".")
  tS <- timeSeries(data, charvec, units)
  
  # Return Value:
  tS
}

#CHECK SYSTEM DATE
Sys.Date()

#DOWNLOAD SPECIFIED TICKER PRICE/VOLUME DATA
#INDEX CLOSING PRICE & CONVERT TO MONTHLY RETURNS
#NAME TS OBJECT as TICKER
AAPL <- monthlyReturn(yahooDownload("AAPL", units="AAPL", from=Sys.Date()-5*366)[ , 4])
names(AAPL) <- c("AAPL")
MSFT <- monthlyReturn(yahooDownload("MSFT", units="MSFT", from=Sys.Date()-5*366)[ , 4])
names(MSFT) <- c("MSFT")
GOOG <- monthlyReturn(yahooDownload("GOOG", units="GOOG", from=Sys.Date()-5*366)[ , 4])
names(GOOG) <- c("GOOG")
BP <- monthlyReturn(yahooDownload("BP", units="BP", from=Sys.Date()-5*366)[ , 4])
names(BP) <- c("BP")
PFE <- monthlyReturn(yahooDownload("PFE", units="PFE", from=Sys.Date()-5*366)[ , 4])
names(PFE) <- c("PFE")
JPM <- monthlyReturn(yahooDownload("JPM", units="JPM", from=Sys.Date()-5*366)[ , 4])
names(JPM) <- c("JPM")
HPQ <- monthlyReturn(yahooDownload("HPQ", units="HPQ", from=Sys.Date()-5*366)[ , 4])
names(HPQ) <- c("HPQ")
IBM <- monthlyReturn(yahooDownload("IBM", units="IBM", from=Sys.Date()-5*366)[ , 4])
names(IBM) <- c("IBM")
ORCL <- monthlyReturn(yahooDownload("ORCL", units="ORCL", from=Sys.Date()-5*366)[ , 4])
names(ORCL) <- c("ORCL")
GLD <- monthlyReturn(yahooDownload("GLD", units="GLD", from=Sys.Date()-5*366)[ , 4])
names(GLD) <- c("GLD")
SLV <- monthlyReturn(yahooDownload("SLV", units="SLV", from=Sys.Date()-5*366)[ , 4])
names(SLV) <- c("SLV")
USO <- monthlyReturn(yahooDownload("USO", units="USO", from=Sys.Date()-5*366)[ , 4])
names(USO) <- c("USO")

head(AAPL)

#combine stocks into different portfolios 
portfolio1 <- cbind(AAPL,MSFT,GOOG)
portfolio2 <- cbind(AAPL,MSFT,GOOG,BP,PFE,JPM)
portfolio3 <- cbind(AAPL,MSFT,GOOG,BP,PFE,JPM,HPQ,IBM,ORCL)
portfolio4 <- cbind(AAPL,MSFT,GOOG,BP,PFE,JPM,GLD,SLV,USO)

#remove data not in the periods to be analyzed 
portfolio1 <- portfolio1[c(-1,-61),]
portfolio2 <- portfolio2[c(-1,-61),]
portfolio3 <- portfolio3[c(-1,-61),]
portfolio4 <- portfolio4[c(-1,-61),]





##Data Exploratory Analysis

#get the means, variances and correlations in protfolio A
dat = portfolio3
dat = as.timeSeries(dat)
colMeans(dat)
colVars(dat)
library(corrplot)
corrplot(cor(dat))


#boxplot and density plot for stocks in portfolio A
library(ggplot2)
library(reshape)
pd = data.frame(dat)
ggplot(melt(pd),aes(x=variable,y=value))+geom_boxplot()
ggplot(melt(pd),aes(x=value))+geom_density()+ facet_wrap(~variable,scales="free")




##Portfolio A Optimization in Four Methods 


#Feasible Portfolio: given the weights
ewSpec <- portfolioSpec()
nAssets <- ncol(dat)
setWeights(ewSpec) <- rep(1/nAssets, times = nAssets)

ewPortfolio <- feasiblePortfolio(
  data = dat,
  spec = ewSpec,
  constraints = "LongOnly")
print(ewPortfolio)

#pie plots for weights, weighted return and covariance risk
col = divPalette(ncol(dat), "RdBu")
weightsPie(ewPortfolio, radius = 0.7, col = col)
mtext(text = "Equally Weighted MV Portfolio", side = 3, line = 1.5,
      font = 2, cex = 0.7, adj = 0)
weightedReturnsPie(ewPortfolio, radius = 0.7, col = col)
mtext(text = "Equally Weighted MV Portfolio", side = 3, line = 1.5,
      font = 2, cex = 0.7, adj = 0)
covRiskBudgetsPie(ewPortfolio, radius = 0.7, col = col)
mtext(text = "Equally Weighted MV Portfolio", side = 3, line = 1.5,
      font = 2, cex = 0.7, adj = 0)



#Efficient Portfolio: minimum risk portfolio for given return
minriskSpec <- portfolioSpec()
targetReturn <- getTargetReturn(ewPortfolio@portfolio)["mean"]
setTargetReturn(minriskSpec) <- targetReturn
minriskPortfolio <- efficientPortfolio(
  data = dat,
  spec = minriskSpec,
  constraints = "LongOnly")
print(minriskPortfolio)

col = qualiPalette(ncol(dat), "Dark2")
weightsPie(minriskPortfolio, radius = 0.7, col = col)
mtext(text = "Minimal Risk MV Portfolio", side = 3, line = 1.5,
      font = 2, cex = 0.7, adj = 0)
weightedReturnsPie(minriskPortfolio, radius = 0.7, col = col)
mtext(text = "Minimal Risk MV Portfolio", side = 3, line = 1.5,
      font = 2, cex = 0.7, adj = 0)
covRiskBudgetsPie(minriskPortfolio, radius = 0.7, col = col)
mtext(text = "Minimal Risk MV Portfolio", side = 3, line = 1.5,
      font = 2, cex = 0.7, adj = 0)



#Global Minimum Risk Portfolio
globminSpec <- portfolioSpec()
globminPortfolio <- minvariancePortfolio(
  data = dat,
  spec = globminSpec,
  constraints = "LongOnly")
print(globminPortfolio)

col <- seqPalette(ncol(dat), "YlGn")
weightsPie(globminPortfolio, col = col)
mtext(text = "Global Minimum Variance MV Portfolio", side = 3,
      line = 1.5, font = 2, cex = 0.7, adj = 0)
weightedReturnsPie(globminPortfolio, col = col)
mtext(text = "Global Minimum Variance MV Portfolio", side = 3,
      line = 1.5, font = 2, cex = 0.7, adj = 0)
covRiskBudgetsPie(globminPortfolio, col = col)
mtext(text = "Global Minimum Variance MV Portfolio", side = 3,
      line = 1.5, font = 2, cex = 0.7, adj = 0)



#Tangency Portfolio: minimizing the Sharpe Ratio for agiven risk-free rate. 
#The Sharpe ratio is the ratio of the target return lowered by the risk-free rate and the covariance risk
tgSpec <- portfolioSpec()
setRiskFreeRate(tgSpec) <- 0
tgPortfolio <- tangencyPortfolio(
  data = dat,
  spec = tgSpec,
  constraints = "LongOnly")
print(tgPortfolio)

col <- seqPalette(ncol(dat), "BuPu")
weightsPie(tgPortfolio, col = col)
mtext(text = "Tangency MV Portfolio", side = 3, line = 1.5,
      font = 2, cex = 0.7, adj = 0)
weightedReturnsPie(tgPortfolio, col = col)
mtext(text = "Tangency MV Portfolio", side = 3, line = 1.5,
      font = 2, cex = 0.7, adj = 0)
covRiskBudgetsPie(tgPortfolio, col = col)
mtext(text = "Tangency MV Portfolio", side = 3, line = 1.5,
      font = 2, cex = 0.7, adj = 0)




##Diversafication Effect of Portfolio A, B and C on Efficient Frontiers  

dat2 <- portfolio2
dat2 = as.timeSeries(dat2)
dat3 <- portfolio1
dat3 = as.timeSeries(dat3)
dat4 <- portfolio4
dat4 = as.timeSeries(dat4)

#frontier for portfolio A
lppSpec <- portfolioSpec()
setNFrontierPoints(lppSpec) <- 50
fr1 <- portfolioFrontier(dat, lppSpec)
print(fr1)
plot(fr1)
tailoredFrontierPlot(object = fr1, mText = "MV Portfolio - LongOnly Constraints",risk = "Cov",sharpeRatio = FALSE)

#frontier for portfolio B
lppSpec <- portfolioSpec()
setNFrontierPoints(lppSpec) <- 50
fr2 <- portfolioFrontier(dat2, lppSpec)
tailoredFrontierPlot(object = fr2, risk = "Cov",sharpeRatio = FALSE)

#frontier for portfolio C
lppSpec <- portfolioSpec()
setNFrontierPoints(lppSpec) <- 50
fr3 <- portfolioFrontier(dat3, lppSpec)
tailoredFrontierPlot(object = fr3, risk = "Cov",sharpeRatio = FALSE,ylim=c(-0.01,0.02))

#frontier for portfolio D
lppSpec <- portfolioSpec()
setNFrontierPoints(lppSpec) <- 50
fr4<- portfolioFrontier(dat4, lppSpec)
tailoredFrontierPlot(object = fr4, risk = "Cov",sharpeRatio = FALSE)


#plot all frontiers in one plot
tailoredFrontierPlot(object = fr3, risk = "Cov",sharpeRatio = FALSE,ylim=c(-0.01,0.02))
tailoredFrontierPlot(object=fr2,add=TRUE,risk = "Cov",sharpeRatio = FALSE)
tailoredFrontierPlot(object=fr1,add=TRUE,risk = "Cov",sharpeRatio = FALSE)
tailoredFrontierPlot(object=fr4,add=TRUE,risk = "Cov",sharpeRatio = FALSE)


#weights plot (efficient frontier in a more numerical version) for porfolio A 
weightsPlot(fr1, mtext = FALSE)
text <- "Mean-Variance Portfolio - Long Only Constraints"
mtext(text, side = 3, line = 3, font = 2, cex = 0.9)
weightedReturnsPlot(fr1, mtext = FALSE)
covRiskBudgetsPlot(fr1, mtext = FALSE)
