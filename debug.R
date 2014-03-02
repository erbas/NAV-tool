# 
path1 <- "CRNCY_Trade File_Model/Sub Strategy/"
path2 <- "CMDTY_Trade File_Model/Sub Strategy/Gold"
path3 <- "Revaluation rates/"
path1 <- "E:/Cloud Data/Published Returns/Global Currency Program/CRNCY_31 Dec 2013/CRNCY_Trade File_Model"
path2 <- "E:/Cloud Data/Published Returns/Global Commodity Program/Dec 31_2013/CMDTY_Trade File_Model/"
path3 <- "E:/Cloud Data/Data History/Revaluation rates/"
files.to.load <- c(list.files(path1,pattern="*.csv",full.names=TRUE,recursive=TRUE), list.files(path2,pattern="*.csv",full.names=TRUE,recursive=TRUE))

trade.data <- load.all.trades(files.to.load)
reval <- load.reval.files(path3,c("2010-01-01","2013-12-31"))
trades.usd <- make.trades.USD(trade.data, reval)
extended.trades.usd <- split.trades.at.month.ends(trades.usd, reval)
extended.trades.pnl <- calc.pnl(extended.trades.usd, reval)

# trade.data <- read.saved.trades()
# reval <- read.saved.reval()
# trades.usd <- read.saved.usd()
# extended.trades.pnl <- read.saved.extended.pnl()



all.ccys <- "XAUUSD"

gold.idx <- grep("XAUUSD",extended.trades.pnl$"Ccy pair",value=FALSE,fixed=TRUE)
gold <- extended.trades.pnl[gold.idx,]

april.12 <- gold[ gold$"Exit time" < ymd("2012-05-01",tz="Europe/London"),]
gold.raw <- trade.data[ trade.data$"Ccy pair" == "XAUUSD",]
gold.raw.12.04 <- gold.raw[gold.raw$"Entry time" < ymd("2012-05-01",tz="Europe/London"),]

gold.usd <- trades.usd[ trades.usd$"Ccy pair" == "XAUUSD",]
gold.usd.12.04 <- gold.usd[gold.usd$"Entry time" <= ymd_hm("2012-04-30 17:00",tz="Europe/London"),]

gold.split <- extended.trades.pnl[extended.trades.pnl$"Ccy pair" == "XAUUSD", ]
gold.split.12.04 <- gold.split[gold.split$"Entry time" <= ymd_hm("2012-04-30 17:00",tz="Europe/London"),]
gold.split.12.05 <- gold.split[gold.split$"Entry time" <= ymd_hm("2012-05-31 17:00",tz="Europe/London") & gold.split$"Entry time" > ymd_hm("2012-04-30 17:00",tz="Europe/London"),]

gold.xts <- xts( (gold.usd$"Exit price" - gold.usd$"Entry price")*gold.usd$"Amount major"*gold.usd$Sign, gold.usd$"Exit time")

gold.dates <- gold.xts
index(gold.dates) <- as.Date(index(gold.dates))
gold.daily <- apply.daily(gold.dates,sum)/1.e8

#####
all.ccys <- c("AUDCAD", "AUDJPY", "AUDUSD", "EURAUD", "EURCAD", "EURJPY", "EURUSD", "GBPAUD", "GBPJPY", "GBPUSD", "NZDUSD", "USDCAD", "USDCHF", "USDJPY", "USDSGD", "XAUUSD")

rtns <- calc.returns(extended.trades.pnl,ccy.pairs=all.ccys)
colnames(rtns) <- "PnL.USD"

eom <- get.ends.of.months(reval)
rtns.monthly <- my.apply.monthly(rtns,eom.datetimes=index(eom),FUN=sum)

rtns.daily.prg <- read.zoo("Extended Markets BSc_pnl_daily.csv",header=TRUE,sep=",")
rtns.monthly.prg <- read.zoo("Extended Markets BSc_pnl_monthly.csv",header=TRUE,sep=",")

rtns.xts <- xts( (trades.usd$"Exit price" - trades.usd$"Entry price")*trades.usd$"Amount major"*trades.usd$Sign, trades.usd$"Exit time")


rtns.net <- calc.net.rtns(rtns.monthly,mgt.fee=0.02,perf.fee=0.2,aum=1.e8)
rtns.net.0 <- calc.net.rtns(rtns.monthly,mgt.fee=0.0,perf.fee=0.0,aum=1.e8)
rtns.net.mgt.only <- calc.net.rtns(rtns.monthly,mgt.fee=0.02,perf.fee=0.0,aum=1.e8)
a <- cbind(rtns.net.0, rtns.net.mgt.only, rtns.net, rtns.net.0 - rtns.net.mgt.only)
colnames(a) <- c("Nav No Fees", "Nav Mgt Fee 2%", "Nav 2&20", "Nav0 - Nav.mgt")

stats <- calc.stats(rtns.net/1.e8)

op <- calc.open.pos(extended.trades.pnl,c("2010-01-01","2013-12-31"))

index(op$Total) <- as.yearmon(index(op$Total))
barplot(op$Total[,"Amount USD"]/1.e6)
chart.TimeSeries(cumsum(op$Total)/1.e6,type="h",date.format="%b-%Y",main="Value of all open positions (mUSD)",xlab="",ylab="")


s <- read.csv("summary trade files.csv")

f.csv <- read.csv("CRNCY_Trade File_Model//Sub Strategy/CIT/CIT SS2_553 Ratio Sell 25bp_Rats 12 5bp/BT4 USDCAD 1440 Sells_01012010 31122013 3x.csv",stringsAsFactors=FALSE)
dt1 <- dmy_hms(f.csv$Entry.time,tz="Europe/London")
dt2 <- dmy_hms(f.csv$Exit.time,tz="Europe/London")



#### find end of month revals ###
idx <- lapply(month.ends, function(x) which(index(reval) <= x))
eom.revals <- Reduce(rbind, lapply(idx, function(x) reval[last(x),]))



####################
### open pos #######
####################
trades.extended.pnl <- extended.trades.pnl
daterange <- c("2010-01-01","2013-12-31")
# NOTE: we use the split trades dataframe and assume no trade goes over month end
print("---> inside calc.open.pos")
print(str(extended.trades.pnl))
idx.range <- which(as.Date(trades.extended.pnl$"Exit time") < as.Date(daterange)[1] | 
                     as.Date(trades.extended.pnl$"Exit time") > as.Date(daterange)[2])
df.range <- trades.extended.pnl[-idx.range,]
# find trades which are open at month end, and trades which close during the month
idx.open <- which( grepl("CarryOver",df.range$"Exit name"))
df <- df.range[idx.open,]
# split opens from closes
open.pos <- data.frame(df$"Ccy pair", df$"TradeId", df$"SplitId", df$"Amount major"*df$Sign, df$"Amount USD"*df$Sign, df$"Exit time")
colnames(open.pos) <- c("CcyPair","Trade ID", "Split ID", "Amount major","Amount USD","Date Open")
# split by ccypair
open.list <- split(open.pos,open.pos$CcyPair)
# turn into xts and sum by month
open.list.xts <- lapply(open.list, function(x) xts(x[,2:5],x[,6]))
open.list.xts.m <- lapply(open.list.xts, function(x) apply.monthly(x,colSums))
# calculate total
total <- apply.monthly(Reduce(rbind,open.list.xts),colSums)



barplot(cumsum(total[,2])/1.e6)

barplot(cumsum(open.pos[["USDJPY"]][,1])/1.e6)
lines(reval[,"AUDUSD"])



# -----------------------------------------------------------------------------------
# pnl attribution by currency pair
# -----------------------------------------------------------------------------------
pnl.monthly <- apply.monthly(xts(extended.trades.pnl$"PnL USD",ymd_hms(extended.trades.pnl$"Exit time")),sum)
pnl.list <- vector("list")
ccy.pairs <- unique(extended.trades.pnl$"Ccy pair")

pnl.by.ccy <- function(x) {
  idx <- which(extended.trades.pnl$"Ccy pair" == x)
  trades <- extended.trades.pnl[idx, ]
  trades.pnl <- xts(trades$"PnL USD",ymd_hms(trades$"Exit time"))
  pnl <- apply.monthly(trades.pnl,sum)
  index(pnl) <- as.yearmon(index(pnl))
  colnames(pnl) <- x
  return(pnl)
}


pnl.ccy <- lapply(ccy.pairs, pnl.by.ccy)

x <- do.call(merge,pnl.ccy)
x[is.na(x)] <- 0

library(PerformanceAnalytics)
y <- t(table.AnnualizedReturns(x,scale=12,geometric=TRUE))


# -----------------------------------------------------------------------------------
#  fees calculation
# -----------------------------------------------------------------------------------
zero.xts <- xts(rep(0,length(rtns.monthly)),index(rtns.monthly))
start.eq <- zero.xts
pnl <- rtns.monthly
names(pnl) <- "pnl"
mgt.fee <- zero.xts
perf.fee <- zero.xts
end.eq <- zero.xts
high.water.mark <- zero.xts
# initialise calc
start.eq[1] <- 1.e8
mgt.fee[1] <- 0.02/12*start.eq[1]
perf.fee[1] <- 0.2*(pnl[1] - mgt.fee[1])
end.eq[1] <- start.eq[1] + pnl[1] - mgt.fee[1] - perf.fee[1] 
high.water.mark[1] <- end.eq[1]
# loop
for (i in 2:length(zero.xts)) {
  start.eq[i] <- end.eq[i-1]
  mgt.fee[i] <- 0.02/12*start.eq[i]
  high.water.mark[i] <- max(high.water.mark[1:i])
  if ( coredata(start.eq[i] + pnl[i] - mgt.fee[i]) > high.water.mark[i-1]  ) {
    perf.fee[i] <- 0.2*( start.eq[i] + pnl[i] - mgt.fee[i] - high.water.mark[i] )
    end.eq[i] <- start.eq[i] + pnl[i] - mgt.fee[i] - perf.fee[i]
    high.water.mark[i] <- coredata(end.eq[i])
  } else {
    perf.fee[i] <- 0.0
    end.eq[i] <- start.eq[i] + pnl[i] - mgt.fee[i] 
  }
}

res <- merge(start.eq, pnl, mgt.fee, perf.fee, end.eq, high.water.mark)
head(res,13)



# -----------------------------------------------------------------------------------

rtns.prg.m <- read.zoo("Extended Markets BSc_pnl_monthly.csv",format="%Y-%m-%d",sep=",",header=TRUE)

rtns.prg.d <- read.zoo("Extended Markets BSc_pnl_daily.csv",header=TRUE,sep=",")
index(rtns.prg.d) <- as.POSIXct(index(rtns.prg.d),tz="Europe/London",origin="1970-01-01 00:00")
rtns.prg.my.m <- my.apply.monthly(rtns=rtns.prg.d[,1],eom.datetimes=index(eom),FUN=sum)
index(rtns.prg.my.m) <- as.Date(index(rtns.prg.my.m))

rtns.monthly.pct <- rtns.monthly/1.e8
index(rtns.monthly.pct) <- as.Date(index(rtns.monthly.pct))

rtns.prg <- merge(rtns.prg.my.m, rtns.prg.m[,1], rtns.monthly.pct, all=TRUE)
colnames(rtns.prg) <- c("my.monthly.prg","monthly.prg","my.monthly")


# -----------------------------------------------------------------------------------
#  debug my.apply.monthly
# -----------------------------------------------------------------------------------

dt <- ymd_hm(c("2012-05-01 11:00","2012-05-31 17:00","2012-05-31 17:05","2012-06-29 16:38","2012-06-29 17:02","2012-07-11 11:00"),tz="Europe/London")
x <- xts(rep(1,6), dt)
my.apply.monthly(x,eom.datetimes=index(eom),FUN=sum)
x
