# 
path1 <- "CRNCY_Trade File_Model/"
path2 <- "CMDTY_Trade File_Model/"
path3 <- "Revaluation rates/"
# path1 <- "E:/Cloud Data/Published Returns/Global Currency Program/CRNCY_31 Dec 2013/CRNCY_Trade File_Model"
# path2 <- "E:/Cloud Data/Published Returns/Global Commodity Program/Dec 31_2013/CMDTY_Trade File_Model/"
# path3 <- "E:/Cloud Data/Data History/Revaluation rates/"
files.to.load <- c(list.files(path1,pattern="*.csv",full.names=TRUE,recursive=TRUE), list.files(path2,pattern="*.csv",full.names=TRUE,recursive=TRUE))

trade.data <- load.all.trades(files.to.load)
reval <- load.reval.files(path3,c("2010-01-01","2013-12-31"))

trade.data <- read.saved.trades()
reval <- read.saved.reval()
trades.usd <- read.saved.usd()
extended.trades.pnl <- read.saved.extended.pnl()

# trades.usd <- make.trades.USD(trade.data, reval)
# extended.trades.usd <- split.trades.at.month.ends(trades.usd, reval)
# extended.trades.pnl <- calc.pnl(extended.trades.usd, reval)

rtns <- calc.returns(extended.trades.pnl,c("2010-01-01","2013-12-31"),ccy.pair="all")
rtns.monthly <- apply.monthly(rtns,sum)
index(rtns.monthly) <- as.yearmon(index(rtns.monthly))

rtns.net <- calc.net.rtns(rtns.monthly,mgt.fee=0.02,perf.fee=0.2,aum=1.e8)
rtns.net.0 <- calc.net.rtns(rtns.monthly,mgt.fee=0.0,perf.fee=0.0,aum=1.e8)
rtns.net.mgt.only <- calc.net.rtns(rtns.monthly,mgt.fee=0.02,perf.fee=0.0,aum=1.e8)
a <- cbind(rtns.net.0, rtns.net.mgt.only, rtns.net, rtns.net.0 - rtns.net.mgt.only)
colnames(a) <- c("Nav No Fees", "Nav Mgt Fee 2%", "Nav 2&20", "Nav0 - Nav.mgt")

# -----------------------------------------------------------------------------------
# define performance array
p <- xts(matrix(0,ncol=6,nrow=length(rtns.monthly)),index(rtns.monthly))
colnames(p) <- c('Start','PnL', 'Mgt', 'Perf','Net PnL','End')
# initialise first row
p[1,'Start'] <- 1.e8
p[,'PnL'] <- rtns.monthly
p[1,'Mgt'] <- 0.02/12*p[1,'Start']
p[1,'Perf'] <- 0.2*(p[1,'PnL'] - p[1,'Mgt'])
p[1,'Net PnL'] <- p[1,'PnL'] - p[1,'Mgt'] - p[1,'Perf']
p[1,'End'] <- p[1,'Start'] + p[1,'Net PnL']
p[2,'Start'] <- p[1,'End']
# loop over subsequent months
for (i in 2:nrow(p)) {
  p[i,'Mgt'] <- 0.02/12*p[i,'Start']
  p[i,'Perf'] <- 0 
#   cat(i, p[i,'Nav net Mgt'] , max(p[1:i,'Start']), p[i,'Nav net Mgt'] >= max(p[1:i,'Start']),'\n')
  if (p[i,'Start'] + p[i,'PnL'] - p[i,'Mgt'] > max(p[1:i,'Start']))
    p[i,'Perf'] <- 0.2*(p[i,'PnL'] - p[i,'Mgt']) 
  p[i,'Net PnL'] <- p[i,'PnL'] - p[i,'Mgt'] - p[i,'Perf']
  p[i,'End'] <- p[i,'Start'] + p[i,'Net PnL'] 
  if ( i+1 <= nrow(p)) p[i+1,'Start'] <- p[i,'End'] 
}
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
  if ( coredata(start.eq[i] + pnl[i] - mgt.fee[i]) > high.water.mark[i]  ) {
    perf.fee[i] <- 0.2*( start.eq[i] + pnl[i] - mgt.fee[i] - high.water.mark[i] )
    end.eq[i] <- start.eq[i] + pnl[i] - mgt.fee[i] - perf.fee[i]
    high.water.mark[i] <- coredata(end.eq[i])
  } else {
    perf.fee[i] <- 0.0
    end.eq[i] <- start.eq[i] + pnl[i] - mgt.fee[i] - perf.fee[i]
  }
}

res <- merge(start.eq, pnl, mgt.fee, perf.fee, end.eq, high.water.mark)
head(res)



# -----------------------------------------------------------------------------------

x <- cbind(rtns.monthly, nav.0, nav.mgt.only, nav.0 - nav.mgt.only)/1.e6
colnames(x) <- c("monthly returns", "Nav No Fees", "Nav Mgt Fee 2%", "Diff in Nav")

plot.xts(pnl.m,main="Growth of $1")

chart.TimeSeries(pnl.m,date.format="%b-%Y",main="Growth of $1",xlab="",ylab="")
plot.xts(100+cumsum(rtns.sum.m)/1.e6,main="NAV (m USD)")

rtns.net <- calc.net.rtns(rtns.sum.m,mgt.fee=0.02,perf.fee=0.2)
both.rtns <- merge(rtns.sum.m,rtns.net,fill=0)
colnames(both.rtns) <- c("Returns","Net Returns")
barplot(both.rtns/1.e6,beside=TRUE,col=c(3,4),main="Returns and Net Returns",legend.text=c("Returns","Net Returns"), args.legend = list(x = "topleft"))
plot.zoo(cumsum(merge(rtns.sum.m,rtns.net,fill=0))/1.e6,plot.type='single',col=c(3,4))
chart.TimeSeries(cumsum(both.rtns)/1.e6,ylab="",xlab="",main="Returns and Net Returns (mUSD)",date.format="%b-%Y",colorset=c(3,4),legend.loc="topleft")

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

# open positions
idx.open <- which(extended.trades.pnl$"Exit name" == "CarryOver")
df <- extended.trades.pnl[idx.open,]
open.pos <- data.frame(df$"Ccy pair", df$"Amount major"*df$Sign, df$"Amount USD"*df$Sign, df$"Exit time")
colnames(open.pos) <- c("Ccy pair","Amount major","Amount USD","Date")
open.pos$Date <- as.yearmon(open.pos$Date)  
d1 <- aggregate(open.pos[,c("Amount major","Amount USD")],
                list(Date = open.pos$Date, CcyPair =  open.pos$"Ccy pair"),
                sum)
d2 <- split(d1, d1$CcyPair)
d3 <- lapply(d2,function(x) xts(x[,3:4],as.yearmon(x$Date)))
total <- aggregate(open.pos[,c("Amount major","Amount USD")],
                   list(Date = open.pos$Date),
                   sum)
total.xts <- xts(total[,2:3],as.yearmon(total[,1]))
barplot(total.xts$"Amount USD"/1.e6)

barplot(d3$XAUUSD[,"Amount major"]/1.e6,axis.lty=1)

# debug pnl in USD
ccy.pairs <- colnames(reval)
for (x in ccy.pairs) {
  print(x)
  idx <- which(extended.trades.pnl$"Ccy pair" == x)
  if (length(idx)==0) next
  pnl <- xts(extended.trades.pnl[idx,"PnL minor"],extended.trades.pnl$"Exit time"[idx])
  par(ask=TRUE)
  plot.xts(cumsum(pnl),main=x)
}


# debug open positions in USD
ccy.pairs <- colnames(reval)
for (x in ccy.pairs) {
  print(x)
  idx <- which(extended.trades.pnl$"Ccy pair" == x)
  if (length(idx)==0) next
  pnl <- xts(extended.trades.pnl[idx,"PnL minor"],extended.trades.pnl$"Exit time"[idx])
  par(ask=TRUE)
  plot.xts(cumsum(pnl),main=x)
}



x<-rnorm(100);
y <- xts(x, Sys.Date()+1:100);
barplot(y);



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







