
path1 <- "CRNCY_Trade File_Model/"
path2 <- "CMDTY_Trade File_Model/"

files.to.load <- c(list.files(path1,pattern="*.csv",full.names=TRUE,recursive=TRUE), list.files(path2,pattern="*.csv",full.names=TRUE,recursive=TRUE))

trade.data <- load.all.trades(files.to.load)
reval <- load.reval.files("Revaluation rates/",c("2010-01-01","2013-12-31"))

trades.usd <- make.trades.USD(trade.data, reval)
rtns <- calc.returns(trades.usd,as.Date(c("2010-01-01","2013-12-31")))
rtns.sum <- cumsum(rtns)
rtns.sum.m <- apply.monthly(rtns,sum)
index(rtns.sum.m) <- as.yearmon(index(rtns.sum.m) )
pnl.m <- 1+cumsum(rtns.sum.m["2010::2013"])/1.e8

plot.xts(pnl.m,main="Growth of $1")
chart.TimeSeries(pnl.m,date.format="%b-%Y",main="Growth of $1",xlab="",ylab="")
plot.xts(100+cumsum(rtns.sum.m)/1.e6,main="NAV (m USD)")

rtns.net <- calc.net.rtns(rtns.sum.m,mgt.fee=0.02,perf.fee=0.2)
both.rtns <- merge(rtns.sum.m,rtns.net,fill=0)
colnames(both.rtns) <- c("Returns","Net Returns")
barplot(both.rtns/1.e6,beside=TRUE,col=c(3,4),main="Returns and Net Returns",legend.text=c("Returns","Net Returns"), args.legend = list(x = "topleft"))
plot.zoo(cumsum(merge(rtns.sum.m,rtns.net,fill=0))/1.e6,plot.type='single',col=c(3,4))
chart.TimeSeries(cumsum(both.rtns)/1.e6,ylab="",xlab="",main="Returns and Net Returns (mUSD)",date.format="%b-%Y",colorset=c(3,4),legend.loc="topleft")

op <- calc.open.pos(trades.usd)
index(op) <- as.yearmon(index(op))
barplot(op["2010::2013"])
chart.TimeSeries(cumsum(op)/1.e6,type="h",date.format="%b-%Y",main="Value of all open positions (mUSD)",xlab="",ylab="")

s <- read.csv("summary trade files.csv")

f.csv <- read.csv("CRNCY_Trade File_Model//Sub Strategy/CIT/CIT SS2_553 Ratio Sell 25bp_Rats 12 5bp/BT4 USDCAD 1440 Sells_01012010 31122013 3x.csv",stringsAsFactors=FALSE)
dt1 <- dmy_hms(f.csv$Entry.time,tz="Europe/London")
dt2 <- dmy_hms(f.csv$Exit.time,tz="Europe/London")

tryCatch(dmy_hms(f.csv$Exit.time,tz="Europe/London"),
         warning = function(w) {
           print(w)
           dt <- dmy_hm(f.csv$Entry.time,tz="Europe/London")
           return(dt)
         },
         error = function(e) {
           print(e)
           stop()
         }
)
