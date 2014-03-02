# load libraries and functions
source('unreactive.R')

# load data - currencies only
path1 <- "CRNCY_Trade File_Model/"
path2 <- "CMDTY_Trade File_Model/"
path3 <- "Revaluation rates/"
path1 <- "E:/Cloud Data/Published Returns/Global Currency Program/CRNCY_31 Dec 2013/CRNCY_Trade File_Model"
path2 <- "E:/Cloud Data/Published Returns/Global Commodity Program/Dec 31_2013/CMDTY_Trade File_Model/"
path3 <- "E:/Cloud Data/Data History/Revaluation rates/"
files.to.load <- c(list.files(path1,pattern="*.csv",full.names=TRUE,recursive=TRUE), list.files(path2,pattern="*.csv",full.names=TRUE,recursive=TRUE))
# # files.to.load <- list.files(path1,pattern="*.csv",full.names=TRUE,recursive=TRUE)
trade.data <- load.all.trades(files.to.load)
reval <- load.reval.files(path3,c("2010-01-01","2013-12-31"))
trades.usd <- make.trades.USD(trade.data, reval)
trades.extended <- split.trades.at.month.ends(trades.usd, reval)
extended.trades.pnl <- calc.pnl(trades.extended, reval)

trade.data <- read.saved.trades()
reval <- read.saved.reval()
trades.usd <- read.saved.usd()
extended.trades.pnl <- read.saved.extended.pnl()

# recombine pnls 
pnl.entry <- data.frame(TradeId=unique(sort(extended.trades.pnl$TradeId)), Ccy.pair="",Entry.time=0, PnL.USD=0,stringsAsFactors=FALSE)
for (i in 1:nrow(pnl.entry)) {
  idx <- which( extended.trades.pnl$TradeId == pnl.entry$TradeId[i])
  x <- extended.trades.pnl[idx,]
  pnl.entry$Ccy.pair[i] <- x[1,]$"Ccy pair"
  pnl.entry$Entry.time[i] <- x[1,]$"Entry time"
  pnl.entry$PnL.USD[i] <- sum(x$"PnL USD")
}

# analysis
# pnl by ccy by four hour time block

pnl.entry.xts <- xts(pnl.entry$PnL.USD,as.POSIXct(as.numeric(pnl.entry$Entry.time),origin='1970-01-01 00:00.00 UTC',tz="Europe/London"))
colnames(pnl.entry.xts) <- "PnL"
pnl.1 <- pnl.entry.xts['T06/T09:59']
pnl.2 <- pnl.entry.xts['T10/T13:59']
pnl.3 <- pnl.entry.xts['T14/T17:59']
pnl.4 <- pnl.entry.xts['T18/T21:59']
pnl.5.1 <- pnl.entry.xts['T22/T23:59']
pnl.5.2 <- pnl.entry.xts['T00:00/T01:59']
pnl.5 <- rbind(pnl.5.1,pnl.5.2)
pnl.6 <- pnl.entry.xts['T02/T05:59']

pnls <- list(pnl.1,pnl.2,pnl.3,pnl.4,pnl.5,pnl.6)
pnls.date <- lapply(pnls, function(x) {index(x) <- as.Date(index(x)); x})

# plot number of trades in each 4 hour block
AUM <- 1.e8
tz.rtns <- sapply(pnls.date,sum)/AUM*100
tz.str <- paste(format(tz.rtns,digits=2),"%",sep="")
tz.labels <- c("6am-10am",
               "10am-2pm",
               "2pm-6pm",
               "6pm-10pm",
               "10pm-2am",
               "2am-6am")

barplot(sapply(pnls.date,length),names.arg=tz.labels,main="Number of Trades in each 4 hour window",cex.names=0.8)

# plot total returns in each 4 hour block
layout(t(matrix(1:6,3,2)),respect=FALSE)
cex.m <- 1
cex.lg <- 0.8
for (i in 1:6) {
  txt <- paste("Total Returns from",tz.labels[i])
  if (length(pnls.date[[i]]) > 1) 
    plot.zoo(cumsum(pnls.date[[i]])/AUM*100,main=txt,ylab="% AUM",xlab="")
}

# analysis by ccy pair 
trades.ccy <- split(pnl.entry,pnl.entry$Ccy.pair)

# plot returns by ccy and timezone
for (x in trades.ccy) {
  # make xts
  ccy <- x$Ccy.pair[1]
  pnl.xts <- xts(x$PnL.USD,as.POSIXct(as.numeric(x$Entry.time),origin='1970-01-01 00:00.00 UTC',tz="Europe/London"))
  colnames(pnl.xts) <- ccy
  # split by time zone
#   pnl.1 <- pnl.xts['T06/T09:59']
#   pnl.2 <- pnl.xts['T10/T13:59']
#   pnl.3 <- pnl.xts['T14/T17:59']
#   pnl.4 <- pnl.xts['T18/T21:59']
#   pnl.5 <- pnl.xts['T22/T01:59']
#   pnl.6 <- pnl.xts['T02/T05:59']
#   pnl.0 <- pnl.xts['T00/T00:59']
#   pnl.1 <- pnl.xts['T01/T01:59']
#   pnl.2 <- pnl.xts['T02/T02:59']
#   pnl.3 <- pnl.xts['T03/T03:59']
#   pnl.4 <- pnl.xts['T04/T04:59']
#   pnl.5 <- pnl.xts['T05/T05:59']
#   pnl.6 <- pnl.xts['T06/T06:59']
#   pnl.7 <- pnl.xts['T07/T07:59']
#   pnl.8 <- pnl.xts['T08/T08:59']
#   pnl.9 <- pnl.xts['T09/T09:59']
#   pnl.10 <- pnl.xts['T10/T10:59']
#   pnl.11 <- pnl.xts['T11/T11:59']
#   pnl.12 <- pnl.xts['T12/T12:59']
#   pnl.13 <- pnl.xts['T13/T13:59']
#   pnl.14 <- pnl.xts['T14/T14:59']
#   pnl.15 <- pnl.xts['T15/T15:59']
#   pnl.16 <- pnl.xts['T16/T16:59']
#   pnl.17 <- pnl.xts['T17/T17:59']
#   pnl.18 <- pnl.xts['T18/T18:59']
#   pnl.19 <- pnl.xts['T19/T19:59']
#   pnl.20 <- pnl.xts['T20/T20:59']
#   pnl.21 <- pnl.xts['T21/T21:59']
#   pnl.22 <- pnl.xts['T22/T22:59']
#   pnl.23 <- pnl.xts['T23/T23:59']
# 
#   # combine into single data object
#   pnls.ccy <- list(pnl.0,pnl.1,pnl.2,pnl.3,pnl.4,pnl.5,pnl.6,
#                    pnl.7,pnl.8,pnl.9,pnl.10,pnl.11,pnl.12,
#                    pnl.13,pnl.14,pnl.15,pnl.16,pnl.17,pnl.18,
#                    pnl.19,pnl.20,pnl.21,pnl.22,pnl.23)

  pnl.1 <- pnl.xts['T06/T09:59']
  pnl.2 <- pnl.xts['T10/T13:59']
  pnl.3 <- pnl.xts['T14/T17:59']
  pnl.4 <- pnl.xts['T18/T21:59']
  pnl.5.1 <- pnl.xts['T22/T23:59']
  pnl.5.2 <- pnl.xts['T00:00/T01:59']
  pnl.5 <- rbind(pnl.5.1,pnl.5.2)
  pnl.6 <- pnl.xts['T02/T05:59']

  pnls.ccy <- list(pnl.1,pnl.2,pnl.3,pnl.4,pnl.5,pnl.6)

  pnls.ccy <- lapply(pnls.ccy, function(x) {index(x) <- as.Date(index(x)); x})
  # make plot
  f.name <- paste0("Performance by Hour/",ccy,".pdf")
  pdf(file=f.name,width=11,height=8,onefile=TRUE,paper="a4r")
#   f.name <- paste0("Performance by Hour/",ccy,".png")
#   png(filename=f.name,width=800,height=570,quality=0.95)
  layout(t(matrix(1:6,3,2)),respect=FALSE)
  cex.m <- 1
  cex.lg <- 0.7
  for (i in 1:6) {
#     txt <- paste(ccy,"hour",i,sep=" ")
    txt <- paste(ccy,tz.labels[i],sep=" ")
    cat(ccy,i,length(pnls.ccy[[i]]),"\n")
    if (length(pnls.ccy[[i]]) > 1)
      plot.zoo(cumsum(pnls.ccy[[i]])/AUM*100,main=txt,ylab="% AUM",xlab="")
  }
#   barplot(sapply(pnls.ccy,length),names.arg=paste(1:24),main="Trades in hour block",cex.names=0.8)
  
  dev.off()
}


# analysis by ccy and timezone - numbers
performance.by.tz <- vector("list")
for (x in trades.ccy) {
  # make xts
  ccy <- x$Ccy.pair[1]
  pnl.xts <- xts(x$PnL.USD,as.POSIXct(as.numeric(x$Entry.time),origin='1970-01-01 00:00.00 UTC',tz="Europe/London"))
  colnames(pnl.xts) <- ccy
  # split by time zone
  pnl.1 <- pnl.xts['T06/T09:59']
  pnl.2 <- pnl.xts['T10/T13:59']
  pnl.3 <- pnl.xts['T14/T17:59']
  pnl.4 <- pnl.xts['T18/T21:59']
  pnl.5 <- pnl.xts['T22/T01:59']
  pnl.6 <- pnl.xts['T02/T05:59']
  # combine into single data object
  pnls.ccy <- list(pnl.1,pnl.2,pnl.3,pnl.4,pnl.5,pnl.6)
  pnls.ccy <- lapply(pnls.ccy, function(x) {index(x) <- as.Date(index(x)); x})
  # numbers
  tmp.rtns <- sapply(pnls.ccy,sum)/AUM*100
  performance.by.tz[[ccy]] <- tmp.rtns
}
rtns.by.tz <- as.data.frame(performance.by.tz)
row.names(rtns.by.tz) <- c("6am","10am","2pm","6pm","10pm","2am")
y <- t(rtns.by.tz)

raw.total <- as.data.frame(colSums(rtns.by.tz))
colnames(raw.total) <- "Total Return"
raw.total <- rbind(raw.total,sum(rtns.by.tz))
rtns.filter <- rtns.by.tz
rtns.filter[rtns.filter < 0.5] <- 0
filter.total <- as.data.frame(colSums(rtns.filter))
colnames(filter.total) <- "Total Return (filtered)"
filter.total <- rbind(filter.total,sum(rtns.filter))
rtns.all <- cbind(raw.total,filter.total)
row.names(rtns.all)[17] <- "Total"
