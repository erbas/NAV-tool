library(quantmod)
library(lubridate)
options(lubridate.verbose = TRUE)

# --------------------------------------------------------------------
#  load previously saved data
# --------------------------------------------------------------------

# read saved reval data
read.saved.reval <- function() {
  print("---> inside read.saved.reval")
  f.csv <- read.csv(file='cache/reval_rates.csv',header=TRUE,stringsAsFactors=FALSE)
  dt <- ymd_hms(f.csv[,1], tz="Europe/London",truncated=1)
  f.xts <- xts(f.csv[,-1],dt)
  return(f.xts)
}

# read saved trade data 
read.saved.trades <- function() {
  print("---> inside read.saved.trades")
  df <- read.csv(file="cache/all_trades.csv",header=TRUE,stringsAsFactors=FALSE,
                 colClasses=c("NULL","character","character","numeric","numeric","numeric",
                              "character","character","character","character","integer","factor",
                              "factor","factor","factor"))
  cn <- colnames(df)
  cn2 <- gsub("."," ",cn,fixed=TRUE)
  colnames(df) <- cn2
  df$"Entry time" <- ymd_hms(df$"Entry time",tz="Europe/London",truncated=1)
  df$"Exit time" <- ymd_hms(df$"Exit time",tz="Europe/London",truncated=1)
  return(df)
}

# read trade data with amounts in USD
read.saved.usd <- function() {
  print("---> inside read.saved.usd")
  df <- read.csv(file="cache/trades_usd.csv",header=TRUE,stringsAsFactors=FALSE,
                 colClasses=c("NULL","character","character","numeric","numeric","numeric",
                              "character","character","character","character","integer","factor",
                              "factor","factor","factor","numeric","numeric"))
  cn <- colnames(df)
  cn2 <- gsub("."," ",cn,fixed=TRUE)
  colnames(df) <- cn2
  df$"Entry time" <- ymd_hms(df$"Entry time",tz="Europe/London",truncated=1)
  df$"Exit time" <- ymd_hms(df$"Exit time",tz="Europe/London",truncated=1)
  return(df)
}


# read extended trade data
read.saved.extended <- function() {
  print("---> inside read.saved.extended")
  df <- read.csv(file="cache/trades_extended.csv",header=TRUE,stringsAsFactors=FALSE,
                 colClasses=c("NULL","integer","integer","character","character","numeric","numeric","numeric",
                              "character","character","character","character","integer","factor",
                              "factor","factor","factor","numeric","numeric"))
  cn <- colnames(df)
  cn2 <- gsub("."," ",cn,fixed=TRUE)
  colnames(df) <- cn2
  df$"Entry time" <- ymd_hms(df$"Entry time",tz="Europe/London",truncated=1)
  df$"Exit time" <- ymd_hms(df$"Exit time",tz="Europe/London",truncated=1)
  return(df)
}

# read extended trade data
read.saved.extended.pnl <- function() {
  df <- read.csv(file="cache/trades_extended_pnl.csv",header=TRUE,stringsAsFactors=FALSE,
                 colClasses=c("NULL","integer","integer","character","character","numeric","numeric","numeric",
                              "character","character","character","character","integer","factor",
                              "factor","factor","factor","numeric","numeric","numeric","numeric"))
  colnames(df) <- gsub("."," ",colnames(df),fixed=TRUE)
  df$"Entry time" <- ymd_hms(df$"Entry time",tz="Europe/London",truncated=1)
  df$"Exit time" <- ymd_hms(df$"Exit time",tz="Europe/London",truncated=1)
  return(df)
}


# --------------------------------------------------------------------
# find and load all reval files
# --------------------------------------------------------------------
load.reval.files <- function(path,daterange) {
  f.list <- list.files(path,pattern="*_EOD.csv",full.names=TRUE,recursive=FALSE)
  reval.xts <- NULL
  for (f in f.list) {
    ccy.pair <- strsplit(last(strsplit(f,'/')[[1]]),"_")[[1]][1]
    print(ccy.pair)
    f.csv <- read.csv(f,stringsAsFactors=FALSE,header=TRUE,skip=1)
    dt <- dmy(f.csv[,1],tz="Europe/London")
    f.xts <- xts(f.csv[,2],dt)
    idx <- which(as.Date(index(f.xts)) < as.Date(daterange)[1] | as.Date(index(f.xts)) > as.Date(daterange)[2])
    f.xts.trimmed <- f.xts[-idx]
    colnames(f.xts.trimmed) <- ccy.pair
    reval.xts <- merge(f.xts.trimmed,reval.xts,fill=0)
  }
  # fill in missing values
  reval.xts[reval.xts == 0] <- NA
  reval.xts <- na.approx(reval.xts)      
  # turn dates into 5pm London date-times
  dt <- as.POSIXlt(index(reval.xts))
  dt$hour <- 17
  index(reval.xts) <- as.POSIXct(dt)
  idx <- which(duplicated(index(reval.xts)))
  reval.xts <- reval.xts[-idx,]
  write.zoo(reval.xts,file="cache/reval_rates.csv",sep=",")
  return(reval.xts)
}

# --------------------------------------------------------------------
# create the main dataframe of raw trades
# --------------------------------------------------------------------
load.all.trades <- function(files.list) {
  print("---> load.all.trades")
  # read files
  files.summary <- NULL
  files.contents <- NULL
  n.lines <- 0 
  for (f.name in files.list) {
    f.csv <- read.csv(file=f.name,stringsAsFactors=FALSE)
    n.lines <- n.lines + nrow(f.csv)
    cat(n.lines,f.name,"\n")
    # get details about trades from name of folder containing this file
    p <- strsplit(f.name,"/",fixed=TRUE)[[1]]
    a <- strsplit(last(p)," ")[[1]]
    idx <- ifelse(grepl("bp",x=p[length(p)-1]),1,2)
    b <- strsplit(p[length(p)-idx]," ")[[1]]
    time.frame <- a[3]
    exit.type <- b[3]
    trade.size <- gsub('bp','',last(b))
    ratio <- NA
    if (exit.type == "Ratio") {
      ratio <- strsplit(last(a),".",fixed=TRUE)[[1]][1]
    }
    # make the data frame
    df <- f.csv[,c("Instrument","Market.pos.","Quantity","Entry.price","Exit.price",
                   "Entry.time","Exit.time","Entry.name","Exit.name","Bars")]
    # tidy up the instrument names, ie remove $ sign
    df$Instrument <- sapply(df$Instrument,function(x) gsub(pattern='$','',x,fixed=TRUE))
    # add in the other fields
    df <- cbind(df,time.frame,trade.size,exit.type,ratio)
    # convert datetimes, catching the case of missing seconds
    dt.entry <- tryCatch(dmy_hms(df$Entry.time,tz="Europe/London",truncated=1),
                         warning = function(w) {
                           print(w)
                           dt <- dmy_hm(df$Entry.time,tz="Europe/London")
                           return(dt)
                         })
    dt.exit <- tryCatch(dmy_hms(df$Exit.time,tz="Europe/London",truncated=1),
                     warning = function(w) {
                       print(w)
                       dt <- dmy_hm(df$Entry.time,tz="Europe/London")
                       return(dt)
                     })
    df$Entry.time <- dt.entry
    df$Exit.time <- dt.exit
    files.contents <- rbind(files.contents,df)
  }
  # relabale columns
  names(files.contents) <- c('Ccy pair','Direction','Amount major','Entry price','Exit price','Entry time','Exit time','Entry name','Exit name','Bars','Timeframe','RATS','Exit type','Ratio')
  # save the results
  write.csv(files.contents,file="cache/all_trades.csv")
  return(files.contents)
}


# --------------------------------------------------------------------
#  convert quantity to USD, return extended trade data.frame
# --------------------------------------------------------------------
make.trades.USD <- function(df, reval) {
  print("---> inside make.trades.USD")
#   print(str(df))
  Amount.USD <- rep(0,nrow(df))
  # loop over trades converting trade amounts to USD
  for (i in 1:nrow(df)) {
    ccy1 <- substr(df[i,"Ccy pair"],1,3)
    ccy2 <- substr(df[i,"Ccy pair"],4,6)
    if (ccy1 == "USD") {
      to.USD <- 1
    } else if (ccy2 == "USD") {
      to.USD <- df[i,"Exit price"]
    } else {
      ccypair1 <- paste0(ccy2,"USD")
      ccypair2 <- paste0("USD",ccy2)
      idx <- which(index(reval) >= df$Exit.time[i])
      if (length(idx)==0) { 
        dt <- last(index(reval))
      } else {
        dt <- first(index(reval)[idx])
      }
      if (ccypair1 %in% colnames(reval)) {
        to.USD <- reval[dt,ccypair1]
      } else if (ccypair2 %in% colnames(reval)) {
        to.USD <- 1/reval[dt,ccypair2]
      } else {
        stop(paste("Currency pair",ccy1,ccy2,"not found in reval file",sep=" "))
      } 
    }
    Amount.USD[i] <- coredata(to.USD) * df[i,"Amount major"]
  }
  # make numerical field for trade direction
  long.short <- rep(0,nrow(df))
  long.short[which(df$Direction == "Long")] <- 1
  long.short[which(df$Direction == "Short")] <- -1
  extras <- cbind(Amount.USD,long.short)
  colnames(extras) <- c("Amount USD","Sign")
  df.usd <- cbind(df,extras)
  write.csv(df.usd,file="cache/trades_usd.csv")
  return(df.usd)  
}

# --------------------------------------------------------------------
#  Split trades which straddle end-of-month
# --------------------------------------------------------------------

split.trades.at.month.ends <- function(df, reval) {
  # NOTE: df is assumed to have been created by make.trades.USD or get.all.trades
  # find trades which close in a different month to the one they start, or year
  print("---> inside split.trades.at.month.ends")
  df <- data.frame("TradeId"=c(1:nrow(df)),"SplitId"=rep(0,nrow(df)),df)
  names(df) <- gsub("."," ",names(df),fixed=TRUE)
#   print(str(df))
  idx.split.main <- which(month(df$"Entry time") != month(df$"Exit time") | 
                     year(df$"Entry time") != year(df$"Exit time"))
  trades.split <- df[idx.split.main,]
  # find the end of month revals (ie last trading day of each month)
  month.ends <- seq(from=first(index(reval)), 
                    to=last(index(reval)) %m+% months(2), 
                    by="1 month") - days(1)
  idx <- lapply(month.ends, function(x) which(index(reval) <= x))
  eom.reval <- Reduce(rbind, lapply(idx, function(x) reval[last(x),]))  # witchcraft
  eom.reval.index <- index(eom.reval)
  # create "synthetic" trades to nominally close open positions at month ends
  synth <- NULL
  for (i in 1:nrow(trades.split)) {
    ccy.pair <- trades.split[i,"Ccy pair"]
    cat(i,trades.split$TradeId[i],trades.split[i,"Ccy pair"],"\n")
    # how many extra trades do we need to create?  find list of month ends
    idx.split <- which(eom.reval.index > trades.split[i,"Entry time"] & eom.reval.index < trades.split[i,"Exit time"])
    if (length(idx.split) == 0) {
      next   # nothing to see here, move along
    }
    eom.reval.split <- eom.reval.index[idx.split]
    n.split <- length(idx.split)
    # make the last synthetic trade, ie entry is at eom preceding trade exit
    eom.dt <- last(eom.reval.split)
    new.open <- trades.split[i,]
    new.open$"Entry time" <- eom.dt + minutes(1)
    new.open$"Entry price" <- coredata(reval[eom.dt, ccy.pair]) 
    new.open$"Entry name" <- "CarryOver"
    new.open$SplitId <- n.split+1
    # make the first synthetic trade, ie exit is at first eom after entry 
    eom.dt <- first(eom.reval.split)
    new.close <- trades.split[i,]
    new.close$"Exit time" <- eom.dt
    new.close$"Exit price" <- coredata(reval[eom.dt, ccy.pair])
    new.close$"Exit name" <- "CarryOver"
    new.close$SplitId <- 1
    # append new trades
    synth <- rbind(synth,new.close,new.open)
    # handle completely synthetic trades
    if (n.split > 1) {
      for (j in 2:n.split) {
        new.trade <- trades.split[i,]
        dt.entry <- eom.reval.split[j-1]
        dt.exit <- eom.reval.split[j]
        new.trade$"Entry time" <- dt.entry + minutes(1)
        new.trade$"Exit time" <- dt.exit
        new.trade[1,"Entry price"] <- coredata(reval[dt.entry, ccy.pair])
        new.trade[1,"Exit price"] <- coredata(reval[dt.exit, ccy.pair])
        new.trade$"Entry name" <- "CarryOver"
        new.trade$"Exit name" <- "CarryOver"
        new.trade$SplitId <- j
        synth <- rbind(synth,new.trade)
      }
    }
  }
  all.trades <- rbind(df[-idx.split.main,],synth)
  # save the results
  write.csv(all.trades,file="cache/trades_extended.csv")
  return(all.trades)
}

# --------------------------------------------------------------------
#  calculate the pnl of each trade in minor ccy and in USD
# --------------------------------------------------------------------

calc.pnl <- function(trades.extended, reval) {
  print("---> inside calc.pnl")
#   str(trades.extended)
  df <- trades.extended
  pnl.raw <- (df$"Exit price" - df$"Entry price") * df$"Amount major" * df$Sign
  df <- cbind(df,pnl.raw)
  colnames(df)[ncol(df)] <- "PnL minor"
  pnl.USD <- rep(0,nrow(df))
  # loop over trades converting PnL amounts to USD
  for (i in 1:nrow(df)) {
    if (i %% 10 == 0) cat(i,df[i,"TradeId"],df[i,"SplitId"],df[i,"Ccy pair"],"\n")
    ccy1 <- substr(df[i,"Ccy pair"],1,3)
    ccy2 <- substr(df[i,"Ccy pair"],4,6)
    if (ccy1 == "USD") {
      to.USD <- 1/df[i,"Exit price"]
    } else if (ccy2 == "USD") {
      to.USD <- 1
    } else {   # cross-rate
      ccypair1 <- paste0(ccy2,"USD")
      ccypair2 <- paste0("USD",ccy2)
      # find next reval date
      dt <- df[i,"Exit time"]
      if (all(index(reval) < dt)) {
        dt.reval <- last(index(reval))
      } else {
        idx <- which(index(reval) >= dt)
        dt.reval <- first(index(reval)[idx])
      }
      # select conversion rate
      if (ccypair1 %in% colnames(reval)) {
        to.USD <- reval[dt.reval, ccypair1]
      } else if (ccypair2 %in% colnames(reval)) {
        to.USD <- 1/reval[dt.reval, ccypair2]
      } else {
        stop(paste("Currency pair",ccy1,ccy2,"not found in reval file",sep=" "))
      } 
    }
    pnl.USD[i] <- coredata(to.USD) * df[i,"PnL minor"]
  }
  df <- cbind(df,pnl.USD)
  colnames(df)[ncol(df)] <- "PnL USD"
  # save the results
  write.csv(df,file="cache/trades_extended_pnl.csv")
  return(df)
}

# extract returns from extended trade pnl's, trimming to daterange
calc.returns <- function(trades.pnl, daterange, ccy.pair) {
  print("---> inside calc.returns")
#   print(str(trades.pnl))
  if (ccy.pair == "all") {
    rtns.xts <- xts(trades.pnl$"PnL USD",trades.pnl$"Exit time")
  } else {
    idx <- which(trades.pnl$"Ccy pair" == ccy.pair)
    rtns.xts <- xts(trades.pnl[idx,"PnL USD"],trades.pnl[idx,"Exit time"])
  }
  colnames(rtns.xts) <- ccy.pair
  # filter returns outside the range
  idx <- which(as.Date(index(rtns.xts)) < as.Date(daterange)[1] | as.Date(index(rtns.xts)) > as.Date(daterange)[2])
  if (length(idx) > 0) {
    rtns.xts <- rtns.xts[-idx]
  }
  return(rtns.xts)
}

# --------------------------------------------------------------------
#  expenses Calculation
# --------------------------------------------------------------------
calc.net.rtns <- function(rtns.monthly, mgt.fee=0.02, perf.fee=0.20, aum=1.e8, cmpd=FALSE) {
  print("---> inside calc.net.rtns")
  # subtract management fee
  rtns.net <- rtns.monthly - mgt.fee*aum/12
  # calculate highwatermark without fees
  if (cmpd) {
    nav <- cumprod(1+rtns.net/aum)*aum
  } else {
    nav <- aum + cumsum(rtns.net)
  }
  hw.mark <- xts(rep(0,length(nav)),index(nav))
  hw.mark[1] <- nav[1]
  for (i in 2:length(nav)) {
#     cat(i, nav[i], max(hw.mark), nav[i] > max(hw.mark), "\n")
    if (nav[i] > max(hw.mark)) {
      hw.mark[i] <- nav[i]
    }
  }
  # subtract perforamcne fees at period where we have a new highwater mark
  idx <- which(hw.mark != 0)
  rtns.net[idx] <- rtns.net[idx] - perf.fee*rtns.net[idx]
  return(rtns.net)
}


# --------------------------------------------------------------------
#  Open position Calculation
# --------------------------------------------------------------------

calc.open.pos <- function(trades.extended.pnl, daterange) {
  # NOTE: we use the split trades dataframe and assume no trade goes over month end
  print("---> inside calc.open.pos")
#   print(str(trades.extended.pnl))
  idx.range <- which(as.Date(trades.extended.pnl$"Exit time") < as.Date(daterange)[1] | 
                       as.Date(trades.extended.pnl$"Exit time") > as.Date(daterange)[2])
  df.range <- trades.extended.pnl[-idx.range,]
  # find trades which are open at month end, and trades which close during the month
  idx.open <- which( grepl("CarryOver",df.range$"Exit name"))
  df <- df.range[idx.open,]
  # split opens from closes
  open.pos <- data.frame(df$"Ccy pair", df$"TradeId", df$"SplitId", df$"Amount major"*df$Sign, df$"Amount USD"*df$Sign, df$"Exit time")
  colnames(open.pos) <- c("Ccy Pair","Trade ID", "Split ID", "Amount major","Amount USD","Exit time")
  # split by ccypair
  open.list <- split(open.pos,open.pos$"Ccy Pair")
  # turn into xts and sum by month
  open.list.xts <- lapply(open.list, function(x) xts(x[,2:5],x[,6]))
  open.list.xts.m <- lapply(open.list.xts, function(x) apply.monthly(x,colSums))
  # total is a bit meaningless without doing all the cross-rate calculations
  # so we ignore it here
  return(open.list.xts.m)
}


  
