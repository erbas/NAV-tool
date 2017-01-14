library(quantmod)
library(lubridate)
Sys.setenv(TZ="Europe/London")
options(lubridate.verbose = TRUE)
# options(warn=2)

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
  print("-- inside load.reval.files ---")
  f.list <- list.files(path,pattern="*_EOD.csv",full.names=TRUE,recursive=FALSE)
  reval.xts <- NULL
  for (f in f.list) {
    ccy.pair <- strsplit(last(strsplit(f,'/')[[1]]),"_")[[1]][1]
    print(ccy.pair)
    tryCatch({
      f.csv <- read.csv(f,stringsAsFactors=FALSE,header=TRUE,skip=1)
      dt <- dmy(f.csv[,1],tz="Europe/London")
      f.xts <- xts(as.numeric(f.csv[,2]),dt)
      idx <- which(as.Date(index(f.xts)) < as.Date(daterange)[1])
      f.xts.trimmed <- f.xts[-idx]
      colnames(f.xts.trimmed) <- ccy.pair
      reval.xts <- merge(f.xts.trimmed, reval.xts, fill=0)
    }, error = function(e) {
      e$message <- paste0("There is a problem with the ", ccy.pair, " reval file!")
      stop(e)
    })
  }
  # fill in missing values
  reval.xts[reval.xts == 0] <- NA
  reval.xts <- na.approx(reval.xts) 
  # turn dates into 5pm London date-times
  dt <- as.POSIXlt(index(reval.xts))
  dt$hour <- 17
  index(reval.xts) <- as.POSIXct(dt)
  idx <- which(duplicated(index(reval.xts)))
  #   print(idx)
  if (length(idx) > 0) {
    reval.xts <- na.locf(reval.xts[-idx,])
  }
  write.zoo(reval.xts,file="cache/reval_rates.csv",sep=",")
  return(reval.xts)
}

# --------------------------------------------------------------------
# create the main dataframe of raw trades
# --------------------------------------------------------------------
load.all.trades <- function(files.list) {
  print("---> load.all.trades")
  print(files.list)
  # read files
  files.summary <- NULL
  files.contents <- NULL
  n.lines <- 0 
  for (f.name in files.list) {
    tryCatch({
    f.csv <- read.csv(file=f.name,stringsAsFactors=FALSE)
    n.lines <- n.lines + nrow(f.csv)
    #     cat(n.lines,f.name,"\n")
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
    # debug
    if (df$Instrument[1] == "Backtest") {
      print(f.name)
      print(f.csv)
    }
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
    }, error = function(e) {
      e$message <- paste0("There was a problem loading the file ", f.csv)
      stop(e)
    })
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
  Amount.USD <- rep(0,nrow(df))
  # loop over trades converting trade amounts to USD
  for (i in 1:nrow(df)) {
    ccy1 <- substr(df[i,"Ccy pair"],1,3)
    ccy2 <- substr(df[i,"Ccy pair"],4,6)
    if (ccy1 == "USD") {
      to.USD <- 1
    } else if (ccy2 == "USD") {
      to.USD <- as.numeric(df[i,"Exit price"])
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
        print(paste("Currency pair",ccy1,ccy2,"not found in reval file",sep=" "))
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
  print("<--- exit make.trades.USD")
  return(df.usd)  
}

# --------------------------------------------------------------------
#  Split trades which straddle end-of-month
# --------------------------------------------------------------------


split.trades.at.month.ends <- function(df, reval) {
  # NOTE: df is assumed to have been created by make.trades.USD or get.all.trades
  print("---> inside split.trades.at.month.ends")
  df <- data.frame("TradeId"=c(1:nrow(df)),"SplitId"=rep(0,nrow(df)),df)
  names(df) <- gsub("."," ",names(df),fixed=TRUE)
  # use end-of-month datetimes to find trades which need to be split
  eom.reval <- get.ends.of.months(reval)
  eom.reval.dt <- index(eom.reval)
  idx.split.main <- NULL
  
  for (i in 1:length(eom.reval.dt)) {
    idx <- which(df$"Entry time" <= eom.reval.dt[i] & df$"Exit time" > eom.reval.dt[i])
    idx.split.main <- c(idx.split.main,idx)
  }
  idx.split.main <- unique(idx.split.main)
  trades.split <- df[idx.split.main,]
  
  # create "synthetic" trades to nominally close open positions at month ends
  synth <- NULL
  for (i in 1:nrow(trades.split)) {
    ccy.pair <- trades.split[i,"Ccy pair"]
    #     cat(i,trades.split$TradeId[i],trades.split[i,"Ccy pair"],"\n")
    #print(trades.split[i,])
    # how many extra trades do we need to create?  find list of month ends
    idx.split <- which(eom.reval.dt > trades.split[i,"Entry time"] & eom.reval.dt <= trades.split[i,"Exit time"])
    n.split <- length(idx.split)
    if (n.split == 0) {
      next   # nothing to see here, move along
    }
    eom.reval.split <- eom.reval.dt[idx.split]
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
    # if (i %% 10 == 0) cat(i,df[i,"TradeId"],df[i,"SplitId"],df[i,"Ccy pair"],"\n")
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

# extract returns for selected currency pairs from extended trade pnl's 
calc.returns <- function(trades.pnl, ccy.pairs) {
  print("---> inside calc.returns ---")
  idx <- NULL
  for (ccy in ccy.pairs) {
    idx <- c(idx,grep(pattern=ccy,x=trades.pnl$"Ccy pair",fixed=TRUE,ignore.case=FALSE,value=FALSE))
  }
  rtns.df <- trades.pnl[unique(idx), c("Ccy pair","PnL USD","Exit time")]
  rtns.xts <- xts(rtns.df[,"PnL USD"],rtns.df[,"Exit time"])
  colnames(rtns.xts) <- "PnL USD"
  print(paste0("Found ",nrow(rtns.df)," trades in ",ccy.pairs))
  print("<--- leaving calc.returns ---")
  return(rtns.xts)
}

# calcualte win/loss ratios 
calc.ratios <- function(trades.pnl, ccy.pairs) {
  print("---> inside calc.ratios ---")
  idx <- NULL
  for (ccy in ccy.pairs) {
    idx <- c(idx,grep(pattern=ccy,x=trades.pnl$"Ccy pair",fixed=TRUE,ignore.case=FALSE,value=FALSE))
  }
  rtns.df <- trades.pnl[unique(idx), c("Ccy pair", "TradeId", "PnL USD", "Amount USD")]
  pnl <- aggregate(rtns.df$"PnL USD"/rtns.df$"Amount USD", by=list(rtns.df$"TradeId"), sum)[,2]
  names(pnl) <- ''
  #   SUM.AGG = sum(pnl)
  #   SUM.RAW = sum(rtns.df$"PnL USD")
  #   WINLOSS <- length(pnl[pnl > 0])/length(pnl)
  #   ratios <- rbind(SUM.AGG, SUM.RAW, WINLOSS)
  #   row.names(ratios) <- c("SUM.AGG", "SUM.RAW", "WINLOSS")
  WINLOSS <- length(pnl[pnl > 0])/length(pnl)
  AV.WIN <- mean(pnl[pnl > 0])
  AV.LOSS <- mean(pnl[pnl < 0])
  EXPECT = mean(pnl)
  ratios <- data.frame(rbind(WINLOSS, AV.WIN, AV.LOSS, EXPECT))*100
  row.names(ratios) <- c("Win/Loss Ratio (%)", 
                         "Average Winning Trade (%)",
                         "Average Losing Trade (%)",
                         "Expectation (%)")
  colnames(ratios) <- ''
  print("<--- leaving calc.ratios ---")
  return(ratios)
}

# --------------------------------------------------------------------
#  expenses Calculation
# --------------------------------------------------------------------
apply.fees <- function(rtns.monthly, mgt.fee.rate=0.02, perf.fee.rate=0.20, aum=1.e8) {
  print("---> inside apply.fees ---")
  zero.xts <- xts(rep(0,length(rtns.monthly)),index(rtns.monthly))
  start.eq <- zero.xts
  pnl <- rtns.monthly
  names(pnl) <- "pnl"
  mgt.fee <- zero.xts
  perf.fee <- zero.xts
  end.eq <- zero.xts
  high.water.mark <- zero.xts
  # initialise calc
  start.eq[1] <- aum
  mgt.fee[1] <- mgt.fee.rate/12*start.eq[1]
  perf.fee[1] <- perf.fee.rate*(pnl[1] - mgt.fee[1])
  end.eq[1] <- start.eq[1] + pnl[1] - mgt.fee[1] - perf.fee[1] 
  high.water.mark[1] <- end.eq[1]
  # loop
  for (i in 2:length(zero.xts)) {
    start.eq[i] <- end.eq[i-1]
    mgt.fee[i] <- mgt.fee.rate/12*start.eq[i]
    high.water.mark[i] <- max(high.water.mark[1:i])
    if ( coredata(start.eq[i] + pnl[i] - mgt.fee[i]) > high.water.mark[i]  ) {
      perf.fee[i] <- perf.fee.rate*( start.eq[i] + pnl[i] - mgt.fee[i] - high.water.mark[i] )
      end.eq[i] <- start.eq[i] + pnl[i] - mgt.fee[i] - perf.fee[i]
      high.water.mark[i] <- coredata(end.eq[i])
    } else {
      perf.fee[i] <- 0.0
      end.eq[i] <- start.eq[i] + pnl[i] - mgt.fee[i] - perf.fee[i]
    }
  }
  # end.eq is now cumulative NAV minus mgt and performance fees
  # we want percentag returns net of fees
  rtns.net <- diff(rbind(xts(1.e8,index(end.eq)[1] - days(30)),end.eq))
  #   print(head(rtns.net))
  print("<--- leaving apply.fees ---")
  return(rtns.net[-1])
}


# --------------------------------------------------------------------
#  Open position Calculation
# --------------------------------------------------------------------

calc.open.pos <- function(trades.extended.pnl, daterange) {
  # NOTE: we use the split trades dataframe and assume no trade goes over month end
  print("---> inside calc.open.pos ---")
  #   print(str(trades.extended.pnl))
  idx.range <- which(as.Date(trades.extended.pnl$"Exit time") < as.Date(daterange)[1] | 
                       as.Date(trades.extended.pnl$"Exit time") > as.Date(daterange)[2])
  df.range <- trades.extended.pnl[-idx.range,]
  # find trades which are open at month end
  idx.open <- which( grepl("CarryOver",df.range$"Exit name"))
  df <- df.range[idx.open,]
  open.pos <- data.frame(df$"Ccy pair", df$"TradeId", df$"SplitId", df$"Amount major"*df$Sign, df$"Amount USD"*df$Sign, df$"Exit time")
  colnames(open.pos) <- c("Ccy Pair","Trade ID", "Split ID", "Amount major","Amount USD","Exit time")
  # split by ccypair
  open.list <- split(open.pos,open.pos$"Ccy Pair")
  # turn into xts and sum by month
  open.list.xts <- lapply(open.list, function(x) xts(x[,4:5],x[,6]))
  open.list.xts.m <- lapply(open.list.xts, function(x) apply.monthly(x,colSums))
  # total is a bit meaningless without doing all the cross-rate calculations
  # so we ignore it here
  print("<--- leaving calc.open.pos ---")
  return(open.list.xts.m)
}

# --------------------------------------------------------------------
#  statistics
# --------------------------------------------------------------------

calc.stats <- function(pnl,period=12) {
  print("---> inside calc.stats")
  # calculate monthly stats
  ABSRTN <- colSums(pnl)*100
  CAR <- Return.annualized(pnl,scale=period,geometric=T)*100
  VOL <- apply(pnl,2,sd)*sqrt(period)*100
  MAXDRAW <- maxDrawdown(pnl,geometric=T)*100
  SHARPE <- SharpeRatio.annualized(pnl,Rf=0.0025/period,scale=period,geometric=T)
  SORTINO <- SortinoRatio(pnl)
  SKEWNESS <- skewness(pnl,method="moment")
  KURTOSIS <- kurtosis(pnl,method="moment")
  OMEGA <- Omega(pnl,L=0)
  #   KELLY <- KellyRatio(pnl,Rf=0)
  DRAWDNS <- table.Drawdowns(pnl[,1])
  n <- min(10,max(as.integer(row.names(DRAWDNS))))
  DRAWDNS <- table.Drawdowns(pnl[,1],top=n)
  RECOV <- c(DRAWDNS[1,"Recovery"])
  CONSEC <- max(DRAWDNS[,"To Trough"])
  
  statstable <- data.frame(rbind(ABSRTN,CAR,MAXDRAW,RECOV,CONSEC,VOL,SHARPE,SORTINO,SKEWNESS,KURTOSIS,OMEGA))
  colnames(statstable) <- ""
  row.names(statstable) <- c("Total Return (% AUM)",
                             "Compounded Annual Return (%)",
                             "Max Drawdown (% AUM)",
                             "Months to Recovery",
                             "Max Consecutive Losing Months",
                             "Annualized Volatility (%)",
                             "Sharpe Ratio",
                             "Sortino Ratio",
                             "Skewness",
                             "Kurtosis",
                             "Omega Ratio"
  )
  print("<--- leaving calc.stats ---")
  return(statstable)  
}

# calc.winloss(trades.usd, reval.rtes) {
#   result <- trades.usd$sign*(trades.usd$Exit.price - trades.usd$Entry.price)
#   pnl <- result*trades.usd$Amount.major/trades.usd$Amount.USD
#   pnl <- length(result[result > 0])/length(result)
#   
# }

# --------------------------------------------------------------------
#  end of month handling
# --------------------------------------------------------------------

get.ends.of.months <- function(reval) {
  eom <- NULL
  for (y in unique(year(index(reval)))) {
    ii <- which(year(index(reval)) == y)
    reval.year <- reval[ii,]
    for (m in unique(month(index(reval.year)))) {
      jj <- month(index(reval.year)) == m
      reval.month <- reval.year[jj,]
      eom <- rbind(eom,last(reval.month))
    }
  }
  return(eom)
}

my.apply.monthly <- function(rtns, eom.datetimes, FUN=sum) {
  rtns.dt <- index(rtns)
  # find range of datetimes
  eom.first <- first(which(eom.datetimes > rtns.dt[1]))
  eom.last <- last(which(eom.datetimes < last(rtns.dt))) + 1
  eom.dt <- eom.datetimes[eom.first:eom.last]
  # begin applying FUN - first data point is special
  idx <- which(rtns.dt <= eom.dt[1])  
  first.total <- FUN(rtns[idx,])  
  monthly.total <- xts(first.total, eom.dt[1])
  for (i in 2:length(eom.dt)) {
    idx <- which(rtns.dt > eom.dt[i-1] & rtns.dt <= eom.dt[i])
    total <- FUN(rtns[idx,])
    monthly.total <- rbind(monthly.total,xts(total,eom.dt[i]))
  }
  return(monthly.total)
}


