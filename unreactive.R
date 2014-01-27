library(quantmod)
library(lubridate)

# --------------------------------------------------------------------
#  load previously saved data
# --------------------------------------------------------------------

# read saved reval data
read.saved.reval <- function() {
  f.csv <- read.csv(file='reval-rates.csv',header=TRUE,stringsAsFactors=FALSE)
  dt <- ymd_hms(f.csv[,1], tz="Europe/London")
  f.xts <- xts(f.csv[,-1],dt)
  return(f.xts)
}

# read saved trade data
read.saved.trades <- function() {
  df <- read.csv(file="all-trades.csv",header=TRUE,stringsAsFactors=FALSE,
                 colClasses=c("NULL","character","character","numeric","numeric","numeric",
                              "character","character","character","character","integer","factor",
                              "character","factor","factor")
  )
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
    f.csv <- read.csv(f,stringsAsFactors=FALSE,header=TRUE)
    dt <- dmy(f.csv[,1],tz="Europe/London")
    f.xts <- xts(f.csv[,2],dt)
    f.xts.trimmed <- f.xts[as.Date(index(f.xts)) >= daterange[1] & as.Date(index(f.xts)) <= daterange[2]]
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
  write.zoo(reval.xts,file="reval-rates.csv",sep=",")
  return(reval.xts)
}

# --------------------------------------------------------------------
# create the main dataframe of raw trades
# --------------------------------------------------------------------
load.all.trades <- function(files.list) {
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
    dt.entry <- tryCatch(dmy_hms(df$Entry.time,tz="Europe/London"),
                         warning = function(w) {
                           print(w)
                           dt <- dmy_hm(df$Entry.time,tz="Europe/London")
                           return(dt)
                         })
    dt.exit <- tryCatch(dmy_hms(df$Exit.time,tz="Europe/London"),
                     warning = function(w) {
                       print(w)
                       dt <- dmy_hm(df$Entry.time,tz="Europe/London")
                       return(dt)
                     })
    df$Entry.time <- dt.entry
    df$Exit.time <- dt.exit
    files.contents <- rbind(files.contents,df)
  }
  # save debug/summary
  write.csv(files.contents,file="all-trades.csv")
  return(files.contents)
}


# --------------------------------------------------------------------
#  convert quantity to USD, return extended trade data.frame
# --------------------------------------------------------------------
make.trades.USD <- function(df, reval) {
  Quantity.USD <- rep(0,nrow(df))
  # loop over trades converting returns to USD
  for (i in 1:nrow(df)) {
    ccy1 <- substr(df[i,"Instrument"],1,3)
    ccy2 <- substr(df[i,"Instrument"],4,6)
    conv.pair <- ""
    if (ccy1 == "USD") {
      to.USD <- 1/df[i,"Exit.price"]
      conv.pair <- paste0(ccy2,ccy1)
    } else if (ccy2 == "USD") {
      to.USD <- 1
      conv.pair <- paste0(ccy1,ccy2)
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
        conv.pair <- ccypair1
      } else if (ccypair2 %in% colnames(reval)) {
        to.USD <- 1/reval[dt,ccypair2]
        conv.pair <- ccypair2
      } else {
        stop(paste("Currency pair",ccy1,ccy2,"not found in reval file",sep=" "))
      } 
    }
    Quantity.USD[i] <- coredata(to.USD) * df[i,"Quantity"]
  }
  # make numerical field for trade direction
  long.short <- rep(0,nrow(df))
  long.short[which(df$Market.pos == "Long")] <- 1
  long.short[which(df$Market.pos == "Short")] <- -1
  df.usd <- cbind(df,Quantity.USD,long.short)
  return(df.usd)  
}

# --------------------------------------------------------------------
# pnl calculation
# --------------------------------------------------------------------
calc.returns <- function(trades.USD, daterange) {
  df <- trades.USD
  dt <- trades.USD$Exit.time
  if (!is.POSIXt(dt)) {
    dt <- ymd_hms(dt,tz="Europe/London")
  }
  rtns <- (df$Exit.price - df$Entry.price) * df$Quantity.USD * df$long.short
  rtns.xts <- xts(rtns,dt)
  colnames(rtns.xts) <- "APTIC"
  # filter returns outside the range
  idx <- which(as.Date(dt) < as.Date(daterange)[1] | as.Date(dt) > as.Date(daterange)[2])
  print(daterange)
  print(class(daterange))
  cat("inside calc.returns:",idx,"\n")
  rtns.xts <- rtns.xts[-idx]
  return(rtns.xts)
}

# --------------------------------------------------------------------
#  expenses Calculation
# --------------------------------------------------------------------
calc.net.rtns <- function(rtns.monthly, mgt.fee=0.02, perf.fee=0.20) {
  # subtract management fee
  rtns.net <- rtns.monthly - mgt.fee/12*rtns.monthly
  # calculate highwatermark without fees
  hw.mark <- xts(rep(0,length(rtns.net)),index(rtns.net))
  nav <- cumsum(rtns.net)
  for (i in 2:length(rtns.net)) {
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

calc.open.pos <- function(trades.USD) {
  # find trades which straddle month ends
  idx.split <- which(mday(trades.USD$Entry.time) != mday(trades.USD$Exit.time))
  trades.split <- trades.USD[idx.split,]
  # make separate xts objects for entries and exits
  amounts <- trades.split$Quantity.USD * trades.split$long.short
  entries.split <- as.Date(as.character(trades.split$Entry.time))
  exits.split <- as.Date(trades.split$Exit.time)
  opens <- xts(amounts, entries.split)
  closes <- xts(amounts, exits.split)
  # accumulate over months
  open.close <- merge(opens,closes,fill=0)
  open.pos.monthly <- apply.monthly(open.close$opens - open.close$closes,sum)
  return(open.pos.monthly)
}


  
