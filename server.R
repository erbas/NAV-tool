options(warn=0,error=dump.frames)
Sys.setenv(TZ="Europe/London")
library(shiny)
library(quantmod)
library(lubridate)
library(PerformanceAnalytics)

source("unreactive.R")
# options(warn=2)

# define the shiny server 
shinyServer(function(input, output) {

  # ------------------------------------------------------------------
  #  output elements
  # ------------------------------------------------------------------
  # trio of debug outputs
  output$directory1 <- renderText({
    input$directory1
  })
  output$directory2 <- renderText({
    input$directory2
  })
  output$reval.dir <- renderText({
    input$reval.dir
  })
  
  output$dates <- renderText({
    as.Date(input$daterange)
  })

  output$instruments <- renderText({
    input$ccyPairs
  })
  
  # list files found
  output$trade.files <- renderText({
    f.list <- find.all.trade.files()
    paste(f.list,collapse="<br><br>")
  })
  
  # show the concatenated file contents
  output$TradesExtended <- renderDataTable({
    get.trades.extended.cached()
  })


  # download function to save the raw trades
  output$downloadTrades <- downloadHandler(
    filename = function() {
      paste('All Trades ', Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      write.csv(get.trades.extended.cached(), file)
    }
  )
  
  # display pnl
  output$pnl <- renderPlot({
    plotPnL()
  })
  # save pnl data
  output$downloadPnLdata <- downloadHandler(
    filename = function() paste('PnL ', Sys.Date(), '.csv', sep=''),
    content = function(file) {
      rtns <- get.net.returns()
      index(rtns) <- as.Date(index(rtns))
      write.zoo(rtns, file, sep=",")
    }
  )
  # save pnl chart
  output$downloadPnLchart <- downloadHandler(
    filename = function() {
      paste('PnL ',' ', Sys.Date(), '.png', sep='')
    },
    content = function(file) {
      png(file)
      print(plotPnL())
      dev.off()
    },
    contentType = "image/png"
  )

  # display NAV
  output$nav <- renderPlot({
    plotNAV()
  })
  # download function to save the NAV data
  output$downloadNAVdata <- downloadHandler(
    filename = function() {
      paste('NAV ',' ', Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      rtns <- get.net.returns()
      nav <- actual.aum() + cumsum(rtns)
      index(nav) <- as.Date(index(nav))
      write.zoo(nav, file, sep=",")
    }
  )
  # save NAV chart
  output$downloadNAVchart <- downloadHandler(
    filename = function() {
      paste('NAV ',' ', Sys.Date(), '.png', sep='')
    },
    content = function(file) {
      png(file)
      print(plotNAV())
      dev.off()
    },
    contentType = "image/png"
  )

  # display open positions chart
  output$openpos <- renderPlot({    
    plotOpenPos()
  })
  # download function to save the OpenPositions data
  output$downloadOpenPosdata <- downloadHandler(
    filename = function() paste('OpenPos', ' ', Sys.Date(), '.csv', sep=''),
    content = function(file) {
      op <- get.open.positions()
      op.display <- NULL
      for (x in input$ccyPairs) {
        op.selected <- op[[x]]
        index(op.selected) <- as.Date(index(op.selected))
        op.df <- data.frame("date"=index(op.selected),"ccy"=x,"Major Ccy"=op.selected[,1],"USD"=op.selected[,2])
        op.display <- rbind(op.display, op.df)
      }
      write.zoo(op.display, file,sep=",")
    }
  )
  # save open pos chart
  output$downloadOpenPoschart <- downloadHandler(
    filename = function() {
      paste('OpenPos '," ",Sys.Date(), '.png', sep='')
    },
    content = function(file) {
      png(file)
      print(plotOpenPos())
      dev.off()
    },
    contentType = "image/png"
  )
  
  # statistics 
  output$statistics <- renderTable({
    rtns.percent <- get.net.returns()/actual.aum()
    calc.stats(rtns.percent)
  })
  # download function to save the statistics table
  output$downloadStats <- downloadHandler(
    filename = function() {
      paste('Stats',' ', Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      rtns.percent <- get.net.returns()/actual.aum()
      ptf.stats <- calc.stats(rtns.percent)
      write.csv(ptf.stats, file)
    }
  )

  # win/loss ratios
  output$ratios <- renderTable({
    trades.pnl <- get.trades.extended.cached()
    df <- calc.ratios(trades.pnl, input$ccyPairs)
    df
  })
  
  # display durations chart
  output$durations <- renderPlot({    
    plotDurations()
  })
  # save durations chart
  output$downloadDurationsChart <- downloadHandler(
    filename = function() {
      paste('TradeDurations '," ",Sys.Date(), '.png', sep='')
    },
    content = function(file) {
      png(file)
      print(plotDurations())
      dev.off()
    },
    contentType = "image/png"
  )
  
  # --------------------------------------
  # plots for tab panels
  # --------------------------------------
  # PnL
  plotPnL <- reactive({
    pnl <- get.pnl()
    index(pnl) <- as.yearmon(index(pnl))
    main.txt <- paste("Growth of $1 invested in",input$ccyPair,"strategies (net of fees)"," ")
    chart.TimeSeries(pnl,date.format="%b-%Y",main=main.txt,xlab="",ylab="")
  })

  # NAV
  plotNAV <- reactive({
    nav <- get.nav()
    main.txt <- paste("NAV of",input$ccyPair,"strategies (net of fees)"," ")
    y.txt <- "million USD"
    index(nav) <- as.yearmon(index(nav))
    chart.TimeSeries(nav/1.e6,date.format="%b-%Y",main=main.txt,xlab="",ylab=y.txt)
  })

  # Open positions
  plotOpenPos <- reactive({
    op <- get.open.positions()
    op.display <- NULL
    for (x in input$ccyPairs) {
      op.selected <- op[[x]]
      op.display <- rbind(op.display,op.selected[,1])
    }
    if (length(input$ccyPairs) > 1) {
      main.txt = "Warning: open positions not meaningful for more than one currency pair"
    } else {
      main.txt = paste0("Open positions: ",input$ccyPairs)
    }
    index(op.display) <- as.yearmon(index(op.display))
    chart.TimeSeries(op.display/1.e6,type="h",date.format="%b-%Y",main=main.txt,ylab="million",xlab="")
  })

  # Durations
  plotDurations <- reactive({
    trades.usd <- get.trades.usd.cached()
#     durations <- trades.usd$"Exit time" - trades.usd$"Entry time"
    durations <- as.numeric(trades.usd$Bars)*as.numeric(trades.usd$Timeframe)
    df <- data.frame(trades.usd$"Ccy pair", as.numeric(durations))
    colnames(df) <- c("ccyPair", "durations")
    df.display <- df[df$ccyPair %in% input$ccyPairs,]
    x <- df.display$durations/60/24
    n <- ifelse(length(x) > 1000,25,15)
    n <- ifelse( length(x) < n, NULL, n)
    print(n)
    print(summary(df.display))
    print(summary(x))
    if (length(x) > 0) {
      hist(x, n, xlab='days', ylab='Number of Trades',main="Trade Durations")
    } else {
      NULL
    }   
  })

  # find all trade files 
  find.all.trade.files <- reactive({
    c(list.files(path=input$directory1,pattern="*.csv",full.names=TRUE,recursive=TRUE),
      list.files(path=input$directory2,pattern="*.csv",full.names=TRUE,recursive=TRUE))
  })

  # ------------------------------------------------------------------
  #  wrapper to cache the extended trade and pnl calculations
  # ------------------------------------------------------------------
  
  get.reval.cached <- reactive({
    f.name <- "cache/reval_rates.csv"
    if (file.exists(f.name) && !input$reload) {
      df <- read.saved.reval()
    } else {
      df <- load.reval.files(input$reval.dir, input$daterange)
    }
    return(df)    
  })

  get.all.trades.cached <- reactive({
    f.name <- "cache/all_trades.csv"
    if (file.exists(f.name) && !input$reload) {
      df <- read.saved.trades()
    } else {
      files.to.load <- find.all.trade.files()
      df <- load.all.trades(files.to.load)
    }
    return(df)
  })
  
  get.trades.usd.cached <- reactive({
    f.name <- "cache/trades_usd.csv"
    if (file.exists(f.name) && !input$reload) {
      df <- read.saved.usd()
    } else {
      df <- get.trades.usd()
    }
    return(df)
    
  })
  
  get.trades.extended.cached <- reactive({
    f.name <- "cache/trades_extended.csv"
    if (file.exists(f.name) && !input$reload) {
      df <- read.saved.extended.pnl()
    } else {
      df <- get.trades.extended()
    }
    return(df)
  })
    
  get.returns.cached <- reactive({
    print("---> inside get.returns.cached ---")
    f.name <- "cache/trades_extended_pnl.csv"
    if (file.exists(f.name) && !input$reload) {
      trades.pnl <- read.saved.extended.pnl()
      rtns <- calc.returns(trades.pnl, input$ccyPairs)
    } else {
      rtns <- get.returns()
    }
#     print(head(rtns))
    print("<--- leaving get.returns.cached ---")
    return(rtns)
  })

  # ------------------------------------------------------------------
  #  calculations on the trade data
  # ------------------------------------------------------------------
  
  # get trades, convert amounts into USD
  get.trades.usd <- reactive({
    trade.data <- get.all.trades.cached()
    reval.rates <- get.reval.cached()
    trades.usd <- make.trades.USD(trade.data, reval.rates)
    return(trades.usd)
  })
  
  # get trades, split end-of-month trades, calculate pnl for each trade
  get.trades.extended <- reactive({
    trades.usd <- get.trades.usd.cached()
    reval.rates <- get.reval.cached()
    trades.extended <- split.trades.at.month.ends(trades.usd, reval.rates)
    trades.pnl <- calc.pnl(trades.extended, reval.rates)
    return(trades.pnl)
  })
  
  # get returns from extended trade dataframe
  get.returns <- reactive({
    print("---> inside get.returns ---")
    trades.pnl <- get.trades.extended.cached()
    rtns <- calc.returns(trades.pnl, input$ccyPairs)
    # filter returns outside the range
    dr <- as.Date(input$daterange)
    idx <- which(as.Date(index(rtns)) < dr[1] | as.Date(index(rtns)) > dr[2])
    if (length(idx) > 0) {
      rtns <- rtns[-idx]
    }
    print("<--- leaving get.returns ---")
    return(rtns)
  })

  # make returns monthly and subtract fees
  # NOTE: this is the entry point from the UI, 
  #     : ie first reactive function called from pnl and nav tabs
  get.net.returns <- reactive({
    print("-->> inside get.net.returns <--")
    rtns <- get.returns()
    reval.rates <- get.reval.cached()
    eom.revals <- get.ends.of.months(reval.rates)
    rtns.monthly <- my.apply.monthly(rtns, index(eom.revals))
    index(rtns.monthly) <- as.Date(index(rtns.monthly))
    fees <- actual.fees()
    rtns.net <- apply.fees(rtns.monthly, mgt.fee=fees$mgt, perf.fee=fees$perf,aum=actual.aum() )
    print("<<-- leaving get.net.returns -->")    
    return(rtns.net)    
  })

  # functions to gnerate plottable data
  get.pnl <- reactive({
    print("---> inside get.pnl ---")
    rtns.percent <- get.net.returns()/actual.aum()
    if (input$compound) {
      pnl <- cumprod(1+rtns.percent)
    } else {
      pnl <- 1 + cumsum(rtns.percent)
    }
    print("<--- leaving get.pnl ---")
    return(pnl)
  })

  get.nav <- reactive({
    rtns <- get.net.returns()
    if (input$compound) {
      nav <- cumprod(1 + rtns/actual.aum())*actual.aum()
    } else {
      nav <- actual.aum() + cumsum(rtns)
    }
  return(nav)
  })

  # get month end open positions for all ccy pairs
  # NOTE: this is the entry point from the UI 
  get.open.positions <- reactive({
    trades <- get.trades.extended.cached()
    op <- calc.open.pos(trades, input$daterange)
    return(op)
  })
  
  # ------------------------------------------------------------------
  # utility functions
  # ------------------------------------------------------------------
  # convert AUM from gui input value into units of $1
  actual.aum <- reactive({
    return(input$AUM*1.e6)
  })

  # convert fees from gui input values into percentages
  actual.fees <- reactive({
    return(list("mgt"=input$mgtFee/100, "perf"=input$performanceFee/100))
  })

})