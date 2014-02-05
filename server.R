options(warn=0,error=recover)
Sys.setenv(TZ="Europe/London")
library(shiny)
library(quantmod)
library(lubridate)
library(PerformanceAnalytics)
# library(dplyr)

source("unreactive.R")

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
      write.zoo(rtns, file)
    }
  )
  # save pnl chart
  output$downloadPnLchart <- downloadHandler(
    filename = function() {
      paste('PnL ', input$ccyPair,' ', Sys.Date(), '.png', sep='')
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
      paste('NAV ', input$ccyPair,' ', Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      rtns <- get.net.returns()
      nav <- actual.aum() + cumsum(rtns)
      write.zoo(nav, file)
    }
  )
  # save NAV chart
  output$downloadNAVchart <- downloadHandler(
    filename = function() {
      paste('NAV ', input$ccyPair,' ', Sys.Date(), '.png', sep='')
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
    filename = function() paste('OpenPos ', input$ccyPair,' ', Sys.Date(), '.csv', sep=''),
    content = function(file) {
      op <- get.open.positions()
      x <- input$ccyPair
      if (x != "all") {
        op.selected <- op[[x]]
        op.display <- op.selected[,3:4]
        write.zoo(op.display, file)
      }
    }
  )
  # save open pos chart
  output$downloadOpenPoschart <- downloadHandler(
    filename = function() {
      paste('OpenPos ',input$ccyPair," ",Sys.Date(), '.png', sep='')
    },
    content = function(file) {
      png(file)
      print(plotOpenPos())
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
    main.txt <- paste("Growth of $1 invested in",input$ccyPair,"strategies (net of fees)"," ")
    chart.TimeSeries(pnl,date.format="%b-%Y",main=main.txt,xlab="",ylab="")
  })

  # NAV
  plotNAV <- reactive({
    nav <- get.nav()
    main.txt <- paste("NAV of",input$ccyPair,"strategies (net of fees)"," ")
    y.txt <- "million USD"
    chart.TimeSeries(nav/1.e6,date.format="%b-%Y",main=main.txt,xlab="",ylab=y.txt)
  })

  # Open positions
  plotOpenPos <- reactive({
    op <- get.open.positions()
    x <- input$ccyPair
    if (x != "all") {
      op.selected <- op[[x]]
#       print("---> inside plotOpenPos")
#       print(op.selected)
      op.display <- op.selected[,3]
      main.txt <- paste("Month End Open Positions:",x,sep=" ")
      if (max(abs(op.display)) > 1.e6) {
        y.txt <- paste("million",get.ccy1(),sep=" ")
        vs <- 1.e6
      } else {
        y.txt <- paste("thousand",get.ccy1(),sep=" ")
        vs <- 1.e3
      }
      chart.TimeSeries(op.display/vs,type="h",date.format="%b-%Y",main=main.txt,ylab=y.txt,xlab="")
    }
    })

  # ------------------------------------------------------------------
  #  read in the required data, possibly cached in local files
  # ------------------------------------------------------------------
  
  # get the reval data
  get.reval.rates <- reactive({
    if (input$reload) {
      dat <- load.reval.files(input$reval.dir, input$daterange)
    } else {
      dat <- tryCatch(
        read.saved.reval(),
        error = function(e) {
          print(e)
          dat <- load.reval.files(input$reval.dir, input$daterange)
          return(dat)
        }
      )
    }
    return(dat)
  })
  
  # get the trade data
  get.all.trades <- reactive({
    if (input$reload) {
      files.to.load <- find.all.trade.files()
      dat <- load.all.trades(files.to.load)
    } else {
      dat <- tryCatch(
        read.saved.trades(),
        error = function(e) { 
          print(e)
          files.to.load <- find.all.trade.files()
          dat <- load.all.trades(files.to.load)
          return(dat)
        }
      )
    }
    return(dat)
  })

  # find all trade files 
  find.all.trade.files <- reactive({
    c(list.files(path=input$directory1,pattern="*.csv",full.names=TRUE,recursive=TRUE),
      list.files(path=input$directory2,pattern="*.csv",full.names=TRUE,recursive=TRUE))
  })

  # ------------------------------------------------------------------
  #  wrapper to cache the extended trade and pnl calculations
  # ------------------------------------------------------------------
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
    f.name <- "cache/trades_extended_pnl.csv"
    if (file.exists(f.name) && !input$reload) {
      trades.pnl <- read.saved.extended.pnl()
      rtns <- calc.returns(trades.pnl, input$daterange, input$ccyPair)
    } else {
      rtns <- get.returns()
    }
    return(rtns)
  })

  # ------------------------------------------------------------------
  #  calculations on the trade data
  # ------------------------------------------------------------------
  
  # get trades, convert amounts into USD
  get.trades.usd <- reactive({
    trade.data <- get.all.trades()
    reval.rates <- get.reval.rates()
    trades.usd <- make.trades.USD(trade.data, reval.rates)
    return(trades.usd)
  })
  
  # get trades, split end-of-month trades, calculate pnl for each trade
  get.trades.extended <- reactive({
    trades.usd <- get.trades.usd.cached()
    reval.rates <- get.reval.rates()
    trades.extended <- split.trades.at.month.ends(trades.usd, reval.rates)
    trades.pnl <- calc.pnl(trades.extended, reval.rates)
    return(trades.pnl)
  })
  
  # get returns from extended trade dataframe
  get.returns <- reactive({
    trades.pnl <- get.trades.extended.cached()
    rtns <- calc.returns(trades.pnl, input$daterange, input$ccyPair)
    return(rtns)    
  })

  # make returns monthly and subtract fees
  # NOTE: this is the entry point from the UI, 
  #     : ie first reactive function called from pnl and nav tabs
  get.net.returns <- reactive({
    rtns <- get.returns.cached()
    rtns.monthly <- apply.monthly(rtns,sum)
    index(rtns.monthly) <- as.yearmon(index(rtns.monthly))
    fees <- actual.fees()
    print(paste("Fees = ",fees,sep=" "))
    rtns.net <- calc.net.rtns(rtns.monthly, mgt.fee=fees$mgt, perf.fee=fees$perf,aum=actual.aum(),cmpd=input$compound )
    return(rtns.net)    
  })

  # functions to gnerate plottable data
  get.pnl <- reactive({
    rtns.percent <- get.net.returns()/actual.aum()
    if (input$compound) {
      pnl <- cumprod(1+rtns.percent)
    } else {
      pnl <- 1 + cumsum(rtns.percent)
    }
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

  # extract major and minor currencies
  get.ccy2 <- reactive({
    if (input$ccyPair == "all") {
      ccy <- "USD"
    } else {
      ccy <- substr(input$ccyPair,4,6)
    }
    return(ccy)
  })
  
  get.ccy1 <- reactive({
    if (input$ccyPair == "all") {
      ccy <- "USD"
    } else {
      ccy <- substr(input$ccyPair,1,3)
    }
    return(ccy)
  })
  
  
  
})