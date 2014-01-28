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
  
#   # show the concatenated file contents
  output$TradeSummary <- renderTable({
    df <- get.trades.usd()
    head(df,5)
  })


  # download function to save the raw trades
  output$downloadTrades <- downloadHandler(
    filename = function() {
      paste('All Trades ', Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      write.csv(get.trades.usd(), file)
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
      paste('PnL ', Sys.Date(), '.png', sep='')
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
      paste('NAV ', Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      rtns <- get.net.returns()
      write.zoo(rtns, file)
    }
  )
  # save NAV chart
  output$downloadNAVchart <- downloadHandler(
    filename = function() {
      paste('NAV ', Sys.Date(), '.png', sep='')
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
    filename = function() paste('OpenPos ', Sys.Date(), '.csv', sep=''),
    content = function(file) write.zoo(calc.open.pos(), file)
  )
  # save open pos chart
  output$downloadOpenPoschart <- downloadHandler(
    filename = function() {
      paste('OpenPos', Sys.Date(), '.png', sep='')
    },
    content = function(file) {
      png(file)
      print(plotOpenPos())
      dev.off()
    },
    contentType = "image/png"
  )
  
  
  # ------------------------------------------------------------------
  #  read in the required data
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
  
  # find all trade files 
  find.all.trade.files <- reactive({
    c(list.files(path=input$directory1,pattern="*.csv",full.names=TRUE,recursive=TRUE),
      list.files(path=input$directory2,pattern="*.csv",full.names=TRUE,recursive=TRUE))
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
  
  # ------------------------------------------------------------------
  #  calculations on the trade data
  # ------------------------------------------------------------------

  get.trades.usd <- reactive({
    trade.data <- get.all.trades()
    reval.rates <- get.reval.rates()
    trades.usd <- make.trades.USD(trade.data, reval.rates)
    return(trades.usd)
  })

  get.returns <- reactive({
    trades <- get.trades.usd()
    rtns <- calc.returns(trades, input$daterange)
    return(rtns)    
  })

  get.net.returns <- reactive({
    rtns <- get.returns()
    rtns.monthly <- apply.monthly(rtns,sum)
    index(rtns.monthly) <- as.yearmon(index(rtns.monthly))
    fees <- actual.fees()
    rtns.net <- calc.net.rtns(rtns.monthly, mgt.fee=fees$mgt, perf.fee=fees$perf )
    return(rtns.net)    
  })

  actual.aum <- reactive({
    print(attributes(input$AUM))
    return(input$AUM*1.e6)
  })

  actual.fees <- reactive({
    return(list("mgt"=input$mgtFee/100, "perf"=input$performanceFee/100))
  })
  
  get.open.positions <- reactive({
    trades <- get.trades.usd()
    op <- calc.open.pos(trades)
    return(op)
  })
  
  # --------------------------------------
  # plots for tab panels
  # --------------------------------------
  # PnL
  plotPnL <- reactive({
    rtns.percent <- get.net.returns()/actual.aum()
    pnl.m <- 1+cumsum(rtns.percent)
    chart.TimeSeries(pnl.m,date.format="%b-%Y",main="Growth of $1 (net of fees)",xlab="",ylab="")
  })

  # NAV
  plotNAV <- reactive({
    rtns <- actual.aum() + get.net.returns()/1.e6
    chart.TimeSeries(cumsum(rtns),date.format="%b-%Y",main="NAV - net of fees (million USD)",xlab="",ylab="")
  })
  
  # Open positions
  plotOpenPos <- reactive({
    op <- get.open.positions()/1.e6
    dt <- index(op)
    idx <- which(as.Date(dt) < input$daterange[1] | as.Date(dt) > input$daterange[2])
    op <- op[-idx]
    index(op) <- as.yearmon(index(op))
    chart.TimeSeries(op,type="h",date.format="%b-%Y",main="Total Month-End Open Positions (million USD)",ylab="",xlab="",lwd=2)
  })
  

})