library(shiny)

# declare the UI
shinyUI(pageWithSidebar(
  headerPanel("APTIC Live P&L"),
  
  # ----------------------------------------------------------------------------------------------  
  #  input panel with customisations
  # ----------------------------------------------------------------------------------------------  
  sidebarPanel(
    HTML('<style type="text/css">
     .row-fluid .span4{width: 20%;}
     </style>'),
    textInput(
      'directory1', 
      'Enter portfolio path', 
      value = if (grepl("apple",R.version[1],fixed=TRUE)) {
        "/Users/keiran/Dropbox/workspace/APTIC PnL Tool/CRNCY_Trade File_Model"
      } else {
        "E:/Cloud Data/Published Returns/Global Currency Program/CRNCY_31 Dec 2013/CRNCY_Trade File_Model"
      }
    ),
    textInput(
      'directory2', 
      'Enter portfolio path', 
      value = if (grepl("apple",R.version[1],fixed=TRUE)) {
        "/Users/keiran/Dropbox/workspace/APTIC PnL Tool/CMDTY_Trade File_Model"
      } else {
        "E:/Cloud Data/Published Returns/Global Commodity Program/Dec 31_2013/CMDTY_Trade File_Model"
      }
    ),    

    br(),
    
    textInput(
      'reval.dir',
      'Enter path to reval rates',
      value = if (grepl("apple",R.version[1],fixed=TRUE)) {
        "/Users/keiran/Dropbox/workspace/APTIC PnL Tool/Revaluation rates"
      } else {
        "E:/Cloud Data/Data History/Revaluation rates"
      }
    ),
    checkboxInput("reload","Force reload of trade files?",value=FALSE),
    
    tags$hr(),
    
    dateRangeInput("daterange","Date range:",start="2010-01-01",end="2013-12-31"),
    br(),
    numericInput("mgtFee","Management Fee (%):",min=0,max=5,step=0.1,value=2),
    br(),
    numericInput("performanceFee","Performance Fee (%):",min=0,max=30,step=1,value=20),
    br(),
    numericInput("AUM",label="AUM (million USD)",value=100,min=10,max=100,step=10),

    tags$hr(),
    radioButtons("compound",
                 label="Monthly Reinvestment or Additive PnL?",
                 choices=list("Compound"=TRUE,"Additive"=FALSE),
                 selected="Compound"),
    
    selectInput("ccyPair",
                label="Choose Currency pair to view",
                choices=c("AUDCAD", "AUDJPY", "AUDUSD", "EURAUD", "EURCAD", "EURJPY", "EURUSD",
                          "GBPAUD", "GBPJPY", "GBPUSD", "NZDUSD", "USDCAD", "USDCHF", "USDJPY",
                          "USDSGD", "XAUUSD","all"),
                selected="all")
  ),
  
  # ----------------------------------------------------------------------------------------------  
  # main panel with tabs
  # ----------------------------------------------------------------------------------------------  
  mainPanel(
    tabsetPanel(
            
      tabPanel( 
        title = "Trades",
        h3("Combined trade file"),
        h5("(including end-of-month carry-over trades)"),
        dataTableOutput('TradesExtended'),
        br(),
        div(align="center", downloadButton('downloadTrades', 'Download Trades'))
      ),
      
      tabPanel( title = "P&L",
                plotOutput("pnl"),
                div(align='right',
                    downloadButton('downloadPnLchart', 'Save Chart'),
                    downloadButton('downloadPnLdata', 'Save Data'))
      ),
      
      tabPanel( title = "NAV",
                plotOutput("nav"),
                div(align='right',
                    downloadButton('downloadNAVchart', 'Save Chart'),
                    downloadButton('downloadNAVdata', 'Save Data'))
    ),
      
      tabPanel( title = "Open Positions",
                plotOutput("openpos"),                
                div(align='right',
                    downloadButton('downloadOpenPoschart', 'Save Chart'),
                    downloadButton('downloadOpenPosdata', 'Save Data'))
      )
      
    )  
  )
))