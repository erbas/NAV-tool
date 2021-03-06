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
        "/Users/keiran/Dropbox/workspace/APTIC PnL Tool/CRNCY_Trade File_Model/Sub Strategy/"
      } else {
        "C:/Users/Andrew Pether/Documents/MODEL_TRADES/CRNCY_Trade File_Model/Sub Strategy/"
      }
    ),
    textInput(
      'directory2', 
      'Enter portfolio path', 
      value = if (grepl("apple",R.version[1],fixed=TRUE)) {
        "/Users/keiran/Dropbox/workspace/APTIC PnL Tool/CMDTY_Trade File_Model/Sub Strategy/Gold"
      } else {
        "C:/Users/Andrew Pether/Documents/MODEL_TRADES/CMDTY_Trade File_Model/Gold/"
      }
    ),    

    br(),
    
    textInput(
      'reval.dir',
      'Enter path to reval rates',
      value = if (grepl("apple",R.version[1],fixed=TRUE)) {
        "/Users/keiran/Dropbox/workspace/APTIC PnL Tool/Revaluation rates"
      } else {
        "C:/Users/Andrew Pether/Documents/Data History/Revaluation rates"
      }
    ),
    checkboxInput("reload","Force reload of trade files?",value=FALSE),
    
    tags$hr(),
    
    dateRangeInput("daterange","Date range:",start="2015-01-01",end="2015-12-31"),
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
    
    checkboxGroupInput("ccyPairs",
                label="Select currency pairs",
                choices=c("AUDCAD", "AUDJPY", "AUDUSD", "EURAUD", "EURCAD", "EURJPY", "EURUSD",
                          "GBPAUD", "GBPJPY", "GBPUSD", "NZDUSD", "USDCAD", "USDCHF", "USDJPY",
                          "USDSGD", "XAUUSD", "XAGUSD"),
                selected = "XAUUSD"
#                 selected=c("AUDCAD", "AUDJPY", "AUDUSD", "EURAUD", "EURCAD", "EURJPY", "EURUSD",
#                            "GBPAUD", "GBPJPY", "GBPUSD", "NZDUSD", "USDCAD", "USDCHF", "USDJPY",
#                            "USDSGD", "XAUUSD")
                )
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
#                 textOutput("instruments"),
                plotOutput("pnl"),
                div(align='right',
                    downloadButton('downloadPnLchart', 'Save Chart'),
                    downloadButton('downloadPnLdata', 'Save Data'))
      ),
      
      tabPanel( title = "NAV",
#                 h4(textOutput("instruments")),
                plotOutput("nav"),
                div(align='right',
                    downloadButton('downloadNAVchart', 'Save Chart'),
                    downloadButton('downloadNAVdata', 'Save Data'))
      ),
      
      tabPanel( title = "Open Positions",
#                 h4(textOutput("instruments")),
                plotOutput("openpos"),                
                div(align='right',
                    downloadButton('downloadOpenPoschart', 'Save Chart'),
                    downloadButton('downloadOpenPosdata', 'Save Data'))
      ),
    
      tabPanel( title = "Statistics",
#                 h4(textOutput("instruments")),
                tableOutput("statistics"),
                tableOutput("ratios"),
                div(align='center',
                    downloadButton('downloadStats', 'Save Stats'))
      ),

      tabPanel( title = "Trade Durations",
          plotOutput("durations"),                
          div(align='center',
              downloadButton('downloadDurationsChart', 'Save Chart'))
)

      
    )  
  )
))