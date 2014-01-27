library(shiny)

shinyUI(pageWithSidebar(
  headerPanel("APTIC Live P&L"),
  
  sidebarPanel(
    HTML('<style type="text/css">
     .row-fluid .span4{width: 20%;}
     </style>'),
    textInput('directory1', 
              'Enter portfolio path', 
              "E:/Cloud Data/Published Returns/Global Currency Program/CRNCY_31 Dec 2013/CRNCY_Trade File_Model"),
    
    textInput('directory2', 
              'Enter portfolio path', 
              "E:/Cloud Data/Published Returns/Global Commodity Program/Dec 31_2013/CMDTY_Trade File_Model"),
    
    tags$hr(),
    textInput('reval.dir',
              'Enter path to reval rates',
              'E:/Cloud Data/Data History/Revaluation rates'),
    
    tags$hr(),
    dateRangeInput("daterange","Date range:",start="2010-01-01",end="2013-12-31"),
    br(),
    numericInput("mgtFee","Management Fee (%):",min=0,max=5,step=0.1,value=2),
    br(),
    numericInput("performanceFee","Performance Fee (%):",min=0,max=30,step=1,value=20),
    br(),
    numericInput("AUM",label="AUM (million USD)",value=100,min=10,max=100,step=10),
    br(),
    checkboxInput("reload","Reload Trade Files?",value=FALSE),
    submitButton("Go")
  ),
  
  mainPanel(
    tabsetPanel(
      
#       tabPanel(
#         title = "Trade Files",
#         textOutput('directory1'),
#         textOutput('directory2'),
#         tags$hr(),
#         downloadButton('downloadTrades', 'Download Trades'),
#         tags$hr(),
#         tableOutput('trade.files'),
#         br()
#       ),
      
      tabPanel( 
        title = "Trades",
        h3("First 5 lines of combined trade file"),
        tableOutput('TradeSummary'),
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