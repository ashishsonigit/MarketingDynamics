# Title: "Marketing Dynamics - Adoption of new product/innovation"
# Author: Ashish Soni, Principal Data Scientist & Strategy Consultant

if (!require('shiny')) install.packages('shiny'); library('shiny')
if (!require('shinydashboard')) install.packages('shinydashboard'); library('shinydashboard')

## Clear the environment
rm(list=c(ls()))

options(shiny.reactlog=TRUE)

################################################################################
## Load libraries 

if (!require('deSolve')) install.packages('deSolve'); library('deSolve') 
if (!require('gridExtra')) install.packages('gridExtra'); library('gridExtra')
if (!require('FME')) install.packages('FME'); library('FME')
if (!require('ggplot2')) install.packages('ggplot2'); library('ggplot2')
if (!require('plotly')) install.packages('plotly'); library('plotly')
if (!require('scales')) install.packages('scales'); library('scales')
if (!require('plyr')) install.packages('plyr'); library('plyr')

################################################################################
## Load functions for shiny ui from R folder

source("R/Model.R")
source("R/initParam.R")
source("R/simulate.R")


  header<- dashboardHeader(title = span(img(src="main_logo.jpg",
                                            height = 70,
                                            width = 70*1.445,
                                            style = "margin:-55px -15px"
                                            )))
                            # "Marketing Dynamics - Adoption of new product/innovation",titleWidth = 650)
  
  sidebar<-dashboardSidebar(
                        sidebarMenu(
                          menuItem("Introduction", tabName = "Introduction",icon=icon("info-sign",lib = "glyphicon")),
                          menuItem("Marketing Dynamics", tabName = "Dynamics",icon = icon("bar-chart-o")),
                          menuItem("Simulation", tabName = "Simulation",icon = icon("table"))
                        )
            )
  
 BusinessModelUI<-  fluidRow(
          box(title = "Business Model ",status = "primary",width = 10
              ,solidHeader = TRUE 
              ,collapsible = TRUE 
              ,tags$img(src = 'MarketingDynamics_v1.png',height=450,width=550))
          #box(verbatimTextOutput("stats"))
        )
  
  DynamicModelUI<-  fluidRow(
        box(title = "Scenario 1",status = "primary",width = 12
            ,solidHeader = TRUE 
            ,collapsible = TRUE,
             box(title = "Potential vs True Adopters",status = "primary"
                 ,solidHeader = TRUE 
                 ,collapsible = TRUE
                 ,plotOutput("plotSystemStocks1")),
             box(title = "Adoption/Discard Rates",status = "primary"
                 ,solidHeader = TRUE 
                 ,collapsible = TRUE
                 ,plotOutput("plotSystemFlow1")),
            box(title = "Adoption Rate - Decomposition",status = "primary",width = 12
                ,solidHeader = TRUE 
                ,collapsible = TRUE
                ,plotOutput("plotAdoptFlowAnalysis1")),
             box(title = "Controls",status = "primary",width = 12
                 ,solidHeader = TRUE 
                 ,collapsible = TRUE
                 ,box(sliderInput("c1", "Contact Rate", min=100, max=500, value=100, step=1)),
                  box(sliderInput("i1", "Adoption Fraction", min=0.01, max=0.05, value=0.015, step=0.005)),
                  box(sliderInput("a1",  "Advertising Effectivness", min=0.01, max=2.5, value=0.01, step=0.05)),
                  box(sliderInput("T1",  "Average Product Life", min=1, max=10, value=2, step=1))
                 ,box(sliderInput("InitAdopters1", "Initial Adopters", min=10, max=1000, value=100, step=10)),
                  box(sliderInput("N1",  "Total Population", min=1000000, max=10000000, value=1000000, step=50000))
             ))
        # ,
        # box(title = "Scenario 2",status = "primary"
        #     ,solidHeader = TRUE 
        #     ,collapsible = TRUE,
        #     box(title = "Potential vs True Adopters",status = "primary"
        #         ,solidHeader = TRUE 
        #         ,collapsible = TRUE
        #         ,plotOutput("plotSystemStocks2")),
        #     box(title = "Adoption/Discard Rates",status = "primary"
        #         ,solidHeader = TRUE 
        #         ,collapsible = TRUE
        #         ,plotOutput("plotSystemFlow2")),
        #     box(title = "Adoption Rate - Decomposition",status = "primary",width = 12
        #         ,solidHeader = TRUE 
        #         ,collapsible = TRUE
        #         ,plotOutput("plotAdoptFlowAnalysis2")),
        #     box(title = "Controls",status = "primary",width = 12
        #         ,solidHeader = TRUE 
        #         ,collapsible = TRUE
        #         ,box(sliderInput("c2", "Contact Rate", min=100, max=500, value=100, step=1)),
        #         box(sliderInput("i2", "Adoption Fraction", min=0.01, max=0.05, value=0.015, step=0.005)),
        #         box(sliderInput("a2",  "Advertising Effectivness", min=0.01, max=2.5, value=0.01, step=0.05),
        #             sliderInput("timeinstant",  "Time of improved Advertising Effectivness", min=0, max=20, value=10, step=1)
        #             ),
        #         box(sliderInput("T2",  "Average Product Life", min=1, max=10, value=2, step=1))
        #         ,box(sliderInput("InitAdopters2", "Initial Adopters", min=10, max=1000, value=100, step=10)),
        #         box(sliderInput("N2",  "Total Population", min=1000000, max=10000000, value=1000000, step=50000))
        #     ))
  )
  
  SimulateUI<-   fluidRow(  
                 box(title = "Simulate paramter over different business scenarios",
                      status = "primary",width = 12
                      ,solidHeader = TRUE 
                      ,collapsible = TRUE
                      ,selectInput("param", "Parameter to simulate :",characterlist)
                      ,sliderInput("NRUNS", "Simulation Runs", min=10, max=300, value=100, step=10)
                  ),
                 box(title = "Simulation",status = "primary",width = 12
                     ,solidHeader = TRUE 
                     ,collapsible = TRUE
                     ,plotlyOutput("SensitivityPlot_plotly")
                     ,box(title = "Simulation based Optimisation:"
                          ,status = "primary"
                          ,solidHeader = TRUE 
                          ,collapsible = TRUE
                          ,verbatimTextOutput("Optimum"))
                     ),
                 numericInput("maxrows", "Rows to show", 25),
                 verbatimTextOutput("rawtable"),
                 downloadButton("downloadCsv", "Download as CSV")
              )
  
   # f4 <- fluidRow(
   #    infoBoxOutput("sourceBox", width = 8)
   #   ,infoBoxOutput("nameBox", width = 4)
   # )
   
     
body <- dashboardBody(
  tabItems(
    tabItem(tabName = "Introduction",
            BusinessModelUI
    ),
    tabItem(tabName = "Dynamics",
            DynamicModelUI
          ),
    tabItem(tabName = "Simulation",
            SimulateUI
    )
  )
)

ui <- dashboardPage(skin="yellow",header, sidebar, body)
  

server <- function(input, output,session) {
    data1 <- reactive({
      cat(file=stderr(), "Function data (reactive function)...\n")
      START<-0; FINISH<-20; STEP<-0.125
      simtime <- seq(START, FINISH, by=STEP)
      stocks  <- c(sPotentialAdopters=input$N1-input$InitAdopters1,sAdopters=input$InitAdopters1)
      
      # exogenous parameters for the marketing model
      auxs    <- c(aTotalPopulation=as.numeric(input$N1), 
                   aAdvEffectiveRate = as.numeric(input$a1),
                   aEffectiveContactRate=as.numeric(input$c1), 
                   aAdoptionFraction=as.numeric(input$i1),
                   aProductLife=as.numeric(input$T1))
      
      o<-data.frame(ode(y=stocks, times=simtime, func = model1, 
                        parms=auxs, method="euler"))
      
      
    })
    
    data2 <- reactive({
      cat(file=stderr(), "Function data (reactive function)...\n")
      START<-0; FINISH<-20; STEP<-0.125
      simtime <- seq(START, FINISH, by=STEP)
      stocks  <- c(sPotentialAdopters=input$N2-input$InitAdopters2,sAdopters=input$InitAdopters2)
      
      # exogenous parameters for the marketing model
      auxs    <- c(aTotalPopulation=as.numeric(input$N2), 
                   aAdvEffectiveRate = as.numeric(input$a2),
                   aEffectiveContactRate=as.numeric(input$c2), 
                   aAdoptionFraction=as.numeric(input$i2),
                   aProductLife=as.numeric(input$T2),
                   timeinstant=as.numeric(input$timeinstant)
                   )
      
      
      o<-data.frame(ode(y=stocks, times=simtime, func = model2,
                        parms=auxs, method="euler"))
      
      # in case we want to input shocks or step functions on the stock parameters
      # eventsdat<-data.frame(var=c("<stockparname>"),time=c(5), value=c(0.015),method=c("add"))
      # 
      # o<-data.frame(ode(y=stocks,
      #                   times=simtime,
      #                   func = model,
      #                   events = list(data=eventsdat),
      #                   parms=auxs, method="euler"))
    })
  
 
    output$plotSystemStocks1 <- renderPlot({
      cat(file=stderr(), "Function output$plot::renderPlot...\n")
      
      o <- data1()
      ggplot()+
        geom_line(data=o,aes(time,o$sPotentialAdopters,color="1. Potential Adopters"))+
        geom_line(data=o,aes(time,o$sAdopters,color="2. Adopters"))+
        scale_y_continuous(labels = comma)+
        ylab("System Stocks")+
        xlab("Months") +
        labs(color="")+
        theme(legend.position="bottom")
      
    })
    
    
    output$plotSystemFlow1 <- renderPlot({
      cat(file=stderr(), "Function output$plot::renderPlot...\n")
      
      o <- data1()
      
      ggplot()+
        geom_line(data=o,aes(time,o$AR,color="1. Adoption Rate"))+
        geom_line(data=o,aes(time,o$DR,color="2. Discard Rate"))+
        scale_y_continuous(labels = comma)+
        ylab("System Flows")+
        xlab("Months") +
        labs(color="")+
        theme(legend.position="bottom")
      
    })
    
    output$plotAdoptFlowAnalysis1 <- renderPlot({
      cat(file=stderr(), "Function output$plot::renderPlot...\n")
      
      o <- data1()
      
      ggplot()+
        geom_line(data=o,aes(time,o$AR,color="1. Adoption Rate"))+
        geom_line(data=o,aes(time,o$AdopAdv,color="2. Adoption Rate - Advertising"),linetype="dotted",size=1.2)+
        geom_line(data=o,aes(time,o$AdopWoM,color="3. Adopters Rate - WoM"),linetype="dotted",size=1.2)+
        scale_y_continuous(labels = comma)+
        ylab("System Flows")+
        xlab("Months") +
        labs(color="")+
        theme(legend.position="bottom")
      
    })

  output$plotSystemStocks2 <- renderPlot({
    cat(file=stderr(), "Function output$plot::renderPlot...\n")
    
    o <- data2()
    ggplot()+
      geom_line(data=o,aes(time,o$sPotentialAdopters,color="1. Potential Adopters"))+
      geom_line(data=o,aes(time,o$sAdopters,color="2. Adopters"))+
      scale_y_continuous(labels = comma)+
      ylab("System Stocks")+
      xlab("Months") +
      labs(color="")+
      theme(legend.position="bottom")
    
  })
  
  output$plotSystemFlow2 <- renderPlot({
    cat(file=stderr(), "Function output$plot::renderPlot...\n")
    
    o <- data2()
    
    ggplot()+
      geom_line(data=o,aes(time,o$AR,color="1. Adoption Rate"))+
      geom_line(data=o,aes(time,o$DR,color="2. Discard Rate"))+
      scale_y_continuous(labels = comma)+
      ylab("System Flows")+
      xlab("Months") +
      labs(color="")+
      theme(legend.position="bottom")
    
  })
  
  output$plotAdoptFlowAnalysis2 <- renderPlot({
    cat(file=stderr(), "Function output$plot::renderPlot...\n")
    
    o <- data2()
    
    ggplot()+
      geom_line(data=o,aes(time,o$AR,color="1. Adoption Rate"))+
      geom_line(data=o,aes(time,o$AdopAdv,color="2. Adoption Rate - Advertising"),linetype="dotted",size=1.2)+
      geom_line(data=o,aes(time,o$AdopWoM,color="3. Adopters Rate - WoM"),linetype="dotted",size=1.2)+
      scale_y_continuous(labels = comma)+
      ylab("System Flows")+
      xlab("Months") +
      labs(color="")+
      theme(legend.position="bottom")
    
  })
  
  sim <- reactive({
    
    
    # Number of simulation runs
    # NRUNS<-200 or input$NRUNS
    # sampling method : 
    # Monte carlo (more random)
    library(msm)
    a<-rtnorm(input$NRUNS,mean(c(a.MIN,a.MAX)),sd(c(a.MIN,a.MAX)),a.MIN,a.MAX)
    
    # Latin hypercube (more even)
    p<-data.frame(Latinhyper(parRange,input$NRUNS))
    p$aAdvEffectiveRate<-a
    sensRun(p) #run simulation
    df<-rbind.fill(g.simRuns)
    
  })
  output$stats <- renderPrint({
    summary(data()[,c("sPotentialAdopters","sAdopters")])
  })
  
  output$SensitivityPlot_plotly <- renderPlotly({
    
    cat(file=stderr(), "Function output$plot::renderPlot...\n")
    
    df<-sim()
    
    y_label<-names(which(characterlist==input$param))
    
    newdf <- subset(df, sPotentialAdopters>=0 & sAdopters>=0 & AR>=0 & AdopAdv>=0 & AdopWoM>=0 & c!=451.9757 & c!=348.0216)
    df<-newdf
    newdf[, 'run'] <- as.factor(newdf[, 'run'])
    p <- ggplot(newdf,aes(x=time,y=get(input$param),color=c, group=a)) +   #sAdopters
      geom_line()+
      ylab(y_label)+
      xlab("Time(Months)") +
      guides(color=FALSE)
    
    ggplotly(p)
  })
  
  df1 <- reactive({
    # rename columns to be written into file
    df1<-rename(sim(), c("a"="AdvertisingEffectiveness", # exogenous
                         "c"="ContactRate",     # exogenous
                         "i"="AdoptionFraction",# exogenous
                         "T"="AvgProductLife",  # exogenous
                         "AR"="AdoptionRate",   # endogenous
                         "DR"="DiscardRate"))   # endogenous
    
  })
  
  
  
  output$downloadCsv <- downloadHandler(
    filename = "simulation.csv",
    content = function(file) {
      write.csv(df1(), file)
    },
    contentType = "text/csv"
  )
  
  output$Optimum <- renderPrint({
    orig <- options(width = 1000)
    df<-sim()
    val<-as.data.frame((df[which.max(df$sAdopters),]))
    optimum <- sapply(val, as.character)
    
    print(optimum)
    options(orig)
  })
  output$rawtable <- renderPrint({
    
    orig <- options(width = 1000)
    print(tail(df1(), input$maxrows))
    options(orig)
  })
  
  # # fluid row 5: source of the data
  # output$sourceBox <- renderInfoBox({
  #   infoBox(
  #     title = "Source"
  #     ,value = "Business Dynamics"
  #     ,color = "purple"
  #     ,icon = icon("tachometer")
  #   )
  # })
  # 
  # # fluid row 5: source of the data
  # output$nameBox <- renderInfoBox({
  #   infoBox(
  #     title = "Simulated"
  #     ,value = "R version of Marketing Dynamics"
  #     ,color = "purple"
  #     ,icon = icon("code")
  #   )
  # })
  
}

shinyApp(ui, server)