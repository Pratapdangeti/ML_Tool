


rm(list = ls())

library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(plyr)

library(rpart)

library(CORElearn)
library(randomForest)
library(gbm)
library(e1071)
library(pROC)
library(tree)

library(caret)



ui<- dashboardPage(
  dashboardHeader(title = h4("Machine Learning Tool"),titleWidth = 300),
  dashboardSidebar(
    sidebarMenu(
      
      menuItem("Dashboard",icon = icon("dashboard"),tabName = "dashboard"),
      br(),
      menuItem("Data Import", icon = icon("table"), tabName = "Data"),
      menuItem("Data Treatment & EDA", icon = icon("list-alt"), tabName = "EDA"),
      menuItem("Machine Learning Models", icon = icon("gears"), tabName = "Model" ),
      menuItem("Model Outputs", icon = icon("bar-chart-o"), tabName = "Outputmdls")
      
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
              
              fluidRow(
                box(title = "Machine Learning Tool for Quick prototyping the Model",height = "900px",width = "620px",solidHeader = TRUE,status = "success",

                    h4(p("Welcome to Machine Learning Tool ! in which you can quickly prototype various
                         Machine Learning Models on your chosen data")),
                    br(),
                    h4(p("Objective of the site is to make Machine Learning models easy for all in an interactive way !")),
                    br(),
                    h4(p("Major steps in Machine learning models are (Click on each tab left for exploring each section) ")),


                    br(),
                  
                   h4(p(icon("table")," Data Import ")),
                  br(),
                  h4(p(icon("list-alt")," Data Treatment & Exploratory Data Analysis ")),
                  br(),
                  h4(p(icon("gears")," Model Building on Train Data")),
                  
                  br(),
                  h4(p(icon("bar-chart-o")," Model Test on Validation Data"))

                )
              
      )
      

      ),
      
      
      
      tabItem("Data",
              
              fluidRow(
                box(title = "Train Data Import",width = 4,solidHeader = TRUE, status = "primary",
                    
                    
                    fileInput('chosenfile', 'Select Train Data file ',width = "200px",
                              accept = c(
                                'text/csv','text/comma-separated-values','text/tab-separated-values',
                                'text/plain','.csv','.tsv'
                              )),
                    checkboxInput('header', 'Select Header for file with Header', TRUE)
                    
                    
                ),
                
                box(title = "Test Data Import",width = 4,solidHeader = TRUE, status = "primary",
                    
                    
                    fileInput('tchosenfile', 'Select Test Data file ',width = "200px",
                              accept = c(
                                'text/csv','text/comma-separated-values','text/tab-separated-values',
                                'text/plain','.csv','.tsv'
                              )),
                    checkboxInput('theader', 'Select Header for file with Header', TRUE)
                    
                    
                )
                
                
                ),
              
              fluidRow(
                box(title = "Show Train data",width = 12,solidHeader = TRUE,status="primary",
                    DT::dataTableOutput("tbl",width = "98%",height = "auto"))
              ),
              
              fluidRow(
                box(title = "Show Test data",width = 12,solidHeader = TRUE,status="primary",
                    DT::dataTableOutput("ttbl",width = "98%",height = "auto"))
              )
              
              
              
      ),
      
      tabItem(tabName = "EDA",
              
              fluidRow(
                tabBox(
                  title = "",
                  id = "tabset1", height = "900px",width = "620px",
                  
                  tabPanel("Missing & Outlier Treatment",icon = icon("tasks"),
                           
                           fluidRow(
                             box(title = "Select Y variable to convert (If not in correct format)",solidHeader = TRUE,status = "primary", width = 4,
                                  uiOutput("vartm"),
                                 
                                 actionButton("chkclss", "BEFORE CHECK",icon=icon("caret-square-o-right")),
                                  radioButtons("toVarOpts",label = h3("To Variable Selection"), list("integer","character","double"),selected = "character",
                                              inline = TRUE, width = "500px"),
                                 br(),
                                 
                                 actionButton("convertb", "CONVERT",icon=icon("caret-square-o-right")),
                                 br(),
                                 br(),
                                 
                                 actionButton("chkclssa", "AFTER CHECK",icon=icon("caret-square-o-right"))
                                 
                             ),
                             box(title = "Variable Type Viewer",solidHeader = TRUE,status = "primary", width = 6,
                                 #plotlyOutput("chrt")
                                 
                                 textOutput("chkvar"),
                                 textOutput("chkvara")
                                 
                             )
                             
                             
                           )
                           
                           
                  ),
                  
                  
                  tabPanel("Univariate Analysis",icon = icon("area-chart"),
                           
                           fluidRow(
                             box(title = "Univariate Analysis",solidHeader = TRUE,status = "primary", width = 4,
                                 uiOutput("selov"),
                                 verbatimTextOutput("smrysv")
                                 
                             ),
                             box(title = "Histogram plot",solidHeader = TRUE,status = "primary", width = 6,
                                 plotlyOutput("chrt")
                                 
                             )
                                 
                             
                           )
                           
                           
                  ),
                  
                  
                  
                  tabPanel(title = "Bi-Variate Analysis",icon = icon("glyphicon glyphicon-sort", lib = "glyphicon"),
                                    
                            fluidRow(
                              box(title = "Bi-variate Analysis",solidHeader = TRUE,status = "primary", width = 4,
                                  uiOutput("selbvx"),
                                  uiOutput("selbvy")
                                  
                                  
                              ),
                              box(title = "Analysis",solidHeader = TRUE,status = "primary", width = 6,
                                  verbatimTextOutput("smrybv")
                                  
                              )
                            )
                           
                          
                           
                  )
                )
              )),
      
      
      
      tabItem(tabName = "Model",
              
              fluidRow(
                tabBox(
                  title = "",
                  id = "tabset2", height = "1500px",width = "620px",
                  
                  
                  tabPanel("Model Selection",icon = icon("eye"),
                           fluidRow(
                             
                             box(title = "4.1 Select Variables",width = 4,solidHeader = TRUE,status = "primary",
                                 uiOutput("selx"),
                                 uiOutput("sely")
                             ),

                           
                           box(title = "4.2.1 Select_Classification_Model",width = 3,solidHeader = TRUE,status = "primary",
                           uiOutput("rnuic")
                           ),
                           
                           box(title = "4.2.2 Select_Regression_Model",width = 3,solidHeader = TRUE,status = "primary",
                               uiOutput("rnuir")
                           )
                           
                           
                             ),
                           
                           fluidRow(
                             box(title = "4.3 Select_Model_Parameters",width = 9,solidHeader = TRUE,status = "primary",
                                 

                                 selectInput("pselmd", label = NULL, width = "300px",
                                             choices = list("Logistic_Regression", "Linear_Regression","Decision_Tree_Classifier", "Decision_Tree_Regressor",
                                                            "Random_Forest_Classifer","Random_Forest_Regressor","Boosting_GBM_Classifier","Boosting_GBM_Regressor",
                                                            "SVM_Classifier","SVM_Regressor","Naive_Bayes","Kmeans_Clustering", "PCA" ), 
                                             selected = "Logistic_Regression"),
                                 
                                 uiOutput("rnwd")
                                 
                             ),
                             
                             box(title = "4.4 Model Run",solidHeader = TRUE,status = "success", width = 3,
                                 
                                 actionButton("actionc", "RUN CLASSIFICATION MODEL",icon=icon("caret-square-o-right")),
                                 hr(),
                                 actionButton("actionr", "RUN REGRESSION MODEL",icon=icon("caret-square-o-right"))
                                 
                             )
                            
                           ),
                           
                           
                           fluidRow(

                          box(title = "4.5 Model Results",solidHeader = TRUE,status = "success", width = 12,
                              verbatimTextOutput("accrcy"),
                              verbatimTextOutput("accrcyr")
                             
                          )
                          )
                          
                         
                  ),
                  
                  
                  
                  tabPanel("Grid Search", icon = icon("search"),
                           
                           fluidRow(
                             box(title = "Under construction",width = 3,solidHeader = TRUE,status = "primary",
                                 
                                 h4(p(icon("glyphicon glyphicon-wrench", lib = "glyphicon")," Under Construction " ))
                             )
                           )
                           )
                  )
                  
                  
                  
                  
                )),
      
      
      
      tabItem(tabName = "Outputmdls",
              
              fluidRow(
                
                box(title = "Output visualization",width = "650px",solidHeader = TRUE,status = "primary",height = "900px",
                    h4(p(icon("glyphicon glyphicon-wrench", lib = "glyphicon")," Under Construction " ))
                   # imageOutput("image5")
                )

              
      ))
      
      
    
  )
)
)



server <- function(input,output){
  
  data <- reactive({
    dfile <-
      input$chosenfile[1, 4] # <- filename with path is the [1,4] cell in obj
    if (!is.null(dfile))
      readr::read_csv(dfile,col_names = input$header)
  })
  
  tdata <- reactive({
    tdfile <-
      input$tchosenfile[1, 4] # <- filename with path is the [1,4] cell in obj
    if (!is.null(tdfile))
      readr::read_csv(tdfile,col_names = input$theader)
  })
  
  
 cnt = 0
    
 data2ck <- eventReactive(input$convertb,{

   old_data = data.frame(data())
   ipvar = input$varstomodf
   
   if (input$toVarOpts == "integer"){
     old_data[,ipvar] = as.integer(old_data[,ipvar])
   }
   else if (input$toVarOpts == "character"){
     old_data[,ipvar] = as.character(old_data[,ipvar])
   }
   
   else if(input$toVarOpts == "double"){
     old_data[,ipvar] = as.double(old_data[,ipvar])
   }
   
   cnt <<- cnt+1
   old_data

   
 })
 
 
 data2 <- reactive({
   data2ck()
 })
  
 
 intrchkvar <- eventReactive(input$chkclss,{
   xvar = input$varstomodf
   dset = data.frame(data())
   #typeof(dset[,xvar])
   str(dset[,xvar])
 })
 
 
 output$chkvar <- renderPrint({
   
   #acc = intrchkvar()
   cat("Selected variable before conversion is: \n") 
   intrchkvar()
 })
 
 intrchkvara <- eventReactive(input$chkclssa,{
   xvarn = input$varstomodf
   dset = data.frame(data2())
   #typeof(dset[,xvarn])
   str(dset[,xvarn])
 })
 
 
 output$chkvara <- renderPrint({
   cat("Selected variable after conversion is: \n") 
   intrchkvara()
  
   #acc = intrchkvara()
 })
 

  output$tbl <- renderDataTable(data(),
                                options = list(scrollX = TRUE, pageLength = 10,
                                               searching = TRUE))
  
  output$ttbl <- renderDataTable(tdata(),
                                options = list(scrollX = TRUE, pageLength = 10,
                                               searching = TRUE))
  
  output$vartm <- renderUI({
    vars <- names(data())
    selectInput("varstomodf",label = "Select Y variable to modify format", choices = names(data()),multiple = FALSE)
  })
  
  output$selov <- renderUI({
    if (cnt==0){
      vars <- names(data())
      selectInput("xvarsu",label = "Select variable to plot", choices = vars,multiple = FALSE)
    }
    else if (cnt >= 1){
      vars <- names(data2())
      selectInput("xvarsu",label = "Select variable to plot", choices = vars,multiple = FALSE)
      

    }

  })
  
  output$selbvx <- renderUI({
    vars <- names(data())
    selectInput("xvarsb",label = "Select first variable to plot", choices = vars,multiple = FALSE)
  })
  
  output$selbvy <- renderUI({
    vars <- names(data())
    selectInput("yvarsb",label = "Select second variable to plot", choices = vars,multiple = FALSE)
  })
  
  
  output$chrt <- renderPlotly({
    
    if(cnt==0){
      
      dset = data.frame(data())
      xvar = input$xvarsu
      plot_ly(x= dset[,xvar],type = "histogram")
    }
    
    else if (cnt >=1){
      dset = data.frame(data2())
      xvar = input$xvarsu
      plot_ly(x= dset[,xvar],type = "histogram")
    }
    
  })
  
  
  
  output$smrysv <- renderPrint({
    vartos = input$xvarsu
    
    if (cnt>0){
      dset = data.frame(data2())
      if (typeof(dset[,vartos])=="character"){
        cnt = count(dset, vartos)
        cnt$percentagevr = round((cnt$freq)*100/sum(cnt$freq),2)
        cnt
      }
      
      else if (typeof(dset[,vartos])=="integer" | typeof(dset[,vartos])=="double"){
        summary(dset[,vartos])
      }
    }
    else{
      dset = data.frame(data())
      if (typeof(dset[,vartos])=="character"){
        cnt = count(dset, vartos)
        cnt$percentagevr = round((cnt$freq)*100/sum(cnt$freq),2)
        cnt
      }
      
      else if (typeof(dset[,vartos])=="integer" | typeof(dset[,vartos])=="double"){
        summary(dset[,vartos])
      }
    }
    
    
    


  })
  
  
  output$smrybv <- renderPrint({
    
    
    
    bivarx = input$xvarsb
    bivary = input$yvarsb
    
    if (cnt>0){
      
    dset = data.frame(data2())  
    if ( typeof(dset[,bivarx])=="character" & typeof(dset[,bivary])=="character" ){
      cat("Both variables are categorical\n")
      cat("Criteria for Significance: p-value < 0.05\n ")
      chisq.test(table(dset[,bivarx], dset[,bivary]))
    }
    
    else if (  typeof(dset[,bivarx])=="double" &  typeof(dset[,bivary])=="double"){
      cat("Both variables are continuous\n")
      cat("Criteria for Significance: corr > 0.7 \n")
      cor.test(dset[,bivarx], dset[,bivary])
    }
    
    else if (typeof(dset[,bivarx])=="character" &   typeof(dset[,bivary])=="double" )  {
      cat("One variable is Categorical & other is Continuous\n")
      cat("Criteria for Significance: Pr(>F) < 0.05 \n")
      cat("\n")
      summary(aov(dset[,bivary] ~ dset[,bivarx]))
      
    }
    
    else if (  typeof(dset[,bivarx])=="double"  & typeof(dset[,bivary])=="character") {
      cat("One variable is Categorical & other is Continuous\n")
      cat("Criteria for Significance: Pr(>F) < 0.05 \n")
      cat("\n")
      summary(aov(dset[,bivarx] ~ dset[,bivary]))
      
    } }
    
    else {
    
    dset = data.frame(data())
    
    if (typeof(dset[,bivarx])=="character" & typeof(dset[,bivary])=="character"){
      cat("Both variables are categorical\n")
      cat("Criteria for Significance: p-value < 0.05\n ")
      chisq.test(table(dset[,bivarx], dset[,bivary]))
    }
    

    else if (typeof(dset[,bivarx])=="double" &  typeof(dset[,bivary])=="double" ){
      cat("Both variables are continuous\n")
      cat("Criteria for Significance: corr > 0.7 \n")
      cor.test(dset[,bivarx], dset[,bivary])
    }
    
    else if (typeof(dset[,bivarx])=="character" &   typeof(dset[,bivary])=="double"  ) {
      cat("One variable is Categorical & other is Continuous\n")
      cat("Criteria for Significance: Pr(>F) < 0.05 \n")
      cat("\n")
      summary(aov(dset[,bivary] ~ dset[,bivarx]))
      
    }

    else if (  typeof(dset[,bivarx])=="double"  & typeof(dset[,bivary])=="character") {
      cat("One variable is Categorical & other is Continuous\n")
      cat("Criteria for Significance: Pr(>F) < 0.05 \n")
      cat("\n")
      summary(aov(dset[,bivarx] ~ dset[,bivary]))
      
    }
    
    }
    
    
  })
  
  
  
  output$selx <- renderUI({
    vars <- names(data())
    selectInput("xvars",label = "Select X variable", choices = vars,multiple = TRUE)
  })
  
  output$sely <- renderUI({
    vars <- names(data())
    selectInput("yvar",label = "Select Y variables", choices = vars,multiple = FALSE)
  })
 
  
  output$rnuic <- renderUI({

      selectInput("selectc", label = NULL, 
                  choices = list("None_Classifier","Logistic_Regression", "Decision_Tree_Classifier", 
                                 "Random_Forest_Classifer","Boosting_GBM_Classifier",
                                 "SVM_Classifier","Naive_Bayes","Kmeans_Clustering", "PCA"), 
                  selected = "None_Classifier")
      
 
    
    })
  
  output$rnuir <- renderUI({
    
    selectInput("selectr", label = NULL, 
                choices = list("None_Regressor","Linear_Regression",  "Decision_Tree_Regressor",
                              "Random_Forest_Regressor","Boosting_GBM_Regressor",
                               "SVM_Regressor"), 
                selected = "None_Regressor")
    
    
    
  })
  
  output$rnwd <- renderUI({
    
    
    
    if (input$pselmd =="Logistic_Regression"){
      
      fluidRow(
        
        #h4(p("No parameters to select in Linear Regression Just Click RUN button!"))
        
        box(title = "No parameters to select in Logistic Regression Just Click RUN button!",width = 12,solidHeader = TRUE,status = "primary")
            
        
      )
    }
    

    else if (input$pselmd =="Decision_Tree_Classifier"){
      
      fluidRow(
        
        box(title = "Max. Tree Depth",width = 2,solidHeader = TRUE,status = "warning",
            numericInput(inputId = "trcnt",label = NULL,value = 5,min=1,max=10000,step = 1,width = "100px")
        ),
        box(title = "Min. no obs to split",width = 2,solidHeader = TRUE,status = "warning",
            numericInput(inputId = "trcms",label = NULL,value = 2,min=0,max=10000,step = 1,width = "100px")
        ),
        box(title = "Min no. obs per bin",width = 2,solidHeader = TRUE,status = "warning",
            numericInput(inputId = "trcmb",label = NULL,value = 1,min=1,max=15000,step = 1,width ="100px")
        )
        

      )
    }
    

    else if (input$pselmd =="Random_Forest_Classifer"){
      fluidRow(
        
        box(title = "No.of Tress",width = 2,solidHeader = TRUE,status = "warning",
            numericInput(inputId = "rfcnt",label = NULL,value = 200,min=0,max=10000,step = 100,width = "100px")
        ),
        box(title = "No. of Col vars to sample",width = 2,solidHeader = TRUE,status = "warning",
            numericInput(inputId = "rfcmt",label = NULL,value = 2,min=0,max=10000,step = 1,width = "100px")
        ),
        box(title = "Max no. of Nodes",width = 2,solidHeader = TRUE,status = "warning",
            numericInput(inputId = "rfcmn",label = NULL,value = 32,min=1,max=15000,step = 1,width ="100px")
        ),
        box(title = "Min size of Terminal Nodes",width = 2,solidHeader = TRUE,status = "warning",
            numericInput(inputId = "rfcns",label = NULL,value = 1,min=1,max=30,step = 1,width ="100px")
        )
             

      )
    }
    
    
    else if (input$pselmd =="Boosting_GBM_Classifier"){
      fluidRow(
        box(title = "No.of Trees",width = 2,solidHeader = TRUE,status = "warning",
            numericInput(inputId = "btcnt",label = NULL,value = 200,min=0,max=10000,step = 100,width = "100px")
        ),
        box(title = "Min. obs in Node",width = 2,solidHeader = TRUE,status = "warning",
            numericInput(inputId = "btcmo",label = NULL,value = 10,min=1,max=10000,step = 1,width = "100px")
        ),
        box(title = "Interaction Depth",width = 2,solidHeader = TRUE,status = "warning",
            numericInput(inputId = "btcid",label = NULL,value = 1,min=1,max=15000,step = 1,width ="100px")
        ),
        box(title = "Shrinkage",width = 2,solidHeader = TRUE,status = "warning",
            numericInput(inputId = "btcsg",label = NULL,value = 0.05,min=0,max=1,step = 0.01,width ="100px")
        ),
        
        box(title = "Distribution",width = 2,solidHeader = TRUE,status = "warning",
            
            selectInput("btcdu", label = NULL,  choices = list("bernoulli","multinomial","gaussian", "laplace","adaboost", "tdist","huberized",
                                                               "poisson","coxph","quantile","pairwise"),selected = "bernoulli")
        )
      )
    }
    
    
    else if (input$pselmd =="SVM_Classifier"){
      fluidRow(
        box(title = "Cost of Violations",width = 2,solidHeader = TRUE,status = "warning",
            numericInput(inputId = "svccs",label = NULL,value = 0.1,min=0,max=10000,step = 0.1,width = "100px")
        ),
        box(title = "Polynomial Kernel Degree",width = 2,solidHeader = TRUE,status = "warning",
            numericInput(inputId = "svcpl",label = NULL,value = 2,min=1,max=10000,step = 1,width = "100px")
        ),
        box(title = "Gamma Value",width = 2,solidHeader = TRUE,status = "warning",
            numericInput(inputId = "svcgm",label = NULL,value = 0.1,min=0,max=15000,step = 0.05,width ="100px")
        ),
        
        box(title = "Kernel Type",width = 2,solidHeader = TRUE,status = "warning",
            selectInput("svckt", label = NULL,  choices = list("linear","poly","radial"),selected = "linear")
        )

      )
    }
    
    
    else if (input$pselmd =="Naive_Bayes"){
      fluidRow(
        box(title = "Laplace smoothing",width = 2,solidHeader = TRUE,status = "warning",
            numericInput(inputId = "nbls",label = NULL,value = 0.1,min=0,max=10000,step = 0.1,width = "100px")
        )
      )
    }
    
    
    
    else if (input$pselmd =="Linear_Regression"){
      
      fluidRow(
        
        box(title = "No parameters to select in Linear Regression Just Click RUN button!",width = 12,solidHeader = TRUE,status = "primary"
            
        )
      )
    }
    
    
    else if (input$pselmd =="Decision_Tree_Regressor"){
      
      fluidRow(
        box(title = "Max. Tree Depth",width = 2,solidHeader = TRUE,status = "warning",
            numericInput(inputId = "trrnt",label = NULL,value = 5,min=1,max=10000,step = 1,width = "100px")
        ),
        box(title = "Min. no obs to split",width = 2,solidHeader = TRUE,status = "warning",
            numericInput(inputId = "trrms",label = NULL,value = 2,min=0,max=10000,step = 1,width = "100px")
        ),
        box(title = "Min no. obs per bin",width = 2,solidHeader = TRUE,status = "warning",
            numericInput(inputId = "trrmb",label = NULL,value = 1,min=1,max=15000,step = 1,width ="100px")
        )

      )
    }
    
    
    
    else if (input$pselmd =="Random_Forest_Regressor"){
      fluidRow(
        box(title = "No.of Tress",width = 2,solidHeader = TRUE,status = "warning",
            numericInput(inputId = "rfrnt",label = NULL,value = 200,min=0,max=10000,step = 100,width = "100px")
        ),
        box(title = "No. of Col vars to sample",width = 2,solidHeader = TRUE,status = "warning",
            numericInput(inputId = "rfrmt",label = NULL,value = 2,min=0,max=10000,step = 1,width = "100px")
        ),
        box(title = "Max no. of Nodes",width = 2,solidHeader = TRUE,status = "warning",
            numericInput(inputId = "rfrmn",label = NULL,value = 32,min=1,max=15000,step = 1,width ="100px")
        ),
        box(title = "Min size of Terminal Nodes",width = 2,solidHeader = TRUE,status = "warning",
            numericInput(inputId = "rfrns",label = NULL,value = 1,min=1,max=30,step = 1,width ="100px")
        )

      )
    }
    
    
    
    else if (input$pselmd =="Boosting_GBM_Regressor"){
      fluidRow(
        box(title = "No.of Trees",width = 2,solidHeader = TRUE,status = "warning",
            numericInput(inputId = "btrnt",label = NULL,value = 200,min=0,max=10000,step = 100,width = "100px")
        ),
        box(title = "Min. obs in Node",width = 2,solidHeader = TRUE,status = "warning",
            numericInput(inputId = "btrmo",label = NULL,value = 10,min=1,max=10000,step = 1,width = "100px")
        ),
        box(title = "Interaction Depth",width = 2,solidHeader = TRUE,status = "warning",
            numericInput(inputId = "btrid",label = NULL,value = 1,min=1,max=15000,step = 1,width ="100px")
        ),
        box(title = "Shrinkage",width = 2,solidHeader = TRUE,status = "warning",
            numericInput(inputId = "btrsg",label = NULL,value = 0.05,min=0,max=1,step = 0.01,width ="100px")
        ),
        
        box(title = "Distribution",width = 2,solidHeader = TRUE,status = "warning",
            
            selectInput("btrdu", label = NULL,  choices = list("bernoulli","multinomial","gaussian", "laplace","adaboost", "tdist","huberized",
                                                               "poisson","coxph","quantile","pairwise"),selected = "gaussian")
        )
        

      )
    }
    
    
    
    else if (input$pselmd =="SVM_Regressor"){
      fluidRow(
        box(title = "Cost of Violations",width = 2,solidHeader = TRUE,status = "warning",
            numericInput(inputId = "svrcs",label = NULL,value = 0.1,min=0,max=10000,step = 0.1,width = "100px")
        ),
        box(title = "Polynomial Kernel Degree",width = 2,solidHeader = TRUE,status = "warning",
            numericInput(inputId = "svrpl",label = NULL,value = 2,min=1,max=10000,step = 1,width = "100px")
        ),
        box(title = "Gamma Value",width = 2,solidHeader = TRUE,status = "warning",
            numericInput(inputId = "svrgm",label = NULL,value = 0.1,min=0,max=15000,step = 0.05,width ="100px")
        ),
        
        box(title = "Kernel Type",width = 2,solidHeader = TRUE,status = "warning",
            selectInput("svrkt", label = NULL,  choices = list("linear","poly","radial"),selected = "linear")
        )
        
       
      )
    }
    
  })
  
  
  
  
  
  
    
    STextinputC <- eventReactive(input$actionc, {
      
      if (input$selectc =="Logistic_Regression"){
        
        xvars = input$xvars
        yvar = input$yvar
        
        dset = data.frame(data())
        dset[,yvar] = as.factor(dset[,yvar])
        
        tdset = data.frame(tdata())
        tdset[,yvar] = as.factor(tdset[,yvar])
        
        frmla = paste(yvar,"~",paste(xvars,collapse = "+"))
        glm_fit = glm(as.formula(frmla),family = "binomial",data = dset)
        
        glm_probs = predict(glm_fit,data = dset,type = "response")
        dset$glmprobs = glm_probs
        
        tglm_probs = predict(glm_fit,newdata = tdset,type = "response")

        threshold <- function(predict, response) {
          r <- pROC::roc(response, predict)
          r$thresholds[which.max(r$sensitivities + r$specificities)]
        }
        
        thrshld = threshold(dset$glmprobs,dset[,yvar])
       
        dset$glm_pred = 0
        dset$glm_pred[dset$glmprobs>thrshld]=1
        tble = table(dset$glm_pred,dset[,yvar])
        acc = (tble[1,1]+tble[2,2])/sum(tble)
        
        
        tdset$glm_predt = 0
        
       
        tdset$glm_predt[tglm_probs>thrshld]=1
       
        tblet = table(tdset$glm_predt,tdset[,yvar])
        acct = (tblet[1,1]+tblet[2,2])/sum(tblet)
        
        
        print(paste("Logistic Regression Results"))
        
        print(paste("Train accuracy",round(acc*100,2)))
        print(paste("Test accuracy",round(acct*100,2)))
        
        
        print(paste("Model summary"))
        print(summary(glm_fit))
      }
      
      
      
      
      else if (input$selectc =="Random_Forest_Classifer"){
        xvars = input$xvars
        yvar = input$yvar
        dset = data.frame(data())
        dset[,yvar] = as.factor(dset[,yvar])
        
        dsett = data.frame(tdata())
        dsett[,yvar] = as.factor(dsett[,yvar])

        frmla = paste(yvar,"~",paste(xvars,collapse = "+"))
        

        for (i in 1:length(names(dset))){
          if (typeof(dset[,i])=="character"){
            dset[,i] = as.factor(dset[,i])
            
          }
        }
        
        for (i in 1:length(names(dsett))){
          if (typeof(dsett[,i])=="character"){
            dsett[,i] = as.factor(dsett[,i])
            
          }
        }
        
        
        rf_fit = randomForest(as.formula(frmla),data = dset,mtry=input$rfcmt,maxnodes= input$rfcmn,ntree=input$rfcnt,nodesize = input$rfcns)
        rf_pred = predict(rf_fit,data = dset,type = "response")
        rf_predt = predict(rf_fit,newdata = dsett,type = "response")
        
        
        tble = table(rf_pred,dset[,yvar])
        tblet = table(rf_predt,dsett[,yvar])
        
        acc = (tble[1,1]+tble[2,2])/sum(tble)
        acct = (tblet[1,1]+tblet[2,2])/sum(tblet)
     
        print(paste("Random forest classifer Results"))
        print(paste("Train accuracy",round(acc*100,2)))
        print(paste("Test accuracy",round(acct*100,2)))
        print(varImpPlot(rf_fit))
        
        
      }
       
      
      else if (input$selectc =="Boosting_GBM_Classifier"){
        xvars = input$xvars
        yvar = input$yvar
        dset = data.frame(data())
        dset[,yvar] = as.factor(dset[,yvar])
        
        dsett = data.frame(tdata())
        dsett[,yvar] = as.factor(dsett[,yvar])
        
        
        frmla = paste(yvar,"~",paste(xvars,collapse = "+"))
        
        for (i in 1:length(names(dset))){
          if (typeof(dset[,i])=="character"){
            dset[,i] = as.factor(dset[,i])

          }
        }
        
        for (i in 1:length(names(dsett))){
          if (typeof(dsett[,i])=="character"){
            dsett[,i] = as.factor(dsett[,i])
            
          }
        }
        
        gbm_fit = gbm(as.formula(frmla),data = dset,n.trees = input$btcnt,shrinkage = input$btcsg,interaction.depth = input$btcid,
                      n.minobsinnode = input$btcid ,distribution = input$btcdu)
        
        gbm_pred = predict(gbm_fit,data = dset,n.trees= input$btcnt,type = "response")
        gbm_predt = predict(gbm_fit,newdata = dsett,n.trees= input$btcnt,type = "response")
        
        
        tble = table(gbm_pred,dset[,yvar])
        acc = (tble[1,1]+tble[2,2])/sum(tble)
        
        tblet = table(gbm_predt,dsett[,yvar])
        acct = (tblet[1,1]+tblet[2,2])/sum(tblet)
        
        
        print(paste("Boosting classifer Results"))
        print(paste("Train accuracy",round(acc*100,2)))
        print(paste("Test accuracy",round(acct*100,2)))
        
        
      }
      
      
      
      
      else if (input$selectc =="Decision_Tree_Classifier"){
        xvars = input$xvars
        yvar = input$yvar
        
        dset = data.frame(data())
        dset[,yvar] = as.factor(dset[,yvar])
        
        dsett = data.frame(tdata())
        dsett[,yvar] = as.factor(dsett[,yvar])
        
        frmla = paste(yvar,"~",paste(xvars,collapse = "+"))
        
        for (i in 1:length(names(dset))){
          if (typeof(dset[,i])=="character"){
            dset[,i] = as.factor(dset[,i])
            
          }
        }
        
        for (i in 1:length(names(dsett))){
          if (typeof(dsett[,i])=="character"){
            dsett[,i] = as.factor(dsett[,i])
            
          }
        }

        tr_fit = rpart(as.formula(frmla),data = dset,method = "class",maxdepth=input$trcnt,minsplit = input$trcms, minbucket = input$trcmb)
        tr_pred = predict(tr_fit,data = dset,type = "class")
        
        tr_predt = predict(tr_fit,newdata = dsett,type = "class")
        
        
        
        tble = table(tr_pred,dset[,yvar])
        acc = (tble[1,1]+tble[2,2])/sum(tble)
        
        tblet = table(tr_predt,dsett[,yvar])
        acct = (tblet[1,1]+tblet[2,2])/sum(tblet)
        
        print(paste("Decision Tree classifer Results"))
        print(paste("Train accuracy",round(acc*100,2)))
        print(paste("Test accuracy",round(acct*100,2)))
        
        print(paste("Model summary"))
        
        
        print(summary(tr_fit))
      }
      
      
      else if (input$selectc =="SVM_Classifier" & input$svckt == "linear"){
        
        xvars = input$xvars
        yvar = input$yvar
        dset = data.frame(data())
        dset[,yvar] = as.factor(dset[,yvar])
        
        dsett = data.frame(tdata())
        dsett[,yvar] = as.factor(dsett[,yvar])
        
        for (i in 1:length(names(dset))){
          if (typeof(dset[,i])=="character"){
            dset[,i] = as.factor(dset[,i])
            
          }
        }
        
        for (i in 1:length(names(dsett))){
          if (typeof(dsett[,i])=="character"){
            dsett[,i] = as.factor(dsett[,i])
            
          }
        }
        
        
        frmla = paste(yvar,"~",paste(xvars,collapse = "+"))
        svm_fit = svm(as.formula(frmla),data = dset,kernel="linear",cost = input$svccs,scale = TRUE)
        svm_pred = predict(svm_fit,data = dset,type = "response")
        svm_predt = predict(svm_fit,newdata = dsett,type = "response")
        
        tble = table(svm_pred,dset[,yvar])
        acc = (tble[1,1]+tble[2,2])/sum(tble)
        
        tblet = table(svm_predt,dsett[,yvar])
        acct = (tblet[1,1]+tblet[2,2])/sum(tblet)
        
        print(paste("SVM Linear classifer Results"))
        print(paste("Train accuracy",round(acc*100,2)))
        print(paste("Test accuracy",round(acct*100,2)))
      }
      
      else if (input$selectc =="SVM_Classifier" & input$svckt == "poly"){
        xvars = input$xvars
        yvar = input$yvar
        dset = data.frame(data())
        dset[,yvar] = as.factor(dset[,yvar])
        
        dsett = data.frame(tdata())
        dsett[,yvar] = as.factor(dsett[,yvar])
        
        for (i in 1:length(names(dset))){
          if (typeof(dset[,i])=="character"){
            dset[,i] = as.factor(dset[,i])
            
          }
        }
        
        for (i in 1:length(names(dsett))){
          if (typeof(dsett[,i])=="character"){
            dsett[,i] = as.factor(dsett[,i])
            
          }
        }
        
        frmla = paste(yvar,"~",paste(xvars,collapse = "+"))
        svm_fit = svm(as.formula(frmla),data = dset,kernel="poly",cost = input$svccs,degree = input$svcpl,scale = TRUE)
        svm_pred = predict(svm_fit,data = dset,type = "response")
        svm_predt = predict(svm_fit,newdata = dsett,type = "response")
        
        tble = table(svm_pred,dset[,yvar])
        acc = (tble[1,1]+tble[2,2])/sum(tble)
        
        tblet = table(svm_predt,dsett[,yvar])
        acct = (tblet[1,1]+tblet[2,2])/sum(tblet)
        
        print(paste("SVM Polynomical Kernel classifer Results"))
        print(paste("Train accuracy",round(acc*100,2)))
        print(paste("Test accuracy",round(acct*100,2)))
      }
      
      else if (input$selectc =="SVM_Classifier" & input$svckt == "radial"){
        xvars = input$xvars
        yvar = input$yvar
        dset = data.frame(data())
        dset[,yvar] = as.factor(dset[,yvar])
        
        dsett = data.frame(tdata())
        dsett[,yvar] = as.factor(dsett[,yvar])
        
        for (i in 1:length(names(dset))){
          if (typeof(dset[,i])=="character"){
            dset[,i] = as.factor(dset[,i])
            
          }
        }
        
        for (i in 1:length(names(dsett))){
          if (typeof(dsett[,i])=="character"){
            dsett[,i] = as.factor(dsett[,i])
            
          }
        }
        
        
        frmla = paste(yvar,"~",paste(xvars,collapse = "+"))
        svm_fit = svm(as.formula(frmla),data = dset,kernel="radial",cost = input$svccs,gamma =  input$svcgm,scale = TRUE)
        svm_pred = predict(svm_fit,data = dset,type = "response")
        svm_predt = predict(svm_fit,newdata = dsett,type = "response")
        
        tble = table(svm_pred,dset[,yvar])
        acc = (tble[1,1]+tble[2,2])/sum(tble)
        
        tblet = table(svm_predt,dsett[,yvar])
        acct = (tblet[1,1]+tblet[2,2])/sum(tblet)
        
        print(paste("SVM RBF kernel classifer Results"))
        print(paste("Train accuracy",round(acc*100,2)))
        print(paste("Test accuracy",round(acct*100,2)))
      }
      
      
      else if (input$selectc =="Naive_Bayes"){
        xvars = input$xvars
        yvar = input$yvar
        dset = data.frame(data())
        dset[,yvar] = as.factor(dset[,yvar])
        
        dsett = data.frame(tdata())
        dsett[,yvar] = as.factor(dsett[,yvar])
        
        for (i in 1:length(names(dset))){
          if (typeof(dset[,i])=="character"){
            dset[,i] = as.factor(dset[,i])
            
          }
        }
        
        for (i in 1:length(names(dsett))){
          if (typeof(dsett[,i])=="character"){
            dsett[,i] = as.factor(dsett[,i])
            
          }
        }
        
        
        frmla = paste(yvar,"~",paste(xvars,collapse = "+"))
        
        nb_fit = naiveBayes(as.formula(frmla),data = dset,laplace = input$nbls,type = "class")

        rf_pred = predict(nb_fit,data = dset,type = "response")
        rf_predt = predict(nb_fit,newdata = dsett,type = "response")
        
        tble = table(rf_pred,dset[,yvar])
        acc = (tble[1,1]+tble[2,2])/sum(tble)
        
        tblet = table(svm_predt,dsett[,yvar])
        acct = (tblet[1,1]+tblet[2,2])/sum(tblet)
        
        print(paste("Naive Bayes Results"))
        print(paste("Train accuracy",round(acc*100,2)))
        print(paste("Test accuracy",round(acct*100,2)))
      }  
      

    })

    
    output$accrcy <- renderPrint({
       STextinputC()
    })
    
    
    
    STextinputR <- eventReactive(input$actionr, {
      
      if (input$selectr =="Random_Forest_Regressor"){
        xvars = input$xvars
        yvar = input$yvar
        
        dset = data.frame(data())
        dsett = data.frame(tdata())        
        
        
        for (i in 1:length(names(dset))){
          if (typeof(dset[,i])=="character"){
            dset[,i] = as.factor(dset[,i])
            
          }
        }
        
        for (i in 1:length(names(dsett))){
          if (typeof(dsett[,i])=="character"){
            dsett[,i] = as.factor(dsett[,i])
            
          }
        }
        
        frmla = paste(yvar,"~",paste(xvars,collapse = "+"))
        
        rf_fit = randomForest(as.formula(frmla),data = dset,mtry=input$rfrmt,maxnodes= input$rfrmn,ntree=input$rfrnt,nodesize = input$rfrns)
        rf_pred = predict(rf_fit,data = dset)
        rf_predt = predict(rf_fit,newdata = dsett)
        
        R2 <- 1 - (sum((dset[,yvar]-rf_pred )^2)/sum((dset[,yvar]-mean(dset[,yvar]))^2))
        R2T <- 1 - (sum((dsett[,yvar]-rf_predt )^2)/sum((dsett[,yvar]-mean(dsett[,yvar]))^2))
        
        print(paste("Random forest Regressor Results"))
        print(paste("Train Adj R-squared",round(R2*100,2)))
        print(paste("Test Adj R-squared",round(R2T*100,2)))
        
        print(varImpPlot(rf_fit))
      }
      
      
      
      else if (input$selectr =="Decision_Tree_Regressor"){
        xvars = input$xvars
        yvar = input$yvar
        dset = data.frame(data())
        dsett = data.frame(tdata()) 
        
        
        for (i in 1:length(names(dset))){
          if (typeof(dset[,i])=="character"){
            dset[,i] = as.factor(dset[,i])
            
          }
        }
        
        for (i in 1:length(names(dsett))){
          if (typeof(dsett[,i])=="character"){
            dsett[,i] = as.factor(dsett[,i])
            
          }
        }
        
        frmla = paste(yvar,"~",paste(xvars,collapse = "+"))
        
        tr_fit = rpart(as.formula(frmla),data = dset,method = "anova",maxdepth=input$trrnt,minsplit = input$trrms, minbucket = input$trrmb)
        tr_pred = predict(tr_fit,data = dset)
        tr_predt = predict(tr_fit,newdata = dsett)
        
        
        R2 <- 1 - (sum((dset[,yvar]-tr_pred )^2)/sum((dset[,yvar]-mean(dset[,yvar]))^2))
        R2T <- 1 - (sum((dsett[,yvar]-tr_predt )^2)/sum((dsett[,yvar]-mean(dsett[,yvar]))^2))
        
        
        print(paste("Decision Tree Regressor Results"))
        print(paste("Train Adj R-squared",round(R2*100,2)))
        print(paste("Test Adj R-squared",round(R2T*100,2)))
        
        
      }
      else if (input$selectr =="Linear_Regression"){
        xvars = input$xvars
        yvar = input$yvar
        dset = data.frame(data())
        dsett = data.frame(tdata()) 
        
        for (i in 1:length(xvars)){
          if (typeof(dset[,xvars[i]])=="character"){
            dset[,xvars[i]] = as.factor(dset[,xvars[i]])
          }
        }
        
        for (i in 1:length(names(dsett))){
          if (typeof(dsett[,i])=="character"){
            dsett[,i] = as.factor(dsett[,i])
          }
        }
        
        
        frmla = paste(yvar,"~",paste(xvars,collapse = "+"))
        
        lnr_fit = lm(as.formula(frmla),data = dset)
        rf_pred = predict(lnr_fit,data = dset)
        rf_pred = predict(lnr_fit,newdata = dsett)
        
        R2 <- 1 - (sum((dset[,yvar]-rf_pred )^2)/sum((dset[,yvar]-mean(dset[,yvar]))^2))
        R2T <- 1 - (sum((dsett[,yvar]-rf_pred )^2)/sum((dsett[,yvar]-mean(dsett[,yvar]))^2))
        
        print(paste("Linear Regression Results"))
        print(paste("Train Adj R-squared",round(R2*100,2)))
        print(paste("Test Adj R-squared",round(R2T*100,2)))
        print(paste("Model summary"))
        print(summary(lnr_fit))
        
      }
      
      
      
      else if (input$selectr =="SVM_Regressor" & input$svrkt == "linear"){
        
        xvars = input$xvars
        yvar = input$yvar
        dset = data.frame(data())
        dsett = data.frame(tdata()) 
        
        for (i in 1:length(names(dset))){
          if (typeof(dset[,i])=="character"){
            dset[,i] = as.factor(dset[,i])
            
          }
        }
        
        for (i in 1:length(names(dsett))){
          if (typeof(dsett[,i])=="character"){
            dsett[,i] = as.factor(dsett[,i])
            
          }
        }
        
        frmla = paste(yvar,"~",paste(xvars,collapse = "+"))
        svm_fit = svm(as.formula(frmla),data = dset,kernel="linear",cost = input$svrcs,scale = TRUE)
        svm_pred = predict(svm_fit,data = dset)
        svm_predt = predict(svm_fit,newdata = dsett)
        
        R2 <- 1 - (sum((dset[,yvar]-svm_pred )^2)/sum((dset[,yvar]-mean(dset[,yvar]))^2))
        R2T <- 1 - (sum((dsett[,yvar]-svm_predt )^2)/sum((dsett[,yvar]-mean(dsett[,yvar]))^2))
        
        print(paste("SVM Regressor with Linear Kernel Results"))
        print(paste("Train Adj R-squared",round(R2*100,2)))
        print(paste("Test Adj R-squared",round(R2T*100,2)))
        
      }
      
      else if (input$selectr =="SVM_Regressor" & input$svrkt == "poly"){
        xvars = input$xvars
        yvar = input$yvar
        dset = data.frame(data())
        dsett = data.frame(tdata()) 
        
        for (i in 1:length(names(dset))){
          if (typeof(dset[,i])=="character"){
            dset[,i] = as.factor(dset[,i])
            
          }
        }
        
        for (i in 1:length(names(dsett))){
          if (typeof(dsett[,i])=="character"){
            dsett[,i] = as.factor(dsett[,i])
            
          }
        }
        
        
        frmla = paste(yvar,"~",paste(xvars,collapse = "+"))
        svm_fit = svm(as.formula(frmla),data = dset,kernel="poly",cost = input$svrcs,degree = input$svrpl,scale = TRUE)
        svm_pred = predict(svm_fit,data = dset)
        svm_predt = predict(svm_fit,newdata = dsett)
        
        R2 <- 1 - (sum((dset[,yvar]-svm_pred )^2)/sum((dset[,yvar]-mean(dset[,yvar]))^2))
        R2T <- 1 - (sum((dsett[,yvar]-svm_predt )^2)/sum((dsett[,yvar]-mean(dsett[,yvar]))^2))
        
        print(paste("SVM Regressor with Polynomial Kernel Results"))
        print(paste("Train Adj R-squared",round(R2*100,2)))
        print(paste("Test Adj R-squared",round(R2T*100,2)))
      }
      
      else if (input$selectr =="SVM_Regressor" & input$svrkt == "radial"){
        xvars = input$xvars
        yvar = input$yvar
        dset = data.frame(data())
        dsett = data.frame(tdata()) 
        
        for (i in 1:length(names(dset))){
          if (typeof(dset[,i])=="character"){
            dset[,i] = as.factor(dset[,i])
            
          }
        }
        
        for (i in 1:length(names(dsett))){
          if (typeof(dsett[,i])=="character"){
            dsett[,i] = as.factor(dsett[,i])
            
          }
        }
        
        frmla = paste(yvar,"~",paste(xvars,collapse = "+"))
        svm_fit = svm(as.formula(frmla),data = dset,kernel="radial",cost = input$svrcs,gamma =  input$svrgm,scale = TRUE)
        svm_pred = predict(svm_fit,data = dset)
        svm_predt = predict(svm_fit,newdata = dsett)
        
        R2 <- 1 - (sum((dset[,yvar]-svm_pred )^2)/sum((dset[,yvar]-mean(dset[,yvar]))^2))
        R2T <- 1 - (sum((dsett[,yvar]-svm_predt )^2)/sum((dsett[,yvar]-mean(dsett[,yvar]))^2))
        
        print(paste("SVM Regressor with RBF Kernel Results"))
        print(paste("Train Adj R-squared",round(R2*100,2)))
        print(paste("Test Adj R-squared",round(R2T*100,2)))
      }
      
      
      
 
      
      
    
    })
    
    
    output$accrcyr <- renderPrint({
      STextinputR()
    })
    
    
    
    
    
    
    
    
}






shinyApp(ui,server)





