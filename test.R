

rm(list = ls())

library(shiny)
library(shinydashboard)
library(DT)


library(randomForest)
library(gbm)
library(e1071)


ui <- dashboardPage(
  dashboardHeader(title = h4("Test"),titleWidth = 300),
  dashboardSidebar(
    sidebarMenu(
    menuItem("Dashboard",icon = icon("dashboard"),tabName = "dashboard")
    )
  ),
  
  dashboardBody(
    
    tabItems(

      
      tabItem(tabName = "dashboard",
              
              fluidRow(
                tabBox(
                  title = "",
                  id = "tabset2", height = "900px",width = "620px",
                  
                  
                  tabPanel("Model Selection",
                           fluidRow(
                             box(title = "Data Import",width = 4,solidHeader = TRUE, status = "primary",
                                 
                                 
                                 fileInput('chosenfile', 'Select Data file ',width = "200px",
                                           accept = c(
                                             'text/csv','text/comma-separated-values','text/tab-separated-values',
                                             'text/plain','.csv','.tsv'
                                           )),
                                 checkboxInput('header', 'Select Header for file with Header', TRUE)
                                 
                                 
                             ),
                             
                             
                             box(title = "Select Variables",width = 4,solidHeader = TRUE,status = "primary",
                                 
                                 uiOutput("selx"),
                                 uiOutput("sely")
                                 
                             ),
                             box(title = "Select_Type",width = 3,solidHeader = TRUE,status = "primary",
                                 
                                 radioButtons("summaryOptions",label = NULL, list("Classification","Regression","Unsupervised"),
                                              selected = "Classification",inline = FALSE)
                             )
                           ),
                           
                           fluidRow(
                             box(title = "Select_Model",width = 3,solidHeader = TRUE,status = "primary",
                                 
                                 uiOutput("rnui")
                                 
                             ),
                             box(title = "Model Run",solidHeader = TRUE,status = "success", width = 2,
                                 
                                 actionButton("action", "RUN",icon=icon("caret-square-o-right"))
                             )
                           ),
                           
                           
                           fluidRow(
                             uiOutput("rnwd")
                           ),
                           
                           fluidRow(
                             box(title = "Model summary is",width = 3,solidHeader = TRUE,status = "primary",
                                 textOutput("accrcy")
                             )
                             
                           )
                           
                             
                             
                           )
                  )
              
      )
  )
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
  
  output$selx <- renderUI({
    vars <- names(data())
    selectInput("xvar",label = "Select X variable", choices = vars,multiple = TRUE)
  })
  
  
  output$sely <- renderUI({
    vars <- names(data())
    selectInput("yvar",label = "Select Y variable", choices = vars)
  })
  
  output$rnui <- renderUI({
    if (input$summaryOptions=="Classification"){
      selectInput("selectc", label = NULL, 
                  choices = list("Logistic_Regression", "Decision_Tree_Classifier", "Random_Forest_Classifer","Boosting_GBM_Classifier","SVM_Classifier","Naive_Bayes"), 
                  selected = "Logistic_Regression")
      
    }
    
    else if (input$summaryOptions=="Regression"){
      selectInput("selectr", label = NULL, 
                  choices = list("Linear_Regression", "Decision_Tree_Regressor", "Random_Forest_Regressor","Boosting_GBM_Regressor","SVM_Regressor"), 
                  selected = "Linear_Regression")
      
    }
    
    else if (input$summaryOptions=="Unsupervised"){
      selectInput("selectu", label = NULL, 
                  choices = list("Kmeans_Clustering", "PCA", "SOM"), 
                  selected = "Kmeans_Clustering")
      
    }
  })
  
  output$rnwd <- renderUI({
  
  if (input$selectc =="Random_Forest_Classifer"){
    fluidRow(
      
      box(title = "Select Random Forest Parameters for Classification",width = 12,solidHeader = TRUE,status = "primary",
          box(title = "No.of Tress",width = 2,solidHeader = TRUE,status = "warning",
              numericInput(inputId = "rfcnt",label = NULL,value = 200,min=0,max=10000,step = 100,width = "100px")
          ),
          box(title = "No. of Col vars to sample",width = 2,solidHeader = TRUE,status = "warning",
              numericInput(inputId = "rfcmt",label = NULL,value = 10,min=0,max=10000,step = 1,width = "100px")
          ),
          box(title = "Max no. of Nodes",width = 2,solidHeader = TRUE,status = "warning",
              numericInput(inputId = "rfcmn",label = NULL,value = 32,min=1,max=15000,step = 1,width ="100px")
          ),
          box(title = "Min size of Terminal Nodes",width = 2,solidHeader = TRUE,status = "warning",
              numericInput(inputId = "rfcns",label = NULL,value = 1,min=1,max=30,step = 1,width ="100px")
          )
      )
    )
    
  }
  })
  
  
  
  
  
  
  STextinput <- eventReactive(input$action, {
    
    
    
    if (input$selectc =="Logistic_Regression"){
      
      vars = names(data())
      toString(vars)
    }
    
    
    
    
    if (input$selectc =="Random_Forest_Classifer"){
      
      vars = names(data())
      yvar = input$yvar
      xvars = vars[-which(names(data()) == yvar)]  
      
      #frmla = as.formula(paste(yvar,"~",paste(xvars,collapse = "+")))
      
      
      #rf_fit = randomForest(frmla,data = data.frame(data()))
      
      trn_data = data()
      trn_data$class = trn_data$class-1
      
      trn_data$class = as.factor(trn_data$class)
      
      rf_fit = randomForest(class ~ .,data = trn_data)
      
      svm_trainpred = predict(rf_fit,trn_data)
      
      trntble = table(svm_trainpred,trn_data$class)
      
      toString(trntble)
      
      #toString(nrow(trn_data))
 
      
      
    }
    
  })
  
  output$accrcy <- renderText({
    acc = STextinput()
    acc 
  })
  
  
  
  
  
  
}



shinyApp(ui,server)







