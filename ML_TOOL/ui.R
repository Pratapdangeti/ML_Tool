


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
      
      tabItem(tabName = "dashboard",
              
              fluidRow(
                box(title = "Machine Learning Tool for Quickly prototyping Model",height = "900px",width = "620px",solidHeader = TRUE,status = "success",
                    
                    h4(p("Welcome to Machine Learning Tool ! This is Pratap, a Data Science enthusiast who tries to make things easy for fellow Data Science enthusiasts ")),
                    br(),
                    h4(p("In this page you can quickly prototype various Machine Learning Models on your chosen data")),
                    br(),
                    h4(p("Objective of the site is to make Machine Learning models easy for everybody in an interactive way !")),
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
                    
                    fileInput('tchosenfile', 'Select Test Data file',width = "200px",
                              accept=c('text/csv',
                                       'text/comma-separated-values,text/plain',
                                       '.csv','.tsv')),
                    checkboxInput('theader', 'Select Header for file with Header', TRUE)
                    
                    
                ),
                
                box(title = "Check",width = 4,solidHeader = TRUE, status = "warning",
                    h4(p("Upload both Train & Test data files in order to make \"Machine Learning Models\" tab work properly !"))
                )
                
                
              ),
              
              fluidRow(
                box(title = "Show Train data",width = 12,solidHeader = TRUE,status="primary",
                    DT::dataTableOutput("tbl",width = "98%",height = "auto"))
              ),
              
              fluidRow(
                box(title = "Show Test data",width = 12,solidHeader = TRUE,status="primary",
                    DT::dataTableOutput("ttbl",width = "98%",height = "auto"))
              ),
              
              fluidRow(
                box(title = "Show Train Data Structure",width = 12,solidHeader = TRUE,status="primary",
                    verbatimTextOutput("smry1")
                )
              ),
              
              fluidRow(
                box(title = "Show Test Data Structure",width = 12,solidHeader = TRUE,status="primary",
                    verbatimTextOutput("smry2")
                )
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
                                 radioButtons("toVarOpts",label = h3("To Variable Selection"), list("integer","factor","double"),selected = "factor",
                                              inline = TRUE, width = "500px"),
                                 br(),
                                 
                                 actionButton("convertb", "CONVERT",icon=icon("caret-square-o-right")),
                                 br(),
                                 br(),
                                 
                                 actionButton("chkclssa", "AFTER CHECK",icon=icon("caret-square-o-right"))
                                 
                             ),
                             box(title = "Variable Type Viewer",solidHeader = TRUE,status = "primary", width = 6,
                                 
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
                )
                
                
              ))
      
    )
  )
)





