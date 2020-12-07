


setwd('C:\\Users\\prata\\Documents\\current_projects\\SVM_Bigdata\\data')


train_data = read.csv("train_data.csv",header=TRUE,stringsAsFactors = TRUE)
test_data = read.csv("test_data.csv",header=TRUE,stringsAsFactors = TRUE)


str(train_data)

train_data$class = as.factor(train_data$class)
train_data[,"class"] = as.factor(train_data[,"class"])
#str(train_data)


typeof(train_data$class)

sapply(train_data, is.factor)

is.factor(train_data$class)


str(train_data)

summary(train_data$Duration_in_month)

svm_fit = svm(class ~Credit_history+Age_in_years,data = train_data,kernel="poly",cost = 4.0 ,degree = 2,scale = TRUE)
summary(svm_fit)



library(tree)
library(rpart)


tr_fit = tree(class ~Credit_history+Age_in_years,data = train_data)

tr_fit = rpart(class ~.,data = train_data,method = "class",maxdepth=2,minsplit = 2, minbucket = 180)
smry = summary(tr_fit)
smry$call

tr_pred = predict(tr_fit,data = dset)

tble = table(tr_pred,train_data$class)
acc = (tble[1,1]+tble[2,2])/sum(tble)

paste("Train accuracy",round(acc*100,2))


#



library(plyr)

cnt = count(train_data,Credit_history)

sum(cnt$n)
cnt$m = (cnt$n)*100/sum(cnt$n)
cnt

vartos = train_data$class
summarise(train_data, 
          "Frequencies"= count(train_data,vartos), 
          "Percent" = count(train_data,vartos)/ sum(count(train_data,vartos)))


data("mtcars")
attach(mtcars)
str(mtcars)
typeof(mtcars$mpg)

# "double", "integer", 

dset = train_data

dset[,xvars[i]] = as.factor(dset[,xvars[i]])

str(dset)

for (i in 1:length(names(dset))){
  print(typeof(dset[,i]))
}



for (i in 1:length(names(dset))){
  
  if (typeof(dset[,i])=="character"){
    dset[,i] = as.factor(dset[,i])
    
  }
}



# Regular run from here



library(shiny)
library(shinydashboard)
library(randomForest)
library(e1071)
library(gbm)



ui <- dashboardPage(
  dashboardHeader(title = h4("Multiple inputs"),titleWidth = 300),
  dashboardSidebar(
    sidebarMenu(
      
      menuItem("Dashboard",icon = icon("dashboard"),tabName = "dashboard")
    )
  ),
  dashboardBody(
    tabItems(
    
      tabItem(tabName = "dashboard",
              fluidRow(
                box(title = "Welcome",height = "1500px",width = "620px",solidHeader = TRUE,status = "success",
                    
                    fileInput('chosenfile', 'Select Data file ',width = "200px",
                              accept = c(
                                'text/csv','text/comma-separated-values','text/tab-separated-values',
                                'text/plain','.csv','.tsv'
                              )),
                    checkboxInput('header', 'Select Header for file with Header', TRUE),
                    
                    uiOutput("selx"),
                    uiOutput("sely"),
                    
                    actionButton("action", "RUN",icon=icon("caret-square-o-right")),
                    fluidRow(
                      uiOutput("rnwd")
                    ),
                    
                    fluidRow(
                    box(title = "Model Results",solidHeader = TRUE,status = "success", width = 12,
                        
                        verbatimTextOutput("accrcy")
                    )
                    )
                    
                    
                   # textOutput("accrcy")
                    
                  
                    #selectInput("yvar",label = "Select Y variable", choices = names(data()))
                    
                    
                    
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
    selectInput("xvars",label = "Select X variable", choices = vars,multiple = TRUE)
  })
  
  output$sely <- renderUI({
    vars <- names(data())
    selectInput("yvar",label = "Select Y variables", choices = vars,multiple = FALSE)
  })
  
  

  STextinput <- eventReactive(input$action, {
    
    
    xvars = input$xvars
    yvar = input$yvar
    
    dset = data.frame(data())
    
    #dset[,yvar] = as.factor(dset[,yvar])
    

    
    #print(is.vector(xvars))
    
    #print(names(dset))
    #str(dset)
    
    frmla = paste(yvar,"~",paste(xvars,collapse = "+"))
    
    #str(dset)
    
    for (i in 1:length(xvars)){

      print(typeof(dset[,i]))
    }
    
    
    # for (i in 1:length(xvars)){
    #   if (typeof(dset[,i])=="character"){
    #     dset[,xvars[i]] = as.factor(dset[,xvars[i]])
    #     
    #   }
    # }
    
    # for (i in 1:length(names(dset))){
    #   if (typeof(dset[,i])=="character"){
    #     dset[,i] = as.factor(dset[,i])
    #     
    #   }
    # }
    
    print("after")
    
    for (i in 1:length(xvars)){
      print(typeof(dset[,i]))
    }
    
    lnr_fit = lm(as.formula(frmla),data = dset)
    summary(lnr_fit)
    

    # rf_fit = randomForest(as.formula(frmla),data = dset,mtry=input$rfcmt,maxnodes= input$rfcmn,ntree=input$rfcnt)
    # rf_pred = predict(rf_fit,data = dset,type = "response")
    
    # gbm_fit = gbm(as.formula(frmla),data = dset,n.trees = 1000,shrinkage = 0.001,interaction.depth = 2)
    # gbm_pred = predict(gbm_fit,data = dset,n.trees= 1000,type = "response")
    # 
    # tble = table(gbm_pred,dset$class)
    # 
    # acc = (tble[1,1]+tble[2,2])/sum(tble)
    # acc



    # paste("Train accuracy",acc*100)
    
    #print(summary(svm_fit))
    
    #frmla
    
    
  })
  
  output$accrcy <- renderPrint({
    acc = STextinput()
    acc 
  })
  
  
  output$rnwd <- renderUI({
    fluidRow(
      
      box(title = "Select Random Forest Parameters for Classification",width = 12,solidHeader = TRUE,status = "primary",
          
          
          

              
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
      
    )
    
  })
  
  
  
}

shinyApp(ui,server)





