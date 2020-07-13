#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


#==========================================================#
#===     Author: Kirsten Bell Burdett                   ===#
#===     Created: 02 July, 2020                         ===#
#===     R Shiny App for VTE prediction in glioma patients #
#==========================================================#

# https://shiny.rstudio.com/gallery/
# https://shiny.rstudio.com/gallery/radiant.html


#Installing/Loading Packages

if(!require(shiny)){
  install.packages("shiny")
  library(shiny)
}

if(!require(shinydashboard)){
  install.packages("shinydashboard")
  library(shinydashboard)
}


if(!require(shinydashboardPlus)){
  install.packages("shinydashboardPlus")
  library(shinydashboardPlus)
}


# 
# dbHeader <- dashboardHeader(title = "VTE Prediction",
#                             tags$li(a(img(src = 'FeinbergLogo.png',
#                                           title = "Company Home", height = "30px"),
#                                       style = "padding-top:10px; padding-bottom:10px;"),
#                                     class = "dropdown"))


ui <- dashboardPagePlus(skin = "purple",
  dashboardHeader(title = "VTE Prediction"),
  dashboardSidebar(sidebarMenu(
    menuItem("Home", tabName = "home", icon = icon("home")),
    menuItem("Prediction", tabName = "prediction"),
    menuItem("Visualize", tabName = "test"),
    menuItem("References", tabName = "references")
  )),
  ## Body content
  dashboardBody(
    tabItems(
      # Home tab content
      tabItem(tabName = "home",
              
              #======  Objectives  ======#
              titlePanel("Objectives"),
              p("This interactive web application utilizes patient and tumor characteristics to predict venous thromboembolism (VTE)
                in glioma patients after surgical resection of original glioma.") ,
              
              tags$li(tags$ul("Risk prediction for VTE in glioma patients")),
              tags$li(tags$ul("Risk prediction for VTE in glioma patients")),
              
              
              br(),
              
              
              #======  Getting Started  ======#
              titlePanel("Getting Started"),
              p("INSTRUCTIONS HERE for navigating app"),
              
              
              br(),
              
              
              #======  Background  ======#
              titlePanel("Background"),
              p("In order to investigate factors that may help predict glioma patients who are at high risk for developing thrombotic
                complication, a set of predictors were selected using Northwestern data for training of risk predictors. 
                VTE is defined as venous thrombus or pulmonary embolus."),
              
              
              br(),
              
              
              tags$b(tags$u("Statistical Methods")),
              
              tags$li(tags$ul("The regularization method LASSO, along with 5-fold cross-validation to choose the tuning parameter lambda,
                will be used to determine the model used to predict VTE.")),
              tags$li(tags$ul("Before using LASSO, we will do multiple imputation by chained equations treating the missing data as missing at random (MAR). 
                We will only impute variable that have <10% missing.")),
              
              br(),
             
              tags$b(tags$u("Prediction")),
              
              tags$li(tags$ul("Prediction probabilities will be determined using the coefficient estimates from the predictors selected using 
                              the methods described above.  For more information regarding the data used to generate these coefficients and 
                              predictors, please view the `Visualize` tab.")),
              
              
              br(),
              
              
              #======  Help  ======#
              titlePanel("Help"),
              p("Resources for help. Potentially add a link regarding prediction vs. inference.")
              
      ),
      
      # Prediction tab content
      tabItem(tabName = "prediction",
              
              # Application title
              titlePanel("VTE Prediction for Glioma Patients"),
              p("Patient and tumor characteristics at diagnosis of original glioma"),
              br(),
              
              numericInput(inputId = "Age", label = strong("Age (Years)"), min=0, max=100, value=0),
              
              sliderInput('BMI', 'BMI', 0,50,1),
              
              selectInput(inputId = "Sex", label = strong("Sex"),
                          choices = c("Male" , "Female"),
                          multiple = FALSE),
              
              
              radioButtons(inputId = "Current_Smoker", label = "Current Smoker", c("No", "Yes")),
              
              numericInput(inputId = "Platelet_count", label = strong("Platelet count (10^9/L)"), value = 0),
              numericInput(inputId = "WBCcount", label = strong("White blood cell count (10^9/L)"), value = 0),
              
              
              
              radioButtons(inputId = "IDH", label = "IDH Mutation", c("No", "Yes")),
              radioButtons(inputId = "MGMT", label = "MGMT methylation", c("No", "Yes")),
              radioButtons(inputId = "TMZ", label = "Treated with TMZ", c("No", "Yes")),
              radioButtons(inputId = "WHOgrade", label = "WHO grade", c("No", "Yes")),
              
              fluidRow(
                radioButtons(inputId = "Hypertension", label = "Hypertension", c("No", "Yes")),
                
                radioButtons(inputId = "Hypothyroidism", label = "Hypothyroidism", c("No", "Yes"))
              ),
              
              
              
              
              br(),
              br(),
              
              p("Prediction probabilities for patient input."),
              
              br(),
              p("results here")
      ),
      
          
      
      # test tab content
      tabItem(tabName = "test",

              fluidRow(
                box(plotOutput("plot1", height = 250)),
                
                box(
                  title = "Controls",
                  sliderInput("slider", "Number of observations:", 1, 100, 50)
                )
              )
      ),
      
      # references tab content
      tabItem(tabName = "references",
              h2("references"))
    )
  ),
  
  
  
  ## Footnote content
 dashboardFooter(left_text = "VTE Prediction" ,right_text = tags$img(src='FeinbergLogo.png',height='35',width='150'))
)

server <- function(input, output) {
  set.seed(122)
  histdata <- rnorm(500)
  
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
}

shinyApp(ui, server)