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


# Purpose: This interactive web application utilizes patient and tumor characteristics 
#to predict venous thromboembolism (VTE) in glioma patients after surgical resection of original glioma.

#==========================================================#



#Installing/Loading Packages


#library(foreign)
# options("repos" = c("CRAN" = "https://cran.rstudio.com", 
#                     "svn.r-project" = "https://svn.r-project.org/R-packages/trunk/foreign"))

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

# if(!require(tidyverse)){
#   install.packages("tidyverse")
#   library(tidyverse)
# }

if(!require(plotly)){
  install.packages("plotly")
  library(plotly)
}

#library(survival)
#library(survminer)


 
#==========================================================#
#===============        Read in data             ==========#
#==========================================================#

dir <- "/Volumes/fsmresfiles/PrevMed/Projects/Brain_SPORE_BB_Core/Projects/Horbinski/Thrombosis/Code/VTEpredictionGlioma/"

#load in the objects
load(file.path(dir, "predict_VTE_app/VTE_KMplot.rda")) #KM plot "ggsurv1" and "kmplotdat"
#load(file.path(dir, "predict_VTE_app/pcaVTE_VTE.rda")) #pca VTE column "pcaVTE"
#load(file.path(dir, "predict_VTE_app/VTE_pcaplot.rda")) #pca plot "pcaplot"
load(file.path(dir, "predict_VTE_app/PCApoints.rda"))  #pcpoints and pcout in PCAplotData

load(file.path(dir, "predict_VTE_app/VTE_predfit1.rda")) #prediction from original data selected model "predfit1"

load(file.path(dir, "predict_VTE_app/VTE_allWHOplots.rda")) #bar charts by WHO grade "allWHOplots"
#plots are ageWHOgrade, BMIWHOgrade, WBC_WHOgrade, platelet_WHOgrade



catvars <- c("Sex", "Hypertension", "Hypothyroidism", "IDH", "MGMT", "Temozolomide")
contvars <- c("Age", "BMI", "WBCcount", "Platelet_count")









#==========================================================#
#================            UI                  ==========#
#==========================================================#
ui <- dashboardPagePlus(skin = "purple",
  dashboardHeader(title = "VTE Prediction"),
  dashboardSidebar(sidebarMenu(
    menuItem("Prediction", tabName = "prediction"),
    menuItem("Visualize", tabName = "visualize"),
    menuItem("References", tabName = "references")
  )),
  ## Body content
  dashboardBody(
    tabItems(
     
      # Prediction tab content
      tabItem(tabName = "prediction",
              
              titlePanel("VTE Prediction for Glioma Patients"),
              p("Predicting probability of VTE event at 1, 3, 6, and 12 months for the patient entered below."),
              
              fluidRow(
                box(title = "Results", 
                    width = 11,
                    solidHeader = TRUE, status = "warning",
                    "Predicted probability of VTE event using the active covariates selected from the optimal model from regularized Cox regression.", br(), "",
                    tableOutput(outputId = "Prediction_table"))
              ),
              
              br(),
              br(),
              
              
              # Application titleisn
              p("Patient and tumor characteristics at diagnosis of original glioma"),
              
              fluidRow(
                box(title = "Patient Markers", status= "primary", solidHeader = TRUE,
                    collapsible = FALSE, height = 650,
                    
                    "At diagnosis of original glioma",  br(), br(),  "",
                    
                    numericInput(inputId = "Age", label = "Age (Years)", min=0, max=100, value=NULL),

                    radioButtons(inputId = "Sex", label = "Sex", c("Male" , "Female"), selected = character(0)),
                    
                    numericInput(inputId = "BMI", label = "BMI", min=0, max=50, value=NULL),
                    
                    numericInput(inputId = "Platelet_count", label = "Platelet count (10^9/L)", value = NULL),
                    numericInput(inputId = "WBCcount", label = "White blood cell count (10^9/L)", value = NULL),
                    radioButtons(inputId = "Temozolomide", label = "Treated with Temozolomide", c("Yes", "No"), selected = character(0))
                    
                ),
                
                box(title = "Tumor Markers", status = "primary", solidHeader = TRUE,
                    collapsible = FALSE, height = 350,
                    radioButtons(inputId = "IDH", label = "IDH Mutation", c("Yes", "No"), selected = character(0)),
                    radioButtons(inputId = "MGMT", label = "MGMT promoter methylation", c("Yes", "No"), selected = character(0)),
                    radioButtons(inputId = "WHOgrade", label = "WHO grade of original glioma", c("II","III","IV"), selected = character(0))
                    ),
                
                box(title = "History of the following?", status = "primary", solidHeader = TRUE,
                    collapsible = FALSE, height = 280,
                    
                    "At diagnosis of original glioma", br(), "",
                    
                    br(),
                    
                    radioButtons(inputId = "Hypertension", label = "Hypertension", c("Yes", "No"), selected = character(0)),
                    radioButtons(inputId = "Hypothyroidism", label = "Hypothyroidism", c("Yes", "No"), selected = character(0))
                    
                    )
              )
              

      ),
      
          
      
      # visualize tab content
      tabItem(tabName = "visualize",
              
              fluidRow(
                box(title = "Controls",
                    width = 4,
                    collapsible = TRUE, status = "primary", solidHeader = TRUE,
                    "Select the variable you would like to see plotted in the bar graph to the right.",   br(), br(),  "",

                    selectInput(inputId = "contvarplot", label = "Comparison Variable ",
                                choices = c(Choose = '',contvars),
                                multiple = FALSE)),
                
                
                box(title = "Explore Training Cohort by WHO Grade",  
                    width=7,
                    collapsible = TRUE, status = "primary", solidHeader = TRUE,
                    plotlyOutput("plotbar_byGrade")
                )
              ),
              
              
              fluidRow(
                box(title = "PCA", 
                    width = 11,
                    collapsible = TRUE, solidHeader = TRUE, status = "success",
                    "PCA low-dimensional representation that explain variance",
                    plotlyOutput("plot3D"))
              ),
              
              fluidRow(
                box(title = "Kaplan-Meier Plot", 
                    width = 11, 
                    collapsible = TRUE, status = "warning", solidHeader = TRUE,
                    plotOutput("plotKM", height = 400))
              ),

              
      
      ),
      
      # references tab content
      tabItem(tabName = "references",
              h2("List references HERE"), br(),
              tags$li(tags$ul("Resource for predictSurvProb for prediction with Cox model found",
                              tags$a(href =  "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4194196/", "here"))),
              
              
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
                              predictors, please view the 'Visualize' tab.")),
              
              
              br(),
              
              
              #======  Help  ======#
              titlePanel("Help"),
              p("Resources for help."),
              tags$li(tags$ul("Inference using LASSO has been proposed in this", tags$a(href = "https://www.jstor.org/stable/23239544?seq=12#metadata_info_tab_contents", "paper"), "by Minnier, Tian, and Cai.")),
              tags$li(tags$ul("In order to answer our prediction problem, LASSO was used in order to propose a model and appropriately shrink.")),
              tags$li(tags$ul("Similar paper 'Feature selection and survival modeling in The Cancer Genome Atlas' in GBMs", tags$a(href =  "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3790279/", "here")))
              
              
    
      )
    )
  ),
  
  
  
  ## Footnote content
 dashboardFooter(left_text = "VTEpredictionGlioma" ,right_text = tags$img(src='FeinbergLogo.png',height='35',width='150'))
)







#==========================================================#
#================           SERVER               ==========#
#==========================================================#
server <- function(input, output) {

  
  #=============      Predicted table        ============#
  output$Prediction_table <- renderTable({
    
    
    validate(
      need(input$Age != '' | input$Sex != '' |  input$BMI != '' | input$WBCcount != '' | input$Platelet_count != '' 
           | input$Temozolomide != '' | input$IDH != '' | input$MGMT != '' | input$WHOgrade != '' | input$Hypertension != '' | 
             input$Hypothyroidism != '' 
           , 'Please input all clinical and tumor characteristics below.')
    )
    
    
    entered_dat <- data.frame(input$Age, input$Sex, input$BMI,  input$WBCcount, input$Platelet_count,
                              input$Temozolomide, input$IDH, input$MGMT, input$WHOgrade, input$Hypertension, input$Hypothyroidism) %>%
      dplyr::rename("Age" = "input.Age", "Sex" = "input.Sex", "BMI" = "input.BMI", 
                    "WBCcount" = "input.WBCcount", "Platelet_count" = "input.Platelet_count", "Temozolomide" = "input.Temozolomide", "IDH" = "input.IDH", "MGMT" = "input.MGMT", 
                    "WHOgrade" = "input.WHOgrade", "Hypertension" = "input.Hypertension", "Hypothyroidism" = "input.Hypothyroidism") %>%
      dplyr::mutate(Sex= factor(Sex, levels = c("Male","Female")),
                    WHOgrade = factor(WHOgrade))
    

    ##==== use predictSurvProb (pec package)   ===#
    fit1 <- predfit1  #this is the coxph model output
    
    psurv <- 1- pec::predictSurvProb(fit1, newdata = entered_dat, times = c(1,3,6,12))  # Probability of event (cumulative incidence 1-surv) instead of survival
    
    psurv2 <- data.frame(psurv)
    names(psurv2) <- c("1 Month", "3 Months", "6 Months", "12 Months")
    
    psurv2
  })
  
  
  
  
  
  #=============      Plot by WHO Grade        ============#
  output$plotbar_byGrade <- renderPlotly({
    
    validate(
      need(input$contvarplot != '', 'Please choose a comparison variable.')
    )
    
  if(input$contvarplot == "Age"){
    allWHOplots$ageWHOgrade
  } else {
    if (input$contvarplot == "BMI"){
      allWHOplots$BMIWHOgrade
    } else {
      if (input$contvarplot == "WBCcount"){
        allWHOplots$WBC_WHOgrade
      } else {
        allWHOplots$platelet_WHOgrade
      }
      
  } }
    
  })

  #=============      PCA plot 3D        ============#
  output$plot3D <- renderPlotly({
    pcpoints <- PCAplotData$pcpoints
    pcout <- PCAplotData$pcout
    
    plotly::plot_ly(pcpoints, x=~PC1, y=~PC2, z=~PC3,
            color = ~pcadatRAW.VTE) %>%
      add_markers() %>%
      layout(scene = list(xaxis = list(title = paste0("PC1 (", round(pcout["Proportion of Variance", "PC1"] *100,1) , "%)")),
                          yaxis = list(title = paste0("PC2 (", round(pcout["Proportion of Variance", "PC2"] *100,1) , "%)")),
                          zaxis = list(title = paste0("PC3 (", round(pcout["Proportion of Variance", "PC3"] *100,1) , "%)"))))
    
  })
  
  
  #=============      KM Plot       ============#
  output$plotKM <- renderPlot({
    kmplotdat$ggsurv1
  })
  
  

  
}

shinyApp(ui, server)