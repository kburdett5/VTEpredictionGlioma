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

# https://shiny.rstudio.com/gallery/
# https://shiny.rstudio.com/gallery/radiant.html


#Installing/Loading Packages

# if(!require(shiny)){
#   install.packages("shiny")
#   library(shiny)
# }

library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(tidyverse)
library(plotly)




#==========================================================#
#===============        Read in data             ==========#
#==========================================================#

dir <- "/Volumes/fsmresfiles/PrevMed/Projects/Brain_SPORE_BB_Core/Projects/Horbinski/Thrombosis/Code/VTEpredictionGlioma/"

readfile <- file.path(dir,"predict_VTE_app/analysisData_RShiny_2020-07-15.csv")  # data from training population that selected predictors
rawdat <- read_csv(readfile) 

readcoef <- file.path(dir, "predict_VTE_app/LASSOcoef_2020-07-15.csv")  # coefficients from training data
coefdat <- read_csv(readcoef)
  

#=========     clean data     ========#
clindat <- rawdat %>% 
  dplyr::mutate(VTE = recode(iVTE, !!!c("1" = "Yes VTE", "0"= "No VTE")),
                Sex_num = recode(Sex , !!!c("Male" = 1,  "Female" = 0)),
                Hypertension_num = recode(Hypertension, !!!c("Yes" = 1,  "No" = 0)),
                Hypothyroidism_num = recode(Hypothyroidism, !!!c("Yes" = 1,  "No" = 0)),
                IDH_num = recode(IDH, !!!c("Yes" = 1,  "No" = 0)),
                MGMT_num = recode(MGMT, !!!c("Yes" = 1, "No" = 0)),
                TMZ_num = recode(TMZ, !!!c("Yes" = 1,  "No" = 0)),
                Current_Smoker_num = recode(Current_Smoker, !!!c("Yes" = 1,  "No" = 0)),
                WHOgrade_char = recode(WHOgrade, !!!c("2" = "Grade 2", "3" = "Grade 3", "4" = "Grade 4")))

catvars <- c("Sex", "Hypertension", "Hypothyroidism", "IDH", "MGMT", "TMZ", "Current_Smoker")
contvars <- c("Age", "BMI", "WBCcount")


#======    PCA    =======#
library(missMDA)
predictorvars <- c("Age", "BMI", "Sex_num", "WBCcount", "Hypertension_num", "Hypothyroidism_num", "IDH_num",
                   "MGMT_num", "TMZ_num", "WHOgrade", "Current_Smoker_num")

imputePCAout <- imputePCA(as.data.frame(clindat[,predictorvars]), method = "EM", ncp=1)

pca <- prcomp(imputePCAout$completeObs, center = TRUE, scale. = TRUE)
pcpoints <- data.frame(clindat$VTE, pca$x)
pcout <- summary(pca)$importance












#==========================================================#
#================            UI                  ==========#
#==========================================================#
ui <- dashboardPagePlus(skin = "purple",
  dashboardHeader(title = "VTE Prediction"),
  dashboardSidebar(sidebarMenu(
    menuItem("Home", tabName = "home", icon = icon("home")),
    menuItem("Prediction", tabName = "prediction"),
    menuItem("Visualize", tabName = "visualize"),
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
              tags$li(tags$ul("Inference using LASSO has been proposed in this", tags$a(href = "https://www.jstor.org/stable/23239544?seq=12#metadata_info_tab_contents", "paper"), "by Minnier, Tian, and Cai.")),
              tags$li(tags$ul("In order to answer our prediction problem, LASSO was used in order to propose a model and appropriately shrink.")),
              tags$li(tags$ul(tags$a(href= "https://www.stat.berkeley.edu/~aldous/157/Papers/shmueli.pdf",
                                     "https://www.stat.berkeley.edu/~aldous/157/Papers/shmueli.pdf"))),
              tags$li(tags$ul(tags$a(href="https://www.datascienceblog.net/post/commentary/inference-vs-prediction/",
                                     "https://www.datascienceblog.net/post/commentary/inference-vs-prediction/"))),
              p("Resources for help.")
              
      ),
      
      # Prediction tab content
      tabItem(tabName = "prediction",
              
              # Application title
              titlePanel("VTE Prediction for Glioma Patients"),
              p("Patient and tumor characteristics at diagnosis of original glioma"),
              br(),
              
              
              fluidRow(
                box(title = "Clinical Characteristics", status= "primary", solidHeader = TRUE,
                    collapsible = TRUE, height = 700,
                    
                    "At diagnosis of original glioma",  br(), br(),  "",
                    
                    numericInput(inputId = "Age", label = "Age (Years)", min=0, max=100, value=NULL),
                    
                    selectInput(inputId = "Sex", label = "Sex",
                                choices = c(Choose = '',c("Male" , "Female")),
                                multiple = FALSE),
                    
                    radioButtons(inputId = "Current_Smoker", label = "Current Smoker", c("Yes", "No"), selected = character(0)),
                    
                    sliderInput('BMI', 'BMI', 0,50,1),
                    
                    numericInput(inputId = "Platelet_count", label = "Platelet count (10^9/L)", value = NULL),
                    numericInput(inputId = "WBCcount", label = "White blood cell count (10^9/L)", value = NULL),
                    radioButtons(inputId = "TMZ", label = "Treated with TMZ", c("Yes", "No"), selected = character(0))
                    
                ),
                
                box(title = "Molecular Markers", status = "primary", solidHeader = TRUE,
                    collapsible = TRUE, height = 350,
                    radioButtons(inputId = "IDH", label = "IDH Mutation", c("Yes", "No"), selected = character(0)),
                    radioButtons(inputId = "MGMT", label = "MGMT methylation", c("Yes", "No"), selected = character(0)),
                    radioButtons(inputId = "WHOgrade", label = "WHO grade of original glioma", c(2,3,4), selected = character(0))
                    ),
                
                box(title = "History of the following?", status = "primary", solidHeader = TRUE,
                    collapsible = TRUE, height = 330,
                    
                    "At diagnosis of original glioma", br(), "",
                    
                    br(),
                    
                    radioButtons(inputId = "Hypertension", label = "Hypertension", c("Yes", "No"), selected = character(0)),
                    radioButtons(inputId = "Hypothyroidism", label = "Hypothyroidism", c("Yes", "No"), selected = character(0))
                    
                    )
              ),
              

              br(),
              br(),
              
              
              titlePanel("Prediction Probabilities"),
              tags$li(tags$ul("1, 3, 6, 12 months")),
              tags$li(tags$ul("Predicted probabilities from Cox regression."))
           
      ),
      
          
      
      # visualize tab content
      tabItem(tabName = "visualize",
              
              fluidRow(
                box(title = "Controls",
                    width = 4,
                    collapsible = TRUE, status = "warning", solidHeader = TRUE,
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
                box(title = "Controls",
                    width = 4,
                    collapsible = TRUE, status = "warning", solidHeader = TRUE,
                    "Select the variable you would like to see plotted in the bar graph to the right.",   br(), br(),  "",

                    selectInput(inputId = "contvarplot2", label = "Comparison Variable",
                                choices = c(Choose = '',contvars),
                                multiple = FALSE)),
                
                box(title = "Explore Training Cohort", 
                    width=7,
                    collapsible = TRUE, status = "warning", solidHeader = TRUE,
                    plotlyOutput("plotbar", height = 300))
              ),
              
              
              
              fluidRow(
                box(title = "PCA", 
                    width = 11,
                    collapsible = TRUE, solidHeader = TRUE, status = "success",
                    "PCA low-dimensional representation that explain variance",
                    plotlyOutput("plot3D"))
              )

              
      
      ),
      
      # references tab content
      tabItem(tabName = "references",
              h2("List references HERE")
    
      )
    )
  ),
  
  
  
  ## Footnote content
 dashboardFooter(left_text = "VTE Prediction" ,right_text = tags$img(src='FeinbergLogo.png',height='35',width='150'))
)







#==========================================================#
#================           SERVER               ==========#
#==========================================================#
server <- function(input, output) {

  
  
  output$plotbar_byGrade <- renderPlotly({
    
    validate(
      need(input$contvarplot != '', 'Please choose a comparison variable.')
    )
    
  datanew <- clindat %>%
    dplyr::select(one_of(c("VTE",  "WHOgrade_char", "IDH", input$contvarplot))) %>%
    gather(Attribute, value, -VTE, -WHOgrade_char, -IDH)
    
  ggplot(data = datanew, aes(x = IDH, y = value, fill=VTE)) +
      geom_bar(stat = "identity",  position=position_dodge()) + 
      facet_wrap( ~ WHOgrade_char) + ylab(unique(datanew$Attribute))
  })
  
  
  output$plotbar <- renderPlotly({
    
    validate(
      need(input$contvarplot2 != '', 'Please choose a comparison variable.')
    )
    
    datanew2 <- clindat %>%
      dplyr::select(one_of(c("VTE",  "IDH", input$contvarplot2))) %>%
      gather(Attribute, value, -VTE, -IDH)
    
    ggplot(data = datanew2, aes(x = IDH, y = value, fill=VTE)) +
      geom_bar(stat = "identity",  position=position_dodge()) + ylab(unique(datanew2$Attribute))
  })
  
  
  output$plot3D <- renderPlotly({
    plot_ly(pcpoints, x=~PC1, y=~PC2, z=~PC3,
            color = ~clindat.VTE) %>%
      add_markers() %>%
      layout(scene = list(xaxis = list(title = paste0("PC1 (", round(pcout["Proportion of Variance", "PC1"] *100,1) , "%)")),
                          yaxis = list(title = paste0("PC2 (", round(pcout["Proportion of Variance", "PC2"] *100,1) , "%)")),
                          zaxis = list(title = paste0("PC3 (", round(pcout["Proportion of Variance", "PC3"] *100,1) , "%)"))))
    
    #layout(legend=list(title=list(text='<b> VTE </b>')))
  })
  

  
}

shinyApp(ui, server)