############## CSE 674 Project 1 and 2 combined code ##############
## Authors: 
## 1. KARTHIK CHAGANTI :  kchagant :50169441
## 2. VAIBHAV LELLA    :  vaibhavl :50169859
###################################################################


#source("http://bioconductor.org/biocLite.R")
#biocLite("RBGL")
#biocLite("Rgraphviz")
#install.packages("gRain")
#install.packages("bnlearn")
#install.packages("caTools")
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://shiny.rstudio.com

#install.packages("DiagrammeR")
# install.packages("markdown")
# install.packages("googleVis")
# install.packages("ggplot2")
library("shinythemes")
library(Rgraphviz)
library(gRain)
library(bnlearn)
library(caTools)
library(DiagrammeR)
library(ggplot2)
library(googleVis)
library(markdown)
shinyUI(navbarPage("AML - CSE674: Probabilistic Graphical Models",theme="bootstrap.css",
                tabPanel("Bayesian Network", 
                         titlePanel("ESTIMATION OF RISK POSED BY CAR INSURANCE SEEKER" ),br(),br(),
                 sidebarLayout(
                      sidebarPanel(br(),wellPanel(
                       
                        selectInput("evidence_input", "Evidence Variables:",selectize = TRUE,
                                    c("Choose One"="","GoodStudent", "Age", "SocioEcon","RiskAversion","VehicleYear","ThisCarDam","RuggedAuto","Accident","MakeModel",
                                      "DrivQuality","Mileage","Antilock","DrivingSkill","SeniorTrain","ThisCarCost","Theft","CarValue","HomeBase",
                                      "AntiTheft","PropCost","OtherCarCost","OtherCar","MedCost","Cushioning","Airbag","ILiCost","DrivHist"
                                    )),
                        
                        
                        # This outputs the dynamic UI component
                        uiOutput("ui"),
                        actionButton("gobutton","Add")
                      ),
                      wellPanel(
                        selectInput("target_input","Target Variable Marginal Distribution:",selectize = TRUE,
                                    c("Choose One"="","GoodStudent", "Age", "SocioEcon","RiskAversion","VehicleYear","ThisCarDam","RuggedAuto","Accident","MakeModel",
                                      "DrivQuality","Mileage","Antilock","DrivingSkill","SeniorTrain","ThisCarCost","Theft","CarValue","HomeBase",
                                      "AntiTheft","PropCost","OtherCarCost","OtherCar","MedCost","Cushioning","Airbag","ILiCost","DrivHist"
                                    )
                                    
                        )
                      ),
                      wellPanel(
                        selectInput("target_input_joint","Target Variable Joint Distribution:",multiple=TRUE,selectize = TRUE,
                                    c("Choose One"="","GoodStudent", "Age", "SocioEcon","RiskAversion","VehicleYear","ThisCarDam","RuggedAuto","Accident","MakeModel",
                                      "DrivQuality","Mileage","Antilock","DrivingSkill","SeniorTrain","ThisCarCost","Theft","CarValue","HomeBase",
                                      "AntiTheft","PropCost","OtherCarCost","OtherCar","MedCost","Cushioning","Airbag","ILiCost","DrivHist"
                                    )
                                    
                        )
                      ),
                      wellPanel(
                        selectInput("target_input_cond","Target Variable Conditional Distribution:",multiple=TRUE,selectize = TRUE,
                                    c("Choose One"="","GoodStudent", "Age", "SocioEcon","RiskAversion","VehicleYear","ThisCarDam","RuggedAuto","Accident","MakeModel",
                                      "DrivQuality","Mileage","Antilock","DrivingSkill","SeniorTrain","ThisCarCost","Theft","CarValue","HomeBase",
                                      "AntiTheft","PropCost","OtherCarCost","OtherCar","MedCost","Cushioning","Airbag","ILiCost","DrivHist"
                                    )
                                    
                        )
                      ),
                      actionButton("reset", "Reset")
                     
                     
                      
                      ),
                      mainPanel(
                        tags$style(type="text/css",
                                   ".shiny-output-error { visibility: hidden; }",
                                   ".shiny-output-error:before { visibility: hidden; }"
                        ),
                        h4(strong("Selected Evidences:")),
                        
                                verbatimTextOutput("evidence_input_text"),
                                h4(strong("Selected States:")),
                                verbatimTextOutput("dynamic_value"),
                                br(),
                                tabsetPanel(
                                  tabPanel("Bayesian Network",
                                #plotOutput("bayesnet",width="100%", height = "1200px")),
                                
                                uiOutput("bayesnet_main")),
                                tabPanel("Marginal Distribution",
                                         br(),
                                        fluidRow(
                                          column(4,h4(strong("CPT Without Evidence")),uiOutput("bayes_extra"))
                                         ),
                                        fluidRow(
                                          column(4,h4(strong("CPT With Evidence")),uiOutput("bayes")),
                                          column(4,htmlOutput("pie"))
                                          )
                                        ),
                                tabPanel("Joint Distribution",
                                         br(),
                                         fluidRow(
                                           column(4,h4(strong("CPT Without Evidence")),uiOutput("bayes_joint_extra"))
                                         ),
                                         fluidRow(
                                           column(4,h4(strong("CPT With Evidence")),uiOutput("bayes_joint"))
                                         )
                                ),
                                tabPanel("Conditional Distribution",
                                         br(),
                                         fluidRow(
                                           column(4,h4(strong("CPT Without Evidence")),uiOutput("bayes_cond_extra"))
                                         ),
                                         fluidRow(
                                           column(4,h4(strong("CPT With Evidence")),uiOutput("bayes_cond"))
                                         )
                                )
                                  
                                )
                      )
                 
                 
                    )
                  ),
                tabPanel("Markov Chain",titlePanel("CONSECUTIVE WORD PREDICTION"),
                         
                         br(),
                         fluidPage(
                           fluidRow(
                            
                             column(3,
                                    h4(strong("Phrase Input")),
                                    textInput("inputphrase", label=NULL, value=""), 
                                    #radioButtons("inputchoice", "Please choose whether to:", c("Predict a single word (as required for the Capstone Project)"="one", "Provide you a selection of four words (additional functionality)" = "four")),
                                    actionButton("goButton1", "Submit!"),
                                    br()
                             ),
                             column(3,
                                    h4(strong("Word Prediction Output")),
                                    wellPanel(
                                    textOutput("outputtop1"))
                             ),
                             column(3, h4(strong("Confidence of probable words")),
                                    wellPanel(
                                      tableOutput("outputtop2")
                                    ))
                             
                             ),
                           br(),

                           fluidRow(
                             tabsetPanel(
                               tabPanel("Word Clouds",
                                        column(4,
                                               h4(strong("Bi-gram")),
                                               plotOutput("outputbottom23")),
                                        column(4,
                                               h4(strong("Tri-gram")),
                                               plotOutput("outputbottom22")),
                                        column(4,
                                               h4(strong("Quad-gram")),
                                               plotOutput("outputbottom21")) 
                                        
                               )
                             )
                           ))
                         
                         
                )
                # tabPanel("Documentation",
                #          tabsetPanel(
                #            tabPanel("Bayesian Network:",
                #          tags$iframe(src="quad1.pdf", width="1900", height="800")),
                #          tabPanel("Markov Chain:",
                #                   tags$iframe(src="quad1.pdf", width="1900", height="800"))
                #          
                #          ))
                
                ))