############## CSE 674 Project 1 and 2 Combined Code ##############
## Authors: 
## 1. KARTHIK CHAGANTI :  kchagant :50169441
## 2. VAIBHAV LELLA    :  vaibhavl :50169859
###################################################################
# install.packages("data.table")
# install.packages("tm")
# install.packages("wordcloud")
library(Rgraphviz)
library(gRain)
library(bnlearn)
library(caTools)
library(DiagrammeR)
library(igraph)
library(shiny)
library(data.table)
library(tm)
library(wordcloud)
library(ggplot2)

# Process functions
source('Process.R')

# Load the necessary tables
con1 <- file(('./freqs/freq_4grams.txt'), open="r")
dt1 <- as.data.table(read.csv(con1))
setkey(dt1, prefix)
close(con1)

con1 <- file(('./freqs/freq_3grams.txt'), open="r")
dt2 <- as.data.table(read.csv(con1))
setkey(dt2, prefix)
close(con1) 

con1 <- file(('./freqs/freq_2grams.txt'), open="r")
dt3 <- as.data.table(read.csv(con1))
setkey(dt3, prefix)
close(con1) 

for (i in 1:3){
  con1 <- file(paste0('./freqs/freq_',5-i,'grams_mini.txt'), open="r")
  dttemp <- as.data.table(read.csv(con1))
  setkey(dttemp, prefix)
  assign(paste0("dt",i, "mini"), dttemp, '.GlobalEnv')
  close(con1)  
}




shinyServer(function(input, output) {
  
    withProgress({
      setProgress(message="Please Wait")
    
  # Prediction of phrase
  inputcopy <- eventReactive(input$goButton1, {input$inputphrase})
  dttemp <- reactive({as.data.frame(findanswer(inputcopy(), dt1, dt2, dt3))})

  outputtop1 <- eventReactive(input$goButton1, {
    #if (input$inputchoice=="one") {
      paste0(inputcopy()," ", as.character(dttemp()[1,1]))
    #}
  })
  output$outputtop1 <- renderText({outputtop1()})
  
  outputtop2 <- eventReactive(input$goButton1, {
   # if (input$inputchoice=="four"){
      dttemp()
     # paste0(inputcopy()," ", as.character(dttemp()[1,1]))
   #  }
  })
  
  output$outputtop2 <- renderTable({outputtop2()})
  
  
  # Visualisation of word cloud
  # wordcloud1 <- eventReactive(input$goButton1, {
  #   wordcloud(paste0(dt1$prefix, " ", dt1$answer), dt1$freq, max.words=50, scale=c(3, 0.5), colors=brewer.pal(5, "Set2"))})
  # wordcloud2 <- eventReactive(input$goButton1, {
  #   wordcloud(paste0(dt2$prefix, " ", dt2$answer), dt2$freq, max.words=50, scale=c(3, 0.5), colors=brewer.pal(5, "Set2"))})
  # wordcloud3 <- eventReactive(input$goButton1, {
  #   wordcloud(paste0(dt3$prefix, " ", dt3$answer), dt3$freq, max.words=50, scale=c(3, 0.5), colors=brewer.pal(5, "Set2"))})
 
  output$outputbottom21 <- renderPlot({
                              withProgress(
                                {
                                  setProgress(message= "Please Wait..")
                              wordcloud(paste0(dt1$prefix, " ", dt1$answer), dt1$freq, max.words=50, scale=c(3, 0.5), colors=brewer.pal(5, "Set2"))
                                })
                                    })
  
  output$outputbottom22 <- renderPlot({
    withProgress(
      {
        setProgress(message= "Please Wait..")
                              wordcloud(paste0(dt2$prefix, " ", dt2$answer), dt2$freq, max.words=50, scale=c(3, 0.5), colors=brewer.pal(5, "Set2"))
      })   })
  
  output$outputbottom23 <- renderPlot({
    withProgress(
      {
        setProgress(message= "Please Wait..")
                              wordcloud(paste0(dt3$prefix, " ", dt3$answer), dt3$freq, max.words=50, scale=c(3, 0.5), colors=brewer.pal(5, "Set2"))
      })      })
  
    
  
  
  
  
  
  ######################################################################
  globall <- reactiveValues()
  globall$state_variable <- c("")
  globall$evidence_variable <- c("")
  globall$nAttrs <- list()
  output$ui <- renderUI({
    if (is.null(input$evidence_input))
      return()
    
    # Depending on input$evidence_input, we'll generate a different
    # UI component and send it to the client.
    switch(input$evidence_input,
           
           
           "GoodStudent" = radioButtons("dynamic", "Dynamic",
                                        choices = c("True" = "True",
                                                    "False" = "False"),
                                        selected = "True"
           ),
           "Age" = radioButtons("dynamic", "Dynamic",
                                choices = c("Adolescent" = "Adolescent",
                                            "Adult" = "Adult",
                                            "Senior" = "Senior"),
                                selected = "Adolescent"
           ),
           "SocioEcon" = radioButtons("dynamic", "Dynamic",
                                      choices = c("Prole" = "Prole",
                                                  "Middle" = "Middle",
                                                  "UpperMiddle" = "UpperMiddle",
                                                  "Wealthy" = "Wealthy"),
                                      selected = "Prole"
           ),
           "RiskAversion" = radioButtons("dynamic", "Dynamic",
                                         choices = c("Psychopath" = "Psychopath",
                                                     "Adventurous" = "Adventurous",
                                                     "Normal" = "Normal",
                                                     "Cautious" = "Cautious"),
                                         selected = "Psychopath"
           ),
           "SocioEcon" = radioButtons("dynamic", "Dynamic",
                                      choices = c("Prole" = "Prole",
                                                  "Middle" = "Middle",
                                                  "UpperMiddle" = "UpperMiddle",
                                                  "Wealthy" = "Wealthy"),
                                      selected = "Prole"
           ),
           "VehicleYear" = radioButtons("dynamic", "Dynamic",
                                        choices = c("Current" = "Current",
                                                    "Older" = "Older"),
                                        selected = "Current"
           ),
           "ThisCarDam" = radioButtons("dynamic", "Dynamic",
                                       choices = c("None" = "None",
                                                   "Mild" = "Mild",
                                                   "Moderate" = "Moderate",
                                                   "Severe" = "Severe"),
                                       selected = "None"
           ),
           "RuggedAuto" = radioButtons("dynamic", "Dynamic",
                                       choices = c("EggShell" = "EggShell",
                                                   "Football" = "Football",
                                                   "Tank" = "Tank"),
                                       selected = "EggShell"
           ),
           "Accident" = radioButtons("dynamic", "Dynamic",
                                     choices = c("None" = "None",
                                                 "Mild" = "Mild",
                                                 "Moderate" = "Moderate",
                                                 "Severe" = "Severe"),
                                     selected = "None"
           ),
           "MakeModel" = radioButtons("dynamic", "Dynamic",
                                      choices = c("SportsCar" = "SportsCar",
                                                  "Economy" = "Economy",
                                                  "FamilySedan" = "FamilySedan",
                                                  "Luxury" = "Luxury",
                                                  "SuperLuxury" = "SuperLuxury"),
                                      selected = "SportsCar"
           ),
           "DrivQuality" = radioButtons("dynamic", "Dynamic",
                                        choices = c("Poor" = "Poor",
                                                    "Normal" = "Normal",
                                                    "Excellent" = "Excellent"),
                                        selected = "Poor"
           ),
           "Mileage" = radioButtons("dynamic", "Dynamic",
                                    choices = c("FiveThou" = "FiveThou",
                                                "TwentyThou" = "TwentyThou",
                                                "FiftyThou" = "FiftyThou",
                                                "Domino" = "Domino"),
                                    selected = "FiveThou"
           ),
           "Antilock" = radioButtons("dynamic", "Dynamic",
                                     choices = c("True" = "True",
                                                 "False" = "False"),
                                     selected = "True"
           ),
           "DrivingSkill" = radioButtons("dynamic", "Dynamic",
                                         choices = c("SubStandard" = "SubStandard",
                                                     "Normal" = "Normal",
                                                     "Expert" = "Expert"),
                                         selected = "SubStandard"
           ),
           "SeniorTrain" = radioButtons("dynamic", "Dynamic",
                                        choices = c("True" = "True",
                                                    "False" = "False"),
                                        selected = "True"
           ),
           "ThisCarCost" = radioButtons("dynamic", "Dynamic",
                                        choices = c("Thousand" = "Thousand",
                                                    "TenThou" = "TenThou",
                                                    "HundredThou" = "HundredThou",
                                                    "Million" = "Million"),
                                        selected = "Thousand"
           ),
           "Theft" = radioButtons("dynamic", "Dynamic",
                                  choices = c("True" = "True",
                                              "False" = "False"),
                                  selected = "True"
           ),
           "CarValue" = radioButtons("dynamic", "Dynamic",
                                     choices = c("FiveThou" = "FiveThou",
                                                 "TenThou" = "TenThou",
                                                 "TwentyThou" = "TwentyThou",
                                                 "FiftyThou" = "FiftyThou",
                                                 "Million" = "Million"),
                                     selected = "FiveThou"
           ),
           "HomeBase" = radioButtons("dynamic", "Dynamic",
                                     choices = c("Secure" = "Secure",
                                                 "City" = "City",
                                                 "Suburb" = "Suburb",
                                                 "Rural" = "Rural"),
                                     selected = "Secure"
           ),
           "AntiTheft" = radioButtons("dynamic", "Dynamic",
                                      choices = c("True" = "True",
                                                  "False" = "False"),
                                      selected = "True"
           ),
           "PropCost" = radioButtons("dynamic", "Dynamic",
                                     choices = c("Thousand" = "Thousand",
                                                 "TenThou" = "TenThou",
                                                 "HundredThou" = "HundredThou",
                                                 "Million" = "Million"),
                                     selected = "Thousand"
           ),
           "OtherCarCost" = radioButtons("dynamic", "Dynamic",
                                         choices = c("Thousand" = "Thousand",
                                                     "TenThou" = "TenThou",
                                                     "HundredThou" = "HundredThou",
                                                     "Million" = "Million"),
                                         selected = "Thousand"
           ),
           "OtherCar" = radioButtons("dynamic", "Dynamic",
                                     choices = c("True" = "True",
                                                 "False" = "False"),
                                     selected = "True"
           ),
           "MedCost" = radioButtons("dynamic", "Dynamic",
                                    choices = c("Thousand" = "Thousand",
                                                "TenThou" = "TenThou",
                                                "HundredThou" = "HundredThou",
                                                "Million" = "Million"),
                                    selected = "Thousand"
           ),
           "Cushioning" = radioButtons("dynamic", "Dynamic",
                                       choices = c("Poor" = "Poor",
                                                   "Fair" = "Fair",
                                                   "Good" = "Good",
                                                   "Excellent" = "Excellent"),
                                       selected = "Poor"
           ),
           "Airbag" = radioButtons("dynamic", "Dynamic",
                                   choices = c("True" = "True",
                                               "False" = "False"),
                                   selected = "True"
           ),
           "ILiCost" = radioButtons("dynamic", "Dynamic",
                                    choices = c("Thousand" = "Thousand",
                                                "TenThou" = "TenThou",
                                                "HundredThou" = "HundredThou",
                                                "Million" = "Million"),
                                    selected = "Thousand"
           ),
           "DrivHist" = radioButtons("dynamic", "Dynamic",
                                     choices = c("Zero" = "Zero",
                                                 "One" = "One",
                                                 "Many" = "Many"),
                                     selected = "Zero"
           )
    )
    
  })
  
  
  output$evidence_input_text <- renderText({
    text_en()
  })
  
  text_en<- eventReactive(input$gobutton,{
    globall$evidence_variable <- c(input$evidence_input,globall$evidence_variable)
  })
  
  output$dynamic_value <- renderText({
    text_en_1()
  })
  
  text_en_1<- eventReactive(input$gobutton,{
    globall$state_variable <- c(input$dynamic,globall$state_variable)
  })
  
  
  # globall$state_variable <- renderText({
  # paste(input$dynamic,globall$state_variable, sep=",")
  # })
  # 
  # 
  # output$dynamic_value <- renderPrint({(globall$state_variable)})
  
  
  
  net1 = read.net("insurance.net")
  bn2 = as.grain(net1)
  bn1 = bn.net(net1)
  
  output$bayes <- renderUI({
    
    bn3 <- setFinding(bn2, nodes=globall$evidence_variable, states=globall$state_variable) 
    prob_table <- as.data.frame(querygrain(bn3, nodes=input$target_input))
    output$viewTable <- renderTable(prob_table)
    tableOutput("viewTable")
  })

  output$bayes_extra <- renderUI({
    bn3_extra <- setFinding(bn2, nodes=c(""), states=c("")) 
    prob_table_extra <- as.data.frame(querygrain(bn3_extra, nodes=input$target_input))
    output$viewTable_extra <- renderTable(prob_table_extra)
    tableOutput("viewTable_extra")
  })  
  output$bayes_joint <- renderUI({
    bn3_joint <- setFinding(bn2, nodes=globall$evidence_variable, states=globall$state_variable) 
    prob_table_joint <- as.data.frame(querygrain(bn3_joint, nodes=input$target_input_joint,type="joint"))
    output$viewTable_joint <- renderTable(prob_table_joint)
    tableOutput("viewTable_joint")
  })
  
  output$bayes_joint_extra <- renderUI({
    bn3_joint_extra <- setFinding(bn2, nodes=c(""), states=c("")) 
    prob_table_joint_extra <- as.data.frame(querygrain(bn3_joint_extra, nodes=input$target_input_joint,type="joint"))
    output$viewTable_joint_extra <- renderTable(prob_table_joint_extra)
    tableOutput("viewTable_joint_extra")
  })
  output$bayes_cond <- renderUI({
    bn3_cond <- setFinding(bn2, nodes=globall$evidence_variable, states=globall$state_variable) 
    prob_table_cond <- as.data.frame(querygrain(bn3_cond, nodes=input$target_input_cond,type="conditional"))
    output$viewTable_cond <- renderTable(prob_table_cond)
    tableOutput("viewTable_cond")
  })
  
  output$bayes_cond_extra <- renderUI({
    bn3_cond_extra <- setFinding(bn2, nodes=c(""), states=c("")) 
    prob_table_cond_extra <- as.data.frame(querygrain(bn3_cond_extra, nodes=input$target_input_cond,type="conditional"))
    output$viewTable_cond_extra <- renderTable(prob_table_cond_extra)
    tableOutput("viewTable_cond_extra")
  })

    but_target <- eventReactive(input$target_input,{
    bn3 <- setFinding(bn2, nodes=globall$evidence_variable, states=globall$state_variable)
    prob_table <- as.data.frame(querygrain(bn3, nodes=input$target_input))
      })
    but_target_extra <- eventReactive(input$target_input,{
      bn3 <- setFinding(bn2, nodes=c(""), states=c(""))
      prob_table <- as.data.frame(querygrain(bn3, nodes=input$target_input))
    })

    output$pie <- renderGvis(
      {


        bayes_pie <- but_target()
        bayes_pie_extra <- but_target_extra()
        pie_data <- as.data.frame(as.table(bayes_pie[[input$target_input]]))
        pie_data_extra <- as.data.frame(as.table(bayes_pie_extra[[input$target_input]]))
        doughnut <- gvisPieChart(pie_data,
                                 options=list(
                                   width=300,
                                   height=300,
                                   slices="{0: {offset: 0.2},
                                   1: {offset: 0.2},
                                   2: {offset: 0.2}}",
                                   title='Inference Pie',
                                   legend='none',
                                   colors="['black','orange', 'blue',
                                   'red', 'purple', 'green']",
                                   pieSliceText='label',
                                   pieHole=0.5),
                                 chartid="doughnut")
        doughnut_extra <- gvisPieChart(pie_data_extra,
                                 options=list(
                                   width=300,
                                   height=300,
                                   slices="{0: {offset: 0.2},
                                   1: {offset: 0.2},
                                   2: {offset: 0.2}}",
                                   title='Inference Pie',
                                   legend='none',
                                   colors="['black','orange', 'blue',
                                   'red', 'purple', 'green']",
                                   pieSliceText='label',
                                   pieHole=0.5),
                                 chartid="doughnut")
        p <- gvisMerge(doughnut_extra,doughnut,horizontal=TRUE,
                       tableOptions="cellspacing=10")


  })


output$bayesnet_main <- renderUI({ 
  
  #tar = input$target_input
  output$bayesnet <- renderPlot({
    withProgress({
      setProgress(message = "Please Wait")
    
    
       # tar = noquote(tar)
    
    globall$nAttrs$fillcolor <- c(tar="red")
    
     
    plot(bn2[["dag"]], main="Car Insurance Variables Network",nodeAttrs = globall$nAttrs,
         attrs = list(node = list(fillcolor = "lightgreen"),
                      edge = list(color = "blue"),
                      graph = list(rankdir = "TD",size="10")
        )) })
  })
 
      plotOutput("bayesnet",width="100%", height = "1200px")
})

  
 # output$viewTable <- renderTable(bayes())
  
  output$looktab <- eventReactive(input$submit_button,{
     "Click Inference Analysis for results" } )
  
  

    observeEvent(input$reset, {
    globall$evidence_variable <- NULL
    globall$state_variable <- NULL
    
  })

  }) })