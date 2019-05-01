
## server file
server <- function(input, output, session) {
  
  ## limit for the input file size
  options(shiny.maxRequestSize=30*1024^2) 
  
  ## to store the current selection
  selection <- renderText({
    selectedText <- unlist(get_selected(input$taskSelection))
    return(selectedText)
  })
  
  ## to store the error encountered
  error <- renderText({
    if (is.null(input$datafile$datapath))
      error <- c("No model file loaded !!")
    else if (is.null(inputFile$modelData))
      error <- c("Please load a valid model file!!")
    else if (exists("message", where=resTask()))
      error <- c(resTask()$message)
    else
      error <- ''
    
    return(error)
  })
  
  ## to display the error 
  output$errorOut <- renderText({
    textOutput("error")
    return(error())
  })
  
  ## To store the information of input model
  inputFile <- reactiveValues()
  observe({
    if (is.null(input$datafile))
      return()

    inputFile$fileNames <- input$datafile$name
    inputFile$dirName <- dirname(input$datafile$datapath)
    inputFile$modelData <- NULL
    for (i in 1:length(input$datafile$name)){
      inputFileName <- input$datafile$name[i]
      if (grepl("\\.cps$",inputFileName)){
        inputFile$modelData <- CoRC::loadModel(input$datafile$datapath[i])
        inputFile$modelName <- inputFileName
        inputFile$rootnode <- xmlTreeParse(input$datafile$datapath[i])
      }
      else if (grepl("\\.xml$", inputFileName)){
        inputFile$modelData <- CoRC::loadSBML(input$datafile$datapath[i])
        inputFile$modelName <- inputFileName
      }
    }
    
    if (is.null(inputFile$modelData)){
      return(error())
    }

    inputFile$compartments <- CoRC::getCompartments(model=inputFile$modelData)
    inputFile$species <- CoRC::getSpecies(model=inputFile$modelData)
    inputFile$reactions <- CoRC::getReactions(model=inputFile$modelData)
    inputFile$globalQuantities <- CoRC::getGlobalQuantities(model=inputFile$modelData)
    inputFile$events <- CoRC::getEvents(model=inputFile$modelData)
    inputFile$parameters <- CoRC::getParameters(model=inputFile$modelData)
    inputFile$stoichiometry <- CoRC::getStoichiometryMatrix(model=inputFile$modelData)
    inputFile$linkMatrix <- CoRC::getLinkMatrix(model=inputFile$modelData)
    inputFile$settingsPE <- CoRC::getParameterEstimationSettings(model=inputFile$modelData)
  })
  
  
  ## Theme functions for the plots
  theme_pm <- function () {
    theme_bw(base_size=12) + #base_family="Arial Black"
      theme(
        panel.grid=element_line(linetype="dashed", color="light grey", size=0.2),
        axis.ticks.length=unit(-0.15, "cm"),
        axis.text.x = element_text(margin=unit(c(0.25,0.25,0.25,0.25),"cm")),
        axis.text.y = element_text(margin=unit(c(0.25,0.25,0.25,0.25),"cm"))
      )
  }
  
  output$modelInfo <- renderText({
    if (is.null(input$datafile) || is.null(inputFile$modelData))
      return()
    selectedTask = selection()
    if (selectedTask %in% c("Steady State", "Time Course", "Metabolic Control Analysis","Optimization","Parameter Estimation", "Linear Noise Approximation") ){
      if (selectedTask == "Parameter Estimation" ){
        expfileName= ""
        valfileName= ""
        if (xmlSize(inputFile$rootnode$doc$children$COPASI[[3]][[6]][[2]][[9]]) >= 1){
          xmlList= xmlChildren(inputFile$rootnode$doc$children$COPASI[[3]][[6]][[2]][[9]][[1]])
          for (i in 1:xmlSize(xmlList)){
            paramValue= xmlToList(xmlList[[i]])
            if (paramValue[[1]] == "File Name"){
              expfileName= paramValue[[3]]
              break
            }
          }
          if (expfileName %in% inputFile$fileNames){
            file.copy(input$datafile$datapath[inputFile$fileNames == expfileName], paste0(inputFile$dirName,"/",expfileName), overwrite = TRUE, recursive = FALSE,copy.mode = TRUE, copy.date = FALSE)
          }
          else
            expfileName= paste(expfileName," <font color=\"red\"> ------NOT PROPERLY LOADED!------ </font> ")
        }
          
        if (xmlSize(inputFile$rootnode$doc$children$COPASI[[3]][[6]][[2]][[10]]) > 2){
          xmlList= xmlChildren(inputFile$rootnode$doc$children$COPASI[[3]][[6]][[2]][[10]][[1]])
          for (i in 1:xmlSize(xmlList)){
            paramValue= xmlToList(xmlList[[i]])
            if (paramValue[[1]] == "File Name"){
              valfileName= paramValue[[3]]
              break
            }
          }
          if (valfileName %in% inputFile$fileNames){
            file.copy(input$datafile$datapath[inputFile$fileNames == valfileName], paste0(inputFile$dirName,"/",valfileName), overwrite = TRUE, recursive = FALSE,copy.mode = TRUE, copy.date = FALSE)
          }
          else
            valfileName= paste(valfileName," <font color=\"red\"> ------NOT PROPERLY LOADED!------ </font> ")
        }
        
        return(paste("<h2>",selectedTask,"</h2> \b", "<table style=\"width:100%\"><tr><th>Experimental Data:</th><th>Validation Data:</th></tr><tr><td>",expfileName,"</td><td>",valfileName, "</td></tr></table> \b"))
      }
      else
        return(paste("<h2>",selectedTask,"</h2> \b"))
    }
  })
  
  output$methodSelectionPE <- renderText({
    if (is.null(input$datafile) || is.null(inputFile$rootnode))
      return()
    namesPE= toupper(names(inputFile$settingsPE$method))
    strOut= ""
    for (i in 1:length(namesPE)){
      strOut= paste(strOut, "<b>",namesPE[[i]],"</b>:&nbsp ", inputFile$settingsPE$method[[i]], "<br>")
    }
    return(paste(strOut,"<br>"))
  })
    
  paramListPE <- function () {
    if (is.null(input$datafile)|| is.null(inputFile$rootnode))
      return()
    xmlList= xmlChildren(inputFile$rootnode$doc$children$COPASI[[3]][[6]][[2]][[4]])
    numParameters= xmlSize(xmlList)
    if (numParameters < 1)
      return()
    resTable <- setNames(data.frame(matrix(ncol = 4, nrow = numParameters)), c("LowerBound", "Parameter", "UpperBound","StartValue"))
    for (i in 1:numParameters){
      xmlListIN <- xmlChildren(inputFile$rootnode$doc$children$COPASI[[3]][[6]][[2]][[4]][[i]])
      for (j in 1:xmlSize(xmlListIN)){
        checkParam = names(xmlListIN) == "Parameter"
        if (checkParam[j]){
          paramValue= xmlToList(xmlListIN[[j]])
          if (paramValue[[1]] == "LowerBound"){
            resTable$LowerBound[i] = paramValue[[3]]
          }
          else if (paramValue[[1]]== "ObjectCN"){
            resTable$Parameter[i] = gsub(",Reference=",".",gsub(".*Vector=","",paramValue[[3]]))
          }
          else if (paramValue[[1]]== "UpperBound"){
            resTable$UpperBound[i] = paramValue[[3]]
          }
          else if (paramValue[[1]]== "StartValue"){
            resTable$StartValue[i] = paramValue[[3]]
          }
        }
      }
    }
    return(resTable)
  }
  
  constrListPE <- function () {
    if (is.null(input$datafile)|| is.null(inputFile$rootnode))
      return()
    xmlList= xmlChildren(inputFile$rootnode$doc$children$COPASI[[3]][[6]][[2]][[5]])
    numParameters= xmlSize(xmlList)
    if (numParameters < 1)
      return()
    resTable <- setNames(data.frame(matrix(ncol = 3, nrow = numParameters)), c("LowerBound", "Parameter", "UpperBound"))
    
    for (i in 1:numParameters){
      xmlListIN <- xmlChildren(inputFile$rootnode$doc$children$COPASI[[3]][[6]][[2]][[5]][[i]])
      for (j in 1:xmlSize(xmlListIN)){
        checkParam = names(xmlListIN) == "Parameter"
        if (checkParam[j]){
          paramValue= xmlToList(xmlListIN[[j]])
          if (paramValue[[1]] == "LowerBound"){
            resTable$LowerBound[i] = paramValue[[3]]
          }
          else if (paramValue[[1]]== "ObjectCN"){
            resTable$Parameter[i] = gsub(",Reference=",".",gsub(".*Vector=","",paramValue[[3]]))
          }
          else if (paramValue[[1]]== "UpperBound"){
            resTable$UpperBound[i] = paramValue[[3]]
          }
        }
      }
    }
    return(resTable)
  }
  

#### To execute different tasks ####
  resTask <- eventReactive(input$runTask, {
    modelData <- inputFile$modelData
    selectedTask <- selection()
    
    # Create a Progress object
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    progress$set(message = paste("Running ", selectedTask), value = 0)
    
    
    if (selectedTask == "Steady State"){
      res <- tryCatch(CoRC::runSS(calculate_jacobian = input$calculateJacobian,perform_stability_analysis =input$calculateJacobian,model=modelData), warning = function(warning_condition){return(warning_condition) }, error = function(error_condition){return(error_condition) })
      resTask <- res
    }
    else if (selectedTask == "Mass Conservation"){
      
    }
    else if (selectedTask == "Time Course"){
      if (input$timeCourseSelection == 1)
        res <- tryCatch(CoRC::runTC(duration=input$obsTime,dt=input$obsIntervalSize,start_in_steady_state=input$startSteady,method="deterministic",model=modelData,save_result_in_memory = T), warning = function(warning_condition){return(warning_condition) }, error = function(error_condition){return(error_condition) })
      else if (input$timeCourseSelection == 2)
        res <- tryCatch(CoRC::runTC(duration=input$obsTime,dt=input$obsIntervalSize,start_in_steady_state=input$startSteady,method="stochastic",model=modelData,save_result_in_memory = T), warning = function(warning_condition){return(warning_condition) },error = function(error_condition){return(error_condition) })
      else if (input$timeCourseSelection == 3)
        res <- tryCatch(CoRC::runTC(duration=input$obsTime,dt=input$obsIntervalSize,start_in_steady_state=input$startSteady,method="directMethod",model=modelData,save_result_in_memory = T), warning = function(warning_condition){return(warning_condition) }, error = function(error_condition){return(error_condition) })
      resTask <- res
    }
    else if(selectedTask == "Metabolic Control Analysis"){
      res <- tryCatch(CoRC::runMCA(perform_steady_state_analysis = input$mcaSelection, model=modelData), warning = function(warning_condition){return(warning_condition) }, error = function(error_condition){return(error_condition) })
      resTask <- res
    }
    else if (selectedTask == "Parameter Estimation"){
      res <- tryCatch(CoRC::runParameterEstimation(model=modelData), warning = function(warning_condition){return(warning_condition) }, error = function(error_condition){return(error_condition) })
      resTask <- res
    }
    else if (selectedTask == "Linear Noise Approximation"){
      res <- tryCatch(CoRC::runLNA(perform_steady_state_analysis = input$lnaSelection,model=modelData), warning = function(warning_condition){return(warning_condition) }, error = function(error_condition){return(error_condition) })
      resTask <- res
    }
    else
      resTask <- "No Task found"
    
    return(resTask)
  })
  
  
#### To download tables for different tasks ####  
  ## For data download
  output$downloadData<-downloadHandler(
    filename = function() { 
      if (is.null(input$datafile$datapath))
        return(NULL)
      paste(sub("\\..*$", '',inputFile$modelName) , '.csv', sep='')
      },
    content = function(file) {
      if (is.null(file) || error() != "" || is.null(resTask()))
        return(NULL)
      selectedTask <- selection()
      
      if (selectedTask == "Steady State"){
        writeData <- resTask()$species[,c("name","concentration","rate","transition_time")]
      }
      else if (selectedTask == "Stoichiometric Analysis"){
        
      }
      else if (selectedTask == "Time Course" && "Time" %in% names(resTask()$result) && !is.null(input$columns)){
        writeData <- resTask()$result[, c("Time",input$columns), drop = FALSE]
      }
      else if(selectedTask == "Metabolic Control Analysis"){
        writeData <- resTask()$elasticities_unscaled
      }
      else if (selectedTask == "Linear Noise Approximation"){
        writeData <- resTask()$covariance_matrix
      }
      else
        writeData <- "No Data found"
      
      write.csv(writeData,file)
      }
  )
  #content = function(file) { write.csv(resTask()[, c("Time",input$columns), drop = FALSE],file)
  
  
#### To render output tables for different tasks ####
  output$tableResults <- DT::renderDataTable({
    if (error() != "" || is.null(resTask()))
      return(NULL)
    
    selectedTask <- selection()
    if (selectedTask == "Time Course" && "Time" %in% names(resTask()$result) && !is.null(input$columns)){
      data <- resTask()$result[, c("Time",input$columns), drop = FALSE]
    }
    else if(selectedTask == "Metabolic Control Analysis"){
      data <- resTask()$elasticities_unscaled
    }
    else if(selectedTask == "Parameter Estimation"){
      data <- t(as.data.frame(resTask()$main))
      colnames(data) <- c("Value")  
    }
    else if(selectedTask == "Linear Noise Approximation"){
      data <- resTask()$covariance_matrix
    }
    else
      data <- NULL
    
    return(data)
  },options = list(scrollX = TRUE, scrollY = "400px"))
  
  ## Output task-specific results
  output$tableSS <- DT::renderDataTable({
    if (error() != "" || is.null(resTask()))
      return(NULL)
    data <- resTask()$species[,c("name","concentration","rate","transition_time")]
    return(data)
  },options = list(scrollX = TRUE, scrollY = "400px"))
  
  output$tableJac <- DT::renderDataTable({
    if (error() != "" || is.null(resTask()))
      return(NULL)
    colNames <- colnames(resTask()$jacobian_complete)
    data <- data.frame(resTask()$jacobian_complete)
    data <- formattable(data, list(area(col = colnames(data)) ~ color_tile("lightpink", "lightgreen")))
    colnames(data) <- colNames
    return(as.datatable(data,options = list(scrollX = TRUE, scrollY = "400px")))
  })
  
  output$tableLM <- DT::renderDataTable({
    colNames <- colnames(inputFile$linkMatrix)
    data <- data.frame(inputFile$linkMatrix)
    data <- formattable(data, list(area(col = colnames(data)) ~ color_tile("lightpink", "lightgreen")))
    colnames(data) <- colNames
    return(as.datatable(data,options = list(scrollX = TRUE, scrollY = "400px")))
  })
  
  output$tableStoich <- DT::renderDataTable({
    colNames <- colnames(inputFile$stoichiometry)
    data <- data.frame(inputFile$stoichiometry)
    data <- formattable(data, list(area(col = colnames(data)) ~ color_tile("lightpink", "lightgreen")))
    colnames(data) <- colNames
    return(as.datatable(data,options = list(scrollX = TRUE, scrollY = "400px")))
  })
  
  output$tablePEfit <- DT::renderDataTable({
    if (error() != "" || is.null(resTask()))
      return(NULL)
    data <- resTask()$parameter
    return(data)
  },options = list(scrollX = TRUE, scrollY = "400px"))
  
  output$tablePEexp <- DT::renderDataTable({
    if (error() != "" || is.null(resTask()))
      return(NULL)
    data <- resTask()$experiments
    return(data)
  },options = list(scrollX = TRUE, scrollY = "400px"))
  
  
  ## Display the selected parameters and constraints 
  output$tableParameterListPE <- DT::renderDataTable({
    data = paramListPE()
    if (!is.null(data)) return(data)
  },options = list(scrollX = TRUE, scrollY = "200px"))
  
  output$tableConstraintListPE <- DT::renderDataTable({
    data = constrListPE()
    if (!is.null(data)) return(data)
  },options = list(scrollX = TRUE, scrollY = "200px"))
  
  ## Display information of the loaded model
  output$tableModel <- DT::renderDataTable({
    selectedTask <- selection()
    if (selectedTask == "Compartments"){
      tableCompartments <- inputFile$compartments
      if (!is.null(tableCompartments)){
        tableCompartments <- tableCompartments[,c(-1)]    
      }
      return(tableCompartments)
    }
    else if (selectedTask == "Species"){
      tableSpecies <- inputFile$species
      tableSpecies <- tableSpecies[,c(-1,-7,-9,-11)]
      return(tableSpecies)
    }
    else if (selectedTask == "Reactions"){
      tableReactions <- inputFile$reactions
      if (!is.null(tableReactions)){
        tableReactions <- tableReactions[,c("name","reaction","rate_law","flux")]
        tableReactions$rate_law <- gsub(".*\\[|\\]", "", tableReactions$rate_law)        
      }
      return(tableReactions)
    }
    else if (selectedTask == "Global Quantities"){
      tableGlobalQuantities <- inputFile$globalQuantities
      tableGlobalQuantities <- tableGlobalQuantities[,-1]
      return(tableGlobalQuantities)
    } 
    else if (selectedTask == "Events"){
      tableEvents <- data.frame(inputFile$events)
      if (!is.null(tableEvents)){
        tableEvents <- tableEvents[,-1]
        tableEvents$assignment_target <- gsub(".*\\(|\\)", "", tableEvents$assignment_target)
        tableEvents$assignment_expression <- gsub(".*\\(|\\)", "", tableEvents$assignment_expression)
      }
      #tableEvents <- tableEvents[,c(-1)]
      return(data.frame(tableEvents))
    } 
    else if (selectedTask == "Parameters"){
      tableParameters <- inputFile$parameters
      if (!is.null(tableParameters)){
        tableParameters <- tableParameters[,-1]
        tableParameters$mapping <- gsub(".*\\[|\\]", "", tableParameters$mapping)  
      }
      return(tableParameters)
    } 
  },options = list(scrollX = TRUE, scrollY = "400px"))
  
  
#### To render UI and plots for different tasks ####
  output$plotOverview <- renderPlot({
    selectedTask <- selection()

    if (selectedTask == "Species"){
      tableSpecies <- inputFile$species
      if (!is.null(tableSpecies) && nrow(tableSpecies) !=0 ){
        ylabel <- paste("Concentration (",tableSpecies$unit[1], ")")
        barplot(tableSpecies$initial_concentration, main="Species overview",  ylab=ylabel, names.arg=tableSpecies$name, cex.names=0.8,las=2)
      }
    }
    else if (selectedTask == "Global Quantities"){
      tableGlobalQuantities <- inputFile$globalQuantities
      if (!is.null(tableGlobalQuantities) && nrow(tableGlobalQuantities) !=0 ){
        barplot(tableGlobalQuantities$initial_value, main="Global Quantities overview", ylab= "Initial value", names.arg=tableGlobalQuantities$name, cex.names=0.8,las=2)
      }
    }
    else if (selectedTask == "Parameters"){
      tableParameters <- inputFile$parameters
      if (!is.null(tableParameters) && nrow(tableParameters) !=0 && !all(is.na(tableParameters$value))){
        ylabel <- paste()
        barplot(tableParameters$value, main="Parameters overview", ylab="Value", names.arg=tableParameters$name, cex.names=0.8,las=2)
      }
    }
    
  })
  
  output$plot <- renderPlot({
    if (error() != "" || is.null(resTask()) || is.null(input$columns))
      return(NULL)
    
    selectedTask <- selection()
    data <- resTask()$result
    
    if (selectedTask == "Time Course" && "Time" %in% names(data)){
      data <- data[, c("Time",input$columns), drop = FALSE]
      melted <- melt(data,id.vars="Time")
      colnames(melted)[2:3] <- c("Species", "Number")
      plot <- ggplot(melted, aes(x=Time, y=Number, group=Species, color= Species)) + geom_line(size = 1) + theme_classic(base_size = 18) + ggtitle("Time-course of selected species") + ylab("#") + xlab("Time (s)") + theme_pm()
      print(plot)
    }
    else{
      textOutput("error")
    }
  })
  
  ## To load the output UI showing table/Plot
  output$show_output<- renderUI({
    selectedTask <- selection()
    if (is.null(input$datafile))
      return(NULL)
    if (selectedTask %in% c("Species","Global Quantities","Parameters")){
      tabsetPanel(id = "mdl"
                  ,tabPanel("Table",DT::dataTableOutput("tableModel"))
                  ,tabPanel("Overview", plotOutput("plotOverview"))
      )
    }
    else if (selectedTask %in% c("Reactions","Compartments", "Events")){
        tabPanel("Table",DT::dataTableOutput("tableModel"))
      }
    else if (selectedTask == "Time Course"){
      tabsetPanel(id = "TC"
        ,tabPanel("Time Course",DT::dataTableOutput("tableResults"))
        ,tabPanel("Plot", plotOutput("plot"))
      )
    }
    else if (selectedTask == "Steady State"){
      tabsetPanel(id = "SS"
        ,tabPanel("Steady State", DT::dataTableOutput("tableSS"))
        ,tabPanel("Jacobian", DT::dataTableOutput("tableJac"))
      )
    }
    else if(selectedTask == "Metabolic Control Analysis"){
      tabPanel("Table",DT::dataTableOutput("tableResults"))
    }
    else if(selectedTask == "Parameter Estimation"){
      tabsetPanel(id = "PE"
                  ,tabPanel("Main",DT::dataTableOutput("tableResults"))
                  ,tabPanel("Fit results",DT::dataTableOutput("tablePEfit"))
                  ,tabPanel("Experiments", DT::dataTableOutput("tablePEexp"))
      )
    }
    else if(selectedTask == "Linear Noise Approximation"){
      tabPanel("Table",DT::dataTableOutput("tableResults"))
    }
    else if(selectedTask == "Mass Conservation"){
      tabsetPanel(id = "MC"
        ,tabPanel("Stoichiometry",DT::dataTableOutput("tableStoich"))
        ,tabPanel("Link Matrix",DT::dataTableOutput("tableLM"))
      )
    }
    else{
      
    }
  })
  
#### To choose species for table and plot output ** ONLY FOR TIME_COURSE ** ####
  output$choose_columns <- renderUI({
    # If missing input, return to avoid error later in function
    if(error() != "" || is.null(resTask()))
      return(NULL)
    selectedTask <- selection()
    data <- resTask()$result
    output = tagList()
    
    if (selectedTask == "Time Course" && "Time" %in% names(data)){
      # Get the data set with the appropriate name
      melted <- melt(data,id.vars="Time")
      colnames(melted)[2:3] <- c("Species", "Number")
      colnames <- unique(melted$Species)
      
      output[[1]] = actionButton("showAll", "Show/Hide All")
      # Create the checkboxes and select them all by default
      output[[2]] = checkboxGroupInput("columns", "", 
                         choices  = colnames,
                         selected = colnames,
                         inline = T)
    }
    output
  })
  
  observeEvent(input$showAll,{
    if(error() != "" || is.null(resTask()))
      return(NULL)
    else {
      data <- resTask()$result
      melted <- melt(data,id.vars="Time")
      colnames(melted)[2:3] <- c("Species", "Number")
      colnames <- unique(melted$Species)
      
      if (input$showAll %% 2 == 0){
        updateCheckboxGroupInput(session=session,"columns",
                                 choices  = colnames,
                                 selected = colnames,
                                 inline = T)
      }
      else {
        updateCheckboxGroupInput(session=session,"columns",
                                 choices  = colnames,
                                 selected = NULL,
                                 inline = T)
      }
      }
  })

#### To generate options interface for tasks ####
  output$choose_options <- renderUI({
    
    # If missing input, return to avoid error later in function
    if(length(get_selected(input$taskSelection))==0)
      return(NULL)
    else{
      textOutput("selection")
    }

    output = tagList()
    selectedTask <- selection()
    if (selectedTask %in% c("Reactions","Species","Compartments", "Global Quantities","Events","Parameters")){
      output[[1]] = ""
    }
    else if (selectedTask == "Steady State"){
      output[[1]] = splitLayout(
        checkboxInput("calculateJacobian","calculate Jacobian", value= T)
        #,checkboxInput("performStabilityAnalysis","perform Stability Analysis", value= T)
      )
      output[[2]] = actionButton("runTask", "Run Task",icon=icon("angle-double-right"))
      output[[3]] = downloadButton("downloadData", "Download Results")
    }
    else if (selectedTask == "Mass Conservation"){
      output[[1]] = ""
    }
    else if (selectedTask == "Time Course"){
      output[[1]] = splitLayout(
        numericInput("obsTime", "Duration [s]:", 100, min = 1, max = 1000),
        #numericInput("obsInterval", "Interval:", 10, min = 10, max = 100),
        numericInput("obsIntervalSize", "Interval Size [s]:", 1, min = 0.1, max = 100)
      )
      output[[2]] = checkboxInput("startSteady","start in Steady State", value= F)
      output[[3]] = selectInput("timeCourseSelection", "Select a Method:", choices = c('Deterministic (LSODA)'='1',' Stochastic (Gibson & Bruck) '='2', 'Stochastic (Direct method)'='3'))
      output[[4]] = actionButton("runTask", "Run Task",icon=icon("angle-double-right"))
      output[[5]] = downloadButton("downloadData", "Download Results")
    }
    else if (selectedTask == "Metabolic Control Analysis"){
      output[[1]] = checkboxInput("mcaSelection","perform Steady State Analysis",value = T)
      output[[2]] = actionButton("runTask", "Run Task",icon=icon("angle-double-right"))
      output[[3]] = downloadButton("downloadData", "Download Results")
    }
    else if (selectedTask == "Optimization"){
      output[[1]] = tabsetPanel(id = "PE"
                                ,tabPanel("Parameters", DT::dataTableOutput("tableParameterListPE"))
                                ,tabPanel("Constraints", DT::dataTableOutput("tableConstraintListPE")) )
      output[[2]] = htmlOutput("methodSelectionPE")
      output[[3]] = actionButton("runTask", "Run Task",icon=icon("angle-double-right"))
      output[[4]] = downloadButton("downloadData", "Download Results")
    }
    else if (selectedTask == "Parameter Estimation"){
      output[[1]] = tabsetPanel(id = "PE"
                                ,tabPanel("Parameters", DT::dataTableOutput("tableParameterListPE"))
                                ,tabPanel("Constraints", DT::dataTableOutput("tableConstraintListPE")) )
      output[[2]] = htmlOutput("methodSelectionPE")
      output[[3]] = actionButton("runTask", "Run Task",icon=icon("angle-double-right"))
      output[[4]] = downloadButton("downloadData", "Download Results")
    }
    else if (selectedTask == "Linear Noise Approximation"){
      output[[1]] = checkboxInput("lnaSelection","perform Steady State Analysis",value = T)
      output[[2]] = actionButton("runTask", "Run Task",icon=icon("angle-double-right"))
      output[[3]] = downloadButton("downloadData", "Download Results")
    }
    
    output
  })
  
  ### Tree structure for task selection
  output$taskSelection <- renderTree({ 
    sss=list('Model'= structure(list('Compartments'= structure('1',sticon='')
                                     ,'Species'= structure('2',sticon='')
                                     ,'Reactions'= structure('3',sticon='')
                                     ,'Global Quantities'= structure('4',sticon='')
                                     ,'Events'= structure('5',sticon='')
                                     ,'Parameters'= structure('6',sticon=''))
                                , sticon='')
      ,'Tasks'= structure(list('Steady State'= structure('1',sticon='')
                               ,'Stoichiometric Analysis'= structure(list('Mass Conservation'= structure('1',sticon='')),sticon='')
                               ,'Time Course'= structure('2',sticon='')
                               ,'Metabolic Control Analysis'= structure('3',sticon='')
                               ,'Optimization'= structure('4',sticon='')
                               ,'Parameter Estimation'= structure('5',sticon='')
                               ,'Linear Noise Approximation'= structure('6',sticon=''))
                          , sticon='')
      )
    #attr(sss[[1]],"stopened")=TRUE 
    sss
  })

  
}