
## server file
server <- function(input, output) {
  
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
  
  warnText <- renderText({
    if (exists("message", where=resTask()) && grepl("WARNING",resTask()))
      warnText <- c(resTask()$message)
    else
      warnText <- ''
    
    return(warnText)
  })
  
  output$warnOut <- renderText({
    textOutput("warnText")
    return(warnText())
  })
  
  inputFile <- reactiveValues()
  observe({
    if (is.null(input$datafile))
      return()
    inputFile$fileName <- basename(input$datafile$datapath)
    inputFile$modelData <- modelData()
    inputFile$compartments <- CoRC::getCompartments(model=inputFile$modelData)
    inputFile$species <- CoRC::getSpecies(model=inputFile$modelData)
    inputFile$reactions <- CoRC::getReactions(model=inputFile$modelData)
    inputFile$globalQuantities <- CoRC::getGlobalQuantities(model=inputFile$modelData)
    inputFile$events <- CoRC::getEvents(model=inputFile$modelData)
    inputFile$parameters <- CoRC::getParameters(model=inputFile$modelData)
    inputFile$stoichiometry <- CoRC::getStoichiometryMatrix(model=inputFile$modelData)
    inputFile$linkMatrix <- CoRC::getLinkMatrix(model=inputFile$modelData)
  })
  
  modelData <- function(){
    inputFile <- basename(input$datafile$datapath)
    #validate(need(grepl("\\.cps$",inputFile)|| grepl("\\.xml$", inputFile) , "Only .cps or .xml files are supported"))
    if (grepl("\\.cps$",inputFile))
      modelData <- CoRC::loadModel(input$datafile$datapath)
    else if (grepl("\\.xml$", inputFile))
      modelData <- CoRC::loadSBML(input$datafile$datapath)
    else{
      modelData <- NULL
    }
    return(modelData)
  }

#### To execute different tasks ####
  resTask <- eventReactive(input$runTask, {

    modelData <- inputFile$modelData
    selectedTask <- selection()
    
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
      resTask <- res$result
    }
    else if(selectedTask == "Metabolic Control Analysis"){
      res <- tryCatch(CoRC::runMCA(perform_steady_state_analysis = input$mcaSelection, model=modelData), warning = function(warning_condition){return(warning_condition) }, error = function(error_condition){return(error_condition) })
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
      paste(sub("\\..*$", '',basename(input$datafile$datapath)) , '.csv', sep='')
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
      else if (selectedTask == "Time Course"){
        writeData <- resTask()[, c("Time",input$columns), drop = FALSE]
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
  output$tableTC <- DT::renderDataTable({
    if (error() != "" || is.null(resTask()))
      return(NULL)
    data <- resTask()[, c("Time",input$columns), drop = FALSE]
    return(data)
  })
  

  output$tableSS <- DT::renderDataTable({
    if (error() != "" || is.null(resTask()))
      return(NULL)
    data <- resTask()$species[,c("name","concentration","rate","transition_time")]
    return(data)
  })
  
  output$tableJac <- DT::renderDataTable({
    if (error() != "" || is.null(resTask()))
      return(NULL)
    data <- resTask()$jacobian_complete
    return(data)
  })
  
  output$tableMCA <- DT::renderDataTable({
    if (error() != "" || is.null(resTask()))
      return(NULL)
    data <- resTask()$elasticities_unscaled
    return(data)
  })
  
  output$tableLNA <- DT::renderDataTable({
    if (error() != "" || is.null(resTask()))
      return(NULL)
    data <- resTask()$covariance_matrix
    return(data)
  })
  
  output$tableLM <- DT::renderDataTable({
    return(inputFile$linkMatrix)
  })
  
  output$tableStoich <- DT::renderDataTable({
    return(inputFile$stoichiometry)
  })
  
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
      tableEvents <- inputFile$events
      tableEvents <- tableEvents[,c(-1)]
      return(tableEvents)
    } 
    else if (selectedTask == "Parameters"){
      tableParameters <- inputFile$parameters
      if (!is.null(tableParameters)){
        tableParameters <- tableParameters[,-1]
        tableParameters$mapping <- gsub(".*\\[|\\]", "", tableParameters$mapping)  
      }
      return(tableParameters)
    } 
  })
  
  
#### To render UI and plots for different tasks ####
  output$plot <- renderPlot({
    if (error() != "" || is.null(resTask()))
      return(NULL)
    
    selectedTask <- selection()
    data <- resTask()
    if (selectedTask == "Time Course" && "Time" %in% names(data)){
      data <- data[, c("Time",input$columns), drop = FALSE]
      melted <- melt(data,id.vars="Time")
      colnames(melted)[2:3] <- c("Species", "Number")
      plot <- ggplot(melted, aes(x=Time, y=Number, group=Species, color= Species)) + geom_line(size = 1) + theme_classic(base_size = 18) + ggtitle("Time-course of selected species") + ylab("#") + xlab("Time (s)")
      print(plot)
    }
    else{
      textOutput("error")
    }
  })
  
  ## To load the output UI showing table/Plot
  output$show_output<- renderUI({
    selectedTask <- selection()
    if (selectedTask %in% c("Reactions","Species","Compartments", "Global Quantities","Events","Parameters")){
      tabPanel("Table",DT::dataTableOutput("tableModel"))
    }
    else if (selectedTask == "Time Course"){
      tabsetPanel(
        tabPanel("Time Course",DT::dataTableOutput("tableTC")),
        tabPanel("Plot", plotOutput("plot"))
      )
    }
    else if (selectedTask == "Steady State"){
      tabsetPanel(
        tabPanel("Steady State",DT::dataTableOutput("tableSS")),
        tabPanel("Jacobian",DT::dataTableOutput("tableJac"))
      )
    }
    else if(selectedTask == "Metabolic Control Analysis"){
      tabPanel("Table",DT::dataTableOutput("tableMCA"))
    }
    else if(selectedTask == "Linear Noise Approximation"){
      tabPanel("Table",DT::dataTableOutput("tableLNA"))
    }
    else if(selectedTask == "Mass Conservation"){
      tabsetPanel(
        tabPanel("Stoichiometry",DT::dataTableOutput("tableStoich"))
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
    
    if (selectedTask == "Steady State"){
      
    }
    else if (selectedTask == "Time Course" && "Time" %in% names(resTask())){
      # Get the data set with the appropriate name
      melted <- melt(resTask(),id.vars="Time")
      colnames(melted)[2:3] <- c("Species", "Number")
      colnames <- unique(melted$Species)
      
      # Create the checkboxes and select them all by default
      checkboxGroupInput("columns", "Choose Species", 
                         choices  = colnames,
                         selected = colnames,
                         inline = T)
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
    #, "slices"
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
                               ,'Linear Noise Approximation'= structure('4',sticon=''))
                          , sticon='')
      )
    #attr(sss[[1]],"stopened")=TRUE 
    sss
  })

  
}