
## server file
server <- function(input, output, session) {
  ## limit for the input file size
  options(shiny.maxRequestSize=30*1024^2) 
  ## to store the current selection
  selection <- renderText({
    return(unlist(get_selected(input$taskSelection)))
  })
  
  ## to store the error encountered
  error <- renderText({
    if (is.null(inputFile$dataPaths))
      error <- c('No model file loaded !!')
    else if (is.null(inputFile$modelData))
      error <- c('Please load a valid model file!!')
    else if (exists('message', where=resTask()))
      error <- c(resTask()$message)
    else
      error <- ''
    
    return(error)
  })
  
  ## to display the error 
  output$errorOut <- renderText({
    textOutput('error')
    return(error())
  })
  
  ## To store the information of input model
  inputFile <- reactiveValues()
  observe({
    if (is.null(input$datafile))
      return()

    inputFile$fileNames <- input$datafile$name
    inputFile$dataPaths <- input$datafile$datapath
    inputFile$dirName <- dirname(input$datafile$datapath)
    inputFile$modelData <- NULL
    inputFile$rootnode <- NULL
    inputFile$modelLoaded <- FALSE
    for (i in 1:length(inputFile$fileNames)){
      inputFileName <- inputFile$fileNames[i]
      if (grepl('\\.cps$',inputFileName)){
        inputFile$modelData <- CoRC::loadModel(inputFile$dataPaths[i])
        inputFile$modelName <- inputFileName
        inputFile$rootnode <- xmlTreeParse(inputFile$dataPaths[i])
        inputFile$modelAttrs <- xmlAttrs(inputFile$rootnode$doc$children$COPASI[names(inputFile$rootnode$doc$children$COPASI)=='Model']$Model)
        inputFile$modelLoaded <- TRUE
      }
      else if (grepl('\\.xml$', inputFileName)){
        inputFile$modelData <- CoRC::loadSBML(inputFile$dataPaths[i])
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
    inputFile$settingsTC <- CoRC::getTimeCourseSettings(model=inputFile$modelData)
    inputFile$settingsOpt <- CoRC::getOptimizationSettings(model=inputFile$modelData)
    inputFile$settingsPE <- CoRC::getParameterEstimationSettings(model=inputFile$modelData)
    inputFile$taskList <- c('Compartments', 'Species', 'Reactions'
                            ,'Global Quantities', 'Events', 'Parameters'
                            ,'Stoichiometry','Steady State','Time Course'
                            ,'Metabolic Control Analysis','Optimization'
                            ,'Parameter Estimation', 'Linear Noise Approximation')
  })
  
  
  ## Theme functions for the plots
  theme_pm <- function () {
    theme_bw(base_size=12) + #base_family='Arial Black'
      theme(panel.grid=element_line(linetype='dashed', color='light grey', size=0.2),axis.ticks.length=unit(-0.15, 'cm'),axis.text.x = element_text(margin=unit(c(0.25,0.25,0.25,0.25),'cm')),axis.text.y = element_text(margin=unit(c(0.25,0.25,0.25,0.25),'cm')))
  }
  
  output$modelInfo <- renderText({
    if (is.null(inputFile$modelData))
      return()
    selectedTask = selection()
    if (selectedTask %in% inputFile$taskList ){
      if (selectedTask == 'Parameter Estimation' ){
        expfileName= ''
        valfileName= ''
        if (xmlSize(inputFile$rootnode$doc$children$COPASI[names(inputFile$rootnode$doc$children$COPASI)=='ListOfTasks']$ListOfTasks[[6]][[2]][[9]]) >= 1){
          xmlList= xmlChildren(inputFile$rootnode$doc$children$COPASI[names(inputFile$rootnode$doc$children$COPASI)=='ListOfTasks']$ListOfTasks[[6]][[2]][[9]][[1]])
          for (i in 1:xmlSize(xmlList)){
            paramValue= xmlToList(xmlList[[i]])
            if (paramValue[[1]] == 'File Name'){
              expfileName= paramValue[[3]]
              break
            }
          }
          if (expfileName %in% inputFile$fileNames){
            file.copy(inputFile$dataPaths[inputFile$fileNames == expfileName], paste0(inputFile$dirName,'/',expfileName), overwrite = TRUE, recursive = FALSE,copy.mode = TRUE, copy.date = FALSE)
          }
          else
            expfileName= paste(' <font color=','red','>  Please load a valid data file along with the model. File name: <b>', expfileName ,'</b> </font> ')
        }
          
        if (xmlSize(inputFile$rootnode$doc$children$COPASI[names(inputFile$rootnode$doc$children$COPASI)=='ListOfTasks']$ListOfTasks[[6]][[2]][[10]]) > 2){
          xmlList= xmlChildren(inputFile$rootnode$doc$children$COPASI[names(inputFile$rootnode$doc$children$COPASI)=='ListOfTasks']$ListOfTasks[[6]][[2]][[10]][[1]])
          for (i in 1:xmlSize(xmlList)){
            paramValue= xmlToList(xmlList[[i]])
            if (paramValue[[1]] == 'File Name'){
              valfileName= paramValue[[3]]
              break
            }
          }
          if (valfileName %in% inputFile$fileNames){
            file.copy(inputFile$dataPaths[inputFile$fileNames == valfileName], paste0(inputFile$dirName,'/',valfileName), overwrite = TRUE, recursive = FALSE,copy.mode = TRUE, copy.date = FALSE)
          }
          else
            valfileName= paste(' <font color=','red','>  Please load a valid data file along with the model. File name: <b>', valfileName ,'</b> </font> ')
        }
        strOut= paste('<h2>',selectedTask,'</h2>')
        strOut= paste(strOut, '<pre><b> Experimental Data: </b>',expfileName,'<br> <br>')
        strOut= paste(strOut, '<b>Validation Data: </b>',valfileName,'<br>')
        strOut= paste(strOut, '<pre><b> Randomize Start Values: </b>',inputFile$settingsPE$randomize_start_values)
        strOut= paste(strOut, '<b>  Calculate Statistics: </b>',inputFile$settingsPE$calculate_statistics, '</pre></pre>')
      }
      else if (selectedTask == 'Optimization' ){
        strOut= paste('<h2>',selectedTask,'</h2>')
        strOut= paste(strOut, '<pre><b> Expression: </b>',inputFile$settingsOpt$expression)
        strOut= paste(strOut, '<pre><b> Maxmize: </b>',inputFile$settingsOpt$maximize, '</pre>')
        strOut= paste(strOut, '<b>Subtask: </b>',inputFile$settingsOpt$subtask)
        strOut= paste(strOut, '<pre><b> Randomize Start Values: </b>',inputFile$settingsOpt$randomize_start_values)
        strOut= paste(strOut, '<b>  Calculate Statistics: </b>',inputFile$settingsOpt$calculate_statistics, '</pre></pre>')
      }
      else
        strOut= paste('<h2>',selectedTask,'</h2>')
    }
    else if (selectedTask == 'Model' || inputFile$modelLoaded == T){
      strOut= paste('<pre><b> Model: </b>',inputFile$modelAttrs[[2]],'<br>')
      strOut= paste(strOut,'<pre><table><tr><th>Time Unit:</th><td>',inputFile$modelAttrs[[4]]
                    ,'</td><th>Volume Unit:</th><td>',inputFile$modelAttrs[[5]],'</td></tr>')
      strOut= paste(strOut,'<tr><th>Quantity Unit:</th><td>',inputFile$modelAttrs[[8]]
                    ,'</td><th>Area Unit:</th><td>',inputFile$modelAttrs[[6]],'</td></tr>')
      strOut= paste(strOut,'<tr><th>Avogadro Constant:</th><td>',inputFile$modelAttrs[[10]]
                    ,'</td><th>Length Unit:</th><td>',inputFile$modelAttrs[[7]],'</td></tr></table></pre></pre>')
      }
    return(strOut)
  })
  
  output$selectedMethod<- renderText({
    if (is.null(inputFile$rootnode))
      return()
    
    selectedTask = selection()
    if (selectedTask == 'Parameter Estimation' ){
      methodSetting= inputFile$settingsPE$method
    }
    else if(selectedTask == 'Optimization' ){
      methodSetting= inputFile$settingsOpt$method
    }
    else{
      return()
    }
  
    strOut= ''
    namesMethod= paste0(toupper(substring(names(methodSetting),1,1)),substring(names(methodSetting),2))
    strOut= paste('<pre><b>',namesMethod[[1]],'</b>:',methodSetting[[1]],'<br><pre>')
    for (i in 2:length(namesMethod)){
      strOut= paste(strOut, '<b>',namesMethod[[i]],'</b>:&nbsp ', methodSetting[[i]], '<br>')
    }
    return(paste(strOut,'</pre></pre>'))
  })
    
  paramList <- function () {
    if (is.null(inputFile$rootnode))
      return()
    
    selectedTask = selection()
    if (selectedTask == 'Parameter Estimation' ){
      xmlList= inputFile$rootnode$doc$children$COPASI[names(inputFile$rootnode$doc$children$COPASI)=='ListOfTasks']$ListOfTasks[[6]][[2]][[4]]
    }
    else if(selectedTask == 'Optimization' ){
      xmlList= inputFile$rootnode$doc$children$COPASI[names(inputFile$rootnode$doc$children$COPASI)=='ListOfTasks']$ListOfTasks[[5]][[2]][[6]]
    }
    else{
      return()
    }
    
    numParameters= xmlSize(xmlChildren(xmlList))
    if (numParameters < 1)
      return()
    resTable <- setNames(data.frame(matrix(ncol = 4, nrow = numParameters)), c('LowerBound', 'Parameter', 'UpperBound','StartValue'))
    for (i in 1:numParameters){
      xmlListIN <- xmlChildren(xmlList[[i]])
      for (j in 1:xmlSize(xmlListIN)){
        checkParam = names(xmlListIN) == 'Parameter'
        if (checkParam[j]){
          paramValue= xmlToList(xmlListIN[[j]])
          if (paramValue[[1]] == 'LowerBound'){
            resTable$LowerBound[i] = paramValue[[3]]
          }
          else if (paramValue[[1]]== 'ObjectCN'){
            resTable$Parameter[i] = gsub(',Reference=','.',gsub('.*Vector=','',paramValue[[3]]))
          }
          else if (paramValue[[1]]== 'UpperBound'){
            resTable$UpperBound[i] = paramValue[[3]]
          }
          else if (paramValue[[1]]== 'StartValue'){
            resTable$StartValue[i] = paramValue[[3]]
          }}}}
    return(resTable)
  }
  
  constrList <- function () {
    if (is.null(inputFile$rootnode))
      return()
    
    selectedTask = selection()
    if (selectedTask == 'Parameter Estimation' ){
      xmlList= inputFile$rootnode$doc$children$COPASI[names(inputFile$rootnode$doc$children$COPASI)=='ListOfTasks']$ListOfTasks[[6]][[2]][[5]]
    }
    else if(selectedTask == 'Optimization' ){
      xmlList= inputFile$rootnode$doc$children$COPASI[names(inputFile$rootnode$doc$children$COPASI)=='ListOfTasks']$ListOfTasks[[5]][[2]][[7]]
    }
    else{
      return()
    }

    numParameters= xmlSize(xmlChildren(xmlList))
    if (numParameters < 1)
      return()
    resTable <- setNames(data.frame(matrix(ncol = 3, nrow = numParameters)), c('LowerBound', 'Parameter', 'UpperBound'))
    
    for (i in 1:numParameters){
      xmlListIN <- xmlChildren(xmlList[[i]])
      for (j in 1:xmlSize(xmlListIN)){
        checkParam = names(xmlListIN) == 'Parameter'
        if (checkParam[j]){
          paramValue= xmlToList(xmlListIN[[j]])
          if (paramValue[[1]] == 'LowerBound'){
            resTable$LowerBound[i] = paramValue[[3]]
          }
          else if (paramValue[[1]]== 'ObjectCN'){
            resTable$Parameter[i] = gsub(',Reference=','.',gsub('.*Vector=','',paramValue[[3]]))
          }
          else if (paramValue[[1]]== 'UpperBound'){
            resTable$UpperBound[i] = paramValue[[3]]
            }}}}
    return(resTable)
  }
  

#### To execute different tasks ####
  resTask <- eventReactive(input$runTask, {
    modelData <- inputFile$modelData
    selectedTask <- selection()
    
    progress <- shiny::Progress$new() # Create a Progress object
    on.exit(progress$close()) # To make sure it closes when we exit this reactive, even if there's an error
    progress$set(message = paste('Running ', selectedTask), value = 0)

    if (selectedTask %in% c('Steady State','Linear Noise Approximation')){
      if (selectedTask == 'Steady State') settingTask = CoRC::getSteadyStateSettings(model=modelData)
      else settingTask = CoRC::getLinearNoiseApproximationSettings(model=modelData)
      settingTask$method$resolution = input$resolution
      settingTask$method$derivation_factor = input$derivationFac
      settingTask$method$use_newton = input$useNewton
      settingTask$method$use_integration = input$useIntegration
      settingTask$method$use_back_integration = input$useBackIntegration
      if (selectedTask == 'Steady State') resTask <- tryCatch(CoRC::runSS(calculate_jacobian = input$calculateJacobian,perform_stability_analysis =input$performStabilityAnalysis,method=settingTask$method,model=modelData), warning = function(warning_condition){return(warning_condition) }, error = function(error_condition){return(error_condition) })
      if (selectedTask == 'Linear Noise Approximation') resTask <- tryCatch(CoRC::runLNA(perform_steady_state_analysis = input$lnaSelection,method=settingTask$method,model=modelData), warning = function(warning_condition){return(warning_condition) }, error = function(error_condition){return(error_condition) })
    }
    else if (selectedTask == 'Time Course'){
      resTask <- tryCatch(CoRC::runTC(duration=input$obsTime,dt=input$obsIntervalSize,start_in_steady_state=input$startSteady,method=input$timeCourseSelection,model=modelData,save_result_in_memory = T), warning = function(warning_condition){return(warning_condition) }, error = function(error_condition){return(error_condition) })
    }
    else if(selectedTask == 'Metabolic Control Analysis'){
      settingTask = CoRC::getMetabolicControlAnalysisSettings(model=modelData)
      settingTask$method$modulation_factor = input$modulationFactor
      settingTask$method$use_reder = input$useReder
      settingTask$method$use_smallbone = input$useSmallbone
      resTask <- tryCatch(CoRC::runMCA(perform_steady_state_analysis = input$mcaSelection, method= settingTask$method, model=modelData), warning = function(warning_condition){return(warning_condition) }, error = function(error_condition){return(error_condition) })
    }
    else if (selectedTask == 'Optimization'){
      resTask <- tryCatch(CoRC::runOptimization(model=modelData), warning = function(warning_condition){return(warning_condition) }, error = function(error_condition){return(error_condition) })
    }
    else if (selectedTask == 'Parameter Estimation'){
      resTask <- tryCatch(CoRC::runParameterEstimation(model=modelData), warning = function(warning_condition){return(warning_condition) }, error = function(error_condition){return(error_condition) })
    }
    else
      resTask <- 'No Task found'

    return(resTask)
  })
  
#### To download tables for different tasks ####  
  ## For data download
  output$downloadData<-downloadHandler(
    filename = function() { 
      if (is.null(inputFile$dataPaths))
        return(NULL)
      paste(sub('\\..*$', '',inputFile$modelName) , '.csv', sep='')
      },
    content = function(file) {
      if (is.null(file) || error() != '' || is.null(resTask()))
        return(NULL)
      selectedTask <- selection()
      if (selectedTask == 'Steady State'){
        writeData <- resTask()$species[,c('name','concentration','rate','transition_time')]
      }
      else if (selectedTask == 'Time Course' && 'Time' %in% names(resTask()$result) && !is.null(input$columns)){
        writeData <- resTask()$result[, c('Time',input$columns), drop = FALSE]
      }
      else if(selectedTask == 'Metabolic Control Analysis'){
        writeData <- resTask()$elasticities_unscaled
      }
      else  if(selectedTask == 'Optimization'){
        writeData <- resTask()$parameters
      }
      else if(selectedTask == 'Parameter Estimation'){
        writeData <- resTask()$parameter
      }
      else if (selectedTask == 'Linear Noise Approximation'){
        writeData <- resTask()$covariance_matrix
      }
      else
        writeData <- 'No Data found'
      
      write.csv(writeData,file)
      }
  )
  
#### To render output tables for different tasks ####
  
  ## Output task-specific results
  output$tableResults1 <- DT::renderDataTable({
    if (error() != '' || is.null(resTask()))
      return(NULL)
    
    selectedTask <- selection()
    if (selectedTask == 'Steady State'){
      return(resTask()$species[,c('name','concentration','rate','transition_time')])
    }
    else if (selectedTask == 'Time Course' && 'Time' %in% names(resTask()$result) && !is.null(input$columns)){
      return(resTask()$result[, c('Time',input$columns), drop = FALSE])
    }
    else if(selectedTask == 'Metabolic Control Analysis'){
      return(resTask()$elasticities_unscaled)
    }
    else if(selectedTask %in% c('Optimization','Parameter Estimation') && !is.null(resTask()$main)){
      data <- t(as.data.frame(resTask()$main))
      colnames(data) <- c('Value')
      return(data)
    }
    else if(selectedTask == 'Linear Noise Approximation'){
      return(resTask()$covariance_matrix)
    }
    else
      return(NULL)
    },options = list(scrollX = TRUE, scrollY = '400px'))
  
  
  output$tableResults2 <- DT::renderDataTable({
    if (error() != '' || is.null(resTask()))
      return(NULL)
    
    selectedTask <- selection()
    if (selectedTask == 'Steady State'){
      colNames <- colnames(resTask()$jacobian_complete)
      data <- data.frame(resTask()$jacobian_complete)
      data <- formattable(data, list(area(col = colnames(data)) ~ color_tile('lightpink', 'lightgreen')))
      colnames(data) <- colNames
      return(as.datatable(data,options = list(scrollX = TRUE, scrollY = '400px')))
    }
    else if(selectedTask == 'Metabolic Control Analysis'){
      return(resTask()$elasticities_unscaled)
    }
    else if(selectedTask == 'Optimization'){
      return(resTask()$parameters)
    }
    else if(selectedTask == 'Parameter Estimation'){
      return(resTask()$parameter)
    }
    else if(selectedTask == 'Linear Noise Approximation'){
      return(resTask()$covariance_matrix)
    }
    else
      return(NULL)
  })
  
  output$tablePEexp <- DT::renderDataTable({
    if (error() != '' || is.null(resTask()))
      return(NULL)
    return(resTask()$experiments)
  },options = list(scrollX = TRUE, scrollY = '400px'))
  
  
  ## Display the selected parameters and constraints 
  output$tableParameterList <- DT::renderDataTable({
    if(!is.null(paramList())) return(paramList())
  },options = list(scrollX = TRUE, scrollY = '200px'))
  
  output$tableConstraintList <- DT::renderDataTable({
    if (!is.null(constrList())) return(constrList())
  },options = list(scrollX = TRUE, scrollY = '200px'))
  
  ## Display information of the loaded model
  output$tableModel <- DT::renderDataTable({
    if (is.null(inputFile$modelData))
      return()
    selectedTask <- selection()
    tableModel <-  data.frame()
    if (selectedTask == 'Compartments'){
      tableModel <- inputFile$compartments
      if (!is.null(tableModel)){
        tableModel <- tableModel[,c(-1)]    
      }}
    else if (selectedTask == 'Species'){
      tableModel <- inputFile$species
      tableModel <- tableModel[,c(-1,-7,-9,-11)]
    }
    else if (selectedTask == 'Reactions'){
      tableModel <- inputFile$reactions
      if (!is.null(tableModel)){
        tableModel <- tableModel[,c('name','reaction','rate_law','flux')]
        tableModel$rate_law <- gsub('.*\\[|\\]', '', tableModel$rate_law)        
      }}
    else if (selectedTask == 'Global Quantities'){
      tableModel <- inputFile$globalQuantities
      tableModel <- tableModel[,-1]
    } 
    else if (selectedTask == 'Events'){
      tableModel <- data.frame(inputFile$events)
      if (!is.null(tableModel)){
        tableModel <- tableModel[,-1]
        tableModel$assignment_target <- gsub('.*\\(|\\)', '', tableModel$assignment_target)
        tableModel$assignment_expression <- gsub('.*\\(|\\)', '', tableModel$assignment_expression)
      }} 
    else if (selectedTask == 'Parameters'){
      tableModel <- inputFile$parameters
      if (!is.null(tableModel)){
        tableModel <- tableModel[,-1]
        tableModel$mapping <- gsub('.*\\[|\\]', '', tableModel$mapping)  
      }}
    return(tableModel)
  },options = list(scrollX = TRUE, scrollY = '400px'))
  
  output$tableLM <- DT::renderDataTable({
    if (is.null(inputFile$modelData))
      return()
    colNames <- colnames(inputFile$linkMatrix)
    data <- data.frame(inputFile$linkMatrix)
    data <- formattable(data, list(area(col = colnames(data)) ~ color_tile('lightpink', 'lightgreen')))
    colnames(data) <- colNames
    return(as.datatable(data,options = list(scrollX = TRUE, scrollY = '400px')))
  })
  output$tableStoich <- DT::renderDataTable({
    if (is.null(inputFile$modelData))
      return()
    colNames <- colnames(inputFile$stoichiometry)
    data <- data.frame(inputFile$stoichiometry)
    data <- formattable(data, list(area(col = colnames(data)) ~ color_tile('lightpink', 'lightgreen')))
    colnames(data) <- colNames
    return(as.datatable(data,options = list(scrollX = TRUE, scrollY = '400px')))
  })

#### To render UI and plots for different tasks ####
  output$plotOverview <- renderPlot({
    selectedTask <- selection()
    if (selectedTask == 'Species'){
      tableSpecies <- inputFile$species
      if (!is.null(tableSpecies) && nrow(tableSpecies) !=0 ){
        ylabel <- paste('Concentration (',tableSpecies$unit[1], ')')
        barplot(tableSpecies$initial_concentration, main='Species overview',  ylab=ylabel, names.arg=tableSpecies$name, cex.names=0.8,las=2)
      }
    }
    else if (selectedTask == 'Global Quantities'){
      tableGlobalQuantities <- inputFile$globalQuantities
      if (!is.null(tableGlobalQuantities) && nrow(tableGlobalQuantities) !=0 ){
        barplot(tableGlobalQuantities$initial_value, main='Global Quantities overview', ylab= 'Initial value', names.arg=tableGlobalQuantities$name, cex.names=0.8,las=2)
      }
    }
    else if (selectedTask == 'Parameters'){
      tableParameters <- inputFile$parameters
      if (!is.null(tableParameters) && nrow(tableParameters) !=0 && !all(is.na(tableParameters$value))){
        barplot(tableParameters$value, main='Parameters overview', ylab='Value', names.arg=tableParameters$name, cex.names=0.8,las=2)
      }
    }
  })
  
  output$plotTC <- renderPlot({
    if (error() != '' || is.null(resTask()) || is.null(input$columns))
      return(NULL)
    
    selectedTask <- selection()
    data <- resTask()$result
    if (selectedTask == 'Time Course' && 'Time' %in% names(data)){
      data <- data[, c('Time',input$columns), drop = FALSE]
      melted <- melt(data,id.vars='Time')
      colnames(melted)[2:3] <- c('Species', 'Number')
      plot <- ggplot(melted, aes(x=Time, y=Number, group=Species, color= Species)) + geom_line(size = 1) + theme_classic(base_size = 18) + ggtitle('Time-course of selected species') + ylab('#') + xlab('Time (s)') + theme_pm()
      print(plot)
    }
    else{
      textOutput('error')
    }
  })

  ## To load the output UI showing table/Plot
  output$show_output<- renderUI({
    selectedTask <- selection()
    if (is.null(inputFile$modelData))
      return(NULL)
    if (selectedTask %in% c('Species','Global Quantities','Parameters')){
      tabsetPanel(id = 'mdl',tabPanel('Table',DT::dataTableOutput('tableModel')),tabPanel('Overview', plotOutput('plotOverview')))
    }
    else if (selectedTask %in% c('Reactions','Compartments', 'Events')){
        tabPanel('mdlTable',DT::dataTableOutput('tableModel'))
    }
    else if(selectedTask == 'Stoichiometry'){
      tabsetPanel(id = 'mdlTable',tabPanel('Stoichiometry Matrix',DT::dataTableOutput('tableStoich')),tabPanel('Link Matrix',DT::dataTableOutput('tableLM')))
    }
    else if (selectedTask == 'Time Course'){
      tabsetPanel(id = 'TC',tabPanel('Time Course',DT::dataTableOutput('tableResults1')),tabPanel('Plot', plotOutput('plotTC')))
    }
    else if (selectedTask == 'Steady State'){
      tabsetPanel(id = 'SS',tabPanel('Steady State', DT::dataTableOutput('tableResults1')),tabPanel('Jacobian', DT::dataTableOutput('tableResults2')))
    }
    else if(selectedTask == 'Metabolic Control Analysis'){
      tabPanel('Table',DT::dataTableOutput('tableResults1'))
    }
    else if(selectedTask == 'Optimization'){
      tabsetPanel(id = 'PE',tabPanel('Main',DT::dataTableOutput('tableResults1')),tabPanel('Optimized Parameters',DT::dataTableOutput('tableResults2')))
    }
    else if(selectedTask == 'Parameter Estimation'){
      tabsetPanel(id = 'PE',tabPanel('Main',DT::dataTableOutput('tableResults1')),tabPanel('Fitted Parameters',DT::dataTableOutput('tableResults2')),tabPanel('Experiments', DT::dataTableOutput('tablePEexp')))
    }
    else if(selectedTask == 'Linear Noise Approximation'){
      tabPanel('Table',DT::dataTableOutput('tableResults1'))
    }
    else{
    }
  })
  
#### To choose species for table and plot output ** ONLY FOR TIME_COURSE ** ####
  output$choose_columns <- renderUI({
    # If missing input, return to avoid error later in function
    if(error() != '' || is.null(resTask()))
      return(NULL)
    selectedTask <- selection()
    data <- resTask()$result
    output = tagList()
    
    if (selectedTask == 'Time Course' && 'Time' %in% names(data)){
      # Get the data set with the appropriate name
      melted <- melt(data,id.vars='Time')
      colnames(melted)[2:3] <- c('Species', 'Number')
      colnames <- unique(melted$Species)
      output[[1]] = actionButton('showAll', 'Show/Hide All')
      # Create the checkboxes and select them all by default
      output[[2]] = checkboxGroupInput('columns', '', choices  = colnames, selected = colnames, inline = T)
    }
    output
  })
  
  observeEvent(input$showAll,{
    if(error() != '' || is.null(resTask()))
      return(NULL)
    
    data <- resTask()$result
    melted <- melt(data,id.vars='Time')
    colnames(melted)[2:3] <- c('Species', 'Number')
    colnames <- unique(melted$Species)
    if (input$showAll %% 2 == 0){
      updateCheckboxGroupInput(session=session,'columns', choices= colnames, selected= colnames,inline = T)
    }
    else {
      updateCheckboxGroupInput(session=session,'columns', choices= colnames, selected= NULL,inline = T)
    }
  })

#### To generate options interface for tasks ####
  output$choose_options <- renderUI({
    # If missing input, return to avoid error later in function
    if(length(get_selected(input$taskSelection))==0)
      return(NULL)
    else{
      textOutput('selection')
    }
    output = tagList()
    selectedTask <- selection()
    if (selectedTask %in% c('Reactions','Species','Compartments', 'Global Quantities','Events','Parameters','Stoichiometry')){
      output[[1]] = ''
    }
    else if (selectedTask %in% c('Steady State','Linear Noise Approximation')){
      if (selectedTask == 'Steady State'){
        if (!is.null(inputFile$modelData)) settingTask = CoRC::getSteadyStateSettings(model=inputFile$modelData)
        output[[1]] = splitLayout(checkboxInput('calculateJacobian','calculate Jacobian', value= ifelse(is.null(inputFile$modelData), T, settingTask$calculate_jacobian)),checkboxInput('performStabilityAnalysis','perform Stability Analysis', value= ifelse(is.null(inputFile$modelData), T, settingTask$perform_stability_analysis)))
      }
      else {
        if (!is.null(inputFile$modelData))settingTask = CoRC::getLinearNoiseApproximationSettings(model=inputFile$modelData)
        output[[1]] = checkboxInput('lnaSelection','perform Steady State Analysis',value = ifelse(is.null(inputFile$modelData), T, settingTask$perform_steady_state_analysis))
      }
      output[[2]] = splitLayout(numericInput('resolution', 'Resolution:', ifelse(is.null(inputFile$modelData), 1e-9, settingTask$method$resolution), min = 1e-9, max = 1),numericInput('derivationFac', 'Derivation Factor:', ifelse(is.null(inputFile$modelData), 1e-3, settingTask$method$derivation_factor), min = 1e-3, max = 1))
      output[[3]] = splitLayout(checkboxInput('useNewton','Use Newton', value= ifelse(is.null(inputFile$modelData), T, settingTask$method$use_newton)),checkboxInput('useIntegration','Use Integration', value= ifelse(is.null(inputFile$modelData), T, settingTask$method$use_integration)),checkboxInput('useBackIntegration','Use Back Integration', value= ifelse(is.null(inputFile$modelData), F, settingTask$method$use_back_integration)))
      output[[4]] = actionButton('runTask', 'Run Task',icon=icon('angle-double-right'))
      output[[5]] = downloadButton('downloadData', 'Download Results')
    }
    else if (selectedTask == 'Time Course'){
      output[[1]] = splitLayout(numericInput('obsTime', 'Duration [s]:', ifelse(is.null(inputFile$modelData), 10, inputFile$settingsTC$duration), min = 1, max = 1000),numericInput('obsIntervalSize', 'Interval Size [s]:', ifelse(is.null(inputFile$modelData), 1, inputFile$settingsTC$dt), min = 0.0001, max = 100))
      output[[2]] = checkboxInput('startSteady','start in Steady State', value= ifelse(is.null(inputFile$modelData), F, inputFile$settingsTC$start_in_steady_state))
      output[[3]] = selectInput('timeCourseSelection', 'Select a Method:', choices = c('Deterministic (LSODA)'='deterministic'
                                                                                       ,'Stochastic (Gibson + Bruck) '='stochastic'
                                                                                       ,'Stochastic (Direct method)'='directMethod'
                                                                                       ,'Stochastic (Tau leap)'='tauLeap'
                                                                                       ,'Stochastic (Adaptive SSA)'='adaptiveSA'
                                                                                       ,'Hybrid (Runge-Kutta)'='hybrid'
                                                                                       ,'Hybrid (LSODA)'='hybridLSODA'
                                                                                       #,'Hybrid (RK45)'='hybridODE45'
                                                                                       ,'SDE solver (RI5)'='stochasticRunkeKuttaRI5')
                                ,selected = ifelse(is.null(inputFile$modelData), 'deterministic', inputFile$settingsTC$method$method))
      output[[4]] = actionButton('runTask', 'Run Task',icon=icon('angle-double-right'))
      output[[5]] = downloadButton('downloadData', 'Download Results')
    }
    else if (selectedTask == 'Metabolic Control Analysis'){
      if (!is.null(inputFile$modelData)) settingTask = CoRC::getMetabolicControlAnalysisSettings(model=inputFile$modelData)
      output[[1]] = checkboxInput('mcaSelection','perform Steady State Analysis',value = T)
      output[[2]] = numericInput('modulationFactor', 'Modulation Factor:', ifelse(is.null(inputFile$modelData), 1e-9, settingTask$method$modulation_factor), min = 1e-9, max = 1)
      output[[3]] = splitLayout(checkboxInput('useReder','Use Reder', value= ifelse(is.null(inputFile$modelData), T, settingTask$method$use_reder)),checkboxInput('useSmallbone','Use Smallbone', value= ifelse(is.null(inputFile$modelData), T, settingTask$method$use_smallbone)))
      output[[4]] = actionButton('runTask', 'Run Task',icon=icon('angle-double-right'))
      output[[5]] = downloadButton('downloadData', 'Download Results')
    }
    else if (selectedTask == 'Optimization'){
      output[[1]] = tabsetPanel(id = 'PE',tabPanel('Parameters', DT::dataTableOutput('tableParameterList')),tabPanel('Constraints', DT::dataTableOutput('tableConstraintList')) )
      output[[2]] = htmlOutput('selectedMethod')
      output[[3]] = actionButton('runTask', 'Run Task',icon=icon('angle-double-right'))
      output[[4]] = downloadButton('downloadData', 'Download Results')
    }
    else if (selectedTask == 'Parameter Estimation'){
      output[[1]] = tabsetPanel(id = 'PE',tabPanel('Parameters', DT::dataTableOutput('tableParameterList')),tabPanel('Constraints', DT::dataTableOutput('tableConstraintList')) )
      output[[2]] = htmlOutput('selectedMethod')
      output[[3]] = actionButton('runTask', 'Run Task',icon=icon('angle-double-right'))
      output[[4]] = downloadButton('downloadData', 'Download Results')
    }
    return(output)
  })
  
  ### Tree structure for task selection
  output$taskSelection <- renderTree({ 
    structTree =list('Model'= structure(list('Compartments'= structure('1',sticon=''),'Species'= structure('2',sticon=''),'Reactions'= structure('3',sticon=''),'Global Quantities'= structure('4',sticon=''),'Events'= structure('5',sticon=''),'Parameters'= structure('6',sticon=''),'Stoichiometry'= structure('7',sticon='')),sticon='')
                     ,'Tasks'= structure(list('Steady State'= structure('1',sticon=''),'Time Course'= structure('2',sticon=''),'Metabolic Control Analysis'= structure('3',sticon=''),'Optimization'= structure('4',sticon=''),'Parameter Estimation'= structure('5',sticon=''),'Linear Noise Approximation'= structure('6',sticon='')), sticon=''))
    #attr(structTree[[1]],'stopened')=TRUE 
    structTree
  })
}