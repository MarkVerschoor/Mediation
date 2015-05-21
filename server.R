##WIP app
#Include control variables
#(Standardized estimates in output lavaan)
#Load required packages if necessary
if(!require(lavaan)){install.packages('plyr')}
if(!require(shiny)){install.packages('shiny')}
if(!require(lavaan)){install.packages('foreign')}
if(!require(lavaan)){install.packages('semPlot')}
require(plyr)
require(shiny)
require(foreign)
require(semPlot)
#Create an empty df for shiny to work with. Needs to be done prior to shinyServer()
df <- NULL
shinyServer(
  function(input, output) {
    
    #Handle the file upload
    filedata <- reactive({
      infile <- input$datafile
      if (is.null(infile)) {
        #User has not uploaded a file yet, so don't show anything
        return(NULL)
      }
      
      #Uploading the data is based on the extension. If it's .sav, do this, if it's .csv, do the next if() loop.
      if(length(grep("\\.(sav|por)$", tolower(input$datafile$name[1]))) > 0) {
        dat <- read.spss(file = input$datafile$datapath[1], to.data.frame=TRUE, use.value.labels = FALSE, use.missings = TRUE)
      } 
      else {
        if(length(grep("\\.(csv|csv\\.gz)$", tolower(input$datafile$name[1]))) > 0) {
          dat <- read.csv(file = input$datafile$datapath[1], header=TRUE, sep=",")
        } 
        else {
          return(NULL)
        }
      }
      
      na.omit(dat) #This line needs to be added, otherwise the .sav files don't show anything in Step 2.
      
    })
    output$fileUploaded <- reactive({
      return(!is.null(filedata()))
    })
    outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)
    
    #dat <- read.spss(file = "patient-trust.sav", to.data.frame=TRUE, use.value.labels = FALSE, use.missings = TRUE)
    #df <- as.data.frame(dat)   
    #Contv <- df[,"Y3"]
    #df$Cont <- df[,input$Contv]
    
    fit <- reactive({   #everytime one of the elements changes, the model will be recalculated
      df <- filedata()
      if(is.null(df)) return(NULL)    
      
      #Specify the variables below. Needs to be changed to account for multiple or zero control variables.
      df$Xiv <- df[,input$Iv] #Add the variable with the right model name to the dataframe df, so R can find it in line 80
      df$Mmv <- df[,input$M]  #input$M, because the label given to it is "M" (line 45). In ui.R referred to it as mCol -> (output$mCol)
      df$Ydv <- df[,input$Dv] 
      
      #numvar <- length(df[,])
      
      #for (i in (numvar+1):(numvar+length(input$Contv))){ 
      #  df[,(numvar+i)] <- input$Contv[i]
      #}
      #df$Contv <- df[,input$Contv]
      
      #Specify the model. Should probably be in if loops based on length(control variable)
      model <- '
      Ydv ~ c*Xiv      #Ydv instead of Y, etcetera, because X, M and Y often already occur in the dataset. Problem if these found.
      #mediator
      Mmv ~ a*Xiv {covariates}
      Ydv ~ b*Mmv {covariates}
      #indirect effect (a*b)
      indirect := a*b
      #total effect
      total := c + (a*b)      
      '
    
      if (length(input$Contv) != 0) {
        model <- gsub("\\{covariates\\}", paste(" + " , paste(input$Contv, collapse = "+")), model)
      } else {
        model <- gsub("\\{covariates\\}", "", model)
      }
      
      output$temp <- renderText({
        paste(names(df$Contv)) 
      })
 
      # Change the {covariates} by the actual covariates
      
      #if(length(df$Contv) == 0){
      #  model <- '
      #  Ydv ~ c*Xiv
      #  #mediator
      #  Mmv ~ a*Xiv
      #  Ydv ~ b*Mmv
        #indirect effect (a*b)
      #  indirect := a*b
      #  #total effect
      #  total := c + (a*b)      
      #  '
      #}
      
      sem(model, data = df)
      })
    
    output$step2 <- renderText({
      df <-filedata()
      if (is.null(df)) return(NULL)
      #If df (input datafile) is not null, then paste. ##ONLY SMALL##
      paste("Step 2")
    })
    
    #Populate the list boxes in the UI with column names from the uploaded file  
    output$ivCol <- renderUI({
      df <-filedata()
      if (is.null(df)) return(NULL)
      items=names(df)
      names(items)=items
      selectInput("Iv", label = "Independent Variable (X):", choices = items)
    })
    
    output$mCol <- renderUI({
      df <-filedata()
      if (is.null(df)) return(NULL)
      items=names(df)
      names(items)=items
      selectInput("M", label = "Mediator (M):", choices = items[!items %in% input$Iv]) #Only show items that are not selected in ivCol
    })
    
    output$dvCol <- renderUI({
      df <-filedata()
      if (is.null(df)) return(NULL)
      items=names(df)
      names(items)=items
      selectInput("Dv", label = "Dependent Variable (Y):", choices = items[!items %in% input$M & !items %in% input$Iv]) #Only show items that are not selected in both M and Iv
    })
    
    output$contCol <- renderUI({
      df <-filedata()
      if (is.null(df)) return(NULL)
      items=names(df)
      names(items)=items
      selectInput("Contv", label = "Control for (Please select one or zero control variables. You can press backspace or delete to remove a variable):", choices = items[!items %in% input$M & !items %in% input$Iv & !items %in% input$Dv], multiple = TRUE) #Only show items that are not selected in M, Iv, and Dv
    })
    
    #In this way, the variables can never be selected twice. Iv offers the choice of the variable that is selected as Dv, but then the selected variable of Dv will change.
    #This makes it easier to change the variables. You probably choose X first, which offers all variables as option, and this restricts the later variables.
    #It is harder if you select a particular variable in X and you want to make it your Dv, because you need to select a different X first, but this will occur less often.
    
    output$summary <- renderTable({   #Refer to "fit" that changes (is reactive)
      fit <- fit()
      if (is.null(fit)) return(NULL)
      param.ests <- parameterEstimates(fit, standardized = TRUE)  #standardized
      df.eff <- as.data.frame(rbind(param.ests[param.ests$label == 'c', c('est', 'se', 'z', 'pvalue', 'std.all')],
                                    param.ests[param.ests$label == 'indirect', c('est', 'se', 'z', 'pvalue', 'std.all')],
                                    param.ests[param.ests$label == 'total', c('est', 'se', 'z', 'pvalue', 'std.all')]
      ))
      row.names(df.eff) <- c("direct effect", "indirect effect", "total effect")
      colnames(df.eff) <- c("unstd. est", "se", "z-value", "p-value", "std. est")
      df.eff
    })
    
    output$conclusion <- renderText({
      fit <- fit() #Refer to "fit" that changes (is reactive)
      if (is.null(fit)) {
        paste("Upload a .sav or .csv file")
        return(NULL)
      }
      param.ests <- parameterEstimates(fit, standardized = TRUE)  #standardized
      df.eff <- as.data.frame(rbind(param.ests[param.ests$label == 'c', c('est', 'se', 'z', 'pvalue', 'std.all')],
                                    param.ests[param.ests$label == 'indirect', c('est', 'se', 'z', 'pvalue', 'std.all')],
                                    param.ests[param.ests$label == 'total', c('est', 'se', 'z', 'pvalue', 'std.all')]
      ))
      if(df.eff[2, 4] < 0.05 && df.eff[1, 4]  < 0.05){    #both significant = partial mediation
        mediationtext = "PARTIAL MEDIATION"
      }else{
        if(df.eff[2, 4] < 0.05 && df.eff[1, 4]  > 0.05){  #only indirect significant = full mediation
          mediationtext ="FULL MEDIATION"
        } else {
          mediationtext = "NO MEDIATION"
        }
      }
      paste("Lavaan shows us that there is", mediationtext,".")
    })
    #paste(as.character(covariates_chosen), collapse = "+")
    output$plot <- renderPlot({
      fit <- fit()   #semPlotModel
      if (is.null(fit)) return(NULL)
      
      semPaths(fit, what = input$lineType, whatLabels = input$stand, style = "ram", rotation=2, nCharNodes = 1, edge.label.cex = 1.0)   #Change plottype (what) with radiobuttons in ui.R
      #now only double-headed selfloops as "style" instead of "input$resvar"
    })
    })
#Other options plot: the right names
#manifests = c("Mmv", "Ydv", "Xiv"); c(input$Xiv, input$Xmv, input$input$Ydv)
#Feedback when uploaded wrong file type