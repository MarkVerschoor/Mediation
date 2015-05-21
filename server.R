##WIP app

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
      } else {
        if(length(grep("\\.(csv|csv\\.gz)$", tolower(input$datafile$name[1]))) > 0) {
          dat <- read.csv(file = input$datafile$datapath[1], header=TRUE, sep=",")
        } else {
            output$wrongfile <- renderText({
            paste("ERROR: Please upload a file with a .csv or .sav extension.")
            })
          return(NULL)
        }
      }
      output$wrongfile <- renderText({
        paste("")
        })
      
      na.omit(dat) #This line needs to be added, otherwise the .sav files don't show anything in Step 2.
      
    })
    output$fileUploaded <- reactive({
      return(!is.null(filedata()))
    })
    outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)
    
    fit <- reactive({   #everytime one of the elements changes, the model will be recalculated
      df <- filedata()
      if(is.null(df)) return(NULL)    
      
      #Specify the variables below. Needs to be changed to account for multiple or zero control variables.
      df$Xiv <- df[,input$Iv] #Add the variable with the right model name to the dataframe df, so R can find it in line 80
      df$Mmv <- df[,input$M]  #input$M, because the label given to it is "M" (line 45). In ui.R referred to it as mCol -> (output$mCol)
      df$Ydv <- df[,input$Dv] 
      
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
      selectInput("Contv", label = "Control for:", choices = items[!items %in% input$M & !items %in% input$Iv & !items %in% input$Dv], multiple = TRUE) #Only show items that are not selected in M, Iv, and Dv
    })
    
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
  
    output$plot <- renderPlot({
      fit <- fit()   #semPlotModel
      if (is.null(fit)) return(NULL)
      
      semPaths(fit, what = input$lineType, edge.label.bg = TRUE, trans = TRUE, whatLabels = input$stand, style = "ram", rotation=2, nCharNodes = 1, edge.label.cex = 1.0, edge.label.position = 0.6)   #Change plottype (what) with radiobuttons in ui.R
    })
    })
