# Shiny app Mediation analysis. Server

# Load required packages if necessary -----------------------------------------
if(!require(lavaan)){install.packages('plyr')}
if(!require(shiny)){install.packages('shiny')}
if(!require(lavaan)){install.packages('foreign')}
if(!require(lavaan)){install.packages('semPlot')}
require(plyr)
require(shiny)
require(foreign)
require(semPlot)

# Create an empty df for shiny to work with. Needs to be done prior to shinyServer()
df <- NULL

shinyServer(
  function(input, output) {
    
    # Handle the file upload
    filedata <- reactive({
      infile <- input$datafile
      if (is.null(infile)) {
        # User has not uploaded a file yet, so don't show anything
        return(NULL)
      }
      
      # Uploading the data is based on the extension. If it's .sav, do read.spss(). 
      # If it's .csv, do read.csv(). Else, paste error and return(NULL).
      if (length(grep("\\.(sav|por)$", tolower(input$datafile$name[1]))) > 0) {
        dat <- read.spss(file = input$datafile$datapath[1], to.data.frame=TRUE, use.value.labels = FALSE, use.missings = TRUE)
      } else {
        if (length(grep("\\.(csv|csv\\.gz)$", tolower(input$datafile$name[1]))) > 0) {
          dat <- read.csv(file = input$datafile$datapath[1], header=TRUE, sep=",")
        } else {
          output$wrongfile <- renderText({
            paste("ERROR: Please upload a file with a .csv or .sav extension.")
          })
          return(NULL)
        }
      }

      # Replace errortext by nothing -> errortext disappears even when it was there before.
      output$wrongfile <- renderText({
        paste("")
      })
      
      # This line needs to be added, otherwise the .sav files don't show anything in Step 2.
      na.omit(dat) 
    })
    
    output$fileUploaded <- reactive({
      return(!is.null(filedata()))
    })
    
    outputOptions(output, 'fileUploaded', suspendWhenHidden = FALSE)
    
    fit <- reactive({   #Everytime one of the elements changes, the model will be recalculated
      df <- filedata()
      if (is.null(df)) return(NULL)    
      
      # Add the variables with the right model names to the dataframe df ------
      df$Xiv <- df[,input$iv] 
      df$Mmv <- df[,input$m] 
      df$Ydv <- df[,input$dv] 
      
      # Specify the model. Gets adjusted based on length(input$Contv)
      model <- '
      Ydv ~ c * Xiv
      #mediator
      Mmv ~ a * Xiv {covariates}
      Ydv ~ b * Mmv {covariates}
      #indirect effect (a * b)
      indirect := a * b
      #total effect
      total := c + (a * b)      
      '
      
      # Adjust the specified model based on the amount of covariates. 
      if (length(input$Contv) != 0) {
        model <- gsub("\\{covariates\\}", paste(" + " , paste(input$Contv, collapse = "+")), model)
      } else {
        model <- gsub("\\{covariates\\}", "", model)
      }
      
      # Analysis is done using sem()
      sem(model, data = df)
    })
    
    # Populate the list boxes in the UI with column names from the uploaded file.
    # For mediator, DV, and control variables, it does not show options already selected. 
    # Populate IV list box
    output$ivcol <- renderUI({
      df <- filedata()
      if (is.null(df)) return(NULL)
      items=names(df)
      names(items)=items
      selectInput("iv", label = "Independent Variable (X):", choices = items)
    })
    
    # Populate median list box
    output$mcol <- renderUI({
      df <- filedata()
      if (is.null(df)) return(NULL)
      items = names(df)
      names(items) = items
      selectInput("m", label = "Mediator (M):", choices = items[!items %in% input$iv]) #Only show items that are not selected in ivCol
    })
    
    # Populate DV list box
    output$dvcol <- renderUI({
      df <- filedata()
      if (is.null(df)) return(NULL)
      items = names(df)
      names(items) = items
      selectInput("dv", label = "Dependent Variable (Y):", choices = items[!items %in% input$m & !items %in% input$iv]) #Only show items that are not selected in both M and Iv
    })
    
    # Populate control variable list box
    output$contcol <- renderUI({
      df <- filedata()
      if (is.null(df)) return(NULL)
      items = names(df)
      names(items) = items
      selectInput("Contv", label = "Control for:", choices = items[!items %in% input$m & !items %in% input$iv & !items %in% input$dv], multiple = TRUE) #Only show items that are not selected in M, Iv, and Dv
    })
    
    # Create the table with the statistical results ---------------------------
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
    
    # Create the conclusion text. 
    output$conclusion <- renderText({
      fit <- fit() # Refer to "fit" that changes (is reactive)
      if (is.null(fit)) return(NULL)
      param.ests <- parameterEstimates(fit, standardized = TRUE)  #standardized
      df.eff <- as.data.frame(rbind(param.ests[param.ests$label == 'c', c('est', 'se', 'z', 'pvalue', 'std.all')],
                                    param.ests[param.ests$label == 'indirect', c('est', 'se', 'z', 'pvalue', 'std.all')],
                                    param.ests[param.ests$label == 'total', c('est', 'se', 'z', 'pvalue', 'std.all')]
      ))
      if (df.eff[2, 4] < 0.05 && df.eff[1, 4] < 0.05){    #both significant = partial mediation
        mediationtext = "PARTIAL MEDIATION."
      } else {
        if (df.eff[2, 4] < 0.05 && df.eff[1, 4] > 0.05){  #only indirect significant = full mediation
          mediationtext = "FULL MEDIATION."
        } else {
          mediationtext = "NO MEDIATION."
        }
      }
      paste("Lavaan shows us that there is", mediationtext)
    })
    
    # Create the plot using semPaths() ----------------------------------------
    output$plot <- renderPlot({
      fit <- fit()   # semPlotModel
      if (is.null(fit)) return(NULL)
      
      semPaths(fit, what = input$linetype, edge.label.bg = TRUE, trans = TRUE, whatLabels = input$stand, style = "ram", rotation=2, nCharNodes = 1, edge.label.cex = 0.8, edge.label.position = 0.6)   #Change plottype (what) with radiobuttons in ui.R
    })
  })