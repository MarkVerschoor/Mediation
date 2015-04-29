## WIP app

# Include control variables
# (Standardized estimates in output lavaan)

if(!require(lavaan)){install.packages('plyr')}
if(!require(shiny)){install.packages('shiny')}
if(!require(lavaan)){install.packages('foreign')}
if(!require(lavaan)){install.packages('semPlot')}
require(plyr)
require(shiny)
require(foreign)
require(semPlot)

df <- NULL

shinyServer(
  function(input, output) {
   
    #Handle the file upload
    filedata <- reactive({
      infile <- input$datafile
      if (is.null(infile)) {
        # User has not uploaded a file yet
        return(NULL)
      }

    # read.spss(infile$datapath, to.data.frame=TRUE, use.value.labels = FALSE, use.missings = TRUE)
    read.csv(infile$datapath) 
    
  })
  
  fit <- reactive({   #everytime one of the elements changes, the model will be recalculated
    df <- filedata()
    if(is.null(df)) return(NULL)    
    
    df$Xiv <- df[,input$Iv] #Add the variable with the right model name to the dataframe df, so R can find it in line 80
    df$Mmv <- df[,input$M]       #input$M, because the label given to it is "M" (line 45). In ui.R referred to it as mCol -> (output$mCol)
    df$Ydv <- df[,input$Dv] 
    
    model <- '
        Ydv ~ c*Xiv      #Ydv instead of Y, etcetera, because X, M and Y often already occur in the dataset. Problem if these found.
        # mediator
        Mmv ~ a*Xiv
        Ydv ~ b*Mmv
        # indirect effect (a*b)
        indirect := a*b
        # total effect
        total := c + (a*b)      
        '
    
    sem(model, data = df)
    # Another fit with "std.ov = TRUE" -> option to show standardized/unstandardized plot later on
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
        # In this way, the variables can never be selected twice. Iv offers the choice of the variable that is selected as Dv, but then the selected variable of Dv will change.
    # This makes it easier to change the variables. You probably choose X first, which offers all variables as option, and this restricts the later variables.
    # It is harder if you select a particular variable in X and you want to make it your Dv, because you need to select a different X first, but this will occur less often.
    
  output$space <- renderText({
    paste(" ")
  })
  
    output$conclusion <- renderText({
        fit <- fit()                  #Refer to "fit" that changes (is reactive)
        if (is.null(fit)) return(NULL)
        param.ests <- parameterEstimates(fit, standardized = TRUE)  #standardized
        p.eff <- param.ests[[8]][c(1,7:8)]
        names(p.eff) <- c("direct effect", "indirect effect", "total effect")
        
          if(p.eff["indirect effect"] < 0.05 && p.eff["direct effect"] < 0.05){    #both significant = partial mediation
          mediationtext = "PARTIAL MEDIATION"
          }else{
            if(p.eff["indirect effect"] < 0.05 && p.eff["direct effect"] > 0.05){  #only indirect significant = full mediation
              mediationtext ="FULL MEDIATION"
            } else {
              mediationtext = "NO MEDIATION"
            }
          }
        #Now changed so doesn't have to calculate the whole table, but only the p-value.
      
        paste("Lavaan shows us that there is", mediationtext,".")
      })
    
    
    output$summary <- renderTable({   #Refer to "fit" that changes (is reactive)
      fit <- fit()
      if (is.null(fit)) return(NULL)
      
      param.ests <- parameterEstimates(fit, standardized = TRUE)  #standardized
      est.eff <- param.ests[[5]][c(1,7:8)] #1 for direct, 7 for indirect and 8 for total effect
      est.effS <- param.ests[[12]][c(1,7:8)] #Standardized coefficients
      se.eff <- param.ests[[6]][c(1,7:8)]
      z.eff <- param.ests[[7]][c(1,7:8)]
      p.eff <- param.ests[[8]][c(1,7:8)]
      df.eff <- cbind(est.eff, est.effS, se.eff, z.eff, p.eff)
      row.names(df.eff) <- c("direct effect", "indirect effect", "total effect")
      colnames(df.eff) <- c("unstd. est", "std. est", "se", "z-value", "p-value")
      df.eff
      })
  
  # Changed, so also includes standardized coefficients
  
    output$plot <- renderPlot({
      fit <- fit()   #semPlotModel
      if (is.null(fit)) return(NULL)
      
      semPaths(fit, what = input$lineType, whatLabels = input$stand, style = input$resvar, rotation=2, nCharNodes = 1)   #Change plottype (what) with radiobuttons in ui.R
      
    })

    })

# Other options plot: the right names
# manifests = c("Mmv", "Ydv", "Xiv"); c(input$Xiv, input$Xmv, input$input$Ydv)


# Use the right variable names in the model -> make model with "input$Iv" etc. Doesn't work.

# Feedback is uploaded wrong file type
