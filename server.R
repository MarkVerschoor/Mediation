## WIP app

if(!require(lavaan)){install.packages('plyr')}
if(!require(shiny)){install.packages('shiny')}
require(plyr)
require(shiny)

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
      read.csv(infile$datapath)
    })
    
    
      step2 <- renderText({
      if (!is.null(df))  #If df (input datafile) is not null, then paste
      paste("Step 2")
      })
       #HIER VERWIJZEN NAAR DE STEP 2 EN SELECT VARIABLES TO MAKE THEM APPEAR WHEN THE DATA IS LOADED
    
    #Populate the list boxes in the UI with column names from the uploaded file  
    output$ivCol <- renderUI({
      df <-filedata()
      if (is.null(df)) return(NULL)

      items=names(df)
      names(items)=items
      
      selectInput("Iv", label = "Independent Variable:", choices = items)
    })
    
    output$mCol <- renderUI({
      df <-filedata()
      if (is.null(df)) return(NULL)
      
      items=names(df)
      names(items)=items
      selectInput("M", label = "Mediatior:", choices = items) #Only show items that are not selected in ivCol

    })
    
    output$dvCol <- renderUI({
      df <-filedata()
      if (is.null(df)) return(NULL)
      
      items=names(df)
      names(items)=items
      selectInput("Dv", label = "Dependent Variable:", choices = items)
    })
        
    
  
    output$conclusion <- renderText({
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
    
    fit <- sem(model, data = df) ###### NOT ALLOWED ######
        
    # Sobel test = ab (=indirect effect) / SE (=its standard error of measurement)
    # So, this is already tested in lavaan.
        
    param.ests <- parameterEstimates(fit)
    est.eff <- param.ests[[5]][c(1,7:8)] #1 for direct, 7 for indirect and 8 for total effect
    se.eff <- param.ests[[6]][c(1,7:8)]
    z.eff <- param.ests[[7]][c(1,7:8)]
    p.eff <- param.ests[[8]][c(1,7:8)]
    df.eff <- cbind(est.eff, se.eff, z.eff, p.eff)
    row.names(df.eff) <- c("direct effect", "indirect effect", "total effect")
    colnames(df.eff) <- c("est", "se", "z-value", "p-value")
    
    if(df.eff["indirect effect","p-value"] < 0.05 && df.eff["direct effect","p-value"] < 0.05){    #both significant = partial mediation
      mediationtext = "PARTIAL MEDIATION"
      }else{
        if(df.eff["indirect effect","p-value"] < 0.05 && df.eff["direct effect","p-value"] > 0.05){  #only indirect significant = full mediation
          mediationtext ="FULL MEDIATION"
        } else {
          mediationtext = "NO MEDIATION"
        }
      }
      
      paste("Lavaan shows us that there is", mediationtext,".")
    })
    
    
    output$summary <- renderDataTable({
      sumfit <- summary(fit)  # This as table
      if(is.null(df)) return(NULL)
      sumfit
    })
    
    
  })


# Werkt allemaal, behalve dat je wel 3 verschillende variabelen moet kiezen om het te laten werken.
