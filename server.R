## WIP app

if(!require(lavaan)){install.packages('plyr')}
if(!require(shiny)){install.packages('shiny')}
require(plyr)
require(shiny)

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
      selectInput("m", label = "Mediatior:", choices = items) #Only show items that are not selected in ivCol

    })
    
    output$dvCol <- renderUI({
      df <-filedata()
      if (is.null(df)) return(NULL)
      
      items=names(df)
      names(items)=items
      selectInput("Dv", label = "Dependent Variable:", choices = items)
    })
        
    model <- ' # direct effect
    Y ~ c*X
    # mediator
    M ~ a*X
    Y ~ b*M
    # indirect effect (a*b)
    indirect := a*b
    # total effect
    total := c + (a*b)
    '
    
    df <- filedata()
    if(is.null(df)) return(NULL)
    
    fit <- sem(model, data = df) ###### NOT ALLOWED ######
    sumfit <- summary(fit)  # This as table
    
    # Sobel test = ab (=indirect effect) / SE (=its standard error of measurement)
    # So, this is already tested in lavaan.
    
    param.ests <- parameterEstimates(fit)
    est.eff <- param.ests[[5]][c(1,7:8)] #1 for direct, 7 for indirect and 8 for total effect
    se.eff <- param.ests[[6]][c(1,7:8)]
    z.eff <- param.ests[[7]][c(1,7:8)]
    p.eff <- param.ests[[8]][c(1,7:8)]
    df.eff <- cbind(est.eff, se.eff, z.eff, p.eff); df.eff
    row.names(df.eff) <- c("direct effect", "indirect effect", "total effect")
    colnames(df.eff) <- c("est", "se", "z-value", "p-value")
        
    mediation <- function (){
      if(df.eff["indirect effect","p-value"] < 0.05 && df.eff["direct effect","p-value"] < 0.05){    #both significant = partial mediation
        mediationtext ="Partial Mediation"
      }else{
        if(df.eff["indirect effect","p-value"] < 0.05 && df.eff["direct effect","p-value"] > 0.05){  #only indirect significant = full mediation
          mediationtext ="Full Mediation"
        } else {
          mediationtext ="No Mediation"   #Nog verder specificeren
        }
      }
    }
    mediationtext <- mediation(); mediationtext
    
    output$conclusion <- renderText({
      df <- filedata()
      if(is.null(df)) return(NULL)
      
      paste("The result of the mediation analysis is", mediationtext)
    })
  })

