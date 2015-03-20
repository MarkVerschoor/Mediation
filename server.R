## WIP app

shinyServer(
  function(input, output) {
    
    output$file2 <- renderText({ 
      paste(input$file)  # The dataset
    })
    
    output$IndepV2 <- renderText({
      paste(input$IndepV) #InDepV is in the ui.R. IndepV2 is in the server.R. Ever
    })
    
    output$MedV2 <- renderText({
      paste(input$MedV)
    })
    
    output$DepV2 <- renderText({
      paste(input$DepV)
    })

  }
)