## WIP app

shinyUI(
  pageWithSidebar(
    headerPanel("Mediation analysis with lavaan"),
      sidebarPanel(
        h1("Step 1"),
        fileInput('datafile', 'Choose .csv or .sav file.', multiple=FALSE))
                                                                
        ,
    
    mainPanel( 
      h1("Step 2"),
      h3("Select variables"),
      textOutput("step2"),  #Doesn't work yet
      uiOutput("ivCol"),
      uiOutput("mCol"),
      uiOutput("dvCol"),
      
      h1("Step 3"),
      h3("Output"),
      tableOutput("summary"),
      radioButtons("plotType", label = "Plot type", 
                   choices = c("Unweighted"="path", "Weighted"="est", "Standardized as weighted"="stand")), 
                      #Can extend options. Note: first name is name in app and second is argument
     #Better if the buttons are not shown yet if no plot is shown
      plotOutput("plot"),
      helpText("Mediation analysis by lavaan package (Yves Rosseel) and plot by semPlot package (Sacha Epskamp).",
               "UI by Mark Verschoor and Lieke Voncken.", "See the package documentation for complete description",
               "of the procedures used."),
      textOutput("conclusion")
    )
  ))