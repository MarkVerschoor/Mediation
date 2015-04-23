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
      textOutput("step2"),  #Doesn't work yet: too small
      uiOutput("ivCol"),
      uiOutput("mCol"),
      uiOutput("dvCol"),
      textOutput("space"),
      h1("Step 3"),
      h3("Output"),
      tableOutput("summary"),
      textOutput("conclusion"),
      plotOutput("plot"),
      radioButtons("lineType", label = "Line type", 
                   choices = c("Unweighted"="path", "Weighted"="est", "Standardized as weighted"="stand")), 
                      #Can extend options. Note: first name is name in app and second is argument
     #Better if the buttons are not shown yet if no plot is shown
      radioButtons("resvar", label = "Residual variance type", 
                  choices = c("double headed selfloops"="ram", "single headed edge without node as origin"="lisrel")),
      radioButtons("stand", label = "standardized/unstandardized coefficients", 
                  choices = c("standardized"="stand", "unstandardized"="est")),
      helpText("Mediation analysis by lavaan package (Yves Rosseel) and plot by semPlot package (Sacha Epskamp).",
               "UI by Mark Verschoor and Lieke Voncken.", "See the package documentation for complete description",
               "of the procedures used.")
      
    )
  ))

# Meaning standardized as weighted