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
      #textOutput("step2"),  #Doesn't work yet: too small
      uiOutput("ivCol"),
      uiOutput("mCol"),
      uiOutput("dvCol"),
      uiOutput("contCol"),
      textOutput("space"),
      h1("Step 3"),
      h3("Output"),
      tableOutput("summary"),
      textOutput("conclusion"),
      plotOutput("plot", width = "80%"),
      h4("Plot options:"),
      radioButtons("lineType", label = "Line type", 
                   choices = c("Unweighted"="path", "Weighted"="est")), 
                      #Can extend options. Note: first name is name in app and second is argument. e.g., "Standardized as weighted"="stand"
     #Better if the buttons are not shown yet if no plot is shown
     #radioButtons("resvar", label = "Residual variance type", 
                  #choices = c("Double headed selfloops"="ram", "Single headed edge without node as origin"="lisrel")),
      radioButtons("stand", label = "Standardized or unstandardized coefficients", 
                  choices = c("Standardized"="stand", "Unstandardized"="est")),
      helpText("Mediation analysis by lavaan package (Yves Rosseel) and plot by semPlot package (Sacha Epskamp).",
               "UI by Mark Verschoor and Lieke Voncken.", "See the package documentation for complete description",
               "of the procedures used.")
      
    )
  ))

# Meaning standardized as weighted