## WIP app

shinyUI(
  pageWithSidebar(
    headerPanel("Mediation analysis with lavaan"),
      sidebarPanel(
        h1("Step 1"),
        fileInput('datafile', 'Choose .csv or .sav file',
                  accept=c('text/csv', 'text/comma-separated-values,text/plain'))
                                                  # Use that to dynamically change the choice list
                                                    # http://shiny.rstudio.com/gallery/telephones-by-region.html
                                                    # http://shiny.rstudio.com/gallery/selectize-examples.html
        ),
    
    mainPanel( 
      h1("Step 2"),
      h3("Select variables"),
      textOutput("step2"),  #Doesn't work yet
      uiOutput("ivCol"),
      uiOutput("mCol"),
      uiOutput("dvCol"),
      
      #Perhaps an action button between step 2 and 3?
      h1("Step 3"),
      h3("Output"),
      tableOutput("summary"),
      textOutput("conclusion")
    )
  )
)


# Next steps: 
# 1) Only allow .csv and .sav files
# 2) Make the buttons update . Names opvragen van input$file en updaten. Zie vb Daniel
# 3) Gebruiker heeft IV M DV geselecteerd en daar moeten we naar verwijzen, dus die moeten we gebruiken in een functie die een mediatie doet. Misschien met padanalyse in lavaan. Output/summary op het scherm schrijven
# 3) Zie plaatje in daniels app. Website tutorial example (variabele kiezen en dan gemiddeleden )


#      verbatimTextOutput('ex_out'),
