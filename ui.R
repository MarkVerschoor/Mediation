## WIP app

shinyUI(
  fluidPage(
    titlePanel("Mediation analysis with lavaan"),
    sidebarLayout(
      sidebarPanel(
        h1("Step 1"),
        fileInput("file", label = h3("File input")) # Only .csv or .sav
      ),                                            # Use that to dynamically change the choice list
                                                    # http://shiny.rstudio.com/gallery/telephones-by-region.html
                                                    # http://shiny.rstudio.com/gallery/selectize-examples.html
    mainPanel(
      h1("Step 2"),
      h3("Select variables"),
      selectInput("IndepV", label = h5("Select your Independent Variable"), 
                  choices = list("Choice 1", "Choice 2",
                                 "Choice 3"), selected = "Choice 1"), # You can use selected like this
      selectInput("MedV", label = h5("Select your mediator"), 
                  choices = list("Choice 1", "Choice 2",
                                 "Choice 3"), selected = "Choice 2"),
      selectInput("DepV", label = h5("Select your Dependent Variable"), 
                  choices = list("Choice 1", "Choice 2",
                                 "Choice 3"), selected = "Choice 3"),
      p("You have chosen the dataset ", textOutput("file2", inline = TRUE)),
      
      p("You have selected ", textOutput("IndepV2", inline = TRUE), " as your IV,", textOutput("MedV2", inline = TRUE), "as your mediator, and", textOutput("DepV2", inline = TRUE), "as your dependent variable")
 # Is there a way to create a list with a length based on the amount of variables       

    )
  )
)
)


# Next steps: 
# 1) Only allow .csv and .sav files
# 2) Make the buttons update . Names opvragen van input$file en updaten. Zie vb Daniel
# 3) Gebruiker heeft IV M DV geselecteerd en daar moeten we naar verwijzen, dus die moeten we gebruiken in een functie die een mediatie doet. Misschien met padanalyse in lavaan. Output/summary op het scherm schrijven
# 3) Zie plaatje in daniels app. Website tutorial example (variabele kiezen en dan gemiddeleden )


#      verbatimTextOutput('ex_out'),
