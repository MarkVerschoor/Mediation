shinyUI(
  pageWithSidebar(
    headerPanel("Mediation analysis with lavaan and semPlot"),
    sidebarPanel(
      h1("Step 1"),
      h3("Upload data"),
      fileInput('datafile', 'Choose .csv or .sav file.', multiple=FALSE)
    ),
    mainPanel(
      h4(textOutput("wrongfile")),

      # The rest is in a conditional panel, so the app does not clutter the screen prior to uploading a .csv or .sav file.
      conditionalPanel(condition='output.fileUploaded',         
                       h1("Step 2"),
                       h3("Select variables"),
                       uiOutput("ivCol"),
                       uiOutput("mCol"),
                       uiOutput("dvCol"),
                       uiOutput("contCol"),
                       h1("Step 3"),
                       h3("Output"),
                       tableOutput("summary"),
                       textOutput("conclusion"),
                       plotOutput("plot", width = "100%"),
                       h4("Plot options:"),
                       radioButtons("lineType", label = "Line type", choices = c("Unweighted"="path", "Weighted"="est")), 
                       radioButtons("stand", label = "Standardized or unstandardized coefficients", choices = c("Standardized"="stand", "Unstandardized"="est")),
                       p("Mediation analysis by",
                         a("lavaan package", href = "http://cran.r-project.org/web/packages/lavaan/lavaan.pdf"), 
                         "(Yves Rosseel) and plot by",
                         a("semPlot package", href = "http://cran.r-project.org/web/packages/semPlot/semPlot.pdf"),   
                         "(Sacha Epskamp). UI by Mark Verschoor and Lieke Voncken under supervision of Daniel Oberski. See the package documentation for a complete description of the procedures used.")
      )
    )
  )
)