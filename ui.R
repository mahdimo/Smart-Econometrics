library(shinyBS)
shinyUI(fluidPage(
  titlePanel("Smart Econometrics (test)"),
  p("Author:",
    a(href="http://www.moqri.com/", "Mahdi Moqri"),
    tags$br(),"Warrington College of Business Administration"),
  sidebarLayout(
    sidebarPanel(
      fileInput('file1', 'Choose file to upload',accept = c('text/csv','text/comma-separated-values','text/tab-separated-values','text/plain','.csv','.tsv')),
      tags$hr(), checkboxInput('header', 'Header', TRUE), radioButtons('sep', 'Separator', c(Comma=',',Semicolon=';',Tab='\t'), ','), radioButtons('quote', 'Quote',   c(None='','Double Quote'='"','Single Quote'="'"),'"'),tags$hr(),      
      selectInput("dVar", "Depenedent Variable:",c( )),
      selectInput("indVar", "Indepenedent Variables:",c( ),multiple = TRUE)
      
    ),
    mainPanel(
      bsCollapse(multiple = TRUE, open = "col5", id = "collapse1",
                 bsCollapsePanel("Data", div(dataTableOutput('contents'), id="col1", value="test1", style = 'overflow:auto;')),
                 bsCollapsePanel("Summary", tableOutput('summary'), id="col2", value="test2"),
                 bsCollapsePanel("Plots", 
                                 selectInput("pVar", "Plot Variable:",c( )),
                                 plotOutput('distPlot'), id="col3", value="test3"),
                 bsCollapsePanel("Correlatins", plotOutput('corrPlot'), id="col4", value="test4"),
                 bsCollapsePanel("Model", htmlOutput("formula"), tags$br(), tableOutput('model'), id="col5", value="test5"),
                 bsCollapsePanel("Validation", htmlOutput('multicollinearity'),htmlOutput('autocorrelation'),
                                 checkboxInput('robust', label = "Robust Estimates", value = FALSE),tags$br(),
                                 htmlOutput('functionalMiss'),
                                 id="col6", value="test6")
      ),
      textOutput("text1")
    )
    
  )
))