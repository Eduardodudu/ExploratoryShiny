Exploratory <- function(Data){

  # Must have data.rmd on same folder location
  
  # 0. Packages  --------------------------------------------------------------
  require(shiny)
  require(foreign)
  require(summarytools)
  require(DT)
  require(shinydashboard)
  
  
  if(Sys.getenv('SHINY_PORT') == "") options(shiny.maxRequestSize=10000*1024^2)
  
  # 1. Interface -------------------------------------------------------------------
  
  ui <- fluidPage(
    
    # Header:
    headerPanel("Exploratory Shiny"),
    
    
    # 1.1 Sidepanel -----------------------------------------------------------
    
    # Empty Sidepanel:
    sidebarPanel(
      downloadButton(outputId = "download_data",
                     label = "Download Summary PDF")
    ),
    
    # 1.2 Main ----------------------------------------------------------------
    
    # Main:
    mainPanel(
      
      tableOutput("table"),
      
      tabsetPanel(
        tabPanel("Dataframe", DT::dataTableOutput('x1')), 
        tabPanel("Data Summary", htmlOutput('x2', height = 500))
      )
    )
  )
  
  
  
  
  # 3. Server ---------------------------------------------------------------
  
  server <- shinyServer(function(input, output) {
    
    
    # 3.1 Dataframe -----------------------------------------------------------
    
    
    
    # two columns of the mtcars data
    # mtcars2 = mtcars#[, c('hp', 'mpg')]
    mydata <- reactive(Data)
    
    # render the table (with row names)
    
    output$x1 = DT::renderDataTable(datatable(mydata(),
                                              filter = 'top', extensions = c('Buttons',"ColReorder"),
                                              options = list(pageLength = 10, autoWidth = FALSE, colReorder = TRUE,
                                                             columnDefs = list(list(width = '200px', targets = "_all")),
                                                             dom = 'Bfrtip',buttons = c("colvis",'csv', 'excel','pdf'))),
                                    server = FALSE)
    
    # 3.2 dfsummary -----------------------------------------------------------
    
    current_x <- reactive({ mydata()[input$x1_rows_all,] })
    
    #dfSummary data
    output$x2 <- renderUI({
      SumProfile <- print(dfSummary(current_x()), omit.headings = FALSE,
                          method = 'render',
                          bootstrap.css = FALSE)
      SumProfile
    })
    
    
    output$download_data <- downloadHandler(
      filename = "Summary.pdf",
      content = function(file) {
        
        tempReport <- file.path(tempdir(), "./Data.Rmd")
        file.copy("Data.Rmd", tempReport, overwrite = TRUE)
        
        params <- list(n = current_x())
        
        rmarkdown::render(tempReport, output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv()))
        
        
        
      }
    )
    
  })
  
  
  # 4. Application ----------------------------------------------------------
  
  
  # Run the application 
  shinyApp(ui = ui, server = server)
}
