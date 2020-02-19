
  # 0. Packages  --------------------------------------------------------------
  require(shiny)
  require(foreign)
  require(summarytools)
  require(DT)
  require(readr)
  require(readxl)
  
  # Best file input, Credits to SachaEpskamp: https://gist.github.com/SachaEpskamp/5796467
  
  #Set wd as folder of script -> Don't use on shinyapps.io
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  
  if(Sys.getenv('SHINY_PORT') == "") options(shiny.maxRequestSize=10000*1024^2)
  
  # 1. Interface -------------------------------------------------------------------
  
  ui <- fluidPage(
    
    # Header:
    headerPanel("Exploratory Shiny"),
    
    
    # 1.1 Sidepanel -----------------------------------------------------------
    
    # Empty Sidepanel:
    sidebarPanel(
      
      tags$style(type='text/css', ".well { max-width: 20em; }"),
      # Tags:
      tags$head(
        tags$style(type="text/css", "select[multiple] { width: 100%; height:10em}"),
        tags$style(type="text/css", "select { width: 100%}"),
        tags$style(type="text/css", "input { width: 19em; max-width:100%}")
      ),
      
      # Select filetype:
      selectInput("readFunction", "Function to read data:", c(

        #readr
        "read_csv",
        "read_delim",
        
        #readxl
        "read_excel"
        

      )),
      
      # Argument selecter:
      htmlOutput("ArgSelect"),
      
      # Argument field:
      htmlOutput("ArgText"),
      
      # Upload data:
      fileInput("file", "Upload data-file:"),
      
      # Variable selection:
      htmlOutput("varselect"),

      br(),
      

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
    
    ### Argument names:
    ArgNames <- reactive({
      Names <- names(formals(input$readFunction)[-1])
      Names <- Names[Names!="..."]
      return(Names)
    })
    
    # Argument selector:
    output$ArgSelect <- renderUI({
      if (length(ArgNames())==0) return(NULL)
      
      selectInput("arg","Argument:",ArgNames())
    })
    
    ## Arg text field:
    output$ArgText <- renderUI({
      fun__arg <- paste0(input$readFunction,"__",input$arg)
      
      if (is.null(input$arg)) return(NULL)
      
      Defaults <- formals(input$readFunction)
      
      if (is.null(input[[fun__arg]]))
      {
        textInput(fun__arg, label = "Enter value:", value = deparse(Defaults[[input$arg]])) 
      } else {
        textInput(fun__arg, label = "Enter value:", value = input[[fun__arg]]) 
      }
    })
    
    
    ### Data import:
    Dataset <- reactive({
      if (is.null(input$file)) {
        # User has not uploaded a file yet
        return(data.frame())
      }
      
      args <- grep(paste0("^",input$readFunction,"__"), names(input), value = TRUE)
      
      argList <- list()
      for (i in seq_along(args))
      {
        argList[[i]] <- eval(parse(text=input[[args[i]]]))
      }
      names(argList) <- gsub(paste0("^",input$readFunction,"__"),"",args)
      
      argList <- argList[names(argList) %in% ArgNames()]
      
      Dataset <- as.data.frame(do.call(input$readFunction,c(list(input$file$datapath),argList)))
      return(Dataset)
    })
    
    # Select variables:
    output$varselect <- renderUI({
      
      if (identical(Dataset(), '') || identical(Dataset(),data.frame())) return(NULL)
      
      # Variable selection:    
      selectInput("vars", "Variables to use:",
                  names(Dataset()), names(Dataset()), multiple =TRUE)            
    })
    
  
    
    # 3.1 Dataframe -----------------------------------------------------------
    
    
    
    output$x1 = DT::renderDataTable(datatable(Dataset(),
                                              filter = 'top', extensions = c('Buttons',"ColReorder"),
                                              options = list(pageLength = 10, autoWidth = FALSE, colReorder = TRUE,
                                                             columnDefs = list(list(width = '200px', targets = "_all")),
                                                             dom = 'Bfrtip',buttons = c("colvis",'csv', 'excel','pdf'))),
                                    server = FALSE)
    
    # 3.2 dfsummary -----------------------------------------------------------
    
    current_x <- reactive({ Dataset()[input$x1_rows_all,] })
    
    #dfSummary data
    output$x2 <- renderUI({
      SumProfile <- print(dfSummary(current_x()), omit.headings = FALSE,
                          method = 'render',
                          bootstrap.css = FALSE)
      SumProfile
    })
    

# 3.3 download data -------------------------------------------------------

    
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
    }
    
  )
  
  
  # 4. Application ----------------------------------------------------------
  
  
  # Run the application 
  shinyApp(ui = ui, server = server)

