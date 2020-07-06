library(shiny)
library(shinydashboard)
library(curl)
library(httr)
library(jsonlite)
library(DT)
library(shinyalert)
library(stringr)
library(shinyjs)

ui <- dashboardPage(
  skin = "blue",
  title = "DVM Demo",
  dashboardHeader(
    title = h3("DVM Demo", style="font-style: bold; align : top;"),
    titleWidth = 1850
    
  ),
  dashboardSidebar(
    HTML('<center><img src="IBM.jpg" width="220"></center>'),
    tags$hr(),
    sidebarMenu(id="tabs",
      sidebarMenu(
        menuItem(strong("Qery Virtual Table"), tabName = "dashboard2", icon = icon("th"),badgeColor = "green")
      )
    ),
    useShinyalert(),
    useShinyjs(),
    br(),
    br(),
    br(),
    br(),
    br(),
    #HTML('<center><img src="P&PStores.png" width="180"></center>'),
    br(),
    HTML('<center><font size="2"><b>This portal is a containerized shiny R application that uses APIs, hosted in containers, to access data from virtualised Mainframe data sources like VSAM, DB2, IMS etc and joining disperate data sources. </b></font><width="150"></center>'),
    #HTML('<center>This portal is a containerized shiny R application that consumes APIs from Mainframe CICS and integrates with other APIs. The CICS <a href="https://www.ibm.com/support/knowledgecenter/en/SSGMCP_5.1.0/com.ibm.cics.ts.exampleapplication.doc/topics/dfhxa_t100.html" target=_blank>Catalog Manager</a> application is a sample stationary stores application.<width="150"></center>'),
    br(),
    HTML('<center><font size="2"><b>For more info, contact</b></font><width="150"></center>'),
    HTML('<center><font size="2"><b><a href="mailto:shami.gupta@in.ibm.com">Shami Gupta</a></b></font><width="150"></center>'),
    br()
  ),
  dashboardBody(
    tags$img(
      src = "data-cloud.jpg",
      hspace = 0,
      vspace = 0,
      width = '86.5%',height = '90%',
      style = 'position: absolute'
    ),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "United.css")
    ),
    tags$head(
      tags$style(HTML(
        '.content-wrapper, .right-side {background-color: darkcyan;}'
        )
      )
    ),
    tags$head( 
      tags$style(HTML(".main-sidebar { font-size: 16px; }")) #change the font size to 20
    ),
    tags$head(
      tags$style(HTML(
        #'.skin-blue .main-sidebar{background-color: black;}',
        #'.skin-blue .main-sidebar .sidebar{color: red;}',
        #'.skin-blue .main-sidebar .sidebar .sidebar-menu{background-color: grey;}',
        #'.skin-blue .main-sidebar .sidebar .sidebar-menu .active a{background-color: red;}',
        #'.skin-blue .main-header .logo{background-color: black;}',
        '.content-wrapper, .right-side {background-color: darkcyan;}',
        '.box.box-solid.box-primary>.box-header {
        background:rgba(0, 0, 54 ,0);
        background-image: linear-gradient(to right,rgba(0, 0, 139 ,1), rgba(0, 0, 139 ,0));
        }',
        '.box.box-solid.box-success>.box-header {
            background:rgba(20, 90, 50 ,0);
            background-image: linear-gradient(to right,rgba(20, 90, 50 ,1), rgba(20, 90, 50 ,0));
        }',
        '.box.box-solid.box-danger>.box-header {
            background:rgba(120, 40, 31 ,0);
            background-image: linear-gradient(to right,rgba(120, 40, 31 ,1), rgba(120, 40, 31 ,0));
        }',
        '.box.box-solid.box-warning>.box-header {
            background:rgba(126, 81, 9  ,0);
        background-image: linear-gradient(to right,rgba(126, 81, 9 ,1), rgba(126, 81, 9 ,0));
        }',
        '.box.box-solid.box-primary{
            background-image: linear-gradient(to right,rgba(174, 214, 241 ,1), rgba(174, 214, 241 ,0));
        }',
        '.box.box-solid.box-success{
            background-image: linear-gradient(to right,rgba(130, 224, 170 ,1), rgba(130, 224, 170 ,0));
        }',
        '.box.box-solid.box-danger{
            background-image: linear-gradient(to right,rgba(241, 148, 138 ,1), rgba(241, 148, 138 ,0));
        }',
        '.box.box-solid.box-warning{
            background-image: linear-gradient(to right,rgba(248, 196, 113 ,1), rgba(248, 196, 113 ,0));
        }'
      )
      )
    ),
    br(),
    tabItems(
      tabItem(tabName = "dashboard2",
            #HTML("{google_map}60.1705,24.9384{/google_map}"),
            box (
              title = span(icon("table","fa-x"),strong("Data Components")), status = "primary", width = 3,solidHeader = TRUE,collapsible=FALSE,  
              fluidRow(
                column(12, uiOutput("select_table"))
              ),
              fluidRow(
                column(12,h6(DT::dataTableOutput("ListFields")))
              )
            ),
            box (
              title = span(icon("search","fa-x"),strong("Query")), status = "primary", width = 9,solidHeader = TRUE,collapsible=FALSE,  
              fluidRow(
                column(12,textInput("input_sql", h4(strong("SQL on Virtual Table"), style="font-style: bold; color: darkblue"),value = ""))
              ),
              fluidRow(
                column(2, actionButton("RunSQL", label = "Run Query",width='100%',style="font-size: large; font-weight: bold; color: yellow; background-color: darkblue"))
              ),
              fluidRow(
                column(12,h6(DT::dataTableOutput("ListSQLResults")))
              )
            #),
            # box (
            #   title = span(icon("search","fa-x"),strong("Mainframe")), status = "primary", width = 12,solidHeader = TRUE,collapsible=FALSE,  
            #   column(12,htmlOutput("inc"))
            )
          )
    )
  )
)

options(shiny.maxRequestSize = 9*1024^2)

server <- shinyServer(function(input, output, session) {

  #baseurl <- "http://173.193.99.244:30482/"
  baseurl <- "http://localhost:8000/"
  
  store_runsql <- 0
    
  output$select_table <- renderUI({
    urlname <- paste(baseurl,"getDVMzEUSTablesDocker",sep="")
    listoftables <- as.vector(fromJSON(urlname))
    selectInput("selected_table", h4(strong("Select Table"), style="font-style: bold; color: darkblue"),choices=c(as.vector(listoftables)), selected="")
  })
  
  output$ListFields = DT::renderDataTable({
    
    urlname <- paste(baseurl,"getDVMzEUSFieldsDocker?tablename=",input$selected_table,sep="")
    d1 <- data.frame(as.vector(fromJSON(urlname)))
    names(d1) <- "List of Fields"
      
    DT::datatable(
      d1, class = 'cell-border stripe', rownames = FALSE, 
      #caption = h4('Selected Item',style = 'caption-side: top; color:darkgreen ; font-Weight:bold'),
      extensions = list(FixedColumns = list(leftColumns = 1)),
      options = list (dom = 't',
                      #columnDefs = list(list(className = 'dt-center', targets = c(0,2,3,4,5))),
                      headerCallback = JS(headerCallback3)
      )
    )%>%
      formatStyle(columns = c(1), fontSize = '130%', color = 'white', fontWeight = 'bold', backgroundColor = 'darkblue') 
  })
  
  headerCallback3 <- c(
    "function(thead, data, start, end, display){",
    "  $('th', thead).css('border-top', '2px solid darkblue');",
    "  $('th', thead).css('border-bottom', '2px solid darkblue');",
    "  $('th', thead).css('color', 'darkblue');",
    "}"
  )
  
  output$ListSQLResults = DT::renderDataTable({
    d1 <- data.frame()
    if (input$RunSQL > store_runsql) {
      store_runsql <<- input$RunSQL
      res <- POST(paste(baseurl,"getDVMzEUSDocker?",sep="")
                  ,body=list(myquerry = input$input_sql),
                  ,encode = "json")
      appData <- content(res)
      
      if (appData[[2]][[2]] > 0) {
        d1 <- as.data.frame(matrix(unlist(appData[[1]]), ncol=appData[[2]][[2]], byrow=TRUE), stringsAsFactors=FALSE)
        names(d1) <- names(data.frame(appData[[1]][[1]]))
      }
    }

    DT::datatable(
      d1, class = 'cell-border stripe', rownames = FALSE, 
      #caption = h4('Selected Item',style = 'caption-side: top; color:darkgreen ; font-Weight:bold'),
      extensions = list(FixedColumns = list(leftColumns = 1)),
      options = list (dom = 't',scrollX = TRUE,fixedColumns = TRUE,
                      #columnDefs = list(list(className = 'dt-center', targets = c(0,2,3,4,5))),
                      headerCallback = JS(headerCallback3)
      )
    )%>%
      formatStyle(columns = c(1), fontSize = '130%', color = 'white', fontWeight = 'bold', backgroundColor = 'darkblue') 
  })
  
  getPage<-function() {
    #return(tags$iframe(src = "https://192.86.33.143:10443/zosmf/LogOnPanel.jsp"
    return(tags$iframe(src = "http://e1.c3.33a9.ip4.static.sl-reverse.com:31252/"
                       , style="width:100%;",  frameborder="0"
                       ,id="iframe"
                       , height = "500px"))
  }
  
  output$inc<-renderUI({
    x <- input$test  
    getPage()
  })
  
  
    
})

shinyApp(ui = ui, server = server)