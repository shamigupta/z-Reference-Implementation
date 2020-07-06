library(shiny)
library(shinydashboard)
library(plotly)
library(curl)
library(httr)
library(jsonlite)
library(DT)
library(shinyalert)
library(stringr)
#library(shinyjs)
#library(googleVis)
library(dplyr)
library(htmlwidgets)
#library(shinyBS)
#library(base64enc)
library(data.table)


ui <- dashboardPage(
  skin = "blue",
  title = "z/OS Operations",
  dashboardHeader(
    title = h3("z/OS Operations", style="font-style: bold; align : top;"),
    titleWidth = 2250
    
  ),
  dashboardSidebar(
    HTML('<center><img src="IBM.jpg" width="220"></center>'),
    tags$hr(),
    sidebarMenu(id="tabs",
      sidebarMenu(
        menuItem(strong("Monitor Batch Jobs"), tabName = "dashboard1", icon = icon("th"),badgeColor = "green"),
        menuItem(strong("Re-Submit Job"), tabName = "dashboard2", icon = icon("th"),badgeColor = "green"),
        menuItem(strong("File Management"), tabName = "dashboard3", icon = icon("th"),badgeColor = "green"),
        menuItem(strong("Data Management"), tabName = "dashboard4", icon = icon("th"),badgeColor = "green")
      )
    ),
    useShinyalert(),
#    useShinyjs(),
#    extendShinyjs(text = "shinyjs.resetClick = function() { Shiny.onInputChange('.clientValue-plotly_click-A', 'null'); }"),
    br(),
    br(),
    br(),
    br(),
    br(),
    #HTML('<center><img src="P&PStores.png" width="180"></center>'),
    br(),
    HTML('<center><font size="2"><b>This portal is a containerized shiny R application that uses zos mf APIs, hosted in containers, to monitor the batch environment and files. </b></font><width="150"></center>'),
    br(),
    HTML('<center><font size="2"><b>For more info, contact</b></font><width="150"></center>'),
    HTML('<center><font size="2"><b><a href="mailto:shami.gupta@in.ibm.com">Shami Gupta</a></b></font><width="150"></center>'),
    br()
  ),
  dashboardBody(
    #useShinyjs(),
    tags$img(
      src = "data-cloud.jpg",
      hspace = 0,
      vspace = 0,
      width = '90.5%',height = '90%',
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
    tags$style(type="text/css", "#display_listing {white-space: pre-wrap; font-family: Courier New;}"),
    tags$style(type="text/css", "#display_be_file {white-space: pre-wrap; font-family: Courier New;}"),
    
    tags$style(type="text/css", "#accept_jcl {white-space: pre-wrap; font-family: Courier New;}"),
    tags$style(type="text/css", "#display_be_file_edit {white-space: pre-wrap; font-family: Courier New;}"),
    
    tags$style(type="text/css", "#showdata {white-space: pre-wrap; font-family: Courier New;}"),
    tags$style(type="text/css", "#display_listing_table {white-space: pre-wrap; font-family: Courier New;}"),
    tags$style(type="text/css", "#display_listing_table tr {background:rgba(174, 214, 241 , 0);font-family: Courier New;font-size: 12px;}"),
    tags$head(
      tags$style(HTML(
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
      tabItem(tabName = "dashboard1",
        fluidRow(
          column(width = 6,
            box (
              title = span(icon("list-alt","fa-x"),strong("Joblist")), status = "primary", width = 12,solidHeader = TRUE,collapsible=FALSE,  
              fluidRow(
                column(1,h4(strong("Owner"), style="font-style: bold; color: darkblue")),
                column(2,textInput("input_owner", label=NULL),value = ""),
                column(1,offset=6,h4(strong("Prefix"), style="font-style: bold; color: darkblue")),
                column(2,uiOutput("input_prefix_set"))
                #column(2,textInput("input_prefix", label=NULL),value = "")
              ),
              fluidRow(
                column(12,h6(DT::dataTableOutput("ListJobs")))
              ),
              fluidRow(
                column(2, actionButton("refreshList", label = h5("Refresh",style="font-weight: bold; color: yellow"),width='100%',style="background-color: darkblue")),
                column(2, actionButton("resubmitList", label = h5("Resubmit",style="font-weight: bold; color: yellow"),width='100%',style="background-color: darkgreen")),
                column(2, actionButton("purgeList", label = h5("Purge",style="font-weight: bold; color: yellow"),width='100%',style="background-color: darkred")),
                column(2,textOutput("job_select_message"))
              )
            )
          ),
          column(width = 6,
            conditionalPanel(
              condition = "output.job_select_message != ''",
              box (
                title = span(icon("folder-open","fa-x"),strong("Job Details")), status = "primary", width = 12,solidHeader = TRUE,collapsible=FALSE,  
                fluidRow(
                  column(2,h4(strong("Job Name"), style="font-style: bold; color: darkblue")),
                  column(2,h4(strong(textOutput("display_jobname")), style="font-weight: bold; color: darkblue")),
                  column(2,offset=4, h4(strong("Job ID"), style="font-style: bold; color: darkblue")),
                  column(2,h4(strong(textOutput("display_jobid")), style="font-weight: bold; color: darkblue"))
                ),
                fluidRow(
                  column(12,h6(DT::dataTableOutput("ListJobOutput")))
                ),
                fluidRow(
                  column(2,textOutput("joboutput_select_message"))
                )
              )
            )
          )
        ),
        fluidRow(
          column(width = 6,
            box (
              title = span(icon("chart-bar","fa-x"),strong("Job Stat")), status = "primary", width = 12,solidHeader = TRUE,collapsible=FALSE,  
              
              #title = strong(textOutput("JobStat")), status = "primary", width = 12,solidHeader = TRUE,collapsible=FALSE,  
              conditionalPanel(
                condition = "output.job_select_message != ''",
                fluidRow(
                  column(12,h4(strong(textOutput("JobStat")), style="font-weight: bold; color: darkblue"))
                ),
                fluidRow(
                  #column(12,htmlOutput("CPU_usage_graph"))
                  column(12,plotlyOutput("CPU_usage_graph"))
                )
              )
            )
          ),
          column(width = 6,
            box (
              title = span(icon("file","fa-x"),strong("Job Log")), status = "primary", width = 12,solidHeader = TRUE,collapsible=FALSE,  
              conditionalPanel(
                condition = "output.joboutput_select_message != ''",
                fluidRow(
                  column(12,h6(DT::dataTableOutput("display_listing_table")))
                ),
                fluidRow(
                  column(6,h6(textOutput("display_file")))
                )  
              )
            )
          )
        )
      ),
      tabItem(tabName = "dashboard2",
        box (
          title = span(icon("elementor","fa-x"),strong("Job Submit Panel")), status = "primary", width = 12,solidHeader = TRUE,collapsible=FALSE,
          fluidRow(
            column(2, actionButton("submitjob", label = h5("Submit Job",style="font-weight: bold; color: yellow"),width='100%',style="background-color: darkgreen"))
          ),
          fluidRow(
            column(10,uiOutput("update_jcl"))
          )
        )
      ),
      tabItem(tabName = "dashboard3",
        box (
          title = span(icon("elementor","fa-x"),strong("File Management Panel")), status = "primary", width = 12,solidHeader = TRUE,collapsible=FALSE,  
          fluidRow(
            column(3,selectInput("selected_file_operation", h4(strong("Select Operation"), style="font-style: bold; color: darkblue"), choices=c("Create","Browse/Edit","Rename/Copy","Delete"),selected = "Browse/Edit"))
          )
        ),
        conditionalPanel(
          condition = "input.selected_file_operation == 'Create'",
          box (
            title = span(icon("plus-square","fa-x"),strong("Create Dataset")), status = "primary", width = 12,solidHeader = TRUE,collapsible=FALSE,  
            fluidRow(
              column(6,textInput("accept_dsname", h4(strong("Dataset Name"), style="font-style: bold; color: darkblue"),value = ""))
            ),
            fluidRow(
              column(2,selectInput("selected_ds_type", h4(strong("Type"), style="font-style: bold; color: darkblue"), choices=c("PS","PO"),selected = "PS")),
              column(2,selectInput("selected_allocation", h4(strong("Allocation"), style="font-style: bold; color: darkblue"), choices=c("TRK","BLK","CYL"),selected = "TRK")),
              column(2,numericInput("selected_primary", h4(strong("Primary"), style="font-style: bold; color: darkblue"),value = 1)),
              column(2,numericInput("selected_secondary", h4(strong("Secondary"), style="font-style: bold; color: darkblue"),value = 1)),
              conditionalPanel(
                condition = "input.selected_ds_type == 'PO'",
                column(2,numericInput("selected_dir_blk", h4(strong("Directory Block"), style="font-style: bold; color: darkblue"),value = 1))
              )
            ),
            fluidRow(
              column(2,selectInput("selected_rec_format", h4(strong("Rec Format"), style="font-style: bold; color: darkblue"), choices=c("FB","VB"),selected = "FB")),
              column(2,numericInput("selected_lrecl", h4(strong("Rec Length"), style="font-style: bold; color: darkblue"),value = 80)),
              column(2,numericInput("selected_blksize", h4(strong("Block Size"), style="font-style: bold; color: darkblue"),value = 800)),
              column(2,numericInput("selected_avg_blk", h4(strong("Avg Block"), style="font-style: bold; color: darkblue"),value = 800))
            ),
            fluidRow(
              column(2, actionButton("createds", label = h5("Create",style="font-weight: bold; color: yellow"),width='100%',style="background-color: darkgreen"))
            )  
          )
        ),
        conditionalPanel(
          condition = "input.selected_file_operation == 'Browse/Edit'",
          box (
            title = span(icon("user-edit","fa-x"),strong("Browse/Edit Dataset")), status = "primary", width = 12,solidHeader = TRUE,collapsible=FALSE,  
            fluidRow(
              column(2,selectInput("selected_be_option", h4(strong("Operation"), style="font-style: bold; color: darkblue"), choices=c("Browse","Edit"),selected = "Browse")),
              column(6,textInput("accept_be_dsname", h4(strong("Dataset Name"), style="font-style: bold; color: darkblue"),value = "")),
              column(2, actionButton("search_be_ds", label = h5("Search",style="font-weight: bold; color: yellow"),width='100%',style="background-color: darkblue;margin-top: 30px;"))
            ),
            br(),
            br(),
            fluidRow(
              column(4,h4(strong(textOutput("display_be_file_name"))),style="font-style: bold; color: darkblue"),
              conditionalPanel(
                condition = "(input.selected_be_option == 'Edit') && (output.display_be_file_name != '')",
                column(1, offset=2,actionButton("search_be_save", label = h5("Save",style="font-weight: bold; color: yellow"),width='100%',style="background-color: darkgreen;")),
                column(1, actionButton("search_be_sub", label = h5("Submit",style="font-weight: bold; color: yellow"),width='100%',style="background-color: darkred;"))
              )  
            ),
            fluidRow(
              column(8,uiOutput("display_be_file"))
            )
          )
        ),
        conditionalPanel(
          condition = "input.selected_file_operation == 'Rename/Copy'",
          box (
            title = span(icon("copy","fa-x"),strong("Rename/Copy Dataset")), status = "primary", width = 12,solidHeader = TRUE,collapsible=FALSE,  
            fluidRow(
              column(2,selectInput("selected_mc_option", h4(strong("Operation"), style="font-style: bold; color: darkblue"), choices=c("Rename","Copy"),selected = "Copy")),
              column(2,selectInput("selected_mc_type", h4(strong("Operand"), style="font-style: bold; color: darkblue"), choices=c("Dataset","Dataset-Member"),selected = "Dataset-Member"))
            ),
            fluidRow(
              column(3,textInput("accept_mc_source_dsname", h4(strong("From Dataset"), style="font-style: bold; color: darkblue"),value = "")),
              conditionalPanel(
                condition = "input.selected_mc_type == 'Dataset-Member'",
                column(2,uiOutput("select_mc_source_member"))
              ),
              conditionalPanel(
                condition = "!((input.selected_mc_option == 'Rename') && (input.selected_mc_type == 'Dataset-Member'))",
                column(3,textInput("accept_mc_target_dsname", h4(strong("To Dataset"), style="font-style: bold; color: darkblue"),value = ""))
              ),
              conditionalPanel(
                condition = "input.selected_mc_type == 'Dataset-Member'",
                column(2,uiOutput("select_mc_target_member"))
              ),
              conditionalPanel(
                condition = "input.selected_mc_option == 'Rename'",
                column(1, actionButton("rename_mc", label = h5("Rename",style="font-weight: bold; color: yellow"),width='100%',style="background-color: darkblue;margin-top: 30px;"))
              ),
              conditionalPanel(
                condition = "input.selected_mc_option == 'Copy'",
                column(1, actionButton("copy_mc", label = h5("Copy",style="font-weight: bold; color: yellow"),width='100%',style="background-color: darkgreen;margin-top: 30px;"))
              )
            )
          )
        ),
        conditionalPanel(
          condition = "input.selected_file_operation == 'Delete'",
          box (
            title = span(icon("ban","fa-x"),strong("Delete Dataset")), status = "primary", width = 12,solidHeader = TRUE,collapsible=FALSE,  
            fluidRow(
              column(2,selectInput("selected_d_type", h4(strong("Operand"), style="font-style: bold; color: darkblue"), choices=c("Dataset","Dataset-Member"),selected = "Dataset-Member")),
              column(3,textInput("accept_d_source_dsname", h4(strong("From Dataset"), style="font-style: bold; color: darkblue"),value = "")),
              conditionalPanel(
                condition = "input.selected_d_type == 'Dataset-Member'",
                column(2,uiOutput("select_d_source_member"))
              ),
              column(1, actionButton("delete_d", label = h5("Delete",style="font-weight: bold; color: yellow"),width='100%',style="background-color: darkred;margin-top: 30px;"))
            )
          )
        )
      ),
      tabItem(tabName = "dashboard4",
        box (
          title = span(icon("elementor","fa-x"),strong("Data Management Panel")), status = "primary", width = 12,solidHeader = TRUE,collapsible=FALSE,  
          fluidRow(
            column(3,selectInput("selected_data_operation", h4(strong("Select Operation"), style="font-style: bold; color: darkblue"), choices=c("Insert","Browse","Edit","Delete"),selected = "Browse")),
            column(3,selectInput("selected_data_source", h4(strong("Select Data Source"), style="font-style: bold; color: darkblue"), choices=c("File","DB2","IMS"),selected = "File"))
          )
        ),
        conditionalPanel(
          condition = "input.selected_data_operation == 'Browse' && input.selected_data_source == 'File'",
          box (
            title = span(icon("table","fa-x"),strong("File Data Management")), status = "primary", width = 12,solidHeader = TRUE,collapsible=FALSE,
            fluidRow(
              column(3,textInput("accept_filedata_source_dsname", h4(strong("Source File"), style="font-style: bold; color: darkblue"),value = "")),
              column(3,textInput("accept_filedata_layout_dsname", h4(strong("Layout Dataset"), style="font-style: bold; color: darkblue"),value = "")),
              column(2,uiOutput("select_filedata_layout_member")),
              column(2,selectInput("selected_language", h4(strong("Language"), style="font-style: bold; color: darkblue"), choices=c("COBOL","PLI","HLASM","Auto"),selected = "COBOL"))
            ),
            fluidRow(
              column(2,numericInput("accept_num_of_records", h4(strong("No of Records"), style="font-style: bold; color: darkblue"),value = 1)),
              column(2,numericInput("accept_offset", h4(strong("Offset"), style="font-style: bold; color: darkblue"),value = 0)),
              column(3,textInput("accept_key_value", h4(strong("Key Value"), style="font-style: bold; color: darkblue"),value = "")),
              column(1, actionButton("browseData", label = h5("Browse",style="font-weight: bold; color: yellow"),width='100%',style="background-color: darkgreen;margin-top: 30px;"))
            ),
            fluidRow(
              column(3,uiOutput("select_filedata_layout"))
            ),
            fluidRow(
              column(12,h6(DT::dataTableOutput("ListFormatedData")))
            )
          )
        ),
        conditionalPanel(
          condition = "input.selected_data_operation == 'Browse' && input.selected_data_source == 'DB2'",
          box (
            title = span(icon("database","fa-x"),strong("DB2 Data Management")), status = "primary", width = 12,solidHeader = TRUE,collapsible=FALSE,
            fluidRow(
              column(2,textInput("accept_db2_ssid", h4(strong("DB2 Subsystem"), style="font-style: bold; color: darkblue"),value = "DBCG")),
              column(2,textInput("accept_db2_owner", h4(strong("DB Owner"), style="font-style: bold; color: darkblue"),value = "")),
              column(3,textInput("accept_db2_tablename", h4(strong("DB2 Table"), style="font-style: bold; color: darkblue"),value = ""))
            ),
            fluidRow(
              column(2,numericInput("accept_db2_num_of_records", h4(strong("No of Records"), style="font-style: bold; color: darkblue"),value = 1)),
              column(2,numericInput("accept_db2_offset", h4(strong("Offset"), style="font-style: bold; color: darkblue"),value = 0)),
              column(4,textInput("accept_db2_where", h4(strong("Where clause"), style="font-style: bold; color: darkblue"),value = "")),
              column(1, actionButton("browseDB2Data", label = h5("Browse",style="font-weight: bold; color: yellow"),width='100%',style="background-color: darkgreen;margin-top: 30px;"))
            ),
            fluidRow(
              column(12,h6(DT::dataTableOutput("ListFormatedDB2Data")))
            )
          )
        ),
        conditionalPanel(
          condition = "input.selected_data_operation != 'Browse' || input.selected_data_source == 'IMS'",
          box (
            title = span(icon("times-circle","fa-x"),strong("Yet to be implemented")), status = "danger", width = 12,solidHeader = TRUE,collapsible=FALSE,
            fluidRow(
              column(12,h1(strong(textOutput("display_incomplete")), style="font-weight: bold; color: darkred"))
            )
          )
        )
      )
    )
  )
)


options(shiny.maxRequestSize = 9*1024^2)

server <- shinyServer(function(input, output, session) {

  baseurl <- "http://192.86.33.143:9080/"
  #zoweurl = 
  store_runsql <- 0
  resubmitListcount <<- 0
  filefunctionscount <<- 0
  hidefilefunctionscount <<- 0
  browseDatacount <<- 0 
  browseDB2Datacount <<- 0 
  
  dx4 <- data.frame()
  
  show_welcome <- reactiveVal(TRUE)
  accept_credentials <- reactiveVal(FALSE)
  #baseurlzoweejobs <- "https://192.86.33.143:10443/zosmf/restjobs/"
  #baseurlzoweefiles <- "https://192.86.33.143:10443/zosmf/restfiles/"
  
  #s0w1.dal-ebis.ihost.com
  baseurlzoweejobs <- "https://s0w1.dal-ebis.ihost.com:10443/zosmf/restjobs/"
  baseurlzoweefiles <- "https://s0w1.dal-ebis.ihost.com:10443/zosmf/restfiles/"
  
  reactive_prefix <- reactiveVal("")
  reactive_be_ds <- reactiveVal("")
  reactive_be_mem <- reactiveVal("")
  reactive_fm_table <- reactiveVal(0)
  #uid <- "ASHISSA"
  #pwd <- "A9SHISAH"
  uid <-NULL
  pwd <- NULL

  output$ListJobs = DT::renderDataTable({

    # if (show_welcome()) {
    #   shinyalert("Welcome", "This containerized application is built with zosmf APIs for Mainframe Job Monitoring", type = "success", confirmButtonCol = "#3F27B3")
    #   show_welcome(FALSE)
    # }
    
    input$refreshList
    input$purgeList
    #event_register(jstat,"plotly_click")
    
    if (show_welcome()) {
      showModal(modalDialog(
        title = HTML('<left><font size="5"><font color="darkblue"><b><body style="background-color:powderblue;">Sign in</body></b></font></font></left>'),
        size ="s",
        textInput("input_uid", "User ID",value=""),
        passwordInput("input_pwd", "Password",value=""),
        easyClose = FALSE,
        footer = tagList(
          actionButton("ok_login", label = "Login",width='30%',style="font-size: large; font-weight: bold; color: white; background-color: darkblue")
        )
      ))
      show_welcome(FALSE)
    }
    #input$input_uid
    if (accept_credentials()) {
      httr::set_config(config(ssl_verifypeer = FALSE))
      joburl <- paste(baseurlzoweejobs,"jobs?",sep="")
      # if (input$input_owner != "") {
      #   joburl <- paste(joburl,"owner=",input$input_owner,sep="")
      #   if (input$input_prefix != "") {
      #     joburl <- paste(joburl,"&prefix=",input$input_prefix,sep="")
      #   }
      # }
      # else {
      #   if (input$input_prefix != "") {
      #     joburl <- paste(joburl,"prefix=",input$input_prefix,sep="")
      #   }
      # }

      if (input$input_owner != "") {
        joburl <- paste(joburl,"owner=",input$input_owner,sep="")
        jobowner <- input$input_owner
      }
      else {
        joburl <- paste(joburl,"owner=",uid,sep="")
        jobowner <- uid
      }
      if (input$input_prefix != "") {
          joburl <- paste(joburl,"&prefix=",input$input_prefix,sep="")
      }

      myjob <- GET(url = joburl, add_headers("X-CSRF-ZOSMF-HEADER" = "ANY"),authenticate(uid,pwd,type="basic"))
      
      if (myjob$status_code != 200) {
        shinyalert("Error", "Incorrect Credential or Server Issue", type = "error",confirmButtonCol = "#E74C3C")
        stop("Incorrect Credential or Server Issue")
      }
      
      #if (is.null(content(myjob)[[1]]$retcode)) {
      if (length(content(myjob)) == 0) {
        ErrorMsg <- paste("No Jobs found with Owner ",toupper(jobowner),sep="")
        if (input$input_prefix != "") {
          ErrorMsg <- paste(ErrorMsg," and Prefix ",toupper(input$input_prefix),sep="")
        }
        shinyalert("Error", ErrorMsg, type = "error",confirmButtonCol = "#E74C3C")
        stop(ErrorMsg)
      } 
      else {
        if (is.null(content(myjob)[[1]]$retcode)) {
          myjoblist <- as.data.frame(content(myjob)[[1]][1:12],stringsAsFactors = FALSE)
          myjoblist$retcode <- ""
        }
        else {
          myjoblist <- as.data.frame(content(myjob)[[1]],stringsAsFactors = FALSE)
        }
      }
      
      if (length(content(myjob)) > 1) {
        for (i in 2:length(content(myjob))) {
          if (is.null(content(myjob)[[i]]$retcode)) {
            x <- as.data.frame(content(myjob)[[i]][1:12],stringsAsFactors = FALSE)
            x$retcode <- ""
            myjoblist <- rbind(myjoblist,x)
          }
          else {
            myjoblist <- rbind(myjoblist,as.data.frame(content(myjob)[[i]],stringsAsFactors = FALSE))
          }
        }
      }
    }  

    DT::datatable(
      myjoblist[,c(1:3,6,8,9,11:13)], class = 'cell-border stripe', rownames = FALSE, selection = "single",
      #caption = h4('Selected Item',style = 'caption-side: top; color:darkgreen ; font-Weight:bold'),
      extensions = list(FixedColumns = list(leftColumns = 1)),
      callback=DT::JS("table.on('click.dt', 'tr', function() {
                      var data=table.row(this).data();
                      Shiny.onInputChange('selected_job_id',data[4]);
                      Shiny.onInputChange('selected_job_name',data[6]);
                      Shiny.onInputChange('selected_type',data[4]);
  });"), 
      options = list (dom = 'tp',
                      columnDefs = list(list(className = 'dt-center', targets = c(0:8))), pageLength = 5,
                      headerCallback = JS(headerCallback5)
    )
  )%>%
      formatStyle(columns = c(1), fontSize = '120%', color = 'white', backgroundColor = 'darkblue') %>%
      formatStyle(columns = c(2), fontSize = '120%', color = 'darkblue') %>%
      formatStyle(columns = c(3), fontSize = '120%', color = 'darkblue') %>%
      formatStyle(columns = c(4), fontSize = '120%', color = 'darkblue') %>%
      formatStyle(columns = c(5), fontSize = '120%', color = 'white', backgroundColor = 'darkblue') %>%
      formatStyle(columns = c(6), fontSize = '120%', color = 'darkblue') %>%
      formatStyle(columns = c(7), fontSize = '120%', color = 'white', backgroundColor = 'darkblue') %>%
      formatStyle(columns = c(8), fontSize = '120%', color = 'white', backgroundColor = 'darkblue') 
})
  
  headerCallback5 <- c(
    "function(thead, data, start, end, display){",
    "  $('th', thead).css('border-top', '2px solid darkblue');",
    "  $('th', thead).css('border-bottom', '2px solid darkblue');",
    "  $('th', thead).css('color', 'darkblue');",
    "}"
  )
  
  
  output$job_select_message <- renderText({
    if (is.null(input$selected_job_name)) {
      return("")
    } else {
      if (input$selected_job_name != "" && input$selected_job_id != "") {
        return(".")
      } else {
        return("")
      }
    }
  })
  
  observeEvent(input$ok_login, {
    uid <<- input$input_uid
    pwd <<- input$input_pwd
    accept_credentials(TRUE)
    removeModal()
  })
  
  output$joboutput_select_message <- renderText({
    if (is.null(input$selected_file_id)) {
      return("")
    } else {
      if (input$selected_file_id != "" ) {
        return(".")
      } else {
        return("")
      }
    }
  })  
  
  output$display_jobname <- renderText({
    if (is.null(input$selected_job_name)) {
      return("")
    } else {
      return(input$selected_job_name)
    }
  })  
  
  output$display_jobid <- renderText({
    if (is.null(input$selected_job_id)) {
      return("")
    } else {
      return(input$selected_job_id)
    }
  })  
  
  output$ListJobOutput = DT::renderDataTable({
    input$refreshList
    if (!is.null(input$selected_job_id)) {
      joburl <- paste(baseurlzoweejobs,"jobs/",input$selected_job_name,"/",input$selected_job_id,"/files",sep="")
      myjob <- GET(url = joburl, add_headers("X-CSRF-ZOSMF-HEADER" = "ANY"), authenticate(uid,pwd,type="basic"))
      myjoblist <- as.data.frame(content(myjob)[[1]][1:12],stringsAsFactors = FALSE)
      if (is.null(content(myjob)[[1]]$procstep)) {
        myjoblist$procstep <- ""
      } else {
        myjoblist$procstep <- content(myjob)[[1]]$procstep
      }
      for (i in 2:length(content(myjob))) {
        x <- as.data.frame(content(myjob)[[i]][1:12],stringsAsFactors = FALSE)
        if (is.null(content(myjob)[[i]]$procstep)) {
          x$procstep <- ""
        }
        else {
          x$procstep <- content(myjob)[[i]]$procstep
        }
        myjoblist <- rbind(myjoblist,x)
      }
    }
    DT::datatable(
      myjoblist[,c(10,13,3,9,12,11,6,1,7)], class = 'cell-border stripe', rownames = FALSE, selection = "single",
      #caption = h4('Selected Item',style = 'caption-side: top; color:darkgreen ; font-Weight:bold'),
      extensions = list(FixedColumns = list(leftColumns = 1)),
      callback=DT::JS("table.on('click.dt', 'tr', function() {
                      var data=table.row(this).data();
                      Shiny.onInputChange('selected_file_id',data[0]);
                      });"), 
      options = list (dom = 'tp',
                      columnDefs = list(list(className = 'dt-center', targets = c(0:8))), pageLength = 5,
                      headerCallback = JS(headerCallback6)
      )
    )%>%
      formatStyle(columns = c(1), fontSize = '120%', color = 'white', backgroundColor = 'darkblue') %>%
      formatStyle(columns = c(2), fontSize = '120%', color = 'darkblue') %>%
      formatStyle(columns = c(3), fontSize = '120%', color = 'darkblue') %>%
      formatStyle(columns = c(4), fontSize = '120%', color = 'white', backgroundColor = 'darkblue') %>%
      formatStyle(columns = c(5), fontSize = '120%', color = 'darkblue') %>%
      formatStyle(columns = c(6), fontSize = '120%', color = 'white', backgroundColor = 'darkblue')
  })
  
  headerCallback6 <- c(
    "function(thead, data, start, end, display){",
    "  $('th', thead).css('border-top', '2px solid darkblue');",
    "  $('th', thead).css('border-bottom', '2px solid darkblue');",
    "  $('th', thead).css('color', 'darkblue');",
    "}"
  )
  
  # output$display_listing <- renderText({
  #   joburl <- paste(baseurlzoweejobs,"jobs/",input$selected_job_name,"/",input$selected_job_id,"/files/",input$selected_file_id,"/records",sep="")
  #   
  #   print(joburl)
  #   
  #   myjob <- GET(url = joburl, add_headers("X-CSRF-ZOSMF-HEADER" = "ANY"), authenticate(uid,pwd,type="basic"))
  #   
  #   return(content(myjob))
  #   
  # })
  # 
  output$input_prefix_set <- renderUI({
    textInput("input_prefix", label=NULL,value = reactive_prefix())
  })
  
  output$update_jcl <- renderUI({

    if (input$resubmitList > resubmitListcount) {
      resubmitListcount <<- input$resubmitList
      joburl <- paste(baseurlzoweejobs,"jobs/",input$selected_job_name,"/",input$selected_job_id,"/files/JCL/records?mode=text",sep="")
      myjob <- GET(url = joburl, add_headers("X-CSRF-ZOSMF-HEADER" = "ANY"), authenticate(uid,pwd,type="basic"))
      jobcontent <- (content(myjob))
      jobcontent <- gsub(paste(" ",input$selected_job_id,sep=""),"",jobcontent)
      textAreaInput("accept_jcl", h4(strong(paste("JCL",input$selected_job_id,sep=" ")), width='100%',height='600',resize="vertical",style="font-style: bold; color: darkblue"),value = jobcontent)
    }

  })
  

  output$display_listing_table = DT::renderDataTable({
    input$refreshList
    if (!is.null(input$selected_job_id)) {
      joburl <- paste(baseurlzoweejobs,"jobs/",input$selected_job_name,"/",input$selected_job_id,"/files/",input$selected_file_id,"/records",sep="")
      myjob <- GET(url = joburl, add_headers("X-CSRF-ZOSMF-HEADER" = "ANY"), authenticate(uid,pwd,type="basic"))
      myjoblist <- strsplit(content(myjob), "\n")
      #myjoblist <- gsub(' ', '&nbsp', myjoblist)
      #myjoblist <- paste('<font face = "Courier New" size = "4">',myjoblist,'</font>',paste="")
      myjoblist <- as.data.frame(myjoblist,stringsAsFactors = FALSE)
    }
    
    DT::datatable(
      myjoblist, escape = FALSE,rownames = FALSE,colnames=NULL,selection = "single",class="compact",
      callback=DT::JS("table.on('click.dt', 'tr', function() {
                var data=table.row(this).data();
                Shiny.onInputChange('codeline',data[0]);
      });"),
      options = list(pageLength=nrow(myjoblist),fixedHeader = FALSE, paging = FALSE,searching = FALSE)
    )
  })
  
  
  output$display_file <- renderText({
    if (is.null(input$codeline)) {
      return("")
    } else {
      mycodeline <- gsub(","," ",input$codeline)
      mycodeline <- gsub("="," ",mycodeline)
      mycodeline <- gsub("'"," ",mycodeline)
      mycodeline <- gsub("\""," ",mycodeline)
      x <- unlist(strsplit(mycodeline, " "))
      y <<- x[grep('\\.', x)]
      z <- x[grep('\\/', x)]
      if (length(z) != 0) {
        z <- gsub("\\(","",z)
        z <- gsub("\\)","",z)
        z <- substr(z[1],unlist(lapply(gregexpr(pattern = '\\/', z), min))+1,nchar(z[1]))
        ussfileurl <- paste(baseurlzoweefiles,"fs/",gsub("/","%2F",z),sep="")
        mydata <- GET(url = ussfileurl, add_headers("X-CSRF-ZOSMF-HEADER" = "ANY"),authenticate(uid,pwd,type="basic"))
        if (mydata$status_code == 200) {
          showdata <- strsplit(content(mydata), "\n")
          showdata <- as.data.frame(showdata,stringsAsFactors = FALSE)
          names(showdata) <- c("Contents")
          for (i in 1:nrow(showdata)) {
            showdata[i,] <- gsub(' ', '&nbsp', showdata[i,])
            showdata[i,] <- paste('<font face = "Courier New" size = "2">',showdata[i,],'</font>',paste="")
          }
          showModal(modalDialog(
            title = h4(paste("File ",z,sep="")),
            size ="l",
            DT::renderDataTable({
              DT::datatable(
                showdata, escape = FALSE,rownames = FALSE,colnames=NULL,class="compact",
                options = list(pageLength=15,fixedHeader = FALSE, searching = FALSE)
              )
            }),  
            easyClose = FALSE,
            footer = modalButton("Dismiss")
          ))
        }
      }
      if (length(y) != 0) {
        pdsfileurl <- paste(baseurlzoweefiles,"ds/",y,"/member",sep="")
        mydata <- GET(url = pdsfileurl, add_headers("X-CSRF-ZOSMF-HEADER" = "ANY"), authenticate(uid,pwd,type="basic"))
        if (mydata$status_code == 200) {
          memberlist <- as.data.frame(unlist(content(mydata)$items),stringsAsFactors = FALSE)
          names(memberlist) <- "PDS_Member"
          findmember <- intersect(x,memberlist$PDS_Member)
          if (length(findmember)==0) {
            showModal(modalDialog(
              title = h4(paste("PDS ",y," members",sep="")),
              size ="l",
              DT::renderDataTable({
                DT::datatable(
                  memberlist, escape = FALSE,rownames = FALSE,colnames=NULL,selection = "single",class="compact",
                  callback=DT::JS("table.on('click.dt', 'tr', function() {
                                  var data=table.row(this).data();
                                  Shiny.onInputChange('memberselected',data[0]);
                  });"),
                  options = list(
                    pageLength=15,fixedHeader = FALSE, searching = FALSE)
                )
              }),  
              easyClose = FALSE,
              footer = tagList(
                actionButton("ok_cd", "Select"),
                modalButton("Dismiss")
              )
            ))
          } else {
            psfileurl <- paste(baseurlzoweefiles,"ds/",y,"(",findmember[1],")",sep="")
            mydata <- GET(url = psfileurl, add_headers("X-CSRF-ZOSMF-HEADER" = "ANY"), authenticate(uid,pwd,type="basic"))
            if (mydata$status_code == 200) {
              showdata <- strsplit(content(mydata), "\n")
              showdata <- as.data.frame(showdata,stringsAsFactors = FALSE)
              names(showdata) <- c("Contents")
              for (i in 1:nrow(showdata)) {
                showdata[i,] <- gsub(' ', '&nbsp', showdata[i,])
                showdata[i,] <- paste('<font face = "Courier New" size = "2">',showdata[i,],'</font>',paste="")
              }
              showModal(modalDialog(
                title = h4(paste("File ",y,"(",findmember[1],")",sep="")),
                size ="l",
                DT::renderDataTable({
                  DT::datatable(
                    showdata, escape = FALSE,rownames = FALSE,colnames=NULL,class="compact",
                    options = list(pageLength=15,fixedHeader = FALSE, searching = FALSE)
                  )
                }),  
                easyClose = FALSE,
                footer = modalButton("Dismiss")
              ))
            }
            
          }
        } else {
          psfileurl <- paste(baseurlzoweefiles,"ds/",y,sep="")
          mydata <- GET(url = psfileurl, add_headers("X-CSRF-ZOSMF-HEADER" = "ANY"), authenticate(uid,pwd,type="basic"))
          if (mydata$status_code == 200) {
            showdata <- strsplit(content(mydata), "\n")
            showdata <- as.data.frame(showdata,stringsAsFactors = FALSE)
            names(showdata) <- c("Contents")
            for (i in 1:nrow(showdata)) {
              showdata[i,] <- gsub(' ', '&nbsp', showdata[i,])
              showdata[i,] <- paste('<font face = "Courier New" size = "2">',showdata[i,],'</font>',paste="")
            }
            showModal(modalDialog(
              title = h4(paste("File ",y,sep="")),
              size ="l",
              DT::renderDataTable({
                DT::datatable(
                  showdata, escape = FALSE,rownames = FALSE,colnames=NULL,class="compact",
                  options = list(pageLength=15,fixedHeader = FALSE, searching = FALSE)
                )
              }),  
              easyClose = FALSE,
              footer = modalButton("Dismiss")
            ))
          }
        }
      }
      return("")
    }
  })
  
  observeEvent(input$ok_cd, {
    psfileurl <- paste(baseurlzoweefiles,"ds/",y,"(",input$memberselected,")",sep="")
    mydata <- GET(url = psfileurl, add_headers("X-CSRF-ZOSMF-HEADER" = "ANY"), authenticate(uid,pwd,type="basic"))
    
    if (mydata$status_code == 200) {
      showdata <- strsplit(content(mydata), "\n")
      showdata <- as.data.frame(showdata,stringsAsFactors = FALSE)
      names(showdata) <- c("Contents")
      for (i in 1:nrow(showdata)) {
        showdata[i,] <- gsub(' ', '&nbsp', showdata[i,])
        showdata[i,] <- paste('<font face = "Courier New" size = "2">',showdata[i,],'</font>',paste="")
      }
      showModal(modalDialog(
        title = h4(paste("File ",y,"(",input$memberselected,")",sep="")),
        size ="l",
        DT::renderDataTable({
          DT::datatable(
            showdata, escape = FALSE,rownames = FALSE,colnames=NULL,class="compact",
            options = list(pageLength=15,fixedHeader = FALSE, searching = FALSE)
          )
        }),  
        #renderTable(showdata),
        easyClose = FALSE,
        footer = modalButton("Dismiss")
      ))
    }
    
  })

  
  output$CPU_usage_graph <- renderPlotly({
    input$refreshList
    if(!is.null(input$selected_job_id)) {
      joburl <- paste(baseurlzoweejobs,"jobs/",input$selected_job_name,"/",input$selected_job_id,"/files/4/records",sep="")
      myjob <- GET(url = joburl, add_headers("X-CSRF-ZOSMF-HEADER" = "ANY"), authenticate(uid,pwd,type="basic"))
      myjob <- GET(url = joburl, add_headers("X-CSRF-ZOSMF-HEADER" = "ANY"), authenticate(uid,pwd,type="basic"))
      if (myjob$status_code != 200) {
        #plot_ly(type = 'table',header = list(values=c("Error")),cells = list(values=c("No Job Stats")))
        #NoDataToShow <- as.data.frame("No Job Stats")
        #names(NoDataToShow) <- c(" ")
        #gvisTable(NoDataToShow)
      }
      else {
        JESYSMSG <- as.data.frame(unlist(strsplit(content(myjob), "\n")),stringsAsFactors = FALSE)
        names(JESYSMSG) <- c("logtext")
        saveJESYSMSG <- JESYSMSG
        logdata <- dplyr::filter(JESYSMSG, grepl('/START |/STOP | CPU: ', logtext))
        if (nrow(logdata) >= 3) {
          statdata <<- data.frame(StepName=as.character(),StartTime=as.character(),EndTime=as.character(),CPUTime=as.numeric(),GraphColor.style=as.character(),stringsAsFactors=FALSE,row.names=NULL)
          k <- 1
          for (i in 1:nrow(logdata)) {
            x <- logdata[i,]
            x1 <- gsub("/START "," ",x)
            if (x != x1) {
              y1 <- unlist(strsplit(x1," "))
              y1 <- y1[y1!=""]
              statdata[k,]$StepName <<- y1[2]
              statdata[k,]$StartTime <<- y1[3]
            }
            x1 <- gsub("/STOP "," ",x)
            if (x != x1) {
              y1 <- unlist(strsplit(x1," "))
              y1 <- y1[y1!=""]
              statdata[k,]$EndTime <<- y1[3]
            }
            x1 <- gsub("CPU: "," ",x)
            if (x != x1) {
              y1 <- unlist(strsplit(x1," "))
              y1 <- y1[y1!=""]
              statdata[k,]$CPUTime <<- as.numeric(y1[1])*3600 + as.numeric(y1[3])*60 + as.numeric(y1[5])
              k <- k + 1
            }
          }
          row.names(statdata) <<- NULL
          statdata$GraphColor.style <<- "darkblue"
          statdata[k-1,]$GraphColor.style <<- "darkgreen"
          jscode1 <<- "var sel = chart.getSelection();
          var row = sel[0].row;
          var selectnamedata = data.getValue(row,0);
          Shiny.onInputChange('selected_step',selectnamedata);"
          mystatdata <- statdata[,c(1,4,5)]
          jstat <-  plot_ly(
            x = factor(statdata$StepName,levels = statdata$StepName),
            y = statdata$CPUTime,
            name = "Job Stat",
            type = "bar",
            marker = list(color = statdata$GraphColor.style),
            source = "jobstat",
            height = "300"
          ) %>% layout(plot_bgcolor  = "rgba(0, 0, 0, 0)",
                       paper_bgcolor = "rgba(0, 0, 0, 0)",
                       xaxis = list(zerolinecolor = "black"),
                       yaxis = list(zeroline = TRUE, showline = TRUE,gridwidth = 0.25,dash="dashdot",gridcolor = "grey",title="CPU in sec")
          ) %>% onRender("
                function(el, x) {
                  Plotly.d3.select('.cursor-crosshair').style('cursor', 'default')
                }
          ")
          event_register(jstat,"plotly_click")
          jstat
        }
        else {
          #plot_ly(type = 'table',header = list(values=c("Error")),cells = list(values=c("No Job Stats")))
          #NoDataToShow <- as.data.frame("No Job Stats")
          #names(NoDataToShow) <- c(" ")
          #gvisTable(NoDataToShow)
        }
      }
    }
    
  })
  
  
  
  
  output$JobStat <- renderText({
    input$refreshList
    if(!is.null(input$selected_job_id)) {
      joburl <- paste(baseurlzoweejobs,"jobs/",input$selected_job_name,"/",input$selected_job_id,"/files/4/records",sep="")
      myjob <- GET(url = joburl, add_headers("X-CSRF-ZOSMF-HEADER" = "ANY"), authenticate(uid,pwd,type="basic"))
      if (myjob$status_code != 200) {
        return(paste(input$selected_job_name,"(",input$selected_job_id,")","Message Log missing",sep=""))
      }
      else {
        JESYSMSG <- as.data.frame(unlist(strsplit(content(myjob), "\n")),stringsAsFactors = FALSE)
        names(JESYSMSG) <- c("logtext")
        saveJESYSMSG <- JESYSMSG
        logdata0 <- dplyr::filter(JESYSMSG, grepl('JOB/', logtext))
        logdata1 <- dplyr::filter(logdata0, grepl('/START ', logtext))
        logdata1[1,] <- gsub("/START "," ",logdata1[1,])
        y <- unlist(strsplit(logdata1[1,]," "))
        y <- y[y!=""]
        StartData <- as.POSIXct(strptime(y[3], format="%Y%j.%H%M"), tz="America/Chicago") 
        logdata1 <- dplyr::filter(logdata0, grepl('/STOP ', logtext))
        logdata1[1,] <- gsub("/STOP "," ",logdata1[1,])
        y <- unlist(strsplit(logdata1[1,]," "))
        y <- y[y!=""]
        EndData <- as.POSIXct(strptime(y[3], format="%Y%j.%H%M"), tz="America/Chicago") 
        if (is.na(StartData)) {
          return(paste(input$selected_job_name,"(",input$selected_job_id,")"," Active / Did not complete",sep=""))
        }
        else {
          return(paste(input$selected_job_name,"(",input$selected_job_id,")"," Start ",StartData," End ",EndData,sep=""))
        }
      }
      
    }
  })

  observeEvent(event_data("plotly_click",source = "jobstat"), {
    selected_step <- event_data("plotly_click",source = "jobstat")$x
    if (selected_step != "") {
      Type <- substr(selected_step,1,unlist(gregexpr(pattern ='/',selected_step))[1]-1)
      shinyalert("Info", paste(Type," Name  ",selected_step,
                               "\nStart Time ",statdata[statdata$StepName==selected_step,]$StartTime,
                               "\nEnd Time   ",statdata[statdata$StepName==selected_step,]$EndTime,
                               "\nCPU Sec    ",statdata[statdata$StepName==selected_step,]$CPUTime,sep=""
                                ), type = "info",confirmButtonCol = "#3F27B3")
    }
  })
  
  observeEvent(input$resubmitList, {
    if (!is.null(input$selected_job_id)) {
      updateTabItems(session, "tabs", "dashboard2")
    }
    else {
      shinyalert("Error", "Select a Job to proceed",type = "error",confirmButtonCol = "#E74C3C")
    }
  })
    
  observeEvent(input$submitjob, {
    joburl <- paste(baseurlzoweejobs,"jobs?Content-Type=text/plain",sep="")
    myjob <- PUT(url = joburl, add_headers(.headers = c('X-CSRF-ZOSMF-HEADER'= 'ANY','Content-Type' = 'text/plain')), body=input$accept_jcl,authenticate(uid,pwd,type="basic"))
    myresult <- (content(myjob))
    if (myjob$status_code == 201) {
      shinyalert("Success", 
                 paste("Job ", myresult$jobname, " (",myresult$jobid,") submitted", sep=""), 
                 type = "success",confirmButtonCol = "#54BA60")
      reactive_prefix(myresult$jobname)
      updateTabItems(session, "tabs", "dashboard1")
    } else {
      shinyalert("Error", 
                 paste("Job Submission failed with status ", myjob$status_code, sep=""), 
                 type = "error",confirmButtonCol = "#E74C3C")
    }
  })
  
  observeEvent(input$purgeList, {

    if (is.null(input$selected_job_id)) {
      shinyalert("Error", "Select a Job to proceed",type = "error",confirmButtonCol = "#E74C3C")
    } 
    else {
      joburl <- paste(baseurlzoweejobs,"jobs/",input$selected_job_name,"/",input$selected_job_id,sep="")
      myjob <- DELETE(url = joburl, add_headers("X-CSRF-ZOSMF-HEADER" = "ANY"), authenticate(uid,pwd,type="basic"))
      myresult <- (content(myjob))
      if (myjob$status_code == 200) {
        shinyalert("Success", paste("Job ", myresult$jobname, " (",myresult$jobid,") purged", sep=""), type = "success",confirmButtonCol = "#54BA60")
        updateTabItems(session, "tabs", "dashboard1")
      } else {
        shinyalert("Error", paste("Job Purge Request failed with status ", myjob$status_code, sep=""),type = "error",confirmButtonCol = "#E74C3C")
      }
    }
  })
  
  observeEvent(input$createds, {
    
    pc_json <- list(
      dsorg = input$selected_ds_type,
      alcunit = input$selected_allocation,
      primary =  input$selected_primary,
      secondary = input$selected_secondary,
      dirblk = ifelse(input$selected_ds_type == "PO",input$selected_dir_blk,0),
      avgblk = input$selected_avg_blk,
      recfm = input$selected_rec_format,
      blksize = input$selected_blksize,
      lrecl = input$selected_lrecl
    )
    
    createdsurl <- paste(baseurlzoweefiles,"ds/",input$accept_dsname,sep="")
    #myjob <- PUT(url = joburl, add_headers(.headers = c('X-CSRF-ZOSMF-HEADER'= 'ANY','Content-Type' = 'text/plain')), body=input$accept_jcl,authenticate(uid,pwd,type="basic"))
    myrequest <- POST(url = createdsurl, add_headers('X-CSRF-ZOSMF-HEADER' = 'ANY'), body = pc_json, encode = "json",authenticate(uid,pwd,type="basic"))
    myresult <- (content(myrequest))
    if (myrequest$status_code == 201) {
      shinyalert("Success", 
                 paste(ifelse(input$selected_ds_type == "PO","PDS ","PS "), input$accept_dsname," created", sep=""), 
                 type = "success",confirmButtonCol = "#54BA60")
    } else {
      shinyalert("Error", 
                 paste(ifelse(input$selected_ds_type == "PO","PDS ","PS ")," creation failed due to ", myresult$message, sep=""), 
                 type = "error",confirmButtonCol = "#E74C3C")
    }
  })
  
  observeEvent(input$search_be_ds, {
    searchdsurl <- paste(baseurlzoweefiles,"ds?dslevel=",input$accept_be_dsname,sep="")
    myrequest <- GET(url = searchdsurl, add_headers('X-CSRF-ZOSMF-HEADER' = 'ANY'), authenticate(uid,pwd,type="basic"))
    if (myrequest$status_code == 200) {
      dslist <- as.data.frame(unlist((content(myrequest))$items),stringsAsFactors = FALSE)
      if (nrow(dslist) == 0) {
        shinyalert("Error","Dataset Not Found",type = "error",confirmButtonCol = "#E74C3C")
        #stop("Dataset not found")
      } else {
        names(dslist) <- "DSN"
        if (nrow(dslist) == 1) {
          reactive_be_ds(dslist[1]$DSN)
          searchmemurl <- paste(baseurlzoweefiles,"ds/",dslist[1]$DSN,"/member",sep="")
          myrequestmem <- GET(url = searchmemurl, add_headers('X-CSRF-ZOSMF-HEADER' = 'ANY'), authenticate(uid,pwd,type="basic"))
          if (myrequestmem$status_code == 200) {
            memlist <- as.data.frame(unlist((content(myrequestmem))$items),stringsAsFactors = FALSE)
            if (nrow(memlist) >= 0) {
              showModal(modalDialog(
                #title = h4("Select Member"),
                title = h4(paste(dslist[1]$DSN," Select Member",sep="")),
                size ="l",
                DT::renderDataTable({
                  DT::datatable(
                    memlist, escape = FALSE,rownames = FALSE,colnames=NULL,selection = "single",class="compact",
                    callback=DT::JS("table.on('click.dt', 'tr', function() {
                                    var data=table.row(this).data();
                                    Shiny.onInputChange('mem1selected',data[0]);
                });"),
                options = list(
                  pageLength=15,fixedHeader = FALSE, searching = TRUE)
                )
            }),  
            easyClose = FALSE,
            footer = tagList(
              actionButton("ok_mem1", "Select"),
              actionButton("ok_new_mem1", "New"),
              modalButton("Dismiss")
            )
                ))
                }
            else {
              shinyalert("Error","No Member found",type = "error",confirmButtonCol = "#E74C3C")
            }
            }
          else {
            reactive_be_mem("")
          }
          }
        else {
          showModal(modalDialog(
            title = h4("Select Dataset"),
            size ="l",
            DT::renderDataTable({
              DT::datatable(
                dslist, escape = FALSE,rownames = FALSE,colnames=NULL,selection = "single",class="compact",
                callback=DT::JS("table.on('click.dt', 'tr', function() {
                                var data=table.row(this).data();
                                Shiny.onInputChange('dsselected',data[0]);
            });"),
              options = list(
                pageLength=15,fixedHeader = FALSE, searching = FALSE)
            )
        }),  
        easyClose = FALSE,
        footer = tagList(
          actionButton("ok_ds", "Select"),
          modalButton("Dismiss")
        )
        ))
        }
      }
    }
    else {
    shinyalert("Error","Dataset not found",type = "error",confirmButtonCol = "#E74C3C")
  }
  })

  observeEvent(input$ok_ds, {
    removeModal()
    reactive_be_ds(input$dsselected)
    searchmemurl <- paste(baseurlzoweefiles,"ds/",input$dsselected,"/member",sep="")
    myrequestmem <- GET(url = searchmemurl, add_headers('X-CSRF-ZOSMF-HEADER' = 'ANY'), authenticate(uid,pwd,type="basic"))
    if (myrequestmem$status_code == 200) {
      memlist <- as.data.frame(unlist((content(myrequestmem))$items),stringsAsFactors = FALSE)
      if (nrow(memlist) >= 0) {
        showModal(modalDialog(
          title = h4(paste(input$dsselected," Select Member",sep="")),
          size ="l",
          DT::renderDataTable({
            DT::datatable(
              memlist, escape = FALSE,rownames = FALSE,colnames=NULL,selection = "single",class="compact",
              callback=DT::JS("table.on('click.dt', 'tr', function() {
                              var data=table.row(this).data();
                              Shiny.onInputChange('mem2selected',data[0]);
              });"),
              options = list(
                pageLength=15,fixedHeader = FALSE, searching = TRUE)
              )
          }),  
          easyClose = FALSE,
          footer = tagList(
            actionButton("ok_mem2", "Select"),
            actionButton("ok_new_mem2", "New"),
            modalButton("Dismiss")
          )
        ))
      }
      else {
        shinyalert("Error","No Member found",type = "error",confirmButtonCol = "#E74C3C")
      }
      }
    else {
      reactive_be_mem("")
    }
    
  })
  
  observeEvent(input$ok_mem1, {
    removeModal()
    reactive_be_mem(input$mem1selected)
  })
  
  observeEvent(input$ok_new_mem1, {
    removeModal()
    shinyalert("Enter New Member Name", type = "input",inputType="text",confirmButtonCol = "#3F27B3",
      callbackR = mycallbacknewmem1
    )
  })
  
  observeEvent(input$ok_mem2, {
    removeModal()
    reactive_be_mem(input$mem2selected)
  })

  observeEvent(input$ok_new_mem2, {
    removeModal()
    shinyalert("Enter New Member Name", type = "input",inputType="text",confirmButtonCol = "#3F27B3",
               callbackR = mycallbacknewmem2
    )
  })
  
  mycallbacknewmem1 <- function(value) {
    # if (nchar(value) > 0 && nchar(value) < 9) {
    #   reactive_be_mem(value)
    # }
    # else {
    #   shinyalert("Error", "Invalid Member Name", type = "error",confirmButtonCol = "#E74C3C")
    # }
    reactive_be_mem(value)
  }
  
  mycallbacknewmem2 <- function(value) {
    # if (nchar(value) > 0 && nchar(value) < 9) {
    #   reactive_be_mem(value)
    # }
    # else {
    #   shinyalert("Error", "Invalid Member Name", type = "error",confirmButtonCol = "#E74C3C")
    # }
    reactive_be_mem(value)
  }
  
  
    
  output$display_be_file <- renderUI({  
    
    if (reactive_be_mem() == "") {
      dsn <- reactive_be_ds()
    }
    else {
      dsn <- paste(reactive_be_ds(),"(",reactive_be_mem(),")",sep="")
    }
    
    readdsurl <- paste(baseurlzoweefiles,"ds/",dsn,sep="")
    
    browsedsn <- GET(url = readdsurl, add_headers("X-CSRF-ZOSMF-HEADER" = "ANY"), authenticate(uid,pwd,type="basic"))
    if (browsedsn$status_code == 200) {
      textAreaInput("display_be_file_edit", label=NULL,value = content(browsedsn))
    } else {
      if (reactive_be_mem() != "") {
        textAreaInput("display_be_file_edit", label=NULL,value = "")
      } else {
        textOutput("")
      }
      
    }
  })

  output$display_be_file_name <- renderText({
    
    if (reactive_be_mem() == "") {
      dsn <- reactive_be_ds()
    }
    else {
      dsn <- paste(reactive_be_ds(),"(",reactive_be_mem(),")",sep="")
    }
    
    readdsurl <- paste(baseurlzoweefiles,"ds/",dsn,sep="")
    
    browsedsn <- GET(url = readdsurl, add_headers("X-CSRF-ZOSMF-HEADER" = "ANY"), authenticate(uid,pwd,type="basic"))
    if (browsedsn$status_code == 200 || reactive_be_mem() != "") {
      return(dsn)
    } else {
      return("")
    }
  })
  
  observeEvent(input$search_be_sub, {
    joburl <- paste(baseurlzoweejobs,"jobs?Content-Type=text/plain",sep="")
    myjob <- PUT(url = joburl, add_headers(.headers = c('X-CSRF-ZOSMF-HEADER'= 'ANY','Content-Type' = 'text/plain')), body=input$display_be_file_edit,authenticate(uid,pwd,type="basic"))
    myresult <- (content(myjob))
    if (myjob$status_code == 201) {
      shinyalert("Success", 
                 paste("Job ", myresult$jobname, " (",myresult$jobid,") submitted", sep=""), 
                 type = "success",confirmButtonCol = "#54BA60")
      reactive_prefix(myresult$jobname)
      updateTabItems(session, "tabs", "dashboard1")
    } else {
      shinyalert("Error", 
                 paste("Job Submission failed with status ", myjob$status_code, sep=""), 
                 type = "error",confirmButtonCol = "#E74C3C")
    }
  })
  
  observeEvent(input$search_be_save, {
    if (reactive_be_mem() == "") {
      dsn <- reactive_be_ds()
    }
    else {
      dsn <- paste(reactive_be_ds(),"(",reactive_be_mem(),")",sep="")
    }
    writedsurl <- paste(baseurlzoweefiles,"ds/",dsn,sep="")
    myjob <- PUT(url = writedsurl, add_headers(.headers = c('X-CSRF-ZOSMF-HEADER'= 'ANY','Content-Type' = 'text/plain')), body=input$display_be_file_edit,authenticate(uid,pwd,type="basic"))
    #myresult <- (content(myjob))
    if (myjob$status_code == 201 || myjob$status_code == 204) {
      shinyalert("Success", "Save Successful", type = "success",confirmButtonCol = "#54BA60")
    } else {
      shinyalert("Error", 
                 paste("Save failed with status ", myjob$status_code, sep=""), 
                 type = "error",confirmButtonCol = "#E74C3C")
    }
  })
  
  output$select_mc_source_member <- renderUI({  
    searchmemurl <- paste(baseurlzoweefiles,"ds/",input$accept_mc_source_dsname,"/member",sep="")
    myrequestmem <- GET(url = searchmemurl, add_headers('X-CSRF-ZOSMF-HEADER' = 'ANY'), authenticate(uid,pwd,type="basic"))
    if (myrequestmem$status_code == 200) {
      memlist <- as.data.frame(unlist((content(myrequestmem))$items),stringsAsFactors = FALSE)
      if (nrow(memlist) == 0) {
        shinyalert("Error", "No member in source pds",type = "error",confirmButtonCol = "#E74C3C")
      } else {
        x <- memlist[,1]
        selectInput("selected_mc_source_member", h4(strong("From Member"), style="font-style: bold; color: darkblue"), choices=x,selected = x[1])
      }
    }
    else {
      #shinyalert("Error", "Not a pds",type = "error",confirmButtonCol = "#E74C3C")
    } 
  })
  
  output$select_mc_target_member <- renderUI({  
    if (input$selected_mc_option == "Copy") {
      x <- input$selected_mc_source_member
    } else {
      x <- ""
    }
    textInput("selected_mc_target_member", h4(strong("To Member"), style="font-style: bold; color: darkblue"), value=x)
  })
  
  observeEvent(input$copy_mc, {
    myoperation <- "copy"
    if (input$selected_mc_type == "Dataset") {
      pc_json <- list(
        request = myoperation,
        "from-dataset" = list(
          dsn = input$accept_mc_source_dsname
        )
      )
      copyurl <- paste(baseurlzoweefiles,"ds/",input$accept_mc_target_dsname,sep="")
    } else {
      pc_json <- list(
        request = myoperation,
        "from-dataset" = list(
          dsn = input$accept_mc_source_dsname,
          member = input$selected_mc_source_member
        )
      )
      copyurl <- paste(baseurlzoweefiles,"ds/",input$accept_mc_target_dsname,"(",input$selected_mc_target_member,")",sep="")
    } 
    
    myrequest <- PUT(url = copyurl, add_headers('X-CSRF-ZOSMF-HEADER' = 'ANY'), body = pc_json, encode = "json",authenticate(uid,pwd,type="basic"))
    myresult <- (content(myrequest))
    if (myrequest$status_code == 200) {
      shinyalert("Success", "Copy Successful",type = "success",confirmButtonCol = "#54BA60")
    } else {
      if(!is.null(unlist(myresult$details))) {
        Error <- paste(myresult$message,"\n",unlist(myresult$details),sep="")
      } else {
        Error <- myresult$message
      }
      shinyalert("Error", Error,type = "error",confirmButtonCol = "#E74C3C")
    }
  })
  
  observeEvent(input$rename_mc, {
    myoperation <- "rename"
    if (input$selected_mc_type == "Dataset") {
      pc_json <- list(
        request = myoperation,
        "from-dataset" = list(
          dsn = input$accept_mc_source_dsname
        )
      )
      copyurl <- paste(baseurlzoweefiles,"ds/",input$accept_mc_target_dsname,sep="")
    } else {
      pc_json <- list(
        request = myoperation,
        "from-dataset" = list(
          dsn = input$accept_mc_source_dsname,
          member = input$selected_mc_source_member
        )
      )
      copyurl <- paste(baseurlzoweefiles,"ds/",input$accept_mc_source_dsname,"(",input$selected_mc_target_member,")",sep="")
    } 
    
    myrequest <- PUT(url = copyurl, add_headers('X-CSRF-ZOSMF-HEADER' = 'ANY'), body = pc_json, encode = "json",authenticate(uid,pwd,type="basic"))
    myresult <- (content(myrequest))
    if (myrequest$status_code == 200) {
      shinyalert("Success", "Rename Successful",type = "success",confirmButtonCol = "#54BA60")
    } else {
      if(!is.null(unlist(myresult$details))) {
        Error <- paste(myresult$message,"\n",unlist(myresult$details),sep="")
      } else {
        Error <- myresult$message
      }
      shinyalert("Error", Error,type = "error",confirmButtonCol = "#E74C3C")
    }
  })

  output$select_d_source_member <- renderUI({  
    searchmemurl <- paste(baseurlzoweefiles,"ds/",input$accept_d_source_dsname,"/member",sep="")
    myrequestmem <- GET(url = searchmemurl, add_headers('X-CSRF-ZOSMF-HEADER' = 'ANY'), authenticate(uid,pwd,type="basic"))
    if (myrequestmem$status_code == 200) {
      memlist <- as.data.frame(unlist((content(myrequestmem))$items),stringsAsFactors = FALSE)
      if (nrow(memlist) == 0) {
        shinyalert("Error", "No member in source pds",type = "error",confirmButtonCol = "#E74C3C")
      } else {
        x <- memlist[,1]
        selectInput("selected_d_source_member", h4(strong("From Member"), style="font-style: bold; color: darkblue"), choices=x,selected = x[1])
      }
    }
    else {
      #shinyalert("Error", "Not a pds",type = "error",confirmButtonCol = "#E74C3C")
    } 
  })
  
  observeEvent(input$delete_d, {
    if (input$selected_d_type == "Dataset") {
      delurl <- paste(baseurlzoweefiles,"ds/",input$accept_d_source_dsname,sep="")
    } else {
      delurl <- paste(baseurlzoweefiles,"ds/",input$accept_d_source_dsname,"(",input$selected_d_source_member,")",sep="")
    } 
    
    myrequest <- DELETE(url = delurl, add_headers('X-CSRF-ZOSMF-HEADER' = 'ANY'), authenticate(uid,pwd,type="basic"))
    myresult <- (content(myrequest))
    if (myrequest$status_code == 204) {
      shinyalert("Success", "Delete Successful",type = "success",confirmButtonCol = "#54BA60")
    } else {
      if(!is.null(unlist(myresult$details))) {
        Error <- paste(myresult$message,"\n",unlist(myresult$details),sep="")
      } else {
        Error <- myresult$message
      }
      shinyalert("Error", Error,type = "error",confirmButtonCol = "#E74C3C")
    }
  })
  
  output$select_filedata_layout_member <- renderUI({  
    searchmemurl <- paste(baseurlzoweefiles,"ds/",input$accept_filedata_layout_dsname,"/member",sep="")
    myrequestmem <- GET(url = searchmemurl, add_headers('X-CSRF-ZOSMF-HEADER' = 'ANY'), authenticate(uid,pwd,type="basic"))
    if (myrequestmem$status_code == 200) {
      memlist <- as.data.frame(unlist((content(myrequestmem))$items),stringsAsFactors = FALSE)
      if (nrow(memlist) == 0) {
        shinyalert("Error", "No member in source pds",type = "error",confirmButtonCol = "#E74C3C")
      } else {
        x <- memlist[,1]
        selectInput("selected_filedata_layout_member", h4(strong("Member"), style="font-style: bold; color: darkblue"), choices=x,selected = x[1])
      }
    }
    else {
      #shinyalert("Error", "Not a pds",type = "error",confirmButtonCol = "#E74C3C")
    } 
  })
  
  output$select_filedata_layout <- renderUI({
    
    if(input$browseData > browseDatacount) {
      browseDatacount <<- input$browseData
      showModal(modalDialog(
        title = "Please wait...",
        h4("Retrieving Data"),
        easyClose = FALSE,
        footer = NULL
      ))
      qryurl <- paste(baseurl,"filemanager/getData",sep="")
      pc_json <- list(
        resource = list(
          resourceName = input$accept_filedata_source_dsname
        ),
        template = list(
          copybookName = paste(input$accept_filedata_layout_dsname,"(",input$selected_filedata_layout_member,")",sep=""),
          language = input$selected_language
        ),
        operation = list(
          numRecords = input$accept_num_of_records,
          session = TRUE,
          token = ""
        ),
        position = list(
          record = input$accept_offset,
          key = input$accept_key_value
        )
      )
      myrequest <- POST(url = qryurl, body = pc_json, encode = "json",authenticate(uid,pwd,type="basic"))
      if (myrequest$status_code != 200) {
        removeModal()
        shinyalert("Error", content(myrequest)$errorDetails,type = "error",confirmButtonCol = "#E74C3C")
      }
      else {
        mydata <- unlist(content(myrequest)$records)
        mydata <- mydata[names(mydata) != "last"]
        layoutlist <- mydata[names(mydata) == "layouts.layout"]
        layoutmatrix <- unique(layoutlist)
        if(length(layoutmatrix) > 1) {
          shinyalert("Error", "Multiple Layouts not supported yet",type = "error",confirmButtonCol = "#E74C3C")
        }
        else {
          mylayout <- layoutmatrix[1]
          numrec <- length(layoutlist[layoutlist==mylayout])
          dx5 <- data.frame(matrix(mydata, ncol=numrec, byrow=F),stringsAsFactors = F)
          dx6 <- data.frame(matrix(names(mydata), ncol=numrec, byrow=F),stringsAsFactors = F)
          dx4 <<- cbind(dx6[,1],dx5)
          dx4[,1] <<- as.character(dx4[,1])
          removeModal()
          reactive_fm_table(browseDatacount)
          selectInput("selected_filedata_layout", h4(strong("Layout Name"), style="font-style: bold; color: darkblue"), choices=layoutmatrix,selected = layoutmatrix[1])
        }
      }
    }
    
  })
  
  output$ListFormatedData = DT::renderDataTable({
    input$selected_filedata_layout
    reactive_fm_table()
    if (!is.null(input$selected_filedata_layout)) {
      showModal(modalDialog(
        title = "Please wait...",
        h4("Formatting Output"),
        easyClose = FALSE,
        footer = NULL
      ))
      dx8 <- cbind(dx4[,1],dx4[,dx4[1,] == input$selected_filedata_layout])
      if (class(dx8) != "data.frame") {
        dx8 <- as.data.frame(dx8)
      }
      dx8[,1] <- as.character(dx8[,1])
      varnames <- dx8[dx8[,1]=="layouts.fields.field",][,2]
      vartypes <- dx8[dx8[,1]=="layouts.fields.type",][,2]
      dx7 <- dx8[dx8[,1]=="layouts.fields.value",]
      dx7[1] <- NULL
      dx7 <- transpose(dx7)
      names(dx7) <- varnames
      
      for (i in 1:length(vartypes)) {
        if (vartypes[i] != "AN") {
          dx7[,i] <- as.numeric(dx7[,i])
        }
      }
      removeModal()
      print(dx7)
      DT::datatable(
        dx7, class = 'cell-border stripe', rownames = FALSE, 
        extensions = list(FixedColumns = list(leftColumns = 1)), 
        options = list (pageLength = 10,headerCallback = JS(headerCallbackFM))
      )%>%
        formatStyle(columns = c(1), fontSize = '130%', color = 'darkblue', fontWeight = 'bold') %>% 
        formatStyle(columns = c(2:ncol(dx7)), fontSize = '130%')
        #formatStyle(columns = c(1), fontSize = '130%', color = 'white', fontWeight = 'bold', backgroundColor = 'darkblue') 
    }
  })

  headerCallbackFM <- c(
    "function(thead, data, start, end, display){",
    "  $('th', thead).css('border-top', '2px solid darkblue');",
    "  $('th', thead).css('border-bottom', '2px solid darkblue');",
    "  $('th', thead).css('color', 'white');",
    "  $('th', thead).css('backgroundColor', 'darkblue');",
    "}"
  )
  
  
  output$ListFormatedDB2Data = DT::renderDataTable({
    if(input$browseDB2Data > browseDB2Datacount) {
      browseDB2Datacount <<- input$browseDB2Data
      showModal(modalDialog(
        title = "Please wait...",
        h4("Retrieving Data"),
        easyClose = FALSE,
        footer = NULL
      ))
      qryurl <- paste(baseurl,"fmdb2genapi/getData",sep="")
      pc_json <- list(
        resource = list(
          ssid = input$accept_db2_ssid,
          owner = input$accept_db2_owner,
          tableName = input$accept_db2_tablename
        ),
        operation = list(
          numRows = input$accept_db2_num_of_records,
          session = TRUE,
          token = "",
          where = input$accept_db2_where
        ),
        position = list(
          row = input$accept_db2_offset
        )
      )
      
      myrequest <- POST(url = qryurl, body = pc_json, encode = "json",authenticate(uid,pwd,type="basic"))
      if (myrequest$status_code != 200) {
        removeModal()
        shinyalert("Error", content(myrequest)$errorDetails,type = "error",confirmButtonCol = "#E74C3C")
      }
      else {
        mydata <- unlist(content(myrequest))
        mydata <- mydata[names(mydata) != "rows.last"]
        token <- mydata[names(mydata) == "token"]
        mydata <- mydata[names(mydata) != "token"]
        dx5 <- matrix(mydata,ncol=3,byrow = T)
        fieldlist <- unique(dx5[,1])
        numrecs <- nrow(dx5)/length(fieldlist)
        dx6 <- data.frame(matrix(dx5[,3],nrow=numrecs,byrow = T),stringsAsFactors = F)
        names(dx6) <- fieldlist
        formatlist <- dx5[1:length(fieldlist),2]
        
        for (i in 1:length(formatlist)) {
          if(grepl("INT",formatlist[i]) || (formatlist[i] == "PD")) {
            dx6[,i] <- as.numeric(dx6[,i])
          }
          if(formatlist[i] == "DATE") {
            dx6[,i] <- as.Date(dx6[,i])
          }
        }
        removeModal()
        DT::datatable(
          dx6, class = 'cell-border stripe', rownames = FALSE,  
          extensions = list(FixedColumns = list(leftColumns = 1)), 
          options = list (pageLength = 10,headerCallback = JS(headerCallbackFM))
        )%>%
          formatStyle(columns = c(1), fontSize = '130%', color = 'darkblue', fontWeight = 'bold') %>% 
          formatStyle(columns = c(2:ncol(dx6)), fontSize = '130%')
          #formatStyle(columns = c(1), fontSize = '130%', color = 'white', fontWeight = 'bold', backgroundColor = 'darkblue') 
      }
    }
  })
  
  output$display_incomplete <- renderText({
    return("Feature is yet to be implemented")
  })  
  
  
    
})

shinyApp(ui = ui, server = server)