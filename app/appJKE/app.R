library(shiny)
library(shinydashboard)
library(httr)
library(jsonlite)
library(DT)
library(shinyjs)
library(stringr)
library(shinyalert)
#library(shinyWidgets)

ui <- dashboardPage(
  skin = "blue",
  title = "JKE Bank Customer Service Portal",
  dashboardHeader(
    title = h3("JKE Bank Customer Service Portal - Account Services", style="font-style: bold; align : top;"),
    titleWidth = "97%"
    
  ),
  dashboardSidebar(
    #width = "11%",
    #tags$hr(),
    #column(12, h4(strong("Account Services"),style="color: white")),
    #br(),
    #br(),
    HTML('<center><img src="JKEBankLogonew.png" width="240"></center>'),
    br(),
    tags$hr(),
    sidebarMenu(
      menuItem(strong("View Account"), tabName = "dashboard2", icon = icon("file-invoice-dollar"),badgeColor = "green"),
      menuItem(strong("Deposit Money"), tabName = "dashboard3", icon = icon("piggy-bank"),badgeColor = "green"),
      menuItem(strong("Withdraw Money"), tabName = "dashboard4", icon = icon("coins"),badgeColor = "green"),
      menuItem(strong("Create Account"), tabName = "dashboard5", icon = icon("landmark"),badgeColor = "green"),
      menuItem(strong("Update Account"), tabName = "dashboard1", icon = icon("edit"),badgeColor = "green"),
      menuItem(strong("Mortgage Advisor"), tabName = "dashboard6", icon = icon("hand-holding-usd"),badgeColor = "green"),
      useShinyalert(),
      useShinyjs()
    ),
    tags$hr(),
    #br(),
    # br(),
    # br(),
    # br(),
    # br(),
    #HTML('<center><img src="JKEBankLogo.png" width="220"></center>'),
    #img(align = "middle", src = "JKEBankLogo.png", height = 140, style="opacity: 0.7;"),
    br(),
    HTML('<center><font size="2"><b>This portal is a containerized shiny R application that consumes APIs from Mainframe CICS and integrates with other APIs. <a href="https://www.ibm.com/support/knowledgecenter/SSYMRC_5.0.2/com.ibm.help.common.jazz.calm.doc/topics/s_mtm_sample.html" target=_blank>JKE Banking</a> example project is shipped with Rational CLM.</b></font><width="150"></center>'),
    br(),
    HTML('<center><font size="2"><b>For more info, contact</b></font><width="150"></center>'),
    HTML('<center><font size="2"><b><a href="mailto:shami.gupta@in.ibm.com">Shami Gupta</a></b></font><width="150"></center>'),
    br()
  ),
  dashboardBody(
    tags$img(
      src = "Banking1.jpg",
      hspace = 0,
      vspace = 0,
      width = '86.5%',height = '91%',
      style = 'position: absolute '
    ),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "United.css")
    ),
    tags$head( 
      tags$style(HTML(".main-sidebar { font-size: 16px; }")) #change the font size to 20
    ),
    tags$style(HTML("
        input[type=number] {
          -moz-appearance:textfield;
        }
        input[type=number]::{
          -moz-appearance:textfield;
        }
        input[type=number]::-webkit-outer-spin-button,
        input[type=number]::-webkit-inner-spin-button {
        -webkit-appearance: none;
        margin: 0;
        }"
      )
    ),
    #setShadow("box"),
    #setShadow("DT"),
    #tags$head(tags$style(HTML('.box{-webkit-box-shadow: none; -moz-box-shadow: none;box-shadow: none;}'))),
    tags$head(
      tags$style(HTML(
        #'.skin-blue .main-sidebar{background-color: black;}',
        #'.skin-blue .main-sidebar .sidebar{color: red;}',
        #'.skin-blue .main-sidebar .sidebar .sidebar-menu{background-color: grey;}',
        #'.skin-blue .main-sidebar .sidebar .sidebar-menu .active a{background-color: red;}',
        #'.skin-blue .main-header .logo{background-color: black;}',
        '.box.box-solid.box-primary{border-bottom:none;border-left:none;border-right:none;border-top:none;}',
        '.box.box-solid.box-success{border-bottom:none;border-left:none;border-right:none;border-top:none;}',
        '.box.box-solid.box-danger{border-bottom:none;border-left:none;border-right:none;border-top:none;}',
        '.box.box-solid.box-warning{border-bottom:none;border-left:none;border-right:none;border-top:none;}',
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
            background: linear-gradient(to right,rgba(174, 214, 241 ,1), rgba(174, 214, 241 ,0));
        }',
        '.box.box-solid.box-success{
            background: linear-gradient(to right,rgba(130, 224, 170 ,1), rgba(130, 224, 170 ,0));
        }',
        '.box.box-solid.box-danger{
            background: linear-gradient(to right,rgba(241, 148, 138 ,1), rgba(241, 148, 138 ,0));
        }',
        '.box.box-solid.box-warning{
            background: linear-gradient(to right,rgba(248, 196, 113 ,1), rgba(248, 196, 113 ,0));
        }'
        )
      )
    ),
    br(),
    # fluidRow (
    #   column(3,
    #   dropdownButton(
    #     inputId = "mydropdown1",
    #     size="lg",
    #     label = "Controls",
    #     icon = icon("sliders"),
    #     status = "primary",
    #     circle = FALSE,
    #       sliderInput(
    #         inputId = "n",
    #         label = "Number of observations",
    #         min = 10, max = 100, value = 30
    #       )
    #     )
    #   ),
    #   column(3,
    #   dropdownButton(
    #     inputId = "mydropdown2",
    #     width=3,
    #     label = "Controls",
    #     icon = icon("sliders"),
    #     status = "primary",
    #     circle = FALSE,
    #     sliderInput(
    #       inputId = "n",
    #       label = "Number of observations",
    #       min = 10, max = 100, value = 30
    #     )
    #     )
    #   )
    # ),
    tabItems(
      tabItem(tabName = "dashboard1",
              box (
                title = span(icon("edit","fa-2x"),strong("Update Account Holder Information")), status = "success", width = 12,solidHeader = TRUE,collapsible=FALSE, 
                #style = 'position: absolute ; opacity: 0',
                fluidRow(
                  column(2,numericInput("accept_update_account_ref", h4(strong("Account Number"), style="font-style: bold; color: darkgreen"),value = 0))
                ),
                fluidRow(
                  column(2, actionButton("RetrieveAccount4update", label = h4("Select Account",style="font-weight: bold; color: yellow"),width='100%',style="background-color: darkgreen"))
                ),
                fluidRow(
                  column(3,textOutput("read4update_message"))
                ),
                conditionalPanel(
                  condition = "output.read4update_message == 'Account Retrieved'",
                    fluidRow(
                      column(3,h4(strong("Account Holder Name"), style="font-style: bold; color: darkgreen"),h3(strong(textOutput("display_account_holderU")), style="font-weight: bold; color: darkgreen; margin-top:1px"))
                      #column(3,h3(textOutput("display_account_holderU"),style="font-weight: bold; color: darkgreen"))
                    ),
                    br(),
                    fluidRow(
                      column(3,uiOutput("select_update_address")),
                      column(3,uiOutput("select_update_Cell"))
                    ),
                    fluidRow(
                      column(3,textInput("accept_update_remarks", h4(strong("Remarks"), style="font-style: bold; color: darkgreen")))
                    ),
                    fluidRow(
                      column(2, actionButton("UpdateAccount", label = h4("Update Account",style="font-weight: bold; color: yellow"),width='100%',style="background-color: darkgreen"))
                    ),
                    fluidRow(
                      column(3,textOutput("update_message"))
                    )
                )
              )
    ),
    tabItem(tabName = "dashboard2",
            box (
              title = span(icon("file-invoice-dollar","fa-2x"),strong("View Account Details")), status = "primary", width = 12,solidHeader = TRUE,collapsible=FALSE, 
              fluidRow(
                column(2,numericInput("accept_account_ref", h4(strong("Account Number"), style="font-style: bold; color: darkblue"),value = 0))
              ),
              fluidRow(
                column(2, actionButton("RetrieveAccount", label = h4("Retrieve Account",style="font-weight: bold; color: yellow"),width='100%',style="background-color: darkblue"))
              ),
              fluidRow(
                column(3,textOutput("retrieve_message"))
              ),
              conditionalPanel(
                condition = "output.retrieve_message == 'Account Retrieved'",
                fluidRow(
                  column(6,h6(DT::dataTableOutput("show_account_details")))
                  #column(4,img(src='Balance.png', height="250", align = "right"))
                )
              )
            )
    ),
    tabItem(tabName = "dashboard3",
            box (
              title = span(icon("piggy-bank","fa-2x"),strong("Deposit Money")), status = "success", width = 12,solidHeader = TRUE,collapsible=FALSE, 
              fluidRow(
                column(2,numericInput("accept_deposit_account_ref", h4(strong("Account Number"), style="font-style: bold; color: darkgreen"),value = 0))
              ),
              fluidRow(
                column(2, actionButton("RetrieveAccount4deposit", label = h4("Select Account",style="font-weight: bold; color: yellow"),width='100%',style="background-color: darkgreen"))
              ),
              fluidRow(
                column(3,textOutput("read4deposit_message"))
              ),
              conditionalPanel(
                condition = "output.read4deposit_message == 'Account Retrieved'",
                fluidRow(
                  column(3,h4(strong("Account Holder Name"), style="font-style: bold; color: darkgreen"),h3(strong(textOutput("display_account_holder")), style="font-weight: bold; color: darkgreen; margin-top:1px")),
                  column(3,h4(strong("Account Balance"), style="font-style: bold; color: darkgreen"),h3(strong(textOutput("display_current_balance")), style="font-weight: bold; color: darkgreen; margin-top:1px"))
                ),
                br(),
                fluidRow(
                  column(3, numericInput("accept_deposit_amount", h4(strong("Deposit"), style="font-style: bold; color: darkgreen"),value = 0)),
                  column(3, selectInput("selected_deposit_currency", h4(strong("Select Currency"), style="font-style: bold; color: darkgreen"), choices=c("USD","EUR","AUD","CAD","CHF","CNY","GBP","JPY","INR"),selected = "USD"))
                ),
                fluidRow(
                  column(6,textInput("accept_deposit_remarks", h4(strong("Remarks"), style="font-style: bold; color: darkgreen")))
                ),
                fluidRow(
                  column(2, actionButton("DepositAccount", label = h4("Deposit",style="font-weight: bold; color: yellow"),width='100%',style="background-color: darkgreen"))
                ),
                fluidRow(
                  column(3,textOutput("deposit_message"))
                )
              )
            )
    ),
    tabItem(tabName = "dashboard4",
            box (
              title = span(icon("coins","fa-2x"),strong("Withdraw Money")), status = "danger", width = 12,solidHeader = TRUE,collapsible=FALSE, 
              fluidRow(
                column(2,numericInput("accept_withdrawal_account_ref", h4(strong("Account Number"), style="font-weight: bold; color: darkred"),value = 0))
              ),
              fluidRow(
                column(2, actionButton("RetrieveAccount4withdrawal", label = h4("Select Account",style="font-weight: bold; color: yellow"),width='100%',style="background-color: darkred"))
              ),
              fluidRow(
                column(3,textOutput("read4withdrawal_message"))
              ),
              conditionalPanel(
                condition = "output.read4withdrawal_message == 'Account Retrieved'",
                fluidRow(
                  column(3,h4(strong("Account Holder Name"), style="font-style: bold; color: darkred"),h3(strong(textOutput("display_account_holderw")), style="font-weight: bold; color: darkred; margin-top:1px")),
                  column(3,h4(strong("Account Balance"), style="font-style: bold; color: darkred"),h3(strong(textOutput("display_current_balancew")), style="font-weight: bold; color: darkred; margin-top:1px"))
                ),
                br(),
                fluidRow(
                  column(3, numericInput("accept_withdrawal_amount", h4(strong("Withdrawal"), style="font-style: bold; color: darkred"),value = 0)),
                  column(3, selectInput("selected_withdrawal_currency", h4(strong("Select Currency"), style="font-style: bold; color: darkred"), choices=c("USD","EUR","AUD","CAD","CHF","CNY","GBP","JPY","INR"),selected = "USD"))
                ),
                fluidRow(
                  column(6,textInput("accept_withdrawal_remarks", h4(strong("Remarks"), style="font-style: bold; color: darkred")))
                ),
                fluidRow(
                  column(2, actionButton("withdrawalAccount", label = h4("Withdraw",style="font-weight: bold; color: yellow"),width='100%',style="background-color: darkred"))
                ),
                fluidRow(
                  column(3,textOutput("withdrawal_message"))
                )
              )
            )
    ),
    
    tabItem(tabName = "dashboard5",
            box (
              title = span(icon("landmark","fa-2x"),strong("Open New Account")), status = "warning", width = 12,solidHeader = TRUE,collapsible=FALSE, 
              fluidRow(
                column(2,numericInput("accept_add_account_ref", h4(strong("Account Number"), style="font-style: bold; color: #935116"),value = 0)),
                column(3,offset = 1,textInput("accept_add_name", h4(strong("Account Holder Name"), style="font-style: bold; color: #935116")))
              ),
              fluidRow(
                column(3,textInput("accept_add_address", h4(strong("Mailing Address"), style="font-style: bold; color: #935116"))),
                column(3,numericInput("accept_add_Cell", h4(strong("Cell No (Prefix Country Code)"), style="font-style: bold; color: #935116"),value = 0))
              ),
              fluidRow(
                column(2,numericInput("accept_add_balance", h4(strong("Account Balance (USD)"), style="font-style: bold; color: #935116"),value = 0.00)),
                column(3,offset = 1,textInput("accept_add_remarks", h4(strong("Remarks"), style="font-style: bold; color: #935116")))
              ),
              fluidRow(
                column(2, actionButton("AddAccount", label = h4("Create Account",style="font-weight: bold; color: yellow"),width='100%',style="background-color: #935116"))
              ),
              fluidRow(
                column(3,textOutput("add_message"))
              )
            )
    ),
    tabItem(tabName = "dashboard6",
            box (
              title = span(icon("hand-holding-usd","fa-2x"),strong("Mortgage Advisor")), status = "primary", width = 12,solidHeader = TRUE,collapsible=FALSE, 
              fluidRow(
                column(2,numericInput("accept_mortgage_amt", h4(strong("Mortgage Amount in USD"), style="font-style: bold; color: darkblue"),value = 0)),
                column(2,numericInput("accept_mortgage_term", h4(strong("Mortgage Term in years"), style="font-style: bold; color: darkblue"),value = 0)),
                column(2,numericInput("accept_mortgage_rate", h4(strong("Interest Rate %"), style="font-style: bold; color: darkblue"),value = 0))
              ),
              fluidRow(
                column(2, actionButton("get_mortgage_options", label = h4("Get Advice",style="font-weight: bold; color: yellow"),width='100%',style="background-color: darkblue"))
              ),
              fluidRow(
                column(5,textOutput("mortgage_message"))
              ),
              conditionalPanel(
                condition = "output.mortgage_message == ' ' || output.mortgage_message == '  '",
                fluidRow(
                  column(3,h4(strong("EMI in USD"), style="font-style: bold; color: darkblue"),h3(strong(textOutput("display_EMI")), style="font-weight: bold; color: darkblue; margin-top:1px"))
                ),
                fluidRow(
                  #column(3,textOutput("mortgage_offer_message"))
                  column(3,h3(strong(textOutput("mortgage_offer_message")), style="font-weight: bold; color: darkblue"))
                )
              ),
              conditionalPanel(
                condition = "output.mortgage_message == ' '",
                fluidRow(
                  column(10,h6(DT::dataTableOutput("show_mortgage_offers")))
                )
              )
            )
    )
  )
  )
)

options(shiny.maxRequestSize = 9*1024^2)

server <- shinyServer(function(input, output, session) {

  ReappData <<- reactiveValues()
  ReappData$a <<- ""
  ReappData$b <<- ""
  refreshbalance  <- reactiveVal(FALSE)
  refreshbalancewithdrawal  <- reactiveVal(FALSE)
  reactiveEMI <- reactiveVal(0)
  reactiveOptions <- reactiveVal(0)
  reactivenumofoption <- reactiveVal(0)
  d2 <<- data.frame(Parameter=as.character(),CustomerData=as.character(),stringsAsFactors=FALSE)
  retrieveacountpressedcount <<- 0
  addaccountcounter <<- 0
  RetrieveAccount4updatepressedcount <<- 0 
  updateaccountpressed <<- 0
  RetrieveAccount4depositpressedcount <<- 0 
  MortgageOptionpressedcount <<- 0
  depositpressed <<-0
  RetrieveAccount4withdrawalpressedcount <<- 0 
  withdrawalpressed <<- 0
  
  basemobilenumber <- "919830191760"
  AUTH_ID <- "MAZTI5MWUZZDY5NTCYYJ"
  AUTH_TOKEN <- "YzAyM2ExNjdiOTA0YjA2NTdiNzhmOTkyOTBmZWIx"
  smsurl <- "https://api.plivo.com/v1/Account/MAZTI5MWUZZDY5NTCYYJ/Message/"
  
  basedata <- data.frame()
  basedatadeposit <- data.frame()
  basedatawithdrawal <- data.frame()
  OTPIN <- ""
  OTPGenerated <- FALSE
  OTPValidated <- FALSE
  show_welcome <- reactiveVal(TRUE)

  output$mortgage_message <- renderText({
    if (!is.numeric(input$accept_mortgage_amt)) {
      disable("get_mortgage_options")
      return("Mortgage Amount must be numeric")
    }
    if (!is.numeric(input$accept_mortgage_term)) {
      disable("get_mortgage_options")
      return("Mortgage Term must be numeric")
    }
    if (!is.numeric(input$accept_mortgage_rate)) {
      disable("get_mortgage_options")
      return("Mortgage Interest Rate % must be numeric")
    }
    if (input$accept_mortgage_amt < 500 || input$accept_mortgage_amt > 500000) {
      disable("get_mortgage_options")
      return("Mortgage Amount must be between USD 500 and USD 500000")
    }
    if ((input$accept_mortgage_term - floor(input$accept_mortgage_term)) > 0 ) {
      disable("get_mortgage_options")
      return("Mortgage Term must in Whole years")
    }
    if (input$accept_mortgage_rate >= 100) {
      disable("get_mortgage_options")
      return("Mortgage Interest Rate % cant be 100% or more")
    }
    if (input$accept_mortgage_amt <= 0) {
      disable("get_mortgage_options")
      return("Mortgage Amount must be positive")
    }
    if (input$accept_mortgage_term <= 0 ) {
      disable("get_mortgage_options")
      return("Mortgage Term must be positive")
    }
    if (input$accept_mortgage_rate <= 0) {
      disable("get_mortgage_options")
      return("Mortgage Interest Rate % must be positive")
    }
    enable("get_mortgage_options")
    if (input$get_mortgage_options > MortgageOptionpressedcount) {
      MortgageOptionpressedcount <<- input$get_mortgage_options
      urlname <- paste("http://192.86.33.143:9080/jkeMortgage/user/",input$accept_mortgage_amt,",",input$accept_mortgage_term,",0,",input$accept_mortgage_rate,",Y",sep="")
      accountdata2 <- fromJSON(urlname)
      if (accountdata2[[1]][[1]][[2]] != "") {
        return(accountdata2[[1]][[1]][[2]])
      }
      else {
        x <- accountdata2[[1]][[1]][[1]]
        reactiveEMI(x)
      }
      urlname <- paste("http://192.86.33.143:9080/jkeMortgageCompany/user/0,",input$accept_mortgage_amt,",",input$accept_mortgage_term,",0,",input$accept_mortgage_rate,",Y",sep="")
      accountdata1 <- fromJSON(urlname)
      if (accountdata1[[1]][[1]] != "") {
        return(accountdata1[[1]][[1]])
      }
      else {
        if (accountdata1[[1]][[3]][[1]] == 0) {
          z <- -1
          reactivenumofoption(z)
          return("  ")
        }
        else {
          y <- accountdata1[[1]][[3]][[2]][1:accountdata1[[1]][[3]][[1]],]
          reactiveOptions(y)
          z <- accountdata1[[1]][[3]][[1]]
          reactivenumofoption(z)
          return(" ")
        }
      }
    }

  })
  
  output$display_EMI <- renderText({
    return(format(round(reactiveEMI(), 2), nsmall = 2))
  })

  output$mortgage_offer_message <- renderText({
    if (reactivenumofoption() < 0) {
      return("No better offer as of now")
    }
    else {
      return("Mortgage Offers")
    }
  })
  
    
  output$show_mortgage_offers = DT::renderDataTable({
    
    d2 <- reactiveOptions()
    d2 <- d2[,c(3,2,5,1,4)]
    names(d2) <- c("Mortgage Offering Partner","Contact","Term in Years","Interest Rate %","EMI in USD")
    d2[,4] <- format(round(d2[,4], 2), nsmall = 2)
    d2[,5] <- format(round(d2[,5], 2), nsmall = 2)

    DT::datatable(
      d2, 
      class = 'cell-border stripe',rownames = FALSE, 
      extensions = list(FixedColumns = list(leftColumns = 1)),
      options = list (pageLength = NROW(d2), searchable = FALSE, dom = 't',
                      headerCallback = JS(headerCallback1),
                      autoWidth = TRUE,
                      columnDefs = list(list(className = 'dt-center', targets = c(1,2,3,4)))
                      )
    ) %>%
      #formatStyle(columns = c(1, 5), fontSize = '150%', fontWeight = 'bold') %>%
      formatStyle(columns = c(1), fontSize = '150%', color = 'white', fontWeight = 'bold', backgroundColor = 'darkblue') %>%
      formatStyle(columns = c(2), fontSize = '150%', color = 'darkblue', fontWeight = 'bold') %>%
      formatStyle(columns = c(3), fontSize = '150%', color = 'darkblue', fontWeight = 'bold') %>%
      formatStyle(columns = c(4), fontSize = '150%', color = 'darkblue', fontWeight = 'bold') %>%
      formatStyle(columns = c(5), fontSize = '150%', color = 'white', fontWeight = 'bold', backgroundColor = 'darkblue') 
      
      
  })
  
  headerCallback1 <- c(
    "function(thead, data, start, end, display){",
    "  $('th', thead).css('border-top', '2px solid darkblue');",
    "  $('th', thead).css('border-bottom', '2px solid darkblue');",
    "  $('th', thead).css('color', 'darkblue');",
    "  $('th', thead).css('background-color', 'lightcyan');",
    "  $('th', thead).css('font-size', '16px');",
    "}"
  )
  output$retrieve_message <- renderText({
    if (show_welcome()) {
      shinyalert("Welcome", "This containerized application is built with APIs from Mainframe CICS Application", type = "success",confirmButtonCol = "#3F27B3")
      show_welcome(FALSE)
    }
    if (!is.numeric(input$accept_account_ref)) {
      disable("RetrieveAccount")
      return("Enter 6 digit Accound Code")
    }
    
    if(nchar(str_pad(input$accept_account_ref,6,pad="0")) == 6) {
      urlname <- paste("http://192.86.33.143:9080/jkebanking/accno/",str_pad(input$accept_account_ref,6,pad="0"),sep="")
      accountdata <- fromJSON(urlname)
      if(accountdata[[1]][[1]][[1]][[2]] != 0){
        disable("RetrieveAccount")
        return("Account Not Found")
      }
      else {
        if (input$RetrieveAccount > retrieveacountpressedcount) {
          return("Account Retrieved")
        }
        else {
          enable("RetrieveAccount")
          return("Account Found - Press Retrieve to continue -")
        }
      }
    } 
    else {
      disable("RetrieveAccount")
      return("Enter 6 digit Accound Code")
    }
      
  })
  
  
  
  output$show_account_details = DT::renderDataTable({

    if (input$RetrieveAccount > retrieveacountpressedcount) {
      retrieveacountpressedcount <<- input$RetrieveAccount
      urlname <- paste("http://192.86.33.143:9080/jkebanking/accno/",str_pad(input$accept_account_ref,6,pad="0"),sep="")
      accountdata <- fromJSON(urlname)
      
      if(nchar(str_pad(input$accept_account_ref,6,pad="0")) != 6) {
        stop("")
      } 
      
      
      if(accountdata[[1]][[1]][[1]][[2]] != 0){
        d2 <<- data.frame(Parameter=as.character(),CustomerData=as.character(),stringsAsFactors=FALSE)
        stop("Account Not Found")
      }
      
      d1 <- as.data.frame(accountdata[[1]][[2]][[1]],stringsAsFactors = F)    
      
      d2 <<- data.frame(Parameter=as.character(),CustomerData=as.character(),stringsAsFactors=FALSE)
      
      for (i in 1:NCOL(d1)) {
        d2[i,]$Parameter <<- names(d1)[i]
        d2[i,]$CustomerData <<- d1[1,i]
      }
      
      d2 <<- d2[c(6,8,2,4,3,5,7),]
      d2$Parameter <<- c("Account No", "Name", "Address", "Cell No", "Balance", "Last Update","Remarks")

    }

    DT::datatable(
      d2, 
      class = 'cell-border stripe',rownames = FALSE, colnames = NULL,
      extensions = list(FixedColumns = list(leftColumns = 1)),
      options = list (pageLength = NROW(d2), searchable = FALSE, dom = 't',
                      autoWidth = TRUE,
                      columnDefs = list(list(width = '500px', targets = c(1),width = '900px', targets = c(2))))
    ) %>%
      formatStyle(columns = c(1, 2), fontSize = '150%', fontWeight = 'bold') %>%
      formatStyle(columns = c(1), fontSize = '150%', color = 'white', fontWeight = 'bold', backgroundColor = 'darkblue') %>%
      formatStyle(columns = c(2), fontSize = '150%', color = 'darkblue', fontWeight = 'bold')
    
  })


  output$add_message <- renderText({
    
    if (!is.numeric(input$accept_add_account_ref)) {
      return("Account Number must be 6 digit numeric")
    }
    
    if (input$accept_add_account_ref > 999999) {
      return("Account Number must be 6 digit numeric")
    }
    
    if(nchar(str_pad(input$accept_add_account_ref,6,pad="0")) != 6){
      return("Account Number must be 6 digits")
    }
    
    if(!is.numeric(input$accept_add_account_ref)){
      return("Account Number must be numeric")
    }

    if(nchar(input$accept_add_address) > 20){
      return("Address can be max 20 chars long")
    }
    
    if(!is.numeric(input$accept_add_Cell)){
      return("Cell Number must be numeric")
    }
        
    if(!is.numeric(input$accept_add_balance)){
      return("Balance must be numeric")
    }
    
    if(input$accept_add_balance < 100 || input$accept_add_balance > 99999){
      return("Balance must be between 100 and 99999 USD")
    }

    if(nchar(input$accept_add_remarks) > 9){
      return("Remarks can be max 9 chars long")
    }
    
        
    add_date <- paste(as.integer(format(Sys.time(), "%d")),as.integer(format(Sys.time(), "%m")),as.integer(format(Sys.time(), "%y")))
    
    if (input$AddAccount > addaccountcounter) {
      addaccountcounter <<- input$AddAccount
      pc_json <- list(
        DFHCOMMAREA = list(
          LINK = list(
            LINK_COMM = list(
              KEYNUM = as.integer(input$accept_add_account_ref)
            )
          ),
          FILEA = list(
            FILEREC = list(
              NUMB = str_pad(input$accept_add_account_ref,6,pad="0"),
              NAME = input$accept_add_name,
              ADDRX = input$accept_add_address,
              PHONE = paste("+",input$accept_add_Cell,sep=""),
              DATEX = add_date,
              AMOUNT = paste("$",input$accept_add_balance,sep=""),
              COMMENT = input$accept_add_remarks
            )
          )
        )
      )
    
      res <- POST("http://192.86.33.143:9080/jkebanking/accno"
                , body = pc_json
                , encode = "json")
    
      appData <- content(res)
    
      if (appData[[1]][[1]][[1]][[2]] == 14) {
        shinyalert("Error", "Duplicate Account number", type = "error",confirmButtonCol = "#E74C3C")
        return("Duplicate Account number")
      }
    
      if (appData[[1]][[1]][[1]][[2]] == 0) {
        shinyalert("Success", "Account succesfully created. Please register your mobile", type = "success",confirmButtonCol = "#54BA60")
        return("Account Created")
      }
      
    }
    
    else {
      return("Press Create Account Button to Add")
    }
    
  })
  
  output$display_account_holderU <- renderText({
    input$RetrieveAccount4update
    #account_holder_name <- paste("Account Holder",basedata$NAME, sep = " : ")
    account_holder_name <- basedata$NAME
    return(account_holder_name)
  })
  
  
  output$select_update_address <- renderUI({
    input$RetrieveAccount4update
    current_address <- basedata$ADDRX
    textInput("selected_update_address", h4(strong("Mailing Address"), style="font-style: bold; color: darkgreen"),value = current_address)
  })
  
  output$select_update_Cell <- renderUI({
    input$RetrieveAccount4update
    current_cell_no <- as.numeric(gsub("\\+","",basedata$PHONE))
    numericInput("selected_update_Cell", h4(strong("Cell No (Prefix Country Code)"), style="font-style: bold; color: darkgreen"),value = current_cell_no)
  })
  
  output$read4update_message <- renderText({
    disable("UpdateAccount")
    if (input$RetrieveAccount4update > RetrieveAccount4updatepressedcount) {
      
      if (!is.numeric(input$accept_update_account_ref)) {
        return("Account Number must be 6 digit numeric")
      }
      
      if (input$accept_update_account_ref > 999999) {
        return("Account Number must be 6 digit numeric")
      }
      
      if(nchar(str_pad(input$accept_update_account_ref,6,pad="0")) < 6) {
        return("")
      } 
      RetrieveAccount4updatepressedcount <<- input$RetrieveAccount4update
      urlname <- paste("http://192.86.33.143:9080/jkebanking/accno/",str_pad(input$accept_update_account_ref,6,pad="0"),sep="")
      accountdata <- fromJSON(urlname)
      

      if(accountdata[[1]][[1]][[1]][[2]] != 0){
        return("Account Not Found")
      }
      basedata <<- as.data.frame(accountdata[[1]][[2]][[1]],stringsAsFactors = F)
      enable("UpdateAccount")
      return("Account Retrieved")
    }
  })  
  
  output$update_message <- renderText({
    
    
    if(nchar(input$selected_update_address) > 20){
      return("Address can be max 20 chars long")
    }
    
    if(!is.numeric(input$selected_update_Cell)){
      return("Cell Number must be numeric")
    }
    
    if(nchar(input$accept_update_remarks) > 9){
      return("Remarks can be max 9 chars long")
    }
    
    add_date <- paste(as.integer(format(Sys.time(), "%d")),as.integer(format(Sys.time(), "%m")),as.integer(format(Sys.time(), "%y")))
    
    if (input$UpdateAccount > updateaccountpressed) {
      updateaccountpressed <<- input$UpdateAccount
      pc_json <- list(
        DFHCOMMAREA = list(
          FILEA = list(
            FILEREC = list(
              STAT = "U",
              NUMB = basedata$NUMB,
              NAME = basedata$NAME,
              ADDRX = input$selected_update_address,
              PHONE = paste("+",input$selected_update_Cell,sep=""),
              DATEX = add_date,
              AMOUNT = basedata$AMOUNT,
              COMMENT = input$accept_update_remarks
            )
          ),
          COMM_AREA = list(
            FILEREC = list(
              STAT = basedata$STAT,
              NUMB = basedata$NUMB,
              NAME = basedata$NAME,
              ADDRX = basedata$ADDRX,
              PHONE = basedata$PHONE,
              DATEX = basedata$DATEX,
              AMOUNT = basedata$AMOUNT,
              COMMENT = basedata$COMMENT
            )
          )
        )
      )  
      
      
      res <- PUT(paste("http://192.86.33.143:9080/jkebanking/accno/",str_pad(input$accept_update_account_ref,6,pad="0"),sep="")
                 , body = pc_json
                 , encode = "json")
      
      appData <- content(res)
      
      if (appData[[1]][[1]][[1]][[2]] != 0) {
        return("Update Unsuccessful")
      }
      
      if (appData[[1]][[1]][[1]][[2]] == 0) {
        #RetrieveAccount4updatepressedcount <<- input$RetrieveAccount4update
        urlname <- paste("http://192.86.33.143:9080/jkebanking/accno/",str_pad(input$accept_update_account_ref,6,pad="0"),sep="")
        accountdata <- fromJSON(urlname)
        
        
        if(accountdata[[1]][[1]][[1]][[2]] != 0){
          shinyalert("Error", "Update Failed", type = "error",confirmButtonCol = "#E74C3C")
          return("Unexpected Error")
        }
        basedata <<- as.data.frame(accountdata[[1]][[2]][[1]],stringsAsFactors = F)
        #AUTH_ID="MAZTI5MWUZZDY5NTCYYJ"
        #AUTH_TOKEN="YzAyM2ExNjdiOTA0YjA2NTdiNzhmOTkyOTBmZWIx"
        account_masked <- paste(substring(basedata$NUMB,1,1),"X",substring(basedata$NUMB,3,3),"X",substring(basedata$NUMB,5,5),"X",sep="")
        message <- paste("Account Holder information for JKEBank Account ", account_masked, " is successfully updated",sep="")
        target_no <- gsub("\\+","",basedata$PHONE)
        #url="https://api.plivo.com/v1/Account/MAZTI5MWUZZDY5NTCYYJ/Message/"
        x <- POST(smsurl,authenticate(AUTH_ID,AUTH_TOKEN),body=list(src=basemobilenumber,dst=target_no,text=message))
        if(x[[2]] == 400){
          shinyalert("Warning", "Account successfully updated but cannot send sms - Please register Mobile Number", type = "warning",confirmButtonCol = "#E74C3C")
        }
        else {
          shinyalert("Success", "Account successfully updated", type = "success",confirmButtonCol = "#54BA60")
        }
        return("Account Updated")
      }
      
    }
    
    else {
      return("Press Update Account Button to Update")
    }
    
  })
  output$display_account_holder <- renderText({
    input$RetrieveAccount4deposit
    refreshbalance()
    account_holder_name <- basedatadeposit$NAME
    return(account_holder_name)
  })
  
  output$display_current_balance <- renderText({
    input$RetrieveAccount4deposit
    refreshbalance()
    current_balance <- basedatadeposit$AMOUNT
    return(current_balance)
  })
  
  output$read4deposit_message <- renderText({
    if (input$RetrieveAccount4deposit > RetrieveAccount4depositpressedcount) {
      
      if (!is.numeric(input$accept_deposit_account_ref)) {
        return("Account Number must be 6 digit numeric")
      }
      
      if (input$accept_deposit_account_ref > 999999) {
        return("Account Number must be 6 digit numeric")
      }
      
      if(nchar(str_pad(input$accept_deposit_account_ref,6,pad="0")) < 6) {
        disable("input$DepositAccount")
        return("")
      } 
      RetrieveAccount4depositpressedcount <<- input$RetrieveAccount4deposit
      urlname <- paste("http://192.86.33.143:9080/jkebanking/accno/",str_pad(input$accept_deposit_account_ref,6,pad="0"),sep="")
      accountdata <- fromJSON(urlname)
      
      
      if(accountdata[[1]][[1]][[1]][[2]] != 0){
        disable("input$DepositAccount")
        return("Account Not Found")
      }
      basedatadeposit <<- as.data.frame(accountdata[[1]][[2]][[1]],stringsAsFactors = F)
      enable("DepositAccount")
      return("Account Retrieved")
    }
  })  
  
  output$deposit_message <- renderText({
    
    
    if(!is.numeric(input$accept_deposit_amount)){
      return("deposit amount must be numeric")
    }
    
    if(nchar(input$accept_deposit_remarks) > 9){
      return("Remarks can be max 9 chars long")
    }
    
    
    accountbalance <- as.numeric(gsub("\\$","",basedatadeposit$AMOUNT))
    
    if (input$selected_deposit_currency != "USD") {
      input_currency <- paste("USD,",input$selected_deposit_currency,sep="")
      
      urlname <- paste("http://data.fixer.io/api/latest?access_key=79dc687089494f9b6ff9cf4eb66040f6&symbols=",input_currency,",USD",sep="")
      convrate <- fromJSON(urlname)
      if(convrate[[1]][[1]]) {
        rate <- convrate[[5]][[2]]/convrate[[5]][[1]]
      }
      else {
        return("Currency conversion Failed")
      }
    }
    else {
      rate <- 1
    }
    newaccountbalance <- round(accountbalance + input$accept_deposit_amount / rate, 2)

    if(newaccountbalance > 99999){
      return("Account balance exceeding the max limit")
    }
    
    
    add_date <- paste(as.integer(format(Sys.time(), "%d")),as.integer(format(Sys.time(), "%m")),as.integer(format(Sys.time(), "%y")))
    
    if (input$DepositAccount > depositpressed) {
      depositpressed <<- input$DepositAccount
      
      pc_json <- list(
        DFHCOMMAREA = list(
          FILEA = list(
            FILEREC = list(
              STAT = "U",
              NUMB = basedatadeposit$NUMB,
              NAME = basedatadeposit$NAME,
              ADDRX = basedatadeposit$ADDRX,
              PHONE = basedatadeposit$PHONE,
              DATEX = add_date,
              AMOUNT = paste("$",newaccountbalance,sep=""),
              COMMENT = input$accept_deposit_remarks
            )
          ),
          COMM_AREA = list(
            FILEREC = list(
              STAT = basedatadeposit$STAT,
              NUMB = basedatadeposit$NUMB,
              NAME = basedatadeposit$NAME,
              ADDRX = basedatadeposit$ADDRX,
              PHONE = basedatadeposit$PHONE,
              DATEX = basedatadeposit$DATEX,
              AMOUNT = basedatadeposit$AMOUNT,
              COMMENT = basedatadeposit$COMMENT
            )
          )
        )
      )  
      
      res <- PUT(paste("http://192.86.33.143:9080/jkebanking/accno/",str_pad(input$accept_deposit_account_ref,6,pad="0"),sep="")
                 , body = pc_json
                 , encode = "json")
      
      appData <- content(res)
      
      if (appData[[1]][[1]][[1]][[2]] != 0) {
        return("Deposit Unsuccessful")
      }
      
      if (appData[[1]][[1]][[1]][[2]] == 0) {
        #RetrieveAccount4depositpressedcount <<- input$RetrieveAccount4deposit
        urlname <- paste("http://192.86.33.143:9080/jkebanking/accno/",str_pad(input$accept_deposit_account_ref,6,pad="0"),sep="")
        accountdata <- fromJSON(urlname)
        
        
        if(accountdata[[1]][[1]][[1]][[2]] != 0){
          return("Unexpected Error")
        }
        basedatadeposit <<- as.data.frame(accountdata[[1]][[2]][[1]],stringsAsFactors = F)
        if (refreshbalance()) {
          newrefreshbalance <- FALSE
          refreshbalance(newrefreshbalance)
        }
        else {
          newrefreshbalance <- TRUE
          refreshbalance(newrefreshbalance)
        }
        disable("DepositAccount")
        #shinyalert("Success", "Deposit Request processed", type = "success")
        #AUTH_ID="MAZTI5MWUZZDY5NTCYYJ"
        #AUTH_TOKEN="YzAyM2ExNjdiOTA0YjA2NTdiNzhmOTkyOTBmZWIx"
        account_masked <- paste(substring(basedatadeposit$NUMB,1,1),"X",substring(basedatadeposit$NUMB,3,3),"X",substring(basedatadeposit$NUMB,5,5),"X",sep="")
        message <- paste("An amount of ", input$selected_deposit_currency, " ", input$accept_deposit_amount, " was credited to JKEBank account number ", account_masked, ". The updated balance is ", basedatadeposit$AMOUNT,sep="")
        target_no <- gsub("\\+","",basedatadeposit$PHONE)
        #url="https://api.plivo.com/v1/Account/MAZTI5MWUZZDY5NTCYYJ/Message/"
        x <- POST(smsurl,authenticate(AUTH_ID,AUTH_TOKEN),body=list(src=basemobilenumber,dst=target_no,text=message))
        if(x[[2]] == 400){
          shinyalert("Warning", "Deposit Request processed - But cannot send sms. Please register your mobile", type = "warning",confirmButtonCol = "#E74C3C")
        }else {
          shinyalert("Success", "Deposit Request processed", type = "success",confirmButtonCol = "#54BA60")
        }
        return("Deposit accepted")
      }
      
    }
    
    else {
      return("Press Deposit Button to Post")
    }
    
  })

  
  output$display_account_holderw <- renderText({
    input$RetrieveAccount4withdrawal
    refreshbalancewithdrawal()
    account_holder_name <- basedatawithdrawal$NAME
    return(account_holder_name)
  })
  
  output$display_current_balancew <- renderText({
    input$RetrieveAccount4withdrawal
    refreshbalancewithdrawal()
    current_balance <- basedatawithdrawal$AMOUNT
    return(current_balance)
  })
  
  output$read4withdrawal_message <- renderText({
    if (input$RetrieveAccount4withdrawal > RetrieveAccount4withdrawalpressedcount) {
      
      if (!is.numeric(input$accept_withdrawal_account_ref)) {
        return("Account Number must be 6 digit numeric")
      }
      
      if (input$accept_withdrawal_account_ref > 999999) {
        return("Account Number must be 6 digit numeric")
      }
      
      if(nchar(str_pad(input$accept_withdrawal_account_ref,6,pad="0")) < 6) {
        disable("input$withdrawalAccount")
        return("")
      } 
      RetrieveAccount4withdrawalpressedcount <<- input$RetrieveAccount4withdrawal
      urlname <- paste("http://192.86.33.143:9080/jkebanking/accno/",str_pad(input$accept_withdrawal_account_ref,6,pad="0"),sep="")
      accountdata <- fromJSON(urlname)
      
      
      if(accountdata[[1]][[1]][[1]][[2]] != 0){
        disable("input$withdrawalAccount")
        return("Account Not Found")
      }
      basedatawithdrawal <<- as.data.frame(accountdata[[1]][[2]][[1]],stringsAsFactors = F)
      OTPGenerated <<- FALSE
      return("Account Retrieved")
    }
  })  
  
  output$withdrawal_message <- renderText({
    
    
    if(!is.numeric(input$accept_withdrawal_amount)){
      return("withdrawal amount must be numeric")
    }
    
    if(nchar(input$accept_withdrawal_remarks) > 9){
      return("Remarks can be max 9 chars long")
    }
    
    
    accountbalance <- as.numeric(gsub("\\$","",basedatawithdrawal$AMOUNT))
    
    if (input$selected_withdrawal_currency != "USD") {
      input_currency <- paste("USD,",input$selected_withdrawal_currency,sep="")
      
      urlname <- paste("http://data.fixer.io/api/latest?access_key=79dc687089494f9b6ff9cf4eb66040f6&symbols=",input_currency,",USD",sep="")
      convrate <- fromJSON(urlname)
      if(convrate[[1]][[1]]) {
        rate <- convrate[[5]][[2]]/convrate[[5]][[1]]
      }
      else {
        return("Currency conversion Failed")
      }
    }
    else {
      rate <- 1
    }
    
    newaccountbalance <- round(accountbalance - input$accept_withdrawal_amount / rate, 2)
    
    if(newaccountbalance < 100){
      return("Account balance reducing below the min balance")
    }
    
    if (!OTPGenerated) {
      OTPIN <<- str_pad(sample(1:9999,1),4,pad="0")
      #AUTH_ID="MAZTI5MWUZZDY5NTCYYJ"
      #AUTH_TOKEN="YzAyM2ExNjdiOTA0YjA2NTdiNzhmOTkyOTBmZWIx"
      account_masked <- paste(substring(basedatawithdrawal$NUMB,1,1),"X",substring(basedatawithdrawal$NUMB,3,3),"X",substring(basedatawithdrawal$NUMB,5,5),"X",sep="")
      message <- paste("Your 4 digit OTP against JKEBANK account ",account_masked, " is ",OTPIN,sep="")
      target_no <- gsub("\\+","",basedatawithdrawal$PHONE)
      #url="https://api.plivo.com/v1/Account/MAZTI5MWUZZDY5NTCYYJ/Message/"
      #x <- POST(url,authenticate(AUTH_ID,AUTH_TOKEN),body=list(src="919830201760",dst="916290938787",text=message))
      x <- POST(smsurl,authenticate(AUTH_ID,AUTH_TOKEN),body=list(src=basemobilenumber,dst=target_no,text=message))
      if(x[[2]] == 400){
        shinyalert("Error", "Please register Mobile Number", type = "error",confirmButtonCol = "#E74C3C")
        disable("withdrawalAccount")
        return("Mobile setup needed")
      }
      else {
        enable("withdrawalAccount")
      }
      OTPGenerated <<- TRUE
      OTPValidated <<- FALSE
    }
    
    add_date <- paste(as.integer(format(Sys.time(), "%d")),as.integer(format(Sys.time(), "%m")),as.integer(format(Sys.time(), "%y")))
    
    if (input$withdrawalAccount > withdrawalpressed) {
      withdrawalpressed <<- input$withdrawalAccount
      if (!OTPValidated) {
        shinyalert(
         "Enter your OTP", type = "input",inputType="password",confirmButtonCol = "#3F27B3",
          callbackR = mycallbackOTPCheck
        )
        return("Press Withdraw Button to continue")
      }
      if (!OTPValidated) {
        return(" ")
      }
      pc_json <- list(
        DFHCOMMAREA = list(
          FILEA = list(
            FILEREC = list(
              STAT = "U",
              NUMB = basedatawithdrawal$NUMB,
              NAME = basedatawithdrawal$NAME,
              ADDRX = basedatawithdrawal$ADDRX,
              PHONE = basedatawithdrawal$PHONE,
              DATEX = add_date,
              AMOUNT = paste("$",newaccountbalance,sep=""),
              COMMENT = input$accept_withdrawal_remarks
            )
          ),
          COMM_AREA = list(
            FILEREC = list(
              STAT = basedatawithdrawal$STAT,
              NUMB = basedatawithdrawal$NUMB,
              NAME = basedatawithdrawal$NAME,
              ADDRX = basedatawithdrawal$ADDRX,
              PHONE = basedatawithdrawal$PHONE,
              DATEX = basedatawithdrawal$DATEX,
              AMOUNT = basedatawithdrawal$AMOUNT,
              COMMENT = basedatawithdrawal$COMMENT
            )
          )
        )
      )  
      
      res <- PUT(paste("http://192.86.33.143:9080/jkebanking/accno/",str_pad(input$accept_withdrawal_account_ref,6,pad="0"),sep="")
                 , body = pc_json
                 , encode = "json")
      
      appData <- content(res)
      
      if (appData[[1]][[1]][[1]][[2]] != 0) {
        return("withdrawal Unsuccessful")
      }
      
      if (appData[[1]][[1]][[1]][[2]] == 0) {
        #RetrieveAccount4withdrawalpressedcount <<- input$RetrieveAccount4withdrawal
        urlname <- paste("http://192.86.33.143:9080/jkebanking/accno/",str_pad(input$accept_withdrawal_account_ref,6,pad="0"),sep="")
        accountdata <- fromJSON(urlname)
        
        
        if(accountdata[[1]][[1]][[1]][[2]] != 0){
          return("Unexpected Error")
        }
        basedatawithdrawal <<- as.data.frame(accountdata[[1]][[2]][[1]],stringsAsFactors = F)
        if (refreshbalancewithdrawal()) {
          newrefreshbalancewithdrawal <- FALSE
          refreshbalancewithdrawal(newrefreshbalancewithdrawal)
        }
        else {
          newrefreshbalancewithdrawal <- TRUE
          refreshbalancewithdrawal(newrefreshbalancewithdrawal)
        }
        #shinyalert("Success", "Withdrawal Request processed", type = "success")
        #OTPGenerated <<- FALSE
        disable("withdrawalAccount")
        #AUTH_ID="MAZTI5MWUZZDY5NTCYYJ"
        #AUTH_TOKEN="YzAyM2ExNjdiOTA0YjA2NTdiNzhmOTkyOTBmZWIx"
        account_masked <- paste(substring(basedatawithdrawal$NUMB,1,1),"X",substring(basedatawithdrawal$NUMB,3,3),"X",substring(basedatawithdrawal$NUMB,5,5),"X",sep="")
        message <- paste("An amount of ", input$selected_withdrawal_currency, " ", input$accept_withdrawal_amount, " was debited from JKEBank account number ", account_masked, ". The updated balance is ", basedatawithdrawal$AMOUNT,sep="")
        target_no <- gsub("\\+","",basedatawithdrawal$PHONE)
        #url="https://api.plivo.com/v1/Account/MAZTI5MWUZZDY5NTCYYJ/Message/"
        x <- POST(smsurl,authenticate(AUTH_ID,AUTH_TOKEN),body=list(src=basemobilenumber,dst=target_no,text=message))
        if(x[[2]] == 400){
          shinyalert("Warning", "Withdrawal Request processed - But cannot send sms. Please register your mobile", type = "warning",confirmButtonCol = "#E74C3C")
        }else {
          shinyalert("Success", "Withdrawal Request processed", type = "success",confirmButtonCol = "#54BA60")
        }
        
        return("withdrawal accepted")
      }
      
    }
    
    else {
      return("Press Withdraw Button to Post")
    }
    
  })
  
  mycallbackOTPCheck <- function(value) {
    if (OTPIN == value) {
      OTPValidated <<- TRUE
    }
    else {
      shinyalert("Error", "Incorrect OTP Entered", type = "error",confirmButtonCol = "#E74C3C")
      OTPValidated <<- FALSE
    }
  }
  
  
    
})

shinyApp(ui = ui, server = server)