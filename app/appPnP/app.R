library(shiny)
#mspan <- span
#library(geosphere)
library(shinydashboard)
library(curl)
library(httr)
library(jsonlite)
library(DT)
library(shinyalert)
library(stringr)
library(shinyjs)
library(dplyr)
library(rmarkdown)
library(leaflet)

ui <- dashboardPage(
  skin = "blue",
  #title = "Pen & Paper Stores - Customer Application",
  dashboardHeader(
    title = "Pen & Paper Stores - Customer Application", 
    #style="font-style: bold; align : top;"),
    titleWidth = 1850
    
  ),
  dashboardSidebar(
    HTML('<center><img src="PnPStores.png" width="220"></center>'),
    #HTML('<center><img src="https://drive.google.com/uc?id=1wws90vaHoFfXIGh-MQUnCT-QAKjtowIt" width="220"></center>'),
    #tags$hr(),
    #column(12, h4(strong("Customer Order"),style="color: white")),
    #br(),
    #br(),
    tags$script('
              $(document).ready(function () {
              navigator.geolocation.getCurrentPosition(onSuccess, onError);
              
              function onError (err) {
              Shiny.onInputChange("geolocation", false);
              }
              
              function onSuccess (position) {
              setTimeout(function () {
              var coords = position.coords;
              console.log(coords.latitude + ", " + coords.longitude);
              Shiny.onInputChange("geolocation", true);
              Shiny.onInputChange("lat", coords.latitude);
              Shiny.onInputChange("long", coords.longitude);
              }, 1100)
              }
              });
    '),
    tags$hr(),
    sidebarMenu(id="tabs",
      conditionalPanel(
        #condition = "(output.OrderPage == ' ') || (output.retrieve_message == 'Payment Successful')",
        condition = "output.OrderPage == ' '",
        sidebarMenu(
          menuItem(strong("Item Stores"), tabName = "dashboard2", icon = icon("th"),badgeColor = "green")
        )
      ),
      conditionalPanel(
        condition = "output.OrderPage == '  ' ",
        sidebarMenu(
          menuItem("Payment", tabName = "dashboard1", icon = icon("coins"),badgeColor = "green")
        )
      )
    ),
    useShinyalert(),
    useShinyjs(),
    br(),
    br(),
    br(),
    br(),
    br(),
    #HTML('<center><img src="PnPStores.png" width="180"></center>'),
    br(),
    HTML('<center><font size="2"><b>This portal is a containerized shiny R application that consumes APIs from Mainframe CICS and integrates with other APIs. The CICS <a href="https://www.ibm.com/support/knowledgecenter/en/SSGMCP_5.1.0/com.ibm.cics.ts.exampleapplication.doc/topics/dfhxa_t100.html" target=_blank>Catalog Manager</a> application is a sample stationary stores application.</b></font><width="150"></center>'),
    #HTML('<center>This portal is a containerized shiny R application that consumes APIs from Mainframe CICS and integrates with other APIs. The CICS <a href="https://www.ibm.com/support/knowledgecenter/en/SSGMCP_5.1.0/com.ibm.cics.ts.exampleapplication.doc/topics/dfhxa_t100.html" target=_blank>Catalog Manager</a> application is a sample stationary stores application.<width="150"></center>'),
    br(),
    HTML('<center><font size="2"><b>For more info, contact</b></font><width="150"></center>'),
    HTML('<center><font size="2"><b><a href="mailto:shami.gupta@in.ibm.com">Shami Gupta</a></b></font><width="150"></center>'),
    br(),
    actionButton("ViewArchitecture", label = "View Architecture",width='80%',style="font-size: large; font-weight: bold; color: yellow; background-color: darkblue")
    #bsModal("modalExample", "Architecture", "ViewArchitecture", size = "large", "P&PArchitecture.jpg")
  ),
  dashboardBody(
    titlePanel(tags$head(tags$link(rel = "icon", type = "image/png", href = "favicon-16x16.png"),
                         tags$title("Pen & Paper Stores"))),
    
    tags$img(
      src = "stationary2.jpg",
      hspace = 0,
      vspace = 0,
      width = '100%',height = '90%',
      style = 'position: absolute'
    ),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "United.css")
    ),
    tags$head(
      tags$style(HTML(
        '.content-wrapper, .right-side {background-color: darkcyan;}',
        '.modal-l {width: 1000px;}'
        )
        
      )
    ),
    # tags$script('
    #           $(document).ready(function () {
    #           navigator.geolocation.getCurrentPosition(onSuccess, onError);
    #           
    #           function onError (err) {
    #           Shiny.onInputChange("geolocation", false);
    #           }
    #           
    #           function onSuccess (position) {
    #           setTimeout(function () {
    #           var coords = position.coords;
    #           console.log(coords.latitude + ", " + coords.longitude);
    #           Shiny.onInputChange("geolocation", true);
    #           Shiny.onInputChange("lat", coords.latitude);
    #           Shiny.onInputChange("long", coords.longitude);
    #           }, 1100)
    #           }
    #           });
    # '),
    
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
      tabItem(tabName = "dashboard1",
            box (
              title = span(icon("coins","fa-x"),strong("Order Processing")), status = "primary", width = 12,solidHeader = TRUE,collapsible=FALSE,  
              conditionalPanel(
                condition = "output.retrieve_message != 'Order Successfully Placed'",
                fluidRow(
                  column(4,textOutput("validation_message"))
                ),
                fluidRow(
                   column(6,h6(DT::dataTableOutput("show_stock_report")))
                )
              ),
              conditionalPanel(
                condition = "output.validation_message == 'Stock is not fully available'",
                br(),
                HTML('<left><font size="2"><font color="darkblue"><b>Please note that that all the items you ordered cannot be fulfilled at this point of time. If you agree to the same, please click the Accept button</b></font></font><width="150"></left>'),
                br(),
                br(),
                fluidRow(
                  column(2, actionButton("AcceptChanges", label = "Accept Changes",width='100%',style="font-size: large; font-weight: bold; color: yellow; background-color: darkgreen")),
                  column(2, actionButton("CancelButton", label = "Cancel",width='100%',style="font-size: large; font-weight: bold; color: yellow; background-color: darkred"))
                )
              ),
              column(width=6,
                conditionalPanel(
                  condition = "output.validation_message == 'Stock is available' && output.retrieve_message != 'Order Successfully Placed'",
                  fluidRow(
                    column(6,h4(strong(textOutput("payment_advice"),style="font-style: bold; color: darkblue; align:bottom"))),
                    conditionalPanel(
                      condition = "output.delivery_message == '  '",
                      column(6,h4(strong(textOutput("delivery_charges"),style="font-style: bold; color: green; align:bottom")))
                    )  
                  ),
                  conditionalPanel(
                    condition = "output.delivery_message == '  '",
                    br(),
                    br(),
                    fluidRow(style = "background-color:lightcyan;",
                      HTML('<left><font size="4"><font color="darkblue"><b><u>Delivery Address</u></b></font></font><width="150"></left>'),
                      fluidRow(
                        column(12,h5(strong(textOutput("delivery_address"),style="font-style: bold; color: darkblue; align:bottom")))
                      ),
                      br(),
                      fluidRow(
                        column(12,h4(strong(textOutput("delivery_date"),style="font-style: bold; color: darkred; align:bottom")))
                      )
                    )
                  ),
                  fluidRow(
                    column(8,textOutput("delivery_message"))
                  ),
                  br(),
                  br(),
                  br(),
                  br(),
                  br(),
                  conditionalPanel(
                    condition = "output.delivery_message == '  '",
                    fluidRow(
                      column(4,uiOutput("select_account_ref")),
                      column(2,textOutput("PaymentPage"))
                    )
                  ),
                  fluidRow(
                    conditionalPanel(
                      condition = "output.delivery_message == '  '",
                      column(4, actionButton("RetrieveAccount", label = "Enter OTP",width='100%',style="font-size: large; font-weight: bold; color: yellow; background-color: darkblue"))
                    ),
                    column(4, actionButton("CancelButton2", label = "Cancel",width='100%',style="font-size: large; font-weight: bold; color: yellow; background-color: darkred"))
                  )
                ),
                fluidRow(
                  column(6,textOutput("retrieve_message"))
                ),
                conditionalPanel(
                  condition = "output.retrieve_message == 'Order Successfully Placed'",
                  br(),
                  br(),
                  fluidRow(
                    column(6,downloadButton("GenerateInvoice", h4('Generate Invoice')))
                  ),
                  br(),
                  br(),
                  fluidRow(
                    column(4, actionButton("CreateNewOrder", label = "Create Another Order",width='100%',style="font-size: large; font-weight: bold; color: yellow; background-color: darkgreen"))
                  )
                )
            ),
            column(width = 6,
                conditionalPanel(
                  condition = "output.validation_message == 'Stock is available' && output.retrieve_message != 'Order Successfully Placed' && output.delivery_message == '  '",
                  fluidRow(
                    #column(6,textInput("accept_landmark", h4(strong("Landmark"), style="font-style: bold; color: darkblue"),value = "")),
                    column(9,style="font-style: bold; color: darkblue",
                           tags$table(width = "100%",
                                      tags$tr(width = "100%",
                                              tags$td(width = "20%", div(style = "font-weight: bold; color: darkblue;font-size:16px;", "Get Landmark")),
                                              tags$td(width = "80%", textInput(inputId = "accept_landmark", label = NULL)))
                           )
                    ),
                    column(2, actionButton("GoToLandmark", label = "Go",width='100%',style="font-size: large; font-weight: bold; color: yellow; background-color: darkblue"))
                  ),
                  fluidRow(
                    column(11,leafletOutput("incidentmap",height = 360, width = "100%"))
                  )
                )     
            )
          )
    ),
    tabItem(tabName = "dashboard2",
            box (
              title = span(icon("list","fa-x"),strong("Next Item Listing")), status = "primary", width = 6,solidHeader = TRUE,collapsible=TRUE,  
              #title = strong("Next Item Listing"), status = "primary", width = 12,solidHeader = TRUE,collapsible=TRUE,
              fluidRow(
                column(12,h6(DT::dataTableOutput("ListData")))
              ),
              fluidRow(
                column(1,textOutput("OrderPage"))
              )
            ),
            box (
              title = span(icon("anchor","fa-x"),strong("Selected Item Info")), status = "success", width = 6,solidHeader = TRUE,collapsible=TRUE,  
              br(),
              fluidRow(
                column(2,uiOutput("select_item_ref")),
                column(10,h6(DT::dataTableOutput("show_item_details")))
              ),
              br(),
              br(),
              fluidRow(
                #column(2, actionButton("PlaceOrder", label = h4("Order Item",style="font-weight: bold; color: yellow"),width='100%',style="background-color: darkgreen"))
                column(4, actionButton("PlaceOrder", label = "Add / Update item to cart",width='100%',style="font-size: large; font-weight: bold; color: yellow; background-color: darkgreen"))
                #column(2, actionButton("PlaceOrder", label = h6("Place Order",style="font-style: bold; color: black"),width='100%'))
              ),
              br(),
              br(),
              br()
            ),
            conditionalPanel(
              condition = "input.PlaceOrder != 0",
                box (
                  title = span(icon("cart-plus","fa-x"),strong("Shopping Cart")), status = "warning", width = 12,solidHeader = TRUE,collapsible=FALSE,  
                  #title = strong("Place Order"), status = "warning", width = 12,solidHeader = TRUE,collapsible=TRUE,
                  fluidRow(
                    column(6,h6(DT::dataTableOutput("show_item_details_order"))),
                    column(2, uiOutput("select_currency")),
                    column(4,h3(strong(textOutput("currency_description"),style="font-style: bold; color: #935116; align:bottom"))),
                    conditionalPanel(
                      condition = "input.PlaceOrder == 45",
                      column(2,numericInput("accept_order_quantity", h4(strong("Order Quantity"), style="font-style: bold; color: #935116"),value = 0, min = 0)),
                      column(4,h3(strong(textOutput("order_total_message"),style="font-style: bold; color: #935116; margin-top:20px")))
                    )
                  ),
                  fluidRow(
                    column(2, actionButton("SendOrder", label = "Proceed to Pay",width='100%',style="font-size: large; font-weight: bold; color: yellow; background-color: #935116"))
                    #column(2, actionButton("SendOrder", label = h4("Make Payment",style="font-weight: bold; color: yellow"),width='100%',style="background-color: #935116"))
                    #column(2, actionButton("SendOrder", label = h6("Place Order",style="font-style: bold; color: black"),width='100%'))
                  ),
                  fluidRow(
                    column(3,textOutput("order_message"),style="font-weight: bold; color: #935116")
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
  LocationInfo <<- reactiveValues()
  LocationInfo$a <<- ""
  ReappData$a <<- ""
  ReappData$b <<- ""
  order_unit_price <<- reactiveVal(0)
  additemstolist <- reactiveVal(FALSE)
  stock_verfied_reactive <- reactiveVal(FALSE)
  create_new_order_reactive <- reactiveVal(FALSE)
  cancel_order_reactive <- reactiveVal(FALSE)
  reactive_delivery_detail <- reactiveVal(FALSE)
  
  show_welcome <- reactiveVal(TRUE)
  retrieveacountpressedcount <- 0
  stock_Verified_order_table <<- data.frame(ItemRef=as.character(),Ordered=as.character(),Fulfillable=as.character(),ItemQty=as.integer(),stringsAsFactors=FALSE)
  #world_warehouse <- read.csv("deliver_center.csv",stringsAsFactors = F)
  google_distance_base_url <- "https://maps.googleapis.com/maps/api/distancematrix/json?units=metric&origins="
  google_distance_api_key <- "&mode=transit&sensor=true&key=AIzaSyCZBzc0BzKJ3U_MgqUk5JkAQAnSV7IHpyQ"
  dc_per_km <- 0.0005
  order_status <<- data.frame(ItemRef=as.character(),QtyRquested=as.character(),OrderPlaced=as.character(),ItemQty=as.integer(),stringsAsFactors=FALSE)
  saved_address <<- ""
  Additionalinfo <<- data.frame(WarehouseName=as.character(),WarehouseAddress=as.numeric(),DeliveryAddress=as.character(),DeliveryCharges=as.numeric(),DeliveryDate=as.character(),stringsAsFactors=FALSE)
  StoreGoToLandmark <<- 0
  
  stock_verified_message <<- ""
  stock_verified_price <<- ""
    
  #retrieveacountpressedcount <<- 0
  dx2 <<- data.frame(Parameter=as.character(),CustomerData=as.character(),stringsAsFactors=FALSE)
  orderitemlselectionlist <<- data.frame(ItemRef=as.character(),ItemDesc=as.character(),ItemCost=as.character(),ItemQty=as.integer(),stringsAsFactors=FALSE)
  OTPIN <- ""
  OTPGenerated <- FALSE
  OTPValidated <- FALSE
  basemobilenumber <- "919830191760"
  AUTH_ID <- "MAZTI5MWUZZDY5NTCYYJ"
  AUTH_TOKEN <- "YzAyM2ExNjdiOTA0YjA2NTdiNzhmOTkyOTBmZWIx"
  smsurl <- "https://api.plivo.com/v1/Account/MAZTI5MWUZZDY5NTCYYJ/Message/"
  ReactiveOrderToPayment <- reactiveVal(FALSE)
  ReactivePaymentMade <- reactiveVal(FALSE)
  ReactiveOrderPlaced <- reactiveVal(FALSE)
  #ReactiveItemSelect <- reactiveVal(TRUE)
  RefundMessage <<- ""
  PaymentMessage <<- ""
  BankAccountUsed <<- ""
  BankName <<- "JKE Bank"
  AccountHolderName <<- ""
  PaymentTable <<- data.frame(BankName=as.character(),BankAccountUsed=as.character(),AccountHolderName=as.character(),DebitTxn=as.numeric(),CreditTxn=as.numeric(),TxnCurrency=as.character(),stringsAsFactors=FALSE)
  #basemicroserviceurl <<- "http://173.193.92.99:32650/"
  basemicroserviceurl <<- "http://localhost:8000/"
  
  output$select_currency <- renderUI({
    z1 <- as.data.frame(fromJSON("http://data.fixer.io/api/symbols?access_key=79dc687089494f9b6ff9cf4eb66040f6"))
    currlist <<- as.data.frame(cbind(t(z1),gsub("symbols.","",names(z1))))
    row.names(currlist) <<- NULL
    currlist <<- currlist[2:NROW(currlist),]
    currlist <<- currlist[order(currlist[,1]),]
    names(currlist) <<- c("Currency.Name","Currency.Symbol")
    selectInput("selected_currency", h4(strong("Payment Currency"), style="font-style: bold; color: #935116"),choices=c(as.vector(currlist$Currency.Symbol)), selected="USD")
  })
  
  
  
  output$OrderPage <- renderText({
    
    if (ReactiveOrderToPayment()) {
      updateTabItems(session, "tabs", "dashboard1")
      return("  ")
    }
    else {
      updateTabItems(session, "tabs", "dashboard2")
      return(" ")
    }
  })
  
  
  # output$PaymentPage <- renderText({
  #   
  #   if (ReactivePaymentToOrder()) {
  #     updateTabItems(session, "tabs", "dashboard2")
  #     return("  ")
  #   }
  #   else {
  #     updateTabItems(session, "tabs", "dashboard1")
  #     return(" ")
  #   }
  # })
  # 
  
  
  
  mycallbackOTPCheck <- function(value) {
    if (OTPIN == value) {
      OTPValidated <<- TRUE
      updateActionButton(session, "RetrieveAccount", label = "Make Payment")
    }
    else {
      shinyalert("Error", "Incorrect OTP Entered", type = "error",confirmButtonCol = "#E74C3C")
      OTPValidated <<- FALSE
    }
  }

  mycallbackQuantity <- function(value) {
    if (value != "") {
      if ((as.numeric(value) - round(as.numeric(value),0)) == 0) {
        orderitemlselectionlist[orderitemlselectionlist$ItemRef==input$accept_item_ref,]$ItemQty <<- value
        if (additemstolist()) {
          additemstolist(FALSE)
        }
        else {
          additemstolist(TRUE) 
        }
      }
      else {
        shinyalert("Error", "Invalid Quantity", type = "error",confirmButtonCol = "#E74C3C")
      }
    }
    else {
      shinyalert("Error", "Invalid Quantity", type = "error",confirmButtonCol = "#E74C3C")
    }
  }

  output$validation_message <- renderText({
    stock_verfied_reactive()
    ReactiveOrderPlaced()
    return(stock_verified_message)
  })

  output$delivery_charges <- renderText({
    #reactive_delivery_detail()
    LocationInfo$a
    if (input$selected_currency != "USD") {
      urlname <- paste("http://data.fixer.io/api/latest?access_key=79dc687089494f9b6ff9cf4eb66040f6&symbols=",input$selected_currency,",USD",sep="")
      convrate <- fromJSON(urlname)
      if(convrate[[1]][[1]]) {
        rate <- convrate[[5]][[2]]/convrate[[5]][[1]]
        Additionalinfo$DeliveryCharges <<- round(Additionalinfo$DeliveryCharges / rate, 2)
      }
      else {
        Additionalinfo$DeliveryCharges <<- 0
      }
    }
    #return(paste("Delivery charges in", input$selected_currency, Additionalinfo$DeliveryCharges, sep=" "))
    return(paste("Delivery charges in", input$selected_currency, format(round(Additionalinfo$DeliveryCharges, 2), nsmall = 2), sep=" "))
  })
  
  output$delivery_date <- renderText({
    reactive_delivery_detail()
    LocationInfo$a
    return(paste("Expected Date of Delivery", Additionalinfo$DeliveryDate))
  })
  
  output$delivery_address <- renderText({
    reactive_delivery_detail()
    LocationInfo$a
    return(Additionalinfo$DeliveryAddress)
  })
  
  
  output$delivery_message <- renderText({
    
    if(!input$geolocation) {
      return("Cannot be delivered to your location")
    }

    #print(paste("User Lat ",input$lat))
    #print(paste("User Long",input$long))
    
    showModal(modalDialog(
      title = "Please wait...",
      h4("Checking Delivery"),
      easyClose = FALSE,
      footer = NULL
    ))
    
    
    store_runsql <- paste("select  CITY_ID, CITY_NAME,  LAT, LONG, sqrt(((long-(",input$long,"))*(long-(",input$long,")))+((lat-(",input$lat,"))*(lat-(",input$lat,")))) as A from world_city  order by A Fetch first 5 rows only",sep="")
    
    #basedvmurl <- "http://168.1.144.246:32576/"
    
    res <- POST(paste(basemicroserviceurl,"getDVMzEUSDocker?",sep="")
                ,body=list(myquerry = store_runsql),
                ,encode = "json")
    
    appData <- content(res)
	print(appData)
    if (appData[[2]][[2]] > 0) {
      mywarehouse <- as.data.frame(matrix(unlist(appData[[1]]), ncol=appData[[2]][[2]], byrow=TRUE), stringsAsFactors=FALSE)
      names(mywarehouse) <- c("WarehouseID","WarehouseName","WarehouseLat","WarehouseLong","cartesian")
    }
    else {
      removeModal()
      return("Cannot be delivered to your location")
    }
    
    warehouse_found <- FALSE
    i <- 1
    
    while (!warehouse_found && i <= nrow(mywarehouse)) {
      distance_get_url <- paste(google_distance_base_url,mywarehouse[i,]$WarehouseLat,",",mywarehouse[i,]$WarehouseLong,"&destinations=",input$lat,",",input$long,google_distance_api_key,sep="")
      #print(paste("Iteration ",i))
      #print(paste("Url ",distance_get_url))
      responsedata <- fromJSON(distance_get_url)
      if( responsedata$status != "OK") {
        i <- i + 1
      }
      if( responsedata$status == "OK") {
        if ((responsedata$rows)$elements[[1]]$status != "OK") {
          i <- i + 1
        }
        else {
          warehouse <- mywarehouse[i,]
          Additionalinfo[1,]$WarehouseName <<- mywarehouse[i,]$WarehouseName
          Additionalinfo[1,]$WarehouseAddress <<- responsedata$origin_addresses[1]
          Additionalinfo[1,]$DeliveryAddress <<- responsedata$destination_addresses[1]
          mydistance <- ((responsedata$rows)$elements[[1]]$distance)$value/1000
          mycost <- round(((responsedata$rows)$elements[[1]]$distance)$value/1000 * dc_per_km,2)
          if (mydistance > 500) {
            mycost <- mycost + 1
          }
          Additionalinfo[1,]$DeliveryCharges <<- mycost
          delivery_time <- ((responsedata$rows)$elements[[1]]$duration)$value * 2
          delivery_date <- format(Sys.time() + delivery_time,"%a %b %d")
          Additionalinfo[1,]$DeliveryDate <<- as.character(delivery_date)
          LocationInfo$a <<- Additionalinfo
          warehouse_found <- TRUE
          slat <<- as.numeric(warehouse$WarehouseLat)
          slong <<- as.numeric(warehouse$WarehouseLong)
          google_route_base_url <- "https://maps.googleapis.com/maps/api/directions/json?origin="
          get_route_url <- paste(google_route_base_url,slat,",",slong,"&destination=",input$lat,",",input$long,"&key=AIzaSyBggeTxDlyA7CcJq7hWhHPFgc10kIqLFH8",sep="")
          responsedata <- fromJSON(get_route_url)
          if (responsedata[[3]] == "OK") {
            polybasedata <<- data.frame(responsedata$routes$legs[[1]]$steps[[1]]$start_location$lat, responsedata$routes$legs[[1]]$steps[[1]]$start_location$lng,responsedata$routes$legs[[1]]$steps[[1]]$end_location$lat,responsedata$routes$legs[[1]]$steps[[1]]$end_location$lng)
            names(polybasedata) <<- c("StartLat","StartLong","EndLat","EndLong")
          }
          else {
            polybasedata <<- data.frame(StartLat=slat,StartLong=slong,EndLat=input$lat,EndLong=input$long)
          }
          #inter <<- geosphere::gcIntermediate(c(slong, slat), c(input$long, input$lat), n=50, addStartEnd=TRUE)
        }
      }
    }
    removeModal()
    if (!warehouse_found) {
      return("Cannot be delivered to your location")
    }
    else {
      return("  ")
    }
    
  })
  
  output$incidentmap <- renderLeaflet({
    
    icons <- awesomeIcons(
      icon = 'ios-close',
      iconColor = 'black',
      library = 'ion',
      markerColor = 'red'
    )
  
    
    leaflet() %>%
      setView(lng = input$long, lat = input$lat, zoom=16) %>%
      addTiles(options = providerTileOptions(noWrap = TRUE)) %>%
      addAwesomeMarkers(lng=input$long, lat=input$lat, icon=icons) %>%
      addMarkers(lng=slong, lat=slat) %>%
      fitBounds(slong, slat, input$long, input$lat) %>%
      #lines(inter)
      addPolylines(lng=polybasedata$StartLong, lat=polybasedata$StartLat, color = "darkred", weight = 3)
      #addPolylines(lng=c(polybasedata$StartLong, polybasedata$EndLong), lat=c(polybasedata$StartLat, polybasedata$EndLat),color = "darkred", weight = 2)
      #addPolylines(lng=c(slong, input$long), lat=c(slat, input$lat),color = "darkred", weight = 4, dashArray =c(10,10))
          #addMouseCoordinates(native.crs = TRUE)
  })

  observeEvent(input$incidentmap_click, {
    click <- input$incidentmap_click
    clat <- click$lat
    clong <- click$lng
    #print(paste(clat,"-",clong,"-",input$incidentmap_zoom))
    
    store_runsql <- paste("select  CITY_ID, CITY_NAME,  LAT, LONG, sqrt(((long-",clong,")*(long-",clong,"))+((lat-",clat,")*(lat-",clat,"))) as A from world_city  order by A Fetch first 5 rows only",sep="")

    showModal(modalDialog(
      title = "Please wait...",
      h4("Recalculating Delivery"),
      easyClose = FALSE,
      footer = NULL
    ))
    
    #basedvmurl <- "http://168.1.144.246:32576/"
    
    res <- POST(paste(basemicroserviceurl,"getDVMzEUSDocker?",sep="")
                ,body=list(myquerry = store_runsql),
                ,encode = "json")
    
    appData <- content(res)
    if (appData[[2]][[2]] > 0) {
      mywarehouse <- as.data.frame(matrix(unlist(appData[[1]]), ncol=appData[[2]][[2]], byrow=TRUE), stringsAsFactors=FALSE)
      names(mywarehouse) <- c("WarehouseID","WarehouseName","WarehouseLat","WarehouseLong","cartesian")
    }

    warehouse_found <- FALSE
    i <- 1
    
    while (!warehouse_found && i <= nrow(mywarehouse)) {
      distance_get_url <- paste(google_distance_base_url,mywarehouse[i,]$WarehouseLat,",",mywarehouse[i,]$WarehouseLong,"&destinations=",clat,",",clong,google_distance_api_key,sep="")
      #print(paste("Iteration ",i))
      #print(paste("Url ",distance_get_url))
      responsedata <- fromJSON(distance_get_url)
      if( responsedata$status != "OK") {
        i <- i + 1
      }
      if( responsedata$status == "OK") {
        if ((responsedata$rows)$elements[[1]]$status != "OK") {
          i <- i + 1
        }
        else {
          warehouse <- mywarehouse[i,]
          Additionalinfo[1,]$WarehouseName <<- mywarehouse[i,]$WarehouseName
          Additionalinfo[1,]$WarehouseAddress <<- responsedata$origin_addresses[1]
          Additionalinfo[1,]$DeliveryAddress <<- responsedata$destination_addresses[1]
          mydistance <- ((responsedata$rows)$elements[[1]]$distance)$value/1000
          mycost <- round(((responsedata$rows)$elements[[1]]$distance)$value/1000 * dc_per_km,2)
          if (mydistance > 500) {
            mycost <- mycost + 1
          }
          Additionalinfo[1,]$DeliveryCharges <<- mycost
          delivery_time <- ((responsedata$rows)$elements[[1]]$duration)$value * 2
          delivery_date <- format(Sys.time() + delivery_time,"%a %b %d")
          Additionalinfo[1,]$DeliveryDate <<- as.character(delivery_date)
          LocationInfo$a <<- Additionalinfo
          slat <<- as.numeric(warehouse$WarehouseLat)
          slong <<- as.numeric(warehouse$WarehouseLong)
          #print(slat)
          #print(slong)
          google_route_base_url <- "https://maps.googleapis.com/maps/api/directions/json?origin="
          get_route_url <- paste(google_route_base_url,slat,",",slong,"&destination=",clat,",",clong,"&key=AIzaSyBggeTxDlyA7CcJq7hWhHPFgc10kIqLFH8",sep="")
          responsedata <- fromJSON(get_route_url)
          if (responsedata[[3]] == "OK") {
            polybasedata <<- data.frame(responsedata$routes$legs[[1]]$steps[[1]]$start_location$lat, responsedata$routes$legs[[1]]$steps[[1]]$start_location$lng,responsedata$routes$legs[[1]]$steps[[1]]$end_location$lat,responsedata$routes$legs[[1]]$steps[[1]]$end_location$lng)
            names(polybasedata) <<- c("StartLat","StartLong","EndLat","EndLong")
          }
          else {
            polybasedata <<- data.frame(StartLat=slat,StartLong=slong,EndLat=clat,EndLong=clong)
          }
          
          warehouse_found <- TRUE
        }
      }
      
    }
    removeModal()
    
    icons <- awesomeIcons(
      icon = 'ios-close',
      iconColor = 'black',
      library = 'ion',
      markerColor = 'red'
    )
    
  if (warehouse_found) {
    leafletProxy('incidentmap') %>%
      clearMarkers() %>%
      clearShapes() %>%
      addAwesomeMarkers(lng=clong, lat=clat,icon=icons) %>%
      addMarkers(lng=slong, lat=slat) %>%
      #fitBounds(slong, slat, clong, clat) %>%
      fitBounds(responsedata$routes$bounds$northeast$lng, responsedata$routes$bounds$northeast$lat, responsedata$routes$bounds$southwest$lng, responsedata$routes$bounds$southwest$lat) %>%
      addPolylines(lng=polybasedata$StartLong, lat=polybasedata$StartLat, color = "darkred", weight = 3)
      #addPolylines(lng=c(slong, clong), lat=c(slat, clat),color = "darkred", weight = 4, dashArray =c(10,10))
    
  }  
  else {
    shinyalert("Error", "Location not not uniquely identified. Zoom in more to select", type = "error",confirmButtonCol = "#E74C3C")
  }
    
  })
  
#############################################

  observeEvent(input$ViewArchitecture, {
    
     showModal(modalDialog(
       title = h4("Architecture"),
       size ="l",
       HTML('<img src="P&PArchitecture.jpg" width="860">'),
       easyClose = FALSE,
       footer = modalButton("Dismiss")
     ))
    
    #shinyalert("", imageUrl = "P&PArchitecture.jpg", imageWidth = 1000,imageHeight = 1000, type = "info",confirmButtonCol = "#3F27B3")
  
  })
  
  observeEvent(input$GoToLandmark, {
    
    if (input$GoToLandmark > StoreGoToLandmark) {
      StoreGoToLandmark <<- input$GoToLandmark
      LandmarkName <- trimws(gsub("\\s+", " ", input$accept_landmark))
      
      ModLandmarkName <- gsub("\\ ","\\+",LandmarkName) 
      
      landmark_found <- FALSE
      
      get_milestone_url <- paste("https://maps.googleapis.com/maps/api/geocode/json?address=",ModLandmarkName,"&key=AIzaSyBggeTxDlyA7CcJq7hWhHPFgc10kIqLFH8",sep="")
      
      responsedata <- fromJSON(get_milestone_url)
      
      if(responsedata$status == "OK") {
        if (nrow(responsedata$results$geometry$location) == 1) {
          llong <- responsedata$results$geometry$location$lng
          llat <- responsedata$results$geometry$location$lat
          landmark_found <- TRUE
        }
      }
    }

    if (landmark_found) {
      leafletProxy('incidentmap') %>%
        setView(lng = llong, lat = llat, zoom=17) 
    }  
    else {
      shinyalert("Error", "Milestone is not uniquely identified", type = "error",confirmButtonCol = "#E74C3C")
    }
    
  })
  
  
  
  
##############################################
  
    

  output$payment_advice <- renderText({
    stock_verfied_reactive()
    #return(paste("Order Value in",input$selected_currency, stock_verified_price, sep=" "))
    return(paste("Order Value in",input$selected_currency, format(round(stock_verified_price, 2), nsmall = 2), sep=" "))
  })
  
      
    
  output$retrieve_message <- renderText({
    if (create_new_order_reactive()) {
      #create_new_order_reactive(FALSE)
      return(" ")
    }
    if (cancel_order_reactive()) {
      #cancel_order_reactive(FALSE)
      dx1$PHONE <<- "+0"
      return(" ")
    }
    
    if (!is.numeric(input$accept_account_ref)) {
      disable("RetrieveAccount")
      return(" ")
    }
    
    if(nchar(str_pad(input$accept_account_ref,6,pad="0")) == 6) {
      urlname <- paste("http://192.86.33.143:9080/jkebanking/accno/",str_pad(input$accept_account_ref,6,pad="0"),sep="")
      accountdata <- fromJSON(urlname)
      if(accountdata[[1]][[1]][[1]][[2]] != 0){
        disable("RetrieveAccount")
        return(" ")
      }
      else {
        if (input$RetrieveAccount > retrieveacountpressedcount) {
          retrieveacountpressedcount <<- input$RetrieveAccount
          if (!OTPValidated){
            shinyalert(
              "Enter your OTP", type = "input",inputType="password",confirmButtonCol = "#3F27B3",
              callbackR = mycallbackOTPCheck
            )
            return("Press Make Payment to Pay")
            #return("Press Enter OTP to continue Payment")
          }
          else {
            y <- paste(input$selected_currency,stock_verified_price + Additionalinfo$DeliveryCharges,str_pad(input$accept_account_ref,6,pad="0"),sep=" ")
            
            x <-  paste(as.vector(t(stock_Verified_order_table[,c(1,3)])),collapse=" ")
            
            res <- POST(paste(basemicroserviceurl,"order?",sep="")
            #res <- POST("http://zmicroservices-debahmuk-build-app.apps.ocp-na1.prod.nextcle.com/order?"
                        , body = paste("x=",x,"\\&","y=",y,sep=""),
                        , encode = "json")
            
            appData <- content(res)
            
            if (appData[[3]] != "") {
              disable("RetrieveAccount")
              shinyalert("Error", paste("Order Unsuccessful - ", appData[[3]],sep=""), type = "error", confirmButtonCol = "#E74C3C")
              return("Order Placement Failed")
            }
            else {
              shinyalert("Success", "Order Successfully Placed", type = "success",confirmButtonCol = "#54BA60")
              shinyalert("Success", paste("Refund Amount ", appData[[2]][[1]], " ", input$selected_currency, sep=""), type = "success",confirmButtonCol = "#54BA60")
              #disable("RetrieveAccount")
              order_status <<- as.data.frame(matrix(unlist(appData[[1]]), ncol=3, byrow=TRUE), stringsAsFactors=FALSE)
              names(order_status) <<- c("ItemRef", "QtyRquested", "OrderPlaced")
              order_status$ItemRef <<- as.character(order_status$ItemRef)
              order_status <<- left_join(order_status,orderitemlselectionlist)
              order_status <<- order_status[,c(1,4,5,3)]
              order_status[,3] <<- as.numeric(order_status[,3])
#20200229#    
              if (input$selected_currency != "USD") {
                urlname <- paste("http://data.fixer.io/api/latest?access_key=79dc687089494f9b6ff9cf4eb66040f6&symbols=",input$selected_currency,",USD",sep="")
                convrate <- fromJSON(urlname)
                if(convrate[[1]][[1]]) {
                  rate <- convrate[[5]][[2]]/convrate[[5]][[1]]
                }
                else {
                  rate <- 0
                }
              }
              else {
                rate <- 1
              }
              order_status[,3] <<- round(order_status[,3] / rate, 2)
              
              names(order_status) <<- c("Item Ref", "Item Description", paste("Unit Price in ",input$selected_currency,sep=""), "Quantity Ordered")
              PaymentTable <<- data.frame(BankName=BankName,BankAccountUsed=str_pad(input$accept_account_ref,6,pad="0"),AccountHolderName=AccountHolderName,DebitTxn=(as.numeric(stock_verified_price)+ Additionalinfo$DeliveryCharges),CreditTxn=as.numeric(appData[[2]][[1]]),TxnCurrency=input$selected_currency,stringsAsFactors=FALSE)
              names(PaymentTable) <<- c("Bank Name", "Account No", "Account Holder", "Debit Amount", "Credit Amount","Currency")
              #RefundMessage <<- paste("Refund Amount ", input$selected_currency, " ", appData[[2]][[1]], sep="")
              #PaymentMessage <<- paste("Payment Amount ", input$selected_currency, " ", stock_verified_price, sep="")
              #BankAccountUsed <<- str_pad(input$accept_account_ref,6,pad="0")
              
              print(order_status)
              print("**********")
              print(PaymentTable)
              
              x <- FALSE
              y <- TRUE
              ReactiveOrderToPayment(x)
              ReactivePaymentMade(y)
              #disable("accept_order_quantity")
              #disable("select_item_ref")
              #disable("PlaceOrder")
              #ReactiveItemSelect(x)
              updateActionButton(session, "RetrieveAccount", label = "Enter OTP")
              #updateActionButton(session, "SendOrder", label = "Confirm Order")
              #updateTabItems(session, "tabs", "dashboard2")
              return("Order Successfully Placed")
            }
          }
        }
        else {
          dx1 <<- as.data.frame(accountdata[[1]][[2]][[1]],stringsAsFactors = F)
          acountbalance <- as.numeric(gsub("\\$","",dx1$AMOUNT))
          #OTPIN <<- ""
          OTPGenerated <<- FALSE
          #OTPValidated <<- FALSE
          if ((acountbalance - order_unit_price()*input$accept_order_quantity) < 100) {
            disable("RetrieveAccount")
            return("Insufficient Balance to Pay")
          }
          else {
            if (!OTPGenerated) {
              OTPIN <<- str_pad(sample(1:9999,1),4,pad="0")
              #AUTH_ID="MAZTI5MWUZZDY5NTCYYJ"
              #AUTH_TOKEN="YzAyM2ExNjdiOTA0YjA2NTdiNzhmOTkyOTBmZWIx"
              AccountHolderName <<- dx1$NAME
              account_masked <- paste(substring(dx1$NUMB,1,1),"X",substring(dx1$NUMB,3,3),"X",substring(dx1$NUMB,5,5),"X",sep="")
              message <- paste("Your 4 digit OTP against JKEBANK account ",account_masked, " is ",OTPIN,sep="")
              target_no <- gsub("\\+","",dx1$PHONE)
              #url="https://api.plivo.com/v1/Account/MAZTI5MWUZZDY5NTCYYJ/Message/"
              #x <- POST(url,authenticate(AUTH_ID,AUTH_TOKEN),body=list(src="919830201760",dst="916290938787",text=message))
              #x <- POST(smsurl,authenticate(AUTH_ID,AUTH_TOKEN),body=list(src=basemobilenumber,dst=target_no,text=message))
              checksql <- paste ("Select * from IDZPOT.OTP_PROCESS WHERE MOBILE_NO = ",target_no,sep="")
              res <- POST(paste(basemicroserviceurl,"getDB2zEUSDocker?",sep="")
                          ,body=list(myquerry = checksql),
                          ,encode = "json")
              appData <- content(res)
              if (length(appData$result) == 0) {
                shinyalert("Error", "Please register with JKE Bank OTP Mobile Tracker App", type = "error",confirmButtonCol = "#E74C3C")
                disable("RetrieveAccount")
                return("Mobile not registered for online Payment")
              }
              else {
                newOTP_TS <- fromJSON("http://worldtimeapi.org/api/timezone/Etc/UTC")$unixtime
                updatesql <- paste ("UPDATE IDZPOT.OTP_PROCESS SET OTP = ", OTPIN, ", OTP_TS = ", newOTP_TS, " WHERE MOBILE_NO = ",target_no,sep="")
                res <- POST(paste(basemicroserviceurl,"getDB2zEUSDocker?",sep="")
                            ,body=list(myquerry = updatesql),
                            ,encode = "json")
                appData <- content(res)
                enable("RetrieveAccount")
                OTPGenerated <<- TRUE
                OTPValidated <<- FALSE
                shinyalert("Info", "OTP Pushed to JKE Bank OTP Tracker App", type = "info",confirmButtonCol = "#3F27B3")
                return("Press Enter OTP to continue Payment")
              }
            }
          }
        }
      }
    } 
    else {
      disable("RetrieveAccount")
      return("Enter 6 digit Accound Code")
    }
    
  })
  
  
  output$select_item_ref <- renderUI({
    if (!is.null(input$table_item_ref)) {
      #if (ReactiveItemSelect()) {
        x <- input$table_item_ref
      #}
    }
    else {
      x <- 10
    }
    numericInput("accept_item_ref", h4(strong("Item Ref"), style="font-style: bold; color: darkgreen"),value = x, min = 10, step = 10)
  })
  

  output$select_account_ref <- renderUI({
    if (create_new_order_reactive()) {
      x <- 0
      #OTPIN <<- ""
      #OTPGenerated <<- FALSE
      #OTPValidated <<- FALSE
    }
    if (cancel_order_reactive()) {
      x <- 5
    }
    numericInput("accept_account_ref", h4(strong("Payment Account Number"), style="font-style: bold; color: darkblue"),value = 0)
  })
  
    
  output$show_item_details = DT::renderDataTable({
    
    z <- ReappData$c
    urlname <- paste("http://192.86.33.143:9080/catalogManager/items/",input$accept_item_ref,sep="")
    itemrefdata <- fromJSON(urlname)
    d1 <- as.data.frame(itemrefdata[[1]][[2]][[1]])    
    
    if(d1[1,1] != input$accept_item_ref){
      stop("Record Not Found")
    }
    
    d1 <- d1[,c(1,2,5)]
  
    DT::datatable(
      d1, class = 'cell-border stripe', rownames = FALSE, 
      #caption = h4('Selected Item',style = 'caption-side: top; color:darkgreen ; font-Weight:bold'),
      extensions = list(FixedColumns = list(leftColumns = 1)),
      options = list (dom = 't',
                      columnDefs = list(list(className = 'dt-center', targets = c(0,2))),
                      headerCallback = JS(headerCallback3)
                      )
    )%>%
      formatStyle(columns = c(1), fontSize = '130%', color = 'white', fontWeight = 'bold', backgroundColor = 'darkgreen') %>%
      formatStyle(columns = c(2), fontSize = '130%', color = 'darkgreen', fontWeight = 'bold') %>%
      formatStyle(columns = c(3), fontSize = '130%', color = 'white', fontWeight = 'bold', backgroundColor = 'darkgreen') 
      #formatStyle(columns = c(4), fontSize = '130%', color = 'white', fontWeight = 'bold', backgroundColor = 'darkgreen') %>%
      #formatStyle(columns = c(5), fontSize = '130%', color = 'darkgreen', fontWeight = 'bold') %>%
      #formatStyle(columns = c(6), fontSize = '130%', color = 'darkgreen', fontWeight = 'bold')
  })

  headerCallback3 <- c(
    "function(thead, data, start, end, display){",
    "  $('th', thead).css('border-top', '2px solid darkgreen');",
    "  $('th', thead).css('border-bottom', '2px solid darkgreen');",
    "  $('th', thead).css('color', 'darkgreen');",
    "}"
  )
  

  output$show_stock_report = DT::renderDataTable({
    
    z <- ReappData$c
    stock_verfied_reactive()
    
  
    d1 <- left_join(stock_Verified_order_table,orderitemlselectionlist)[,c(1,4,2,3)]
    
    DT::datatable(
      d1, class = 'cell-border stripe', rownames = FALSE,
      extensions = list(FixedColumns = list(leftColumns = 1)),
      options = list (dom = 't',
                      columnDefs = list(list(className = 'dt-center', targets = c(0,2,3))),
                      headerCallback = JS(headerCallback1),
                      rowCallback = DT::JS(
                        'function(row, data) {',
                        '$("td", row).each(function(i) {',
                        'if (data[2] == data[3])',
                        '$(this).css("background-color", "lightgreen");',
                        'if (data[2] != data[3] && data[3] == 0)',
                        '$(this).css("background-color", "red");',
                        'if (data[2] != data[3] && data[3] != 0)',
                        '$(this).css("background-color", "orange");',
                        
                        '});',
                        '}')
      )
    ) %>%
      #formatStyle(columns = c(1), fontSize = '150%', color = 'white', fontWeight = 'bold', backgroundColor = '#AF601A') %>%
      #formatStyle(columns = c(2), fontSize = '150%', color = 'white', fontWeight = 'bold', backgroundColor = '#AF601A') %>%
      #formatStyle(columns = c(3), fontSize = '150%', color = 'white', fontWeight = 'bold', backgroundColor = '#AF601A') %>%
      #formatStyle(columns = c(4), fontSize = '150%', color = 'white', fontWeight = 'bold', backgroundColor = '#AF601A') 
      formatStyle(columns = c(1), fontSize = '150%', color = 'darkblue', fontWeight = 'bold') %>%
      formatStyle(columns = c(2), fontSize = '150%', color = 'darkblue', fontWeight = 'bold') %>%
      formatStyle(columns = c(3), fontSize = '150%', color = 'darkblue', fontWeight = 'bold') %>%
      formatStyle(columns = c(4), fontSize = '150%', color = 'darkblue', fontWeight = 'bold') 
    
  })
  
  headerCallback2 <- c(
    "function(thead, data, start, end, display){",
    "  $('th', thead).css('border-top', '2px solid #935116');",
    "  $('th', thead).css('border-bottom', '2px solid #935116');",
    "  $('th', thead).css('color', '#935116');",
    "}"
  )
  
  
    
  output$show_item_details_order = DT::renderDataTable({
    
    z <- ReappData$c
    additemstolist()
    

    d1 <- orderitemlselectionlist

    DT::datatable(
      d1, class = 'cell-border stripe', rownames = FALSE,
      extensions = list(FixedColumns = list(leftColumns = 1)),
      options = list (dom = 't',
                      columnDefs = list(list(className = 'dt-center', targets = c(0,3))),
                      language = list(zeroRecords = "No Items Selected for Order"),
                      headerCallback = JS(headerCallback2)
                      )
    ) %>%
      formatStyle(columns = c(1), fontSize = '150%', color = 'white', fontWeight = 'bold', backgroundColor = '#AF601A') %>%
      formatStyle(columns = c(2), fontSize = '150%', color = 'white', fontWeight = 'bold', backgroundColor = '#AF601A') %>%
      formatStyle(columns = c(3), fontSize = '150%', color = 'white', fontWeight = 'bold', backgroundColor = '#AF601A') %>%
      formatStyle(columns = c(4), fontSize = '150%', color = 'white', fontWeight = 'bold', backgroundColor = '#AF601A')
  })
  
  headerCallback2 <- c(
    "function(thead, data, start, end, display){",
    "  $('th', thead).css('border-top', '2px solid #935116');",
    "  $('th', thead).css('border-bottom', '2px solid #935116');",
    "  $('th', thead).css('color', '#935116');",
    "}"
  )
  
  output$order_total_message <- renderText({
    input$table_item_ref
    return(paste("Order Price = $",format(round(order_unit_price()*input$accept_order_quantity,2),nsmall=2),sep=""))
    
  })
    
  output$currency_description <- renderText({
    input$selected_currency
    x <- as.character(currlist[currlist$Currency.Symbol == input$selected_currency,]$Currency.Name)
    return(x)
  })
  
  
  
  output$ListData = DT::renderDataTable({
    if (show_welcome()) {
      shinyalert("Welcome", "This containerized application is built with APIs from Mainframe CICS Application", type = "success", confirmButtonCol = "#3F27B3")
      show_welcome(FALSE)
    }
    showModal(modalDialog(
      title = "Please wait...",
      h4("Building the Item List"),
      easyClose = FALSE,
      footer = NULL
    ))
    z <- ReappData$c
    urlname <- paste("http://192.86.33.143:9080/catalogManager/items?startItemId=",input$accept_item_ref,sep="")
    itemrefdata <- fromJSON(urlname)
    d1 <- as.data.frame(itemrefdata[[1]][[2]][[2]])
    d1 <- d1[,c(2,5,3)]
    d1 <- d1[d1$CA_ITEM_REF != 0,]
    
    if (nrow(d1) > 7) {
      d1 <- d1[1:7,]
    }
    removeModal()
    # if(d1[1,1] != input$accept_item_ref){
    #   stop("Record Not Found")
    # }
    # 
    DT::datatable(
      d1, class = 'cell-border stripe', rownames = FALSE,selection = "single",
      callback=DT::JS("table.on('click.dt', 'tr', function() {
                      var data=table.row(this).data();
                      Shiny.onInputChange('table_item_ref',data[0]);
      });"), 
      extensions = list(FixedColumns = list(leftColumns = 1)), 
      options = list (pageLength = NROW(d1),dom = 't',
                      headerCallback = JS(headerCallback1),
                      columnDefs = list(list(className = 'dt-center', targets = c(0,2)))
                      )
      )%>%
        #formatStyle(columns = c(1, 5), fontSize = '150%', fontWeight = 'bold') %>%
        #formatStyle(columns = c(1), target = 'row', fontSize = '100%', color = 'white', fontWeight = 'bold', backgroundColor = 'darkred') %>%
        formatStyle(columns = c(1), fontSize = '110%', color = 'white', fontWeight = 'bold', backgroundColor = 'darkblue') %>%
        formatStyle(columns = c(2), fontSize = '110%', color = 'white', fontWeight = 'bold', backgroundColor = 'darkblue') %>%
        formatStyle(columns = c(3), fontSize = '110%', color = 'darkblue', fontWeight = 'bold') 
        #formatStyle(columns = c(4), fontSize = '110%', color = 'darkblue', fontWeight = 'bold') %>%
        #formatStyle(columns = c(5), fontSize = '110%', color = 'darkblue', fontWeight = 'bold') %>%
        #formatStyle(columns = c(6), fontSize = '110%', color = 'darkblue', fontWeight = 'bold') 
  })
  
  observeEvent(input$AcceptChanges, {
    ReappData$c <<- ""
    ReappData$d <<- ""
    x <- FALSE
    y <- TRUE
    orderitemlselectionlist[,4] <<- stock_Verified_order_table$Fulfillable
    orderitemlselectionlist <<- orderitemlselectionlist[orderitemlselectionlist[,4] > 0,]
    if (additemstolist()) {
      additemstolist(FALSE)
    }
    else {
      additemstolist(TRUE) 
    }
    
    if (NROW(orderitemlselectionlist) > 0) {
      #res <- POST("http://zmicroservices-debahmuk-build-app.apps.ocp-na1.prod.nextcle.com/validate?"
      res <- POST(paste(basemicroserviceurl,"validate?",sep="")
                  , body = paste("l1=",paste(as.vector(t(orderitemlselectionlist[,c(1,4)])),collapse=" "),"\\&","mycurrency=",input$selected_currency,sep=""),
                  , encode = "json")
      appData <- content(res)
      stock_Verified_order_table <<- as.data.frame(matrix(unlist(appData[[1]]), ncol=3, byrow=TRUE), stringsAsFactors=FALSE)
      
      
      names(stock_Verified_order_table) <<- c("ItemRef", "Ordered", "Fulfillable")
      stock_Verified_order_table$ItemRef <<- as.character(stock_Verified_order_table$ItemRef)
      stock_verified_message <<- unlist(appData[[3]])
      stock_verified_price <<- unlist(appData[[2]])
      if (stock_verfied_reactive()) {
        stock_verfied_reactive(FALSE)
      }
      else {
        stock_verfied_reactive(TRUE) 
      }
    }    
    
  })
  
  observeEvent(input$PlaceOrder, {
    dxx <- orderitemlselectionlist[orderitemlselectionlist$ItemRef==input$accept_item_ref,]
    if (NROW(dxx) == 0) {
      message <- "Enter Order Quantity for"
      rowpos <- NROW(orderitemlselectionlist) + 1
      urlname <- paste("http://192.86.33.143:9080/catalogManager/items/",input$accept_item_ref,sep="")
      itemrefdata <- fromJSON(urlname)
      d1 <- as.data.frame(itemrefdata[[1]][[2]][[1]],stringsAsFactors=F)
      if(d1[1,1] != input$accept_item_ref){
        stop("Record Not Found")
      }
      x <- as.numeric(d1[1,5])
      order_unit_price(x)
      d1 <- d1[,c(1,2,5)]
      itemname <- d1[1,2]
      orderitemlselectionlist[rowpos,]$ItemRef <<- d1[1,1]
      orderitemlselectionlist[rowpos,]$ItemDesc <<- d1[1,2]
      orderitemlselectionlist[rowpos,]$ItemCost <<- d1[1,3]
      orderitemlselectionlist[rowpos,]$ItemQty <<- 1
    }
    else {
      message <- "Update Order Quantity for"
      itemname <- dxx[1,2]
    }
    enable("SendOrder")
    shinyalert(
      message,text=itemname, type = "input",inputType="number",confirmButtonCol = "#3F27B3",
      closeOnEsc = TRUE,closeOnClickOutside = FALSE,
      callbackR = mycallbackQuantity
    )
  })

  observeEvent(input$CreateNewOrder, {
    enable("RetrieveAccount")
    x <- FALSE
    y <- TRUE
    orderitemlselectionlist <<- orderitemlselectionlist[0,]
    ReactiveOrderToPayment(x)
    ReactiveOrderPlaced(x)
    ReactivePaymentMade(x)
    # OTPIN <<- ""
    # OTPGenerated <<- FALSE
    # OTPValidated <<- FALSE
    updateActionButton(session, "RetrieveAccount", label = "Enter OTP")
    if (additemstolist()) {
      additemstolist(FALSE)
    }
    else {
      additemstolist(TRUE) 
    }
    create_new_order_reactive(TRUE)
    #disable("SendOrder")
    updateTabItems(session, "tabs", "dashboard2")
    session$reload()
  })
  
    
  observeEvent(input$CancelButton, {
    enable("RetrieveAccount")
    x <- FALSE
    y <- TRUE
    orderitemlselectionlist <<- orderitemlselectionlist[0,]
    ReactiveOrderToPayment(x)
    ReactiveOrderPlaced(x)
    ReactivePaymentMade(x)
    OTPIN <<- ""
    OTPGenerated <<- FALSE
    OTPValidated <<- FALSE
    updateActionButton(session, "RetrieveAccount", label = "Enter OTP")
    if (additemstolist()) {
      additemstolist(FALSE)
    }
    else {
      additemstolist(TRUE) 
    }
    #disable("SendOrder")
    updateTabItems(session, "tabs", "dashboard2")
  })
  
  observeEvent(input$CancelButton2, {
    enable("RetrieveAccount")
    x <- FALSE
    y <- TRUE
    orderitemlselectionlist <<- orderitemlselectionlist[0,]
    ReactiveOrderToPayment(x)
    ReactiveOrderPlaced(x)
    ReactivePaymentMade(x)
    #OTPIN <<- ""
    #OTPGenerated <<- FALSE
    #OTPValidated <<- FALSE
    updateActionButton(session, "RetrieveAccount", label = "Enter OTP")
    if (additemstolist()) {
      additemstolist(FALSE)
    }
    else {
      additemstolist(TRUE) 
    }
    cancel_order_reactive(TRUE)
    #create_new_order_reactive(TRUE)
    #disable("SendOrder")
    updateTabItems(session, "tabs", "dashboard2")
  })
  
  headerCallback1 <- c(
      "function(thead, data, start, end, display){",
    "  $('th', thead).css('border-top', '2px solid darkblue');",
    "  $('th', thead).css('border-bottom', '2px solid darkblue');",
    "  $('th', thead).css('color', 'darkblue');",
    "}"
  )
  
  observeEvent(input$SendOrder, {
    ReappData$c <<- ""
    ReappData$d <<- ""
    x <- FALSE
    y <- TRUE
    #shami
    create_new_order_reactive(FALSE)
    cancel_order_reactive(FALSE)
    if (NROW(orderitemlselectionlist) > 0) {
      #res <- POST("http://zmicroservices-debahmuk-build-app.apps.ocp-na1.prod.nextcle.com/validate?"
      res <- POST(paste(basemicroserviceurl,"validate?",sep="")
                  , body = paste("l1=",paste(as.vector(t(orderitemlselectionlist[,c(1,4)])),collapse=" "),"\\&","mycurrency=",input$selected_currency,sep=""),
                  , encode = "json")
      appData <- content(res)
      stock_Verified_order_table <<- as.data.frame(matrix(unlist(appData[[1]]), ncol=3, byrow=TRUE), stringsAsFactors=FALSE)
      
      
      names(stock_Verified_order_table) <<- c("ItemRef", "Ordered", "Fulfillable")
      stock_Verified_order_table$ItemRef <<- as.character(stock_Verified_order_table$ItemRef)
      stock_verified_message <<- unlist(appData[[3]])
      stock_verified_price <<- unlist(appData[[2]])
    } 
    enable("SendOrder")
    enable("AcceptChanges")
    if(sum(stock_Verified_order_table$Fulfillable) == 0) {
      #stock_Verified_order_table <<- left_join(stock_Verified_order_table,orderitemlselectionlist)[,c(1,4,2,3)]
      enable("RetrieveAccount")
      x <- FALSE
      y <- TRUE
      #orderitemlselectionlist <<- orderitemlselectionlist[0,]
      ReactiveOrderToPayment(x)
      ReactiveOrderPlaced(x)
      ReactivePaymentMade(x)
      OTPIN <<- ""
      OTPGenerated <<- FALSE
      OTPValidated <<- FALSE
      updateActionButton(session, "RetrieveAccount", label = "Enter OTP")
      if (additemstolist()) {
        additemstolist(FALSE)
      }
      else {
        additemstolist(TRUE) 
      }
      #disable("SendOrder")
      disable("AcceptChanges")
      shinyalert("Error", "Order Items are out of stock", type = "error",confirmButtonCol = "#E74C3C")
      #updateTabItems(session, "tabs", "dashboard2")
    }
    
    if (stock_verfied_reactive()) {
      stock_verfied_reactive(FALSE)
    }
    else {
      stock_verfied_reactive(TRUE) 
    }
    
    if (!ReactiveOrderPlaced()) {
      ReactiveOrderPlaced(y)
      ReactiveOrderToPayment(y)
      updateTabItems(session, "tabs", "dashboard1")
    }
    
    # #ReactivePaymentToOrder(x)
    # if (ReactivePaymentMade()) {
    #   pc_json <- list(
    #     DFH0XCP1 = list(
    #       orderRequest = list(
    #         itemId = input$accept_item_ref,
    #         orderQuantity = input$accept_order_quantity
    #       )
    #     )
    #   )
    #   
    #   res <- POST("http://192.86.33.143:9080/catalogManager/orders"
    #               , body = pc_json
    #               , encode = "json")
    #   
    #   appData <- content(res)
    #   
    #   ReappData$c <<- as.character(as.data.frame(appData)[,1])
    #   x <- FALSE
    #   ReactivePaymentMade(y)
    # }
    # 
    # # if(ReappData$a == "Record was added"){
    # #   ReappData$b <<- "Success"
    # # }

  })

  output$order_message <- renderText({

    if(input$SendOrder == 0){
      return("")
    }
    else {
      if (ReappData$c != "" ) {
        shinyalert("Order", ReappData$c, type = "info",confirmButtonCol = "#3F27B3")
        if (ReappData$c != "ORDER SUCCESSFULLY PLACED") {
          urlname <- paste("http://192.86.33.143:9080/jkebanking/accno/",str_pad(input$accept_account_ref,6,pad="0"),sep="")
          accountdata <- fromJSON(urlname)
          if(accountdata[[1]][[1]][[1]][[2]] != 0){
            shinyalert("Error", "Reversal Account Not found", type = "error",confirmButtonCol = "#E74C3C")
          }
          else {
            dx2 <<- as.data.frame(accountdata[[1]][[2]][[1]],stringsAsFactors = F)
          }
          add_date <- paste(as.integer(format(Sys.time(), "%d")),as.integer(format(Sys.time(), "%m")),as.integer(format(Sys.time(), "%y")))
          acountbalance <- as.numeric(gsub("\\$","",dx2$AMOUNT)) + order_unit_price()*input$accept_order_quantity
          pc_json <- list(
            DFHCOMMAREA = list(
              FILEA = list(
                FILEREC = list(
                  STAT = "U",
                  NUMB = dx2$NUMB,
                  NAME = dx2$NAME,
                  ADDRX = dx2$ADDRX,
                  PHONE = dx2$PHONE,
                  DATEX = add_date,
                  AMOUNT = paste("$",acountbalance,sep=""),
                  COMMENT = "Refund"
                )
              ),
              COMM_AREA = list(
                FILEREC = list(
                  STAT = dx2$STAT,
                  NUMB = dx2$NUMB,
                  NAME = dx2$NAME,
                  ADDRX = dx2$ADDRX,
                  PHONE = dx2$PHONE,
                  DATEX = dx2$DATEX,
                  AMOUNT = dx2$AMOUNT,
                  COMMENT = dx2$COMMENT
                )
              )
            )
          )
          
          res <- PUT(paste("http://192.86.33.143:9080/jkebanking/accno/",dx2$NUMB,sep="")
                     , body = pc_json
                     , encode = "json")
          
          appData <- content(res)
          if (appData[[1]][[1]][[1]][[2]] != 0) {
            shinyalert("Error", "Payment Reversal Unsuccessful", type = "error",confirmButtonCol = "#E74C3C")
          }
          else {
            shinyalert("Success", "Payment Reversed", type = "success",confirmButtonCol = "#54BA60")
          }
          ReappData$c <<- ""
        }
        
        x <- FALSE
        y <- TRUE
        ReactiveOrderPlaced(x)
        ReactiveOrderToPayment(x)
        ReactivePaymentMade(x)
        OTPIN <<- ""
        OTPGenerated <<- FALSE
        OTPValidated <<- FALSE
        enable("RetrieveAccount")
        enable("accept_order_quantity")
        enable("select_item_ref")
        enable("PlaceOrder")
        #ReactiveItemSelect(y)
        updateActionButton(session, "SendOrder", label = "Proceed to Pay")
        return(ReappData$c)
      }
      else {
        return("")
      }
    }

  })
  
  
  output$GenerateInvoice <- downloadHandler(
    filename = "report2.html",
    content = function(file) {
      tempReport <- file.path(tempdir(), "PrintInvoice.Rmd")
      file.copy("PrintInvoice.Rmd", tempReport, overwrite = TRUE)
      params <- list(df1=order_status,df2=PaymentTable,df3=Additionalinfo)
      rmarkdown::render(tempReport, output_file = file, params = params, envir = new.env(parent = globalenv()))
    })
  
    
})

shinyApp(ui = ui, server = server)