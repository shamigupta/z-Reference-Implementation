library(shinydashboard)
library(curl)
library(httr)
library(jsonlite)
library(DT)
library(shinyalert)
library(stringr)
library(shinyjs)
library(dplyr)

ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(
    title = "GenApps - Policy Admin", 
    #style="font-style: bold; align : top;"),
    titleWidth = 2250
    
  ),
  dashboardSidebar(
    HTML('<center><img src="GenApps.jpg" width="220"></center>'),
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
    tags$hr(),
    sidebarMenu(id="tabs",
        menuItem(strong("Customer"), tabName = "dashboard1", icon = icon("th"),badgeColor = "green"),
        menuItem(strong("Policy"), tabName = "dashboard2", icon = icon("th"),badgeColor = "green"),
        menuItem(strong("Claim"), tabName = "dashboard3", icon = icon("th"),badgeColor = "green")
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
    HTML('<center><font size="2"><b>This portal is a containerized shiny R application that consumes APIs from Mainframe CICS and integrates with other APIs. The CICS <a href="https://www.ibm.com/support/knowledgecenter/SSGMCP_5.2.0/com.ibm.cics.ts.samples.doc/topics/genapp.html" target=_blank>GenApps</a> application is a sample Insurance application.</b></font><width="150"></center>'),
    br(),
    HTML('<center><font size="2"><b>For more info, contact</b></font><width="150"></center>'),
    HTML('<center><font size="2"><b><a href="mailto:shami.gupta@in.ibm.com">Shami Gupta</a></b></font><width="150"></center>'),
    br(),
    actionButton("ViewArchitecture", label = "View Architecture",width='80%',style="font-size: large; font-weight: bold; color: yellow; background-color: darkblue")
  ),
  dashboardBody(
    titlePanel(tags$head(tags$link(rel = "icon", type = "image/png", href = "favicon.ico"),
                         tags$title("GenApps"))),
    
    tags$img(
      src = "GenApps2.jpg",
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
    tags$head( 
      tags$style(HTML(".main-sidebar { font-size: 16px; }")) #change the font size to 20
    ),
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
            box (
              title = span(icon("users","fa-x"),strong("Select Customer")), status = "warning", width = 12,solidHeader = TRUE,collapsible=FALSE,  
              fluidRow(
                column(2,numericInput("accept_customernumber", h4(strong("Customer Number"), style="font-style: bold; color: #935116"),value = 0)),
                column(1,textOutput("add_or_update_message")),
                column(2,offset = 3,textInput("accept_lastname", h4(strong("Last Name"), style="font-style: bold; color: #935116"),value = "")),
                column(2,textInput("accept_firstname", h4(strong("First Name"), style="font-style: bold; color: #935116"),value = "")),
                column(1, offset = 1,actionButton("refreshCustomer", label = h5("Refresh",style="font-weight: bold; color: yellow"),width='100%',style="background-color: #935116"))
              ),
              fluidRow(
                column(12,h6(DT::dataTableOutput("show_customer_details")))
              ),
              fluidRow(
                column(2, actionButton("addCustomer", label = h5("Add Customer",style="font-weight: bold; color: yellow"),width='100%',style="background-color: #935116"))
              )
            ),
            box(
              title = span(icon("user-tie","fa-x"),strong("Customer Details")), status = "warning", width = 12,solidHeader = TRUE,collapsible=FALSE,  
              conditionalPanel(
                condition = "output.add_or_update_message == 'U'",
                fluidRow(
                  column(3,h4(strong("Customer Number"), style="font-style: bold; color: #935116"),h4(strong(textOutput("display_customer_number")), style="font-weight: bold; color: #935116; margin-top:1px")),
                  column(3,h4(strong("First Name"), style="font-style: bold; color: #935116"),h4(strong(textOutput("display_first_name")), style="font-weight: bold; color: #935116; margin-top:1px")),
                  column(3,h4(strong("Last Name"), style="font-style: bold; color: #935116"),h4(strong(textOutput("display_last_name")), style="font-weight: bold; color: #935116; margin-top:1px")),
                  column(3,h4(strong("Date of Birth"), style="font-style: bold; color: #935116"),h4(strong(textOutput("display_dob")), style="font-weight: bold; color: #935116; margin-top:1px"))
                )
              ),
              conditionalPanel(
                condition = "output.add_or_update_message == 'A'",
                fluidRow(
                  column(3,textInput("accept_firstname_add", h4(strong("First Name"), style="font-style: bold; color: #935116"),value = "")),
                  column(3,textInput("accept_lastname_add", h4(strong("Last Name"), style="font-style: bold; color: #935116"),value = "")),
                  column(3,dateInput("accept_dob_add", h4(strong("Date of Birth"), style="font-style: bold; color: #935116"),format="yyyy-mm-dd",value = ""))
                )
              ),
            
            column(width = 9,
                conditionalPanel(
                  condition = "output.add_or_update_message == 'U'",
                  fluidRow(
                    column(4,uiOutput("update_housename")),
                    column(4,uiOutput("update_housenumber")),
                    column(4,uiOutput("update_postcode"))
                  ),
                  fluidRow(
                    column(4,uiOutput("update_jkeaccount")),
                    column(4,uiOutput("update_mobile")),
                    column(4,uiOutput("update_email"))
                  )
                ),
                conditionalPanel(
                  condition = "output.add_or_update_message == 'A'",
                  fluidRow(
                    column(4,textInput("accept_housenameadd", h4(strong("House Name"), style="font-style: bold; color: #935116"),value = "")),
                    column(4,textInput("accept_housenumberadd", h4(strong("House Number"), style="font-style: bold; color: #935116"),value = "")),
                    column(4,textInput("accept_postcodeadd", h4(strong("Post Code"), style="font-style: bold; color: #935116"),value = ""))
                  ),
                  fluidRow(
                    column(4,numericInput("accept_jkeaccountadd", h4(strong("JKE Bank Account"), style="font-style: bold; color: #935116"),value = 0)),
                    column(4,textInput("accept_mobileadd", h4(strong("Mobile Number"), style="font-style: bold; color: #935116"),value = "")),
                    column(4,textInput("accept_emailadd", h4(strong("email Id"), style="font-style: bold; color: #935116"),value = ""))
                  )
                ),
              conditionalPanel(
                condition = "output.add_or_update_message == 'U'",
                  fluidRow(
                    column(2, actionButton("updateCustomer", label = h5("Update",style="font-weight: bold; color: yellow"),width='100%',style="background-color: #935116"))
                  ),
                  fluidRow(
                    column(6,textOutput("update_message"))
                  )
              ),
              conditionalPanel(
                condition = "output.add_or_update_message == 'A'",
                fluidRow(
                  column(2, actionButton("addCustomeradd", label = h5("Add",style="font-weight: bold; color: yellow"),width='100%',style="background-color: #935116"))
                ),
                fluidRow(
                  column(6,textOutput("add_message"))
                )
              )
            ),
            conditionalPanel(
              condition = "output.add_or_update_message == 'U'",
              column(width = 3,
                column(12,h6(DT::dataTableOutput("show_customer_policy")))
              )
            )
          )
          
    ),
    tabItem(tabName = "dashboard2",
             box (
              title = span(icon("folder-open","fa-x"),strong("Select Policy")), status = "primary", width = 12,solidHeader = TRUE,collapsible=FALSE,
              fluidRow(
                column(2,uiOutput("select_customernumber_policy")),
                column(3,uiOutput("select_insurance_type")),
                column(2,numericInput("accept_policy_number", h4(strong("Policy Number"), style="font-style: bold; color: darkblue"),value = 0)),
                column(2,textOutput("policy_type_message")),
                column(1,offset=2,actionButton("refreshPolicy", label = h5("Refresh",style="font-weight: bold; color: yellow"),width='100%',style="background-color: darkblue"))
              ),
              fluidRow(
                column(12,h6(DT::dataTableOutput("show_policy_details")))
              )
            ),
            conditionalPanel(
              condition = "output.policy_type_message == 'MR'",
              box(
                title = span(icon("car","fa-x"),strong("Motor Policy Details")), status = "primary", width = 12,solidHeader = TRUE,collapsible=FALSE,  
                fluidRow(
                  column(2,h4(strong("Customer Number"), style="font-style: bold; color: darkblue"),h5(strong(textOutput("display_customer_number_mr")), style="font-weight: bold; color: darkblue; margin-top:1px")),
                  column(2,h4(strong("Policy Number"), style="font-style: bold; color: darkblue"),h5(strong(textOutput("display_policy_number_mr")), style="font-weight: bold; color: darkblue; margin-top:1px")),
                  column(2,h4(strong("Policy Type"), style="font-style: bold; color: darkblue"),h5(strong(textOutput("display_policy_type_mr")), style="font-weight: bold; color: darkblue; margin-top:1px")),
                  column(2,h4(strong("Start Date"), style="font-style: bold; color: darkblue"),h5(strong(textOutput("display_policy_start_date_mr")), style="font-weight: bold; color: darkblue; margin-top:1px")),
                  column(2,h4(strong("Expiry Date"), style="font-style: bold; color: darkblue"),h5(strong(textOutput("display_policy_expiry_date_mr")), style="font-weight: bold; color: darkblue; margin-top:1px"))
                ),
                column(width = 9,
                  fluidRow(
                    column(4,h4(strong("Make"), style="font-style: bold; color: darkblue"),h5(strong(textOutput("display_make_mr")), style="font-weight: bold; color: darkblue; margin-top:1px")),
                    column(4,h4(strong("Model"), style="font-style: bold; color: darkblue"),h5(strong(textOutput("display_model_mr")), style="font-weight: bold; color: darkblue; margin-top:1px")),
                    column(4,h4(strong("Value in US$"), style="font-style: bold; color: darkblue"),h5(strong(textOutput("display_value_mr")), style="font-weight: bold; color: darkblue; margin-top:1px"))
                  ),
                  fluidRow(
                    column(4,h4(strong("Registration No"), style="font-style: bold; color: darkblue"),h5(strong(textOutput("display_registration_mr")), style="font-weight: bold; color: darkblue; margin-top:1px")),
                    column(4,h4(strong("Color"), style="font-style: bold; color: darkblue"),h5(strong(textOutput("display_color_mr")), style="font-weight: bold; color: darkblue; margin-top:1px")),
                    column(4,h4(strong("Engine Power in CC"), style="font-style: bold; color: darkblue"),h5(strong(textOutput("display_power_mr")), style="font-weight: bold; color: darkblue; margin-top:1px"))
                  ),
                  fluidRow(
                    column(4,h4(strong("No of Accidents"), style="font-style: bold; color: darkblue"),h5(strong(textOutput("display_numofaccidents_mr")), style="font-weight: bold; color: darkblue; margin-top:1px")),
                    column(4,h4(strong("Yearly Premium in US$"), style="font-style: bold; color: darkblue"),h5(strong(textOutput("display_premium_mr")), style="font-weight: bold; color: darkblue; margin-top:1px")),
                    column(4,h4(strong("Date Manufactured"), style="font-style: bold; color: darkblue"),h5(strong(textOutput("display_mfg_date_mr")), style="font-weight: bold; color: darkblue; margin-top:1px"))
                  ),
                  fluidRow(
                    column(2, actionButton("update_policy_request_mr", label = h5("Update Policy",style="font-weight: bold; color: yellow"),width='100%',style="background-color: darkblue")),
                    column(2, actionButton("delete_policy_request_mr", label = h5("Delete Policy",style="font-weight: bold; color: yellow"),width='100%',style="background-color: darkred"))
                  ),
                  fluidRow(
                    column(6,textOutput("policy_message_request_mr"))
                  )
                ),
                column(width = 3,
                  column(12,h6(DT::dataTableOutput("show_claim_details_mr")))
                )
              )
            ),
            conditionalPanel(
              condition = "output.policy_type_message == 'MU'",
              box(
                title = span(icon("car","fa-x"),strong("Motor Policy Details")), status = "primary", width = 12,solidHeader = TRUE,collapsible=FALSE,  
                fluidRow(
                  column(2,h4(strong("Customer Number"), style="font-style: bold; color: darkblue"),h5(strong(textOutput("display_customer_number_mu")), style="font-weight: bold; color: darkblue; margin-top:1px")),
                  column(2,h4(strong("Policy Number"), style="font-style: bold; color: darkblue"),h5(strong(textOutput("display_policy_number_mu")), style="font-weight: bold; color: darkblue; margin-top:1px")),
                  column(2,h4(strong("Policy Type"), style="font-style: bold; color: darkblue"),h5(strong(textOutput("display_policy_type_mu")), style="font-weight: bold; color: darkblue; margin-top:1px")),
                  column(2,h4(strong("Start Date"), style="font-style: bold; color: darkblue"),h5(strong(textOutput("display_policy_start_date_mu")), style="font-weight: bold; color: darkblue; margin-top:1px")),
                  column(2,uiOutput("update_policy_expiry_date_mu"))
                ),
                fluidRow(
                  column(4,uiOutput("update_make_mu")),
                  column(4,uiOutput("update_model_mu")),
                  column(4,uiOutput("update_value_mu"))
                ),
                fluidRow(
                  column(4,uiOutput("update_registration_mu")),
                  column(4,uiOutput("update_color_mu")),
                  column(4,uiOutput("update_power_mu"))
                ),
                fluidRow(
                  column(4,uiOutput("update_numofaccidents_mu")),
                  column(4,uiOutput("update_premium_mu")),
                  column(4,uiOutput("update_mfg_date_mu"))
                ),
                fluidRow(
                  column(2, actionButton("save_policy_request_mu", label = h5("Save",style="font-weight: bold; color: yellow"),width='100%',style="background-color: darkblue"))
                ),
                fluidRow(
                  column(6,textOutput("policy_save_request_mu"))
                )
              )
            ),
            conditionalPanel(
              condition = "output.policy_type_message == 'ER'",
              box(
                title = span(icon("funnel-dollar","fa-x"),strong("Endowment Policy Details")), status = "primary", width = 12,solidHeader = TRUE,collapsible=FALSE,  
                fluidRow(
                  column(2,h4(strong("Customer Number"), style="font-style: bold; color: darkblue"),h5(strong(textOutput("display_customer_number_er")), style="font-weight: bold; color: darkblue; margin-top:1px")),
                  column(2,h4(strong("Policy Number"), style="font-style: bold; color: darkblue"),h5(strong(textOutput("display_policy_number_er")), style="font-weight: bold; color: darkblue; margin-top:1px")),
                  column(2,h4(strong("Policy Type"), style="font-style: bold; color: darkblue"),h5(strong(textOutput("display_policy_type_er")), style="font-weight: bold; color: darkblue; margin-top:1px")),
                  column(2,h4(strong("Start Date"), style="font-style: bold; color: darkblue"),h5(strong(textOutput("display_policy_start_date_er")), style="font-weight: bold; color: darkblue; margin-top:1px")),
                  column(2,h4(strong("Expiry Date"), style="font-style: bold; color: darkblue"),h5(strong(textOutput("display_policy_expiry_date_er")), style="font-weight: bold; color: darkblue; margin-top:1px"))
                ),
                column(width = 9,
                       fluidRow(
                         column(3,h4(strong("Fund Name"), style="font-style: bold; color: darkblue"),h5(strong(textOutput("display_fundname_er")), style="font-weight: bold; color: darkblue; margin-top:1px")),
                         column(3,h4(strong("Sum Assured US$"), style="font-style: bold; color: darkblue"),h5(strong(textOutput("display_sumassured_er")), style="font-weight: bold; color: darkblue; margin-top:1px")),
                         column(3,h4(strong("Name of Life Assured"), style="font-style: bold; color: darkblue"),h5(strong(textOutput("display_nameoflifeassured_er")), style="font-weight: bold; color: darkblue; margin-top:1px"))
                       ),
                       fluidRow(
                         column(3,h4(strong("Term"), style="font-style: bold; color: darkblue"),h5(strong(textOutput("display_term_er")), style="font-weight: bold; color: darkblue; margin-top:1px")),
                         column(3,h4(strong("With Profit"), style="font-style: bold; color: darkblue"),h5(strong(textOutput("display_wprofit_er")), style="font-weight: bold; color: darkblue; margin-top:1px")),
                         column(3,h4(strong("With Equities"), style="font-style: bold; color: darkblue"),h5(strong(textOutput("display_wequities_er")), style="font-weight: bold; color: darkblue; margin-top:1px")),
                         column(3,h4(strong("Managed Fund"), style="font-style: bold; color: darkblue"),h5(strong(textOutput("display_managedfund_er")), style="font-weight: bold; color: darkblue; margin-top:1px"))
                       ),
                       fluidRow(
                         column(2, actionButton("update_policy_request_er", label = h5("Update Policy",style="font-weight: bold; color: yellow"),width='100%',style="background-color: darkblue")),
                         column(2, actionButton("delete_policy_request_er", label = h5("Delete Policy",style="font-weight: bold; color: yellow"),width='100%',style="background-color: darkred"))
                       ),
                       fluidRow(
                         column(6,textOutput("policy_message_request_er"))
                       )
                ),
                column(width = 3,
                       column(12,h6(DT::dataTableOutput("show_claim_details_er")))
                )
              )
            ),
            conditionalPanel(
              condition = "output.policy_type_message == 'EU'",
              box(
                title = span(icon("funnel-dollar","fa-x"),strong("Endowment Policy Details")), status = "primary", width = 12,solidHeader = TRUE,collapsible=FALSE,  
                fluidRow(
                  column(2,h4(strong("Customer Number"), style="font-style: bold; color: darkblue"),h5(strong(textOutput("display_customer_number_eu")), style="font-weight: bold; color: darkblue; margin-top:1px")),
                  column(2,h4(strong("Policy Number"), style="font-style: bold; color: darkblue"),h5(strong(textOutput("display_policy_number_eu")), style="font-weight: bold; color: darkblue; margin-top:1px")),
                  column(2,h4(strong("Policy Type"), style="font-style: bold; color: darkblue"),h5(strong(textOutput("display_policy_type_eu")), style="font-weight: bold; color: darkblue; margin-top:1px")),
                  column(2,h4(strong("Start Date"), style="font-style: bold; color: darkblue"),h5(strong(textOutput("display_policy_start_date_eu")), style="font-weight: bold; color: darkblue; margin-top:1px")),
                  column(2,uiOutput("update_policy_expiry_date_eu"))
                ),
                fluidRow(
                  column(2,uiOutput("update_fundname_eu")),
                  column(2,uiOutput("update_sumassured_eu")),
                  column(2,uiOutput("update_nameoflifeassured_eu"))
                ),
                fluidRow(
                  column(2,uiOutput("update_term_eu")),
                  column(2,uiOutput("update_wprofit_eu")),
                  column(2,uiOutput("update_wequities_eu")),
                  column(2,uiOutput("update_managedfund_eu"))
                ),
                fluidRow(
                  column(2, actionButton("save_policy_request_eu", label = h5("Save",style="font-weight: bold; color: yellow"),width='100%',style="background-color: darkblue"))
                ),
                fluidRow(
                  column(6,textOutput("policy_save_request_eu"))
                )
              )
            ),
            conditionalPanel(
              condition = "output.policy_type_message == 'HR'",
              box(
                title = span(icon("home","fa-x"),strong("House Policy Details")), status = "primary", width = 12,solidHeader = TRUE,collapsible=FALSE,  
                fluidRow(
                  column(2,h4(strong("Customer Number"), style="font-style: bold; color: darkblue"),h5(strong(textOutput("display_customer_number_hr")), style="font-weight: bold; color: darkblue; margin-top:1px")),
                  column(2,h4(strong("Policy Number"), style="font-style: bold; color: darkblue"),h5(strong(textOutput("display_policy_number_hr")), style="font-weight: bold; color: darkblue; margin-top:1px")),
                  column(2,h4(strong("Policy Type"), style="font-style: bold; color: darkblue"),h5(strong(textOutput("display_policy_type_hr")), style="font-weight: bold; color: darkblue; margin-top:1px")),
                  column(2,h4(strong("Start Date"), style="font-style: bold; color: darkblue"),h5(strong(textOutput("display_policy_start_date_hr")), style="font-weight: bold; color: darkblue; margin-top:1px")),
                  column(2,h4(strong("Expiry Date"), style="font-style: bold; color: darkblue"),h5(strong(textOutput("display_policy_expiry_date_hr")), style="font-weight: bold; color: darkblue; margin-top:1px"))
                ),
                column(width = 9,
                       fluidRow(
                         column(3,h4(strong("Property Type"), style="font-style: bold; color: darkblue"),h5(strong(textOutput("display_propertytype_hr")), style="font-weight: bold; color: darkblue; margin-top:1px")),
                         column(3,h4(strong("House Name"), style="font-style: bold; color: darkblue"),h5(strong(textOutput("display_housename_hr")), style="font-weight: bold; color: darkblue; margin-top:1px")),
                         column(3,h4(strong("House Number"), style="font-style: bold; color: darkblue"),h5(strong(textOutput("display_housenumber_hr")), style="font-weight: bold; color: darkblue; margin-top:1px"))
                       ),
                       fluidRow(
                         column(3,h4(strong("Post Code"), style="font-style: bold; color: darkblue"),h5(strong(textOutput("display_postcode_hr")), style="font-weight: bold; color: darkblue; margin-top:1px")),
                         column(3,h4(strong("Number of Bedrooms"), style="font-style: bold; color: darkblue"),h5(strong(textOutput("display_numofbedrooms_hr")), style="font-weight: bold; color: darkblue; margin-top:1px")),
                         column(3,h4(strong("Property Value"), style="font-style: bold; color: darkblue"),h5(strong(textOutput("display_propertyvalue_hr")), style="font-weight: bold; color: darkblue; margin-top:1px"))
                       ),
                       fluidRow(
                         column(2, actionButton("update_policy_request_hr", label = h5("Update Policy",style="font-weight: bold; color: yellow"),width='100%',style="background-color: darkblue")),
                         column(2, actionButton("delete_policy_request_hr", label = h5("Delete Policy",style="font-weight: bold; color: yellow"),width='100%',style="background-color: darkred"))
                       ),
                       fluidRow(
                         column(6,textOutput("policy_message_request_hr"))
                       )
                ),
                column(width = 3,
                       column(12,h6(DT::dataTableOutput("show_claim_details_hr")))
                )
              )
            ),
            conditionalPanel(
              condition = "output.policy_type_message == 'HU'",
              box(
                title = span(icon("home","fa-x"),strong("House Policy Details")), status = "primary", width = 12,solidHeader = TRUE,collapsible=FALSE,  
                fluidRow(
                  column(2,h4(strong("Customer Number"), style="font-style: bold; color: darkblue"),h5(strong(textOutput("display_customer_number_hu")), style="font-weight: bold; color: darkblue; margin-top:1px")),
                  column(2,h4(strong("Policy Number"), style="font-style: bold; color: darkblue"),h5(strong(textOutput("display_policy_number_hu")), style="font-weight: bold; color: darkblue; margin-top:1px")),
                  column(2,h4(strong("Policy Type"), style="font-style: bold; color: darkblue"),h5(strong(textOutput("display_policy_type_hu")), style="font-weight: bold; color: darkblue; margin-top:1px")),
                  column(2,h4(strong("Start Date"), style="font-style: bold; color: darkblue"),h5(strong(textOutput("display_policy_start_date_hu")), style="font-weight: bold; color: darkblue; margin-top:1px")),
                  column(2,uiOutput("update_policy_expiry_date_hu"))
                ),
                fluidRow(
                  column(3,uiOutput("update_propertytype_hu")),
                  column(3,uiOutput("update_housename_hu")),
                  column(3,uiOutput("update_housenumber_hu"))
                ),
                fluidRow(
                  column(3,uiOutput("update_postcode_hu")),
                  column(3,uiOutput("update_numofbedrooms_hu")),
                  column(3,uiOutput("update_propertyvalue_hu"))
                ),
                fluidRow(
                  column(4, actionButton("save_policy_request_hu", label = h4("Save",style="font-weight: bold; color: yellow"),width='100%',style="background-color: darkblue"))
                ),
                fluidRow(
                  column(6,textOutput("policy_save_request_hu"))
                )
              )
            ),
            conditionalPanel(
              condition = "output.policy_type_message == 'CR'",
              box(
                title = span(icon("landmark","fa-x"),strong("Commercial Policy Details")), status = "primary", width = 12,solidHeader = TRUE,collapsible=FALSE,  
                fluidRow(
                  column(2,h4(strong("Customer Number"), style="font-style: bold; color: darkblue"),h5(strong(textOutput("display_customer_number_cr")), style="font-weight: bold; color: darkblue; margin-top:1px")),
                  column(2,h4(strong("Policy Number"), style="font-style: bold; color: darkblue"),h5(strong(textOutput("display_policy_number_cr")), style="font-weight: bold; color: darkblue; margin-top:1px")),
                  column(2,h4(strong("Policy Type"), style="font-style: bold; color: darkblue"),h5(strong(textOutput("display_policy_type_cr")), style="font-weight: bold; color: darkblue; margin-top:1px")),
                  column(2,h4(strong("Start Date"), style="font-style: bold; color: darkblue"),h5(strong(textOutput("display_policy_start_date_cr")), style="font-weight: bold; color: darkblue; margin-top:1px")),
                  column(2,h4(strong("Expiry Date"), style="font-style: bold; color: darkblue"),h5(strong(textOutput("display_policy_expiry_date_cr")), style="font-weight: bold; color: darkblue; margin-top:1px"))
                ),
                column(width = 9,
                       fluidRow(
                         column(4,h4(strong("Customer Name"), style="font-style: bold; color: darkblue"),h5(strong(textOutput("display_CustomerName_cr")), style="font-weight: bold; color: darkblue; margin-top:1px")),
                         column(2,h4(strong("Property Type"), style="font-style: bold; color: darkblue"),h5(strong(textOutput("display_PropertyType_cr")), style="font-weight: bold; color: darkblue; margin-top:1px")),
                         column(2,offset=1,h4(strong("Fire Peril"), style="font-style: bold; color: darkblue"),h5(strong(textOutput("display_FirePeril_cr")), style="font-weight: bold; color: darkblue; margin-top:1px")),
                         column(2,h4(strong("Fire Premium"), style="font-style: bold; color: darkblue"),h5(strong(textOutput("display_FirePremium_cr")), style="font-weight: bold; color: darkblue; margin-top:1px"))
                       ),
                       fluidRow(
                         column(6,h4(strong("Property Address"), style="font-style: bold; color: darkblue"),h5(strong(textOutput("display_PropertyAddress_cr")), style="font-weight: bold; color: darkblue; margin-top:1px")),
                         column(2,offset=1,h4(strong("Crime Peril"), style="font-style: bold; color: darkblue"),h5(strong(textOutput("display_CrimePeril_cr")), style="font-weight: bold; color: darkblue; margin-top:1px")),
                         column(2,h4(strong("Crime Premium"), style="font-style: bold; color: darkblue"),h5(strong(textOutput("display_CrimePremium_cr")), style="font-weight: bold; color: darkblue; margin-top:1px"))
                       ),
                       fluidRow(
                         column(2,h4(strong("Post Code"), style="font-style: bold; color: darkblue"),h5(strong(textOutput("display_PostCode_cr")), style="font-weight: bold; color: darkblue; margin-top:1px")),
                         column(2,h4(strong("Status"), style="font-style: bold; color: darkblue"),h5(strong(textOutput("display_Status_cr")), style="font-weight: bold; color: darkblue; margin-top:1px")),
                         column(2,h4(strong("Reject Reason"), style="font-style: bold; color: darkblue"),h5(strong(textOutput("display_RejectReason_cr")), style="font-weight: bold; color: darkblue; margin-top:1px")),
                         column(2,offset=1,h4(strong("Flood Peril"), style="font-style: bold; color: darkblue"),h5(strong(textOutput("display_FloodPeril_cr")), style="font-weight: bold; color: darkblue; margin-top:1px")),
                         column(2,h4(strong("Flood Premium"), style="font-style: bold; color: darkblue"),h5(strong(textOutput("display_FloodPremium_cr")), style="font-weight: bold; color: darkblue; margin-top:1px"))
                       ),
                       fluidRow(
                         column(2,h4(strong("Location Lat"), style="font-style: bold; color: darkblue"),h5(strong(textOutput("display_LocationLat_cr")), style="font-weight: bold; color: darkblue; margin-top:1px")),
                         column(2,h4(strong("Location Long"), style="font-style: bold; color: darkblue"),h5(strong(textOutput("display_LocationLong_cr")), style="font-weight: bold; color: darkblue; margin-top:1px")),
                         column(2,offset=3,h4(strong("Weather Peril"), style="font-style: bold; color: darkblue"),h5(strong(textOutput("display_WeatherPeril_cr")), style="font-weight: bold; color: darkblue; margin-top:1px")),
                         column(2,h4(strong("Weather Premium"), style="font-style: bold; color: darkblue"),h5(strong(textOutput("display_WeatherPremium_cr")), style="font-weight: bold; color: darkblue; margin-top:1px"))
                       ),
                       fluidRow(
                         #column(2, actionButton("update_policy_request_cr", label = h5("Update Policy",style="font-weight: bold; color: yellow"),width='100%',style="background-color: darkblue")),
                         column(2, actionButton("delete_policy_request_cr", label = h5("Delete Policy",style="font-weight: bold; color: yellow"),width='100%',style="background-color: darkred"))
                       ),
                       fluidRow(
                         column(6,textOutput("policy_message_request_cr"))
                       )
                ),
                column(width = 3,
                       column(12,h6(DT::dataTableOutput("show_claim_details_cr")))
                )
              )
            )
    ),
    tabItem(tabName = "dashboard3",
            box (
              title = span(icon("folder-open","fa-x"),strong("Select Claim")), status = "success", width = 12,solidHeader = TRUE,collapsible=FALSE,
              fluidRow(
                column(3,uiOutput("select_claim_policy")),
                column(3,uiOutput("select_claim_status")),
                column(2,numericInput("accept_claim_number", h4(strong("Claim Number"), style="font-style: bold; color: darkgreen"),value = 0)),
                column(2,offset = 1,textOutput("policy_clicked_message")),
                column(1, actionButton("refreshClaim", label = h5("Refresh",style="font-weight: bold; color: yellow"),width='100%',style="background-color: darkgreen"))
              ),
              fluidRow(
                column(12,h6(DT::dataTableOutput("show_claim_details")))
              )
            ),
            conditionalPanel(
              condition = "output.policy_clicked_message == '.'",
                box(
                  title = span(icon("hand-holding-usd","fa-x"),strong("Claim Details")), status = "success", width = 12,solidHeader = TRUE,collapsible=FALSE,  
                  column(width = 9,
                         br(),
                         fluidRow(
                           column(4,h4(strong("Policy Number"), style="font-style: bold; color: darkgreen"),h5(strong(textOutput("display_policy_number_claim")), style="font-weight: bold; color: darkgreen; margin-top:1px")),
                           column(4,h4(strong("Claim Number"), style="font-style: bold; color: darkgreen"),h5(strong(textOutput("display_claim_number_claim")), style="font-weight: bold; color: darkgreen; margin-top:1px")),
                           column(4,h4(strong("Claim Date"), style="font-style: bold; color: darkgreen"),h5(strong(textOutput("display_claim_date_claim")), style="font-weight: bold; color: darkgreen; margin-top:1px"))
                         ),
                         br(),
                         br(),
                         fluidRow(
                           column(4,h4(strong("Cause"), style="font-style: bold; color: darkgreen"),h5(strong(textOutput("display_cause_claim")), style="font-weight: bold; color: darkgreen; margin-top:1px")),
                           column(4,h4(strong("Observation"), style="font-style: bold; color: darkgreen"),h5(strong(textOutput("display_observation_claim")), style="font-weight: bold; color: darkgreen; margin-top:1px"))
                         ),
                         br(),
                         br(),
                         fluidRow(
                           column(4,h4(strong("Claimed Amount"), style="font-style: bold; color: darkgreen"),h5(strong(textOutput("display_claimedamount_claim")), style="font-weight: bold; color: darkgreen; margin-top:1px")),
                           column(4,h4(strong("Settled Amount"), style="font-style: bold; color: darkgreen"),h5(strong(textOutput("display_settledamount_claim")), style="font-weight: bold; color: darkgreen; margin-top:1px")),
                           column(4,h4(strong("Status"), style="font-style: bold; color: darkgreen"),h5(strong(textOutput("display_status_claim")), style="font-weight: bold; color: darkgreen; margin-top:1px"))
                         ),
                         br(),
                         br(),
                         br(),
                         br(),
                         
                         fluidRow(
                           column(6,textOutput("claim_message_request_claim"))
                         )
                  ),
                  column(width = 3,
                         column(12,h4(strong("Evidence"), style="font-style: bold; color: darkgreen"),h5(strong(uiOutput("display_evidence_claim")), style="font-weight: bold; color: darkgreen; margin-top:1px"))
                         #column(3,uiOutput("update_propertyvalue_hu"))
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
  mr_refresh <- reactiveVal(0)
  mr_counter <<- 0
  er_refresh <- reactiveVal(0)
  er_counter <<- 0
  hr_refresh <- reactiveVal(0)
  hr_counter <<- 0
  cr_refresh <- reactiveVal(0)
  cr_counter <<- 0
  savedstatus <<- ""
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
  #basemicroserviceurl <<- "http://159.122.181.240:31300/"
  basemicroserviceurl <<- "http://localhost:8000/"

  "localhost:8000/"
  basecustomerdata <- data.frame()
  mrcommon <- data.frame()
  mrmotor <- data.frame()
  ercommon <- data.frame()
  erendowment <- data.frame()
  hrcommon <- data.frame()
  hrhouse <- data.frame()
  crcommon <- data.frame()
  crcommercial <- data.frame()
  update_policy_request_mr_pressed <<- 0
  update_policy_request_er_pressed <<- 0
  update_policy_request_hr_pressed <<- 0
  update_policy_request_cr_pressed <<- 0
  save_policy_request_mu_pressed <<-0
  save_policy_request_eu_pressed <<-0
  save_policy_request_hu_pressed <<-0
  save_policy_request_cu_pressed <<-0
  
  updatecustomerpressed <<- 0
  addcustomerpressed <<- 0
  addCustomeraddpressed <<- 0
  refreshcustomer  <- reactiveVal(FALSE)
  refreshPolicy  <- reactiveVal(FALSE)
  refreshClaim  <- reactiveVal(FALSE)
  deleteok <<- FALSE
  saveclaimtable <<- data.frame()
  selectedclaimdata <<- data.frame()
    
  
  output$show_customer_details = DT::renderDataTable({
    
    if (show_welcome()) {
      shinyalert("Welcome", "This containerized application is built with APIs from Mainframe CICS Application", type = "success", confirmButtonCol = "#3F27B3")
      show_welcome(FALSE)
    }
    
    z <- ReappData$c
    refreshcustomer()
    
    
    d1 <- data.frame(CUSTOMERNUMBER=as.integer(),FIRSTNAME=as.character(),LASTNAME=as.character(),DATEOFBIRTH=as.character(),HOUSENAME=as.character(),HOUSENUMBER=as.character(),POSTCODE=as.integer(),JKEACCOUNTNUM=as.integer(),PHONEMOBILE=as.character(),EMAILADDRESS=as.character(),stringsAsFactors=FALSE)

    if (input$accept_customernumber == 0) {
      query1 <- paste("SELECT * FROM VCUSTOMER WHERE UPPER(FIRSTNAME) LIKE '%",toupper(input$accept_firstname),"%' AND UPPER(LASTNAME) LIKE '%",toupper(input$accept_lastname),"%'",sep="")
    }
    else {
      query1 <- paste("SELECT * FROM VCUSTOMER WHERE CUSTOMERNUMBER = ",input$accept_customernumber,"%'",sep="")
    }
    
    showModal(modalDialog(
      title = "Please wait...",
      h4("Getting Data"),
      easyClose = FALSE,
      footer = NULL
    ))
    
    res <- POST(paste(basemicroserviceurl,"getDVMzEUSDocker?",sep="")
                ,body=list(myquerry = query1),
                ,encode = "json")
    removeModal()
    appData <- content(res)
    if (length(appData) > 1) {
      if (appData[[2]][[2]] > 0 && length(appData[[1]]) != 0) {
        d1 <- as.data.frame(matrix(unlist(appData[[1]]), ncol=appData[[2]][[2]], byrow=TRUE), stringsAsFactors=FALSE)
        names(d1) <- names(data.frame(appData[[1]][[1]]))
        names(d1) <- gsub("PHONEHOME","JKEACCOUNTNUM",names(d1))
      }
    }
    # else {
    #   d1[1,]$Blank <- "No Data Found"
    # }


    DT::datatable(
      d1, class = 'cell-border stripe', rownames = FALSE, selection = "single",
      #caption = h4('Selected Item',style = 'caption-side: top; color:darkgreen ; font-Weight:bold'),
      extensions = list(FixedColumns = list(leftColumns = 1)),
      callback=DT::JS("table.on('click.dt', 'tr', function() {
                      var data=table.row(this).data();
                      Shiny.onInputChange('table_customer_ref',data[0]);
                      });"), 
      options = list (dom = 'tp',
                      columnDefs = list(list(className = 'dt-center', targets = c(0,2))), pageLength = 5,
                      headerCallback = JS(headerCallback3)
      )
    )%>%
      formatStyle(columns = c(1), fontSize = '120%', color = 'white', backgroundColor = '#935116') %>%
      formatStyle(columns = c(2), fontSize = '120%', color = 'white', backgroundColor = '#935116') %>%
      formatStyle(columns = c(3), fontSize = '120%', color = 'white', backgroundColor = '#935116') %>%
      formatStyle(columns = c(4), fontSize = '120%', color = '#935116') %>%
      formatStyle(columns = c(5), fontSize = '120%', color = '#935116') %>%
      formatStyle(columns = c(6), fontSize = '120%', color = '#935116') %>%
      formatStyle(columns = c(7), fontSize = '120%', color = '#935116') %>%
      formatStyle(columns = c(8), fontSize = '120%', color = '#935116') %>%
      formatStyle(columns = c(9), fontSize = '120%', color = '#935116') %>%
      formatStyle(columns = c(10), fontSize = '120%', color = '#935116')
  })
  
  headerCallback3 <- c(
    "function(thead, data, start, end, display){",
    "  $('th', thead).css('border-top', '2px solid #935116');",
    "  $('th', thead).css('border-bottom', '2px solid #935116');",
    "  $('th', thead).css('color', '#935116');",
    "}"
  )
  
  
  output$display_customer_number <- renderText({
    
    #input$RetrieveAccount4withdrawal
    #refreshbalancewithdrawal()
    #account_holder_name <- basedatawithdrawal$NAME
    if (!is.null(input$table_customer_ref)) {
      showModal(modalDialog(
        title = "Please wait...",
        h4("Retrieving Customer Data"),
        easyClose = FALSE,
        footer = NULL
      ))
      
      urlname <- paste("http://192.86.33.143:9080/CB12Customer/customer/",input$table_customer_ref,sep="")
      accountdata <- fromJSON(urlname)
      removeModal()
      if (accountdata$LGCMAREA$CA_RETURN_CODE == 0) {
        basecustomerdata <<- as.data.frame(accountdata$LGCMAREA$CA_CUSTOMER_REQUEST,stringsAsFactors=FALSE)
      }
    }
    return(input$table_customer_ref)
  })
  

  output$display_last_name <- renderText({
    input$table_customer_ref
    return(basecustomerdata$CA_LAST_NAME)
  })
  
  output$display_first_name <- renderText({
    input$table_customer_ref
    return(basecustomerdata$CA_FIRST_NAME)
  })
  
  output$display_dob <- renderText({
    input$table_customer_ref
    return(basecustomerdata$CA_DOB)
  })
  
  output$update_housename <- renderUI({
    input$table_customer_ref
    textInput("accept_housename", h4(strong("House Name"), style="font-style: bold; color: #935116"),value = basecustomerdata$CA_HOUSE_NAME)
  })
  
  output$update_housenumber <- renderUI({
    input$table_customer_ref
    textInput("accept_housenumber", h4(strong("House Number"), style="font-style: bold; color: #935116"),value = basecustomerdata$CA_HOUSE_NUM)  
  })
  
  output$update_postcode <- renderUI({
    input$table_customer_ref
    textInput("accept_postcode", h4(strong("Post Code"), style="font-style: bold; color: #935116"),value = basecustomerdata$CA_POSTCODE)  
  })

  output$update_jkeaccount <- renderUI({
    input$table_customer_ref
    numericInput("accept_jkeaccount", h4(strong("JKE Bank Account"), style="font-style: bold; color: #935116"),value = basecustomerdata$CA_PHONE_HOME)
  })
  
  output$update_mobile <- renderUI({
    input$table_customer_ref
    textInput("accept_mobile", h4(strong("Mobile Number"), style="font-style: bold; color: #935116"),value = basecustomerdata$CA_PHONE_MOBILE)
  })
  
  output$update_email <- renderUI({
    input$table_customer_ref
    textInput("accept_email", h4(strong("email Id"), style="font-style: bold; color: #935116"),value = basecustomerdata$CA_EMAIL_ADDRESS)
  })

  
  output$show_customer_policy = DT::renderDataTable({
    
    z <- ReappData$c
    input$table_customer_ref
    input$refreshPolicy
    input$refreshCustomer
    
    d1 <- data.frame(POLICYTYPE=as.character(),POLICYCOUNT=as.integer(),stringsAsFactors=FALSE)

    if (!is.null(input$table_customer_ref)) {
      urlname <- paste("http://192.86.33.143:9080/CB12_getCustomerPolicy/Customer?CustomerNumber=",input$table_customer_ref,sep="")
      accountdata <- fromJSON(urlname)
      if ( accountdata$StatusCode == 200) {
        d1 <- as.data.frame(accountdata$`ResultSet Output`,stringsAsFactors=FALSE)
        if (nrow(d1) > 0) {
          d1 <- d1 %>% group_by(POLICYTYPE) %>% summarise(POLICYCOUNT=n())
          d1 <- as.data.frame(d1)
          if(nrow(d1[d1$POLICYTYPE=="C",]) > 0) {
            d1[d1$POLICYTYPE=="C",]$POLICYTYPE <- "Commercial"
          }
          if(nrow(d1[d1$POLICYTYPE=="E",]) > 0) {
            d1[d1$POLICYTYPE=="E",]$POLICYTYPE <- "Endowment"
          }
          if(nrow(d1[d1$POLICYTYPE=="H",]) > 0) {
            d1[d1$POLICYTYPE=="H",]$POLICYTYPE <- "House"
          }
          if(nrow(d1[d1$POLICYTYPE=="M",]) > 0) {
            d1[d1$POLICYTYPE=="M",]$POLICYTYPE <- "Motor"
          }
        }
        else {
          d1 <- data.frame(POLICYTYPE=as.character(),POLICYCOUNT=as.integer(),stringsAsFactors=FALSE)
        }
      }
    }
    
    DT::datatable(
      d1, class = 'cell-border stripe', rownames = FALSE, selection = "single", 
      #caption = h4('Selected Item',style = 'caption-side: top; color:darkgreen ; font-Weight:bold'),
      extensions = list(FixedColumns = list(leftColumns = 1)),
      callback=DT::JS("table.on('click.dt', 'tr', function() {
                      var data=table.row(this).data();
                      Shiny.onInputChange('table_customerpolicy_ref',data[0]);
      });"), 
      options = list (dom = 't',
                      columnDefs = list(list(className = 'dt-center', targets = c(0,1))),
                      headerCallback = JS(headerCallback4)
    )
  )%>%
      formatStyle(columns = c(1), fontSize = '130%', color = 'white', fontWeight = 'bold', backgroundColor = '#935116') %>%
      formatStyle(columns = c(2), fontSize = '130%', color = '#935116', fontWeight = 'bold')
  })
  
  headerCallback4 <- c(
    "function(thead, data, start, end, display){",
    "  $('th', thead).css('border-top', '2px solid #935116');",
    "  $('th', thead).css('border-bottom', '2px solid #935116');",
    "  $('th', thead).css('color', '#935116');",
    "}"
  )
  
  
  
  output$add_or_update_message <- renderText({
    if (!is.null(input$table_customer_ref)) {
      if (input$addCustomer > addcustomerpressed) {
        addcustomerpressed <<- input$addCustomer
        return("A")
      }
      else {
        return("U")
      }
    }
    else {
      if (input$addCustomer > addcustomerpressed) {
        addcustomerpressed <<- input$addCustomer
        return("A")
      }
      else {
        return("")
      }
    }
  })

  
  output$add_message <- renderText({
    
    
    if (input$addCustomeradd > addCustomeraddpressed) {
      addCustomeraddpressed <<- input$addCustomeradd
      pc_json <- list(
        LGCMAREA = list(
          CA_CUSTOMER_REQUEST = list(
            CA_FIRST_NAME = input$accept_firstname_add,
            CA_LAST_NAME = input$accept_lastname_add,
            CA_DOB = input$accept_dob_add,
            CA_HOUSE_NAME = input$accept_housenameadd,
            CA_HOUSE_NUM = input$accept_housenumberadd,
            CA_POSTCODE = input$accept_postcodeadd,
            CA_NUM_POLICIES = 0,
            CA_PHONE_MOBILE = input$accept_mobileadd,
            CA_PHONE_HOME = input$accept_jkeaccountadd,
            CA_EMAIL_ADDRESS = input$accept_emailadd
          )
        )
      )
      
      showModal(modalDialog(
        title = "Please wait...",
        h4("Adding Customer Data"),
        easyClose = FALSE,
        footer = NULL
      ))
      
      res <- POST("http://192.86.33.143:9080/CB12Customer/AddCustomer"
                 , body = pc_json
                 , encode = "json")
      
      appData <- content(res)
      
      removeModal()
      
      if (appData$LGCMAREA$CA_RETURN_CODE != 0) {
        return("Update Unsuccessful")
      }
      else {
        shinyalert("Success", paste("New Customer ID ", appData$LGCMAREA$CA_CUSTOMER_NUM, " successfully created", sep=""), type = "success",confirmButtonCol = "#54BA60")
        if (refreshcustomer()) {
          newrefreshcustomer <- FALSE
          refreshcustomer(newrefreshcustomer)
        }
        else {
          newrefreshcustomer <- TRUE
          refreshcustomer(newrefreshcustomer)
        }
        
        return("Add Successful")
      }
    }
    else {
      return("Press Add Button to Add")
    }
    
  })
  
  
  output$update_message <- renderText({
    
    
    if (input$updateCustomer > updatecustomerpressed) {
      updatecustomerpressed <<- input$updateCustomer
      pc_json <- list(
        LGCMAREA = list(
          CA_CUSTOMER_REQUEST = list(
            CA_FIRST_NAME = basecustomerdata$CA_FIRST_NAME,
            CA_LAST_NAME = basecustomerdata$CA_LAST_NAME,
            CA_DOB = basecustomerdata$CA_DOB,
            CA_HOUSE_NAME = input$accept_housename,
            CA_HOUSE_NUM = input$accept_housenumber,
            CA_POSTCODE = input$accept_postcode,
            CA_NUM_POLICIES = 0,
            CA_PHONE_MOBILE = input$accept_mobile,
            CA_PHONE_HOME = input$accept_jkeaccount,
            CA_EMAIL_ADDRESS = input$accept_email
            )
          )
        )
      
      showModal(modalDialog(
        title = "Please wait...",
        h4("Updating Customer Data"),
        easyClose = FALSE,
        footer = NULL
      ))
      
      res <- PUT(paste("http://192.86.33.143:9080/CB12Customer/customer/",input$table_customer_ref,sep="")
                 , body = pc_json
                 , encode = "json")
      
      appData <- content(res)
      removeModal()
      if (appData$LGCMAREA$CA_RETURN_CODE != 0) {
        return("Update Unsuccessful")
      }
      else {
        shinyalert("Success", "Customer Data successfully updated", type = "success",confirmButtonCol = "#54BA60")
        if (refreshcustomer()) {
          newrefreshcustomer <- FALSE
          refreshcustomer(newrefreshcustomer)
        }
        else {
          newrefreshcustomer <- TRUE
          refreshcustomer(newrefreshcustomer)
        }
        
        return("Update Successful")
      }
    }
    else {
      return("Press Update Button to Update")
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
  
  
  output$select_customernumber_policy <- renderUI({
    input$table_customer_ref
    numericInput("accept_customernumber_policy", h4(strong("Customer Number"), style="font-style: bold; color: darkblue"),value = ifelse(is.null(input$table_customer_ref),0,input$table_customer_ref))
  })
  
  output$select_insurance_type <- renderUI({
    input$table_customerpolicy_ref
    #updateTabItems(session, "tabs", "dashboard2")
    selectInput("selected_insurance_type", h4(strong("Select Insurance Type"), style="font-style: bold; color: darkgreen"), choices=c("All","Motor","Endowment","House","Commercial"),selected = ifelse(is.null(input$table_customerpolicy_ref),"All",input$table_customerpolicy_ref))
  })
  
  observeEvent(input$table_customerpolicy_ref, {
    updateTabItems(session, "tabs", "dashboard2")
  })
  
  output$show_policy_details = DT::renderDataTable({
    
    z <- ReappData$c
    input$accept_customernumber_policy
    input$refreshPolicy
    refreshPolicy()
    
    d1 <- data.frame(POLICYNUMBER = as.integer(), CUSTOMERNUMBER=as.integer(),ISSUEDATE=as.character(),EXPIRYDATE=as.character(),POLICYTYPE=as.character(),stringsAsFactors=FALSE)

    converted_policy_type <- substr(input$selected_insurance_type,1,1)
    if (converted_policy_type == "A") {
      converted_policy_type <- ""
    }
    
    if (input$accept_policy_number == 0) {
      query1 <- paste ("sELECT POLICYNUMBER, CUSTOMERNUMBER, ISSUEDATE, EXPIRYDATE, POLICYTYPE FROM VPOLICY WHERE CUSTOMERNUMBER = ",input$accept_customernumber_policy," AND POLICYTYPE LIKE '%", converted_policy_type, "%'",sep="")
    }
    else {
      if (input$accept_customernumber_policy == 0) {
        query1 <- paste("SELECT POLICYNUMBER, CUSTOMERNUMBER, ISSUEDATE, EXPIRYDATE, POLICYTYPE FROM VPOLICY WHERE POLICYNUMBER = ",input$accept_policy_number,sep="")
      }
      else {
        query1 <- paste("SELECT POLICYNUMBER, CUSTOMERNUMBER, ISSUEDATE, EXPIRYDATE, POLICYTYPE FROM VPOLICY WHERE POLICYNUMBER = ",input$accept_policy_number," AND CUSTOMERNUMBER = ",input$accept_customernumber_policy,sep="")
      }
    }
    
    showModal(modalDialog(
      title = "Please wait...",
      h4("Retrieving Policy List"),
      easyClose = FALSE,
      footer = NULL
    ))
    
    
    res <- POST(paste(basemicroserviceurl,"getDVMzEUSDocker?",sep="")
                ,body=list(myquerry = query1),
                ,encode = "json")
    appData <- content(res)
    removeModal()
    if (length(appData) > 1) {
      if (appData[[2]][[2]] > 0 && length(appData[[1]]) != 0) {
        d1 <- as.data.frame(matrix(unlist(appData[[1]]), ncol=appData[[2]][[2]], byrow=TRUE), stringsAsFactors=FALSE)
        names(d1) <- names(data.frame(appData[[1]][[1]]))
        if(nrow(d1[d1$POLICYTYPE=="C",]) > 0) {
          d1[d1$POLICYTYPE=="C",]$POLICYTYPE <- "Commercial"
        }
        if(nrow(d1[d1$POLICYTYPE=="E",]) > 0) {
          d1[d1$POLICYTYPE=="E",]$POLICYTYPE <- "Endowment"
        }
        if(nrow(d1[d1$POLICYTYPE=="H",]) > 0) {
          d1[d1$POLICYTYPE=="H",]$POLICYTYPE <- "House"
        }
        if(nrow(d1[d1$POLICYTYPE=="M",]) > 0) {
          d1[d1$POLICYTYPE=="M",]$POLICYTYPE <- "Motor"
        }
        
      }
    }

    DT::datatable(
      d1, class = 'cell-border stripe', rownames = FALSE, selection = "single",
      #caption = h4('Selected Item',style = 'caption-side: top; color:darkgreen ; font-Weight:bold'),
      extensions = list(FixedColumns = list(leftColumns = 1)),
      callback=DT::JS("table.on('click.dt', 'tr', function() {
                      var data=table.row(this).data();
                      Shiny.onInputChange('selected_policy',data[0]);
                      Shiny.onInputChange('selected_customer',data[1]);
                      Shiny.onInputChange('selected_type',data[4]);
  });"), 
      options = list (dom = 'tp',
                      columnDefs = list(list(className = 'dt-center', targets = c(0,2))), pageLength = 5,
                      headerCallback = JS(headerCallback5)
    )
  )%>%
      formatStyle(columns = c(1), fontSize = '120%', color = 'white', backgroundColor = 'darkblue') %>%
      formatStyle(columns = c(2), fontSize = '120%', color = 'white', backgroundColor = 'darkblue') %>%
      formatStyle(columns = c(3), fontSize = '120%', color = 'darkblue') %>%
      formatStyle(columns = c(4), fontSize = '120%', color = 'darkblue') %>%
      formatStyle(columns = c(5), fontSize = '120%', color = 'white', backgroundColor = 'darkblue')
  })
  
  headerCallback5 <- c(
    "function(thead, data, start, end, display){",
    "  $('th', thead).css('border-top', '2px solid darkblue');",
    "  $('th', thead).css('border-bottom', '2px solid darkblue');",
    "  $('th', thead).css('color', 'darkblue');",
    "}"
  )
  
  output$policy_type_message <- renderText({

    input$save_policy_request_mu
    input$save_policy_request_eu
    input$save_policy_request_hu
    input$ok_md
    input$ok_ed
    input$ok_hd
    input$ok_cd
    input$selected_policy
    input$refreshPolicy
    
    if (is.null(input$selected_type)) {
      return("")
    }
    if (deleteok) {
      deleteok <<- FALSE
      return("")
    }
    
    if (input$selected_type == "Motor") {
      if (input$update_policy_request_mr > update_policy_request_mr_pressed) {
        update_policy_request_mr_pressed <<- input$update_policy_request_mr 
        return("MU")
      }
      else {
        return("MR")
      }
    }
    else {
      if (input$selected_type == "Endowment") {
        if (input$update_policy_request_er > update_policy_request_er_pressed) {
          update_policy_request_er_pressed <<- input$update_policy_request_er 
          return("EU")
        }
        else {
          return("ER")
        }
      }
      else {
        if (input$selected_type == "House") {
          if (input$update_policy_request_hr > update_policy_request_hr_pressed) {
            update_policy_request_hr_pressed <<- input$update_policy_request_hr 
            return("HU")
          }
          else {
            return("HR")
          }
        }
        else {
          if (input$selected_type == "Commercial") {
            return("CR")
          }
          else {
            return("Error")
          }
          
        }
      }
    }

  })

  output$policy_message_request_mr <- renderText({
    
    showModal(modalDialog(
      title = "Please wait...",
      h4("Retrieving Policy"),
      easyClose = FALSE,
      footer = NULL
    ))
    
    
    urlname <- paste("http://192.86.33.143:9080/CB12MotorPolicy/Policy/",input$selected_customer,",",input$selected_policy,sep="")
    mr_policy_data <- fromJSON(urlname)
    accountdata1 <- fromJSON(urlname)
    removeModal()
    if ( !is.null(mr_policy_data$LGCMAREA$CA_RETURN_CODE)) {
      if ( mr_policy_data$LGCMAREA$CA_RETURN_CODE == 0) {
        mrcommon <<- as.data.frame(mr_policy_data$LGCMAREA$CA_POLICY_REQUEST$CA_POLICY_COMMON,stringsAsFactors=FALSE)
        mrmotor <<- as.data.frame(mr_policy_data$LGCMAREA$CA_POLICY_REQUEST$CA_MOTOR,stringsAsFactors=FALSE)
        mr_counter <<- mr_counter + 1
        mr_refresh(mr_counter)
        return("Policy Details Retrived")
      }
      else {
        return(paste("Error retrieving - Return Code ", mr_policy_data$LGCMAREA$CA_RETURN_CODE, sep=""))
      }
    }
    else {
      return("API Error")
    }
  })
  
  output$display_customer_number_mr <- renderText({
    return(input$selected_customer)
  })
  
  output$display_policy_number_mr <- renderText({
    return(input$selected_policy)
  })
  
  output$display_policy_type_mr <- renderText({
    return(input$selected_type)
  })
  
  output$display_policy_start_date_mr <- renderText({
    mr_refresh()
    input$selected_type
    return(mrcommon$CA_ISSUE_DATE)
  })

  output$display_policy_expiry_date_mr <- renderText({
    mr_refresh()
    input$selected_type
    return(mrcommon$CA_EXPIRY_DATE)
  })

  output$display_make_mr <- renderText({
    mr_refresh()
    input$selected_type
    return(mrmotor$CA_M_MAKE)
  })

  output$display_model_mr <- renderText({
    mr_refresh()
    input$selected_type
    return(mrmotor$CA_M_MODEL)
  })
  
  output$display_value_mr <- renderText({
    mr_refresh()
    input$selected_type
    return(mrmotor$CA_M_VALUE)
  })
  
  output$display_registration_mr <- renderText({
    mr_refresh()
    input$selected_type
    return(mrmotor$CA_M_REGNUMBER)
  })
  
  output$display_color_mr <- renderText({
    mr_refresh()
    input$selected_type
    return(mrmotor$CA_M_COLOUR)
  })
  
  output$display_power_mr <- renderText({
    mr_refresh()
    input$selected_type
    return(mrmotor$CA_M_CC)
  })

  output$display_numofaccidents_mr <- renderText({
    mr_refresh()
    input$selected_type
    return(mrmotor$CA_M_ACCIDENTS)
  })
  
  output$display_premium_mr <- renderText({
    mr_refresh()
    input$selected_type
    return(mrmotor$CA_M_PREMIUM)
  })
  
  output$display_mfg_date_mr <- renderText({
    mr_refresh()
    input$selected_type
    return(mrmotor$CA_M_MANUFACTURED)
  })

  
  output$show_claim_details_mr = DT::renderDataTable({
    
    z <- ReappData$c
    input$selected_type
    input$refreshPolicy
    input$refreshClaim
    
    d1 <- data.frame(CLAIMSTATUS=as.character(),CLAIMSTATUSCOUNT=as.integer(),stringsAsFactors=FALSE)
    
    query1 <- paste("select CLAIMNUMBER, OBSERVATIONS  FROM VCLAIM WHERE POLICYNUMBER = ",input$selected_policy,sep="")
    showModal(modalDialog(
      title = "Please wait...",
      h4("Retrieving Claims List"),
      easyClose = FALSE,
      footer = NULL
    ))
    
    res <- POST(paste(basemicroserviceurl,"getDVMzEUSDocker?",sep="")
                ,body=list(myquerry = query1),
                ,encode = "json")
    appData <- content(res)
    removeModal()
    if (length(appData) > 1) {
      if (appData[[2]][[2]] > 0 && length(appData[[1]]) != 0) {
        d1 <- as.data.frame(matrix(unlist(appData[[1]]), ncol=appData[[2]][[2]], byrow=TRUE), stringsAsFactors=FALSE)
        names(d1) <- names(data.frame(appData[[1]][[1]]))
        d1$CLAIMSTATUS <- ifelse(gsub(" ","",d1$OBSERVATIONS)=="","Open","Settled")
        d1$OBSERVATIONS <- NULL
        d1 <- d1 %>% group_by(CLAIMSTATUS) %>% summarise(CLAIMSTATUSCOUNT=n())
      }
    }

    DT::datatable(
      d1, class = 'cell-border stripe', rownames = FALSE, selection = "single",
      #caption = h4('Selected Item',style = 'caption-side: top; color:darkgreen ; font-Weight:bold'),
      extensions = list(FixedColumns = list(leftColumns = 1)),
      callback=DT::JS("table.on('click.dt', 'tr', function() {
                      var data=table.row(this).data();
                      Shiny.onInputChange('table_claimstatus_mr',data[0]);
      });"), 
      options = list (dom = 't',
                      columnDefs = list(list(className = 'dt-center', targets = c(0,1))),
                      headerCallback = JS(headerCallback4)
    )
  )%>%
      formatStyle(columns = c(1), fontSize = '130%', color = 'white', fontWeight = 'bold', backgroundColor = 'darkblue') %>%
      formatStyle(columns = c(2), fontSize = '130%', color = 'darkblue', fontWeight = 'bold')
  })
  
  headerCallback4 <- c(
    "function(thead, data, start, end, display){",
    "  $('th', thead).css('border-top', '2px solid darkblue');",
    "  $('th', thead).css('border-bottom', '2px solid darkblue');",
    "  $('th', thead).css('color', 'darkblue');",
    "}"
  )

  output$display_customer_number_mu <- renderText({
    return(input$selected_customer)
  })
  
  output$display_policy_number_mu <- renderText({
    return(input$selected_policy)
  })
  
  output$display_policy_type_mu <- renderText({
    return(input$selected_type)
  })
  
  output$display_policy_start_date_mu <- renderText({
    input$update_policy_request_mr
    return(mrcommon$CA_ISSUE_DATE)
  })
  
  output$update_policy_expiry_date_mu <- renderUI({
    input$update_policy_request_mr
    dateInput("accept_policy_expiry_date_mu", h4(strong("Expiry Date"), style="font-style: bold; color: darkblue"),format="yyyy-mm-dd",value = mrcommon$CA_EXPIRY_DATE)
  })

  output$update_make_mu <- renderUI({
    input$update_policy_request_mr
    textInput("accept_make_mu", h4(strong("Make"), style="font-style: bold; color: darkblue"),value = mrmotor$CA_M_MAKE)  
  })

  output$update_model_mu <- renderUI({
    input$update_policy_request_mr
    textInput("accept_model_mu", h4(strong("Model"), style="font-style: bold; color: darkblue"),value = mrmotor$CA_M_MODEL)  
  })
  
  output$update_value_mu <- renderUI({
    input$update_policy_request_mr
    numericInput("accept_value_mu", h4(strong("Value in US$"), style="font-style: bold; color: darkblue"),value = mrmotor$CA_M_VALUE)
  })
  
  output$update_registration_mu <- renderUI({
    input$update_policy_request_mr
    textInput("accept_registration_mu", h4(strong("Registration No"), style="font-style: bold; color: darkblue"),value = mrmotor$CA_M_REGNUMBER)  
  })
  
  output$update_color_mu <- renderUI({
    input$update_policy_request_mr
    textInput("accept_color_mu", h4(strong("Color"), style="font-style: bold; color: darkblue"),value = mrmotor$CA_M_COLOUR)  
  })
  
  output$update_power_mu <- renderUI({
    input$update_policy_request_mr
    numericInput("accept_power_mu", h4(strong("Engine Power in CC"), style="font-style: bold; color: darkblue"),value = mrmotor$CA_M_CC)
  })
  
  output$update_numofaccidents_mu <- renderUI({
    input$update_policy_request_mr
    numericInput("accept_numofaccidents_mu", h4(strong("No of Accidents"), style="font-style: bold; color: darkblue"),value = mrmotor$CA_M_ACCIDENTS)
  })
  
  output$update_premium_mu <- renderUI({
    input$update_policy_request_mr
    numericInput("accept_premium_mu", h4(strong("Yearly Premium in US$"), style="font-style: bold; color: darkblue"),value = mrmotor$CA_M_PREMIUM)
  })
  
  output$update_mfg_date_mu <- renderUI({
    input$update_policy_request_mr
    dateInput("accept_mfg_date_mu", h4(strong("Date Manufactured"), style="font-style: bold; color: darkblue"),format="yyyy-mm-dd",value = mrcommon$CA_M_MANUFACTURED)
  })

  
  output$policy_save_request_mu <- renderText({
    
    
    if (input$save_policy_request_mu > save_policy_request_mu_pressed) {
      save_policy_request_mu_pressed <<- input$save_policy_request_mu
      #op <- options(digits.secs = 6)
      #savetime <- format(Sys.time(),tz="UTC")
      pc_json <- list(
        LGCMAREA = list(
          CA_POLICY_REQUEST = list(
            CA_POLICY_COMMON = list(
              CA_ISSUE_DATE = mrcommon$CA_ISSUE_DATE,
              CA_EXPIRY_DATE = input$accept_policy_expiry_date_mu,
              CA_LASTCHANGED = mrcommon$CA_LASTCHANGED
            ),
            CA_MOTOR = list(
              CA_M_MAKE = input$accept_make_mu,
              CA_M_MODEL = input$accept_model_mu,
              CA_M_VALUE = input$accept_value_mu,
              CA_M_REGNUMBER = input$accept_registration_mu,
              CA_M_COLOUR = input$accept_color_mu,
              CA_M_CC = input$accept_power_mu,
              CA_M_MANUFACTURED = input$accept_mfg_date_mu,
              CA_M_PREMIUM = input$accept_premium_mu,
              CA_M_ACCIDENTS = input$accept_numofaccidents_mu
            )
          )
        )
      )
      
      showModal(modalDialog(
        title = "Please wait...",
        h4("Saving Policy"),
        easyClose = FALSE,
        footer = NULL
      ))
      
      res <- PUT(paste("http://192.86.33.143:9080/CB12MotorPolicy/Policy/",input$selected_customer,",",input$selected_policy,sep="")
                  , body = pc_json
                  , encode = "json")
      
      appData <- content(res)
      removeModal()
      if (appData$LGCMAREA$CA_RETURN_CODE != 0) {
        return("Update Unsuccessful")
      }
      else {
        shinyalert("Success", paste("Policy ID ", input$selected_policy, " successfully updated", sep=""), type = "success",confirmButtonCol = "#54BA60")
        urlname <- paste("http://192.86.33.143:9080/CB12MotorPolicy/Policy/",input$selected_customer,",",input$selected_policy,sep="")
        mr_policy_data <- fromJSON(urlname)
        accountdata1 <- fromJSON(urlname)
        if ( !is.null(mr_policy_data$LGCMAREA$CA_RETURN_CODE)) {
          if ( mr_policy_data$LGCMAREA$CA_RETURN_CODE == 0) {
            mrcommon <<- as.data.frame(mr_policy_data$LGCMAREA$CA_POLICY_REQUEST$CA_POLICY_COMMON,stringsAsFactors=FALSE)
            mrmotor <<- as.data.frame(mr_policy_data$LGCMAREA$CA_POLICY_REQUEST$CA_MOTOR,stringsAsFactors=FALSE)
            mr_counter <<- mr_counter + 1
            mr_refresh(mr_counter)
          }
        }
        
        if (refreshPolicy()) {
          newrefreshPolicy <- FALSE
          refreshPolicy(newrefreshPolicy)
        }
        else {
          newrefreshPolicy <- TRUE
          refreshPolicy(newrefreshPolicy)
        }
        return("Update Successful")
      }
    }
    else {
      return("Press Save Button to Update")
    }
    
  })
  
  observeEvent(input$delete_policy_request_mr, {
    
    showModal(modalDialog(
      title = h4("Warning"),
      size ="m",
      "Do you want to Delete the policy",
      easyClose = FALSE,
      footer = tagList(
        modalButton("Cancel"),
        actionButton("ok_md", "OK")
      )
    ))
    
  })
  
  observeEvent(input$ok_md, {
    
    res <- DELETE(paste("http://192.86.33.143:9080/CB12MotorPolicy/Policy/",input$selected_customer,",",input$selected_policy,sep=""))
    removeModal()
    appData <- content(res)
    if (appData$LGCMAREA$CA_RETURN_CODE != 0) {
      shinyalert("Error", paste("Policy ID ", input$selected_policy, " delete failed", sep=""), type = "error",confirmButtonCol = "#E74C3C")
    }
    else {
      deleteok <<- TRUE
      if (refreshPolicy()) {
        newrefreshPolicy <- FALSE
        refreshPolicy(newrefreshPolicy)
      }
      else {
        newrefreshPolicy <- TRUE
        refreshPolicy(newrefreshPolicy)
      }
      shinyalert("Success", paste("Policy ID ", input$selected_policy, " successfully deleted", sep=""), type = "success",confirmButtonCol = "#54BA60")
    }
  })
################  
  output$policy_message_request_er <- renderText({
    
    showModal(modalDialog(
      title = "Please wait...",
      h4("Retrieving Policy"),
      easyClose = FALSE,
      footer = NULL
    ))
    
    
    urlname <- paste("http://192.86.33.143:9080/CB12EndowmentPolicy/Policy/",input$selected_customer,",",input$selected_policy,sep="")
    er_policy_data <- fromJSON(urlname)
    accountdata1 <- fromJSON(urlname)
    removeModal()
    if ( !is.null(er_policy_data$LGCMAREA$CA_RETURN_CODE)) {
      if ( er_policy_data$LGCMAREA$CA_RETURN_CODE == 0) {
        ercommon <<- as.data.frame(er_policy_data$LGCMAREA$CA_POLICY_REQUEST$CA_POLICY_COMMON,stringsAsFactors=FALSE)
        erendowment <<- as.data.frame(er_policy_data$LGCMAREA$CA_POLICY_REQUEST$CA_ENDOWMENT,stringsAsFactors=FALSE)
        er_counter <<- er_counter + 1
        er_refresh(er_counter)
        return("Policy Details Retrived")
      }
      else {
        return(paste("Error retrieving - Return Code ", er_policy_data$LGCMAREA$CA_RETURN_CODE, sep=""))
      }
    }
    else {
      return("API Error")
    }
  })
  
  output$display_customer_number_er <- renderText({
    return(input$selected_customer)
  })
  
  output$display_policy_number_er <- renderText({
    return(input$selected_policy)
  })
  
  output$display_policy_type_er <- renderText({
    return(input$selected_type)
  })
  
  output$display_policy_start_date_er <- renderText({
    er_refresh()
    input$selected_type
    return(ercommon$CA_ISSUE_DATE)
  })
  
  output$display_policy_expiry_date_er <- renderText({
    er_refresh()
    input$selected_type
    return(ercommon$CA_EXPIRY_DATE)
  })
  
  output$display_fundname_er <- renderText({
    er_refresh()
    input$selected_type
    return(erendowment$CA_E_FUND_NAME)
  })
  
  output$display_sumassured_er <- renderText({
    er_refresh()
    input$selected_type
    return(erendowment$CA_E_SUM_ASSURED)
  })
  
  output$display_nameoflifeassured_er <- renderText({
    er_refresh()
    input$selected_type
    return(erendowment$CA_E_LIFE_ASSURED)
  })
  
  output$display_term_er <- renderText({
    er_refresh()
    input$selected_type
    return(erendowment$CA_E_TERM)
  })
  
  output$display_wprofit_er <- renderText({
    er_refresh()
    input$selected_type
    return(ifelse(erendowment$CA_E_WITH_PROFITS == "Y","Yes","No"))
  })
  
  output$display_wequities_er <- renderText({
    er_refresh()
    input$selected_type
    return(ifelse(erendowment$CA_E_EQUITIES == "Y","Yes","No"))
  })
  
  output$display_managedfund_er <- renderText({
    er_refresh()
    input$selected_type
    return(ifelse(erendowment$CA_E_MANAGED_FUND == "Y","Yes","No"))
  })
  
  
  output$show_claim_details_er = DT::renderDataTable({
    
    z <- ReappData$c
    input$selected_type
    input$refreshPolicy
    input$refreshClaim
    d1 <- data.frame(CLAIMSTATUS=as.character(),CLAIMSTATUSCOUNT=as.integer(),stringsAsFactors=FALSE)
    
    query1 <- paste("select CLAIMNUMBER, OBSERVATIONS  FROM VCLAIM WHERE POLICYNUMBER = ",input$selected_policy,sep="")
    showModal(modalDialog(
      title = "Please wait...",
      h4("Retrieving Claims List"),
      easyClose = FALSE,
      footer = NULL
    ))
    
    res <- POST(paste(basemicroserviceurl,"getDVMzEUSDocker?",sep="")
                ,body=list(myquerry = query1),
                ,encode = "json")
    appData <- content(res)
    removeModal()
    if (length(appData) > 1) {
      if (appData[[2]][[2]] > 0 && length(appData[[1]]) != 0) {
        d1 <- as.data.frame(matrix(unlist(appData[[1]]), ncol=appData[[2]][[2]], byrow=TRUE), stringsAsFactors=FALSE)
        names(d1) <- names(data.frame(appData[[1]][[1]]))
        d1$CLAIMSTATUS <- ifelse(gsub(" ","",d1$OBSERVATIONS)=="","Open","Settled")
        d1$OBSERVATIONS <- NULL
        d1 <- d1 %>% group_by(CLAIMSTATUS) %>% summarise(CLAIMSTATUSCOUNT=n())
      }
    }
    
    DT::datatable(
      d1, class = 'cell-border stripe', rownames = FALSE, selection = "single",
      #caption = h4('Selected Item',style = 'caption-side: top; color:darkgreen ; font-Weight:bold'),
      extensions = list(FixedColumns = list(leftColumns = 1)),
      callback=DT::JS("table.on('click.dt', 'tr', function() {
                      var data=table.row(this).data();
                      Shiny.onInputChange('table_claimstatus_er',data[0]);
  });"), 
    options = list (dom = 't',
                    columnDefs = list(list(className = 'dt-center', targets = c(0,1))),
                    headerCallback = JS(headerCallback7)
    )
  )%>%
      formatStyle(columns = c(1), fontSize = '130%', color = 'white', fontWeight = 'bold', backgroundColor = 'darkblue') %>%
      formatStyle(columns = c(2), fontSize = '130%', color = 'darkblue', fontWeight = 'bold')
  })
  
  headerCallback7 <- c(
    "function(thead, data, start, end, display){",
    "  $('th', thead).css('border-top', '2px solid darkblue');",
    "  $('th', thead).css('border-bottom', '2px solid darkblue');",
    "  $('th', thead).css('color', 'darkblue');",
    "}"
  )
  
  
  output$display_customer_number_eu <- renderText({
    return(input$selected_customer)
  })
  
  output$display_policy_number_eu <- renderText({
    return(input$selected_policy)
  })
  
  output$display_policy_type_eu <- renderText({
    return(input$selected_type)
  })
  
  output$display_policy_start_date_eu <- renderText({
    input$update_policy_request_er
    return(ercommon$CA_ISSUE_DATE)
  })
  
  output$update_policy_expiry_date_eu <- renderUI({
    input$update_policy_request_er
    dateInput("accept_policy_expiry_date_eu", h4(strong("Expiry Date"), style="font-style: bold; color: darkblue"),format="yyyy-mm-dd",value = ercommon$CA_EXPIRY_DATE)
  })
  
  output$update_fundname_eu <- renderUI({
    input$update_policy_request_er
    textInput("accept_fundname_eu", h4(strong("Fund Name"), style="font-style: bold; color: darkblue"),value = erendowment$CA_E_FUND_NAME)  
  })
  
  output$update_sumassured_eu <- renderUI({
    input$update_policy_request_er
    numericInput("accept_sumassured_eu", h4(strong("Sum Assured US$"), style="font-style: bold; color: darkblue"),value = erendowment$CA_E_SUM_ASSURED)
  })
  
  output$update_nameoflifeassured_eu <- renderUI({
    input$update_policy_request_er
    textInput("accept_nameoflifeassured_eu", h4(strong("Name of Life Assured"), style="font-style: bold; color: darkblue"),value = erendowment$CA_E_LIFE_ASSURED)  
  })
  
  output$update_term_eu <- renderUI({
    input$update_policy_request_er
    numericInput("accept_term_eu", h4(strong("Term"), style="font-style: bold; color: darkblue"),value = erendowment$CA_E_TERM)
  })
  
  output$update_wprofit_eu <- renderUI({
    input$update_policy_request_er
    selectInput("accept_wprofit_eu", h4(strong("With Profit"), style="font-style: bold; color: darkblue"), choices=c("Yes","No"),selected = ifelse(erendowment$CA_E_WITH_PROFITS == "Y","Yes","No"))
  })
  
  output$update_wequities_eu <- renderUI({
    input$update_policy_request_er
    selectInput("accept_wequities_eu", h4(strong("With Equities"), style="font-style: bold; color: darkblue"), choices=c("Yes","No"),selected = ifelse(erendowment$CA_E_EQUITIES == "Y","Yes","No"))
  })
  
  output$update_managedfund_eu <- renderUI({
    input$update_policy_request_er
    selectInput("accept_managedfund_eu", h4(strong("Managed Fund"), style="font-style: bold; color: darkblue"), choices=c("Yes","No"),selected = ifelse(erendowment$CA_E_MANAGED_FUND == "Y","Yes","No"))
  })
  
  output$policy_save_request_eu <- renderText({
    
    
    if (input$save_policy_request_eu > save_policy_request_eu_pressed) {
      save_policy_request_eu_pressed <<- input$save_policy_request_eu
      #op <- options(digits.secs = 6)
      #savetime <- format(Sys.time(),tz="UTC")
      pc_json <- list(
        LGCMAREA = list(
          CA_POLICY_REQUEST = list(
            CA_POLICY_COMMON = list(
              CA_ISSUE_DATE = ercommon$CA_ISSUE_DATE,
              CA_EXPIRY_DATE = input$accept_policy_expiry_date_eu,
              CA_LASTCHANGED = ercommon$CA_LASTCHANGED
            ),
            CA_ENDOWMENT = list(
              CA_E_FUND_NAME = input$accept_fundname_eu,
              CA_E_TERM = input$accept_term_eu,
              CA_E_SUM_ASSURED = input$accept_sumassured_eu,
              CA_E_WITH_PROFITS = ifelse(input$accept_wprofit_eu == "Yes","Y","N"),
              CA_E_EQUITIES = ifelse(input$accept_wequities_eu == "Yes","Y","N"),
              CA_E_MANAGED_FUND = ifelse(input$accept_managedfund_eu == "Yes","Y","N"),
              CA_E_LIFE_ASSURED = input$accept_nameoflifeassured_eu          )
          )
        )
      )
      
      showModal(modalDialog(
        title = "Please wait...",
        h4("Saving Policy"),
        easyClose = FALSE,
        footer = NULL
      ))
      
      res <- PUT(paste("http://192.86.33.143:9080/CB12EndowmentPolicy/Policy/",input$selected_customer,",",input$selected_policy,sep="")
                 , body = pc_json
                 , encode = "json")
      
      appData <- content(res)
      removeModal()
      if (appData$LGCMAREA$CA_RETURN_CODE != 0) {
        return("Update Unsuccessful")
      }
      else {
        shinyalert("Success", paste("Policy ID ", input$selected_policy, " successfully updated", sep=""), type = "success",confirmButtonCol = "#54BA60")
        urlname <- paste("http://192.86.33.143:9080/CB12EndowmentPolicy/Policy/",input$selected_customer,",",input$selected_policy,sep="")
        er_policy_data <- fromJSON(urlname)
        accountdata1 <- fromJSON(urlname)
        if ( !is.null(er_policy_data$LGCMAREA$CA_RETURN_CODE)) {
          if ( er_policy_data$LGCMAREA$CA_RETURN_CODE == 0) {
            ercommon <<- as.data.frame(er_policy_data$LGCMAREA$CA_POLICY_REQUEST$CA_POLICY_COMMON,stringsAsFactors=FALSE)
            erendowment <<- as.data.frame(er_policy_data$LGCMAREA$CA_POLICY_REQUEST$CA_ENDOWMENT,stringsAsFactors=FALSE)
            er_counter <<- er_counter + 1
            er_refresh(er_counter)
          }
        }
        
        if (refreshPolicy()) {
          newrefreshPolicy <- FALSE
          refreshPolicy(newrefreshPolicy)
        }
        else {
          newrefreshPolicy <- TRUE
          refreshPolicy(newrefreshPolicy)
        }
        return("Update Successful")
      }
    }
    else {
      return("Press Save Button to Update")
    }
    
  })
  
  
  
  
  observeEvent(input$delete_policy_request_er, {
    
    showModal(modalDialog(
      title = h4("Warning"),
      size ="m",
      "Do you want to Delete the policy",
      easyClose = FALSE,
      footer = tagList(
        modalButton("Cancel"),
        actionButton("ok_ed", "OK")
      )
    ))
    
  })
  
  observeEvent(input$ok_ed, {
    
    res <- DELETE(paste("http://192.86.33.143:9080/CB12EndowmentPolicy/Policy/",input$selected_customer,",",input$selected_policy,sep=""))
    removeModal()
    appData <- content(res)
    if (appData$LGCMAREA$CA_RETURN_CODE != 0) {
      shinyalert("Error", paste("Policy ID ", input$selected_policy, " delete failed", sep=""), type = "error",confirmButtonCol = "#E74C3C")
    }
    else {
      deleteok <<- TRUE
      if (refreshPolicy()) {
        newrefreshPolicy <- FALSE
        refreshPolicy(newrefreshPolicy)
      }
      else {
        newrefreshPolicy <- TRUE
        refreshPolicy(newrefreshPolicy)
      }
      shinyalert("Success", paste("Policy ID ", input$selected_policy, " successfully deleted", sep=""), type = "success",confirmButtonCol = "#54BA60")
    }
  })
################################    
  
  output$policy_message_request_hr <- renderText({
    
    showModal(modalDialog(
      title = "Please wait...",
      h4("Retrieving Policy"),
      easyClose = FALSE,
      footer = NULL
    ))
    
    
    urlname <- paste("http://192.86.33.143:9080/CB12HousePolicy/Policy/",input$selected_customer,",",input$selected_policy,sep="")
    hr_policy_data <- fromJSON(urlname)
    accountdata1 <- fromJSON(urlname)
    removeModal()
    if ( !is.null(hr_policy_data$LGCMAREA$CA_RETURN_CODE)) {
      if ( hr_policy_data$LGCMAREA$CA_RETURN_CODE == 0) {
        hrcommon <<- as.data.frame(hr_policy_data$LGCMAREA$CA_POLICY_REQUEST$CA_POLICY_COMMON,stringsAsFactors=FALSE)
        hrhouse <<- as.data.frame(hr_policy_data$LGCMAREA$CA_POLICY_REQUEST$CA_HOUSE,stringsAsFactors=FALSE)
        hr_counter <<- hr_counter + 1
        hr_refresh(hr_counter)
        return("Policy Details Retrived")
      }
      else {
        return(paste("Error retrieving - Return Code ", hr_policy_data$LGCMAREA$CA_RETURN_CODE, sep=""))
      }
    }
    else {
      return("API Error")
    }
  })
  
  output$display_customer_number_hr <- renderText({
    return(input$selected_customer)
  })
  
  output$display_policy_number_hr <- renderText({
    return(input$selected_policy)
  })
  
  output$display_policy_type_hr <- renderText({
    return(input$selected_type)
  })
  
  output$display_policy_start_date_hr <- renderText({
    hr_refresh()
    input$selected_type
    return(hrcommon$CA_ISSUE_DATE)
  })
  
  output$display_policy_expiry_date_hr <- renderText({
    hr_refresh()
    input$selected_type
    return(hrcommon$CA_EXPIRY_DATE)
  })
  
  output$display_propertytype_hr <- renderText({
    hr_refresh()
    input$selected_type
    return(hrhouse$CA_H_PROPERTY_TYPE)
  })
  
  output$display_housename_hr <- renderText({
    hr_refresh()
    input$selected_type
    return(hrhouse$CA_H_HOUSE_NAME)
  })
  
  output$display_housenumber_hr <- renderText({
    hr_refresh()
    input$selected_type
    return(hrhouse$CA_H_HOUSE_NUMBER)
  })
  
  output$display_postcode_hr <- renderText({
    hr_refresh()
    input$selected_type
    return(hrhouse$CA_H_POSTCODE)
  })
  
  output$display_numofbedrooms_hr <- renderText({
    hr_refresh()
    input$selected_type
    return(hrhouse$CA_H_BEDROOMS)
  })
  
  output$display_propertyvalue_hr <- renderText({
    hr_refresh()
    input$selected_type
    return(hrhouse$CA_H_VALUE)
  })
  
  
  output$show_claim_details_hr = DT::renderDataTable({
    
    z <- ReappData$c
    input$selected_type
    input$refreshPolicy
    input$refreshClaim
    d1 <- data.frame(CLAIMSTATUS=as.character(),CLAIMSTATUSCOUNT=as.integer(),stringsAsFactors=FALSE)
    
    query1 <- paste("select CLAIMNUMBER, OBSERVATIONS  FROM VCLAIM WHERE POLICYNUMBER = ",input$selected_policy,sep="")
    showModal(modalDialog(
      title = "Please wait...",
      h4("Retrieving Claims List"),
      easyClose = FALSE,
      footer = NULL
    ))
    
    res <- POST(paste(basemicroserviceurl,"getDVMzEUSDocker?",sep="")
                ,body=list(myquerry = query1),
                ,encode = "json")
    appData <- content(res)
    removeModal()
    if (length(appData) > 1) {
      if (appData[[2]][[2]] > 0 && length(appData[[1]]) != 0) {
        d1 <- as.data.frame(matrix(unlist(appData[[1]]), ncol=appData[[2]][[2]], byrow=TRUE), stringsAsFactors=FALSE)
        names(d1) <- names(data.frame(appData[[1]][[1]]))
        d1$CLAIMSTATUS <- ifelse(gsub(" ","",d1$OBSERVATIONS)=="","Open","Settled")
        d1$OBSERVATIONS <- NULL
        d1 <- d1 %>% group_by(CLAIMSTATUS) %>% summarise(CLAIMSTATUSCOUNT=n())
      }
    }
    
    DT::datatable(
      d1, class = 'cell-border stripe', rownames = FALSE, selection = "single",
      #caption = h4('Selected Item',style = 'caption-side: top; color:darkgreen ; font-Weight:bold'),
      extensions = list(FixedColumns = list(leftColumns = 1)),
      callback=DT::JS("table.on('click.dt', 'tr', function() {
                      var data=table.row(this).data();
                      Shiny.onInputChange('table_claimstatus_hr',data[0]);
  });"), 
    options = list (dom = 't',
                    columnDefs = list(list(className = 'dt-center', targets = c(0,1))),
                    headerCallback = JS(headerCallback8)
    )
  )%>%
      formatStyle(columns = c(1), fontSize = '130%', color = 'white', fontWeight = 'bold', backgroundColor = 'darkblue') %>%
      formatStyle(columns = c(2), fontSize = '130%', color = 'darkblue', fontWeight = 'bold')
  })
  
  headerCallback8 <- c(
    "function(thead, data, start, end, display){",
    "  $('th', thead).css('border-top', '2px solid darkblue');",
    "  $('th', thead).css('border-bottom', '2px solid darkblue');",
    "  $('th', thead).css('color', 'darkblue');",
    "}"
  )
  
  
  output$display_customer_number_hu <- renderText({
    return(input$selected_customer)
  })
  
  output$display_policy_number_hu <- renderText({
    return(input$selected_policy)
  })
  
  output$display_policy_type_hu <- renderText({
    return(input$selected_type)
  })
  
  output$display_policy_start_date_hu <- renderText({
    input$update_policy_request_hr
    return(hrcommon$CA_ISSUE_DATE)
  })
  
  output$update_policy_expiry_date_hu <- renderUI({
    input$update_policy_request_hr
    dateInput("accept_policy_expiry_date_hu", h4(strong("Expiry Date"), style="font-style: bold; color: darkblue"),format="yyyy-mm-dd",value = hrcommon$CA_EXPIRY_DATE)
  })
  
  output$update_propertytype_hu <- renderUI({
    input$update_policy_request_hr
    textInput("accept_propertytype_hu", h4(strong("Property Type"), style="font-style: bold; color: darkblue"),value = hrhouse$CA_H_PROPERTY_TYPE)  
  })
  
  output$update_housename_hu <- renderUI({
    input$update_policy_request_hr
    textInput("accept_housename_hu", h4(strong("House Name"), style="font-style: bold; color: darkblue"),value = hrhouse$CA_H_HOUSE_NAME)  
  })
  
  output$update_housenumber_hu <- renderUI({
    input$update_policy_request_hr
    textInput("accept_housenumber_hu", h4(strong("House Number"), style="font-style: bold; color: darkblue"),value = hrhouse$CA_H_HOUSE_NUMBER)  
  })
  
  output$update_postcode_hu <- renderUI({
    input$update_policy_request_hr
    textInput("accept_postcode_hu", h4(strong("Post Code"), style="font-style: bold; color: darkblue"),value = hrhouse$CA_H_POSTCODE)  
  })
  
  output$update_numofbedrooms_hu <- renderUI({
    input$update_policy_request_hr
    numericInput("accept_numofbedrooms_hu", h4(strong("Number of Bedrooms"), style="font-style: bold; color: darkblue"),value = hrhouse$CA_H_BEDROOMS)
  })
  
  output$update_propertyvalue_hu <- renderUI({
    input$update_policy_request_hr
    numericInput("accept_propertyvalue_hu", h4(strong("Property Value"), style="font-style: bold; color: darkblue"),value = hrhouse$CA_H_VALUE)
  })
  
  output$policy_save_request_hu <- renderText({
    
    
    if (input$save_policy_request_hu > save_policy_request_hu_pressed) {
      save_policy_request_hu_pressed <<- input$save_policy_request_hu
      #op <- options(digits.secs = 6)
      #savetime <- format(Sys.time(),tz="UTC")
      pc_json <- list(
        LGCMAREA = list(
          CA_POLICY_REQUEST = list(
            CA_POLICY_COMMON = list(
              CA_ISSUE_DATE = hrcommon$CA_ISSUE_DATE,
              CA_EXPIRY_DATE = input$accept_policy_expiry_date_hu,
              CA_LASTCHANGED = hrcommon$CA_LASTCHANGED
            ),
            CA_HOUSE = list(
              CA_H_PROPERTY_TYPE = input$accept_propertytype_hu,
              CA_H_HOUSE_NAME = input$accept_housename_hu,
              CA_H_HOUSE_NUMBER = input$accept_housenumber_hu,
              CA_H_POSTCODE = input$accept_postcode_hu,
              CA_H_BEDROOMS = input$accept_numofbedrooms_hu,
              CA_H_VALUE = input$accept_propertyvalue_hu
            )
          )
        )
      )
      
      showModal(modalDialog(
        title = "Please wait...",
        h4("Saving Policy"),
        easyClose = FALSE,
        footer = NULL
      ))
      
      res <- PUT(paste("http://192.86.33.143:9080/CB12HousePolicy/Policy/",input$selected_customer,",",input$selected_policy,sep="")
                 , body = pc_json
                 , encode = "json")
      
      appData <- content(res)
      removeModal()
      if (appData$LGCMAREA$CA_RETURN_CODE != 0) {
        return("Update Unsuccessful")
      }
      else {
        shinyalert("Success", paste("Policy ID ", input$selected_policy, " successfully updated", sep=""), type = "success",confirmButtonCol = "#54BA60")
        urlname <- paste("http://192.86.33.143:9080/CB12HousePolicy/Policy/",input$selected_customer,",",input$selected_policy,sep="")
        hr_policy_data <- fromJSON(urlname)
        accountdata1 <- fromJSON(urlname)
        if ( !is.null(hr_policy_data$LGCMAREA$CA_RETURN_CODE)) {
          if ( hr_policy_data$LGCMAREA$CA_RETURN_CODE == 0) {
            hrcommon <<- as.data.frame(hr_policy_data$LGCMAREA$CA_POLICY_REQUEST$CA_POLICY_COMMON,stringsAsFactors=FALSE)
            hrhouse <<- as.data.frame(hr_policy_data$LGCMAREA$CA_POLICY_REQUEST$CA_HOUSE,stringsAsFactors=FALSE)
            hr_counter <<- hr_counter + 1
            hr_refresh(hr_counter)
          }
        }
        
        if (refreshPolicy()) {
          newrefreshPolicy <- FALSE
          refreshPolicy(newrefreshPolicy)
        }
        else {
          newrefreshPolicy <- TRUE
          refreshPolicy(newrefreshPolicy)
        }
        return("Update Successful")
      }
    }
    else {
      return("Press Save Button to Update")
    }
    
  })
  
  
  
  
  observeEvent(input$delete_policy_request_hr, {
    
    showModal(modalDialog(
      title = h4("Warning"),
      size ="m",
      "Do you want to Delete the policy",
      easyClose = FALSE,
      footer = tagList(
        modalButton("Cancel"),
        actionButton("ok_hd", "OK")
      )
    ))
    
  })
  
  observeEvent(input$ok_hd, {
    
    res <- DELETE(paste("http://192.86.33.143:9080/CB12HousePolicy/Policy/",input$selected_customer,",",input$selected_policy,sep=""))
    removeModal()
    appData <- content(res)
    if (appData$LGCMAREA$CA_RETURN_CODE != 0) {
      shinyalert("Error", paste("Policy ID ", input$selected_policy, " delete failed", sep=""), type = "error",confirmButtonCol = "#E74C3C")
    }
    else {
      deleteok <<- TRUE
      if (refreshPolicy()) {
        newrefreshPolicy <- FALSE
        refreshPolicy(newrefreshPolicy)
      }
      else {
        newrefreshPolicy <- TRUE
        refreshPolicy(newrefreshPolicy)
      }
      shinyalert("Success", paste("Policy ID ", input$selected_policy, " successfully deleted", sep=""), type = "success",confirmButtonCol = "#54BA60")
    }
  })
  
##################################
  
  output$policy_message_request_cr <- renderText({
    
    showModal(modalDialog(
      title = "Please wait...",
      h4("Retrieving Policy"),
      easyClose = FALSE,
      footer = NULL
    ))
    
    
    urlname <- paste("http://192.86.33.143:9080/CB12CommercialPolicy/Policy/",input$selected_customer,",",input$selected_policy,sep="")
    cr_policy_data <- fromJSON(urlname)
    accountdata1 <- fromJSON(urlname)
    removeModal()
    if ( !is.null(cr_policy_data$LGCMAREA$CA_RETURN_CODE)) {
      if ( cr_policy_data$LGCMAREA$CA_RETURN_CODE == 0) {
        crcommon <<- as.data.frame(cr_policy_data$LGCMAREA$CA_POLICY_REQUEST$CA_POLICY_COMMON,stringsAsFactors=FALSE)
        crcommercial <<- as.data.frame(cr_policy_data$LGCMAREA$CA_POLICY_REQUEST$CA_COMMERCIAL,stringsAsFactors=FALSE)
        cr_counter <<- cr_counter + 1
        cr_refresh(cr_counter)
        return("Policy Details Retrived")
      }
      else {
        return(paste("Error retrieving - Return Code ", cr_policy_data$LGCMAREA$CA_RETURN_CODE, sep=""))
      }
    }
    else {
      return("API Error")
    }
  })
  
  output$display_customer_number_cr <- renderText({
    return(input$selected_customer)
  })
  
  output$display_policy_number_cr <- renderText({
    return(input$selected_policy)
  })
  
  output$display_policy_type_cr <- renderText({
    return(input$selected_type)
  })
  
  output$display_policy_start_date_cr <- renderText({
    cr_refresh()
    input$selected_type
    return(crcommon$CA_ISSUE_DATE)
  })
  
  output$display_policy_expiry_date_cr <- renderText({
    cr_refresh()
    input$selected_type
    return(crcommon$CA_EXPIRY_DATE)
  })
  
  output$display_CustomerName_cr <- renderText({
    cr_refresh()
    input$selected_type
    return(crcommercial$CA_B_CUSTOMER)
  })
  
  output$display_PropertyType_cr <- renderText({
    cr_refresh()
    input$selected_type
    return(crcommercial$CA_B_PROPTYPE)
  })
  
  output$display_FirePeril_cr <- renderText({
    cr_refresh()
    input$selected_type
    return(crcommercial$CA_B_FIREPERIL)
  })
  
  output$display_FirePremium_cr <- renderText({
    cr_refresh()
    input$selected_type
    return(crcommercial$CA_B_FIREPREMIUM)
  })
  
  output$display_PropertyAddress_cr <- renderText({
    cr_refresh()
    input$selected_type
    return(crcommercial$CA_B_ADDRESS)
  })
  
  output$display_CrimePeril_cr <- renderText({
    cr_refresh()
    input$selected_type
    return(crcommercial$CA_B_CRIMEPERIL)
  })
  
  output$display_CrimePremium_cr <- renderText({
    cr_refresh()
    input$selected_type
    return(crcommercial$CA_B_CRIMEPREMIUM)
  })
  
  output$display_PostCode_cr <- renderText({
    cr_refresh()
    input$selected_type
    return(crcommercial$CA_B_POSTCODE)
  })
  
  output$display_Status_cr <- renderText({
    cr_refresh()
    input$selected_type
    return(crcommercial$CA_B_STATUS)
  })
  
  output$display_RejectReason_cr <- renderText({
    cr_refresh()
    input$selected_type
    return(crcommercial$CA_B_REJECTREASON)
  })
  
  output$display_FloodPeril_cr <- renderText({
    cr_refresh()
    input$selected_type
    return(crcommercial$CA_B_FLOODPERIL)
  })
  
  output$display_FloodPremium_cr <- renderText({
    cr_refresh()
    input$selected_type
    return(crcommercial$CA_B_FLOODPREMIUM)
  })
  
  
  output$display_LocationLat_cr <- renderText({
    cr_refresh()
    input$selected_type
    return(crcommercial$CA_B_LATITUDE)
  })
  
  output$display_LocationLong_cr <- renderText({
    cr_refresh()
    input$selected_type
    return(crcommercial$CA_B_LONGITUDE)
  })
  
  output$display_WeatherPeril_cr <- renderText({
    cr_refresh()
    input$selected_type
    return(crcommercial$CA_B_WEATHERPERIL)
  })
  
  output$display_WeatherPremium_cr <- renderText({
    cr_refresh()
    input$selected_type
    return(crcommercial$CA_B_WEATHERPREMIUM)
  })
  
  
  output$show_claim_details_cr = DT::renderDataTable({
    
    z <- ReappData$c
    input$selected_type
    input$refreshPolicy
    input$refreshClaim
    d1 <- data.frame(CLAIMSTATUS=as.character(),CLAIMSTATUSCOUNT=as.integer(),stringsAsFactors=FALSE)
    
    query1 <- paste("select CLAIMNUMBER, OBSERVATIONS  FROM VCLAIM WHERE POLICYNUMBER = ",input$selected_policy,sep="")
    showModal(modalDialog(
      title = "Please wait...",
      h4("Retrieving Claims List"),
      easyClose = FALSE,
      footer = NULL
    ))
    
    res <- POST(paste(basemicroserviceurl,"getDVMzEUSDocker?",sep="")
                ,body=list(myquerry = query1),
                ,encode = "json")
    appData <- content(res)
    removeModal()
    if (length(appData) > 1) {
      if (appData[[2]][[2]] > 0 && length(appData[[1]]) != 0) {
        d1 <- as.data.frame(matrix(unlist(appData[[1]]), ncol=appData[[2]][[2]], byrow=TRUE), stringsAsFactors=FALSE)
        names(d1) <- names(data.frame(appData[[1]][[1]]))
        d1$CLAIMSTATUS <- ifelse(gsub(" ","",d1$OBSERVATIONS)=="","Open","Settled")
        d1$OBSERVATIONS <- NULL
        d1 <- d1 %>% group_by(CLAIMSTATUS) %>% summarise(CLAIMSTATUSCOUNT=n())
      }
    }
    
    DT::datatable(
      d1, class = 'cell-border stripe', rownames = FALSE, selection = "single",
      #caption = h4('Selected Item',style = 'caption-side: top; color:darkgreen ; font-Weight:bold'),
      extensions = list(FixedColumns = list(leftColumns = 1)),
      callback=DT::JS("table.on('click.dt', 'tr', function() {
                      var data=table.row(this).data();
                      Shiny.onInputChange('table_claimstatus_cr',data[0]);
  });"), 
    options = list (dom = 't',
                    columnDefs = list(list(className = 'dt-center', targets = c(0,1))),
                    headerCallback = JS(headerCallback9)
    )
  )%>%
      formatStyle(columns = c(1), fontSize = '130%', color = 'white', fontWeight = 'bold', backgroundColor = 'darkblue') %>%
      formatStyle(columns = c(2), fontSize = '130%', color = 'darkblue', fontWeight = 'bold')
  })
  
  headerCallback9 <- c(
    "function(thead, data, start, end, display){",
    "  $('th', thead).css('border-top', '2px solid darkblue');",
    "  $('th', thead).css('border-bottom', '2px solid darkblue');",
    "  $('th', thead).css('color', 'darkblue');",
    "}"
  )
  
  
  observeEvent(input$delete_policy_request_cr, {
    
    showModal(modalDialog(
      title = h4("Warning"),
      size ="m",
      "Do you want to Delete the policy",
      easyClose = FALSE,
      footer = tagList(
        modalButton("Cancel"),
        actionButton("ok_cd", "OK")
      )
    ))
    
  })
  
  observeEvent(input$ok_cd, {
    
    res <- DELETE(paste("http://192.86.33.143:9080/CB12CommercialPolicy/Policy/",input$selected_customer,",",input$selected_policy,sep=""))
    removeModal()
    appData <- content(res)
    if (appData$LGCMAREA$CA_RETURN_CODE != 0) {
      shinyalert("Error", paste("Policy ID ", input$selected_policy, " delete failed", sep=""), type = "error",confirmButtonCol = "#E74C3C")
    }
    else {
      deleteok <<- TRUE
      if (refreshPolicy()) {
        newrefreshPolicy <- FALSE
        refreshPolicy(newrefreshPolicy)
      }
      else {
        newrefreshPolicy <- TRUE
        refreshPolicy(newrefreshPolicy)
      }
      shinyalert("Success", paste("Policy ID ", input$selected_policy, " successfully deleted", sep=""), type = "success",confirmButtonCol = "#54BA60")
    }
  })
###############################
  
  output$show_claim_details = DT::renderDataTable({
    
    z <- ReappData$c
    input$accept_claim_policy
    input$refreshPolicy
    input$refreshClaim
    refreshClaim()
    
    d1 <- data.frame(CLAIMNUMBER = as.integer(), POLICYNUMBER=as.integer(),CLAIMDATE=as.character(),CAUSE=as.character(),STATUS=as.character(),stringsAsFactors=FALSE)
    
    # converted_policy_type <- substr(input$selected_insurance_type,1,1)
    # if (converted_policy_type == "A") {
    #   converted_policy_type <- ""
    # }
    
    if (input$accept_claim_number == 0) {
      if (input$accept_claim_policy == 0) {
        query1 <- paste("SELECT * FROM VCLAIM",sep="")
      }
      else {
        query1 <- paste("SELECT * FROM VCLAIM WHERE POLICYNUMBER = ",input$accept_claim_policy,sep="")
      }
    }
    else {
      query1 <- paste ("sELECT * FROM VCLAIM WHERE CLAIMNUMBER = ",accept_claim_number,sep="")
    }
    
    
    showModal(modalDialog(
      title = "Please wait...",
      h4("Retrieving Claim List"),
      easyClose = FALSE,
      footer = NULL
    ))
    
    
    res <- POST(paste(basemicroserviceurl,"getDVMzEUSDocker?",sep="")
                ,body=list(myquerry = query1),
                ,encode = "json")
    appData <- content(res)
    
    if (length(appData) > 1) {
      if (appData[[2]][[2]] > 0 && length(appData[[1]]) != 0) {
        d1 <- as.data.frame(matrix(unlist(appData[[1]]), ncol=appData[[2]][[2]], byrow=TRUE), stringsAsFactors=FALSE)
        names(d1) <- names(data.frame(appData[[1]][[1]]))
        numrows <- nrow(d1)
        obs <- str_split(gsub("\\|","\\@",d1$OBSERVATIONS),"@@@")
        for (i in 1:length(obs)) {
          if (length(obs[[i]])==1) {
            obs[[i]] <- c("","")
          }
        }
        y <- trimws(unlist(obs),"both")
        dim(y) <- c(2,numrows)
        obs <- data.frame(t(y),stringsAsFactors = FALSE)
        names(obs) <- c("OBSERVATIONS","EVIDENCE")
        obs$STATUS <- ifelse(obs$OBSERVATIONS == "" & obs$EVIDENCE == "","Open","Settled")
        d1 <- cbind(d1,obs)
        d1$OBSERVATIONS <- NULL
        d1$CAUSE <- trimws(d1$CAUSE,"both")
        saveclaimtable <<- d1
        d1 <- d1[,c(1,2,3,6,9)]
        if (input$selected_claim_status != "All") {
          d1 <- d1[d1$STATUS == input$selected_claim_status,]
        }
      }
    }
    removeModal()
    
    DT::datatable(
      d1, class = 'cell-border stripe', rownames = FALSE, selection = "single",
      #caption = h4('Selected Item',style = 'caption-side: top; color:darkgreen ; font-Weight:bold'),
      extensions = list(FixedColumns = list(leftColumns = 1)),
      callback=DT::JS("table.on('click.dt', 'tr', function() {
                      var data=table.row(this).data();
                      Shiny.onInputChange('selected_claim',data[0]);
  });"), 
      options = list (dom = 'tp',
                      columnDefs = list(list(className = 'dt-center', targets = c(0,2))), pageLength = 5,
                      headerCallback = JS(headerCallback11)
  )
)%>%
    formatStyle(columns = c(1), fontSize = '120%', color = 'white', backgroundColor = 'darkgreen') %>%
    formatStyle(columns = c(2), fontSize = '120%', color = 'white', backgroundColor = 'darkgreen') %>%
    formatStyle(columns = c(3), fontSize = '120%', color = 'darkgreen') %>%
    formatStyle(columns = c(4), fontSize = '120%', color = 'darkgreen') %>%
    formatStyle(columns = c(5), fontSize = '120%', color = 'white', backgroundColor = 'darkgreen')
  })

headerCallback11 <- c(
  "function(thead, data, start, end, display){",
  "  $('th', thead).css('border-top', '2px solid darkgreen');",
  "  $('th', thead).css('border-bottom', '2px solid darkgreen');",
  "  $('th', thead).css('color', 'darkgreen');",
  "}"
)

output$claim_message_request_claim <- renderText({
  
  if (is.null(input$selected_claim)) {
    return("")
  }
  showModal(modalDialog(
    title = "Please wait...",
    h4("Retrieving Claim"),
    easyClose = FALSE,
    footer = NULL
  ))
  selectedclaimdata <<- saveclaimtable[saveclaimtable$CLAIMNUMBER == input$selected_claim,]
  removeModal()
  if ( nrow(selectedclaimdata) != 0 ) {
      return("Claim Details Retrived")
    }
    else {
      return("Claim Details Retrieval Error")
    }
})


output$policy_clicked_message <- renderText({
  
  input$selected_claim
  
  if (is.null(input$selected_claim)) {
    return("")
  }
  else {
    return(".")
  }

})





output$display_policy_number_claim <- renderText({
  return(saveclaimtable[saveclaimtable$CLAIMNUMBER == input$selected_claim,]$POLICYNUMBER)
})

output$display_claim_number_claim <- renderText({
  return(input$selected_claim)
})

output$display_claim_date_claim <- renderText({
  return(saveclaimtable[saveclaimtable$CLAIMNUMBER == input$selected_claim,]$CLAIMDATE)
})

output$display_cause_claim <- renderText({
  return(saveclaimtable[saveclaimtable$CLAIMNUMBER == input$selected_claim,]$CAUSE)
})

output$display_observation_claim <- renderText({
  return(saveclaimtable[saveclaimtable$CLAIMNUMBER == input$selected_claim,]$OBSERVATIONS)
})

output$display_claimedamount_claim <- renderText({
  return(saveclaimtable[saveclaimtable$CLAIMNUMBER == input$selected_claim,]$VALUE)
})

output$display_settledamount_claim <- renderText({
  return(saveclaimtable[saveclaimtable$CLAIMNUMBER == input$selected_claim,]$PAID)
})

output$display_status_claim <- renderText({
  return(saveclaimtable[saveclaimtable$CLAIMNUMBER == input$selected_claim,]$STATUS)
})

#output$display_evidence_claim <- renderText({
#  return(saveclaimtable[saveclaimtable$CLAIMNUMBER == input$selected_claim,]$EVIDENCE)
#})

output$display_evidence_claim<- renderUI({
  imgurl2 <- saveclaimtable[saveclaimtable$CLAIMNUMBER == input$selected_claim,]$EVIDENCE
  if(imgurl2==""){
    tags$img(src='NotFoundtp.png', width = 250, height = 175)
  }
  else {
    tags$img(src=imgurl2, height = 375, width = 250)
  }
})


output$select_claim_policy <- renderUI({
  input$table_claimstatus_mr
  input$table_claimstatus_er
  input$table_claimstatus_hr
  input$table_claimstatus_cr
  numericInput("accept_claim_policy", h4(strong("Policy Number"), style="font-style: bold; color: darkblue"),value = ifelse(is.null(input$selected_policy),0,input$selected_policy))
})

output$select_claim_status <- renderUI({
  input$table_claimstatus_mr
  input$table_claimstatus_er
  input$table_claimstatus_hr
  input$table_claimstatus_cr
  selectInput("selected_claim_status", h4(strong("Select Insurance Type"), style="font-style: bold; color: darkgreen"), choices=c("All","Open","Settled"),selected = ifelse(savedstatus == "","All",savedstatus))
})

observeEvent(input$table_claimstatus_mr, {
  savedstatus <<- input$table_claimstatus_mr
  updateTabItems(session, "tabs", "dashboard3")
})
observeEvent(input$table_claimstatus_er, {
  savedstatus <<- input$table_claimstatus_er
  updateTabItems(session, "tabs", "dashboard3")
})
observeEvent(input$table_claimstatus_hr, {
  savedstatus <<- input$table_claimstatus_hr
  updateTabItems(session, "tabs", "dashboard3")
})
observeEvent(input$table_claimstatus_cr, {
  savedstatus <<- input$table_claimstatus_cr
  updateTabItems(session, "tabs", "dashboard3")
})

  
  
  
  
  
  
#############################################

  observeEvent(input$ViewArchitecture, {
    
     showModal(modalDialog(
       title = h4("Architecture"),
       size ="l",
       HTML('<img src="GenAppsArchitecturev1.jpg" width="860">'),
       easyClose = FALSE,
       footer = modalButton("Dismiss")
     ))
    
    #shinyalert("", imageUrl = "P&PArchitecture.jpg", imageWidth = 1000,imageHeight = 1000, type = "info",confirmButtonCol = "#3F27B3")
  
  })
  

    
})

shinyApp(ui = ui, server = server)