library(shiny)
library(shinyjs)
#library(shinysky)
library(DT)
library(data.table)
library(utils)
#library(lubridate)
library(shinyalert)
useShinyalert()
# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Reference Implementation Server Management"),
  ### This is to adjust the width of pop up "showmodal()" for DT modify table 
  tags$head(tags$style(HTML('

                            .modal-lg {
                            width: 1200px;
                            }
                            '))),
  helpText("Note: Remember to save any updates and refresh the Plumber Microservice Server!"),
  br(),
  ### tags$head() is to customize the download button
  tags$head(tags$style(".butt{background-color:#230682;} .butt{color: #e6ebef;}")),
  #downloadButton("Trich_csv", "Download in CSV", class="butt"),
  useShinyalert(), # Set up shinyalert
  uiOutput("MainBody_trich"),actionButton(inputId = "Updated_trich",label = "Save")
))
