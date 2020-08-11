# plumber.R
library(httr)
library(jsonlite)
library(stringr)
library(DBI)
library(RJDBC)
library(rJava)
library(gmailr)
library(pander)
library(dplyr)


#* Echo back the input
#* @param msg The message to echo
#* @get /echo
function(msg=""){
  list(msg = paste0("The message is: '", msg, "'"))
}

#* Plot a histogram
#* @png
#* @get /plot2
function(){
  rand <- rnorm(100)
  hist(rand)
}

#* Return the sum of two numbers
#* @param a The first number to add
#* @param b The second number to add
#* @post /sum2
function(a, b){
  as.numeric(a) + as.numeric(b)
}


#* Send Gmail
#* @param myentity Entity Type Customer or Policy or Claim
#* @param operation Operation created deleted updated settled
#* @param reference Customer num or Policy Num or Claim Num
#* @post /sendgmailGENApps
function(myentity, operation, reference){
  
  basemicroserviceurl <<- "http://localhost:8000/"

  email_qry <- ifelse(myentity=="Claim", paste("select firstname, lastname, emailaddress from vcustomer A, vpolicy B, vclaim C where A.CUSTOMERNUMBER = B.CUSTOMERNUMBER and B.POLICYNUMBER = C.POLICYNUMBER AND C.CLAIMNUMBER = ",reference,sep=""),
                      ifelse(myentity=="Policy",paste("select firstname, lastname, emailaddress from vcustomer A, vpolicy B where A.CUSTOMERNUMBER = B.CUSTOMERNUMBER and B.POLICYNUMBER = ",reference,sep=""),
                             paste("select firstname, lastname, emailaddress from vcustomer where CUSTOMERNUMBER = ",reference,sep="")))

  print("*******")
  print(email_qry)
    
  res <- POST(paste(basemicroserviceurl,"getDVMzEUSDocker?",sep="")
              ,body=list(myquerry = email_qry),
              ,encode = "json")
  
  appPol <- content(res)
  print(appPol)
  if (length(appPol) > 1) {
    if (appPol[[2]][[2]] > 0 && length(appPol[[1]]) != 0) {
      To_email_id <-  trimws(appPol[[1]][[1]]$EMAILADDRESS)
      First_Name <- trimws(appPol[[1]][[1]]$FIRSTNAME)
      Last_Name <- trimws(appPol[[1]][[1]]$LASTNAME)
    }
  }
  
  data_qry <- ifelse(myentity=="Claim", paste("select * from vclaim C where CLAIMNUMBER = ",reference,sep=""),
                     ifelse(myentity=="Policy",paste("select * from vpolicy C where POLICYNUMBER = ",reference,sep=""),
                            paste("select * from vcustomer where CUSTOMERNUMBER = ",reference,sep="")))
  
  print(data_qry)
  resdata <- POST(paste(basemicroserviceurl,"getDVMzEUSDocker?",sep="")
                  ,body=list(myquerry = data_qry),
                  ,encode = "json")
  
  appData <- content(resdata)
  print(appData)
  if (length(appData) > 1) {
    if (appData[[2]][[2]] > 0 && length(appData[[1]]) != 0) {
      d1 <- as.data.frame(matrix(unlist(appData[[1]]), ncol=appData[[2]][[2]], byrow=TRUE), stringsAsFactors=FALSE)
      names(d1) <- names(data.frame(appData[[1]][[1]]))
    }
  }
  
  
  if (myentity=="Policy") {
    if (d1$POLICYTYPE == "M") {
      policy_data_qry <- paste("select * from VMOTOR C where POLICYNUMBER = ",reference,sep="")
      policy_type <- "Motor"
    }
    if (d1$POLICYTYPE == "E") {
      policy_data_qry <- paste("select * from VENDOWMENT C where POLICYNUMBER = ",reference,sep="")
      policy_type <- "Endowment"
    }
    if (d1$POLICYTYPE == "H") {
      policy_data_qry <- paste("select * from VHOUSE C where POLICYNUMBER = ",reference,sep="")
      policy_type <- "House"
    }
    if (d1$POLICYTYPE == "C") {
      policy_data_qry <- paste("select * from VCOMMERCIAL C where POLICYNUMBER = ",reference,sep="")
      policy_type <- "Commercial"
    }
    print(policy_data_qry)
    respolicydata <- POST(paste(basemicroserviceurl,"getDVMzEUSDocker?",sep="")
                          ,body=list(myquerry = policy_data_qry),
                          ,encode = "json")
    
    appData <- content(respolicydata)
    print(appData)
    if (length(appData) > 1) {
      if (appData[[2]][[2]] > 0 && length(appData[[1]]) != 0) {
        d2 <- as.data.frame(matrix(unlist(appData[[1]]), ncol=appData[[2]][[2]], byrow=TRUE), stringsAsFactors=FALSE)
        names(d2) <- names(data.frame(appData[[1]][[1]]))
      }
    }
    d1 <- left_join(d1,d2)
  } else {
    policy_type <- ""
  }
  
  
  
  salutation <- paste("Dear ",First_Name," ", Last_Name,",",sep="")
  
  welcome_message <- "General Insurance App welcomes you as a new customer."
  
  header_message <- paste("Your",policy_type,myentity,"record #", reference,"is",operation,sep=" ")
  
  #gm_auth_configure(path  = "/srv/shiny-server/genapps.json")
  options(
    gargle_oauth_cache = "/srv/shiny-server/.secretgenapps",
    gargle_oauth_email = "gen.apps.insurance@gmail.com"
  )
  gm_auth(email = "gen.apps.insurance@gmail.com")
  
  print("Authorized")
  
  d1[1,] <- trimws(d1[1,],"both")
  d1 <- as.data.frame(t(d1))
  names(d1) <- "Record"
  
  msg_table <- pander_return(d1,style="grid")
  
  if(myentity == "Customer" && operation == "created") {
    latest_msg <- unlist(list(salutation," ",welcome_message, " ",header_message,msg_table))  
    title_message <- "Welcome to GenApps Insurance"
  } else {
    latest_msg <- unlist(list(salutation," ",header_message,msg_table))
    title_message <- paste(policy_type,myentity,reference,"is",operation,sep=" ")
  }
  latest_msg <- gsub("&nbsp;","Field ",latest_msg)
  latest_msg <- gsub("PHONEHOME","BANK ACCN",latest_msg)
  
  #html_msg_text <- paste(html_msg_text,x2,sep="")
  
  my_email_message <- gm_mime() %>%
    gm_to("shami.gupta@in.ibm.com") %>%
    gm_from("General Insurance Apps (gen.apps.insurance@gmail.com)") %>%
    gm_subject(title_message) %>%
    gm_text_body(paste(latest_msg,collapse = "\n"))
  #  gm_html_body(html_msg_text) 
  #  gm_html_body('<h1>A plot of <b>MotorTrend</b> data <i>(1974)</i></h1><br><img src="cid:foobar">') %>%
  #  gm_attach_file("mtcars.png", id = "foobar")  

  print("Mail ready to send")
  
  gm_send_message(my_email_message)
  output <- "Mail Sent"
}
