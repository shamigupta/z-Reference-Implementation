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
#* @get /echo2
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
#* @post /sendgmailGENAppsOld
function(myentity, operation, reference){
  
  readRenviron("/srv/shiny-server/.env")
  basemicroserviceurl <<- "http://localhost:8000/"

  if (operation == "deleted") {
    inputarray <- unlist(strsplit(reference,"-"))
    reference <- inputarray[1]
    referencecustomer <- inputarray[2]
    email_qry <- paste("select firstname, lastname, emailaddress from vcustomer where CUSTOMERNUMBER = ",referencecustomer,sep="")
    print_policy_type = ""
  } else {
    email_qry <- ifelse(myentity=="Claim", paste("select firstname, lastname, emailaddress from vcustomer A, vpolicy B, vclaim C where A.CUSTOMERNUMBER = B.CUSTOMERNUMBER and B.POLICYNUMBER = C.POLICYNUMBER AND C.CLAIMNUMBER = ",reference,sep=""),
                        ifelse(myentity=="Policy",paste("select firstname, lastname, emailaddress from vcustomer A, vpolicy B where A.CUSTOMERNUMBER = B.CUSTOMERNUMBER and B.POLICYNUMBER = ",reference,sep=""),
                               paste("select firstname, lastname, emailaddress from vcustomer where CUSTOMERNUMBER = ",reference,sep="")))
  }
  res <- POST(paste(basemicroserviceurl,"getDVMzEUSDocker?",sep="")
              ,body=list(myquerry = email_qry),
              ,encode = "json")
  
  appPol <- content(res)
  if (length(appPol) > 1) {
    if (appPol[[2]][[2]] > 0 && length(appPol[[1]]) != 0) {
      To_email_id <-  trimws(appPol[[1]][[1]]$EMAILADDRESS)
      First_Name <- trimws(appPol[[1]][[1]]$FIRSTNAME)
      Last_Name <- trimws(appPol[[1]][[1]]$LASTNAME)
    }
  }
  
  if (myentity == "Customer") {
    urlname <- paste(Sys.getenv("MainframeIP"),":",Sys.getenv("zConnectPort"),"/CB12Customer/customer/",reference,sep="")
    accountdata <- fromJSON(urlname)
    if (accountdata$LGCMAREA$CA_RETURN_CODE == 0) {
      d1 <- as.data.frame(accountdata$LGCMAREA$CA_CUSTOMER_REQUEST,stringsAsFactors=FALSE)
      names(d1) <- gsub("CA_PHONE_HOME","CA_BANK_ACT",names(d1))
      pcount_qry <- paste("SELECT COUNT(*) AS COUNT from VPOLICY WHERE CUSTOMERNUMBER = ",reference,sep="")
      res <- POST(paste(basemicroserviceurl,"getDVMzEUSDocker?",sep="")
                  ,body=list(myquerry = pcount_qry),
                  ,encode = "json")
      appPol <- content(res)
      if (length(appPol) > 1) {
        if (appPol[[2]][[2]] > 0 && length(appPol[[1]]) != 0) {
          d1$CA_NUM_POLICIES <-  trimws(appPol[[1]][[1]]$COUNT)
        }
      }
    }
    print_policy_type = ""
  }
  
  if (myentity == "Policy") {
    data_qry <- paste("select CUSTOMERNUMBER, POLICYNUMBER, POLICYTYPE from vpolicy where POLICYNUMBER = ",reference,sep="")
    res <- POST(paste(basemicroserviceurl,"getDVMzEUSDocker?",sep="")
                ,body=list(myquerry = data_qry),
                ,encode = "json")
    appPol <- content(res)
    if (length(appPol) > 1) {
      if (appPol[[2]][[2]] > 0 && length(appPol[[1]]) != 0) {
        customernumber <-  trimws(appPol[[1]][[1]]$CUSTOMERNUMBER)
        policy_type <- trimws(appPol[[1]][[1]]$POLICYTYPE)
        if (policy_type == "M") {
          print_policy_type = "Motor"
          urlname <- paste(Sys.getenv("MainframeIP"),":",Sys.getenv("zConnectPort"),"/CB12MotorPolicy/Policy/",customernumber,",",reference,sep="")
          mr_policy_data <- fromJSON(urlname)
          if ( mr_policy_data$LGCMAREA$CA_RETURN_CODE == 0) {
            mrcommon <- as.data.frame(mr_policy_data$LGCMAREA$CA_POLICY_REQUEST$CA_POLICY_COMMON,stringsAsFactors=FALSE)
            mrmotor <- as.data.frame(mr_policy_data$LGCMAREA$CA_POLICY_REQUEST$CA_MOTOR,stringsAsFactors=FALSE)
            d1 <- cbind(mrcommon,mrmotor)
          }
        }
        if (policy_type == "E") {
          print_policy_type = "Endowment"
          urlname <- paste(Sys.getenv("MainframeIP"),":",Sys.getenv("zConnectPort"),"/CB12EndowmentPolicy/Policy/",customernumber,",",reference,sep="")
          er_policy_data <- fromJSON(urlname)
          if ( er_policy_data$LGCMAREA$CA_RETURN_CODE == 0) {
            ercommon <- as.data.frame(er_policy_data$LGCMAREA$CA_POLICY_REQUEST$CA_POLICY_COMMON,stringsAsFactors=FALSE)
            erendowment <- as.data.frame(er_policy_data$LGCMAREA$CA_POLICY_REQUEST$CA_ENDOWMENT,stringsAsFactors=FALSE)
            d1 <- cbind(ercommon,erendowment)
          }
        }
        if (policy_type == "H") {
          print_policy_type = "House"
          urlname <- paste(Sys.getenv("MainframeIP"),":",Sys.getenv("zConnectPort"),"/CB12HousePolicy/Policy/",customernumber,",",reference,sep="")
          hr_policy_data <- fromJSON(urlname)
          if ( hr_policy_data$LGCMAREA$CA_RETURN_CODE == 0) {
            hrcommon <- as.data.frame(hr_policy_data$LGCMAREA$CA_POLICY_REQUEST$CA_POLICY_COMMON,stringsAsFactors=FALSE)
            hrhouse <- as.data.frame(hr_policy_data$LGCMAREA$CA_POLICY_REQUEST$CA_HOUSE,stringsAsFactors=FALSE)
            d1 <- cbind(hrcommon,hrhouse)
          }
        }
        if (policy_type == "C") {
          print_policy_type = "Commercial"
          urlname <- paste(Sys.getenv("MainframeIP"),":",Sys.getenv("zConnectPort"),"/CB12CommercialPolicy/Policy/",customernumber,",",reference,sep="")
          cr_policy_data <- fromJSON(urlname)
          if ( cr_policy_data$LGCMAREA$CA_RETURN_CODE == 0) {
            crcommon <- as.data.frame(cr_policy_data$LGCMAREA$CA_POLICY_REQUEST$CA_POLICY_COMMON,stringsAsFactors=FALSE)
            crcommercial <- as.data.frame(cr_policy_data$LGCMAREA$CA_POLICY_REQUEST$CA_COMMERCIAL,stringsAsFactors=FALSE)
            d1 <- cbind(crcommon,crcommercial)
          }
        }
        
      }
    }
  }  
  
  if (myentity == "Claim") {
    urlname <- paste(Sys.getenv("MainframeIP"),":",Sys.getenv("zConnectPort"),"/CB12Claim/Enquire/",reference,sep="")
    claim_data <- fromJSON(urlname)
    if ( claim_data$LGCMAREA$CA_RETURN_CODE == 0) {
      claimdata <- as.data.frame(claim_data$LGCMAREA$CA_POLICY_REQUEST$CA_CLAIM,stringsAsFactors=FALSE)
      claimdata$CA_POLICY_NUM <- claim_data$LGCMAREA$CA_POLICY_REQUEST$CA_POLICY_NUM
      claimdata$CA_CLAIM_NUM <- reference
      d1 <- claimdata[,c(6,7, 1:5)]
    }
    if (d1$CA_C_OBSERVATIONS != "") {
      y <- gsub("\\|","@",d1$CA_C_OBSERVATIONS,"@@@")
      z <- unlist(str_split(y,"@@@"))
      d1$CA_C_OBSERVATIONS <- z[1]
      d1$CA_C_EVIDENCE <- z[2]
    } else {
      d1$CA_C_EVIDENCE <- ""
    }
    print_policy_type = ""
  }
  
  
  salutation <- paste("Dear ",First_Name," ", Last_Name,",",sep="")
  
  welcome_message <- "General Insurance App welcomes you as a new customer."
  
  header_message <- paste("Your",print_policy_type,myentity,"record #", reference,"is",operation,sep=" ")
  
  gm_auth_configure(path  = "/srv/shiny-server/genapps.json")
  options(
    gargle_oauth_cache = "/srv/shiny-server/.secretgenapps",
    gargle_oauth_email = "gen.apps.insurance@gmail.com"
  )
  gm_auth(email = "gen.apps.insurance@gmail.com")
  
  #print("Authorized")
  
  if (operation != "deleted") {
    d1[1,] <- trimws(d1[1,],"both")
    d1 <- as.data.frame(t(d1))
    names(d1) <- "Record"
    
    msg_table <- pander_return(d1,style="grid")
  }
  
  if(myentity == "Customer" && operation == "created") {
    latest_msg <- unlist(list(salutation," ",welcome_message, " ",header_message,msg_table))  
    title_message <- "Welcome to GenApps Insurance"
  } else {
    if (operation != "deleted") {
      latest_msg <- unlist(list(salutation," ",header_message,msg_table))
    } else {
      latest_msg <- unlist(list(salutation," ",header_message))
    }
    title_message <- paste(print_policy_type,myentity,reference,"is",operation,sep=" ")
  }
  latest_msg <- gsub("&nbsp;","Field ",latest_msg)
  
  #html_msg_text <- paste(html_msg_text,x2,sep="")
  
  my_email_message <- gm_mime() %>%
    gm_to(To_email_id) %>%
    gm_from("General Insurance Apps (gen.apps.insurance@gmail.com)") %>%
    gm_subject(title_message) %>%
    gm_text_body(paste(latest_msg,collapse = "\n"))
  #  gm_html_body(html_msg_text) 
  #  gm_html_body('<h1>A plot of <b>MotorTrend</b> data <i>(1974)</i></h1><br><img src="cid:foobar">') %>%
  #  gm_attach_file("mtcars.png", id = "foobar")  

  #print("Mail ready to send")
  
  gm_send_message(my_email_message)
  output <- "Mail Sent"
}

#* Send Gmail
#* @param myentity Entity Type Customer or Policy or Claim
#* @param operation Operation created deleted updated settled
#* @param reference Customer num or Policy Num or Claim Num
#* @post /sendgmailOTPJKE
function(AccountNum, OTP, Vendor, TxnAmount){
  
  readRenviron("/srv/shiny-server/.env")
  urlname <- paste(Sys.getenv("MainframeIP"),":",Sys.getenv("zConnectPort"),"/jkebankaccount/account/",AccountNum,sep="")
  
  accountdata <- fromJSON(urlname)
  
  if(accountdata[[1]][[1]][[1]][[2]] == 0){
    d1 <- as.data.frame(accountdata[[1]][[2]][[1]],stringsAsFactors = F)    
    account_masked <- paste(substring(d1$NUMB,1,1),"X",substring(d1$NUMB,3,3),"X",substring(d1$NUMB,5,5),"X",sep="")
    Name <- d1$NAME
    Balance <- d1$AMOUNT
    Reference <- d1$COMMENT
    sendee <- d1$EMAIL
    
    salutation <- paste("Dear ", Name, ",",sep="")
    Msgline1 <- paste("You have initiated a debit transaction of ", TxnAmount, " at ", Vendor, sep="")
    Msgline2 <- paste("The OTP for this transaction is ", OTP, sep="")
    Msgline3 <- "If you have not initiated this transaction, call up our helpdesk to report"
    
    SubjectLine <-paste("OTP Alert for your JKE Bank account # ",account_masked,".",sep="")
    msg_body <- unlist(list(salutation," ",Msgline1,Msgline2," ",Msgline3))
    
    gm_auth_configure(path  = "/srv/shiny-server/jkebank.json")
    options(
      gargle_oauth_cache = "/srv/shiny-server/.secretjkebank",
      gargle_oauth_email = "jkebank@gmail.com"
    )
    gm_auth(email = "jkebank@gmail.com")
    
    my_email_message <- gm_mime() %>%
      gm_to(sendee) %>%
      gm_from("JKE Bank (jkebank@gmail.com)") %>%
      gm_subject(SubjectLine) %>%
      gm_text_body(paste(msg_body,collapse = "\n"))
    
    gm_send_message(my_email_message)
    mymessage <- "Mail Sent"
    
  } else {
    mymessage <- "Account No is invalid"
  }
  
  output <- mymessage
  
}

#* Send Gmail2
#* @param myentity Entity Type Customer or Policy or Claim
#* @param operation Operation created deleted updated settled
#* @param reference Customer num or Policy Num or Claim Num
#* @post /sendgmailGENApps2
function(savedata, myentity, operation, reference){
  
  readRenviron("/srv/shiny-server/.env")
  basemicroserviceurl <<- "http://localhost:8000/"
  
  if (operation == "deleted") {
    inputarray <- unlist(strsplit(reference,"-"))
    reference <- inputarray[1]
    referencecustomer <- inputarray[2]
    email_qry <- paste("select firstname, lastname, emailaddress from vcustomer where CUSTOMERNUMBER = ",referencecustomer,sep="")
    print_policy_type = ""
  } else {
    email_qry <- ifelse(myentity=="Claim", paste("select firstname, lastname, emailaddress from vcustomer A, vpolicy B, vclaim C where A.CUSTOMERNUMBER = B.CUSTOMERNUMBER and B.POLICYNUMBER = C.POLICYNUMBER AND C.CLAIMNUMBER = ",reference,sep=""),
                        ifelse(myentity=="Policy",paste("select firstname, lastname, emailaddress from vcustomer A, vpolicy B where A.CUSTOMERNUMBER = B.CUSTOMERNUMBER and B.POLICYNUMBER = ",reference,sep=""),
                               paste("select firstname, lastname, emailaddress from vcustomer where CUSTOMERNUMBER = ",reference,sep="")))
  }
  res <- POST(paste(basemicroserviceurl,"getDVMzEUSDocker?",sep="")
              ,body=list(myquerry = email_qry),
              ,encode = "json")
  
  appPol <- content(res)
  if (length(appPol) > 1) {
    if (appPol[[2]][[2]] > 0 && length(appPol[[1]]) != 0) {
      To_email_id <-  trimws(appPol[[1]][[1]]$EMAILADDRESS)
      First_Name <- trimws(appPol[[1]][[1]]$FIRSTNAME)
      Last_Name <- trimws(appPol[[1]][[1]]$LASTNAME)
    }
  }
  
  if (myentity == "Customer") {
    urlname <- paste(Sys.getenv("MainframeIP"),":",Sys.getenv("zConnectPort"),"/CB12Customer/customer/",reference,sep="")
    accountdata <- fromJSON(urlname)
    if (accountdata$LGCMAREA$CA_RETURN_CODE == 0) {
      d1 <- as.data.frame(accountdata$LGCMAREA$CA_CUSTOMER_REQUEST,stringsAsFactors=FALSE)
      names(d1) <- gsub("CA_PHONE_HOME","CA_BANK_ACT",names(d1))
      pcount_qry <- paste("SELECT COUNT(*) AS COUNT from VPOLICY WHERE CUSTOMERNUMBER = ",reference,sep="")
      res <- POST(paste(basemicroserviceurl,"getDVMzEUSDocker?",sep="")
                  ,body=list(myquerry = pcount_qry),
                  ,encode = "json")
      appPol <- content(res)
      if (length(appPol) > 1) {
        if (appPol[[2]][[2]] > 0 && length(appPol[[1]]) != 0) {
          d1$CA_NUM_POLICIES <-  trimws(appPol[[1]][[1]]$COUNT)
        }
      }
    }
    print_policy_type = ""
  }
  
  if (myentity == "Policy") {
    data_qry <- paste("select CUSTOMERNUMBER, POLICYNUMBER, POLICYTYPE from vpolicy where POLICYNUMBER = ",reference,sep="")
    res <- POST(paste(basemicroserviceurl,"getDVMzEUSDocker?",sep="")
                ,body=list(myquerry = data_qry),
                ,encode = "json")
    appPol <- content(res)
    if (length(appPol) > 1) {
      if (appPol[[2]][[2]] > 0 && length(appPol[[1]]) != 0) {
        customernumber <-  trimws(appPol[[1]][[1]]$CUSTOMERNUMBER)
        policy_type <- trimws(appPol[[1]][[1]]$POLICYTYPE)
        if (policy_type == "M") {
          print_policy_type = "Motor"
          urlname <- paste(Sys.getenv("MainframeIP"),":",Sys.getenv("zConnectPort"),"/CB12MotorPolicy/Policy/",customernumber,",",reference,sep="")
          mr_policy_data <- fromJSON(urlname)
          if ( mr_policy_data$LGCMAREA$CA_RETURN_CODE == 0) {
            mrcommon <- as.data.frame(mr_policy_data$LGCMAREA$CA_POLICY_REQUEST$CA_POLICY_COMMON,stringsAsFactors=FALSE)
            mrmotor <- as.data.frame(mr_policy_data$LGCMAREA$CA_POLICY_REQUEST$CA_MOTOR,stringsAsFactors=FALSE)
            d1 <- cbind(mrcommon,mrmotor)
          }
        }
        if (policy_type == "E") {
          print_policy_type = "Endowment"
          urlname <- paste(Sys.getenv("MainframeIP"),":",Sys.getenv("zConnectPort"),"/CB12EndowmentPolicy/Policy/",customernumber,",",reference,sep="")
          er_policy_data <- fromJSON(urlname)
          if ( er_policy_data$LGCMAREA$CA_RETURN_CODE == 0) {
            ercommon <- as.data.frame(er_policy_data$LGCMAREA$CA_POLICY_REQUEST$CA_POLICY_COMMON,stringsAsFactors=FALSE)
            erendowment <- as.data.frame(er_policy_data$LGCMAREA$CA_POLICY_REQUEST$CA_ENDOWMENT,stringsAsFactors=FALSE)
            d1 <- cbind(ercommon,erendowment)
          }
        }
        if (policy_type == "H") {
          print_policy_type = "House"
          urlname <- paste(Sys.getenv("MainframeIP"),":",Sys.getenv("zConnectPort"),"/CB12HousePolicy/Policy/",customernumber,",",reference,sep="")
          hr_policy_data <- fromJSON(urlname)
          if ( hr_policy_data$LGCMAREA$CA_RETURN_CODE == 0) {
            hrcommon <- as.data.frame(hr_policy_data$LGCMAREA$CA_POLICY_REQUEST$CA_POLICY_COMMON,stringsAsFactors=FALSE)
            hrhouse <- as.data.frame(hr_policy_data$LGCMAREA$CA_POLICY_REQUEST$CA_HOUSE,stringsAsFactors=FALSE)
            d1 <- cbind(hrcommon,hrhouse)
          }
        }
        if (policy_type == "C") {
          print_policy_type = "Commercial"
          urlname <- paste(Sys.getenv("MainframeIP"),":",Sys.getenv("zConnectPort"),"/CB12CommercialPolicy/Policy/",customernumber,",",reference,sep="")
          cr_policy_data <- fromJSON(urlname)
          if ( cr_policy_data$LGCMAREA$CA_RETURN_CODE == 0) {
            crcommon <- as.data.frame(cr_policy_data$LGCMAREA$CA_POLICY_REQUEST$CA_POLICY_COMMON,stringsAsFactors=FALSE)
            crcommercial <- as.data.frame(cr_policy_data$LGCMAREA$CA_POLICY_REQUEST$CA_COMMERCIAL,stringsAsFactors=FALSE)
            d1 <- cbind(crcommon,crcommercial)
          }
        }
        
      }
    }
  }  
  
  if (myentity == "Claim") {
    urlname <- paste(Sys.getenv("MainframeIP"),":",Sys.getenv("zConnectPort"),"/CB12Claim/Enquire/",reference,sep="")
    claim_data <- fromJSON(urlname)
    if ( claim_data$LGCMAREA$CA_RETURN_CODE == 0) {
      claimdata <- as.data.frame(claim_data$LGCMAREA$CA_POLICY_REQUEST$CA_CLAIM,stringsAsFactors=FALSE)
      claimdata$CA_POLICY_NUM <- claim_data$LGCMAREA$CA_POLICY_REQUEST$CA_POLICY_NUM
      claimdata$CA_CLAIM_NUM <- reference
      d1 <- claimdata[,c(6,7, 1:5)]
    }
    if (d1$CA_C_OBSERVATIONS != "") {
      y <- gsub("\\|","@",d1$CA_C_OBSERVATIONS,"@@@")
      z <- unlist(str_split(y,"@@@"))
      d1$CA_C_OBSERVATIONS <- z[1]
      d1$CA_C_EVIDENCE <- z[2]
    } else {
      d1$CA_C_EVIDENCE <- ""
    }
    print_policy_type = ""
  }
  
  
  gm_auth_configure(path  = "/srv/shiny-server/genapps.json")
  options(
    gargle_oauth_cache = "/srv/shiny-server/.secretgenapps",
    gargle_oauth_email = "gen.apps.insurance@gmail.com"
  )
  gm_auth(email = "gen.apps.insurance@gmail.com")
  
  salutation <- paste("Dear ",First_Name," ", Last_Name,",",sep="")
  
  if(myentity == "Customer" && operation == "created") {
    welcome_message <- "General Insurance App welcomes you as a new customer."
    title_message <- "Welcome to GenApps Insurance"
  } else {
    welcome_message <- ""
    title_message <- paste(print_policy_type,myentity,reference,"is",operation,sep=" ")
  }
  
  header_message <- paste("Your",print_policy_type,myentity,"record #", reference,"is",operation,sep=" ")
  
  
  if (operation != "deleted") {
    d1[1,] <- trimws(d1[1,],"both")
    d1 <- as.data.frame(t(d1),stringsAsFactors = FALSE)
    names(d1) <- "New"
    table_included <- TRUE
  } else {
    table_included <- FALSE
    d1 <- data.frame()
  }
  
  if (operation == "updated" || operation == "deleted") {
    d2 <- savedata
    names(d2) <- "Old"
    if (myentity == "Customer") {
      d2["CA_NUM_POLICIES",1] <- d1["CA_NUM_POLICIES",1]
    }
  } else {
    d2 <- data.frame()
  }
  
  
  params <<- list(operation = operation,
                  salutation = salutation,
                  welcome_message = welcome_message,
                  header_message = header_message,
                  table_included = table_included,
                  print_data = d1,
                  old_data = d2)
  
  tempReport <- rmarkdown::render("/srv/shiny-server/appGENApps/GenApps2.Rmd",params = params)
  rawHTML <- paste(readLines(tempReport), collapse="\n")
  
  
  my_email_message <- gm_mime() %>%
    gm_to(To_email_id) %>%
    gm_from("General Insurance Apps (gen.apps.insurance@gmail.com)") %>%
    gm_subject(title_message) %>%
    gm_html_body(rawHTML) 
  
  gm_send_message(my_email_message)
  
  output <- "Mail Sent"
}

#* Send Gmail3
#* @param myentity Entity Type Customer or Policy or Claim
#* @param operation Operation created deleted updated settled
#* @param reference Customer num or Policy Num or Claim Num
#* @post /sendgmailGENApps3
function(myentity, operation, reference,CustomerNum){
  
  readRenviron("/srv/shiny-server/.env")
  basemicroserviceurl <<- "http://localhost:8000/"
  urlname <- paste(Sys.getenv("MainframeIP"),":",Sys.getenv("zConnectPort"),"/CB12Customer/customer/",CustomerNum,sep="")
  accountdata <- fromJSON(urlname)
  if (accountdata$LGCMAREA$CA_RETURN_CODE == 0) {
    d1 <- as.data.frame(accountdata$LGCMAREA$CA_CUSTOMER_REQUEST,stringsAsFactors=FALSE)
    names(d1) <- gsub("CA_PHONE_HOME","CA_BANK_ACT",names(d1))
    To_email_id <-  d1$CA_EMAIL_ADDRESS
    First_Name <- d1$CA_FIRST_NAME
    Last_Name <- d1$CA_LAST_NAME
  }  
  if (myentity == "Claim") {
    urlname <- paste(Sys.getenv("MainframeIP"),":",Sys.getenv("zConnectPort"),"/CB12Claim/Enquire/",reference,sep="")
    claim_data <- fromJSON(urlname)
    if ( claim_data$LGCMAREA$CA_RETURN_CODE == 0) {
      claimdata <- as.data.frame(claim_data$LGCMAREA$CA_POLICY_REQUEST$CA_CLAIM,stringsAsFactors=FALSE)
      claimdata$CA_POLICY_NUM <- claim_data$LGCMAREA$CA_POLICY_REQUEST$CA_POLICY_NUM
      claimdata$CA_CLAIM_NUM <- reference
      d1 <- claimdata[,c(6,7, 1:5)]
    }
    if (d1$CA_C_OBSERVATIONS != "") {
      y <- gsub("\\|","@",d1$CA_C_OBSERVATIONS,"@@@")
      z <- unlist(str_split(y,"@@@"))
      d1$CA_C_OBSERVATIONS <- z[1]
      d1$CA_C_EVIDENCE <- z[2]
    } else {
      d1$CA_C_EVIDENCE <- ""
    }
    print_policy_type = ""
  }
  gm_auth_configure(path  = "/srv/shiny-server/genapps.json")
  options(
    gargle_oauth_cache = "/srv/shiny-server/.secretgenapps",
    gargle_oauth_email = "gen.apps.insurance@gmail.com"
  )
  gm_auth(email = "gen.apps.insurance@gmail.com")
  
  salutation <- paste("Dear ",First_Name," ", Last_Name,",",sep="")
  
  if(myentity == "Customer" && operation == "created") {
    welcome_message <- "General Insurance App welcomes you as a new customer."
    title_message <- "Welcome to GenApps Insurance"
  } else {
    welcome_message <- ""
    title_message <- paste(print_policy_type,myentity,reference,"is",operation,sep=" ")
  }
  
  header_message <- paste("Your",print_policy_type,myentity,"record #", reference,"is",operation,sep=" ")
  
  
  if (operation != "deleted") {
    d1[1,] <- trimws(d1[1,],"both")
    d1 <- as.data.frame(t(d1))
    names(d1) <- "Record"
    table_included <- TRUE
  } else {
    table_included <- FALSE
    d1 <- data.frame()
  }
  params <- list(salutation = salutation,
                 welcome_message = welcome_message,
                 header_message = header_message,
                 table_included = table_included,
                 print_data = d1)
  
  tempReport <- rmarkdown::render("/srv/shiny-server/appGENApps/GenApps.Rmd",params = params)
  rawHTML <- paste(readLines(tempReport), collapse="\n")
  
  my_email_message <- gm_mime() %>%
    gm_to(To_email_id) %>%
    gm_from("General Insurance Apps (gen.apps.insurance@gmail.com)") %>%
    gm_subject(title_message) %>%
    gm_html_body(rawHTML) 
  
  gm_send_message(my_email_message)
  output <- "Mail Sent"
}

#* Send Gmail JKE
#* @param myentity Entity Type Customer or Policy or Claim
#* @param operation Operation created deleted updated settled
#* @param reference Customer num or Policy Num or Claim Num
#* @post /sendgmailJKE
function(AccountNum, TxnType, TxnAmount){
  
  readRenviron("/srv/shiny-server/.env")
  urlname <- paste(Sys.getenv("MainframeIP"),":",Sys.getenv("zConnectPort"),"/jkebankaccount/account/",AccountNum,sep="")
  accountdata <- fromJSON(urlname)
  
  if(accountdata[[1]][[1]][[1]][[2]] == 0){
    d1 <- as.data.frame(accountdata[[1]][[2]][[1]],stringsAsFactors = F)    
  }
  
  account_masked <- paste(substring(d1$NUMB,1,1),"X",substring(d1$NUMB,3,3),"X",substring(d1$NUMB,5,5),"X",sep="")
  Name <- d1$NAME
  Balance <- d1$AMOUNT
  Reference <- d1$COMMENT
  sendee <- d1$EMAIL
  
  salutation <- paste("Dear ", Name, ",",sep="")
  
  if (TxnType == "New") {
    NonTxnMessage <- paste("Welcome to JKE Bank. Your new account ",  AccountNum, " is created with ", Balance, " as opening balance",sep="")
  } else {
    if (TxnType == "Update") {
      NonTxnMessage <- "Your Account Information is updated as following"
    } else {
      NonTxnMessage <- ""
    }
  }
  
  if (TxnType == "New" || TxnType == "Update") {
    TxnMessage <- ""
  } else {
    TxnMessage <- paste(TxnType," transaction of ",TxnAmount, " is instrumented against Reference ", Reference, " on your Bank account # ",account_masked,".",sep="")
  }
  
  if (TxnType == "New" || TxnType == "Update") {
    BalanceMessage <- ""
  } else {
    BalanceMessage <- paste("Your updated account Balance is ",Balance,".",sep="")
  }
  
  if (TxnType == "New" || TxnType == "Update") {
    AccountFields <- data.frame("Mailing_Address" = d1$ADDRX, "email_id" = d1$EMAIL, "Cell_No" = d1$PHONE)
  } else {
    AccountFields <- data.frame()
  }
  
  if (TxnType == "New") {
    SubjectLine <-  "Welcome to JKE Bank"  
  } else {
    if (TxnType == "Update") {
      SubjectLine <-paste("Customer Information updated for your JKE Bank account # ",account_masked,".",sep="")
    } else {
      SubjectLine <-paste("Transaction alert on your JKE Bank account # ",account_masked,".",sep="")
    }
  }
  
  
  
  gm_auth_configure(path  = "/srv/shiny-server/jkebank.json")
  options(
    gargle_oauth_cache = "/srv/shiny-server/.secretjkebank",
    gargle_oauth_email = "jkebank@gmail.com"
  )
  gm_auth(email = "jkebank@gmail.com")
  
  
  
  params <<- list(salutation = salutation,
                  NonTxnMessage = NonTxnMessage,
                  TxnMessage = TxnMessage,
                  BalanceMessage = BalanceMessage,
                  AccountFields = AccountFields)
  tempReport <- rmarkdown::render("/srv/shiny-server/appJKE/JKEBank.Rmd",params = params)
  rawHTML <- paste(readLines(tempReport), collapse="\n")
  
  my_email_message <- gm_mime() %>%
    gm_to(sendee) %>%
    gm_from("JKE Bank (jkebank@gmail.com)") %>%
    gm_subject(SubjectLine) %>%
    gm_html_body(rawHTML) 
  
  gm_send_message(my_email_message)
  output <- "Mail Sent"
}  
