# plumber.R
library(httr)
library(jsonlite)
library(stringr)
library(DBI)
library(RJDBC)
library(rJava)

#* Echo back the input
#* @param msg The message to echo
#* @get /echo
function(msg=""){
  list(msg = paste0("The message is: '", msg, "'"))
}

#* Plot a histogram
#* @png
#* @get /plot
function(){
  rand <- rnorm(100)
  hist(rand)
}

#* Return the sum of two numbers
#* @param a The first number to add
#* @param b The second number to add
#* @post /sum
function(a, b){
  as.numeric(a) + as.numeric(b)
}

#* Get DVM results
#* @param myquerry from DVM
#* @post /getDVMzEUS
function(myquerry) {
  .jaddClassPath( "\\jdbc\\dv-jdbc-3.1.jar" )
  driver <- JDBC("com.rs.jdbc.dv.DvDriver",Sys.glob("\\jdbc\\*.jar"))
  #print(myquerry)
  conn = dbConnect(driver,"jdbc:rs:dv://192.86.33.143:1200;DatabaseType=DVS; user=ashissa, password=73e613cc896030ac3e  ", user="ashissa", password="73e613cc896030ac3e")
  result <- dbFetch(dbSendQuery(conn, myquerry), -1)
  dbDisconnect(conn)
  output <- list(result,dim(result),names(result))
}


#* Get DVM results for Tablenames
#* @character
#* @get /getDVMzEUSTables
function() {
  .jaddClassPath( "\\jdbc\\dv-jdbc-3.1.jar" )
  driver <- JDBC("com.rs.jdbc.dv.DvDriver",Sys.glob("\\jdbc\\*.jar"))
  
  conn = dbConnect(driver,"jdbc:rs:dv://192.86.33.143:1200;DatabaseType=DVS; user=ashissa, password=73e613cc896030ac3e  ", user="ashissa", password="73e613cc896030ac3e")
  result <- dbListTables(conn)
  dbDisconnect(conn)
  output <- list(result)
}

#* Get DVM results for FieldName against Table
#* @param tablename from DVM
#* @get /getDVMzEUSFields
function(tablename){
  .jaddClassPath( "\\jdbc\\dv-jdbc-3.1.jar" )
  driver <- JDBC("com.rs.jdbc.dv.DvDriver",Sys.glob("\\jdbc\\*.jar"))
  
  conn = dbConnect(driver,"jdbc:rs:dv://192.86.33.143:1200;DatabaseType=DVS; user=ashissa, password=73e613cc896030ac3e  ", user="ashissa", password="73e613cc896030ac3e")
  result <- dbListFields(conn,tablename)
  dbDisconnect(conn)
  output <- list(result)
}

#* Get DB2 results
#* @param myquerry from DB2
#* @post /getDB2zEUS
function(myquerry) {
  .jaddClassPath( "\\jdbc\\db2jcc.jar" )
  driver <- JDBC("com.ibm.db2.jcc.DB2Driver",Sys.glob("\\jdbc\\*.jar"))
  #print(myquerry)
  conn = conn = dbConnect(driver,"jdbc:db2://192.86.33.143:5035/DALLASB", user="ashissa", password="A9SHISAH", ":memory:")
  if (tolower(str_extract(myquerry, '\\w*')) != "select") {
    result = dbSendUpdate(conn, myquerry)
    dbCommit(conn)
  }
  else {
    result <- dbFetch(dbSendQuery(conn, myquerry), -1)
  }
  dbDisconnect(conn)
  output <- list(result=result,dim=dim(result),names=names(result))
}


#* Get DVM results from Docker
#* @param myquerry from DVM
#* @post /getDVMzEUSDocker
function(myquerry) {
  
  .jaddClassPath( "/srv/shiny-server/jdbc/dv-jdbc-3.1.jar" )
  driver <- JDBC("com.rs.jdbc.dv.DvDriver",Sys.glob("/srv/shiny-server/jdbc/*.jar"))
  
  conn = dbConnect(driver,"jdbc:rs:dv://192.86.33.143:1200;DatabaseType=DVS; user=ashissa, password=73e613cc896030ac3e  ", user="ashissa", password="73e613cc896030ac3e")
  result <- dbFetch(dbSendQuery(conn, myquerry), -1)
  dbDisconnect(conn)
  output <- list(result,dim(result),names(result))
}

#* Get DVM results from Docker
#* @param myquerry from DVM
#* @post /getDVMzEUSDockermobile
function(myquerry) {
  
  .jaddClassPath( "/srv/shiny-server/jdbc/dv-jdbc-3.1.jar" )
  driver <- JDBC("com.rs.jdbc.dv.DvDriver",Sys.glob("/srv/shiny-server/jdbc/*.jar"))
  
  conn = dbConnect(driver,"jdbc:rs:dv://192.86.33.143:1200;DatabaseType=DVS; user=ashissa, password=73e613cc896030ac3e  ", user="ashissa", password="73e613cc896030ac3e")
  result <- dbFetch(dbSendQuery(conn, myquerry), -1)
  dbDisconnect(conn)
  output <- list(result=result,dim=dim(result),names=names(result))
}


#* Get DVM results for Tablenames Docker
#* @character
#* @get /getDVMzEUSTablesDocker
function() {
  .jaddClassPath( "/srv/shiny-server/jdbc/dv-jdbc-3.1.jar" )
  driver <- JDBC("com.rs.jdbc.dv.DvDriver",Sys.glob("/srv/shiny-server/jdbc/*.jar"))
  
  conn = dbConnect(driver,"jdbc:rs:dv://192.86.33.143:1200;DatabaseType=DVS; user=ashissa, password=73e613cc896030ac3e  ", user="ashissa", password="73e613cc896030ac3e")
  result <- dbListTables(conn)
  dbDisconnect(conn)
  output <- list(result)
}

#* Get DVM results for FieldName against Table Docker
#* @param tablename from DVM
#* @get /getDVMzEUSFieldsDocker
function(tablename){
  .jaddClassPath( "/srv/shiny-server/jdbc/dv-jdbc-3.1.jar" )
  driver <- JDBC("com.rs.jdbc.dv.DvDriver",Sys.glob("/srv/shiny-server/jdbc/*.jar"))
  
  conn = dbConnect(driver,"jdbc:rs:dv://192.86.33.143:1200;DatabaseType=DVS; user=ashissa, password=73e613cc896030ac3e  ", user="ashissa", password="73e613cc896030ac3e")
  result <- dbListFields(conn,tablename)
  dbDisconnect(conn)
  output <- list(result)
}

#* Get DB2 results
#* @param myquerry from DB2
#* @post /getDB2zEUSDocker
function(myquerry) {
  .jaddClassPath( "/srv/shiny-server/jdbc/db2jcc.jar" )
  driver <- JDBC("com.ibm.db2.jcc.DB2Driver",Sys.glob("/srv/shiny-server/jdbc/*.jar"))
  #print(myquerry)
  conn = conn = dbConnect(driver,"jdbc:db2://192.86.33.143:5040/DALLASC", user="ashissa", password="A9SHISAH", ":memory:")
  if (tolower(str_extract(myquerry, '\\w*')) != "select") {
    result = dbSendUpdate(conn, myquerry)
    dbCommit(conn)
  }
  else {
    result <- dbFetch(dbSendQuery(conn, myquerry), -1)
  }
  dbDisconnect(conn)
  output <- list(result=result,dim=dim(result),names=names(result))
}



#* Validate Order
#* @param l1 df of item
#* @param mycurrency currency of payment
#* @post /validate
function(l1, mycurrency) {
  l1 <- as.data.frame(matrix(as.integer(unlist(str_extract_all(l1,"[0-9]+"))), ncol=2, byrow=TRUE), stringsAsFactors=FALSE)
  outdata <- data.frame()
  for (i in 1:NROW(l1)) {
    urlname <- paste("http://192.86.33.143:9080/catalogManager/items/",l1[i,1],sep="")
    itemrefdata <- fromJSON(urlname)
    outdata[i,1] <- l1[i,1]
    outdata[i,2] <- l1[i,2]
    d1 <- as.data.frame(itemrefdata[[1]][[2]][[1]],stringsAsFactors=F)
    if(d1[1,]$CA_SNGL_ITEM_REF == l1[i,1]){
      if(l1[i,2] <= d1[1,]$IN_SNGL_STOCK){
        outdata[i,3] <- l1[i,2]
      }
      else {
        outdata[i,3] <- d1[1,]$IN_SNGL_STOCK
      }
      outdata[i,4] <- as.numeric(d1[1,]$CA_SNGL_COST)*as.numeric(outdata[i,3])
    }
    else {
      outdata[i,3] <- 0
      outdata[i,4] <- 0 
    }
  } 
  y <- outdata[,1:3]
  if (sum(outdata[,2]) == sum(outdata[,3])) {
    message <- "Stock is available"
  }
  else {
    message <- "Stock is not fully available"
  }
  
  mysum <- sum(outdata[,4])
  
  if (mycurrency != "USD") {
    urlname <- paste("http://data.fixer.io/api/latest?access_key=79dc687089494f9b6ff9cf4eb66040f6&symbols=",mycurrency,",USD",sep="")
    convrate <- fromJSON(urlname)
    if(convrate[[1]][[1]]) {
      rate <- convrate[[5]][[2]]/convrate[[5]][[1]]
      mysum <- round(mysum / rate, 2)
    }
    else {
      message <- "Currency conversion Failed"
      mysum <- 0.0
    }
  }
  
  z <- list(y,mysum,message)
}

#* Place Order
#* @param x df of order 
#* @param y payment info
#* @post /order
function(x, y) {
  order_input <- as.data.frame(matrix(as.integer(unlist(str_extract_all(x,"[0-9]+"))), ncol=2, byrow=TRUE), stringsAsFactors=FALSE)
  
  account_info <- unlist(strsplit(y, split=" "))
  
  mycurrency <- account_info[1]
  mysum <- as.numeric(account_info[2])
  account_num <- account_info[3]
  
  order_input[,2] <- as.numeric(order_input[,2])
  refund <- 0
  processing_error <- FALSE
  message <- ""
  
  #Convert Currency
  if (mycurrency != "USD") {
    urlname <- paste("http://data.fixer.io/api/latest?access_key=79dc687089494f9b6ff9cf4eb66040f6&symbols=",mycurrency,",USD",sep="")
    convrate <- fromJSON(urlname)
    if(convrate[[1]][[1]]) {
      rate <- convrate[[5]][[2]]/convrate[[5]][[1]]
      mysum <- round(mysum * rate, 2)
    }
    else {
      message <- "Currency conversion Failed"
      processing_error <- TRUE
    }
  }
  
  #print(paste("Amount Paid in USD ", mysum, sep=""))
  
  #Validate Account
  if (!processing_error) {
    urlname <- paste("http://192.86.33.143:9080/jkebanking/accno/",account_num,sep="")
    accountdata <- fromJSON(urlname)
    if(accountdata[[1]][[1]][[1]][[2]] != 0){
      message <- "Account Not Found"
      processing_error <- TRUE
    }
    if(accountdata[[1]][[1]][[1]][[2]] == 0){  
      basedata <- as.data.frame(accountdata[[1]][[2]][[1]],stringsAsFactors = F)
    }
  }
  
  #Check Balance
  if (!processing_error) {
    add_date <- paste(as.integer(format(Sys.time(), "%d")),as.integer(format(Sys.time(), "%m")),as.integer(format(Sys.time(), "%y")))
    accountbalance <- as.numeric(gsub("\\$","",basedata$AMOUNT))
    newaccountbalance <- round(accountbalance - mysum, 2)
    if(newaccountbalance < 100){
      message <- "Insufficient Balance in Payer Account"
      processing_error <- TRUE
    }
  }
  #print(paste("Before Payment Account Balance in USD ", accountbalance, sep=""))
  
  #Pay Bill
  if (!processing_error) {
    pc_json <- list(
      DFHCOMMAREA = list(FILEA = list(FILEREC = list(
        STAT = "U",NUMB = basedata$NUMB,NAME=basedata$NAME,ADDRX=basedata$ADDRX,PHONE=basedata$PHONE,DATEX=add_date,AMOUNT=paste("$",newaccountbalance,sep=""),COMMENT="PnP Buy")),
        COMM_AREA = list(FILEREC = list(
          STAT=basedata$STAT,NUMB=basedata$NUMB,NAME=basedata$NAME,ADDRX=basedata$ADDRX,PHONE=basedata$PHONE,DATEX=basedata$DATEX,AMOUNT=basedata$AMOUNT,COMMENT=basedata$COMMENT)
        )
      ))  
    res <- PUT(paste("http://192.86.33.143:9080/jkebanking/accno/",account_num,sep=""),body=pc_json,encode ="json")
    appData <- content(res)
    if (appData[[1]][[1]][[1]][[2]] != 0) {
      message <- "Payment Unsuccessful"
      processing_error <- TRUE
    }
  }
  
  #Place Orders
  if (!processing_error) {
    for (i in 1:NROW(order_input)) {
      loop_order <- TRUE
      order_qty <- order_input[i,2]
      while (loop_order) {
        pc_json <- list(DFH0XCP1 = list(orderRequest = list(itemId = order_input[i,1], orderQuantity = order_qty)))
        res <- POST("http://192.86.33.143:9080/catalogManager/orders",body = pc_json,encode = "json")
        appData <- content(res)
        ordermessage <- as.character(as.data.frame(appData)[,1])
        #print(paste("Item = ", order_input[i,1], " Order = ", order_qty, " ", ordermessage,sep = ""))
        if (ordermessage !=  "ORDER SUCCESSFULLY PLACED") {
          urlname <- paste("http://192.86.33.143:9080/catalogManager/items/",order_input[i,1],sep="")
          itemrefdata <- fromJSON(urlname)
          d1 <- as.data.frame(itemrefdata[[1]][[2]][[1]],stringsAsFactors=F)
          order_qty <- as.numeric(d1[1,]$IN_SNGL_STOCK)
          if (order_qty == 0) {
            loop_order <- FALSE
          }
        }
        else {
          loop_order <- FALSE
        }
      }
      if (order_input[i,2] != order_qty) {
        refund <- refund + (order_input[i,2] - order_qty) * as.numeric(d1[1,]$CA_SNGL_COST)
      }
      order_input[i,3] <- order_qty
    }
  }
  #print(paste("Refund amount in USD ", refund, sep=""))
  
  #Convert Refund to LOcal Currency
  if (mycurrency != "USD") {
    urlname <- paste("http://data.fixer.io/api/latest?access_key=79dc687089494f9b6ff9cf4eb66040f6&symbols=",mycurrency,",USD",sep="")
    convrate <- fromJSON(urlname)
    if(convrate[[1]][[1]]) {
      rate <- convrate[[5]][[2]]/convrate[[5]][[1]]
      refund_local <- round(refund / rate, 2)
      #print(paste("Refund amount in ", mycurrency, " ", refund_local, sep=""))
      
    }
    else {
      message <- "Currency conversion Failed"
    }
  }
  #Check Account for Refund and process reversal
  if (!processing_error && (refund > 0) ) {
    urlname <- paste("http://192.86.33.143:9080/jkebanking/accno/",account_num,sep="")
    accountdata <- fromJSON(urlname)
    if(accountdata[[1]][[1]][[1]][[2]] != 0){
      message <- "Account Not found for reversal"
      processing_error <- TRUE
    }
    if(accountdata[[1]][[1]][[1]][[2]] == 0){  
      basedata <- as.data.frame(accountdata[[1]][[2]][[1]],stringsAsFactors = F)
    }
    add_date <- paste(as.integer(format(Sys.time(), "%d")),as.integer(format(Sys.time(), "%m")),as.integer(format(Sys.time(), "%y")))
    accountbalance <- as.numeric(gsub("\\$","",basedata$AMOUNT))
    #print(paste("Before Reversal Account Balance in USD ", accountbalance, sep=""))
    newaccountbalance <- round(accountbalance + refund, 2)
    #Pay Bill
    pc_json <- list(
      DFHCOMMAREA = list(FILEA = list(FILEREC = list(
        STAT = "U",NUMB = basedata$NUMB,NAME=basedata$NAME,ADDRX=basedata$ADDRX,PHONE=basedata$PHONE,DATEX=add_date,AMOUNT=paste("$",newaccountbalance,sep=""),COMMENT="Rev PnP")),
        COMM_AREA = list(FILEREC = list(
          STAT=basedata$STAT,NUMB=basedata$NUMB,NAME=basedata$NAME,ADDRX=basedata$ADDRX,PHONE=basedata$PHONE,DATEX=basedata$DATEX,AMOUNT=basedata$AMOUNT,COMMENT=basedata$COMMENT)
        )
      )
    )  
    res <- PUT(paste("http://192.86.33.143:9080/jkebanking/accno/",account_num,sep=""),body=pc_json,encode="json")
    appData <- content(res)
    if (appData[[1]][[1]][[1]][[2]] != 0) {
      message <- "Payment Reversal Unsuccessful"
      processing_error <- TRUE
    }
  }
  
  if (mycurrency != "USD") {
    refund_amount <- refund_local
  }
  if (mycurrency == "USD") {
    refund_amount <- refund
  }  
  z <- list(order_input,refund_amount,message)
}

#* Get DVM results from Docker
#* @param myquerry from DVM
#* @post /getDVMzVADocker
function(myquerry) {
  
  .jaddClassPath( "/srv/shiny-server/jdbc/dv-jdbc-3.1.jar" )
  driver <- JDBC("com.rs.jdbc.dv.DvDriver",Sys.glob("/srv/shiny-server/jdbc/*.jar"))
  
  conn = dbConnect(driver,"jdbc:rs:dv://10.149.60.157:1200;DatabaseType=DVS; user=ibmuser, password=sys1  ", user="ibmuser", password="sys1")
  result <- dbFetch(dbSendQuery(conn, myquerry), -1)
  dbDisconnect(conn)
  output <- list(result,dim(result),names(result))
}

#* Get DVM results from Docker
#* @param myquerry from DVM
#* @post /getDVMzVADockermobile
function(myquerry) {
  
  .jaddClassPath( "/srv/shiny-server/jdbc/dv-jdbc-3.1.jar" )
  driver <- JDBC("com.rs.jdbc.dv.DvDriver",Sys.glob("/srv/shiny-server/jdbc/*.jar"))
  
  conn = dbConnect(driver,"jdbc:rs:dv://10.149.60.157:1200;DatabaseType=DVS; user=ibmuser, password=sys1  ", user="ibmuser", password="sys1")
  result <- dbFetch(dbSendQuery(conn, myquerry), -1)
  dbDisconnect(conn)
  output <- list(result=result,dim=dim(result),names=names(result))
}
