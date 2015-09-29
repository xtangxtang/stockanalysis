setwd("C:\\xtang\\workspace\\R\\stockAnalysis")
library(quantmod)
library(TTR)
library(plyr)

library(xlsx)
library(XLConnect)
library(scales)
library(sqldf)

header <- c("日期","代码","名称","最新","涨幅%","主力净流入","净买率","集合竞价","超大单流入","超大单流出","超大单净额","超大单净占比%","大单流入","大单流出","大单净额","大单净占比%","中单流入","中单流出","中单净额","中单净占比%","小单流入","小单流出","小单净额","小单净占比%")
dailyCapOrigHeader <- c("日期","代码","名称","最新","涨幅%","主力净流入","集合竞价","超大单流入","超大单流出","超大单净额","超大单净占比%","大单流入","大单流出","大单净额","大单净占比%","中单流入","中单流出","中单净额","中单净占比%","小单流入","小单流出","小单净额","小单净占比%")

#公共方法，在data.frame中加入一行
insertRow <- function(existingDF, newrow, r,newRowName) {
  if (r <= nrow(existingDF)) {
    existingDF[seq(r+1,nrow(existingDF)+1),] <- existingDF[seq(r,nrow(existingDF)),]
    existingDF[r,] <- newrow
    row.names(existingDF[r,]) <- newRowName    
  } else if (r == (nrow(existingDF)+1)){
    existingDF[r,] <- newrow
    row.names(existingDF[r,]) <- newRowName  
  }

  return(existingDF)
}

addOneColToDF <- function(oldDF,colName="",newdata,newColName="") {
  exists <- which(names(oldDF)==newColName)[1]
  if(length(exists)!=0) {
    #print(paste("WARNING:the col",newColName,"already exsits in",names(oldDF)))
    return
  }
  
  bspot <- which(names(oldDF)==colName)[1]
  newDF<-data.frame(oldDF[1:bspot],colName=newdata,oldDF[(bspot+1):ncol(oldDF)])
  colnames(newDF)[bspot+1] <- newColName;
  return(newDF)
}

#根据日期打开当天的资金流向文件
openCapitalFile <- function(date) {
  dailyFile <- paste(".\\capital\\daily\\",date,".xlsx",sep="")
  tryCatch({
    dailyCapital <- read.xlsx2(dailyFile,sheetIndex=1,startRow=3,header=FALSE, encoding="UTF-8",colClasses=c("Date","character","character",rep("numeric", 20)),  stringsAsFactors=FALSE)
    #   print(paste("-----------dailyCapital-------------",date))
    #   print(head(dailyCapital))
    #   print("-----------dailyCapital-------------")  
    dailyCapital[,1] <- as.Date(date)
    colnames(dailyCapital) <- dailyCapOrigHeader
    #   dailyCapital <- dailyCapital[,-25]
    #   dailyCapital <- dailyCapital[,-24]  
    #   print(paste("-----------dailyCapital-------------",date))
    #   print(head(dailyCapital))
    #   print("-----------dailyCapital-------------")
    dailyCapital <- addOneColToDF(dailyCapital,"主力净流入",c(0),"净买率")
    #   print(paste("-----------dailyCapital-------------",date))
    #   print(head(dailyCapital))
    #   print("-----------dailyCapital-------------")  
    return(dailyCapital)    
  }, warning = function(w) {
    msg <- paste("get warning:",w$message,"for",fileName)
    print(msg)
    return(NULL)
  }, error = function(e) {
    print(paste("ERROR:",e$message))
    return(NULL)
  }, finally = {
    
  })
  

}

#根据日期打开当天的交易文件
openHSADailyTradeFile <- function(date) {
  dailyFile <- paste(".\\market\\daily\\",date,".xlsx",sep="")
  tryCatch({
    dailyTrade <- read.xlsx2(dailyFile, sheetIndex=1,header=TRUE,encoding="UTF-8",colClasses=c("Date","character","character",rep("numeric", 3),rep("integer",2),rep("numeric", 2),"integer",rep("numeric", 9),rep("integer",2),"character",rep("numeric", 2)), keepFormulas=FALSE, stringsAsFactors=FALSE)  
    dailyTrade[,1] <- as.Date(date)
    return(dailyTrade)    
  }, warning = function(w) {
    msg <- paste("get warning:",w$message,"for",fileName)
    print(msg)
    return(NULL)
  }, error = function(e) {
    print(paste("ERROR:",e$message))
    return(NULL)
  }, finally = {
    
  })
}



#将某天的全部资金流向文件中的数据，写入到个股的资金流向文件中去
uniqueCapColClasses <- c("Date","character","character",rep("numeric", 30))
writeToUniqueCapital <- function(dailyCapital) {
  dailyCapital <- na.omit(dailyCapital)
  date <- dailyCapital[1,1]
  colnames(dailyCapital)[1]="日期"
  
  for(i in 1:nrow(dailyCapital)) {
  #for(i in 1:1) {
    fileName <- paste("C:\\xtang\\workspace\\R\\stockAnalysis\\capital\\unique\\",dailyCapital[i,2],".xlsx",sep="")    
    result = tryCatch({
      uniqueFile <- read.xlsx2(fileName,sheetIndex=1,header=TRUE, encoding="UTF-8",colClasses=uniqueCapColClasses, stringsAsFactors=FALSE,colIndex=(1:24))
#       print(paste("-------------get file------------",fileName))
#       print(uniqueFile)
#       print("-----------------------------------")
      
      exist <- which(uniqueFile$日期 == date)
      if(length(exist)!=0) {
        print(paste("WARNING:get the same date ",date," for ",fileName,sep=""))
        next;
      }
      colnames(uniqueFile) <- header
      data<-dailyCapital[i,]
#      data <- addOneColToDF(dailyCapital[i,],"主力净流入",c(0),"净买率")
#       print("-----------data-----------------")
#       print(data)
#       print("-----------------------------------")      
#       uniqueFile <- uniqueFile[,-24]
#       uniqueFile <- uniqueFile[,-24]
      uniqueFile <- insertRow(uniqueFile,data,nrow(uniqueFile)+1,as.character(nrow(uniqueFile)))
      uniqueFile <- uniqueFile[,-length(uniqueFile)]
#       print(paste("-----------update file -----------------",fileName))
#       print(length(uniqueFile[i,]))
#       print(uniqueFile)
#       print("-----------------------------------")
      write.xlsx2(uniqueFile,fileName,row.names=FALSE,append=FALSE,colClasses=uniqueCapColClasses,colIndex=c(1:24),encoding="UTF-8")
      msg <- paste("update file ",fileName," with ",data[1,1]," data",sep="")
      print(msg)
    }, warning = function(w) {
      msg <- paste("get warning:",w$message,"for",fileName)
      print(msg)      
    }, error = function(e) {
      
      print(e$message)
      msg <- paste("create file ",fileName,sep="")
      print(msg)
      
      #newDF <- addOneColToDF(dailyCapital[i,],"主力净流入",c(0),"净买率")
      newDF <- dailyCapital[i,]
      newDF <- newDF[,-length(newDF)]
      
#       print("********newDF**********")
#       print(length(newDF[i,]))
#       print(newDF)
#       print("***********************")
#       print(dailyCapital[i,])
#       print("***********************")
      write.xlsx2(newDF,fileName,row.names=FALSE,append=TRUE,colClasses=uniqueCapColClasses,colIndex=c(1:24),encoding="UTF-8")
    }, finally = {
      
    })
  }
}

#将某天的全部交易数据写入到个股的交易数据
uniqueMarketColClasses<-c("Date","character","character",rep("numeric", 3),rep("integer",2),rep("numeric", 2),"integer",rep("numeric", 9),rep("integer",2),"character",rep("numeric", 2))
writeToUniqueDailyTrade <- function(dailyTrade) {
  
  dailyTrade <- na.omit(dailyTrade)
  date <- dailyTrade[1,1]
  
  for(i in 1:nrow(dailyTrade)) {
  #for(i in 1:1) {
    fileName <- paste("C:\\xtang\\workspace\\R\\stockAnalysis\\market\\unique\\",dailyTrade[i,2],".xlsx",sep="")    
    result = tryCatch({
      uniqueFile <- read.xlsx2(fileName, sheetIndex=1,header=TRUE,encoding="UTF-8",colClasses=uniqueMarketColClasses, stringsAsFactors=FALSE,colIndex=c(1:25))
#       print(paste("-------------get file------------",fileName))
#       print(uniqueFile)
#       print(length(uniqueFile[1,]))
           
      exist <- which(uniqueFile$日期 == date)
      if(length(exist)!=0) {
        print(paste("WARNING:get the same date ",date," for ",fileName,sep=""))
        next;
      }
      data <- dailyTrade[i,]
#       print(data)
#       print(length(data))
      data <- data[,-(length(data))]
      #print("-----------------------------------") 
      uniqueFile <- insertRow(uniqueFile,data,nrow(uniqueFile)+1,as.character(nrow(uniqueFile)))
      colnames(dailyTrade)[1]="日期"
      #print(paste("-------------update date file-----------",fileName))
      uniqueFile <- uniqueFile[,-length(uniqueFile)]
      #print(uniqueFile)
      #print("------------------------------------")
      write.xlsx2(uniqueFile,fileName,row.names=FALSE,append=FALSE,colIndex=c(1:26),encoding="UTF-8",colClasses=uniqueMarketColClasses)
      msg <- paste("update file ",fileName," with ",data[1,1]," data",sep="")
      print(msg)
    }, warning = function(w) {
      msg <- paste("get warning:",w$message,"for",fileName)
      print(msg)
    }, error = function(e) {
      print(e$message)
      msg <- paste("create file ",fileName,sep="")
      print(msg)
      colnames(dailyTrade)[1]="日期"
#       print(paste("-----------create file-------------",fileName))
#       print(dailyTrade[i,])
#       print("------------------------------------")
      write.xlsx2(dailyTrade[i,],fileName,row.names=FALSE,append=TRUE,colIndex=c(1:25),encoding="UTF-8",colClasses=uniqueMarketColClasses)
    }, finally = {
      
    })
  }
  
}
#根据stock id打开某个个股的所有交易数据
openDailyTradeFile <- function(stock_id) {
  fileName <- paste(".\\market\\unique\\",stock_id,".xlsx",sep="")
  stockDailyTradeFile <- read.xlsx2(fileName, sheetIndex=1,header=TRUE,encoding="UTF-8",colClasses=c("Date","character","character",rep("numeric", 3),rep("integer",2),rep("numeric", 2),"integer",rep("numeric", 9),rep("integer",2),"character",rep("numeric", 2)), keepFormulas=FALSE, stringsAsFactors=FALSE)  
  return(stockDailyTradeFile)
}

#根据stock id打开某个个股的所有资金流向数据
openDailyCapFile <- function(stock_id) {
  fileName <- paste("C:\\xtang\\workspace\\R\\stockAnalysis\\capital\\unique\\",stock_id,".xlsx",sep="")  
  #stockDailyCapFile <- read.xlsx2(fileName, sheetIndex=1,header=TRUE,encoding="UTF-8",colClasses=c("Date","character","character",rep("numeric", 20),rep("integer",3)), keepFormulas=FALSE, stringsAsFactors=FALSE)  
  stockDailyCapFile <- read.xlsx2(fileName,sheetIndex=1,header=TRUE, encoding="UTF-8",colClasses=uniqueCapColClasses, stringsAsFactors=FALSE,colIndex=(1:24))
  return(stockDailyCapFile)  
}



#获得某天所有股票的资金流向文件和某天所有股票的交易文件，然后将两文件中的数据做某些交集合并
combineCapAndMarketData <- function(dailyCapFile,dailyMarketFile) {
  dailyCapFile<-na.omit(dailyCapFile)
  dailyMarketFile<-na.omit(dailyMarketFile)
  date <- dailyCapFile[1,1]
  #print(date)
  for(i in 1:nrow(dailyCapFile)) {
  #for(i in 1:1) {
    stockId <- dailyCapFile[i,2]
    uniqDailyCapFile <- openDailyCapFile(stockId)
    exist <- which(dailyMarketFile$代码 == stockId)
    if(length(exist) == 0) {
      print(paste("skip",stockId,"for market\\unique\\ dir"))
      next
    }
    uniqDailyTradeFile <- openDailyTradeFile(stockId)
    
    netBuyingRate <- calNetBuying(uniqDailyTradeFile,uniqDailyCapFile,date)
    #print(paste("netBuyingRate",netBuyingRate))
    
    fileName <- paste("C:\\xtang\\workspace\\R\\stockAnalysis\\capital\\unique\\",stockId,".xlsx",sep="")
    row <- which(uniqDailyCapFile$日期 == date)
    #print(paste("row:",as.integer(row)))
    col <- which(names(uniqDailyCapFile) == "净买率")
    #print(paste("col:",as.integer(col)))
    
    if(length(row) == 0) {
      print(paste("Warning: can not find date ",date," in ",fileName,sep=""))
      next
    } else if (length(row) > 1) {
      print(paste("ERROR: find multiple date ",date," in ",fileName,sep=""))
      break
    }
    
    if(length(col) == 0) {
      print(paste("Warning: can not find column 净买率 in ",fileName,sep=""))
      next
    } else if (length(col) > 1) {
      print(paste("ERROR: find multiple columns 净买率 in ",fileName,sep=""))
      break
    }
    
#     wb <- loadWorkbook(fileName)
#     sheet <- getSheets(wb)
#     row <- getRows(sheet[[1]],rowIndex=as.integer(row))
#     print(c(col))
#     cell <- getCells(row,c(col))
#     print(cell)
#     values <- getCellValue(cell)
#     print(values)
#     setCellValue(cell,netBuyingRate)
#     saveWorkbook(wb,fileName)
  
   uniqDailyCapFile <- uniqDailyCapFile[,-length(uniqDailyCapFile)]
   #print(uniqDailyCapFile)
   uniqDailyCapFile[row,col] <- netBuyingRate
   #print(paste("uniqDailyCapFile[row,col]",uniqDailyCapFile[row,col]))
   #write.xlsx2(uniqDailyCapFile,fileName,row.names=FALSE,append=FALSE)
   write.xlsx2(uniqDailyCapFile,fileName,row.names=FALSE,append=FALSE,colClasses=uniqueCapColClasses,colIndex=c(1:24),encoding="UTF-8")
  }
}

#净买率=净流入/流通市值*100
calNetBuying <- function(uniqDailyTradeFile,uniqDailyCapFile,date) {  
  if (uniqDailyTradeFile[1,3] != uniqDailyCapFile[1,3]) {
    errmsg <- paste("the trade file stock id ",uniqDailyTradeFile[1,3], " is not match to capital file ", uniqDailyCapFile[1,2],sep="")
    print(errmsg)
    return()    
  }
  
  tradeDateLine <- subset(uniqDailyTradeFile,uniqDailyTradeFile[,1] == date)  
  if (length(tradeDateLine) == 0) {
    errmsg <- paste("can not find daily trade line for stock ",uniqDailyTradeFile[1,3]," in ",date,sep="")
    print(errmsg)
    return()
  } 
  
  capDateLine <- subset(uniqDailyCapFile,uniqDailyCapFile[,1] == date)
  if (length(capDateLine) == 0) {
    errmsg <- paste("can not find captital line for stock ",capDateLine[1,3]," in ",date,sep="")
    print(errmsg)
    return()
  }
  
  #流通市值
  cmv <- tradeDateLine$流通市值
  #print(cmv)
  
  #vol <- tradeDateLine$成交额
  #print(vol)
  
  #净流入
  ni <- capDateLine$主力净流入
  #print(ni)
  
  if (cmv == 0 || length(cmv) == 0) {
    netBuyingRate <- 0
  } else {
    netBuyingRate <- round((ni/cmv)*100,2)  
  }
  
  #netBuyingRate <- round((ni/vol),2)
  print(paste(uniqDailyTradeFile[1,2],date,"净买率:",netBuyingRate))
  
  return(netBuyingRate)
}

flushData <- function(from, to) {
  fromDate <- as.Date(from)
  toDate <- as.Date(to)
  date <- fromDate
  while(date <= toDate) {
    dcf <- openCapitalFile(date)
    if(!is.null(dcf)) {
      writeToUniqueCapital(dcf)
    }
         
    dtf <- openHSADailyTradeFile(date)
    if(!is.null(dtf)) {
      writeToUniqueDailyTrade(dtf)  
    }
    
    if(!is.null(dcf) && !is.null(dtf)) {
      combineCapAndMarketData(dcf,dtf)  
    }    
    
    date <- date + 1
  }
}
