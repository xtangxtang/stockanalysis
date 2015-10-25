setwd("C:\\xtang\\workspace\\mystockAnalysis")
library(sqldf)
library(RCurl)
library(quantmod)
library(plyr)
library(logging)
library(futile.logger)
library(base)
flog.threshold(INFO) 

#file <- readRXSJFile("C:\\xtang\\workspace\\R\\stockAnalysis\\rxsj\\000001.txt")
#partingDataDF[order(as.Date(partingDataDF$日期,format="%d/%m/%Y")),]

Sys.setenv(http_proxy="http://child-prc.intel.com:913")
historyTradeLink <- "http://market.finance.sina.com.cn/downxls.php?date=2015-09-21&symbol=sz300205"
#download.file(historyTradeLink,destfile=".\\test.xlsx")

#读取日线数据
rxsj_col_names <- c("日期","开盘","最高","最低","收盘","成交量","成交额")
rxsj_colClasses <- c("Date","numeric","numeric","numeric","numeric","numeric","numeric")
readRXSJFile <- function(filename) {
  lines <- length(readLines(filename)) 
  #print(lines-3)
  fileDF <- read.csv(filename,sep=",", header=FALSE,skip=2,nrows=(lines-3),col.names=rxsj_col_names,colClasses=rxsj_colClasses)
  return(fileDF)
}

getAllRXSJFiles <- function() {
  wd <- getwd()
  filePath <- paste(wd,"//rxsj",sep="") 
  files <- list.files(filePath)
  return(files)
}

calculateTopParting <- function(origData,startDate,endDate) {
  flog.debug("--------------------------------------------------------------------")
  flog.debug(paste("calculate date between",startDate,"and",endDate))
  
  startDateInds <- shiftDateToRightIfNA(origData,startDate)
  endDateInds <- shiftDateToLeftIfNA(origData,endDate)
  startDate <- origData[startDateInds,]$日期
  endDate <- origData[endDateInds,]$日期
  
  flog.debug(paste("after shift,calculate date between",
                   startDate,"and",endDate))
  
  partingDataDF <- data.frame(
    日期 = as.Date(character("0")),
    开盘 = numeric(0),
    最高 = numeric(0),
    最低 = numeric(0),
    收盘 = numeric(0),
    成交量 = numeric(0),
    成交额 = numeric(0)) 
  
  sDate <- as.Date(startDate)
  start <- -1
  eDate <- as.Date(endDate)
  end <- -1
  
  if ((sDate + 2) > eDate) {
    flog.debug(paste("the start date",startDate,"is less than",endDate,"- 3 "))
    return(NULL)
  }
  
  
  len <- nrow(origData)
  lastDate <- as.Date(origData[len,1])
  flog.debug(paste("lastDate",lastDate))
  while(eDate > sDate) {
    end <- which(origData$日期 == eDate)
    if(length(end)!=0) {
      if(eDate != as.Date(endDate)) {
        flog.warn(paste("WARNING:shift end date from",endDate,"to",eDate))  
      }      
      break
    } 
    eDate <- eDate-1   
  }
  
#   shiftDate2LeftIfNa(origData,eDate,sDate)
#   flog.debug(paste("after shift to left the eDdate is",eDate))
  flog.debug(paste("1 sDate",sDate,"eDate",eDate))
  while(sDate < eDate) {
    start <- which(origData$日期 == sDate)
    if(length(start)!=0) {
      if(sDate != as.Date(startDate)) {
        flog.warn(paste("WARNING:shift start date from",startDate,"to",sDate))
      }      
      break
    } 
    sDate <- sDate + 1
  }
  flog.debug(paste("2 sDate",sDate,"eDate",eDate))
#   shiftDate2RightIfNa(origData,sDate,eDate)
#   flog.debug(paste("after shift to right the sDdate is",sDate))
  
  if(sDate == eDate) {
    flog.debug(paste("can not find start date",startDate))
    return(NULL)
  }
  #subset according to duration
  subData <- subset(origData,origData$日期>=sDate & origData$日期<= eDate,na.rm = TRUE)
  flog.debug(paste("length subdata:",nrow(subData)))
  #get high price in the duration
  highestPrice <- max(subData$最高,na.rm = TRUE)
  flog.debug(paste("highestPrice:",highestPrice))
  #subset according to the high price
  highestDataInds <- which(subData$最高==highestPrice)
  print(subData[highestDataInds,])
  highestData <- subData[highestDataInds,]

  # if the highest price reaches most left or most right, it is not the parting
  highDate <- as.Date(highestData[1,1])
  flog.debug(paste("highDate:",highDate))
  flog.debug(paste("subData[1,1]:",subData[1,1]))
  flog.debug(paste("subData[,1]:",subData[length(subData[,1]),1]))
  if(highDate == subData[length(subData[,1]),1]) {
    flog.info(paste("the highest price date reaches most right. most left:",subData[1,1],"highDate:",highDate,"most right:",subData[length(subData[,1]),1]))
    #pathToLeftFirstLowestPrice(subData,)
    return(calculateTopParting(subData,sDate,highDate-1))
  }
   
  if(highDate == subData[1,1]) {
    flog.info(paste("the highest price date reaches most left. most left:",subData[1,1],"highDate:",highDate,"most right:",subData[length(subData[,1]),1]))
    #return(calculateTopParting(id,subData,sDate-1,highDate))
    return(calculateTopParting(subData,highDate+1,eDate))
  }

  #check if the highest price date is already record
  inds <- which(partingDataDF$日期==highDate)
  if(length(inds) == 0) {
    partingDataDF <- rbind(partingDataDF,highestData)
    partingDataDF <- partingDataDF[order(as.Date(partingDataDF$日期,format="%d/%m/%Y")),]
    flog.debug(paste("partingDataDF$日期",partingDataDF[1,1]))
    flog.info(paste("insert highest date",highDate)) 
  } else {
    flog.error(paste("find the same high date",highDate,"between",sDate,"and",eDate))
    return(NULL)
  }

  newDf <- calculateTopParting(subData,sDate,highDate-1)
  if (!is.null(newDf) && nrow(newDf) != 0) {
    partingDataDF <- rbind(partingDataDF,newDf)
    partingDataDF <- partingDataDF[order(as.Date(partingDataDF$日期,format="%d/%m/%Y")),]    
  }

  newDf <- calculateTopParting(subData,highDate+1,eDate)
  if(!is.null(newDf) && nrow(newDf) != 0) {
    partingDataDF <- rbind(partingDataDF,newDf)
    partingDataDF <- partingDataDF[order(as.Date(partingDataDF$日期,format="%d/%m/%Y")),]      
  }

  return(partingDataDF)
}


calculateBottomParting <- function(origData,startDate,endDate) {
  flog.debug("--------------------------------------------------------------------")
  flog.debug(paste("calculate bottom parting date between",startDate,"and",endDate))
  
  startDateInds <- shiftDateToRightIfNA(origData,startDate)
  endDateInds <- shiftDateToLeftIfNA(origData,endDate)
  startDate <- origData[startDateInds,]$日期
  endDate <- origData[endDateInds,]$日期
  
  flog.debug(paste("after shift,calculate date between",
                   startDate,"and",endDate))  
  
  partingDataDF <- data.frame(
    日期 = as.Date(character("0")),
    开盘 = numeric(0),
    最高 = numeric(0),
    最低 = numeric(0),
    收盘 = numeric(0),
    成交量 = numeric(0),
    成交额 = numeric(0))  
  
  sDate <- as.Date(startDate)
  start <- -1
  eDate <- as.Date(endDate)
  end <- -1
  
  if ((sDate + 2) > eDate) {
    flog.debug(paste("the start date",startDate,"is less than",endDate,"- 3 "))
    return(NULL)
  }
  
  
  len <- nrow(origData)
  lastDate <- as.Date(origData[len,1])
  flog.debug(paste("lastDate",lastDate))
  while(eDate > sDate) {
    end <- which(origData$日期 == eDate)
    if(length(end)!=0) {
      if(eDate != as.Date(endDate)) {
        flog.warn(paste("WARNING:shift end date from",endDate,"to",eDate))  
      }      
      break
    } 
    eDate <- eDate-1   
  }
  
  #   shiftDate2LeftIfNa(origData,eDate,sDate)
  #   flog.debug(paste("after shift to left the eDdate is",eDate))
  flog.debug(paste("1 sDate",sDate,"eDate",eDate))
  while(sDate < eDate) {
    start <- which(origData$日期 == sDate)
    if(length(start)!=0) {
      if(sDate != as.Date(startDate)) {
        flog.warn(paste("WARNING:shift start date from",startDate,"to",sDate))
      }      
      break
    } 
    sDate <- sDate + 1
  }
  flog.debug(paste("2 sDate",sDate,"eDate",eDate))
  #   shiftDate2RightIfNa(origData,sDate,eDate)
  #   flog.debug(paste("after shift to right the sDdate is",sDate))
  
  if(sDate == eDate) {
    flog.debug(paste("can not find start date",startDate))
    return(NULL)
  }
  #subset according to duration
  subData <- subset(origData,origData$日期>=sDate & origData$日期<= eDate,na.rm = TRUE)
  flog.debug(paste("length subdata:",nrow(subData)))
  #get low price in the duration
  lowestPrice <- min(subData$最低,na.rm = TRUE)
  #subset according to the low price
  lowestData <- subset(subData,subData$最低==lowestPrice)
  lowDate <- lowestData[1,1]
  
  # if the lowest price reaches most left or most right, it is not the parting
  lowDate <- as.Date(lowestData[1,1])
  flog.debug(paste("lowDate:",lowDate))
  flog.debug(paste("subData[1,1]:",subData[1,1]))
  flog.debug(paste("subData[,1]:",subData[length(subData[,1]),1]))
  if(lowDate == subData[length(subData[,1]),1]) {
    flog.info(paste("the lowest price date reaches most right. most left:",subData[1,1],"lowDate:",lowDate,"most right:",subData[length(subData[,1]),1]))
    return(calculateBottomParting(subData,sDate,lowDate-1))
  }
  
  if(lowDate == subData[1,1]) {
    flog.info(paste("the lowest price date reaches most left. most left:",subData[1,1],"lowDate:",lowDate,"most right:",subData[length(subData[,1]),1]))
    return(calculateBottomParting(subData,lowDate+1,eDate))
  }
  
  #check if the lowest price date is already record
  inds <- which(partingDataDF$日期==lowDate)
  if(length(inds) == 0) {
    partingDataDF <- rbind(partingDataDF,lowestData)
    partingDataDF <- partingDataDF[order(as.Date(partingDataDF$日期,format="%d/%m/%Y")),]
    flog.debug(paste("partingDataDF$日期",partingDataDF$日期))
    flog.info(paste("insert lowest date",lowDate))    

  } else {
    flog.error(paste("find the same low date",lowDate,"between",sDate,"and",eDate))
    return(NULL)
  }
  
  newDf <- calculateBottomParting(subData,sDate,lowDate-1)
  print("-------------2 bottom newDf")
  print(newDf)
  if(!is.null(newDf) && nrow(newDf) != 0) {
    partingDataDF <- rbind(partingDataDF,newDf)
    partingDataDF <- partingDataDF[order(as.Date(partingDataDF$日期,format="%d/%m/%Y")),]        
  }

  newDf <- calculateBottomParting(subData,lowDate+1,eDate)
  print("-------------2.1 bottom newDf")
  print(newDf)  
  if (!is.null(newDf) && nrow(newDf) != 0) {
    partingDataDF <- rbind(partingDataDF,newDf)
    partingDataDF <- partingDataDF[order(as.Date(partingDataDF$日期,format="%d/%m/%Y")),]    
  }

  return(partingDataDF)
}

validateBottomPartingType <- function(totalDataDF,bottom) {
  botLeftDate <- bottom[1,1] - 1
  oLeftDate <- botLeftDate 

  inds <- -1
  while(botLeftDate > totalDataDF[1,1]) {
    inds <- which(totalDataDF$日期 == botLeftDate)
    if(length(inds)!=0) {
      if(botLeftDate != as.Date(oLeftDate)) {
        flog.warn(paste("WARNING:shift left date from",oLeftDate,"to",botLeftDate))  
      }      
      break
    } 
    botLeftDate <- botLeftDate-1   
  }  
  
  if(length(inds) == 0) {
    flog.warn(paste("the bottom left date",botLeftDate,"is not found in total data"))
    return(FALSE)
  }
  botLeftLowPrice <- totalDataDF$最低[inds]
  
  botRightDate <- bottom[1,1] + 1
  oRightDate <- botRightDate
  
  inds <- -1
  while(botRightDate < totalDataDF[nrow(totalDataDF),1]) {
    inds <- which(totalDataDF$日期 == botRightDate)
    if(length(inds)!=0) {
      if(botRightDate != as.Date(oRightDate)) {
        flog.warn(paste("WARNING:shift left date from",oRightDate,"to",botRightDate))  
      }      
      break
    } 
    botRightDate <- botRightDate+1   
  }   
  
  if(length(inds) == 0) {
    flog.warn(paste("the bottom right date",botRightDate,"is not found in total data"))
    return(FALSE)
  }     
  botRightLowPrice <- totalDataDF$最低[inds]
  
  bottomLowPrice <- bottom$最低[1]
  flog.debug(paste("check botLeftLowPrice",botLeftLowPrice,"bottomLowPrice",bottomLowPrice,"botRightDate",botRightDate))
  
  if(min(botLeftLowPrice,bottomLowPrice,botRightLowPrice) != bottomLowPrice) {
    flog.error(paste("the bottomLowDate",bottom[1,1],"is not the bottom parting, check failed!"))
    return(FALSE)
  }
  return(TRUE)
}

validateLowPriceBetweenTwoBtm <- function(totalDataDF,leftBot,rightBot) {
  leftBotDate <- leftBot[1,1]
  inds <- which(totalDataDF$日期 == leftBotDate)
  if(length(inds) == 0) {
    flog.warn(paste("the leftBotDate date",leftBotDate,"is not found in total data"))
    return(FALSE)
  }
  
  rightBotDate <- rightBot[1,1]
  inds <- which(totalDataDF$日期 == rightBotDate)
  if(length(inds) == 0) {
    flog.warn(paste("the rightBotDate date",rightBot,"is not found in total data"))
    return(FALSE)
  }
  
  df <- calculateBottomParting(totalDataDF,leftBotDate,rightBotDate)
  if(!is.null(df)) {
    flog.error(paste("there is another bottom parting",df[1,1],"between",leftBotDate,"and",rightBotDate,", check failed!"))
    return(FALSE)
  }
  return(TRUE)
}

validateBottomParting <- function(totalDataDF,bottomPartingDF) {
  for(i in 1:nrow(bottomPartingDF)) {
    bot1 <- bottomPartingDF[i,]
    bot1Date <- bot1[1,1]
    flog.debug(paste("check bottom parting",bot1Date))
    inds <- which(totalDataDF$日期 == bot1Date)
    if(length(inds) == 0) {
      flog.warn(paste("the bottom parting",bot1Date,"is not found in total data"))
      return(FALSE)
    }
    
    
    i <- i + 1
    if(i >nrow(bottomPartingDF)) {
      break
    }
    
    
    bot2 <- bottomPartingDF[i,]
    bot2Date <- bot2[1,1]
    flog.debug(paste("check bottom parting",bot2Date))
    inds <- which(totalDataDF$日期 == bot2Date)
    if(length(inds) == 0) {
      flog.warn(paste("the bottom parting",bot2Date,"is not found in total data"))
      return(FALSE)
    }
    
    if(!(validateBottomPartingType(totalDataDF,bot1)
         && validateLowPriceBetweenTwoBtm(totalDataDF,bot1,bot2))) {
      flog.error(paste("check bottom parting",bot1Date,"failed"))
      return(FALSE)
    }
  }
  return(TRUE)
}

findParting <- function(df,startDate=NULL,endDate=NULL) {
  if(is.null(startDate)) {
    startDate <- df[1,]$日期
  }
  
  if(is.null(endDate)) {
    endDate <- df[nrow(df),]$日期
  }
  
  topParting <- calculateTopParting(df,startDate,endDate)
#   print("---------- top parting--------------")
#   print(topParting)
  if (!is.null(topParting)) {
    topParting <- addOneColToDF(topParting,"收盘",c(TRUE),"顶分型")  
  }

  bottomParting <- calculateBottomParting(df,startDate,endDate)
  if(!is.null(bottomParting)) {
    bottomParting <- addOneColToDF(bottomParting,"收盘",c(FALSE),"顶分型")  
  }
  
#   print("----------bottomParting -------------")
#   print(bottomParting)
#   
#   print("--------------df-----------------------")
#   print(df)
  
  partingDataDF <- rbind(topParting,bottomParting)
#   print("----------partingDataDF -------------")
#   print(partingDataDF)  
  if(!is.null(partingDataDF)) {
    partingDataDF <- partingDataDF[order(as.Date(partingDataDF$日期,format="%d/%m/%Y")),]  
  }
  
  return(partingDataDF)
}

shiftDateToLeftIfNA <- function(df,date) {
  inds <- -1 
  oDate <- date
  while(date >= df[1,1]) {
    inds <- which(df$日期 == date)
    if(length(inds)!=0) {
      if(date != as.Date(oDate)) {
        flog.warn(paste("WARNING:shift date to left from",oDate,"to",date))  
      }      
      return(inds)
    } 
    date <- date-1  
  }
  return(inds)
}

shiftDateToRightIfNA <- function(df,date) {
  inds <- -1 
  oDate <- date
  while(date <= df[nrow(df),1]) {
    inds <- which(df$日期 == date)
    if(length(inds)!=0) {
      if(date != as.Date(oDate)) {
        flog.warn(paste("WARNING:shift date to right from",oDate,"to",date))  
      }      
      return(inds)
    } 
    date <- date+1  
  }
  return(inds)
}


#根据分型数据处理包含关系
processInclusion <- function(partingDataDF=data.frame(),origDataDF=data.frame(),startDate=NULL,endDate=NULL) {
  if(is.null(startDate)) {
    startDate <- origDataDF[1,1]
  } 
  
  if (is.null(endDate)) {
    endDate <- origDataDF[nrow(origDataDF),1]
  }
  
  flog.debug(paste("check date between",startDate,"and",endDate))  
  
  startInds <- which(origDataDF$日期 == startDate)
  if(length(startInds) == 0) {
    flog.error(paste("can not find sart date",startDate))
    return(NULL)
  }

  endInds <- which(origDataDF$日期 == endDate)
  if(length(endInds) == 0) {
    flog.error(paste("can not find end date",endDate))
    return(NULL)
  }
  
  emptyDF <- data.frame(
    日期 = as.Date(character("0")),
    开盘 = numeric(0),
    最高 = numeric(0),
    最低 = numeric(0),
    收盘 = numeric(0),
    顶分型 = logical(),
    成交量 = numeric(0),
    成交额 = numeric(0))
  
  
  
  #regionData <- subset(origDataDF,origDataDF$日期 >= startDate && origDataDF$日期 <= endDate)
  
  for(i in 1:nrow(partingDataDF)) {
    candiInds <- c()
    combinedDataDF <- emptyDF
    date <- partingDataDF[i,1]
    highestPrice <- partingDataDF[i,3]
    lowestPrice <- partingDataDF[i,4]
    isTopParting <- partingDataDF[i,6]
    
    flog.debug(paste("check",date,"by",highestPrice,"and",lowestPrice,"with isTopParting",isTopParting))
    
    inds <- which(origDataDF$日期 == date)
    if (length(inds) == 0) {
      flog.error(paste("can not find date",date))
      return(NULL)
    }
    
    if (length(inds) > 1) {
      flog.error(paste("find more than one date",inds))
      return(NULL)      
    }
    
    flog.debug(paste("inds",inds,"startInds",startInds,"endInds",endInds))
    #subData <- subset(origDataDF, origDataDF$最低 >= lowestPrice & origDataDF$最高 <= highestPrice)
    subData <- subset(origDataDF, (origDataDF$最低 >= lowestPrice & origDataDF$最高 <= highestPrice) | 
                        (origDataDF$收盘 >= 2293 & origDataDF$开盘 <= 2347) | (origDataDF$开盘 >= 2293 & origDataDF$收盘 <= 2347))

    #     subDataPlots <- which(origDataDF$日期 == subData$日期)
    #     flog.debug(paste("subDataPlots:",subDataPlots))
    
    #print(subData)

    if (nrow(subData) > 1) {
      j <- 1
      subDataPlots <- c()
      while(j < nrow(subData)) {
        sbdp <- which(origDataDF$日期 == subData[j,]$日期)
        if(length(sbdp) == 0) {
          flog.error(paste("can not find",subData[j,]$日期))
          return(NULL)
        }
        flog.debug(paste("sbdp",sbdp,"date",origDataDF[sbdp,1],"highprice",origDataDF[sbdp,3],"lowprice",origDataDF[sbdp,4]))
        subDataPlots <- c(subDataPlots,sbdp)
        j<-j+1
      }
      #flog.debug(paste("subDataPlots:",subDataPlots))
      #print(subDataPlots)
      
      if(length(subDataPlots) > 1) {
        partingPlot <- which(subData$日期 == date)
        flog.debug(paste("partingPlot",partingPlot,"for date",date))
        
        testPlot <- inds - 1
        while(testPlot %in% subDataPlots) {
          flog.debug(paste("add left candidate plot",testPlot))
          candiInds <- c(candiInds,testPlot)
          combinedDataDF <- rbind(combinedDataDF,origDataDF[testPlot,])
          testPlot <- testPlot - 1
        }
        
        testPlot <- inds + 1
        while(testPlot %in% subDataPlots) {
          flog.debug(paste("add right candidate plot",testPlot))
          candiInds <- c(candiInds,testPlot)
          combinedDataDF <- rbind(combinedDataDF,origDataDF[testPlot,])
          testPlot <- testPlot + 1
        }
                
        flog.debug(paste("candidate inds:",candiInds,"for",inds))
        if(length(candiInds) > 0) {
          combinedDataDF <- combinedDataDF[order(as.Date(combinedDataDF$日期,format="%d/%m/%Y")),]
          print(combinedDataDF)
          if(isTopParting) {
            highestLowPrice <- max(combinedDataDF[,4])
            flog.warn(paste("set the date",origDataDF[inds,1]," original low price",origDataDF[inds,4],"to",highestLowPrice))
            origDataDF[inds,4] <- highestLowPrice
          } else {
            lowestHighPrice <- min(combinedDataDF[,3])
            flog.warn(paste("set the date",origDataDF[inds,1]," original high price",origDataDF[inds,3],"to",lowestHighPrice))
            origDataDF[inds,3] <- lowestHighPrice            
          }
          
          for(i in 1:length(candiInds)) {            
            flog.warn(paste("set the date",as.Date(origDataDF[candiInds[i],1]),"value to NA"))
            origDataDF[candiInds[i],2:ncol(origDataDF)] <- NA
            flog.debug(origDataDF[candiInds[i],])
          }# end for
        }#end if
      }#end if      
    }#end if
    
    i <- i + 1
  }
  origDataDF <- na.omit(origDataDF)
  return(origDataDF)
}

#清除连续相同的分型，比如顶分型-顶分型，底分型-底分型
elimSameParting <- function(partingDF) {
  firstIndex <- 1
  first <- partingDF[firstIndex,]
  secondIndex <- 2
  second <- partingDF[secondIndex,]
  print(partingDF[nrow(partingDF),1])
  flog.debug(paste("check",first$日期,"and",second$日期,"with end date",partingDF[nrow(partingDF),1]))
  
  while(!is.na(second$日期) && second$日期 <= partingDF[nrow(partingDF),1]) {
    flog.debug(paste("check",first$日期,"and",second$日期))
    if (first$顶分型 == second$顶分型) {
      if(first$顶分型) {
        if(first$最高 > second$最高) {
          flog.warn(paste("remove top parting",second$日期))
          partingDF[secondIndex,2:ncol(partingDF)] <- NA
          secondIndex <- secondIndex + 1
          second <- partingDF[secondIndex,]          
        } else {
          flog.warn(paste("remove top parting",first$日期))
          partingDF[firstIndex,2:ncol(partingDF)] <- NA
          firstIndex <- secondIndex
          first <- partingDF[firstIndex,]
          secondIndex <- secondIndex + 1
          second <- partingDF[secondIndex,] 
        }
      }#end if(first$顶分型) 
      else { # it is 底分型
        if(first$最低 < second$最低) {
          flog.warn(paste("remove bottom parting",second$日期))
          partingDF[secondIndex,2:ncol(partingDF)] <- NA
          secondIndex <- secondIndex + 1
          second <- partingDF[secondIndex,]           
        } else {
          flog.warn(paste("remove bottom parting",first$日期))
          partingDF[firstIndex,2:ncol(partingDF)] <- NA
          firstIndex <- secondIndex
          first <- partingDF[firstIndex,]
          secondIndex <- secondIndex + 1
          second <- partingDF[secondIndex,] 
        }
      }#end else { # it is 底分型      
    }#end if (first$顶分型 == second$顶分型) 
    else {
      firstIndex <- secondIndex
      first <- partingDF[firstIndex,]
      secondIndex <- secondIndex + 1
      second <- partingDF[secondIndex,] 
    }
  }#end while
  partingDF <- na.omit(partingDF)
  return(partingDF)
}

#消除不是笔的分型,
#partingDF:通过elimSameParting消除了连续相同分型的分型数据
#combinedData:通过processInclusion处理了包含关系的原始数据

caculatePartingLinePot <- function(partingDF,combinedData) {
  lastLinePartingIndex <- 1
  lastLineParting <- partingDF[lastLinePartingIndex,]
  nextIndex <- 2
  nextParting <- partingDF[nextIndex,]
  #print(partingDF[nrow(partingDF),1])
  flog.debug(paste("caculatePartingLinePot:check",lastLineParting$日期,"and",nextParting$日期,"with end date",partingDF[nrow(partingDF),1]))
  
  emptyDF <- data.frame(
    日期 = as.Date(character("0")),
    开盘 = numeric(0),
    最高 = numeric(0),
    最低 = numeric(0),
    收盘 = numeric(0),
    顶分型 = logical(),
    成交量 = numeric(0),
    成交额 = numeric(0))
  
  linePartingDF <- emptyDF
  
  linePartingDF <- rbind(linePartingDF,lastLineParting)
  
  
  while(!is.na(nextParting$日期) && nextParting$日期 <= partingDF[nrow(partingDF),1]) {
    flog.debug(paste("caculatePartingLinePot:check",lastLineParting$日期,"and",nextParting$日期))
    
    lastLinePartingPlotInOrigData <- which(combinedData$日期 == lastLineParting$日期)
    if(length(lastLinePartingPlotInOrigData) == 0) {
      flog.error(paste("can not find lastLineParting date",lastLineParting$日期))
      return(NULL)
    }
    nextPlotInOrigData <- which(combinedData$日期 == nextParting$日期)
    if(length(nextPlotInOrigData) == 0) {
      flog.error(paste("can not find nextParting date",nextParting$日期))
      return(NULL)
    }
    
    flog.debug(paste("lastLinePartingPlotInOrigData",lastLinePartingPlotInOrigData,"nextPlotInOrigData",nextPlotInOrigData))
    
    if(abs(lastLinePartingPlotInOrigData - nextPlotInOrigData) > 3 && lastLineParting$顶分型!=nextParting$顶分型) {
      if (lastLineParting$顶分型 && lastLineParting$最高 > nextParting$最高) {
        linePartingDF <- rbind(linePartingDF,nextParting)
        lastLinePartingIndex <- nextIndex
        lastLineParting <- partingDF[lastLinePartingIndex,]
      } else if (!lastLineParting$顶分型 && lastLineParting$最高 < nextParting$最高) {
        linePartingDF <- rbind(linePartingDF,nextParting)
        lastLinePartingIndex <- nextIndex
        lastLineParting <- partingDF[lastLinePartingIndex,]
      }
      
      nextIndex <- nextIndex+1
      nextParting <- partingDF[nextIndex,]      
    } else if (abs(lastLinePartingPlotInOrigData - nextPlotInOrigData) >3 && lastLineParting$顶分型==nextParting$顶分型) {
      if (lastLineParting$顶分型 && lastLineParting$最高 < nextParting$最高) {
        linePartingDF <- rbind(linePartingDF,nextParting)
        lastLinePartingIndex <- nextIndex
        lastLineParting <- partingDF[lastLinePartingIndex,]
      } else if (!lastLineParting$顶分型 && lastLineParting$最低>nextParting$最低) {
        linePartingDF <- rbind(linePartingDF,nextParting)
        lastLinePartingIndex <- nextIndex
        lastLineParting <- partingDF[lastLinePartingIndex,]
      }      
      
      nextIndex <- nextIndex+1
      nextParting <- partingDF[nextIndex,]  
    } else if (abs(lastLinePartingPlotInOrigData - nextPlotInOrigData) <=3 && lastLineParting$顶分型!=nextParting$顶分型) {
      nextIndex <- nextIndex+1
      nextParting <- partingDF[nextIndex,]      
    } else if (abs(lastLinePartingPlotInOrigData - nextPlotInOrigData) <=3 && lastLineParting$顶分型==nextParting$顶分型) {
      if (lastLineParting$顶分型&&lastLineParting$最高<nextParting$最高) {
        linePartingDF <- rbind(linePartingDF,nextParting)
        
        lastLinePartingIndex <- nextIndex
        lastLineParting <- partingDF[lastLinePartingIndex,]
        nextIndex <- nextIndex+1
        nextParting <- partingDF[nextIndex,]        
      } else if (!lastLineParting$顶分型&&lastLineParting$最低>nextParting$最低) {
        linePartingDF <- rbind(linePartingDF,nextParting)
        
        lastLinePartingIndex <- nextIndex
        lastLineParting <- partingDF[lastLinePartingIndex,]
        nextIndex <- nextIndex+1
        nextParting <- partingDF[nextIndex,]         
      } else {
        nextIndex <- nextIndex+1
        nextParting <- partingDF[nextIndex,] 
      }
    } else {
      nextIndex <- nextIndex+1
      nextParting <- partingDF[nextIndex,] 
    }
  }
  
  return(linePartingDF)
}


removeUnexpectedCandidate <- function(candidates,origData) {
  if(nrow(candidates) == 1) {
    return(na.omit(candidates))
  }
  
  for(i in 1:nrow(candidates)) {
    candidate <- candidates[i,]
    isTopParting <- candidate$顶分型
    candDate <- candidate$日期
    
    
    if(isTopParting) {
      highDateInds <- which(origData$日期 == candDate)
      leftHightDateInds <- highDateInds - 1
      rightHightDateInds <- highDateInds + 1
           
      if (!(origData[leftHightDateInds,]$最低 < candidate$最低 
          && origData[rightHightDateInds,]$最低 < candidate$最低)) {
        candidates[i,2:ncol(candidate)] <- NA
      }      
    } else {
      lowDateInds <- which(origData$日期 == candDate)
      leftLowDateInds <- lowDateInds - 1
      rightLowDateInds <- lowDateInds + 1
      
      if (!(origData[leftLowDateInds,]$最高 > candidate$最高 
          && origData[rightLowDateInds,]$最高 > candidate$最高)) {
        candidates[i,2:ncol(candidate)] <- NA
      }      
    }    
  }
  candidates <- na.omit(candidates)
  return(candidates)
}

caculateLineStructure <- function(id,startDate=NULL,endDate=NULL) {  
  path <- getwd()
  fileName <- paste(path,"//rxsj//",id,".txt",sep="")
  flog.debug(paste("get file path",fileName))
  data <- readRXSJFile(fileName)
  
  if(is.null(startDate)) {
    startDate <- data[1,]$日期
  }
  
  if(is.null(endDate)) {
    end <- data[nrow(data),]$日期
  }
  
  data <- subset(data,data$日期 >= startDate & data$日期 <= endDate)
  
  startDateIndex <- which(data$日期==startDate)
  startData <- data[startDateIndex,]
  
  combinedOrigData <- concludeInclusion(data)
  
  secondPDF <- findParting(combinedOrigData,startDate,endDate)
  if(is.null(secondPDF)) {
    flog.warn(paste("the firstCandidate is null,maybe there is no parting from",startDate,"to",endDate,"please check the following data"))
    print(data)
    return(NULL)
  }
  #parting <- elimSameParting(firstPDf)
  #combinedOrigData <- processInclusion(firstCandidate,data,startDate,endDate)
  
  
#   secondPDF <- findParting(combinedOrigData,startDate,endDate)
#   if(is.null(secondPDF)) {
#     flog.warn(paste("the secondPDF is null,maybe after combined, there is no parting from",startDate,"to",endDate,"please check the following combined data"))
#     print(combinedOrigData)
#     return(NULL)
#   }
  secondPDF <- removeUnexpectedCandidate(secondPDF,combinedOrigData)
  parting <- elimSameParting(secondPDF)
  
  lineParting <- caculatePartingLinePot(parting,combinedOrigData)
  linePot <- elimSameParting(lineParting)
  
  lineStruct <- transformLinePot2LineStructure(linePot,startData)
  
  return(lineStruct)
}


# caculateLineStructure <- function(id,startDate=NULL,endDate=NULL) {  
#   path <- getwd()
#   fileName <- paste(path,"//rxsj//",id,".txt",sep="")
#   flog.debug(paste("get file path",fileName))
#   data <- readRXSJFile(fileName)
#   
#   if(is.null(startDate)) {
#     startDate <- data[1,]$日期
#   }
#   
#   if(is.null(endDate)) {
#     end <- data[nrow(data),]$日期
#   }
#   
#   startDateIndex <- which(data$日期==startDate)
#   startData <- data[startDateIndex,]
#   
#   firstCandidate <- findParting(data,startDate,endDate)
#   if(is.null(firstCandidate)) {
#     flog.warn(paste("the firstCandidate is null,maybe there is no parting from",startDate,"to",endDate,"please check the following data"))
#     print(data)
#     return(NULL)
#   }
#   #parting <- elimSameParting(firstPDf)
#   #combinedOrigData <- processInclusion(firstCandidate,data,startDate,endDate)
#   combinedOrigData <- concludeInclusion(data)
#   
#   secondPDF <- findParting(combinedOrigData,startDate,endDate)
#   if(is.null(secondPDF)) {
#     flog.warn(paste("the secondPDF is null,maybe after combined, there is no parting from",startDate,"to",endDate,"please check the following combined data"))
#     print(combinedOrigData)
#     return(NULL)
#   }
#   secondPDF <- removeUnexpectedCandidate(secondPDF,combinedOrigData)
#   parting <- elimSameParting(secondPDF)
#   
#   lineParting <- caculatePartingLinePot(parting,combinedOrigData)
#   linePot <- elimSameParting(lineParting)
#   
#   lineStruct <- transformLinePot2LineStructure(linePot,startData)
#   
#   return(lineStruct)
# }


#将分笔的点结构转换成笔的表示形势
transformLinePot2LineStructure <- function(caculatedLinePot,startRow) {
  lineStructure <- data.frame(
    日期 = as.Date(character("0")),
    最高 = numeric(0),
    最低 = numeric(0),
    向上 = logical())

  j <- 1
  for(i in 2:nrow(caculatedLinePot)) {
    first <- caculatedLinePot[i-1,]
    second <- caculatedLinePot[i,]
    date <- first$日期
    
    if(first$顶分型 && !second$顶分型) {
      if(j == 1) {
        lineStructure <- rbind(lineStructure,data.frame(
          日期=startRow$日期,最高=startRow$最高,最低=startRow$最低,向上=TRUE))        
      }
      lineStructure <- rbind(lineStructure,data.frame(
        日期=date,最高=first$最高,最低=second$最低,向上=FALSE))
    } else if (!first$顶分型 && second$顶分型) {
      if(j == 1) {
        lineStructure <- rbind(lineStructure,data.frame(
          日期=startRow$日期,最高=startRow$最高,最低=startRow$最低,向上=FALSE))        
      }      
      lineStructure <- rbind(lineStructure,data.frame(
        日期=date,最高=second$最高,最低=first$最低,向上=TRUE))
    } else {
      flog.error(paste("the line pot is error"))
      return(NULL)
    }
    j <- j + 1
    i <- i + 1
  }
  return(lineStructure)
}



#将笔的结构“伪装”成交易数据
fakeLineStruce2Kline <- function(lineStructure) {
  lineStructure <- addOneColToDF(lineStructure,"日期",0,"开盘")
  lineStructure <- addOneColToDF(lineStructure,"最低",0,"收盘")
  lineStructure <- addOneColToDF(lineStructure,"收盘",0,"成交量")
  lineStructure <- addOneColToDF(lineStructure,"收盘",0,"成交额")
  
  applyFun <- function(x) {
    print(x$日期)
    if(x$向上) {
      x$开盘 <- x$最低
      x$收盘 <- x$最高
    } else {
      x$开盘 <- x$最高
      x$收盘 <- x$最低
    }
    print(x)
  }
  
  lineStructure <- adply(lineStructure,1, applyFun)
  lineStructure <- lineStructure[,-which(names(lineStructure)=="向上")]
  return(lineStructure)
}

concludeUpCharactSequence <- function(lineStructure) {
  return(adply(lineStruct,1, function(x) if(!x$向上) return(x)))
}

concludeDownCharactSequence <- function(lineStructure) {
  return(adply(lineStruct,1, function(x) if(x$向上) return(x)))
}

#lineStruct <- caculateLineStructure("000001","2013-06-25","2015-09-22")
concludeSegment <- function(lineStruct) {
  
  segment <- data.frame(
    日期 = as.Date(character("0")),
    开盘 = numeric(0),
    最高 = numeric(0),
    最低 = numeric(0),
    收盘 = numeric(0),
    顶分型 = logical(),
    isUpSeq = logical(),
    成交额 = numeric(0),
    成交量 = numeric(0))
  
  #segment <- rbind(segment,lineStruct[1,])
  
  upSeq <- concludeUpCharactSequence(lineStruct)
  starndarUpSeq <- concludeInclusion(upSeq)
  fakeUpKLine <- fakeLineStruce2Kline(starndarUpSeq)
  upSeqParting <- findParting(fakeUpKLine)
    
  downSeq <- concludeDownCharactSequence(lineStruct)
  starndarDownSeq <- concludeInclusion(downSeq)
  fakeDownKLine <- fakeLineStruce2Kline(starndarDownSeq)
  downSeqParting <- findParting(fakeDownKLine)
  
#   stardSeq <- rbind(starndarUpSeq,starndarDownSeq)
#   stardSeq <- stardSeq[order(as.Date(stardSeq$日期,format="%d/%m/%Y")),]  
#   fakeStardSeqKLine <- fakeLineStruce2Kline(stardSeq)
#   drawKline(fakeStardSeqKLine)  
  print("####starndarUpSeq:")
  print(starndarUpSeq)  
  print("####starndarDownSeq:")
  print(starndarDownSeq)  
  print("####upSeqParting:")
  print(upSeqParting)
  print("####downSeqParting:")
  print(downSeqParting)
  

  
  upSeqParting<-addOneColToDF(upSeqParting,"顶分型",TRUE,"isUpSeq")
  downSeqParting<-addOneColToDF(downSeqParting,"顶分型",FALSE,"isUpSeq")

  seqParting <- rbind(upSeqParting,downSeqParting)
  seqParting <- seqParting[order(as.Date(seqParting$日期,format="%d/%m/%Y")),]
  print(seqParting)

  validateParting <- function(parting,inds) {    
    if(length(inds) == 0) {
      flog.error(paste("can not find",parting[i,]$日期,"in starndarUpSeq"))
      return(NULL)
    }
    
    left <- parting[inds-1,]
    mid <- parting[inds,]
    right <- parting[inds+1,]
    
    if(is.null(left) || is.na(left) || nrow(left) == 0) {
      flog.error(paste("parting",parting[inds,]$日期,"left data is empty"))
      return(FALSE)
    }
    
    if(is.null(right) || is.na(right) || nrow(right) == 0) {
      flog.error(paste("parting",parting[inds,]$日期,"right data is empty"))
      return(FALSE)
    }
    
    return(TRUE)
  }

  i <- 1
  #isUp <- lineStruct[i,]$向上
  while(i <= nrow(seqParting)) {
    if(seqParting[i,]$isUpSeq & seqParting[i,]$顶分型 ) { 
      #是向上开始的一笔的特征序列，并且是顶分型
      inds <- which(starndarUpSeq$日期 == seqParting[i,]$日期)      
      if(!validateParting(starndarUpSeq,inds)) {
        return(NULL)
      }
      
      left <- starndarUpSeq[inds-1,]
      mid <- starndarUpSeq[inds,]
      right <- starndarUpSeq[inds+1,]
      
      if(left$最高 >= mid$最低) {
        #有重合区域
        segment <- rbind(segment,seqParting[i,])
        flog.info(paste("find up segment at date",seqParting[i,]$日期))
        i <- i + 1
      } else {
        #有跳空缺口
        j <- i + 1
        while(j <= nrow(seqParting)) {
          if (seqParting[j,]$isUpSeq & seqParting[j,]$顶分型) {
            #有一个新的向上序列顶分型,以该顶分型为起点，重新找向下特征序列的底分型
            break            
          } else if(!seqParting[j,]$isUpSeq & !seqParting[j,]$顶分型) {
            #找到向下特征序列的底分型
            segment <- rbind(segment,seqParting[i,])
            flog.info(paste("find up segment at date",seqParting[i,]$日期))
            break            
          }
          j <- j + 1
        }
        i <- j
      }
    } #end if (seqParting[i,]$isUpSeq & seqParting[i,]$顶分型 )  
    else if (!seqParting[i,]$isUpSeq & !seqParting[i,]$顶分型 ) {
      inds <- which(starndarDownSeq$日期 == seqParting[i,]$日期)      
      if(!validateParting(starndarDownSeq,inds)) {
        return(NULL)
      }
      
      left <- starndarDownSeq[inds-1,]
      mid <- starndarDownSeq[inds,]
      right <- starndarDownSeq[inds+1,]
      
      if(left$最低 <= mid$最高) {
        #有重合区域
        segment <- rbind(segment,seqParting[i,])
        flog.info(paste("find down segment at date",seqParting[i,]$日期))
        i <- i + 1
      } else {
        #有跳空缺口
        j <- i + 1
        while(j <= nrow(seqParting)) {
          if (!seqParting[j,]$isUpSeq & !seqParting[j,]$顶分型 ) {
            #有一个新的向下序列底分型,以该顶分型为起点，重新找向上特征序列的顶分型
            break            
          } else if(seqParting[j,]$isUpSeq & seqParting[j,]$顶分型) {
            #找到向上特征序列的顶分型
            segment <- rbind(segment,seqParting[i,])
            flog.info(paste("find down segment at date",seqParting[i,]$日期))
            break            
          }
          j <- j + 1
        }
        i <- j
      }
    } # end !seqParting[i,]$isUpSeq & !seqParting[i,]$顶分型 
    i <- i + 1
  }
  return(segment)
}


concludeInclusion <- function(data) {
  #flog.debug(data)
  if(is.null(data) || nrow(data) == 0) {
    flog.info("data is null or empty")
    return(NULL)
  }
  
  flog.debug(paste("nrow data",nrow(data)))
  
  if(nrow(data) <= 2) {
    return(data)
  }
  
  i <- 1
  j <- i + 1
  
  while(j <= nrow(data)) { #skip all the first inclusion relationship, i always point to the high length Kline
    first <- data[i,]
    second <- data[j,]
    if(first$最高>=second$最高 & first$最低 <= second$最低) {
      data[j,2:ncol(data)] <- NA
      j <- j + 1
    } else if (first$最高<second$最高 & first$最低 > second$最低) {
      data[i,2:ncol(data)] <- NA
      i <- j
      j <- j + 1
    } else {
      break
    }
  }
  
  flog.debug(paste("after skip all the first inclusion relationship","i",i,"j",j))
  
  if(j > nrow(data)) {
    flog.warn(paste("all the data between",data[1,]$日期,"and",data[nrow(data),]$日期,"is the inclusion relationship"))
    return(NULL)
  }
  
  p <- i
  i <- j
  j <- j + 1
  while(j <= nrow(data)) {
    flog.debug(paste("i",i,"j",j))
    left <- data[p,]
    mid <- data[i,]
    right <- data[j,]
    print(left)
    print(mid)
    print(right)
    
    if((left$最高>=mid$最高 & left$最低 <= mid$最低) || (left$最高<mid$最高 & left$最低 > mid$最低)) {
      flog.error(paste("the left one",left$日期,"should not have inclusion relationship with mid",mid$日期))
      return(NULL)
    }
    
    if(left$最低 >= mid$最低) {
      # like                 |          |   |
      #  |         |         | |        | | |
      #  | | | or  | | | or    |  |  or   |
      #    |           |          |
      #
      if (mid$最高>=right$最高 & mid$最低 <= right$最低) { # mid include right
        data[i,]$最高 <- right$最高
        flog.debug(paste("delete right",right$日期,"data"))
        data[j,2:ncol(data)] <- NA
        j <- j + 1
      } else if (mid$最高<right$最高 & mid$最低 > right$最低) { # right include mid
        data[j,]$最高 <- mid$最高
        flog.debug(paste("delete mid",mid$日期,"data"))
        data[i,2:ncol(data)] <- NA
        i <- j
        j <- j + 1
      } else {
        p <- i
        i <- j
        j <- j + 1
      }
    } else { #if (left$最低 < mid$最低) 
      # like
      #                          |
      #    | | or   | | or    |  |  or    |
      #  | |      |   |    |  |        |  |  |
      #  |        |        |           |     |
      #
      if (mid$最高>=right$最高 & mid$最低 <= right$最低) { # mid include right
        data[i,]$最低 <- right$最低
        flog.debug(paste("delete mid",right$日期,"data"))
        data[j,2:ncol(data)] <- NA
        j <- j + 1
      } else if (mid$最高<right$最高 & mid$最低 > right$最低) { # right include mid
        data[j,]$最低 <- mid$最低
        flog.debug(paste("delete right",mid$日期,"data"))
        data[i,2:ncol(data)] <- NA
        i <- j
        j <- j + 1
      } else {
        p <- i
        i <- j
        j <- j + 1
      }      
    }  
  }

  return(na.omit(data))
}

optimizeSegment <- function(segmentStruct) {
  if(is.null(segmentStruct)) {
    flog.error(paste("segmentStruct is null"))
    return(NULL)
  }
  
  i <- 1
  while(i < nrow(segmentStruct)) {
    isTopParting <- segmentStruct[i,]$顶分型
    isUpSeq <- segmentStruct[i,]$isUpSeq
    
    if(isTopParting & isUpSeq) {
      j <- i + 1
      while(j < nrow(segmentStruct)) {
        nextIsTopParting <- segmentStruct[j,]$顶分型
        nextIsUpSeq <- segmentStruct[j,]$isUpSeq
        if ((nextIsTopParting == isTopParting) & (isUpSeq == nextIsUpSeq)) {
          if(segmentStruct[j,]$最高 > segmentStruct[i,]$最高) {
            segmentStruct[i,2:ncol(segmentStruct)] <- NA
            i <- j
          } else {
            segmentStruct[j,2:ncol(segmentStruct)] <- NA
          }
          j <- j + 1  
          next
        } else {
          break
        }
      }
      i <- j
    } else if (!isTopParting & !isUpSeq) {
      j <- i + 1
      while(j < nrow(segmentStruct)) {
        nextIsTopParting <- segmentStruct[j,]$顶分型
        nextIsUpSeq <- segmentStruct[j,]$isUpSeq
        if ((nextIsTopParting == isTopParting) & (isUpSeq == nextIsUpSeq)) {
          if(segmentStruct[j,]$最低 < segmentStruct[i,]$最低) {
            segmentStruct[i,2:ncol(segmentStruct)] <- NA
            i <- j
          } else {
            segmentStruct[j,2:ncol(segmentStruct)] <- NA
          }
          j <- j + 1  
          next
        } else {
          break
        }
      }
      i <- j
    } else {
      flog.error(paste("can not process isTopParting",isTopParting,"isUpSeq",isUpSeq))
      return(NULL)
    }
  }
  segmentStruct <- na.omit(segmentStruct)
  return(segmentStruct)
}



drawKline <- function(data) {
  data <- data[,-ncol(data)]
  data$日期 <- as.Date(data$日期,format="%Y%m%d")
  x <- xts(as.matrix(data[,2:ncol(data)]),data$日期)
  colnames(x) <- c('Open','High','Low','Close','Volume')
  #candleChart(x,TA=NULL)
  chart_Series(x, type = "candlesticks" ) 
}



