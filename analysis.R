# setwd("C:\\xtang\\workspace\\R\\stockAnalysis")
library(quantmod)
library(TTR)
library(plyr)
library(xlsx)
library(sqldf)

dailyData <- read.xlsx2("600519.xlsx",sheetIndex=1, header=TRUE, encoding="UTF-8",colClasses=c("Date",rep("numeric", 11),rep("integer",3)), keepFormulas=FALSE, stringsAsFactors=FALSE)

getSubData <- function(fromDate, toDate) {
  subset(dailyData, dailyData$日期 >= as.Date(fromDate) & dailyData$日期 <= as.Date(toDate))
}

statisConsRedGreenData <- function(subdata) {
  statisRedData <- data.frame()
  statisGreenData <- data.frame()
  staticSameData <- 0
  
  statisRedData[1,1] <- 0
  statisGreenData[1,1] <- 0
  
  continues_days <- 0
  last_is_red <- (subdata$涨跌幅[1]>0)
  
  for(i in 1:nrow(subdata)) {
    this_is_red=0
    if(subdata$涨跌幅[i] > 0 )  {
      this_is_red=1
    } else if (subdata$涨跌幅[i] < 0 ) {
      this_is_red=-1
    } else {
      staticSameData <- staticSameData + 1
    }
    
    if (this_is_red == last_is_red) {
      continues_days <- continues_days + 1
      
      if (is.null(statisRedData[1,continues_days])) {
        statisRedData[1,continues_days] <- 0
      }
      
      if (is.null(statisGreenData[1,continues_days])) {
        statisGreenData[1,continues_days] <- 0
      }
    } else {
      last_value <- statisRedData[1,continues_days]
      if (last_is_red>0) {
        statisRedData[1,continues_days] <- last_value + 1
      } else if (last_is_red<0) {
        statisGreenData[1,continues_days] <- last_value + 1
      }
      continues_days <- 1
    }
    
    last_is_red <- this_is_red
    i <- i+1
    
  }
  statisData <- list(statisRedData[1,],statisGreenData[1,],staticSameData)
  return(statisData)
}


