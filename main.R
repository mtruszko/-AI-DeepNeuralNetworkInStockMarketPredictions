library(rJava)
library(RMOA)
library(readr)
library(stringr)

source('Sequence.R')

#################################### INITIALIZATION ##########################################

trainingData <- read_delim("trainingData/trainingData.csv",
                           ";", escape_double = FALSE, trim_ws = TRUE)

funcTransformedTrainingData <- function(x) {
  x$Decision <- factor(x$Decision)
  x$SymbolID <- factor(x$SymbolID)
  x
}

companyData <- read_delim("trainingData/company_expert.csv", 
                          ";", escape_double = FALSE, trim_ws = TRUE)

funcTransformedCompanyData <- function(x) {
  x <- as.data.frame(x)
  x$CompanyID <- factor(x$CompanyID)
  x$ExpertID <- factor(x$ExpertID)
  x
}

companyData <- funcTransformedCompanyData(companyData)
factoredTrainingData <- funcTransformedTrainingData(trainingData)

################################# FUNCTIONS ##################################################

funcParseToFull <- function(tableWithRecommendarions) {
  #tableOfDecisions - Symbol;Recommendation;Decision
  
  dataFrameAllEvents <- data.frame()
  
  for(i in 1:nrow(tableWithRecommendarions)) {
    transaction <- tableWithRecommendarions[i,]
    
    #parse recommendation
    recommendarions <- transaction$Recommendations
    dataFrameTransactionEvents <- funcParseRecommendatins(recommendarions)
    
    #copy data and mark all recommendatins
    dataFrameTransactionEvents$Decision <- transaction$Decision
    dataFrameTransactionEvents$Decision <- factor(dataFrameTransactionEvents$Decision)
    dataFrameTransactionEvents$SymbolID <- transaction$SymbolID
    dataFrameTransactionEvents$SymbolID <- factor(dataFrameTransactionEvents$SymbolID)
    dataFrameTransactionEvents$TransactionID <- i
    
    #expand previously obtanined data
    dataFrameAllEvents <- rbind(dataFrameAllEvents, dataFrameTransactionEvents)
  }
  
  #merge with CompanyData
  dataFrameAllEvents <- merge(dataFrameAllEvents, companyData)
  
  return(dataFrameAllEvents)
  #retun - SymbolID;CompanyID;ExpertID;Prediction;PredictedValue;DaysBefore;Decision;TransactionID
}

funcParseRecommendatins <- function(stringRecommendations) {
  #stringRecommendations - string of recommendatins to parse
  
  stringWithoutFirstAndLast <- str_sub(stringRecommendations, 2, -2)
  listOfStringEvent <- strsplit(stringWithoutFirstAndLast, "\\}\\{") 
  
  vectorOfStringEvents <- unlist(listOfStringEvent)
  listOfSplitedEvent <- strsplit(vectorOfStringEvents, ",")
  
  matrixOfEvents <- matrix(unlist(listOfSplitedEvent), ncol=4, byrow=TRUE)
  dataFrameOfEvents <- as.data.frame(matrixOfEvents)
  colnames(dataFrameOfEvents) <- c("ExpertID", "Prediction", "PredictedValue", "DaysBefore")
  
  return(dataFrameOfEvents)
  #return data frame of events ExpertID;Prediction;PredictedValue;DaysBefore
}

funcFilterBy <- function(dataFrameOfEvents, symbolID, expertID, companyID) {
  filteredEvents <- dataFrameOfEvents
  if (hasArg(symbolID)) {
    filteredEvents <- filteredEvents[filteredEvents$SymbolID == symbolID,]
  }
  
  if (hasArg(companyID)) {
    filteredEvents <- filteredEvents[filteredEvents$CompanyID == companyID,]
  }
  
  if (hasArg(expertID)) {
    filteredEvents <- filteredEvents[filteredEvents$ExpertID == expertID,]
  }
  
  return(filteredEvents)
}

funcCreateSequenceOneByOne <- function(events,
                                       lenght, 
                                       filterType = "EXPERT") {
  maxTransaction <- max(events$TransactionID, na.rm = TRUE)
  
  sequences <- data.frame()
  
  for (t in 1:maxTransaction) {
    transactionEvents <- events[events$TransactionID == t,]
    
    possibilities <- 1
    
    switch(filterType,
           EXPERT = { 
             possibilities <- nlevels(transactionEvents$ExpertID)
           },
           COMPANY = {
             possibilities <- nlevels(transactionEvents$CompanyID)
           },
           {

           })
    
    for (e in 1:possibilities) {
      switch(filterType,
             EXPERT = { 
               transactionEvents <- transactionEvents[transactionEvents$ExpertID == transactionEvents$ExpertID[e],]
             },
             COMPANY = {
               transactionEvents <- transactionEvents[transactionEvents$CompanyID == transactionEvents$CompanyID[e],]
             },
             {
               
             })
      
      numberOfEvents <- nrow(transactionEvents)
      
      if (numberOfEvents < lenght) {
        next()
      }
      
      for (i in 1:numberOfEvents) {
        if (numberOfEvents - i < lenght) {
          break()
        }
        
        rowToReturn <- data.frame()
        switch(filterType,
               EXPERT = { 
                 rowToReturn <- transactionEvents[1,c("Decision", "SymbolID", "CompanyID", "ExpertID", "TransactionID")]
               },
               COMPANY = {
                 rowToReturn <- transactionEvents[1,c("Decision", "SymbolID","CompanyID", "TransactionID")]
               },
               {
                 rowToReturn <- transactionEvents[1,c("Decision", "SymbolID", "TransactionID")]
               })
        
        for (j in 1:lenght) {
          switch(filterType,
                 EXPERT = { 
                   
                 },
                 COMPANY = {
                   rowToReturn[paste("ExpertID", j, sep = "_")] = transactionEvents$ExpertID[i + j - 1]
                 },
                 {
                   rowToReturn[paste("ExpertID", j, sep = "_")] = transactionEvents$ExpertID[i + j - 1]
                   rowToReturn[paste("CompanyID", j, sep = "_")] = transactionEvents$CompanyID[i + j - 1]
                 })
          
          rowToReturn[paste("Prediction", j, sep = "_")] = transactionEvents$Prediction[i + j - 1]
          rowToReturn[paste("PredictedValue", j, sep = "_")] = transactionEvents$PredictedValue[i + j - 1]
          rowToReturn[paste("DaysBefore", j, sep = "_")] = transactionEvents$DaysBefore[i + j - 1]
          
          sequences <- rbind(sequences, rowToReturn)
        }
      }
    }
  }
  
  return(sequences)
}

#################################### MAIN ##############################################

#smaller subset for optiamlisation
filteredData <- trainingData[factoredTrainingData$SymbolID == "S110280",]

parsed <- funcParseToFull(tableWithRecommendarions = filteredData)

print(parsed)
#str(parsed)

#divide by ExpertID (optional); maybe divide by companyID then maybe withoutExpertID

filtered <- funcFilterBy(parsed, symbolID = "S110280", companyID = "21")

#print(filtered)

#numberOfRecommendationsInSequence - sequence window lenght
#selectFunc - function to specify how select attributes (oneByOne, startMiddleEnd)

#select appropriate number of attributes using selectFunc


seq <- funcCreateSequenceOneByOne(parsed, 1) 

print(seq)



#TODO: predictedValue as numeric
#TODO: add grouping function
