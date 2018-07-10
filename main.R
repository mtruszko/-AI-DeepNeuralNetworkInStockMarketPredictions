library(rJava)
library(RMOA)
library(readr)
library(stringr)

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
    
    print(i)
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
  
  dataFrameOfEvents$PredictedValue <- as.numeric(as.character(dataFrameOfEvents$PredictedValue))
  
  return(dataFrameOfEvents)
  #return data frame of events ExpertID;Prediction;PredictedValue;DaysBefore
}

funcCreateSequenceOneByOne <- function(events,
                                       lenght, 
                                       filterType = "EXPERT") {
  maxTransaction <- max(events$TransactionID, na.rm = TRUE)
  
  sequences <- data.frame()
  
  for (t in 1:maxTransaction) {
    print(t)
    transactionEvents <- events[events$TransactionID == t,]
    
    possibilities <- 1
    
    switch(filterType,
           EXPERT = { 
             uniqueEpertsIDs = unique(transactionEvents$ExpertID)
             possibilities <- length(uniqueEpertsIDs)
           },
           COMPANY = {
             uniqueCompanyIDs <- unique(transactionEvents$CompanyID)
             possibilities <- length(uniqueCompanyIDs)
           },
           {

           })
    
    for (e in 1:possibilities) {
      switch(filterType,
             EXPERT = { 
               filteredTransactionEvents <- transactionEvents[transactionEvents$ExpertID == uniqueEpertsIDs[e],]
             },
             COMPANY = {
               filteredTransactionEvents <- transactionEvents[transactionEvents$CompanyID == uniqueCompanyIDs[e],]
             },
             {
               filteredTransactionEvents <- transactionEvents
             })
      
      numberOfEvents <- nrow(filteredTransactionEvents)
      
      if (numberOfEvents < lenght) {
        next()
      }
      
      for (i in 1:numberOfEvents) {
        if (numberOfEvents - i < lenght - 1) {
          break()
        }
        
        rowToReturn <- data.frame()
        switch(filterType,
               EXPERT = { 
                 rowToReturn <- filteredTransactionEvents[1,c("Decision", "SymbolID", "CompanyID", "ExpertID", "TransactionID")]
               },
               COMPANY = {
                 rowToReturn <- filteredTransactionEvents[1,c("Decision", "SymbolID","CompanyID", "TransactionID")]
               },
               {
                 rowToReturn <- filteredTransactionEvents[1,c("Decision", "SymbolID", "TransactionID")]
               })
        
        for (j in 1:lenght) {
          switch(filterType,
                 EXPERT = { 
                   
                 },
                 COMPANY = {
                   rowToReturn[paste("ExpertID", j, sep = "_")] = filteredTransactionEvents$ExpertID[i + j - 1]
                 },
                 {
                   rowToReturn[paste("ExpertID", j, sep = "_")] = filteredTransactionEvents$ExpertID[i + j - 1]
                   rowToReturn[paste("CompanyID", j, sep = "_")] = filteredTransactionEvents$CompanyID[i + j - 1]
                 })
          
          rowToReturn[paste("Prediction", j, sep = "_")] = filteredTransactionEvents$Prediction[i + j - 1]
          rowToReturn[paste("PredictedValue", j, sep = "_")] = filteredTransactionEvents$PredictedValue[i + j - 1]
          rowToReturn[paste("DaysBefore", j, sep = "_")] = filteredTransactionEvents$DaysBefore[i + j - 1]
          
        }
        sequences <- rbind(sequences, rowToReturn)
      }
    }
  }
  
  return(sequences)
}

#################################### TO ONE ROW ########################################

funcTransactionPartStatistic <- function(table,
                                         numberOfParts = 4) {
  tableWithPartedTransactions <- data.frame()
  
  numberOfTransactions <- 21
  for (ti in 1:numberOfTransactions) {
    oneTransactionTable <- table[table$TransactionID == ti,]
    oneRowTransaction <- funcTransactionToOneRow(transactionTable = oneTransactionTable, numberOfParts = numberOfParts)
    tableWithPartedTransactions <- rbind(tableWithPartedTransactions, oneRowTransaction)
  }
  
  #clean data
  is.na(tableWithPartedTransactions)<-sapply(tableWithPartedTransactions, is.infinite)
  tableWithPartedTransactions[is.na(tableWithPartedTransactions)]<-0
  
  return(tableWithPartedTransactions)
}

funcTransactionToOneRow <- function(transactionTable, numberOfParts) {
  
  rowToReturn <- data.frame(matrix(ncol=0, nrow=0))
  rowToReturn <- transactionTable[1,c("Decision", "SymbolID")]
  
  numberOfAllDays <- 60
    
  for (i in 1:numberOfParts) {
    interval = abs(numberOfAllDays / numberOfParts)
    
    fromDay <- interval * (i - 1)
    toDay <- interval * i - 1
    
    selectedTable <- transactionTable[transactionTable$DaysBefore_1 %in% c(fromDay:toDay),]
    
    onlyPrediction <- selectedTable[,c("Prediction_1", "PredictedValue_1")]
    
    dsds <- funcStaristicForPart(transactionTable = onlyPrediction, part = i)
    
    rowToReturn <- merge(rowToReturn, dsds)
  }
  
  return(rowToReturn)
}

funcStaristicForPart <- function(transactionTable, part = 1) {
  rowToReturn = data.frame(matrix(ncol=0, nrow=1))
  
  rowToReturn[paste("Buys", part, sep = "_")] = 0
  rowToReturn[paste("Sells", part, sep = "_")] = 0
  rowToReturn[paste("Holds", part, sep = "_")] = 0
  rowToReturn[paste("PredictedMin", part, sep = "_")] = 0
  rowToReturn[paste("PredictedMax", part, sep = "_")] = 0
  rowToReturn[paste("PredictedMean", part, sep = "_")] = 0
  
  tableTransitionTable <- as.data.frame(table(unlist(transactionTable[, 1])))
  
  rowToReturn[paste("Buys", part, sep = "_")] = tableTransitionTable[tableTransitionTable$Var1 == "Buy", "Freq"]
  rowToReturn[paste("Sells", part, sep = "_")] = tableTransitionTable[tableTransitionTable$Var1 == "Sell", "Freq"]
  rowToReturn[paste("Holds", part, sep = "_")] = tableTransitionTable[tableTransitionTable$Var1 == "Hold", "Freq"]
  rowToReturn[paste("PredictedMin", part, sep = "_")] = min(transactionTable[,2], na.rm = TRUE)
  rowToReturn[paste("PredictedMax", part, sep = "_")] = max(transactionTable[,2], na.rm = TRUE)
  rowToReturn[paste("PredictedMean", part, sep = "_")] = mean(transactionTable[,2], na.rm = TRUE)
  
  return(rowToReturn)
}

#################################### MAIN ##############################################

#smaller subset for optiamlisation
filteredData <- trainingData[trainingData$SymbolID == "S110280",]

#chunkOfTrainingRecords <- trainingData[1]

parsed <- funcParseToFull(tableWithRecommendarions = filteredData)

print(parsed)

seq <- funcCreateSequenceOneByOne(parsed, 1, filterType = "") 

print(seq)

one <- funcTransactionPartStatistic(table = seq, 4)

print(one)












######################################### END ############################################
