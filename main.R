library(rJava)
library(RMOA)
library(readr)
library(stringr)

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

funcChoseEventsOneByOne <- function(recommendationsEvents, lenght) {
  #recommendationsEvents - CompanyID(n);ExpertID(n);Prediction(n);PredictedValue(n);DaysBefore(n).......
  #lenght - numberOfSequences
  
  #for event in recommendationsEvents
  #for i in lenght
  #addToRow
  #startNewRow
  
  #return - recommendations divided to rows
}

#################################### MAIN ##############################################

exampleOfData <- trainingData[1:100,]
exampleOfData <- funcTransformedTrainingData(exampleOfData)

filteredData <- trainingData[trainingData$SymbolID == "S110280",]

parsed <- funcParseToFull(tableWithRecommendarions = filteredData)

print(parsed)
str(parsed)

#numberOfRecommendationsInSequence - sequence window lenght
#selectFunc - function to specify how select attributes (oneByOne, startMiddleEnd)


#divide by ExpertID (optional); maybe divide by companyID then maybe withoutExpertID

#select appropriate number of attributes using selectFunc

#create new table to return
#retun - Symbol;CompanyID(n);ExpertID(n);Prediction(n);PredictedValue(n);DaysBefore(n);...n>0...;Decision





#TODO: predictedValue as numeric
#TODO: add grouping function
