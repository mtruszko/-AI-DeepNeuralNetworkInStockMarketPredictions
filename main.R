library(rJava)
library(RMOA)
library(readr)

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

exampleOfData <- trainingData[1:100,]
exampleOfData <- funcTransformedTrainingData(exampleOfData)

companyData <- funcTransformedCompanyData(companyData)

filteredData <- trainingData[trainingData$SymbolID == "S110280",]

funcParseToSequences <- function(tableOfDecisions, numberOfRecommendationsInSequence, selectFunc) {
  #tableOfDecisions - Symbol;Recommendation;Decision
  #numberOfRecommendationsInSequence - sequence window lenght
  #selectFunc - function to specify how select attributes (oneByOne, startMiddleEnd)
  x <- data.frame()
  #for each record in tableOfDecisions
  #TODO: change for-in to applay
  for(i in 1:nrow(tableOfDecisions)) {
    #parse recommendation
    recommendarions <- tableOfDecisions[i,]$Recommendations
    a <- funcParseRecommendatins(recommendarions)
    
    #copy data and mark all recommendatins
    a$Decision <- tableOfDecisions[i,]$Decision
    a$Decision <- factor(a$Decision)
    a$SymbolID <- tableOfDecisions[i,]$SymbolID
    a$SymbolID <- factor(a$SymbolID)
    a$TransactionID <- i
    
    #expand previously obtanined data
    x <- rbind(x, a)
  }
  
  #merge with CompanyID
  x <- merge(x, companyData)
  
  print(x)
  str(x)
  
  #divide by ExpertID (optional); maybe divide by companyID then maybe withoutExpertID
  
  #select appropriate number of attributes using selectFunc
  
  #create new table to return
  
  #retun - Symbol;CompanyID(n);ExpertID(n);Prediction(n);PredictedValue(n);DaysBefore(n);...n>0...;Decision
}

library(stringr)
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

parsed <- funcParseToSequences(tableOfDecisions = filteredData)
