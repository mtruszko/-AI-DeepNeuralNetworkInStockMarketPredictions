library(rJava)
library(RMOA)

library(readr)
#library(data.table) 

trainingData <- read_delim("trainingData/trainingData.csv",
";", escape_double = FALSE, trim_ws = TRUE)
View(trainingData)

funcTransformedTrainingData <- function(x) {
  x$Decision <- factor(x$Decision)
  x$SymbolID <- factor(x$SymbolID)
  #x$Recommendations <- list(x$Recommendations)
  x
}

exampleOfData <- trainingData[1:100,]
exampleOfData <- funcTransformedTrainingData(exampleOfData)
str(exampleOfData)

filteredData <- trainingData[trainingData$SymbolID == "S110280",]
str(filteredData)

funcParseToSequences <- function(tableOfDecisions, numberOfRecommendationsInSequence, selectFunc) {
  #tableOfDecisions - Symbol;Recommendation;Decision
  #numberOfRecommendationsInSequence - sequence window lenght
  #selectFunc - function to specify how select attributes (oneByOne, startMiddleEnd)
  
  #TODO: change for-in to applay
  for(i in 1:nrow(tableOfDecisions)) {
    recommendarions <- tableOfDecisions[i,]$Recommendations
    a <- funcParseRecommendatins(recommendarions)
    print(recommendarions)
    print(a)
  }
  
  #for each record in tableOfDecisions
    #parse recommendation 
    #divide by ExpertID (optional); maybe divide by companyID then maybe withoutExpertID
  #select appropriate number of attributes using selectFunc
  #add CompanyID
  #create new table to return
  
  #retun - Symbol;CompanyID(n);ExpertID(n);Prediction(n);PredictedValue(n);DaysBefore(n);...n>0...;Decision
}

library(stringr)
funcParseRecommendatins <- function(stringRecommendations) {
  #stringRecommendations - string of recommendatins to parse
  
  stringWithoutFirstAndLast <- str_sub(stringRecommendations, 2, -2)
  listOfStringEvent <- strsplit(stringWithoutFirstAndLast, "\\}\\{") 
  
  vectorOfStringEvents <- unlist(listOfStringEvent)
  listOfSplitedEvent <- strsplit(vectorOfStringsEvents, ",")
  
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
