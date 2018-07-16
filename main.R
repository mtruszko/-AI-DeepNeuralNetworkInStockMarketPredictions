library(rJava)
library(RMOA)
library(readr)
library(stringr)

#devtools::install_github("rstudio/keras")
library(keras)


#################################### INITIALIZATION ##########################################

trainingData <- read_delim("trainingData/trainingData.csv",
                           ";", escape_double = FALSE, trim_ws = TRUE)
funcTransformedTrainingData <- function(x) {
  x$Decision <- factor(x$Decision)
 
  x
}
trainingData <- funcTransformedTrainingData(trainingData)


companyData <- read_delim("trainingData/company_expert.csv", 
                          ";", escape_double = FALSE, trim_ws = TRUE)
funcTransformedCompanyData <- function(x) {
  x <- as.data.frame(x)
  x$CompanyID <- factor(x$CompanyID)
  x$ExpertID <- factor(x$ExpertID)
  x
}
companyData <- funcTransformedCompanyData(companyData)



testDataCSV <- read_delim("testData/testData.csv",
                           ";", escape_double = FALSE, trim_ws = TRUE)
funcTransformedTestData <- function(x) {
  x$SymbolID <- factor(x$SymbolID)
  x
}
testDataCSV <- funcTransformedTestData(testDataCSV)

testLabelsCSV <- read_delim("testData/true_test_classes.csv",
                          ";", escape_double = FALSE, trim_ws = TRUE, col_names = FALSE)
funcTransformedTestLabels <- function(x) {
  x$Decision <- factor(x$Decision)
  x
}
colnames(testLabelsCSV) <- c("Decision")
testLabelsCSV <- funcTransformedTestLabels(testLabelsCSV)

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
  
  numberOfTransactions <- max(table$TransactionID, na.rm = TRUE)
  for (ti in 1:numberOfTransactions) {
    print(ti)
    oneTransactionTable <- table[table$TransactionID == ti,]
    oneRowTransaction <- funcTransactionToOneRow(transactionTable = oneTransactionTable, numberOfParts = numberOfParts)
    tableWithPartedTransactions <- rbind(tableWithPartedTransactions, oneRowTransaction)
  }
  
  is.na(tableWithPartedTransactions)<-sapply(tableWithPartedTransactions, is.infinite)
  tableWithPartedTransactions[is.na(tableWithPartedTransactions)]<-0
  
  return(tableWithPartedTransactions)
}

funcTransactionToOneRow <- function(transactionTable, numberOfParts) {
  
  rowToReturn <- data.frame(matrix(ncol=0, nrow=0))
  rowToReturn <- transactionTable[1,c("Decision", "SymbolID")]
  
  numberOfAllDays <- 67 - 1
    
  for (i in 1:numberOfParts) {
    interval = floor(numberOfAllDays / numberOfParts)
    
    fromDay <- interval * (i - 1)
    toDay <- interval * i - 1
    
    selectedTable <- transactionTable[transactionTable$DaysBefore %in% c(fromDay:toDay),]
    
    onlyPrediction <- selectedTable[,c("Prediction", "PredictedValue")]
    
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
  
  if (nrow(transactionTable) == 0) {
    return(rowToReturn)
  }
  
  tableTransitionTable <- data.frame(matrix(ncol=0, nrow=1))
  tableTransitionTable <- as.data.frame(table(unlist(transactionTable[, 1])))
  
  rowToReturn[paste("Buys", part, sep = "_")] = tableTransitionTable[tableTransitionTable$Var1 == "Buy", "Freq"]
  rowToReturn[paste("Sells", part, sep = "_")] = tableTransitionTable[tableTransitionTable$Var1 == "Sell", "Freq"]
  rowToReturn[paste("Holds", part, sep = "_")] = tableTransitionTable[tableTransitionTable$Var1 == "Hold", "Freq"]
  rowToReturn[paste("PredictedMin", part, sep = "_")] = min(transactionTable[,2], na.rm = TRUE)
  rowToReturn[paste("PredictedMax", part, sep = "_")] = max(transactionTable[,2], na.rm = TRUE)
  rowToReturn[paste("PredictedMean", part, sep = "_")] = mean(transactionTable[["PredictedValue"]], na.rm = TRUE)
  
  return(rowToReturn)
}

############################################# NORMALIZE ##############################

# funcMins <- function(data) {
#   data[is.na(data)]<-0
#   mins <- apply(data, 2, min)
#   mins
# }
# 
# funcMaxs <- function(data) {
#   data[is.na(data)]<-0
#   maxs <- apply(data, 2, max)
#   maxs
# }

funcCompare <- function(x, rhs) {
  x[x == rhs] <- 1 
  x[x != 1] <- 0
  x
}

#################################### WRITE ###########################################

writeAsCSV <- function(data) {
  write.csv(data, file = "TrainingSet.csv")
}

#################################### KERAS ############################################

# func_MyMetric <- custom_metric("my_metric", function(y_true, y_pred) {
#   k_mean(y_pred)
# })

funcGetSimpleModel <- function(k = 4) {
  model <- keras_model_sequential() %>%
    layer_dense(units = 256, activation = "relu", input_shape = c(k*6)) %>%
    layer_dense(units = 256, activation = "relu") %>%
    layer_dropout(rate = 0.05) %>%
    layer_dense(units = 256, activation = "relu") %>%
    layer_dense(units = 256, activation = "relu") %>%
    layer_dropout(rate = 0.05) %>%
    layer_dense(units = 256, activation = "relu") %>%
    layer_dense(units = 256, activation = "relu") %>%
    layer_dropout(rate = 0.05) %>%
    layer_dense(units = 256, activation = "relu") %>%
    layer_dense(units = 256, activation = "relu") %>%
    layer_dropout(rate = 0.05) %>%
    layer_dense(units = 256, activation = "relu") %>%
    layer_dense(units = 3, activation = "softmax")
  
  model %>% compile(
    optimizer = "rmsprop",
    loss = "categorical_crossentropy",
    metrics = c('categorical_accuracy')
  )
  
  model
}

funcNormalizedAndLabels <- function(data, k = 4, isTrain) {
  #removing SymbolID
  data <- subset(data, select = -2)
  
  #Decision to 0 0 1 and remove
  one_hot_train_labels <- model.matrix(~data$Decision-1)
  data <- subset(data, select = -1)
  
  #as numeric
  data <- as.data.frame(sapply(data, as.numeric))
  
  #mormalize
  data_to_normalize <- data[,1:(k*6)]
  if (isTRUE(isTrain)) {
    trainMean <<- apply(data_to_normalize, 2, mean)
    trainStd <<- apply(data_to_normalize, 2, sd)
  }
  x_train <- scale(data_to_normalize, center = trainMean, scale = trainStd)
  x_train[is.na(x_train)]<-0
  
  list(input = x_train, output = one_hot_train_labels)
}

funcTrain <- function(trainData, k = 4) {
  
  normalized <- funcNormalizedAndLabels(trainData, k, TRUE)
  
  x_train <- normalized$input
  one_hot_train_labels <- normalized$output
  
  model <- funcGetSimpleModel(k)
  
  history <- model %>% fit(
    x_train,
    one_hot_train_labels,
    epochs = 500,
    batch_size = 512
    # validation_split = 0.2
  )
  
  str(history)
  plot(history)
  
  model
}

funcEvaluateModel <- function(model, testData, k) {
  normalized <- funcNormalizedAndLabels(testData, k, FALSE)
  
  x_test <- normalized$input
  one_hot_test_labels <- normalized$output
  
  results <- model %>% evaluate(x_test, one_hot_test_labels)
  
  str(results)
}


#################################### MAIN ##############################################

k <- 15

# trainingDataParsed <- funcParseToFull(tableWithRecommendarions = trainingData)
# write.csv(trainingDataParsed, file = "TrainingSet.csv")
# TrainingSet <- read_csv("TrainingSet.csv")
# testDataSeqStat <- funcTransactionPartStatistic(table = TrainingSet, k)
str(testDataSeqStat)
model <- funcTrain(testDataSeqStat, k)


# testDataRaw <- cbind(testDataCSV, testLabelsCSV)
# testDataParsed <- funcParseToFull(tableWithRecommendarions = testDataRaw)
# write.csv(testDataParsed, file = "TestSet.csv")
# TestSet <- read_csv("TestSet.csv")
# testDataSeqStat <- funcTransactionPartStatistic(table = TestSet, k)
str(testDataSeqStat)
funcEvaluateModel(model, testDataSeqStat, k)


######################################### END ############################################
