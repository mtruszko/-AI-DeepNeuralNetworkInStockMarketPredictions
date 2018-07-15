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

# funcNormalize <- function(dataX) {
#   #removing SymbolID
#   data <- subset(dataX, select = -2)
#   
#   #Decision to 0 0 1 and remove
#   
#   decisionCol <- subset(data, select = 1)
#   data <- subset(data, select = -1)
#   
#   data$Buy <- apply(decisionCol, 2, funcCompare, "Buy")
#   data$Sell <- apply(decisionCol, 2, funcCompare, "Sell")
#   data$Hold <- apply(decisionCol, 2, funcCompare, "Hold")
#   
#   #as numeric
#   
#   data <- as.data.frame(sapply(data, as.numeric))
#   
#   str(data)
#   
#   #mormalize
#   
#   mean <- apply(data, 2, mean)
#   std <- apply(data, 2, sd)
#   train_data <- scale(data, center = mean, scale = std)
#   #test_data <- scale(test_data, center = mean, scale = std)
#   
#   train_data[is.na(train_data)]<-0
#   
#   return(train_data)
# }

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

#################################### ML ################################################

# library("neuralnet")
# 
# funcTrain <- function(scaledData) {
#   data <- funcNormalize(scaledData)
#   net <- neuralnet(Buy+Sell+Hold~Buys_1+Sells_1+Holds_1+PredictedMin_1+PredictedMax_1+PredictedMean_1, data, hidden=c(10,10,10), linear.output=FALSE)
#   #print(net)
#   #plot(net)
#   
#   return(net)
# }


#devtools::install_github("rstudio/keras")
library(keras)
#install_keras()
#install_keras(tensorflow = "gpu")

# funcTrainKeras <- function(trainData, testData) {
#   trainData <- funcNormalize(trainData)
#   testData <- funcNormalize(testData)
#   
#   model <- keras_model_sequential() 
#   model %>% 
#     layer_dense(units = 6, activation = 'relu', input_shape = c(6)) %>% 
#     layer_dropout(rate = 0.4) %>% 
#     layer_dense(units = 10, activation = 'relu') %>%
#     layer_dropout(rate = 0.3) %>%
#     layer_dense(units = 10, activation = 'softmax')
#   
#   summary(model)
#   
#   model %>% compile(
#     loss = 'categorical_crossentropy',
#     optimizer = optimizer_rmsprop(),
#     metrics = c('accuracy')
#   )
#   
#   history <- model %>% fit(
#     trainData, testData, 
#     epochs = 30, batch_size = 128, 
#     validation_split = 0.2
#   )
#   
#   plot(history)
# }

###################################### TEST NET ########################################

# funcTestModel <- function(model, testData) {
#   data <- funcNormalize(testData)
#   testModelData <- data[, 1:6]
#   net.results <- compute(model, testModelData)
#   
#   ls(net.results)
#   
#   print(net.results$net.result)
#   
#   roundedResults <- round(net.results$net.result, 0)
#   
#   print(roundedResults)
#   
#   #Lets display a better version of the results
#   cleanoutput <- cbind(testModelData[,1:3], data[,7:9],
#                        #as.data.frame(net.results$net.result))
#                        as.data.frame(roundedResults))
#   colnames(cleanoutput) <- c("IN_BUYS", "IN_SELLS", "IN_HOLDS", "EXP_BUYS", "EXP_SELLS", "EXP_HOLDS", "OUT_BUYS", "OUT_SELLS", "OUT_HOLDS")
#   print(cleanoutput)
#   
#   #a <- table(testModelData[,1:3], roundedResults)
#   #print(a)
# }

#################################### WRITE ###########################################

writeAsCSV <- function(data) {
  write.csv(data, file = "TrainingSet.csv")
}


#################################### KERAS ############################################

# func_MyMetric <- custom_metric("my_metric", function(y_true, y_pred) {
#   k_mean(y_pred)
# })

# func_to_one_hot <- function(labels, dimension = 46) {
#   results <- matrix(0, nrow = length(labels), ncol = dimension)
#   for (i in 1:length(labels))
#     results[i, labels[[i]] + 1] <- 1
#   results
# }

funcGetSimpleModel <- function(k = 4) {
  model <- keras_model_sequential() %>%
    layer_dense(units = 64, activation = "relu", input_shape = c(k*6)) %>%
    layer_dropout(rate = 0.5) %>% 
    layer_dense(units = 64, activation = "relu") %>%
    layer_dropout(rate = 0.5) %>% 
    layer_dense(units = 3, activation = "softmax")
  
  model %>% compile(
    optimizer = "rmsprop",
    loss = "categorical_crossentropy",
    metrics = c('accuracy')
  )
  
  model
}

funcNormalizedAndLabels <- function(train_data, k = 4) {
  #removing SymbolID
  data <- subset(train_data, select = -2)
  
  #Decision to 0 0 1 and remove
  
  decisionCol <- subset(data, select = 1)
  data <- subset(data, select = -1)
  
  data$Buy <- apply(decisionCol, 2, funcCompare, "Buy")
  data$Sell <- apply(decisionCol, 2, funcCompare, "Sell")
  data$Hold <- apply(decisionCol, 2, funcCompare, "Hold")
  
  #as numeric
  
  data <- as.data.frame(sapply(data, as.numeric))
  
  str(data)
  
  #mormalize
  
  data_to_normalize <- data[,1:(k*6)]
  one_hot_train_labels <- data[, ((k*6)+1):((k*6)+3)]
  
  mean <- apply(data_to_normalize, 2, mean)
  std <- apply(data_to_normalize, 2, sd)
  x_train <- scale(data_to_normalize, center = mean, scale = std)
  #test_data <- scale(test_data, center = mean, scale = std)
  
  x_train[is.na(x_train)]<-0
  one_hot_train_labels[is.na(one_hot_train_labels)]<-0
  
  list(input = x_train, output = one_hot_train_labels)
}

funcTrain <- function(trainData, k = 4) {
  
  normalized <- funcNormalizedAndLabels(trainData, k)
  
  x_train <- normalized$input
  one_hot_train_labels <- normalized$output
  
  # val_indices <- 1:3000
  # x_val <- x_train[val_indices,]
  # partial_x_train <- x_train[-val_indices,]
  # 
  # y_val <- one_hot_train_labels[val_indices,]
  # partial_y_train = one_hot_train_labels[-val_indices,]
  # 
  model <- funcGetSimpleModel(k)
  
  history <- model %>% fit(
    x_train,
    one_hot_train_labels,
    epochs = 20,
    batch_size = 64,
    #validation_data = list(x_val, y_val)
    validation_split = 0.2
  )
  
  str(history)
  plot(history)
}

# funcEvaluateModel <- function() {
#   results <- model %>% evaluate(x_test, one_hot_test_labels)
# }
# 

#################################### MAIN ##############################################

#smaller subset for optiamlisation
# filteredData <- trainingData[trainingData$SymbolID == "S591675",]

#chunkOfTrainingRecords <- trainingData[1]

TrainingSet <- read_csv("TrainingSet.csv")

# parsed <- funcParseToFull(tableWithRecommendarions = filteredData)

# print(parsed)

# seq <- funcCreateSequenceOneByOne(TrainingSet, 1, filterType = "")

# print(seq)

k <- 1

# one <- funcTransactionPartStatistic(table = TrainingSet, k)

str(one)


funcTrain(one, k)


# 
# #netModel <- funcTrain(one)
# 
# 
# 
# filteredTestData <- trainingData[trainingData$SymbolID == "S110280",]
# parsedTest <- funcParseToFull(tableWithRecommendarions = filteredTestData)
# seqTest <- funcCreateSequenceOneByOne(parsedTest, 1, filterType = "")
# oneTest <- funcTransactionPartStatistic(table = seqTest, 1)
# 
# print(oneTest)
# 
# #funcTestModel(netModel, oneTest)
# 
# funcTrainKeras(one,oneTest)
# 
# 



######################################### END ############################################
