
seq <- 15
globalModel <- NaN
columnInSequence <- 6

funcPrepareForModel <- function(paramData, isTrain) {
  #removing SymbolID
  data <- subset(paramData, select = -2)
  
  #Decision to 0 0 1 and remove
  one_hot_train_labels <- model.matrix(~data$Decision-1)
  data <- subset(data, select = -1)
  
  #as numeric
  data <- as.data.frame(sapply(data, as.numeric))
  
  #mormalize
  data_to_normalize <- data[,1:(seq*6)]
  if (isTRUE(isTrain)) {
    trainMean <<- apply(data_to_normalize, 2, mean)
    trainStd <<- apply(data_to_normalize, 2, sd)
  }
  x_train <- scale(data_to_normalize, center = trainMean, scale = trainStd)
  x_train[is.na(x_train)]<-0
  
  #3 destination sequence
  # x_train <- pad_sequences(x_train, maxlen = columnInSequence)
  # cat("input_train shape:", dim(x_train), "\n")
  
  simpleTrain <- x_train #head(x_train, 3)
  
  d3_train <- array(simpleTrain, dim = c(nrow(simpleTrain), columnInSequence, seq))
  d3_train <- aperm(d3_train, c(1,3,2))
  
  list(input = d3_train, output = one_hot_train_labels)
}

funcRNNModel <- function() {
  model <- keras_model_sequential() %>%
    layer_lstm(units = 256, input_shape = c(seq, columnInSequence)) %>%
    layer_lstm(units = 256) %>%
    # layer_lstm(units = 256) %>%
    # layer_lstm(units = 256) %>%
    # layer_lstm(units = 256) %>%
    layer_dense(units = 3, activation = "softmax")
  
  model %>% compile(
    optimizer = "rmsprop",
    loss = "categorical_crossentropy",
    metrics = c('categorical_accuracy')
  )
  globalModel <<- model
  model
}

funcRNNModel()

funcTrainRNN <- function(trainData) {
  
  normalized <- funcPrepareForModel(trainData, TRUE)
  
  x_train <- normalized$input
  one_hot_train_labels <- normalized$output
  
  history <- globalModel %>% fit(
    x_train,
    one_hot_train_labels,
    epochs = 100,
    batch_size = 256
  )
  
  str(history)
  plot(history)
}

funcEvaluateRNN <- function(testData) {
  normalized <- funcPrepareForModel(testData, FALSE)
  
  x_test <- normalized$input
  one_hot_test_labels <<- normalized$output
  
  results <- globalModel %>% evaluate(x_test, one_hot_test_labels)
  
  str(results)
  
  return(globalModel %>% predict(x_test, batch_size = 128))
}

