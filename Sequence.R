funcCreateSequenceOneByOne <- function(events, lenght) {
  
  maxTransaction <- max(events$TransactionID, na.rm = TRUE)
  
  for (t in maxTransaction) {
    transactionEvents <- events[events$TransactionID == t,]
    
    print(transactionEvents)
  }
}

funcChoseEventsOneByOne <- function(events, lenght) {
  #lenght - numberOfSequences
  
  #for event in recommendationsEvents
  #for i in lenght
  #addToRow
  #startNewRow
  
  
  
  #return - recommendations divided to rows
}