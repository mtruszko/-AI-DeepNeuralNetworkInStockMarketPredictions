#MT
k <- 20
# TrainingSet <- read_csv("TrainingSet.csv")
# trainDataSeqStat <- funcTransactionPartStatistic(table = TrainingSet, k)
# str(trainDataSeqStat)


model <- funcTrainRNN(trainDataSeqStat)


# TestSet <- read_csv("TestSet.csv")
# testDataSeqStat <- funcTransactionPartStatistic(table = TestSet, k)
# str(testDataSeqStat)
pred <- funcEvaluateRNN(testDataSeqStat)

cm <- funcConfusionMatrix(pred, one_hot_test_labels)
weights <- rbind(c(8,4,8), c(1, 1, 1), c(8, 4, 8))

acc <- funcCalculateACC(cm, weights)