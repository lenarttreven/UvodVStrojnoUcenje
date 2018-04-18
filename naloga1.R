#najprej naložimo podatke in jih uredimo v eno samo tabelo
library(caret)
library(rpart.plot)

data <- read.csv('ucni_podatki.csv', header=FALSE)
dataX <- data[names(data) != 'V1']

dataClass <- read.csv('ucni_razredi.csv', header = FALSE)
dataY <- dataClass[order(dataClass$V1),]
rownames(dataY) <- NULL
dataY <- dataY[names(dataY) != 'V1']

myData <- data.frame(X1=dataX$V2, X2=dataX$V3, X3=dataX$V4, X4=dataX$V5, X5=dataX$V6, X6=dataX$V7, Y = dataY$V2)

#podatke prečistimo, kaj narediti z manjkajočimi vrednostmi?
#TODO

lapply(myData, typeof)

myData$X1 <- as.numeric(as.character(myData$X1))
myData$X3 <- as.numeric(as.character(myData$X3))

myData <- na.omit(myData)
myData$Y <- droplevels(myData$Y)
rownames(myData) <- NULL

levels(myData$Y)
#sedaj natreniramo model


calculateFBetaMeasure <- function(data, levels, ...){
  # levels[1] = TRUE
  # levels[2] = FALSE
  # napaka tipa 1 = "false positive"
  # moramo paziti kateri level je + in kateri -
  beta <- 2
  
  truePositives <- sum(data$pred==data$obs & data$pred==levels[1])
  falsePositive <- sum(data$pred==levels[1] & data$obs == levels[2])
  actualPositive <- sum(data$obs==levels[1])
  
  ppv <- truePositives / (truePositives + falsePositive)
  tpr <- truePositives / actualPositive
  
  accuracy <- (1 + beta^2)* (ppv*tpr)/(beta^2 * ppv + tpr)
  names(accuracy) <- 'accuracy'
  accuracy
}



cvtc <- trainControl(method='cv', index = createFolds(myData$Y, k=10, returnTrain=TRUE), summaryFunction = calculateFBetaMeasure)
tuneGrd <- data.frame(k=c(1:300));

knnModel<-train(Y~., data=myData, method='knn', metric = 'accuracy',trControl=cvtc, tuneGrid=tuneGrd)


linearModel <- train(Y~., data=myData, metric = 'accuracy', method='glm', trControl=cvtc)

#vidimo, da je linearni Model boljši od knnModela, zato bomo izbrali linearni model

errors <- data.frame(deg=1:6, accuracy=rep(-1,6), accuracySD=rep(-1,6), testerr=rep(-1,6))
for(k in 1:4){
  # Za podrobnosti kako dobimo ta klic poglejte vaja_3_1-modeli visjega reda.R
  tmpData <- data.frame(poly(as.matrix(myData[, 1:6]), degree=k, raw=TRUE), Y=myData$Y)
  tmpModel <- train(Y~., data=tmpData, method='glm', trControl=cvtc)
  # Shranimo izračunane napake
  errors$accuracy[k] <- tmpModel$results$cost
  errors$accuracySD[k] <- tmpModel$results$costSD
  # Za izračun napake na učni množici moramo izračunati napovedi
  tmpPredictions <- predict(tmpModel, newData=tmpData)
  # namesto ročnega klica ki izračuna RMSE si pomagamo z vgrajeno caret funkcijo postResample:
  errors$testerr[k] <- postResample(tmpPredictions, myData$Y)['Accuracy']
}

#vidimo, da so za k = 2 najboljši rezultati

myDataX <- myData[names(myData)!='Y']
myDataY <- myData$Y
myModelrpartLike <- train(myDataX, myDataY,
                          method='rpart',
                          metric= 'cost',
                          trControl=cvtc,
                          tuneGrid = data.frame(cp=0),
                          control=rpart.control(maxdepth = 5, minsplit=1, xval=0))





rpart.plot(myModelrpartLike$finalModel)


#sedaj testiramo naucen model na testnih podatkih

testData <- read.csv('testni_podatki.csv', header=FALSE)
View(testData)

testData <- testData[names(testData) != 'V1']

testData <- data.frame(X1=testData$V2, X2=testData$V3, X3=testData$V4, X4=testData$V5, X5=testData$V6, X6=testData$V7)
testData$X1 <- as.numeric(as.character(testData$X1))
testData$X1[20] = 8.38371346378874 * 10^-5
lapply(testData, typeof)
testData <- na.omit(testData)
rownames(testData) <- NULL
#testData <- testData[1:500, ]


testPredictions <- predict(linearModel, testData)
#View(testPredictions)
#table(testPredictions)


#write to csv
indexColumn = 1001:2000

dataOut = data.frame(X1 = indexColumn, X2 = testPredictions)

write.table(dataOut, file = "rezultati.csv",row.names = FALSE, col.names = FALSE, sep = ',', quote = FALSE)













