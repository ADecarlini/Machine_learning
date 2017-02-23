## separo en conjuntos

finaltestdata <- read.csv("testdata.csv")
inputdata <- read.csv("trainingdata.csv")
set.seed(21073)

fortraindataindex <- createDataPartition(y=inputdata$classe, p=0.70, list=FALSE)
traindata <- inputdata[fortraindataindex,]
testdata <- inputdata[-fortraindataindex,]
rm(inputdata)


'which(names(finaltestdata)=="user_name")
which(names(finaltestdata)=="cvtd_timestamp")
which(names(traindata)=="user_name")
which(names(traindata)=="cvtd_timestamp")
levels(finaltestdata[,2])
levels(traindata[,2])
levels(finaltestdata[,5]) <- levels(traindata[,5])'

## preproceso para el traindata

  'Knn impute'
  
preprocesobj <- preProcess(traindata, method="knnImpute")
traindata <- predict(preprocesobj,traindata)

 'quito zero variance'

zerovar <- nearZeroVar(traindata,saveMetrics = FALSE)
traindata <- traindata[,-zerovar]

  ##'quito columnas con clasificadores (factores) ## columnas 2 user_name,5 cvtd_timestamp'

      ##traindata <- traindata[,-c(2,5)]  

## ajusto modelo

modelfit <- randomForest(classe~.,data=traindata, ntree=300)

## aplico mismo preproceso al test data

testdata <- predict(preprocesobj,testdata)
testdata <- testdata[,-zerovar]
      ##testdata <- testdata[,-c(2,5)]


## test

predictedoutput <- predict(modelfit,newdata=testdata)
cm <- confusionMatrix(predictedoutput,testdata$classe)


print(cm$table)
print(cm$overall[1])


# con datos finales:
##preproceso

finaltestdata <- predict(preprocesobj,finaltestdata)
finaltestdata <- finaltestdata[,-zerovar]

## defino igual level al factor
levels(finaltestdata[,5]) <- levels(traindata[,5])

output <- predict(modelfit,newdata=finaltestdata)




