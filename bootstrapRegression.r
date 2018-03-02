# Function to generate the data set with 10% test data and 90% training data.
generateDataSet<-function(rawDataLoc){
	rawData<-read.table(rawDataLoc,header=FALSE,sep=",")
	# Used below commented code to filter out index column from data set.
	#rawData<-read.table(rawDataLoc,header=FALSE,sep=",")[,-1]
	trainingSetSize<-floor(0.9*nrow(rawData))
	trainingSetIndex<-sample(seq(1:nrow(rawData)),size=trainingSetSize,replace=FALSE)
	dataSet.training<-rawData[trainingSetIndex,]
	dataSet.test<-rawData[-trainingSetIndex,]
	colnames(dataSet.training)[ncol(dataSet.training)]<-"response"
	colnames(dataSet.test)[ncol(dataSet.test)]<-"response"
	dataSet<-list("training"=dataSet.training,"test"=dataSet.test)
	dataSet
}

# Function to get the residual squared error by computing against original response vs predicted response from model.
residualSquaredError<-function(dataSet){
	squaredError <- 0
	for (i in 1:nrow(dataSet)){
		squaredError<-squaredError+(dataSet[i,1]-dataSet[i,2])^2
	}
	squaredError
}

# Function to get the mean squared error by building regression tree and then applying pruning based on 10-fold cross validation.
getMeanSquaredError<-function(dataSet){
	responseCol<-ncol(dataSet$training)
	model.tree<-tree(response ~ . , data = dataSet$training)
	model.cvtree<-cv.tree(model.tree,FUN=prune.tree,K=10)
	bestSize<-model.cvtree$size[model.cvtree$dev==min(model.cvtree$dev)]
	model.prunetree<-prune.tree(model.tree,best=rev(bestSize)[1])
	model.predict<-predict(model.prunetree,dataSet$test)
	test.error<-cbind("Original"=dataSet$test[,responseCol],model.predict)
	squaredError<-round(residualSquaredError(test.error),2)
	meanSquaredError<-squaredError/nrow(dataSet$test)
	meanSquaredError
}

# Function retrives the bootstrap mean squared error rate by building bootstrap 50 replicas and regression tree
# corresponding to each replica by applying learning set for pruning the tree.
getBootstrapMeanSquaredError<-function(dataSet){
	responseCol<-ncol(dataSet$training)
	test.error<-rbind()
	for (i in seq(1:25)){
		bootstrap.training<-dataSet$training[sample(seq(1:nrow(dataSet$training)),size=nrow(dataSet$training),replace=TRUE),]
		model.tree<-tree(as.formula(response ~ .), data = bootstrap.training)
		model.cvtree<-cv.tree(model.tree,FUN=prune.tree,K=10)
		bestSize<-model.cvtree$size[model.cvtree$dev==min(model.cvtree$dev)]
		model.prunetree<-prune.tree(model.tree,best=rev(bestSize)[1])
		model.predict<-predict(model.prunetree,dataSet$test)
		if(is.null(nrow(test.error))==TRUE){
			test.error<-cbind("Original"=dataSet$test[,responseCol],model.predict)
		}
		else{
			test.error[,-1]<-test.error[,-1]+model.predict
		}
	}
	test.error[,-1]<-test.error[,-1]/25
	squaredError<-round(residualSquaredError(test.error),2)
	bootstrapMeanSquaredError<-squaredError/nrow(dataSet$test)
	bootstrapMeanSquaredError
}

rawDataLoc<-"C:\\Users\\shiva.ravi\\Downloads\\waveform.data\\housing.csv"

library(tree)

avgMeanSquaredError<-rbind()
dataSet<-generateDataSet(rawDataLoc)
bootstrap.training<-dataSet$training[sample(seq(1:nrow(dataSet$training)),size=nrow(dataSet$training),replace=TRUE),]
for (j in seq(1:100)){
	dataSet<-generateDataSet(rawDataLoc)
	meanSquaredError<-getMeanSquaredError(dataSet)
	bootstrapMeanSquaredError<-getBootstrapMeanSquaredError(dataSet)
	avgMeanSquaredError<-rbind(avgMeanSquaredError,data.frame("es"=meanSquaredError,"eb"=bootstrapMeanSquaredError))
}
avgMeanSquaredEs<-mean(avgMeanSquaredError[,"es"])
avgMeanSquaredEb<-mean(avgMeanSquaredError[,"eb"])
avgMeanSquaredEs
avgMeanSquaredEb
sd(avgMeanSquaredError[,"es"])*0.1
sd(avgMeanSquaredError[,"eb"])*0.1