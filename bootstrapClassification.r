# Function to generate the data set with 10% test data and 90% training data.
generateDataSet<-function(rawDataLoc){
	rawData<-read.table(rawDataLoc,header=FALSE,sep=',')
	# Used below commented code to filter out index column from data set.
	#rawData<-read.table(rawDataLoc,header=FALSE,sep=',')[,-1]
	trainingSetSize<-floor(0.9*nrow(rawData))
	trainingSetIndex<-sample(seq(1:nrow(rawData)),size=trainingSetSize,replace=FALSE)
	dataSet.training<-rawData[trainingSetIndex,]
	dataSet.test<-rawData[-trainingSetIndex,]
	# Below commented code is specific to waveform data.
	#dataSet.training<-rawData[sample(seq(1:nrow(dataSet.training)),size=1500,replace=FALSE),]
	#dataSet.test<-rawData[sample(seq(1:nrow(dataSet.test)),size=300,replace=FALSE),]
	colnames(dataSet.training)[ncol(dataSet.training)]<-"response"
	colnames(dataSet.test)[ncol(dataSet.test)]<-"response"
	dataSet<-list("training"=dataSet.training,"test"=dataSet.test)
	dataSet
}

# Function to get the match count against original response vs predicted response from model.
trueMatchCount<-function(dataSet){
	count <- 0
	colNames<-colnames(dataSet)[-1]
	for (i in 1:nrow(dataSet)){
		maxValue<-max(dataSet[i,-1])
		minCol<-"-1"
		for (k in 1:length(colNames)){
			if (maxValue == dataSet[i,colNames[k]]){
				minCol<-colNames[k]
				break
			}
		}
		if(dataSet[i,1]==minCol){
			count<-count+1
		}
	}
	count
}

# Function to get the misclassification error rate by building tree and then applying pruning based on 10-fold cross validation.
getMisclassRate<-function(dataSet){
	responseCol<-ncol(dataSet$training)
	dataSet$training[,responseCol]<-as.factor(dataSet$training[,responseCol])
	model.tree<-tree(response ~ . , data = dataSet$training)
	model.cvtree<-cv.tree(model.tree,FUN=prune.tree,K=10)
	bestSize<-model.cvtree$size[model.cvtree$dev==min(model.cvtree$dev)]
	model.prunetree<-prune.tree(model.tree,best=rev(bestSize)[1])
	model.predict<-predict(model.prunetree,dataSet$test)
	test.misclass<-cbind("Original"=dataSet$test[,responseCol],model.predict)
	trueMatchCnt<-trueMatchCount(test.misclass)
	misclassRate<-(1-(trueMatchCnt/nrow(dataSet$test)))*100
	misclassRate
}

# Function retrives the bootstrap misclassification error rate by building bootstrap 50 replicas and classification tree
# corresponding to each replica by applying learning set for pruning the tree.
getBootstrapMisclassRate<-function(dataSet){
	responseCol<-ncol(dataSet$training)
	test.misclass<-rbind()
	for (i in seq(1:50)){
		bootstrap.training<-dataSet$training[sample(seq(1:nrow(dataSet$training)),size=nrow(dataSet$training),replace=TRUE),]
		bootstrap.training[,responseCol]<-as.factor(bootstrap.training[,responseCol])
		model.tree<-tree(as.formula(response ~ .), data = bootstrap.training)
		model.cvtree<-cv.tree(model.tree,FUN=prune.tree,K=10)
		bestSize<-model.cvtree$size[model.cvtree$dev==min(model.cvtree$dev)]
		model.prunetree<-prune.tree(model.tree,best=rev(bestSize)[1])
		model.predict<-predict(model.prunetree,dataSet$test)
		if(is.null(nrow(test.misclass))==TRUE){
			test.misclass<-cbind("Original"=dataSet$test[,responseCol],model.predict)
		}
		else{
			test.misclass[,-1]<-test.misclass[,-1]+model.predict
		}
	}
	trueMatchCnt<-trueMatchCount(test.misclass)
	misclassRate<-(1-(trueMatchCnt/nrow(dataSet$test)))*100
	misclassRate
}

library(tree)

rawDataLoc<-"D:\\Personal\\MATH-569\\waveform.data\\waveform.data"
#rawDataLoc<-"D:\\Personal\\MATH-569\\ionosphere.data.txt"
#rawDataLoc<-"D:\\Personal\\MATH-569\\glass.data.txt"
#rawDataLoc<-"D:\\Personal\\MATH-569\\breast-cancer-wisconsin.data.txt"

avgError<-rbind()
dataSet<-generateDataSet(rawDataLoc)
bootstrap.training<-dataSet$training[sample(seq(1:nrow(dataSet$training)),size=nrow(dataSet$training),replace=TRUE),]
for (j in seq(1:100)){
	dataSet<-generateDataSet(rawDataLoc)
	misCntPer<-getMisclassRate(dataSet)
	bootstrapMisCntPer<-getBootstrapMisclassRate(dataSet)
	avgError<-rbind(avgError,data.frame("es"=misCntPer,"eb"=bootstrapMisCntPer))
}
avgEs<-mean(avgError[,"es"])
avgEb<-mean(avgError[,"eb"])
avgEs
avgEb
sd(avgError[,"es"])*0.1
sd(avgError[,"eb"])*0.1
