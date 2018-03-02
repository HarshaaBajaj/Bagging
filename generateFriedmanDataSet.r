# Function to generate Friedman Data set #1 and split it to 200/1000 ratio of training and test set.
generateFriedmanDataSet<-function(){
	epslan<-rnorm(1200,mean = 0,sd = 1)
	rawData<-rbind()
	for ( i in seq(1:1200)){
		predictors<-runif(10,min=0,max=1)
		response<-epslan[i] + (5*predictors[5]) + (10*predictors[4]) + (20*((predictors[3]-0.5)^2)) + (10*sin(pi*predictors[1]*predictors[2]))
		rawData<-rbind(rawData,cbind(rbind(predictors,deparse.level = 0),"y"=response))
	}
	rawData<-as.data.frame(rawData)
	trainingSetSize<-200
	trainingSetIndex<-sample(seq(1:nrow(rawData)),size=trainingSetSize,replace=FALSE)
	dataSet.training<-rawData[trainingSetIndex,]
	dataSet.test<-rawData[-trainingSetIndex,]
	colnames(dataSet.training)[ncol(dataSet.training)]<-"response"
	colnames(dataSet.test)[ncol(dataSet.test)]<-"response"
	dataSet<-list("training"=dataSet.training,"test"=dataSet.test)
	dataSet
}


# Rest of the functions from bootstrapRegression.R are used to run the iteration of 100 times
# on the simulated freidman data set with 25 bootstrap replicas.
# Pruning of the tree's is made optional in above functions since R default models expects 
# size of the tree model to be atleast 2 nodes.
avgMeanSquaredError<-rbind()
for (j in seq(1:100)){
	dataSet<-generateFriedmanDataSet()
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