library(dplyr)
library(glmnet)
library(MLmetrics)
library(ggplot2)

#reading file
rawfile<-read.csv("ObesityDataSet_fixed.csv")

#shuffling data frames
data<-rawfile[sample(nrow(rawfile)),]

#splitting into five sets of training/testing files
#1
trainset_1<-data[423:nrow(data),]
testset_1<-data[1:422,]
#2
trainset_2<-data[1:422,]
trainset_2_tmp<-data[845:nrow(data),]
trainset_2<-rbind(trainset_2,trainset_2_tmp)
testset_2<-data[423:844,]
#3
trainset_3<-data[1:844,]
trainset_3_tmp<-data[1267:nrow(data),]
trainset_3<-rbind(trainset_3,trainset_3_tmp)
testset_3<-data[845:1266,]
#4
trainset_4<-data[1:1266,]
trainset_4_tmp<-data[1689:nrow(data),]
trainset_4<-rbind(trainset_4,trainset_4_tmp)
testset_4<-data[1267:1688,]
#5
trainset_5<-data[1:1688,]
testset_5<-data[1689:2111,]

#gets measured response variables
data_Y<-data[17]
#creates empty vectors to store mean RMSE values
data_RMSE<-rep(NA,11)

#for each value of alpha
for(i in seq(0,1,0.1)){
  #empty vectors to store predicted Y values
  resultsdata<-rep(NA,nrow(rawfile))
  #empty vectors to store RMSE of each group k 
  data_RMSE_tmp<-rep(NA,5)
  #training models
  model_1 <- cv.glmnet(x=as.matrix(trainset_1[1:16]),y=as.matrix(trainset_1[17]),type.measure="mse", 
                          alpha=i,family="gaussian")
  model_2 <- cv.glmnet(x=as.matrix(trainset_2[1:16]),y=as.matrix(trainset_2[17]),type.measure="mse", 
                             alpha=i,family="gaussian")
  model_3 <- cv.glmnet(x=as.matrix(trainset_3[1:16]),y=as.matrix(trainset_3[17]),type.measure="mse", 
                             alpha=i,family="gaussian")
  model_4 <- cv.glmnet(x=as.matrix(trainset_4[1:16]),y=as.matrix(trainset_4[17]),type.measure="mse", 
                             alpha=i,family="gaussian")
  model_5 <- cv.glmnet(x=as.matrix(trainset_5[1:16]),y=as.matrix(trainset_5[17]),type.measure="mse", 
                             alpha=i,family="gaussian")
  #predicting Ys
  pred_1 <- predict(model_1,s=model_1$lambda.1se,
                           newx=as.matrix(testset_1[1:16]))
  pred_2 <- predict(model_2,s=model_2$lambda.1se,
                          newx=as.matrix(testset_2[1:16]))
  pred_3 <- predict(model_3,s=model_3$lambda.1se,
                          newx=as.matrix(testset_3[1:16]))
  pred_4 <- predict(model_4,s=model_4$lambda.1se,
                          newx=as.matrix(testset_4[1:16]))
  pred_5 <- predict(model_5,s=model_1$lambda.1se,
                          newx=as.matrix(testset_5[1:16]))
  #appends predicted Y values to the dataframe that contains measured Y values
  resultsdata[1:422]<-pred_1
  resultsdata[423:844]<-pred_2
  resultsdata[845:1266]<-pred_3
  resultsdata[1267:1688]<-pred_4
  resultsdata[1689:2111]<-pred_5
  #for each value of alpha, calculates RMSE of every group k
  #and then computes the average RMSE
  data_Y$alpha<-resultsdata
  data_RMSE_tmp[1]<-RMSE(data_Y$alpha[1:422],data_Y$condition[1:422])
  data_RMSE_tmp[2]<-RMSE(data_Y$alpha[423:844],data_Y$condition[423:844])
  data_RMSE_tmp[3]<-RMSE(data_Y$alpha[845:1266],data_Y$condition[845:1266])
  data_RMSE_tmp[4]<-RMSE(data_Y$alpha[1267:1688],data_Y$condition[1267:1688])
  data_RMSE_tmp[5]<-RMSE(data_Y$alpha[1689:2111],data_Y$condition[1689:2111])
  if (i==0){data_RMSE[1]<-mean(data_RMSE_tmp)}
  else if (i==0.1){data_RMSE[2]<-mean(data_RMSE_tmp)}
  else if (i==0.2){data_RMSE[3]<-mean(data_RMSE_tmp)}
  else if (i==0.3){data_RMSE[4]<-mean(data_RMSE_tmp)}
  else if (i==0.4){data_RMSE[5]<-mean(data_RMSE_tmp)}
  else if (i==0.5){data_RMSE[6]<-mean(data_RMSE_tmp)}
  else if (i==0.6){data_RMSE[7]<-mean(data_RMSE_tmp)}
  else if (i==0.7){data_RMSE[8]<-mean(data_RMSE_tmp)}
  else if (i==0.8){data_RMSE[9]<-mean(data_RMSE_tmp)}
  else if (i==0.9){data_RMSE[10]<-mean(data_RMSE_tmp)}
  else if (i==1){data_RMSE[11]<-mean(data_RMSE_tmp)}
}

#compiling results
compiled<-matrix(data=NA,nrow=11,ncol=2)
compiled[,1]<-seq(0,1,0.1)
compiled[,2]<-data_RMSE
colnames(compiled)<-c("Alpha","RMSE")
compiled<-as.data.frame(compiled)
compiled[,]<-sapply(compiled[,],as.numeric)

#making plots
ggplot(compiled,aes(x=Alpha,y=RMSE))+geom_col()+
  scale_x_continuous(breaks=c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1))

ggplot(compiled,aes(x=Alpha,y=RMSE))+geom_point(shape=19,color="blue",size=3)+
  scale_x_continuous(breaks=c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1))
