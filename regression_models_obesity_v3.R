library(dplyr)
library(glmnet)
library(MLmetrics)
library(ggplot2)
library(earth)

# Fixes input dataset to contain only numeric values
data <- read.csv(infile)
  
mapping0 <- c('yes' = 1, 'no' = 0)
data$hist <- mapping0[data$family_history_with_overweight]
data$family_history_with_overweight <- NULL
data$favc <- mapping0[data$FAVC]
data$FAVC <- NULL
data$smoke <- mapping0[data$SMOKE]
data$SMOKE <- NULL
data$scc <- mapping0[data$SCC]
data$SCC <- NULL
  
mapping1 <- c("no" = 0, "Sometimes"= 1, "Frequently" = 2,"Always" = 3)
data$caec <- mapping1[data$CAEC]
data$CAEC <- NULL
data$calc <- mapping1[data$CALC]
data$CALC <- NULL
  
mapping2 <- c('Male' = 0, 'Female' = 1)
data$gender <- mapping2[data$Gender]
data$Gender <- NULL
  
mapping3 <- c('Automobile'=0, 'Motorbike'=1, 'Bike'=2, 'Public_Transportation'=3, 'Walking'=4)
data$mtrans <- mapping3[data$MTRANS]
data$MTRANS <- NULL
  
mapping4 <- c('Insufficient_Weight'=0,'Normal_Weight'=1,'Overweight_Level_I'=2,'Overweight_Level_II'=3,
                'Obesity_Type_I'=4,'Obesity_Type_II'=5,'Obesity_Type_III'=6)
data$condition <- mapping4[data$NObeyesdad]
data$NObeyesdad <- NULL
    
write.csv(data, 'ObesityDataSet_numeric.csv', row.names = FALSE)


#reading file
rawfile<-read.csv("ObesityDataSet_numeric.csv")

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
raw_RMSE <- matrix(, nrow=11, ncol=5)



###Ridge, Lasso, EN (Applied Methods)

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
  
  data_RMSE[(i*10)+1] <- mean(data_RMSE_tmp)
  raw_RMSE[(i*10)+1,] <- data_RMSE_tmp
}

data_RMSE
raw_RMSE

#compiling results
compiled<-matrix(data=NA,nrow=11,ncol=2)
compiled[,1]<-seq(0,1,0.1)
compiled[,2]<-data_RMSE
colnames(compiled)<-c("Model","RMSE")
rownames(raw_RMSE)<-c(seq(0,1,0.1))
raw_RMSE<-as.data.frame(raw_RMSE)
compiled<-as.data.frame(compiled)
raw_RMSE[,]<-sapply(raw_RMSE[,],as.numeric)
compiled[,]<-sapply(compiled[,],as.numeric)




###MARS (Original Analysis)

#builds a model for each k 
fit1<-earth(condition~.,trainset_1)
fit2<-earth(condition~.,trainset_2)
fit3<-earth(condition~.,trainset_3)
fit4<-earth(condition~.,trainset_4)
fit5<-earth(condition~.,trainset_5)

#predicts using each model
prediction1<-predict(fit1,testset_1[1:16])
prediction2<-predict(fit2,testset_2[1:16])
prediction3<-predict(fit3,testset_3[1:16])
prediction4<-predict(fit4,testset_4[1:16])
prediction5<-predict(fit5,testset_5[1:16])

#creates empty vector to store results
marsresultsdata<-rep(NA,nrow(rawfile))
marsresultsdata[1:422]<-prediction1
marsresultsdata[423:844]<-prediction2
marsresultsdata[845:1266]<-prediction3
marsresultsdata[1267:1688]<-prediction4
marsresultsdata[1689:2111]<-prediction5

#appends results to dataframe containing true Y values
data_Y$mars<-marsresultsdata

#computes RMSE
marsdata_RMSE_tmp<-rep(NA,5)
marsdata_RMSE_tmp[1]<-RMSE(data_Y$mars[1:422],data_Y$condition[1:422])
marsdata_RMSE_tmp[2]<-RMSE(data_Y$mars[423:844],data_Y$condition[423:844])
marsdata_RMSE_tmp[3]<-RMSE(data_Y$mars[845:1266],data_Y$condition[845:1266])
marsdata_RMSE_tmp[4]<-RMSE(data_Y$mars[1267:1688],data_Y$condition[1267:1688])
marsdata_RMSE_tmp[5]<-RMSE(data_Y$mars[1689:2111],data_Y$condition[1689:2111])
mars.rmse<-as.data.frame(t(matrix(c("MARS",as.numeric(mean(marsdata_RMSE_tmp))))))
colnames(mars.rmse)<-c("Model","RMSE")

raw_RMSE<-rbind(raw_RMSE, marsdata_RMSE_tmp)
rownames(raw_RMSE)[rownames(raw_RMSE) == '12'] <- 'MARS'

compiled<-rbind(compiled,mars.rmse)
compiled[,2]<-sapply(compiled[,2],as.numeric)

###making plots
ggplot(compiled,aes(x=Model,y=RMSE))+geom_col()
ggplot(compiled,aes(x=Model,y=RMSE))+geom_point(shape=19,color="blue",size=3)

### final RMSE data
# all RMSE data gathered
raw_RMSE

# average RMSE data for each model
compiled

