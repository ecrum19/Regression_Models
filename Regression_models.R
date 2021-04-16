library(dplyr)
library(glmnet)
library(MLmetrics)
library(ggplot2)

#reading file
rawfile<-read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/parkinsons/telemonitoring/parkinsons_updrs.data"))
motorUPDRS<-dplyr::select(rawfile,-"subject.",-"age",-"sex",-"test_time",-"total_UPDRS")
totalUPDRS<-dplyr::select(rawfile,-"subject.",-"age",-"sex",-"test_time",-"motor_UPDRS")

#shuffling data frames
motorUPDRS<-motorUPDRS[sample(nrow(motorUPDRS)),]
totalUPDRS<-totalUPDRS[sample(nrow(totalUPDRS)),]

#splitting into five sets of training/testing files
#1
trainmotorUPDRS_1<-motorUPDRS[1176:nrow(motorUPDRS),]
testmotorUPDRS_1<-motorUPDRS[1:1175,]
traintotalUPDRS_1<-totalUPDRS[1176:nrow(totalUPDRS),]
testtotalUPDRS_1<-totalUPDRS[1:1175,]
#2
trainmotorUPDRS_2<-motorUPDRS[1:1175,]
trainmotorUPDRS_2_tpm<-motorUPDRS[2351:nrow(motorUPDRS),]
trainmotorUPDRS_2<-rbind(trainmotorUPDRS_2,trainmotorUPDRS_2_tpm)
testmotorUPDRS_2<-motorUPDRS[1176:2350,]
traintotalUPDRS_2<-totalUPDRS[1:1175,]
traintotalUPDRS_2_tpm<-totalUPDRS[2351:nrow(totalUPDRS),]
traintotalUPDRS_2<-rbind(traintotalUPDRS_2,traintotalUPDRS_2_tpm)
testtotalUPDRS_2<-totalUPDRS[1176:2350,]
#3
trainmotorUPDRS_3<-motorUPDRS[1:2350,]
trainmotorUPDRS_3_tpm<-motorUPDRS[3526:nrow(motorUPDRS),]
trainmotorUPDRS_3<-rbind(trainmotorUPDRS_3,trainmotorUPDRS_3_tpm)
testmotorUPDRS_3<-motorUPDRS[2351:3525,]
traintotalUPDRS_3<-totalUPDRS[1:2350,]
traintotalUPDRS_3_tpm<-totalUPDRS[3526:nrow(totalUPDRS),]
traintotalUPDRS_3<-rbind(traintotalUPDRS_3,traintotalUPDRS_3_tpm)
testtotalUPDRS_3<-totalUPDRS[2351:3525,]
#4
trainmotorUPDRS_4<-motorUPDRS[1:3525,]
trainmotorUPDRS_4_tpm<-motorUPDRS[4701:nrow(motorUPDRS),]
trainmotorUPDRS_4<-rbind(trainmotorUPDRS_4,trainmotorUPDRS_4_tpm)
testmotorUPDRS_4<-motorUPDRS[3526:4700,]
traintotalUPDRS_4<-totalUPDRS[1:3525,]
traintotalUPDRS_4_tpm<-totalUPDRS[4701:nrow(totalUPDRS),]
traintotalUPDRS_4<-rbind(traintotalUPDRS_4,traintotalUPDRS_4_tpm)
testtotalUPDRS_4<-totalUPDRS[3526:4700,]
#5
trainmotorUPDRS_5<-motorUPDRS[1:4700,]
testmotorUPDRS_5<-motorUPDRS[4701:5875,]
traintotalUPDRS_5<-totalUPDRS[1:4700,]
testtotalUPDRS_5<-totalUPDRS[4701:5875,]

#gets measured response variables
motorUPDRS_Y<-motorUPDRS[1]
totalUPDRS_Y<-totalUPDRS[1]
#creates empty vectors to store mean RMSE values
motorUPDRS_RMSE<-rep(NA,11)
totalUPDRS_RMSE<-rep(NA,11)

#for each value of alpha
for(i in seq(0,1,0.1)){
  #empty vectors to store predicted Y values
  resultsmotor<-rep(NA,nrow(rawfile))
  resultstotal<-rep(NA,nrow(rawfile))
  #empty vectors to store RMSE of each group k 
  motorUPDRS_RMSE_tmp<-rep(NA,5)
  totalUPDRS_RMSE_tmp<-rep(NA,5)
  #training models
  motor.model_1 <- cv.glmnet(x=as.matrix(trainmotorUPDRS_1[2:ncol(trainmotorUPDRS_1)]),y=as.matrix(trainmotorUPDRS_1[1]),type.measure="mse", 
                          alpha=i,family="gaussian")
  motor.model_2 <- cv.glmnet(x=as.matrix(trainmotorUPDRS_2[2:ncol(trainmotorUPDRS_2)]),y=as.matrix(trainmotorUPDRS_2[1]),type.measure="mse", 
                             alpha=i,family="gaussian")
  motor.model_3 <- cv.glmnet(x=as.matrix(trainmotorUPDRS_3[2:ncol(trainmotorUPDRS_3)]),y=as.matrix(trainmotorUPDRS_3[1]),type.measure="mse", 
                             alpha=i,family="gaussian")
  motor.model_4 <- cv.glmnet(x=as.matrix(trainmotorUPDRS_4[2:ncol(trainmotorUPDRS_4)]),y=as.matrix(trainmotorUPDRS_4[1]),type.measure="mse", 
                             alpha=i,family="gaussian")
  motor.model_5 <- cv.glmnet(x=as.matrix(trainmotorUPDRS_5[2:ncol(trainmotorUPDRS_5)]),y=as.matrix(trainmotorUPDRS_5[1]),type.measure="mse", 
                             alpha=i,family="gaussian")
  total.model_1 <- cv.glmnet(x=as.matrix(traintotalUPDRS_1[2:ncol(traintotalUPDRS_1)]),y=as.matrix(traintotalUPDRS_1[1]),type.measure="mse", 
                             alpha=i,family="gaussian")
  total.model_2 <- cv.glmnet(x=as.matrix(traintotalUPDRS_2[2:ncol(traintotalUPDRS_2)]),y=as.matrix(traintotalUPDRS_2[1]),type.measure="mse", 
                             alpha=i,family="gaussian")
  total.model_3 <- cv.glmnet(x=as.matrix(traintotalUPDRS_3[2:ncol(traintotalUPDRS_3)]),y=as.matrix(traintotalUPDRS_3[1]),type.measure="mse", 
                             alpha=i,family="gaussian")
  total.model_4 <- cv.glmnet(x=as.matrix(traintotalUPDRS_4[2:ncol(traintotalUPDRS_4)]),y=as.matrix(traintotalUPDRS_4[1]),type.measure="mse", 
                             alpha=i,family="gaussian")
  total.model_5 <- cv.glmnet(x=as.matrix(traintotalUPDRS_5[2:ncol(traintotalUPDRS_5)]),y=as.matrix(traintotalUPDRS_5[1]),type.measure="mse", 
                             alpha=i,family="gaussian")
  #predicting Ys
  motor.pred_1 <- predict(motor.model_1,s=motor.model_1$lambda.1se,
                           newx=as.matrix(testmotorUPDRS_1[2:ncol(testmotorUPDRS_1)]))
  motor.pred_2 <- predict(motor.model_2,s=motor.model_2$lambda.1se,
                          newx=as.matrix(testmotorUPDRS_2[2:ncol(testmotorUPDRS_2)]))
  motor.pred_3 <- predict(motor.model_3,s=motor.model_3$lambda.1se,
                          newx=as.matrix(testmotorUPDRS_3[2:ncol(testmotorUPDRS_3)]))
  motor.pred_4 <- predict(motor.model_4,s=motor.model_4$lambda.1se,
                          newx=as.matrix(testmotorUPDRS_4[2:ncol(testmotorUPDRS_4)]))
  motor.pred_5 <- predict(motor.model_5,s=motor.model_1$lambda.1se,
                          newx=as.matrix(testmotorUPDRS_5[2:ncol(testmotorUPDRS_5)]))
  total.pred_1 <- predict(total.model_1,s=total.model_1$lambda.1se,
                          newx=as.matrix(testtotalUPDRS_1[2:ncol(testtotalUPDRS_1)]))
  total.pred_2 <- predict(total.model_2,s=total.model_2$lambda.1se,
                          newx=as.matrix(testtotalUPDRS_2[2:ncol(testtotalUPDRS_2)]))
  total.pred_3 <- predict(total.model_3,s=total.model_3$lambda.1se,
                          newx=as.matrix(testtotalUPDRS_3[2:ncol(testtotalUPDRS_3)]))
  total.pred_4 <- predict(total.model_4,s=total.model_4$lambda.1se,
                          newx=as.matrix(testtotalUPDRS_4[2:ncol(testtotalUPDRS_4)]))
  total.pred_5 <- predict(total.model_5,s=total.model_1$lambda.1se,
                          newx=as.matrix(testtotalUPDRS_5[2:ncol(testtotalUPDRS_5)]))
  #appends predicted Y values to the dataframe that contains measured Y values
  resultsmotor[1:1175]<-motor.pred_1
  resultsmotor[1176:2350]<-motor.pred_2
  resultsmotor[2351:3525]<-motor.pred_3
  resultsmotor[3526:4700]<-motor.pred_4
  resultsmotor[4701:5875]<-motor.pred_5
  resultstotal[1:1175]<-total.pred_1
  resultstotal[1176:2350]<-total.pred_2
  resultstotal[2351:3525]<-total.pred_3
  resultstotal[3526:4700]<-total.pred_4
  resultstotal[4701:5875]<-total.pred_5
  #for each value of alpha, calculates RMSE of every group k
  #and then computes the average RMSE
  if (i==0){
    motorUPDRS_Y$alpha<-resultsmotor
    totalUPDRS_Y$alpha<-resultstotal
    motorUPDRS_RMSE_tmp[1]<-RMSE(motorUPDRS_Y$motor_UPDRS[1:1175],motorUPDRS_Y$alpha[1:1175])
    totalUPDRS_RMSE_tmp[1]<-RMSE(totalUPDRS_Y$total_UPDRS[1:1175],totalUPDRS_Y$alpha[1:1175])
    motorUPDRS_RMSE_tmp[2]<-RMSE(motorUPDRS_Y$motor_UPDRS[1176:2350],motorUPDRS_Y$alpha[1176:2350])
    totalUPDRS_RMSE_tmp[2]<-RMSE(totalUPDRS_Y$total_UPDRS[1176:2350],totalUPDRS_Y$alpha[1176:2350])
    motorUPDRS_RMSE_tmp[3]<-RMSE(motorUPDRS_Y$motor_UPDRS[2351:3525],motorUPDRS_Y$alpha[2351:3525])
    totalUPDRS_RMSE_tmp[3]<-RMSE(totalUPDRS_Y$total_UPDRS[2351:3525],totalUPDRS_Y$alpha[2351:3525])
    motorUPDRS_RMSE_tmp[4]<-RMSE(motorUPDRS_Y$motor_UPDRS[3526:4700],motorUPDRS_Y$alpha[3526:4700])
    totalUPDRS_RMSE_tmp[4]<-RMSE(totalUPDRS_Y$total_UPDRS[3526:4700],totalUPDRS_Y$alpha[3526:4700])
    motorUPDRS_RMSE_tmp[5]<-RMSE(motorUPDRS_Y$motor_UPDRS[4701:5875],motorUPDRS_Y$alpha[4701:5875])
    totalUPDRS_RMSE_tmp[5]<-RMSE(totalUPDRS_Y$total_UPDRS[4701:5875],totalUPDRS_Y$alpha[4701:5875])
    motorUPDRS_RMSE[1]<-mean(motorUPDRS_RMSE_tmp)
    totalUPDRS_RMSE[1]<-mean(totalUPDRS_RMSE_tmp)
  }
  else if (i==0.1){
    motorUPDRS_Y$alpha<-resultsmotor
    totalUPDRS_Y$alpha<-resultstotal
    motorUPDRS_RMSE_tmp[1]<-RMSE(motorUPDRS_Y$motor_UPDRS[1:1175],motorUPDRS_Y$alpha[1:1175])
    totalUPDRS_RMSE_tmp[1]<-RMSE(totalUPDRS_Y$total_UPDRS[1:1175],totalUPDRS_Y$alpha[1:1175])
    motorUPDRS_RMSE_tmp[2]<-RMSE(motorUPDRS_Y$motor_UPDRS[1176:2350],motorUPDRS_Y$alpha[1176:2350])
    totalUPDRS_RMSE_tmp[2]<-RMSE(totalUPDRS_Y$total_UPDRS[1176:2350],totalUPDRS_Y$alpha[1176:2350])
    motorUPDRS_RMSE_tmp[3]<-RMSE(motorUPDRS_Y$motor_UPDRS[2351:3525],motorUPDRS_Y$alpha[2351:3525])
    totalUPDRS_RMSE_tmp[3]<-RMSE(totalUPDRS_Y$total_UPDRS[2351:3525],totalUPDRS_Y$alpha[2351:3525])
    motorUPDRS_RMSE_tmp[4]<-RMSE(motorUPDRS_Y$motor_UPDRS[3526:4700],motorUPDRS_Y$alpha[3526:4700])
    totalUPDRS_RMSE_tmp[4]<-RMSE(totalUPDRS_Y$total_UPDRS[3526:4700],totalUPDRS_Y$alpha[3526:4700])
    motorUPDRS_RMSE_tmp[5]<-RMSE(motorUPDRS_Y$motor_UPDRS[4701:5875],motorUPDRS_Y$alpha[4701:5875])
    totalUPDRS_RMSE_tmp[5]<-RMSE(totalUPDRS_Y$total_UPDRS[4701:5875],totalUPDRS_Y$alpha[4701:5875])
    motorUPDRS_RMSE[2]<-mean(motorUPDRS_RMSE_tmp)
    totalUPDRS_RMSE[2]<-mean(totalUPDRS_RMSE_tmp)
  }
  else if (i==0.2){
    motorUPDRS_Y$alpha<-resultsmotor
    totalUPDRS_Y$alpha<-resultstotal
    motorUPDRS_RMSE_tmp[1]<-RMSE(motorUPDRS_Y$motor_UPDRS[1:1175],motorUPDRS_Y$alpha[1:1175])
    totalUPDRS_RMSE_tmp[1]<-RMSE(totalUPDRS_Y$total_UPDRS[1:1175],totalUPDRS_Y$alpha[1:1175])
    motorUPDRS_RMSE_tmp[2]<-RMSE(motorUPDRS_Y$motor_UPDRS[1176:2350],motorUPDRS_Y$alpha[1176:2350])
    totalUPDRS_RMSE_tmp[2]<-RMSE(totalUPDRS_Y$total_UPDRS[1176:2350],totalUPDRS_Y$alpha[1176:2350])
    motorUPDRS_RMSE_tmp[3]<-RMSE(motorUPDRS_Y$motor_UPDRS[2351:3525],motorUPDRS_Y$alpha[2351:3525])
    totalUPDRS_RMSE_tmp[3]<-RMSE(totalUPDRS_Y$total_UPDRS[2351:3525],totalUPDRS_Y$alpha[2351:3525])
    motorUPDRS_RMSE_tmp[4]<-RMSE(motorUPDRS_Y$motor_UPDRS[3526:4700],motorUPDRS_Y$alpha[3526:4700])
    totalUPDRS_RMSE_tmp[4]<-RMSE(totalUPDRS_Y$total_UPDRS[3526:4700],totalUPDRS_Y$alpha[3526:4700])
    motorUPDRS_RMSE_tmp[5]<-RMSE(motorUPDRS_Y$motor_UPDRS[4701:5875],motorUPDRS_Y$alpha[4701:5875])
    totalUPDRS_RMSE_tmp[5]<-RMSE(totalUPDRS_Y$total_UPDRS[4701:5875],totalUPDRS_Y$alpha[4701:5875])
    motorUPDRS_RMSE[3]<-mean(motorUPDRS_RMSE_tmp)
    totalUPDRS_RMSE[3]<-mean(totalUPDRS_RMSE_tmp)
  }
  else if (i==0.3){
    motorUPDRS_Y$alpha<-resultsmotor
    totalUPDRS_Y$alpha<-resultstotal
    motorUPDRS_RMSE_tmp[1]<-RMSE(motorUPDRS_Y$motor_UPDRS[1:1175],motorUPDRS_Y$alpha[1:1175])
    totalUPDRS_RMSE_tmp[1]<-RMSE(totalUPDRS_Y$total_UPDRS[1:1175],totalUPDRS_Y$alpha[1:1175])
    motorUPDRS_RMSE_tmp[2]<-RMSE(motorUPDRS_Y$motor_UPDRS[1176:2350],motorUPDRS_Y$alpha[1176:2350])
    totalUPDRS_RMSE_tmp[2]<-RMSE(totalUPDRS_Y$total_UPDRS[1176:2350],totalUPDRS_Y$alpha[1176:2350])
    motorUPDRS_RMSE_tmp[3]<-RMSE(motorUPDRS_Y$motor_UPDRS[2351:3525],motorUPDRS_Y$alpha[2351:3525])
    totalUPDRS_RMSE_tmp[3]<-RMSE(totalUPDRS_Y$total_UPDRS[2351:3525],totalUPDRS_Y$alpha[2351:3525])
    motorUPDRS_RMSE_tmp[4]<-RMSE(motorUPDRS_Y$motor_UPDRS[3526:4700],motorUPDRS_Y$alpha[3526:4700])
    totalUPDRS_RMSE_tmp[4]<-RMSE(totalUPDRS_Y$total_UPDRS[3526:4700],totalUPDRS_Y$alpha[3526:4700])
    motorUPDRS_RMSE_tmp[5]<-RMSE(motorUPDRS_Y$motor_UPDRS[4701:5875],motorUPDRS_Y$alpha[4701:5875])
    totalUPDRS_RMSE_tmp[5]<-RMSE(totalUPDRS_Y$total_UPDRS[4701:5875],totalUPDRS_Y$alpha[4701:5875])
    motorUPDRS_RMSE[4]<-mean(motorUPDRS_RMSE_tmp)
    totalUPDRS_RMSE[4]<-mean(totalUPDRS_RMSE_tmp)
  }
  else if (i==0.4){
    motorUPDRS_Y$alpha<-resultsmotor
    totalUPDRS_Y$alpha<-resultstotal
    motorUPDRS_RMSE_tmp[1]<-RMSE(motorUPDRS_Y$motor_UPDRS[1:1175],motorUPDRS_Y$alpha[1:1175])
    totalUPDRS_RMSE_tmp[1]<-RMSE(totalUPDRS_Y$total_UPDRS[1:1175],totalUPDRS_Y$alpha[1:1175])
    motorUPDRS_RMSE_tmp[2]<-RMSE(motorUPDRS_Y$motor_UPDRS[1176:2350],motorUPDRS_Y$alpha[1176:2350])
    totalUPDRS_RMSE_tmp[2]<-RMSE(totalUPDRS_Y$total_UPDRS[1176:2350],totalUPDRS_Y$alpha[1176:2350])
    motorUPDRS_RMSE_tmp[3]<-RMSE(motorUPDRS_Y$motor_UPDRS[2351:3525],motorUPDRS_Y$alpha[2351:3525])
    totalUPDRS_RMSE_tmp[3]<-RMSE(totalUPDRS_Y$total_UPDRS[2351:3525],totalUPDRS_Y$alpha[2351:3525])
    motorUPDRS_RMSE_tmp[4]<-RMSE(motorUPDRS_Y$motor_UPDRS[3526:4700],motorUPDRS_Y$alpha[3526:4700])
    totalUPDRS_RMSE_tmp[4]<-RMSE(totalUPDRS_Y$total_UPDRS[3526:4700],totalUPDRS_Y$alpha[3526:4700])
    motorUPDRS_RMSE_tmp[5]<-RMSE(motorUPDRS_Y$motor_UPDRS[4701:5875],motorUPDRS_Y$alpha[4701:5875])
    totalUPDRS_RMSE_tmp[5]<-RMSE(totalUPDRS_Y$total_UPDRS[4701:5875],totalUPDRS_Y$alpha[4701:5875])
    motorUPDRS_RMSE[5]<-mean(motorUPDRS_RMSE_tmp)
    totalUPDRS_RMSE[5]<-mean(totalUPDRS_RMSE_tmp)
  }
  else if (i==0.5){
    motorUPDRS_Y$alpha<-resultsmotor
    totalUPDRS_Y$alpha<-resultstotal
    motorUPDRS_RMSE_tmp[1]<-RMSE(motorUPDRS_Y$motor_UPDRS[1:1175],motorUPDRS_Y$alpha[1:1175])
    totalUPDRS_RMSE_tmp[1]<-RMSE(totalUPDRS_Y$total_UPDRS[1:1175],totalUPDRS_Y$alpha[1:1175])
    motorUPDRS_RMSE_tmp[2]<-RMSE(motorUPDRS_Y$motor_UPDRS[1176:2350],motorUPDRS_Y$alpha[1176:2350])
    totalUPDRS_RMSE_tmp[2]<-RMSE(totalUPDRS_Y$total_UPDRS[1176:2350],totalUPDRS_Y$alpha[1176:2350])
    motorUPDRS_RMSE_tmp[3]<-RMSE(motorUPDRS_Y$motor_UPDRS[2351:3525],motorUPDRS_Y$alpha[2351:3525])
    totalUPDRS_RMSE_tmp[3]<-RMSE(totalUPDRS_Y$total_UPDRS[2351:3525],totalUPDRS_Y$alpha[2351:3525])
    motorUPDRS_RMSE_tmp[4]<-RMSE(motorUPDRS_Y$motor_UPDRS[3526:4700],motorUPDRS_Y$alpha[3526:4700])
    totalUPDRS_RMSE_tmp[4]<-RMSE(totalUPDRS_Y$total_UPDRS[3526:4700],totalUPDRS_Y$alpha[3526:4700])
    motorUPDRS_RMSE_tmp[5]<-RMSE(motorUPDRS_Y$motor_UPDRS[4701:5875],motorUPDRS_Y$alpha[4701:5875])
    totalUPDRS_RMSE_tmp[5]<-RMSE(totalUPDRS_Y$total_UPDRS[4701:5875],totalUPDRS_Y$alpha[4701:5875])
    motorUPDRS_RMSE[6]<-mean(motorUPDRS_RMSE_tmp)
    totalUPDRS_RMSE[6]<-mean(totalUPDRS_RMSE_tmp)
  }
  else if (i==0.6){
    motorUPDRS_Y$alpha<-resultsmotor
    totalUPDRS_Y$alpha<-resultstotal
    motorUPDRS_RMSE_tmp[1]<-RMSE(motorUPDRS_Y$motor_UPDRS[1:1175],motorUPDRS_Y$alpha[1:1175])
    totalUPDRS_RMSE_tmp[1]<-RMSE(totalUPDRS_Y$total_UPDRS[1:1175],totalUPDRS_Y$alpha[1:1175])
    motorUPDRS_RMSE_tmp[2]<-RMSE(motorUPDRS_Y$motor_UPDRS[1176:2350],motorUPDRS_Y$alpha[1176:2350])
    totalUPDRS_RMSE_tmp[2]<-RMSE(totalUPDRS_Y$total_UPDRS[1176:2350],totalUPDRS_Y$alpha[1176:2350])
    motorUPDRS_RMSE_tmp[3]<-RMSE(motorUPDRS_Y$motor_UPDRS[2351:3525],motorUPDRS_Y$alpha[2351:3525])
    totalUPDRS_RMSE_tmp[3]<-RMSE(totalUPDRS_Y$total_UPDRS[2351:3525],totalUPDRS_Y$alpha[2351:3525])
    motorUPDRS_RMSE_tmp[4]<-RMSE(motorUPDRS_Y$motor_UPDRS[3526:4700],motorUPDRS_Y$alpha[3526:4700])
    totalUPDRS_RMSE_tmp[4]<-RMSE(totalUPDRS_Y$total_UPDRS[3526:4700],totalUPDRS_Y$alpha[3526:4700])
    motorUPDRS_RMSE_tmp[5]<-RMSE(motorUPDRS_Y$motor_UPDRS[4701:5875],motorUPDRS_Y$alpha[4701:5875])
    totalUPDRS_RMSE_tmp[5]<-RMSE(totalUPDRS_Y$total_UPDRS[4701:5875],totalUPDRS_Y$alpha[4701:5875])
    motorUPDRS_RMSE[7]<-mean(motorUPDRS_RMSE_tmp)
    totalUPDRS_RMSE[7]<-mean(totalUPDRS_RMSE_tmp)
  }
  else if (i==0.7){
    motorUPDRS_Y$alpha<-resultsmotor
    totalUPDRS_Y$alpha<-resultstotal
    motorUPDRS_RMSE_tmp[1]<-RMSE(motorUPDRS_Y$motor_UPDRS[1:1175],motorUPDRS_Y$alpha[1:1175])
    totalUPDRS_RMSE_tmp[1]<-RMSE(totalUPDRS_Y$total_UPDRS[1:1175],totalUPDRS_Y$alpha[1:1175])
    motorUPDRS_RMSE_tmp[2]<-RMSE(motorUPDRS_Y$motor_UPDRS[1176:2350],motorUPDRS_Y$alpha[1176:2350])
    totalUPDRS_RMSE_tmp[2]<-RMSE(totalUPDRS_Y$total_UPDRS[1176:2350],totalUPDRS_Y$alpha[1176:2350])
    motorUPDRS_RMSE_tmp[3]<-RMSE(motorUPDRS_Y$motor_UPDRS[2351:3525],motorUPDRS_Y$alpha[2351:3525])
    totalUPDRS_RMSE_tmp[3]<-RMSE(totalUPDRS_Y$total_UPDRS[2351:3525],totalUPDRS_Y$alpha[2351:3525])
    motorUPDRS_RMSE_tmp[4]<-RMSE(motorUPDRS_Y$motor_UPDRS[3526:4700],motorUPDRS_Y$alpha[3526:4700])
    totalUPDRS_RMSE_tmp[4]<-RMSE(totalUPDRS_Y$total_UPDRS[3526:4700],totalUPDRS_Y$alpha[3526:4700])
    motorUPDRS_RMSE_tmp[5]<-RMSE(motorUPDRS_Y$motor_UPDRS[4701:5875],motorUPDRS_Y$alpha[4701:5875])
    totalUPDRS_RMSE_tmp[5]<-RMSE(totalUPDRS_Y$total_UPDRS[4701:5875],totalUPDRS_Y$alpha[4701:5875])
    motorUPDRS_RMSE[8]<-mean(motorUPDRS_RMSE_tmp)
    totalUPDRS_RMSE[8]<-mean(totalUPDRS_RMSE_tmp)
  }
  else if (i==0.8){
    motorUPDRS_Y$alpha<-resultsmotor
    totalUPDRS_Y$alpha<-resultstotal
    motorUPDRS_RMSE_tmp[1]<-RMSE(motorUPDRS_Y$motor_UPDRS[1:1175],motorUPDRS_Y$alpha[1:1175])
    totalUPDRS_RMSE_tmp[1]<-RMSE(totalUPDRS_Y$total_UPDRS[1:1175],totalUPDRS_Y$alpha[1:1175])
    motorUPDRS_RMSE_tmp[2]<-RMSE(motorUPDRS_Y$motor_UPDRS[1176:2350],motorUPDRS_Y$alpha[1176:2350])
    totalUPDRS_RMSE_tmp[2]<-RMSE(totalUPDRS_Y$total_UPDRS[1176:2350],totalUPDRS_Y$alpha[1176:2350])
    motorUPDRS_RMSE_tmp[3]<-RMSE(motorUPDRS_Y$motor_UPDRS[2351:3525],motorUPDRS_Y$alpha[2351:3525])
    totalUPDRS_RMSE_tmp[3]<-RMSE(totalUPDRS_Y$total_UPDRS[2351:3525],totalUPDRS_Y$alpha[2351:3525])
    motorUPDRS_RMSE_tmp[4]<-RMSE(motorUPDRS_Y$motor_UPDRS[3526:4700],motorUPDRS_Y$alpha[3526:4700])
    totalUPDRS_RMSE_tmp[4]<-RMSE(totalUPDRS_Y$total_UPDRS[3526:4700],totalUPDRS_Y$alpha[3526:4700])
    motorUPDRS_RMSE_tmp[5]<-RMSE(motorUPDRS_Y$motor_UPDRS[4701:5875],motorUPDRS_Y$alpha[4701:5875])
    totalUPDRS_RMSE_tmp[5]<-RMSE(totalUPDRS_Y$total_UPDRS[4701:5875],totalUPDRS_Y$alpha[4701:5875])
    motorUPDRS_RMSE[9]<-mean(motorUPDRS_RMSE_tmp)
    totalUPDRS_RMSE[9]<-mean(totalUPDRS_RMSE_tmp)
  }
  else if (i==0.9){
    motorUPDRS_Y$alpha<-resultsmotor
    totalUPDRS_Y$alpha<-resultstotal
    motorUPDRS_RMSE_tmp[1]<-RMSE(motorUPDRS_Y$motor_UPDRS[1:1175],motorUPDRS_Y$alpha[1:1175])
    totalUPDRS_RMSE_tmp[1]<-RMSE(totalUPDRS_Y$total_UPDRS[1:1175],totalUPDRS_Y$alpha[1:1175])
    motorUPDRS_RMSE_tmp[2]<-RMSE(motorUPDRS_Y$motor_UPDRS[1176:2350],motorUPDRS_Y$alpha[1176:2350])
    totalUPDRS_RMSE_tmp[2]<-RMSE(totalUPDRS_Y$total_UPDRS[1176:2350],totalUPDRS_Y$alpha[1176:2350])
    motorUPDRS_RMSE_tmp[3]<-RMSE(motorUPDRS_Y$motor_UPDRS[2351:3525],motorUPDRS_Y$alpha[2351:3525])
    totalUPDRS_RMSE_tmp[3]<-RMSE(totalUPDRS_Y$total_UPDRS[2351:3525],totalUPDRS_Y$alpha[2351:3525])
    motorUPDRS_RMSE_tmp[4]<-RMSE(motorUPDRS_Y$motor_UPDRS[3526:4700],motorUPDRS_Y$alpha[3526:4700])
    totalUPDRS_RMSE_tmp[4]<-RMSE(totalUPDRS_Y$total_UPDRS[3526:4700],totalUPDRS_Y$alpha[3526:4700])
    motorUPDRS_RMSE_tmp[5]<-RMSE(motorUPDRS_Y$motor_UPDRS[4701:5875],motorUPDRS_Y$alpha[4701:5875])
    totalUPDRS_RMSE_tmp[5]<-RMSE(totalUPDRS_Y$total_UPDRS[4701:5875],totalUPDRS_Y$alpha[4701:5875])
    motorUPDRS_RMSE[10]<-mean(motorUPDRS_RMSE_tmp)
    totalUPDRS_RMSE[10]<-mean(totalUPDRS_RMSE_tmp)
  }
  else if (i==1){
    motorUPDRS_Y$alpha<-resultsmotor
    totalUPDRS_Y$alpha<-resultstotal
    motorUPDRS_RMSE_tmp[1]<-RMSE(motorUPDRS_Y$motor_UPDRS[1:1175],motorUPDRS_Y$alpha[1:1175])
    totalUPDRS_RMSE_tmp[1]<-RMSE(totalUPDRS_Y$total_UPDRS[1:1175],totalUPDRS_Y$alpha[1:1175])
    motorUPDRS_RMSE_tmp[2]<-RMSE(motorUPDRS_Y$motor_UPDRS[1176:2350],motorUPDRS_Y$alpha[1176:2350])
    totalUPDRS_RMSE_tmp[2]<-RMSE(totalUPDRS_Y$total_UPDRS[1176:2350],totalUPDRS_Y$alpha[1176:2350])
    motorUPDRS_RMSE_tmp[3]<-RMSE(motorUPDRS_Y$motor_UPDRS[2351:3525],motorUPDRS_Y$alpha[2351:3525])
    totalUPDRS_RMSE_tmp[3]<-RMSE(totalUPDRS_Y$total_UPDRS[2351:3525],totalUPDRS_Y$alpha[2351:3525])
    motorUPDRS_RMSE_tmp[4]<-RMSE(motorUPDRS_Y$motor_UPDRS[3526:4700],motorUPDRS_Y$alpha[3526:4700])
    totalUPDRS_RMSE_tmp[4]<-RMSE(totalUPDRS_Y$total_UPDRS[3526:4700],totalUPDRS_Y$alpha[3526:4700])
    motorUPDRS_RMSE_tmp[5]<-RMSE(motorUPDRS_Y$motor_UPDRS[4701:5875],motorUPDRS_Y$alpha[4701:5875])
    totalUPDRS_RMSE_tmp[5]<-RMSE(totalUPDRS_Y$total_UPDRS[4701:5875],totalUPDRS_Y$alpha[4701:5875])
    motorUPDRS_RMSE[11]<-mean(motorUPDRS_RMSE_tmp)
    totalUPDRS_RMSE[11]<-mean(totalUPDRS_RMSE_tmp)
  }
}

#compiling results
compiled<-matrix(data=NA,nrow=22,ncol=3)
compiled[1:22]<-rep(seq(0,1,0.1),2)
compiled[1:11,2]<-rep("Motor",11)
compiled[12:22,2]<-rep("Total",11)
compiled[1:11,3]<-motorUPDRS_RMSE
compiled[12:22,3]<-totalUPDRS_RMSE
colnames(compiled)<-c("Alpha","UPDRS","RMSE")
compiled<-as.data.frame(compiled)
compiled[,c(1,3)]<-sapply(compiled[,c(1,3)],as.numeric)
totalonly<-dplyr::filter(compiled,compiled$UPDRS=="Total")
motoronly<-dplyr::filter(compiled,compiled$UPDRS=="Motor")

#making plots
ggplot(compiled,aes(x=Alpha,y=RMSE,fill=UPDRS))+
  geom_col(position="dodge")+facet_wrap(~UPDRS)+
  scale_x_continuous(breaks=c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1))

ggplot(totalonly,aes(x=Alpha,y=RMSE))+geom_point(shape=19,color="blue",size=3)+
  scale_x_continuous(breaks=c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1))+
  labs(title="Total UPDRS Score RMSE")

ggplot(motoronly,aes(x=Alpha,y=RMSE))+geom_point(shape=19,color="blue",size=3)+
  scale_x_continuous(breaks=c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1))+
  labs(title="Motor UPDRS Score RMSE")