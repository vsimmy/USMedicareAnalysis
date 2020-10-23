library("dplyr")
library("corrplot")
library("ROSE")
library("smotefamily")
library("ggplot2")
library("cowplot")
library("class")
library("glmnet")
library("leaps")
library("ROCR")
library("tree")
library("randomForest")
library("pls")
library("e1071")
library("gbm")
library("rpart")
library("rpart.plot")
library("caret")


#Function for plotting roc curves
#Set working directory and import datasets

MP_train<-read.csv("Medicare_Provider_PartB.csv")
MP_test<-read.csv("Medicare_Provider_Eval_PartB.csv")
MB_train<-read.csv("Medicare_Outpatient_Inpatient_Beneficiary_PartB.csv")
MB_test<-read.csv("Medicare_Outpatient_Inpatient_Beneficiary_Eval_PartB.csv")


#Create columns for age, claim type binary, claim duration and hospital duration
MB_train$age<-ifelse(is.na(MB_train$DOD),
                     difftime(strptime("2020-6-30", format = "%Y-%m-%d"),as.Date(as.character(MB_train$DOB), format = "%Y-%m-%d"), units = "weeks")/52.25,
                     difftime(as.Date(as.character(MB_train$DOD), format = "%Y-%m-%d"),as.Date(as.character(MB_train$DOB), format = "%Y-%m-%d"), units = "weeks")/52.25)
MB_train$ClaimType_N<-ifelse(MB_train$ClaimType=="Outpatient",2,1)
MB_train$RenalDiseaseIndicator<-ifelse(MB_train$RenalDiseaseIndicator=="Y",1,2)
MB_train$ClaimDur<-as.numeric(as.Date(MB_train$ClaimEndDt)-as.Date(MB_train$ClaimStartDt))
MB_train$InpatDur<-as.numeric(as.Date(MB_train$DischargeDt)-as.Date(MB_train$AdmissionDt))

MB_test$age<-ifelse(is.na(MB_test$DOD),
                     difftime(strptime("2020-6-30", format = "%Y-%m-%d"),as.Date(as.character(MB_test$DOB), format = "%Y-%m-%d"), units = "weeks")/52.25,
                     difftime(as.Date(as.character(MB_test$DOD), format = "%Y-%m-%d"),as.Date(as.character(MB_test$DOB), format = "%Y-%m-%d"), units = "weeks")/52.25)
MB_test$ClaimType_N<-ifelse(MB_test$ClaimType=="Outpatient",2,1)
MB_test$RenalDiseaseIndicator<-ifelse(MB_test$RenalDiseaseIndicator=="Y",1,2)
MB_test$ClaimDur<-as.numeric(as.Date(MB_test$ClaimEndDt)-as.Date(MB_test$ClaimStartDt))
MB_test$InpatDur<-as.numeric(as.Date(MB_test$DischargeDt)-as.Date(MB_test$AdmissionDt))


#Merge
MC<-merge(MP_train,MB_train, by ="ProviderID")
MC_eval<-merge(MP_test,MB_test, by="ProviderID")
str(MC)

####Data Cleaning####

#Impute data by deleting
MC[is.na(MC)]<-0
MC_eval[is.na(MC_eval)]<-0
#Omit columns for all zero-columns
MC<-MC[,colSums(MC!=0)>0]
MC_eval<-MC_eval[,colSums(MC_eval!=0)>0]

#Balance data
#% of fraudulent providers in training set
mean(MP_train$Fraud=="Yes")
sum(MP_train$Fraud=="Yes")
#9.017% or 400 fraudulent providers in training set

####Data Aggregation Per Provider data ####

MC_count<-MC[,c(1,4)] #no. of claims
MC_money<-MC[,c(1,7,13)]  #IP or OP annual will overlap per provider hence not counted
MC_Phys_Code<-MC[,c(1,8:11,15:29)] #number of physicians and each diagnosis code
MC_times<-MC[,c(1,37,38,55,57,58)]  #age and duration
MC_bin<-MC[,c(1,39:49,56)] #Chronic Conditions and claim type
MC_demo<-MC[,c(1,32,33,35,36)] # demographic factors

#Time to aggregate!
MC_count<-aggregate(.~ProviderID, MC_count, length)
MC_money<-aggregate(.~ProviderID, MC_money, sum)
MC_Phys_Code<-aggregate(.~ProviderID,MC_Phys_Code, function(x) sum(x!=0))
MC_times<-aggregate(.~ProviderID,MC_times, mean)
MC_bin1<-aggregate(.~ProviderID, MC_bin,function(x) sum(x!=2)) #for having the condition and for inpatient
MC_outpatient<-aggregate(.~ProviderID,MC_bin[,c(1,13)], function(x) sum(x!=1)) #for outpatient
MC_demo<-aggregate(.~ProviderID,MC_demo, mean)
MC_demo<-cbind(MC_demo$ProviderID,round(MC_demo[,-1],0))
colnames(MC_demo)[1]<-"ProviderID"
#Merge for aggregate data per provide
MC_pro_all<-merge(MP_train,MC_money,by = "ProviderID")
MC_pro_all<-merge(MC_pro_all,MC_count,by = "ProviderID")
MC_pro_all<-merge(MC_pro_all,MC_Phys_Code,by = "ProviderID")
MC_pro_all<-merge(MC_pro_all,MC_times,by = "ProviderID")
MC_pro_all<-merge(MC_pro_all,MC_bin1,by="ProviderID")
MC_pro_all<-merge(MC_pro_all,MC_outpatient,by="ProviderID")
MC_pro_all<-merge(MC_pro_all,MC_demo,by="ProviderID")
MC_pro_all$Fraud<-ifelse(MC_pro_all$Fraud=="Yes",1,0)
View(MC_pro_all)
colnames(MC_pro_all)[grep("ClaimType_N.x",colnames(MC_pro_all))]<-"No_of_inpat"
colnames(MC_pro_all)[grep("ClaimType_N.y",colnames(MC_pro_all))]<-"No_of_outpat"


#Correlation matrix to visualize factors material for Fraud
C<-cor(MC_pro_all[,-1])
corrplot::corrplot(C,method = "circle",tl.cex = 0.5)
##can omit from ClmProcedureCode4 to InpatDur

MC_pro_all<-MC_pro_all[,-c(24:29,43:46)]
#Balance data

MC_pro_bal<-BLSMOTE(MC_pro_all[,-1],MC_pro_all$Fraud)$data[,-36]
View(MC_pro_bal)

###Same for testing data set###
eval_MC_count<-MC_eval[,c(1,3)]
eval_MC_money<-MC_eval[,c(1,6,12)]  #IP or OP annual will overlap per provider hence not counted
eval_MC_Phys_Code<-MC_eval[,c(1,7:10,14:28)] #number of physicians and each diagnosis code
eval_MC_times<-MC_eval[,c(1,36,37,54,56,57)]  #age and duration
eval_MC_bin<-MC_eval[,c(1,38:48,55)] #Chronic Conditions and claim type
eval_MC_demo<-MC_eval[,c(1,31,32,34,35)] # demographic factors

#Time to aggregate!
eval_MC_count<-aggregate(.~ProviderID, eval_MC_count, length)
eval_MC_money<-aggregate(.~ProviderID, eval_MC_money, sum)
eval_MC_Phys_Code<-aggregate(.~ProviderID,eval_MC_Phys_Code, function(x) sum(x!=0))
eval_MC_times<-aggregate(.~ProviderID,eval_MC_times, mean)
eval_MC_bin1<-aggregate(.~ProviderID, eval_MC_bin,function(x) sum(x!=2)) #for having the condition and for inpatient
eval_MC_outpatient<-aggregate(.~ProviderID,eval_MC_bin[,c(1,13)], function(x) sum(x!=1)) #for outpatient
eval_MC_demo<-aggregate(.~ProviderID,eval_MC_demo, mean)
eval_MC_demo<-cbind(eval_MC_demo$ProviderID,round(eval_MC_demo[,-1],0))
colnames(eval_MC_demo)[1]<-"ProviderID"
#Merge for aggregate data per provide
eval_MC_pro_all<-merge(MP_test,eval_MC_money,by = "ProviderID")
eval_MC_pro_all<-merge(eval_MC_pro_all,eval_MC_count,by = "ProviderID")
eval_MC_pro_all<-merge(eval_MC_pro_all,eval_MC_Phys_Code,by = "ProviderID")
eval_MC_pro_all<-merge(eval_MC_pro_all,eval_MC_times,by = "ProviderID")
eval_MC_pro_all<-merge(eval_MC_pro_all,eval_MC_bin1,by="ProviderID")
eval_MC_pro_all<-merge(eval_MC_pro_all,eval_MC_outpatient,by="ProviderID")
eval_MC_pro_all<-merge(eval_MC_pro_all,eval_MC_demo,by="ProviderID")
colnames(eval_MC_pro_all)[grep("ClaimType_N.x",colnames(eval_MC_pro_all))]<-"No_of_inpat"
colnames(eval_MC_pro_all)[grep("ClaimType_N.y",colnames(eval_MC_pro_all))]<-"No_of_outpat"

View(eval_MC_pro_all)
eval_MC_pro_all<-eval_MC_pro_all[,-c(23:28,42:45)]

#Setup for validation set
set.seed(1)
s_train<-sample(1:nrow(MC_pro_bal),nrow(MC_pro_bal)/2)
s_test<-(-s_train)
y_test<-MC_pro_bal[s_test,1]

####Choosing factors####

#Fitting full model for 38 factors will be too computationally expensive
#Based on factors that have shown 

LR_fit<-glm(Fraud~.,MC_pro_bal,family = "binomial")
LR_sum<-summary(LR_fit)
LR_sum$coefficients
##choose factors with less than 5% significant value
MC_pro_LR<-MC_pro_bal[,-c(3,10:12,23,24,29,31)] #cancer is close 5% but will test further
eval_MC_pro_LR<-eval_MC_pro_all[,-c(3,10:12,23,24,29,31)]
####Logistic Regression####
#Choose best subset
LR_refit<-glm(Fraud~.,MC_pro_LR, family = "binomial")
reLR_summary<-summary(LR_refit)
reLR_summary$coefficients
#validation
LR_valid<-glm(Fraud~.,MC_pro_LR, subset=s_train, family = "binomial")
LR_valid_pred<-rep(0,length(s_test))
LR_valid_prob<-predict(LR_valid,MC_pro_LR[s_test,],type = "response")
LR_valid_pred[LR_valid_prob>0.5]<-1
mean(LR_valid_pred==y_test)
#89.55% accurate


thres<-0.5
LR_prob<-predict(LR_refit,newdata = MC_pro_bal[s_test,],type = "response")
LR_pred<-rep(0,dim(MC_pro_bal)[1])
LR_pred[LR_prob>thres]<-1

LR_df<-data.frame(LR_prob,LR_pred)
LR_predict<-prediction(LR_prob,MC_pro_bal[s_test,1])
perf<-performance(LR_predict,"tpr","fpr")
LR_auc<-performance(LR_predict,"auc")
LR_auc@y.values

###LR result###
LR_pred_r<-predict(LR_refit, eval_MC_pro_LR,type = "response")
LR_pred_r<-sort(LR_pred_r,decreasing = TRUE)
length(which(LR_pred_r>0.5))

#208 fraudulent providers by threshold of 0.5
LR_result<-eval_MC_pro_all[rownames(data.frame(LR_pred_r[1:350])),1]

####Ridge####
#set up
mm<-model.matrix(Fraud~.,MC_pro_bal)[,-1]
grid<-10^seq(10,-2,length=100)

#Fit 
ridge_fit<-glmnet(mm[s_train,],MC_pro_LR$Fraud[s_train],alpha = 0,lambda = grid, family = "binomial")

#validate
cv_ridge<-cv.glmnet(mm[s_train,],MC_pro_LR$Fraud[s_train],alpha=0,family="binomial")
plot(cv_ridge)
plot(cv_ridge$lambda,cv_ridge$cvm,ylab = "mean CV error",xlab="Lambda", title("Ridge CV error by lambda"))
ridge_pred_new<-predict(ridge_fit,s=cv_ridge$lambda.min,newx = mm[s_test,],type = "class")
cv_ridge$lambda.min
mean(ridge_pred_new==y_test)
ridge_predict<-prediction(predict(ridge_fit,s=cv_ridge$lambda.min,newx = mm[s_test,],type = "response"),MC_pro_bal[s_test,1])
ridge_perf<-performance(ridge_predict,"tpr","fpr")
ridge_auc<-performance(ridge_predict,"auc")
ridge_auc@y.values

#86.48% 

####Lasso####
lasso_fit<-glmnet(mm,MC_pro_LR$Fraud,alpha = 1,lambda = grid, family = "binomial")
#Validate
cv_lasso<-cv.glmnet(mm[s_train,],MC_pro_LR$Fraud[s_train],alpha=1,family="binomial")
plot(cv_lasso$lambda,cv_lasso$cvm, ylab = "mean CV error",xlab="Lambda",title("Lasso CV error by lambda"))
lasso_pred_new<-predict(lasso_fit,s=cv_lasso$lambda.min,newx = mm[s_test,],type = "class")
cv_lasso$lambda.min
mean(lasso_pred_new==y_test)
#88.47% fraudulent providers and performs better

lasso_predict<-prediction(predict(lasso_fit,s=cv_lasso$lambda.min,newx = mm[s_test,],type = "response"),MC_pro_bal[s_test,1])
lasso_perf<-performance(lasso_predict,"tpr","fpr")
lasso_auc<-performance(lasso_predict,"auc")
lasso_auc@y.values


####ridge and lasso result###
mm_test<-model.matrix(~.,eval_MC_pro_all[,-1])[,-1]
ridge_pred_r<-predict(ridge_fit,s=cv_ridge$lambda.min,newx=mm_test,type = "response")
ridge_pred_r_sort<-sort(ridge_pred_r,decreasing = TRUE)
ridge_result<-eval_MC_pro_all[rownames(data.frame(ridge_pred_r_sort[1:350])),1]

lasso_pred_r<-predict(lasso_fit,s=cv_lasso$lambda.min,newx=mm_test,type = "response")
lasso_pred_r_sort<-sort(lasso_pred_r,decreasing = TRUE)
lasso_result<-eval_MC_pro_all[rownames(data.frame(lasso_pred_r_sort[1:350])),1]




####Regression ROC curve####
#Cross validation results for each
CV_reg<-data.frame(1-mean(LR_valid_pred==y_test),
                   1-mean(ridge_pred_new==y_test),
                   1-mean(lasso_pred_new==y_test)
                   )
colnames(CV_reg)<-c("Logistic","Ridge","Lasso")
rownames(CV_reg)<-"CV error"
View(CV_reg)

#overlay ROC curve using plot

plot(perf,col="red",main="Regression ROC curve comparison",lwd=2)
plot(ridge_perf,col="blue",add=T,lwd=2)
plot(lasso_perf,col="dark green",add=T, lwd=2)
legend(0.8,0.5,legend = c("Logistic","Ridge","Lasso"),col=c("red","blue","dark green"),lwd=2,cex=1)

####Principle Component Analysis####
MC_pro_bal$Fraud<-as.numeric(MC_pro_bal$Fraud)
pcr_fit<-pcr(Fraud~.,data=MC_pro_bal,subset=s_train,scale=TRUE,validation="CV")
summary(pcr_fit)


pcr_pred<-predict(pcr_fit,MC_pro_bal[s_test,],ncomp = 25)
pcr_predict<-prediction(as.numeric(pcr_pred),as.numeric(y_test))
pcr_perf<-performance(pcr_predict,"tpr","fpr")
pcr_auc<-performance(pcr_predict,"auc")
pcr_test<-rep(0,length(y_test))
pcr_test[pcr_pred>0.5]<-1
sum(pcr_test==1)
sum(y_test==1)
#1651 fraudulent providers out of 1946 in test data 

#Partial Least Square

pls_fit<-plsr(Fraud~.,data=MC_pro_bal,subset=s_train,scale=TRUE,validation="CV")
summary(pls_fit)

pls_pred<-predict(pls_fit,MC_pro_bal[s_test,],ncomp = 15,type = "response")
pls_predict<-prediction(as.numeric(pls_pred),as.numeric(y_test))
pls_perf<-performance(pls_predict,"tpr","fpr")
pls_auc<-performance(pls_predict,"auc")


pls_test<-rep(0,length(y_test))
pls_test[pls_pred>0.5]<-1
mean(pls_test==y_test)
#83.87% of 1946, not much improvement

#Validation plot for PCR and PLS
validationplot(pcr_fit,val.type = "MSEP",main="CV error: PCA vs PLS", col = "red")
validationplot(pls_fit,val.type = "MSEP", main="Fraud PLS",add=T,col="dark green", lwd=1)
legend(22,0.22,legend = c("PCA","PLS"),col=c("red","dark green"), lwd =1, cex = 1)


#####KNN#####
#Error rate for increasing k
max_k<-25
knn_output<-matrix(nrow=max_k,ncol=1)
for(i in 1:max_k){
  knn_MC<-knn(MC_pro_bal[s_train,],MC_pro_bal[s_test,],cl = MC_pro_bal[s_train,1],k=i)
  knn_output[i,1]<-mean(MC_pro_bal[s_test,1]!=knn_MC)
}

knn_output<-data.frame(knn_output)
colnames(knn_output)<-c("Error rate for increasing k")
View(knn_output)
ggplot(knn_output,
       aes(x=seq(1:max_k), y=`Error rate for increasing k`))+geom_point()+xlab("k")+ggtitle("Error rate given k")
#Optimal seems to be k=3 
#Using LOOCV
knn_loocv<-matrix(nrow=max_k,ncol=1)
for(i in 1:max_k){
  knn_t<-knn.cv(MC_pro_bal,cl=MC_pro_bal[,1],k=i)
  knn_loocv[i,]<-mean(knn_t==1)
}
knn_loocv<-data.frame(knn_loocv)
colnames(knn_loocv)<-"CV error for increasing k"
View(knn_loocv)

ggplot(knn_loocv,
       aes(x=seq(1:max_k), y=`CV error for increasing k`))+geom_point()+xlab("k")+ggtitle("CV error given k")
#With k=3, 5.198% CV error is resaonable 

#KNN ROC
knn_pred<-knn(MC_pro_bal[s_train,],MC_pro_bal[s_test,],cl = MC_pro_bal[s_train,1],k=3,prob = T)
knn_prob<-attr(knn_pred,"prob")
knn_prob<-2*ifelse(knn_pred=="-1",1-knn_prob,knn_prob)-1
pred_knn<-prediction(knn_prob,y_test)
knn_perf<-performance(pred_knn,"tpr","fpr")
plot(knn_perf,avg="threshold",color=T,lwd=3,main="KNN ROC curve")
auc_knn<-performance(pred_knn,"auc")
print(auc_knn@y.values)


####KNN result###
knn_test<-knn(MC_pro_bal[,-1],eval_MC_pro_all[,-1],cl=MC_pro_bal[,1],k=3,prob = TRUE)
knn_test_df<-data.frame(seq(1,length(knn_test)),attributes(knn_test)$prob,knn_test)
colnames(knn_test_df)<-c("Provider index","Probability of outcome","Fraud or not")

sum(knn_test_df$`Fraud or not`==1)
#190 fraudulent providers



####classification trees####

tree_fit<-rpart(Fraud~.,MC_pro_bal,subset=s_train,cp=0.01)
rpart.plot(tree_fit,box.palette = "RdBu",nn=T)

summary(tree_fit)
plotcp(tree_fit)
#no need to prune since lowest error for highest tree size
tree_pred<-rpart.predict(tree_fit,MC_pro_bal[s_test,])
tree_predict<-prediction(tree_pred,MC_pro_bal[s_test,1])
#Tree ROC
tree_perf<-performance(tree_predict,"tpr","fpr")
auc_tree<-performance(tree_predict,"auc")


#Predict
tree_test<-predict(tree_fit,newdata = eval_MC_pro_all[,-1])

tree_result<-eval_MC_pro_all[tree_test>0.5,1]
length(which(tree_test>0.5))
#283 fraudulent providers 



####Random Forest####
MC_pro_bal$Fraud<-as.factor(MC_pro_bal$Fraud)
ranFor_fit<-randomForest(Fraud~.,MC_pro_bal[s_train,])
ranFor_prob<-predict(ranFor_fit,MC_pro_bal[s_test,],type = "prob")

classes<-levels(as.factor(y_test))

ranFor_perf<-performance(prediction(ranFor_prob,as.factor(y_test)),"tpr","fpr")
plot(ranFor_fit,main="Random Forest Fit")
summary(ranFor_fit)
ranFor_pred<-predict(ranFor_fit,MC_pro_bal[s_test,],type = "class")
mean(ranFor_pred==y_test)
#94.56% accurate
which.min(ranFor_fit$err.rate[,1])
which.min(ranFor_fit$err.rate[,2])
which.min(ranFor_fit$err.rate[,3])


#Prediction
ranFor_pred<-predict(ranFor_fit,eval_MC_pro_all[,-1],type = "class")
ranFor_result<-eval_MC_pro_all[ranFor_pred==1,1]
length(ranFor_result)


####Bagging####
bag_ranfor_fit<-randomForest(Fraud~.,MC_pro_bal,subset = s_train, mtry=6,importance=T)
importance(bag_ranfor_fit)
varImpPlot(bag_ranfor_fit,main = "Bagged Random Forest")
bag_ranFor_pred<-predict(bag_ranfor_fit,MC_pro_bal[s_test,],type = "prob")
mean(bag_ranFor_pred==y_test)


####Boosting####
MC_pro_bal$Fraud<-as.factor(MC_pro_bal$Fraud)
boost_fit<-gbm(Fraud~.,MC_pro_bal[s_train,],distribution = "gaussian",n.trees = 1000,interaction.depth =4)
boost_sum<-summary(boost_fit)
boost_pred<-predict(boost_fit,newdata = MC_pro_bal[s_test,],n.trees = 1000,type = "response")
mean(boost_pred>0.5)
boost_fit_test<-gbm(Fraud~.,MC_pro_bal,distribution = "gaussian",n.trees = 1000,interaction.depth =4)
boost_test<-predict(boost_fit_test,newdata = eval_MC_pro_all,n.trees = 1000)

boost_df<-data.frame(seq(1:974),boost_test)

####Prediction for ProviderID using Boosted Trees####


boost_ordered<-boost_df[order(-boost_df[,2],boost_df[,1]),]
colnames(boost_ordered)<-c("Provider index","Fraud")
eval_result<-eval_MC_pro_all[boost_ordered$`Provider index`[1:350],1]
write.csv(eval_result,file = "ProviderID",row.names=FALSE)

##plot RF ROC
for(i in 1:2){
  t<-ifelse(as.factor(y_test)==classes[i],1,0)
  pred<-prediction(ranFor_prob[,i],t)
  perf<-performance(pred,"tpr","fpr")
  plot(perf,main="RF ROC Curve",col="red")
  auc_rf<-performance(pred,measure = "auc")
  print(auc_rf@y.values)
}

##plot Bagged RF ROC
for(i in 1:2){
  t<-ifelse(as.factor(y_test)==classes[i],1,0)
  pred<-prediction(bag_ranFor_pred[,i],t)
  perf<-performance(pred,"tpr","fpr")
  plot(perf,add=T,col="blue")
  auc_btree<-performance(pred,measure = "auc")
  print(auc_btree@y.values)
}

##plot Boosting ROC
boost_predict<-prediction(boost_pred,as.factor(y_test))
boost_perf<-performance(boost_predict,"tpr","fpr")
plot(boost_perf,add=T,col="green")
legend(0.6,0.4,legend = c("Random Forest","Bagging","Boosting"),col = c("red","blue","green"),lwd=1)
auc_boost<-performance(boost_predict,measure="auc")
auc_boost@y.values

#AUC for all models

AUC<-data.frame(Models=c("Logistic","Ridge","Lasso","KNN","PLS","PCR","Tree","RF","BTree","Boost"),
                AUC=as.numeric(c(LR_auc@y.values,ridge_auc@y.values,lasso_auc@y.values,auc_knn@y.values,pls_auc@y.values,pcr_auc@y.values,auc_tree@y.values,auc_rf@y.values,auc_btree@y.values,auc_boost@y.values)))

View(AUC)


