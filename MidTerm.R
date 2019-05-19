
#reading input from csv
file<-file.choose()
full.data<-read.csv(file.path(file),header=TRUE)



#counting the number of NA in the columns
sum(is.na(full.data$Employment_Info_1)) #19 #mean
sum(is.na(full.data$Employment_Info_4)) #6779
sum(is.na(full.data$Employment_Info_6)) #10854
sum(is.na(full.data$Medical_History_1)) #8889

#eliminating the below entries because of excessive NA's
sum(is.na(full.data$Insurance_History_5)) #25396
sum(is.na(full.data$Family_Hist_2)) #28656
sum(is.na(full.data$Family_Hist_3)) #34241
sum(is.na(full.data$Family_Hist_4)) #25861
sum(is.na(full.data$Family_Hist_5)) #19184
sum(is.na(full.data$Medical_History_10)) #58824
sum(is.na(full.data$Medical_History_15)) #44596
sum(is.na(full.data$Medical_History_24)) #55580
sum(is.na(full.data$Medical_History_32)) #58274

#amount of NA in columns can be found out by summary as well
summary(full.data)
#determining the correlation between variables using correlation matrix
my_data <- full.data[, c(-3,-1)]
cor_mat<- cor(my_data,method = "pearson", use = "p")
new_cor_mat<- round(cor_mat, 2)
mat_cor<- new_cor_mat[abs(new_cor_mat) < 0.5] <- NA
#Pre-processing of missing values

#mean value for employment_info_1
full.data[is.na(full.data$Employment_Info_1),"Employment_Info_1"] <-boxplot.stats(full.data$Employment_Info_1)$stats[3]


#predicting values for Employment_Info_4 using glm
emp_info4_model<-glm(Employment_Info_4~Employment_Info_1+Employment_Info_2+Employment_Info_3+Employment_Info_5+Family_Hist_1,data = full.data)
#plot(emp_info4_model)
emp_info4<-full.data[is.na(full.data$Employment_Info_4),c("Employment_Info_1","Employment_Info_2","Employment_Info_3","Employment_Info_5","Family_Hist_1")]
info4_predict<-predict(emp_info4_model,newdata = emp_info4)
full.data[is.na(full.data$Employment_Info_4),"Employment_Info_4"] <-info4_predict


#predicting values for Employment_Info_6 using glm #could use splines. non-linear
emp_info6_model<-glm(Employment_Info_6~Employment_Info_1+Employment_Info_3+Employment_Info_4+Family_Hist_1,data = full.data)
#plot(emp_info6_model)
emp_info6<-full.data[is.na(full.data$Employment_Info_6),c("Employment_Info_1","Employment_Info_3","Employment_Info_4","Family_Hist_1")]
info6_predict<-predict(emp_info6_model,newdata = emp_info6)        
full.data[is.na(full.data$Employment_Info_6),"Employment_Info_6"] <-info6_predict 


#predicting values for Medical_History_1
med_his1_model<-glm(Medical_History_1~Medical_History_2+Medical_History_3+Medical_History_6+Medical_History_8+Medical_History_9+Medical_History_12+Medical_History_13   , data = full.data)
med_his1<-full.data[is.na(full.data$Medical_History_1),c("Medical_History_2","Medical_History_3","Medical_History_6","Medical_History_8","Medical_History_9","Medical_History_12","Medical_History_13")]
hist1_predict<-predict(med_his1_model,newdata = med_his1)
full.data[is.na(full.data$Medical_History_1),"Medical_History_1"] <-hist1_predict

#train.set<- train.data[, -c("Product_Info_2","Id","Product_Info_1","Insurance_History_5","Family_Hist_2","Family_Hist_3","Family_Hist_4","Family_Hist_5","Medical_History_10","Medical_History_15","Medical_History_24","Medical_History_32")]
full.set<- subset(full.data, select = -c(Product_Info_2,Product_Info_1,Insurance_History_5,Family_Hist_2,Family_Hist_3,Family_Hist_4,Family_Hist_5,Medical_History_10,Medical_History_15,Medical_History_24,Medical_History_32))

#Normalization of dataset
scale.set<-subset(full.set, select= -c(Response,Medical_History_1,Employment_Info_1,Employment_Info_6,Ins_Age,Ht,Wt,BMI,Product_Info_4))

install.packages("dummies")
library(dummies)
new_my_data <- dummy.data.frame(full.set, names = scale.set)


#sampling using 70-30 distribution
sample_set<-sample(nrow(new_my_data),floor(0.7*nrow(new_my_data)))
train.data<- new_my_data[sample_set,]
test.data<- new_my_data[-sample_set,]

#principal component analysis
pca <- prcomp(train.data, scale. = T)
names(pca)

#biplot(pca, scale = 0)
std_dev <- pca$sdev
pca_var <- std_dev^2
#pca_var[1:10]
prop_varex <- pca_var/sum(pca_var)
#prop_varex[1:20]
plot(prop_varex, xlab = "Principal Component",ylab = "Variance Ratio",type = "b")
plot(cumsum(prop_varex), xlab = "Principal Component",ylab = "Cumulative Variance",type = "b")

data.train <- data.frame(Response = train.data$Response, pca$x)
data.train <- data.train[,1:85]

#Decision Tree
library(rpart)
library(rpart.plot)


library(ggplot2)  
library(XLConnectJars)
library(XLConnect)
library(rpart)

library(party)
library(datasets)

library(rattle)
library(rpart.plot)
library(RColorBrewer)

install.packages("xgboost")
library(xgboost)


rpart.model <- rpart(Response ~ .,data = data.train, method = "anova")
plot(rpart.model)
text(rpart.model,pretty = 0,cex=0.75)

data.test <- predict(pca, newdata = test.data)
data.test <- as.data.frame(data.test)

#select the first 85 components
data.test <- data.test[,1:85]

#make prediction on test data
rpart.prediction <- predict(rpart.model, data.test)

tree_MSE<-mean((rpart.prediction-test.data$Response)^2)
tree_RMSE<-sqrt(tree_MSE)

tree_MSE
tree_RMSE

#rf.model<-randomForest(Response~., data = data.train)

#-------------------------
#Linear Regression
reg_model<- lm(Response ~.,data=data.train)
reg.predict<-predict(reg_model,newdata = data.test)
reg_MSE<-mean((reg.predict-test.data$Response)^2)
reg_RMSE<-sqrt(reg_MSE)
summary(reg_model)
plot(reg_model)

reg_MSE
reg_RMSE
#-----------------------------------
#svm
svm.train <- sample(nrow(data.train),floor(0.01*nrow(data.train)))
m1 <- tune.svm(Response ~ ., data = data.train[svm.train,], gamma = 10^(-4:-1), cost = 10^(-1:1))
m1
s<-svm(Response ~., type="eps-regression",kernel="radial",gamma=0.001,cost=10, data = data.train[svm.train,])
s
#glm_model <- glm(Response ~., type=eps-regression,kernel="radial",gamma=10,cost=1, data = data.train[svm.train,])
# 
# summary(glm_model)
# 
# Real_Values <- test$Response
svm.test <- sample(nrow(data.test),floor(0.01*nrow(data.test)))
svm.predict<- predict(s,data.test[svm.test,])
plot(svm.predict,test.data[svm.test,]$Response)
svm_MSE<-mean((svm.predict-test.data[svm.test,]$Response)^2)
svm_RMSE<-sqrt(svm_MSE)

svm_MSE

svm_RMSE

# predictResponse5 <- predict(s,test)
# predictResponse5_Round <- round(predictResponse5)
# res <- 
# errors5<- ModelErrors(predictResponse5_Round - Real_Values)