
#reading input from csv
file<-file.choose()
full.data<-read.csv(file.path(file),header=TRUE)
#file<-file.choose()
#test.data<-read.csv(file.path(file),stringsAsFactors = FALSE,header=TRUE)

#adding extra columns to the dataset to distinguihs between train and test data
#test.data$Response<-NA
#train.data$isTrain<-TRUE
#test.data$isTrain<-FALSE

#merging train and test data for a common method of preprocessing
#full.data<-rbind(train.data,test.data)



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

#train_model<-train(Response~.-Insurance_History_5-Family_Hist_2-Family_Hist_3-Family_Hist_4-Family_Hist_5-Medical_History_10-Medical_History_15-Medical_History_24-Medical_History_32,data=train.data,na.action = na.pass)

#sampling using 70-30 distribution
sample_set<-sample(nrow(full.data),floor(0.7*nrow(full.data)))
train.data<- full.data[sample_set,]
test.data<- full.data[-sample_set,]

#train.set<- train.data[, -c("Product_Info_2","Id","Product_Info_1","Insurance_History_5","Family_Hist_2","Family_Hist_3","Family_Hist_4","Family_Hist_5","Medical_History_10","Medical_History_15","Medical_History_24","Medical_History_32")]
train.set<- subset(train.data, select = -c(Product_Info_2,Id,Product_Info_1,Insurance_History_5,Family_Hist_2,Family_Hist_3,Family_Hist_4,Family_Hist_5,Medical_History_10,Medical_History_15,Medical_History_24,Medical_History_32))

train<- glm(Response ~.,data=train.set)


#DECISION TREES
#library(rpart)
#fir<-rpart(Response~.,data = train.set)
#plot(fir)
#text(fir)

#SVM
install.packages("caret")
install.packages("e1071", dependencies = TRUE)
library(caret)
#Load SVM package
library(e1071)

#Read the data set
#dataset <- read.csv('C:/Users/ankit/Desktop/ADS/Mid-Term/trainProcess(drop).csv')

# Making an index for creating the subset of the data set from 1 to n row
index<-1:nrow(train.set)

#Creating a subset with 1% of the data in the data set
sample_index<-sample(index,trunc(length(index)*1/100))
#sample_index = sample(60000,10000)

train_dataset <- train.set[sample_index,]


#To find the most important columnspresent in the data set
rm <- lm(Response ~., data = train_dataset)
summary(rm)

#Train and tune the SVM
m1 <- tune.svm(Response ~ ., data = train_dataset, gamma = 10^(-6:-1), cost = 10^(-1:1))
summary(m1)

#Applying the SVM kernel 
model <- svm(Response ~ Product_Info_4 + Product_Info_6 + Ins_Age + Ht + Wt + BMI + Employment_Info_3
             + Employment_Info_5 + InsuredInfo_2 + InsuredInfo_5 +InsuredInfo_6 + InsuredInfo_7 + Insurance_History_1
             + Insurance_History_2 + Insurance_History_3 + Family_Hist_2 +Family_Hist_3 +Family_Hist_4
             + Family_Hist_5 + Medical_History_3_2 +Medical_History_4 + Medical_History_11_2 + Medical_History_12_2
             + Medical_History_13_1 + Medical_History_17_2 + Medical_History_22 + Medical_History_23_1
             + Medical_History_27_1 + Medical_History_29_1 + Medical_History_30_2 + Medical_History_31_1
             + Medical_History_35_1 + Medical_History_39_1 + Medical_History_40_1 + Medical_Keyword_2
             + Medical_Keyword_3  + Medical_Keyword_6 + Medical_Keyword_9  + Medical_Keyword_15 + Medical_Keyword_19
             + Medical_Keyword_22 + Medical_Keyword_25 + Medical_Keyword_33 + Medical_Keyword_34 + Medical_Keyword_38
             + Medical_Keyword_41 + Medical_Keyword_45 , data = train_dataset, kernel = "radial", gamma = 0.1, cost = 10)
print(model)
summary(model)

#Actual vs Observed value
prediction <- predict(model, test_dataset)
tab <- table(pred = prediction, true = test_dataset[,198])

pr <- round(prediction)

#length(prediction)
#length(pr)
#length(test_dataset$Response)

summary(tab)
summary(prediction)

#Confusion matrix
table(pr, test_dataset$Response)

#Mean square error
svm = predict(model,test_dataset)
MSE <-mean((svm-test_dataset$Response)^2)
MSE












