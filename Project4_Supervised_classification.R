library(tidyverse)
# read speed-dating data
dfTrain_raw <- read_csv("speeddating.csv")
glimpse(dfTrain_raw)
head(dfTrain_raw)
dim(dfTrain_raw)


dfTrain <- dfTrain_raw
# change numer in character type --> numeric type
# change range type variables --> average
cols=1:123
rows=1:8387
for (i in cols){
    if (dim(dfTrain[,i] %>% select_if(is.numeric))[2]==1){   # if numeric
        next  
    }
    else {
        if (grepl("^[-]{0,1}[0-9]{0,}.{0,1}[0-9]{1,}$",dfTrain[1,i])){
          dfTrain[,i] = as.numeric(unlist(dfTrain[,i]))}
        if(grepl("[[]",dfTrain[1,i])){
             for(j in rows){
               if(grepl("[[]",dfTrain[j,i])){
                  strs <-unlist(strsplit(as.character(dfTrain[j,i]), "-"))
                  num1=substr(strs[1],2,nchar(strs[1]))
                  num2=substr(strs[2],1,nchar(strs[2])-1)  
                  dfTrain[j,i]<- (as.numeric(num1)+as.numeric(num2))/2
               }
             }
          dfTrain[,i]<- as.numeric(unlist(dfTrain[,i]))
        }
    }  
}

head(dfTrain)
# (a)	Conduct exploratory data analysis (EDA)
# 1) basic statistics of the numerical data
library(dplyr)
library(pastecs)
options(scipen=100)
options(digits=2)
stat_table <- (dfTrain %>% select_if(is.numeric) %>% stat.desc())
stat_table

train <- dfTrain  %>% select_if(is.numeric)
train$match <- as.factor(train$match)


# look at missings
df_missing <-train %>% mutate_all(is.na) %>% summarise_all(mean) %>% glimpse()
df_missing <- t(df_missing) 

class(df_missing)
dim(df_missing)

zero =0
one =0
ten  =0
twenty =0
more =0

for (i in 1:119){
  if (df_missing[,i]==0){
    zero = zero +1
  }
  else if (df_missing[,i]>0 & df_missing[,i]<=0.01){
    one= one + 1
  }
  else if (df_missing[,i]>0.01 & df_missing[,i]<=0.1){
   ten= ten + 1
  }
  else if (df_missing[,i]>0.1 & df_missing[,i]<=0.2){
    twenty= twenty+ 1
  }
  else if (df_missing[,i]>0.2){
    more = more + 1
  }  
}

zero =zero/119
one = one/119
ten  =ten/119
twenty =twenty/119
more =more/119

# 2) display histogram 
library(ggplot2)
dfTrain %>% select_if(is.numeric) %>% 
  gather() %>%
  ggplot(aes(value)) + 
  facet_wrap(~ key, scales = "free") + 
  geom_histogram()
#hist(dfTrain$revenue, breaks=1000)
#sample the data so we can reduce the number of observations

dfSample <- dfTrain %>% sample_frac(0.1)
head(dfSample)


# 3) histogram of 6 traits
library(ggplot2)
df[df=="?"]<-NA ## change ? values into NA

df$attractive_o<-as.numeric(df$attractive_o) ##change into numeric values
df$sinsere_o<-as.numeric(df$sinsere_o) ##change into numeric values
df$intelligence_o<-as.numeric(df$intelligence_o)
df$funny_o<-as.numeric(df$funny_o)
df$ambitous_o<-as.numeric(df$ambitous_o)
df$shared_interests_o<-as.numeric(df$shared_interests_o)

str(df$attractive_o)
str(df$sinsere_o)
df$decision_o<-as.factor(df$decision_o) #change numeric into category (decision_o: Decision of partner)

#subset of originianal data by sex
df_female<-df[df$gender=='female',]
df_male<-df[df$gender=='male',]

#Attractiveness
ggplot(df, aes(x=df$attractive_o, color=decision_o)) +
geom_histogram(fill='white', binwidth = 0.5)+labs(title="Histogram of Attractiveness rated by partner")+theme(plot.title = element_text(hjust = 0.5))

ggplot(df_female, aes(x=attractive_o, color=decision_o)) +
geom_histogram(fill='white', binwidth = 0.5)+labs(title="Histogram of Attractiveness rated by male partner")+theme(plot.title = element_text(hjust = 0.5))

ggplot(df_male, aes(x=attractive_o, color=decision_o)) +
geom_histogram(fill='white', binwidth = 0.5)+labs(title="Histogram of Attractiveness rated by female partner")+theme(plot.title = element_text(hjust = 0.5))

#Sincerity
ggplot(df, aes(x=sinsere_o, color=decision_o)) +
geom_histogram(fill='white', binwidth = 0.5)+labs(title="Histogram of sincerity rated by partner")+theme(plot.title = element_text(hjust = 0.5))

ggplot(df_female, aes(x=sinsere_o, color=decision_o)) +
geom_histogram(fill='white', binwidth = 0.5)+labs(title="Histogram of sincerity rated by male partner")+theme(plot.title = element_text(hjust = 0.5))

ggplot(df_male, aes(x=sinsere_o, color=decision_o)) +
geom_histogram(fill='white', binwidth = 0.5)+labs(title="Histogram of sincerity rated by female partner")+theme(plot.title = element_text(hjust = 0.5))


#Intelligence
ggplot(df, aes(x=intelligence_o, color=decision_o)) +
geom_histogram(fill='white', binwidth = 0.5)+labs(title="Histogram of intelligence rated by partner")+theme(plot.title = element_text(hjust = 0.5))

ggplot(df_female, aes(x=intelligence_o, color=decision_o)) +
geom_histogram(fill='white', binwidth = 0.5)+labs(title="Histogram of intelligence rated by male partner")+theme(plot.title = element_text(hjust = 0.5))

ggplot(df_male, aes(x=intelligence_o, color=decision_o)) +
geom_histogram(fill='white', binwidth = 0.5)+labs(title="Histogram of intelligence rated by female partner")+theme(plot.title = element_text(hjust = 0.5))


#Being funny
ggplot(df, aes(x=funny_o, color=decision_o)) +
geom_histogram(fill='white', binwidth = 0.5)+labs(title="Histogram of being funny rated by partner")+theme(plot.title = element_text(hjust = 0.5))


ggplot(df_female, aes(x=funny_o, color=decision_o)) +
geom_histogram(fill='white', binwidth = 0.5)+labs(title="Histogram of being funny rated by male partner")+theme(plot.title = element_text(hjust = 0.5))


ggplot(df_male, aes(x=funny_o, color=decision_o)) +
geom_histogram(fill='white', binwidth = 0.5)+labs(title="Histogram of being funny rated by female partner")+theme(plot.title = element_text(hjust = 0.5))



#Ambition
ggplot(df, aes(x=ambitous_o, color=decision_o)) +
geom_histogram(fill='white', binwidth = 0.5)+labs(title="Histogram of ambition rated by partner")+theme(plot.title = element_text(hjust = 0.5))

ggplot(df_female, aes(x=ambitous_o, color=decision_o)) +
geom_histogram(fill='white', binwidth = 0.5)+labs(title="Histogram of ambition rated by male partner")+theme(plot.title = element_text(hjust = 0.5))

ggplot(df_male, aes(x=ambitous_o, color=decision_o)) +
geom_histogram(fill='white', binwidth = 0.5)+labs(title="Histogram of ambition rated by female partner")+theme(plot.title = element_text(hjust = 0.5))


#Shared interest
ggplot(df, aes(x=shared_interests_o, color=decision_o)) +
geom_histogram(fill='white', binwidth = 0.5)+labs(title="Histogram of shared interest rated by partner")+theme(plot.title = element_text(hjust = 0.5))

ggplot(df_female, aes(x=shared_interests_o, color=decision_o)) +
geom_histogram(fill='white', binwidth = 0.5)+labs(title="Histogram of shared interest rated by partner")+theme(plot.title = element_text(hjust = 0.5))

ggplot(df_male, aes(x=shared_interests_o, color=decision_o)) +
geom_histogram(fill='white', binwidth = 0.5)+labs(title="Histogram of shared interest rated by partner")+theme(plot.title = element_text(hjust = 0.5))




install.packages("processanimateR")
library(processanimateR)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(tidyr)
library(cluster)    #provides more cluster algorithms than base R (e.g. PAM)
library(useful)     #provides a plot function for clusters and "FitKMeans" and "PlotHartigan" functions
library(NbClust)    #provides tons of tools for identifying the "right" number of clusters
library(rgl)        #for 3D rotating plots
library(gridExtra)
library(cluster)  #for Kmedoids
library(factoextra)  #for Kmedoids


#data loading
df=read.csv("speeddating.csv")
df[df=="?"]<-NA ## change ? values into NA
head(df)
str(df)
summary(df)

df1 <- mutate_all(df, function(x) as.numeric(as.character(x))) #convert factor into num except for charactor
head(df1)
df2<-df1[, colSums(is.na(df1))<800]  #remove chacter columns (get numeric variables)
head(df2)

na_count <-sapply(df2, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count) # check NA count
na_count

df3<-df2[complete.cases(df2),-c(1:35,53:57)] #17features and 3types of classification
head(df3)

df4 <-df3[,-c(18:20)] #remove final decisions, last three columns
head(df4)
# Still too many features.. I'll only select

df4Scaled <-scale(df4)
head(df4Scaled)

#hiearchial clustering
di <- dist(df4Scaledsamp, method="euclidean")
hc <- hclust(di, method="ward")
plot(hc, labels=FALSE)
rect.hclust(hc, k=4, border="red")
df4ScaledsampHC <-data.frame(df4Scaledsamp)
df4ScaledsampHC$hcluster <- as.factor(cutree(hc, k=4))   #cutting at k=4
head(df4ScaledsampHC)

#boxplots for Hiearchial clustering
test <- ggplot(df4ScaledsampHC, aes(x = hcluster, y = yoga)) +
geom_boxplot() + theme_bw()

test2 <- ggplot(df4ScaledsampHC, aes(x = hcluster, y = gaming)) +
geom_boxplot() + theme_bw()

test3 <- ggplot(df4ScaledsampHC, aes(x = hcluster, y = reading)) +
geom_boxplot() + theme_bw()

grid.arrange(test, test2, test3,nrow=1)


test <- ggplot(df4ScaledsampHC, aes(x = hcluster, y = sports)) +
geom_boxplot() + theme_bw()

test2 <- ggplot(df4ScaledsampHC, aes(x = hcluster, y = tvsports)) +
geom_boxplot() + theme_bw()

test3 <- ggplot(df4ScaledsampHC, aes(x = hcluster, y = exercise)) +
geom_boxplot() + theme_bw()

grid.arrange(test, test2, test3,nrow=1)


test <- ggplot(df4ScaledsampHC, aes(x = hcluster, y =  dining )) +
geom_boxplot() + theme_bw()

test2 <- ggplot(df4ScaledsampHC, aes(x = hcluster, y = museums)) +
geom_boxplot() + theme_bw()

test3 <- ggplot(df4ScaledsampHC, aes(x = hcluster, y = art)) +
geom_boxplot() + theme_bw()

grid.arrange(test, test2, test3,nrow=1)

test <- ggplot(df4ScaledsampHC, aes(x = hcluster, y =  hiking)) +
geom_boxplot() + theme_bw()

test2 <- ggplot(df4ScaledsampHC, aes(x = hcluster, y =  clubbing )) +
geom_boxplot() + theme_bw()

test3 <- ggplot(df4ScaledsampHC, aes(x = hcluster, y =  tv )) +
geom_boxplot() + theme_bw()

grid.arrange(test, test2, test3,nrow=1)


test <- ggplot(df4ScaledsampHC, aes(x = hcluster, y =  theater)) +
geom_boxplot() + theme_bw()

test2 <- ggplot(df4ScaledsampHC, aes(x = hcluster, y =   movies )) +
geom_boxplot() + theme_bw()

test3 <- ggplot(df4ScaledsampHC, aes(x = hcluster, y =  concerts )) +
geom_boxplot() + theme_bw()

grid.arrange(test, test2, test3,nrow=1)


test <- ggplot(df4ScaledsampHC, aes(x = hcluster, y =   music )) +
geom_boxplot() + theme_bw()

test2 <- ggplot(df4ScaledsampHC, aes(x = hcluster, y =    shopping  )) +
geom_boxplot() + theme_bw()

grid.arrange(test, test2,nrow=1)

#compare the decision_o variable data with custers
#merge HCcluster data with decison_o

head(df4ScaledsampHC)
merge <- merge(df4ScaledsampHC, df3$decision_o, by="row.names", all=FALSE)
head(merge)
count(filter(merge, hcluster==1&y==0)) #result=112 #False=unselected by a partner
count(filter(merge, hcluster==1&y==1)) #result=82  #True=slected
count(filter(merge, hcluster==2&y==0)) #result=69
count(filter(merge, hcluster==2&y==1)) #result=45
count(filter(merge, hcluster==3&y==0)) #result=136
count(filter(merge, hcluster==3&y==1)) #result=94
count(filter(merge, hcluster==4&y==0)) #result=123
count(filter(merge, hcluster==4&y==1)) #result=87

df4ScaledsampHC$decision_o <-as.factor()







grid.arrange(p1,p2,p3,p4,p5, nrow=2)



# imputation of missing value
library(mice)
impute_cart <- mice(train, m=5, maxit = 10, method = 'cart', seed = 500)
train_imputed <- complete(impute_cart)

# write back-up train file
write.csv(train, file = "train_numeric.csv")
write.csv(train_imputed , file = "train_imputed.csv")
#(-c("decision","decision_o","met","match"))
train <- train_imputed %>% select(-c("decision","decision_o","met"))
names(train)[names(train) == "decision_o"] <- "match"
glimpse(train)

train$match <- as.factor(train$match)

train <- train %>% sample_frac(0.1)
folds <- cut(seq(1,nrow(train)),breaks=5,labels=FALSE)

train$met

#============================ Random forest ==============================#
library(rpart)         # CART algorithm
library(rattle)        # for graphics
library(adabag)        # for boosting
library(ipred)         # for bagging and error estimation
library(randomForest)  # for Random Forests
library(caret)         # for training and modeling

library(ggplot2)
library(mlbench)
library(pROC)

#Perform 5fold cross validation
np = 1
accuracy_ <- array(1:np, dim=c(np))
kappa_ <- array(1:np, dim=c(np))
sensitivity_ <- array(1:np, dim=c(np))
specificity_ <- array(1:np, dim=c(np))
auc_ <- array(1:np, dim=c(np))
pos_pred_=0 <- array(1:np, dim=c(np))

accuracy_=0
kappa_=0
sensitivity_=0
specificity_=0
auc_=0
pos_pred_=0

for (k in 1:np){
  for(i in 1:5){
    #Segement your data by fold using the which() function 
    testIndexes <- which(folds==i,arr.ind=TRUE)
    testData <- train[testIndexes, ]
    trainData <- train[-testIndexes, ]
    
    trainData<-trainData[complete.cases(trainData),]      
    fit_RF1 <- randomForest(match ~ ., data = trainData, importance = TRUE, ntrees=2000, mtry=9)
    fit_RF1
    

    pred_RF1 = predict(fit_RF1, newdata=testData)
    pred_RF1 = factor(pred_RF1, levels = levels(testData$match))
    CM <- confusionMatrix(pred_RF1, testData$match)
    accuracy_[k] = accuracy_[k]+CM$overall[1]
    print(accuracy_[k])
    kappa_[k] = kappa_[k] + CM$overall[2]
    sensitivity_[k] = sensitivity_[k]+CM$byClass[1]
    specificity_[k] = specificity_[k]+CM$byClass[2]
    pos_pred_[k] = pos_pred_[k]+CM$byClass[3]
    
    roc_RF1 <- roc(as.numeric(testData$match),
                   as.numeric(pred_RF1))
    
    auc_[k] = auc_[k]+auc(roc_RF1)
    
  }
}
accuracy_=accuracy_/5
kappa_=kappa_/5
sensitivity_=sensitivity_/5
specificity_=specificity_/5
auc_=auc_/5
pos_pred_=pos_pred_/5

print(accuracy_)
print(kappa_)
print(sensitivity_)
print(specificity_)
print(auc_)
print(pos_pred_)
plot.roc(as.numeric(testData$match),as.numeric(pred_RF1))
library(InformationValue)
ks_plot(as.numeric(testData$match),as.numeric(pred_RF1))
detach("package:InformationValue", unload=TRUE) 

fit_RF1$importanceSD
varImpPlot(fit_RF1)


#============================ boosting ==============================#
np=1
accuracy_=0
kappa_=0
sensitivity_=0
specificity_=0
pos_pred_=0
auc_=0


for (k in 1:np){
  for(i in 1:5){
    #Segement your data by fold using the which() function 
    print(i)
    testIndexes <- which(folds==i,arr.ind=TRUE)
    testData <- train[testIndexes, ]
    trainData <- train[-testIndexes, ]
    
    trainData<-trainData[complete.cases(trainData),]
    fit_boost<-boosting(readmitted ~ ., data = trainData, mfinal = 1)    
    fit_boost
    
    pred_boost = predict(fit_boost, newdata=testData)
    pred_boost = factor(pred_boost, levels = levels(testData$readmitted))
    CM <- confusionMatrix(pred_boost, testData$readmitted)
    accuracy_[k] = accuracy_[k]+CM$overall[1]
    kappa_[k] = kappa_[k] + CM$overall[2]
    sensitivity_[k] = sensitivity_[k]+CM$byClass[1]
    specificity_[k] = specificity_[k]+CM$byClass[2]
    pos_pred_[k] = pos_pred_[k]+CM$byClass[3]
    
    roc_boost <- roc(as.numeric(testDatat$readmitted),
                     as.numeric(pred_boost))
    
    auc_[k] = auc_[k]+auc(roc_boost)
    
    
  }
}
accuracy_=accuracy_/5
kappa_=kappa_/5
sensitivity_=sensitivity_/5
specificity_=specificity_/5
auc_=auc_/5
Dstat_=Dstat_/5

print(accuracy_)
print(kappa_)
print(sensitivity_)
print(specificity_)
print(pos_pred_)
print(auc_)
plot.roc(as.numeric(testData$readmitted),as.numeric(pred_boost))
#library(InformationValue)
#ks_plot(as.numeric(testData$readmitted),as.numeric(pred_boost))
#detach("package:InformationValue", unload=TRUE) 


#============================ boosting2 ==============================#
library('doParallel')
cl <- makeCluster(4) #number of cores
registerDoParallel(cl)

accuracy_=0
kappa_=0
sensitivity_=0
specificity_=0
auc_=0
pos_pred_=0
ctrl <- trainControl(method="repeatedcv", number=5,   # 5 fold cross validation
                     repeats=1)

fit_ada <- train(match ~ .,
                 data=train,
                 method = "AdaBoost.M1",   # Radial kernel
                 preProc = c("center","scale"),  # Center and scale data
                 tuneLength = 1,	                 
                 metric='Accuracy',
                 savePredictions =TRUE,
                 trControl=ctrl)


print(fit_ada)
stopCluster(cl)


pred_boost = predict(fit_ada, newdata=testData)
pred_boost = factor(pred_boost, levels = levels(testData$match))
CM <- confusionMatrix(pred_boost, testData$match)
accuracy_[k] = accuracy_[k]+CM$overall[1]
kappa_[k] = kappa_[k] + CM$overall[2]
sensitivity_[k] = sensitivity_[k]+CM$byClass[1]
specificity_[k] = specificity_[k]+CM$byClass[2]
pos_pred_[k] = pos_pred_[k]+CM$byClass[3]

roc_boost <- plot.roc(as.numeric(testData$match),
                      as.numeric(pred_boost))

auc_[k] = auc_[k]+auc(roc_boost)



print(accuracy_)
print(kappa_)
print(sensitivity_)
print(specificity_)
print(pos_pred_)
print(auc_)
plot.roc(as.numeric(testData$match),as.numeric(pred_boost))
library(InformationValue)
ks_plot(as.numeric(testData$match),as.numeric(pred_boost))
detach("package:InformationValue", unload=TRUE) 


#============================ bagging ==============================#

np=1
accuracy_=0
kappa_=0
sensitivity_=0
specificity_=0
auc_=0
pos_pred_=0

for (k in 1:np){
  for(i in 1:5){
    print (i)
    #Segement your data by fold using the which() function 
    testIndexes <- which(folds==i,arr.ind=TRUE)
    testData <- train[testIndexes, ]
    trainData <- train[-testIndexes, ]
    
    fit_bagging<-bagging(match ~ ., data = trainData, coob = T,nbagg=10)
    fit_bagging
    
    pred_bagging = predict(fit_bagging, newdata=testData)
    pred_bagging = factor(pred_bagging, levels = levels(testData$match))
    CM <- confusionMatrix(pred_bagging, testData$match)
    accuracy_[k] = accuracy_[k]+CM$overall[1]
    kappa_[k] = kappa_[k] + CM$overall[2]
    sensitivity_[k] = sensitivity_[k]+CM$byClass[1]
    specificity_[k] = specificity_[k]+CM$byClass[2]
    pos_pred_[k] = pos_pred_[k]+CM$byClass[3]   
    roc_bagging <- roc(as.numeric(testData$match),
                       as.numeric(pred_bagging))
    
    auc_[k] = auc_[k]+auc(roc_bagging)
    
  }
}
accuracy_=accuracy_/5
kappa_=kappa_/5
sensitivity_=sensitivity_/5
specificity_=specificity_/5
auc_=auc_/5
pos_pred_=pos_pred_/5

print(accuracy_)
print(kappa_)
print(sensitivity_)
print(specificity_)
print(auc_)
print(pos_pred_)
plot.roc(as.numeric(testData$match),as.numeric(pred_bagging))
library(InformationValue)
ks_plot(as.numeric(testData$match),as.numeric(pred_bagging))
detach("package:InformationValue", unload=TRUE) 





#=====================Logistic Regression===========================#

#fit1 <- glm(data=training, match ~.,  family="binomial")        #Target=match
#fit1 <- glm(data=training, decision ~.,  family="binomial")      #Target=decision
fit1 <- glm(data=training, decision_o ~.,  family="binomial")   #Target=decision_o

df2$decision_o
summary(fit1)
#anova(fit1, test="Chisq")

#testing_tr <- testing %>% select(-c('match'))
#testing_tr <- testing %>% select(-c('decision'))
testing_tr <- testing %>% select(-c('decision_o'))

p1 <- predict(fit1,newdata=testing_tr,type='response')
p1 <- ifelse(p1 > 0.5,1,0)
p1
str(testing)
testing$decision_o
#misClasificError1 <- mean(p1 != testing$match)
#misClasificError1 <- mean(p1 != testing$decision)
misClasificError1 <- mean(p1 != testing$decision_o)
misClasificError1
print(paste('Accuracy1',1-misClasificError1))


# Confusion matrix
p1<-as.factor(p1)
head(p1)

#testing$match<-as.factor(testing$match)
#testing$decision<-as.factor(testing$decision)
testing$decision_o<-as.factor(testing$decision_o)

#confusionMatrix(p1, testing$match)
#confusionMatrix(p1, testing$decision)
confusionMatrix(p1, testing$decision_o)

#Performance evalution_KS statistics
install.packages("InformationValue")
library(InformationValue)
#ks_stat(testing$match, p1)  #Target=match
#ks_stat(testing$match,p1, returnKSTable = T)
#ks_plot(testing$match, p1) #KS Chart

#ks_stat(testing$decision, p1)   #Target=decision
#ks_stat(testing$decision,p1, returnKSTable = T)
#ks_plot(testing$decision, p1) #KS Chart

ks_stat(testing$decision_o, p1)   #Target=decision_o
ks_stat(testing$decision_o,p1, returnKSTable = T)
ks_plot(testing$decision_o, p1) #KS Chart

detach("package:InformationValue", unload=TRUE)


##tryout
# ROC and AUROC
library(pROC)

#ROC curve
#plot.roc(as.numeric(p1),as.numeric(testing$match))   #Target=match
#AUROC
#roc1 <- roc(as.numeric(p1),as.numeric(testing$match))
#auc(roc1)


#ROC curve
#plot.roc(as.numeric(p1),as.numeric(testing$decision))   #Target=decision
#AUROC
#roc1 <- roc(as.numeric(p1),as.numeric(testing$decision))
#auc(roc1)

#ROC curve
plot.roc(as.numeric(p1),as.numeric(testing$decision_o))   #Target=decision_o
#AUROC
roc1 <- roc(as.numeric(p1),as.numeric(testing$decision_o))
auc(roc1)

#=================================  SVM  =============================#

library(e1071)
library(dplyr)

df_imp=read.csv("speeddating_imputed.csv")

#cross validation K=5 fold CV
K<-5
#k<-1
#k<-2
#k<-3
#k<-4
k<-5

df1 <-df_imp #entire columns(imputed file)
df2=sample_n(df1,4000)

#df2 <- df1 %>% select(-c("has_null","wave","met", "decision","decision_o" ))  #target='match'
#df2 <- df1 %>% select(-c("has_null","wave","met","decision_o", "match" )) #target='decision'
df2 <- df2 %>% select(-c("has_null","wave","met", "decision","match" )) #target='decision_o'

fold.size<-nrow(df2)/K
testing.index <-(k-1)*fold.size+1:fold.size

training<- df2[-testing.index,]
testing<-df2[testing.index,]
testing


fit3=svm(decision_o~.,data=training, probability=FALSE,kernel='radial', cost=1)
#fit3=svm(decision~.,data=training, probability=FALSE,kernel='radial', cost=1)
#fit3=svm(match~.,data=training, probability=FALSE,kernel='radial', cost=1)

head(fit3)
#tuned<-tune(svm, decision_o~., data=training, probability=TRUE,  kernel='linear',ranges=list(cost=c(0.001, 0.01, 0.1, 1, 10, 100)))
#summary(tuned)  # cost=100 shows the best result and update fit2

#testing_tr <- testing %>% select(-c('match'))
#testing_tr <- testing %>% select(-c('decision'))
testing_tr <- testing %>% select(-c('decision_o'))


p3 <- predict(fit3, newdata=testing_tr,probability=FALSE, type='class')
head(p3)
p3 <- ifelse(p3 > 0.5,1,0)
mean(p3==testing$decision_o)
#mean(p3==testing$decision)
#mean(p3==testing$match)

# Confusion matrix
library(lattice)
library(caret)
str(p3)

p3<-as.factor(p3)
testing$decision_o<-as.factor(testing$decision_o)
#testing$decision<-as.factor(testing$decision)
#testing$match<-as.factor(testing$match)

confusionMatrix(testing$decision_o, p3)
#confusionMatrix(testing$decision, p3)
#confusionMatrix(testing$match, p3)

# ROC and AUROC
library(pROC)

#ROC curve
plot.roc(as.numeric(p3),as.numeric(testing$decision_o))
#plot.roc(as.numeric(p3),as.numeric(testing$decision))
#plot.roc(as.numeric(p3),as.numeric(testing$match))

#AUROC
roc2 <- roc(as.numeric(p3),as.numeric(testing$decision_o))
#roc2 <- roc(as.numeric(p3),as.numeric(testing$decision))
#roc2 <- roc(as.numeric(p3),as.numeric(testing$match))

auc(roc2)

#Performance evalution_KS statistics
install.packages("InformationValue")
library(InformationValue)
ks_stat(testing$dicision_o,p3)
ks_stat(testing$decision_o,p3, returnKSTable = T)
ks_plot(testing$decision_o, p3) #KS Chart

#ks_stat(testing$dicision,p3)
#ks_stat(testing$decision,p3, returnKSTable = T)
#ks_plot(testing$decision, p3) #KS Chart

#ks_stat(testing$match,p3)
#ks_stat(testing$match,p3, returnKSTable = T)
#ks_plot(testing$match, p3) #KS Chart

detach("package:InformationValue", unload=TRUE)








