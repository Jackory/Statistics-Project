# import the dataset
library(psych)
library(vcd)
data1 <- read.csv('Placement_Data_Full_Class.csv')
describe(data1)
summary(data1)



# paint pictures to describe the data
library(ggplot2)
library(Rmisc)

# 各个离散特征与就业状况的分布图 ---------------------------------------------------------

gender = ggplot(data1, aes(x= gender,fill=status))+geom_bar()+
  theme(axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5))
ssc_b = ggplot(data1, aes(x=ssc_b,fill=status))+geom_bar()+
  theme(axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5))
hsc_b = ggplot(data1, aes(x=hsc_b,fill=status))+geom_bar()+
  theme(axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5))
hsc_s = ggplot(data1, aes(x=hsc_s,fill=status))+geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5))
degree_t = ggplot(data1, aes(x=degree_t,fill=status))+geom_bar()+
  theme(axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5))
workex = ggplot(data1, aes(workex,fill=status))+geom_bar()+
  theme(axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5))
specialisation = ggplot(data1, aes(specialisation,fill=status))+geom_bar()+
  theme(axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5))

multiplot(gender,ssc_b,hsc_b,hsc_s,degree_t,workex,specialisation,cols = 3)



# gender-status双向表卡方检验 --------------------------------------------------------

female_count <- subset(data1, gender == 'F', select=c('status'))
table(female_count)
male_count <- subset(data1, gender == 'M', select=c('status'))
table(male_count)

M1 = as.table(rbind(table(female_count),table(male_count)))
dimnames(M1) <- list(gender = c('F','M'),status = c('Not Placed', 'Placed')) # 修改表的列名
M1
chisq.test(M1)

# ssc_b-status双向表卡方检验 -----------------------------------------------------
sscOthers_count <- subset(data1, ssc_b=='Others', select=c('status'))
sscCentral_count <- subset(data1, ssc_b=='Central', select=c('status'))

M2 = as.table(rbind(table(sscOthers_count),table(sscCentral_count)))
dimnames(M2) <- list(ssc_b = c('Others','Central'),status = c('Not Placed', 'Placed')) # 修改表的列名
M2
chisq.test(M2)

# hsc_b-status双向表卡方检验 -----------------------------------------------------
hscOthers_count <- subset(data1, hsc_b=='Others', select=c('status'))
hscCentral_count <- subset(data1, hsc_b=='Central', select=c('status'))

M3 = as.table(rbind(table(hscOthers_count),table(hscCentral_count)))
dimnames(M3) <- list(hsc_b = c('Others','Central'),status = c('Not Placed', 'Placed')) # 修改表的列名
M3
chisq.test(M3)


# hsc_s-status双向表卡方检验 -----------------------------------------------------
commerce_count <- subset(data1, hsc_s=='Commerce',select=c('status'))
sci_count <- subset(data1,hsc_s=='Science',select=c('status'))
art_count <- subset(data1,hsc_s=='Arts',select=c('status'))

M4 = as.table(rbind(table(commerce_count),table(sci_count),table(art_count)))
dimnames(M4) <- list(hsc_s = c('Commerce','Science','Arts'),status = c('Not Placed', 'Placed')) # 修改表的列名
M4
chisq.test(M4)
# degree_t-status双向表卡方检验 --------------------------------------------------
Sci <- subset(data1, degree_t=="Sci&Tech",select=c('status'))
Comm <- subset(data1,degree_t=="Comm&Mgmt",select=c('status'))
Others <- subset(data1,degree_t=="Others",select=c('status'))

M5 = as.table(rbind(table(Sci),table(Comm),table(Others)))
dimnames(M5) <- list(degree_t = c('Sci&Tech','Comm&Mgmt','Others'),status = c('Not Placed', 'Placed')) # 修改表的列名
M5
chisq.test(M5)
# workex-status双向表卡方检验 ----------------------------------------------------
workex_count0 <- subset(data1, workex == 'No', select=c('status'))
table(workex_count0)
workex_count1 <- subset(data1, workex == 'Yes', select=c('status'))
table(workex_count1)

M6 = as.table(rbind(table(workex_count0),table(workex_count1)))
dimnames(M6) <- list(workex = c('No','Yes'),status = c('Not Placed', 'Placed')) # 修改表的列名
chisq.test(M6)

# specialisation-status双向表卡方检验 --------------------------------------------
specialisation_count0 <- subset(data1, specialisation == 'Mkt&Fin', select=c('status'))
table(specialisation_count0)
specialisation_count1 <- subset(data1, specialisation == 'Mkt&HR', select=c('status'))
table(specialisation_count1)

M7 = as.table(rbind(table(specialisation_count0),table(specialisation_count1)))
dimnames(M7) <- list(specialisation = c('Mkt&Fin','Mkt&HR'),status = c('Not Placed', 'Placed')) # 修改表的列名
M7
chisq.test(M7)


# 类别变量转换为factor----------------------------------------------------------------

data2 <- data1[,2:15]
# gender
table(data2$gender)
data2$gender <- factor(data2$gender,
                       levels = c('M','F'),
                       labels = c(0,1))
data2$ssc_b <- factor(data2$ssc_b,
                     levels = c('Central','Others'),
                     labels = c(0,1))
data2$hsc_b <- factor(data2$hsc_b,
                      levels = c('Central','Others'),
                      labels = c(0,1))
# hsc_s
table(data2$hsc_s)
data2$hsc_s <- factor(data2$hsc_s,
                      levels = c("Arts","Commerce","Science"),
                      labels = c(0,1,2))
table(data2$degree_t)
data2$degree_t <- factor(data2$degree_t,
                         levels = c("Comm&Mgmt","Others","Sci&Tech"),
                         labels = c(0,1,2))
table(data2$workex)
data2$workex <- factor(data2$workex,
                      levels = c("No","Yes"),
                      labels = c(0,1))
table(data2$specialisation)
data2$specialisation <- factor(data2$specialisation,
                              levels = c("Mkt&Fin","Mkt&HR"),
                              labels = c(0,1))
table(data2$status)
data2$status <- factor(data2$status,
                      levels = c("Not Placed","Placed"),
                      labels = c(0,1))

data2$salary[data2$status == 0] <- 0

summary(data2)
describe(data2)


# correlation hotmap ------------------------------------------------------

# factor转化为数值变量，以便用于回归、分类
data2$status = as.numeric(data2$status) - 1
data2$ssc_b = as.numeric(data2$ssc_b) - 1
data2$hsc_b = as.numeric(data2$hsc_b) - 1
data2$hsc_s = as.numeric(data2$hsc_s) - 1
data2$degree_t = as.numeric(data2$degree_t) - 1
data2$specialisation = as.numeric(data2$specialisation) - 1
data2$gender = as.numeric(data2$gender) - 1
data2$workex = as.numeric(data2$workex) - 1

library(corrplot)
m <- cor(data2[,c("ssc_p","hsc_p","degree_p","etest_p","mba_p","status","salary")])
corrplot(m,method = 'shade',tl.col ="black", tl.srt = 45, order = "AOE")

# selcet the key feature --------------------------------------------------

# the first algorithm:Feature selection method based on penalty(Lasso)
library(Matrix)
feature_matrix = as.matrix(data2[,1:12])
target_matrix = data2[,13]
library(glmnet)

r2 <- glmnet(feature_matrix, target_matrix, family = "binomial", alpha = 1)
r2.cv <- cv.glmnet(feature_matrix, 
                   target_matrix, 
                   family = "binomial", 
                   type.measure="auc",
                   alpha = 1, 
                   nfolds = 10)
plot(r2.cv)
r2.min <- glmnet(feature_matrix, 
                 target_matrix, 
                 family = "binomial", 
                 type.measure="auc",
                 alpha = 1, 
                 lambda = r2.cv$lambda.min)
r2.min_coef <- coef(r2.min)
r2.min
r2.min_coef

# the second algorithm: Recursive Feature Elimination
set.seed(7)
library(lattice)
library(ggplot2)
library(caret)
library(randomForest)
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
results <- rfe(data2[,1:12], data2[,13], sizes=c(1:12), rfeControl=control)
predictors(results)
plot(results, type=c("g", "o"))

# 20 folds Cross-validation
control <- rfeControl(functions=rfFuncs, method="cv", number=20)
results <- rfe(data2[,1:12], data2[,13], sizes=c(1:12), rfeControl=control)
print(results)
predictors(results)
plot(results, type=c("g", "o"))


# classification based on status ------------------------------------------

data3 <- data2[,c("gender","ssc_p","hsc_p","degree_p","mba_p",
                  "workex","specialisation","mba_p","status")]
data3$status <- factor(data3$status,levels=c(0,1),
                       labels=c("Placed","Not Placed"))
train0 <- createDataPartition(y=data3$status, p = 0.75, list=FALSE)
train_data1 <- data3[train0,]
train_test1 <- data3[-train0,]
# SVM
library(e1071)
set.seed(1234)
fit.svm <- svm(status~.,data=train_data1,)
fit.svm
svm()
svm.pred <- predict(fit.svm, na.omit(train_test1))
table(na.omit(train_test1)$status,svm.pred,
      dnn = c("Actually","Predicted"))

# randomForest
set.seed(1234)
fit.forest <- randomForest(status~.,data=train_data1,
                           importance=TRUE,proximity=TRUE,ntree=100)
round(importance(fit.forest, type=2))
forest_prediction <- predict(fit.forest, train_test1)
forest_perf <- table(train_test1$status,forest_prediction,
                     dnn = c("Actually","Predicted"))
forest_perf


# regression based on salary ----------------------------------------------

# correlations about salary and regression
library(BBmisc)
data4 <- data2[,c("gender","ssc_p","hsc_p","degree_p","mba_p",
                  "workex","specialisation","mba_p","salary")]

data4 <- data4[which(data4$salary!=0),]
train <- createDataPartition(y=data4$salary, p = 0.75, list=FALSE)
train_data = data4[train,]
test_data = data4[-train,]

# Lasso 
feature_matrix4 = as.matrix(train_data[,-c(9)])
target_matrix4 = train_data$salary
lasso <- glmnet(feature_matrix4, target_matrix4,lambda = 1e-4)
lasso.pred = predict(lasso, s=1e-4,newx = as.matrix(test_data[,-c(9)]))
library(MLmetrics)
MSE(lasso.pred, test_data$salary)

# 随机森林
fit.forest <- randomForest(salary~.,data=train_data,
                           importance=TRUE,proximity=TRUE,ntree=100)

forest_prediction <- predict(fit.forest, test_data)
MSE(forest_prediction,test_data$salary)
