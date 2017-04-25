

data.train <- read.table(file="/home/.../.../main_table_sokrash_train.csv",header = TRUE ,dec = ",", sep=";", quote = '\t', encoding = 'UTF-8')

data.control <- read.table(file="/home/.../.../main_table_sokrash_control.csv",header = TRUE , dec = ",",sep=";", quote = '\t', encoding = 'UTF-8')

data.train$XAR9<- NULL
data.control$XAR9<- NULL
data.train$XAR13<- NULL
data.control$XAR13<- NULL
data.train$XAR22<- NULL
data.control$XAR22<- NULL
data.train$XAR4<- NULL
data.control$XAR4<- NULL
data.train$XAR5<- NULL
data.control$XAR5<- NULL
data.train$XAR22<- NULL
data.control$XAR22<- NULL
data.train$FNN<- NULL
data.control$FNN<- NULL
data.train$XAR3<- NULL
data.control$XAR3<- NULL
data.train$XAR25<- NULL
data.control$XAR25<- NULL

"готовый вариант для ЛДА"
library(MASS)
data.lda <- lda(data.frame(data.train[,10:13],XAR44 = data.train[,15] ), data.train[,1])
data.ldap <- predict(data.lda,data.frame(data.control[,10:13],XAR44 = data.control[,15])
data.ldap$class 
tb_lda <- data.frame(ldap= data.ldap$class, control = data.control[,1])
tb2<-table(data.ldap$class)
write.table(file= "/home/.../rezult_lda.csv", tb_lda)
misclass <- function(pred, obs){
   tbl <-table(pred, obs)
   sum<- colSums(tbl)
   dia<-diag(tbl)
   msc<-(sum-dia)/sum*100
   m.m <-mean(msc)
   cat("table:", "\n")
   print(tbl)
   cat("errors", "\n")
   print(round(msc,1))
   }
misclass(data.ldap$class,data.control[,1])
                     
"готовый вариант для деревьев классификации"
library(tree)
library(DMwR)
library(rpart)
library(party) 
data.tree <- rpart(KL~. , data =  data.train, method = "class")
prettyTree(data.tree, compress = )
plot(data.tree)
text(data.tree)
data.treep<-  predict(data.tree, data.control)
write.table(file= "/home/.../rezult_for_class.csv", data.treep)
                     
'если работать только с качественными признаками'
(ctree.a1 <- ctree(KL ~ ., data = data.train ))
plot(ctree.a1 )
                     
                     
                     
'готовый вариант для РАНДОМ ФОРЕСТ деревьев классификации'
library(randomForest)
data.RFtree <- randomForest(data.train[,1] ~ ., data.train[,2:7])
data.RFtreep<-  predict(data.RFtree, data.control[,2:7])
write.table(file= "/home/.../rezult_for_classRF.csv", data.RFtreep)
                     
                     
'готовый вариант для SVM'
data.svm<-svm(KL~. , data =  data.train)
data.svmp<-  predict(data.svm, data.control)
write.table(file= "/home/.../rezult_for_SVM.csv", data.svmp)
plot(data.RFtree)
text(data.RFtree)
data.RFtreep<-predict(data.RFtree, data.control[,3:10])
                     