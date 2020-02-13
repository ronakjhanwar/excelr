sms_data<-read.csv(file.choose())
class(sms_data)
str(sms_data)
table(sms_data$type)
library(tm)
sms_corpus<-Corpus(VectorSource(sms_data$text))
sms_corpus$content[1:10]
options(warn=-1)
corpus_clean<-tm_map(sms_corpus,tolower)
corpus_clean<-tm_map(corpus_clean,removeNumbers)
corpus_clean<-tm_map(corpus_clean,removeWords,stopwords())
corpus_clean<-tm_map(corpus_clean,removePunctuation)
#removenumpunct<- function(x) gsub("[^[:alpha:][:space:]]*","",x)


#corpus_clean<-tm_map(corpus_clean,content_transformer,(removenumpunct))

corpus_clean<-tm_map(corpus_clean,stripWhitespace)
class(corpus_clean)
corpus_clean$content[1:10]
sms_dtm<-DocumentTermMatrix(corpus_clean)
class(sms_dtm)
sms_raw_train<-sms_data[1:4169,]
sms_raw_test<-sms_data[4170:5559,]
sms_dtm_train<-sms_dtm[1:4169,]
sms_dtm_test<-sms_dtm[4170:5559,]
sms_corpus_train<-corpus_clean[1:4169]
sms_corpus_test<-corpus_clean[4170:5559]
sms_dict<-findFreqTerms(sms_dtm_train,3)
list(sms_dict[1:100])
sms_train<-DocumentTermMatrix(sms_corpus_train,list(dictionary=sms_dict))
sms_test<-DocumentTermMatrix(sms_corpus_test,list(dictionary=sms_dict))
convert_counts<-function(x){
  x<-ifelse(x>0,1,0)
  x<-factor(x,levels=c(0,1),labels=c("no","yes"))
}
sms_train<-apply(sms_train,MARGIN = 2,convert_counts)
sms_test<-apply(sms_test,MARGIN = 2,convert_counts)
library(e1071)
sms_classifier<-naiveBayes(sms_train,sms_raw_train$type)
sms_classifier$levels
sms_test_pred<-predict(sms_classifier,sms_test)
sms_test_pred[1:25]
confusion1<-table(sms_test_pred,sms_raw_test$type)
confusion1
Acc1<-sum(diag(confusion1))/sum(confusion1)
Acc1

sms_classifier2<-naiveBayes(sms_train,sms_raw_train$type,laplace=1)
sms_classifier2$levels
sms_test_pred2<-predict(sms_classifier2,sms_test)
sms_test_pred2[1:25]
confusion2<-table(sms_test_pred2,sms_raw_test$type)
Acc2<-sum(diag(confusion2))/sum(confusion2)
Acc2
