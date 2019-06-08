sms_raw<-read.csv("sms_spam_ansi.txt",stringsAsFactors = FALSE)
sms_raw
sms_raw$text
str(sms_raw)
sms_raw$type<-factor(sms_raw$type)

table(sms_raw$type)

install.packages("tm")
library(tm)

# 1단계 : 말뭉치(corpus) 생성 -> VCorpus

sms_corpus<-VCorpus(VectorSource(sms_raw$text)) #source 객체로 표현해 주어야 함
inspect(sms_corpus[1:2])

sms_corpus[1]
as.character(sms_corpus[[1]])

# 여러개의 문서를 한번에 타입변환
lapply(sms_corpus[1:2], as.character)

#메세지를 단어로 분리 -> 텍스트 분석
# 점 제거,일괄적으로 동일한 단어로 변형(대소문자 구별된 단어)
# 일괄적으로 소문자로 메세지를 변환

tolower("Hi")
sms_corpus_clean<-tm_map(sms_corpus,content_transformer(tolower))

#숫자제거
sms_corpus_clean<-tm_map(sms_corpus_clean,removeNumbers)

#불용어 제거
stopwords()
sms_corpus_clean<-tm_map(sms_corpus_clean,removeWords, stopwords())

# 구둣점 제거거
sms_corpus_clean<-tm_map(sms_corpus_clean,removePunctuation)

#wordStem 함수 : 어근추출 가능
#snowballC 패키지 안에 들어있음
install.packages("SnowballC")
library(SnowballC)

wordStem(c("learn","learned","learns"))

sms_corpus_clean<-tm_map(sms_corpus_clean,stemDocument)

#추가 여백 제거
sms_corpus_clean<-tm_map(sms_corpus_clean,stripWhitespace)

lapply(sms_corpus_clean[1:10], as.character)

sms_dtm<-DocumentTermMatrix(sms_corpus_clean) #tdm을 만들어주는 함수 

# sms_dtm을 훈련용과 데스트 용으로 분리
sms_dtm_train<-sms_dtm[2:4169,]
sms_dtm_test<-sms_dtm[4170:5559,]

sms_train_labels<-sms_raw[2:4169,]$type
sms_test_labels<-sms_raw[4170:5559,]$type

table(sms_train_labels)
table(sms_test_labels)

prop.table(table(sms_train_labels))

install.packages("wordcloud")
library(wordcloud)

wordcloud(sms_corpus_clean,min.freq=50,random.order =FALSE,colors = brewer.pal(8,"Dark2"))

spam<-subset(sms_raw,type=="spam")
ham<-subset(sms_raw,type=="ham")

wordcloud(spam$text, max.words = 40,scale = c(5,0.5))
wordcloud(ham$text, max.words = 40,scale = c(4,0.5))

#희소행렬을 나이브베이즈 분뷸기를 훈련시키기 위해 변환
sms_freq_words<-findFreqTerms(sms_dtm_train,5)

sms_dtm_freq_train<-sms_dtm_train[,sms_freq_words]
sms_dtm_freq_test<-sms_dtm_test[,sms_freq_words]

#나이브 베이즈 분류기는 범주형 특징으로 된 데이터에 대해 적용

# 빈도수가 0이면 "no" 0보다 크면 "yes"
convert_counts<- function(x){
  x<-ifelse(x>0, "Yes","Np")
}

sms_train<-apply(sms_dtm_freq_train,MARGIN=2,convert_counts)
sms_test<-apply(sms_dtm_freq_test,MARGIN=2,convert_counts)

# 나이브 베이즈 핉터 생성
install.packages("e1071")
library(e1071)

sms_classifier<-naiveBayes(sms_train,sms_train_labels)

#모델평가
sms_test_predict<-predict(sms_classifier,sms_test)

install.packages("gmodels")
library(gmodels)

CrossTable(sms_test_predict,sms_test_labels,dnn=c('predictied','actual'),prop.chisq = FALSE,prop.t=FALSE,prop.r=FALSE)

sms_classifier2<-naiveBayes(sms_train,sms_train_labels,laplace = 1)
sms_test_predict2<-predict(sms_classifier2,sms_test)
CrossTable(sms_test_predict2,sms_test_labels,dnn=c('predictied','actual'),prop.chisq = FALSE,prop.t=FALSE,prop.r=FALSE)








