
teens<-read.csv("snsdata.csv")
str(teens)

table(teens$gender) #NA는 제외

table(teens$gender, useNA = "ifany")

summary(teens)


summary(teens$age)
#고등학생 : 13세이상~20세미만
teens$age<-ifelse(teens$age>=13 & teens$age<20, teens$age, NA)
summary(teens$age)

#성별이 "F"이면서 성별이 "NA"가 아니라면
teens$female<-ifelse(teens$gender=="F" & !is.na(teens$gender), 1, 0)
table(teens$female)#여성인지 아닌지에 대한 구분 가능
# 0(남성+NA)     1(여성) 
# 7946          22054 
teens$no_gender<-ifelse(is.na(teens$gender),1,0)
table(teens$no_gender) #NA 2724


table(teens$gender, useNA = "ifany")

table(teens$female)

table(teens$no_gender)

mean(teens$age)

mean(teens$age,na.rm=TRUE) #17.25243

#각 졸업년도에 대한 age의 평균
aggregate(data=teens,age~gradyear, mean ,na.rm=TRUE)
#grdyear 컬럼으로 그룹핑 -> 각 그룹에 대한 age 값의 평균

#grdyear 별로 age에 대한 평균
ave_age<-ave(teens$age, teens$gradyear, FUN=function(x) mean(x,na.rm=TRUE))
ave_age #18.65586, ..., 

teens$age<-ifelse(is.na(teens$age), ave_age, teens$age)
teens$age

summary(teens$age)

library(stats) #클러스터링 관련 함수가 있는 패키지

str(teens)
interests<-teens[5:40]

#표준화작업
#z점수표준화(sclae) : 특징을 평균:0, 표준편차:1
interests_z<-as.data.frame(lapply(interests,scale))#interest데이터에 scale함수 적용
#lapply:행렬(리스트로 리턴 -> 데이터프레임 변환)
set.seed(2345) #난수설정
#kmeans(interests_z)
teen_clusters<-kmeans(interests_z,5) #5개의 클러스터 생성
teen_clusters

#데이터 차원:36차원(basketball,...,drugs)
#36차원 공간에 점(데이터)이 3만개있음.
#점들을 5개로 그룹핑
#각 그룹마다 centroid존재(5개)

#(3,5)
#(0.4815795, 0.44, ...., 0.39 ):36차원 공간에서 각 
#그룹의 평균에 대한 좌표

teen_clusters$centers #중심점

#모든 신문사 -> 신문기사 스크래핑(10년)
#->정치/사회 -> 형태소 분리 -> 자주사용된 단어
#좌/우/중도
#조선왕조실록

teens$cluster<- teen_clusters$cluster

teen_clusters$cluster

teens[1:5,c("cluster", "gender", "age", "friends")]

aggregate(data=teens, age~cluster ,mean)

#각 그룹에 속하는 여성 비율
teens$female
aggregate(data=teens, female~cluster ,mean)

#각 그룹에 대한 친구들의 평균 출력
aggregate(data=teens, friends~cluster ,mean)
