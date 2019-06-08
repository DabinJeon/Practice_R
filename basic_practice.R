# #이상치 제거, R시각화, 
# install.packages("rJava") 
# install.packages("memoise") 
# install.packages("KoNLP") 
# #Sys.setenv(JAVA_HOME="C:/Program Files/Java/jdk-12") 
# Sys.getenv("JAVA_HOME")
# # 패키지 로드
# library(KoNLP) 
# #에러발생시 => https://www.java.com/en/download/manual.jsp
# ## Checking user defined dictionary! 
# library(dplyr) 
# useNIADic()

#이상치 정제
#존재할 수 없는 값(잘못입력):성별 변수값을 -5 => 결측 처리
#극단적인 값 몸무게 300kg => 정상범위를 정해서 처리

#이상치 제거
outlier<-data.frame(sex=c(1,2,1,3,2,1),
                    score=c(5,4,3,4,2,6))
outlier

#이상치 확인
table(outlier$sex)

table(outlier$score)

#결측처리(성별 3 -> NA)
outlier$sex<-ifelse(outlier$sex==3,NA,outlier$sex)
outlier

#score가 5점이 넘으면 NA
outlier$score<-ifelse(outlier$score>5,NA,outlier$score)
outlier

library(dplyr)

#결측치 제외하고 분석
outlier %>% 
  filter(!is.na(sex) & !is.na(score)) %>% 
  group_by(sex) %>% 
  summarise(mean_score=mean(score))

#sex컬럼으로 그룹화 -> 각 그룹별 score 평균
#is.na(outlier$sex) => FALSE FALSE FALSE  TRUE FALSE FALSE


#2. 극단적인 값에 대한 이상치 처리
#정상범위를 벗어난 경우 => 결측 처리
#판단 기준-논리적 판단기준(성인 40~150kg 벗어나면 이상치)
#판단 기준-통계적 판단기준
#(상하위 0.3%극단치 or boxplot에서 1.5 IQR 벗어난 경우)

library(ggplot2)
str(mpg)

boxplot(mpg$hwy)

boxplot(mpg$hwy)$stats
#stats결과에서 [1]번과 [5]번 값이 극단치 경계값
#[1],[5]:하위/상위 극단치 경계,[2]:1사분위수
#[3]:중위수, [4]:3사분위수

mpg$hwy<-ifelse(mpg$hwy<12 | mpg$hwy>37, NA, mpg$hwy)
table(is.na(mpg$hwy)) #이상치가 총 3개 검색됨
#library(help = "stats")

#이상치를 제외한 나머지 데이터에서 drv에 따라 
#그룹화 -> hwy에 대한 평균
mpg %>% 
  group_by(drv) %>% 
  summarise(mean_hwy=mean(hwy,na.rm=T))

mpg[c(10,14,58,93), "drv"]<-"k" #이상치 할당
mpg[c(29,43,129,203), "cty"]<-c(3,4,39,42) #이상치 할당
#boxplot(mpg$cty)$stats#극단치 경계: <9, 26<

#요구사항:구동방식별로 도시 연비가 어떻게 다른지 분석?
#1. drv에 이상치 확인. 이상치 있으면 결측치 처리 -> 이상치가
#제거됐는지 확인.(%in% 사용)

#이상치 확인
table(mpg$drv)
#이상치는 NA처리
mpg$drv<-ifelse(mpg$drv %in% c("4","f","r"), mpg$drv, NA)
#이상치 확인
table(mpg$drv)

#2. boxplot을 사용하여 cty컬럼에 이상치가 있는지 확인하세요
boxplot(mpg$cty)$stats
mpg$cty<-ifelse(mpg$cty<9 | mpg$cty>26, NA, mpg$cty)
boxplot(mpg$cty)

#is.na(mpg$drv)
#is.na(mpg$cty)
#이상치를 제외한 다음 drv별로 cty평균을 구해보세요
#dplyr구문(ctrl + shift + m)으로 작성할 것
mpg %>% 
  filter(!is.na(drv) & !is.na(cty)) %>% 
  group_by(drv) %>% 
  summarise(mean_hwy=mean(cty))
  
#선그래프(시계열:환율,주가지수...)
economics
str(economics)

ggplot(data=economics, aes(x=date, y=psavert))+geom_line()

ggplot(data=mpg, aes(x=drv, y=hwy))+geom_boxplot()
#drv에 따른 hwy 출력

mpg$class
table(mpg$class)
#compact, subcompact, suv 자동차에 대한 cty연비 비교

class_mpg<-mpg %>% 
  filter(class %in% c('compact', 'subcompact', 'suv'))

ggplot(data=class_mpg, aes(x=class, y=cty))+geom_boxplot()

#geom_함수
#함수:point(), bar(), line(), boxplot(), col()


#r데이터분석 프로젝트

install.packages("foreign")
library(foreign)#SAS, SPSS 데이터셋을 읽을 때 사용
library(dplyr)
library(ggplot2)
install.packages("readxl")
library(readxl)

raw<-read.spss(file="2015_data.sav", to.data.frame=T)
str(raw)
#2006~2015년도, 만 가구 경제활동 추적 조사
#경제활동, 생활, ... 정보 -> 분석

#1. 성별에 따른 급여 차이
# 1) 변수확인-class함수, table함수
# 2) 전처리(이상치 등 처리)
# 3) 시각화-> 비교
# 
# #2. 나이와 월급의 관계
# 1) 변수확인-class함수, table함수
# 2) 전처리(이상치 등 처리)
# 3) 시각화-> 비교


head(raw)
str(raw)
View(raw)

raw<-rename(raw, 
            sex=h10_g3, #성별
            birth=h10_g4, #태어난 연도
            marriage=h10_g10, #혼인 여부
            religion=h10_g11, #종교
            income=p1002_8aq1, #월급여
            code_job=h10_eco9, #직종 코드
            code_region=h10_reg7) #지역 코드

#전처리 -> 요약표 작성 -> 그래프 작성

#성별 변수 확인 및 전처리
class(raw$sex) #"numeric"
table(raw$sex) #1,2로 구성

#만약 성별값이 9가 있었다고 가정하고 이상치 결측 처리
raw$sex<-ifelse(raw$sex==9, NA, raw$sex)

#결측치 확인
table(is.na(raw$sex))

#성별 항목 이름 : 1->male, 2 -> female
raw$sex<-ifelse(raw$sex==1, "male","female")
table(raw$sex)
qplot(raw$sex)


#월급 변수 확인, 전처리
class(raw$income)#numeric
summary(raw$income)#기술통계
qplot(raw$income)

qplot(raw$income)+xlim(0,1000)

boxplot(raw$income)
boxplot(raw$income)$stats
#608만원 초과시 이상치로 간주하고 전처리

#이상치 결측치 처리
raw$income<-ifelse(raw$income %in% c(0,9999),NA,raw$income)
#급여 0원이거나 9999마원은 결측치

#10 %in% c(10,20) #TRUE
#15 %in% c(10,20) #FALSE

table(is.na(raw$income))
#급여가 0원 또는 9999만원이 12044건 ->NaN처리->분석 제외

#성별 급여 평균표 작성

sex_income<-raw %>% 
  filter(!is.na(income)) %>% 
  group_by(sex) %>% 
  summarise(mean_income=mean(income))
sex_income
# sex   mean_income
# female   190
# male     220
  
ggplot(data=sex_income, aes(x=sex, y=mean_income))+geom_col()


#어느 나이에 가장 많은 급여 수령
class(raw$birth)#"numeric"
summary(raw$birth)#연속형 변수
qplot(raw$birth)
table(is.na(raw$birth))

#유도변수(파생변수):feature engineering => 변수->처리->파생변수
raw$age<-2015-raw$birth+1
summary(raw$age)
qplot(raw$age)

#x축:age, y축:income 시각화
#각 나이에 대한 월급에 대한 평균을 시각화
age_income<-raw %>% 
  filter(!is.na(income)) %>% 
  group_by(age) %>% 
  summarise(mean_income =mean(income))

ggplot(data=age_income, aes(x=age, y=mean_income))+geom_line()

#"young","middle", "old"
raw<-raw %>% 
  mutate(ageg=ifelse(age<30, "young",
         ifelse(age<=59,"middle","old")))

table(raw$ageg)
qplot(raw$ageg)

#ageg 값을 바탕으로 연령대별 급여 평균
ageg_income<-raw %>% 
  filter(!is.na(income)) %>% 
  group_by(ageg) %>% 
  summarise(mean_income=mean(income))
ageg_income

ggplot(data=ageg_income, aes(x=ageg,y=mean_income))+
  geom_col()+
  scale_x_discrete(limits=c("young","middle","old"))


#연령대 별 성별에 따른 월급여 차이 분석
sex_income<-raw %>% 
  filter(!is.na(income)) %>% 
  group_by(ageg, sex) %>% 
  summarise(mean_income=mean(income))
#1차 그룹화 기준 : ageg
#2차 그룹화 기준 : sex
sex_income

ggplot(data=sex_income, aes(x=ageg, y=mean_income, fill=sex))+
  geom_col()+
  scale_x_discrete(limits=c("young","middle","old"))


ggplot(data=sex_income, aes(x=ageg, y=mean_income, fill=sex))+
  geom_col(position="dodge")+
  scale_x_discrete(limits=c("young","middle","old"))


#선 그래프로 나이,성별 급여 차이
sex_age<-raw %>% 
  filter(!is.na(income)) %>% 
  group_by(age, sex) %>% 
  summarise(mean_income=mean(income))


ggplot(data=sex_age, aes(x=age, y=mean_income, col=sex))+
  geom_line()

raw$code_job
table(raw$code_job)

list_job<-read_excel("Koweps_Codebook.xlsx",col_names = T, sheet=2)
list_job
dim(list_job)
str(raw)
str(list_job)

#code_job을 기준으로 raw와 list_job을 조인
raw<-left_join(raw, list_job, id="code_job")
str(raw)
#raw$job
#raw$code_job


raw %>% 
  filter(!is.na(code_job)) %>% 
  select(code_job, job) %>% 
  head(20)


#상위 10개의 직업군에 대한 평균 급여를 시각화

job_income<-raw %>% 
  filter(!is.na(job) & !is.na(income)) %>% 
  group_by(job) %>% 
  summarise(mean_income=mean(income))

job_income

top10<-job_income %>% 
  arrange(desc(mean_income)) %>% 
  head(10)
top10

ggplot(data=top10, aes(x=reorder(job,mean_income),y=mean_income))+
  geom_col()+
  coord_flip()



#남성 직업 빈도 상위 10개를 추출(내림차순)
job_male<-raw %>% 
  filter(!is.na(job) & sex=="male") %>% 
  group_by(job) %>% 
  summarise(count=n()) %>% 
  arrange(desc(count)) %>% 
  head(10)


# job       count
# 영업       100

job_female<-raw %>% 
  filter(!is.na(job) & sex=="female") %>% 
  group_by(job) %>% 
  summarise(count=n()) %>% 
  arrange(desc(count)) %>% 
  head(10)
job_female





















