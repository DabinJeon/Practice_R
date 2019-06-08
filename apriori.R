# 1. 거래 데이터 특징 파악
# 2. 데이터 간 연관성 찾기
# 3. 패턴 식별
# => 연관 규칙
# 
# 연관 규칙 응용 분야
# 1)장바구니 분석
# 2)DNA 패턴, 염기 서열 검색
# 3)사기성 결제, 보험 이용 구매 또는 의료비 청구 패턴
# 4)고객 서비스 중단, 상품 변경시 선행되는 행동의 조합
# 
# 장바구니 분석 -> 연관규칙(아이템집합 사이의 관계 패턴) 모음
# 
# 
# {젤리, 우유, 버터} -> {빵}
#     (조건)LHS -> RHS(조건 만족시 기대되는 결과)
# *데이터 속에 숨어있는 지식 발견
# 
# 아프리오리 알고리즘
# 
# EX)마트에서 판매되는 아이템 종류 총 100가지인 경우,
# 아이템 집합=
# 빵
# {빵},{}
# 빵,우유
# {},{빵},{우유},{빵,우유}
# 
# {9999개}-> {빵}
# 1315,1547,2326,6453,8633,5614,5345,7567,2345,8552,7460,6234,6345,1574,7571,6345,8569,2034,3211
# 
# 작은 데이터 셋으로는 유용하지 않다
# 비논리적인 결론이 나올 수 있음

#연관규칙(Association rules)

install.packages("arules")
library(arules)
#groceries<-read.csv("groceries.csv") 사용하면 안됨
groceries<-read.transactions("groceries.csv",sep=",")
#희소행렬을 생성함(9835*169)
groceries
summary(groceries)

inspect(groceries[1:5])

itemFrequency(groceries[,1:169]) #지지도

itemFrequencyPlot(groceries, support=0.1)
#최소 지지도 10% 이상에 대한 아이템 시각화

itemFrequencyPlot(groceries, topN=20)
#지지도 내림차순했을때 상위 20개 아이템 tㅣ각화

#희소행렬(대략 9800행*170열)

image(groceries[1:5])
#희소행렬(1행~5행까지 출력)


image(sample(groceries,100))
#임의로 100개의 거래데이터 추출

apriori(groceries)
#최소 지지도 임계치 설정?최소 거래 건수 고려
#최소 하루에 5번 거래는 있어야만, 이 아이템에
#대한 연관규칙 생성이 의미가 나한테는 있다.라고
#전제했다면.... 150/10000 => 0.015지지도 이상

#신뢰도가 낮으면
#신뢰도(볼펜->맥주)=sup(볼펜,맥주)/sup(볼펜)=0.25
#conf(X->Y) = sup(X,Y) / sup(X)
#minlen:2로 설정,최소 2개 이상의 아이템을 갖는 규칙을 생성


groceryrules<-apriori(groceries, parameter = list(support=0.006,confidence=0.25, minlen=2))
# 하루최소2번거래*30/ 10000 =0.006지지도 이상 됐을때 규칙 생성

inspect(groceryrules[1:10])


summary(groceryrules)

# x->y,z
# x,y->z

inspect(sort(groceryrules, by="support")[1:5])
inspect(sort(groceryrules, by="confidence")[1:5])
inspect(sort(groceryrules, by="lift")[1:5])

#베리 홍보 마게팅팅 팀 요구사항: berries -> ???

#subset() : 규칙 -> 하위규칙
berryrules<-subset(groceryrules, items %in% "berries")
inspect(berryrules)

byrules<-subset(groceryrules, items %in% c("berries","yogurt"))
byrules
inspect(byrules)
#연산자 in은 최소 1개의 아이템을 규칙에서 발견했을시에 추출

#부분매칭
fruitrules<-subset(groceryrules, items %pin% c("fruit"))
fruitrules
inspect(fruitrules)

#완전매칭: 모든 아이템이 반드시 존재해야함
by2rules<-subset(groceryrules, items %ain% c("berries","yogurt"))
by2rules
inspect(by2rules)

#연관규칙을 파일로 저장
write(groceryrules, file="groceryrules.csv",sep=",",row.names=FALSE)
