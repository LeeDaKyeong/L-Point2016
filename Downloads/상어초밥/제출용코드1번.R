#####################################
######## L.Point - 상어초밥 #########
#####################################

#1. 겨울데이터 생성
#2. 구매목록 데이터 생성
#3. Word2Vec 학습데이터
#4. 구매횟수 데이터
#5. 물품 점수 데이터


################################################################################
################################################################################

#####계절별 구매물품 차이 확인#####
#계절별로 물품에 차이가 있는지 plot으로 확인한다.

#"구매상품TR 데이터"는 크기가 커서 data.table패키지를 이용해 좀더 빠르게 읽는다.
install.packages("data.table")
library(data.table) 

#구매상품
data <-fread("제3회 Big Data Competition-분석용데이터-02.구매상품TR.txt",header=T,sep=",")

#data의 크기확인과 변수명들이 잘 불러와졌는지 확인한다.
nrow(data)#28593030
colnames(data)


#전체 데이터를 전부 활용하지 않고 일부분만 가져와서 계절요인을 확인한다
small_data<- data[,c("소분류코드","고객번호","구매일자")]

small_data$구매일자[1]
#구매일자 형식이 "20140222" YYYYMMDD 형식이다.


#컴파일을 시켜 반복문의 속도를 높이기 위해 패키지를 설치한다.
install.packages("complier") 
library(compiler)

#계절별 요인을 확인하기 위해 stringr 패키지에서
#substr 함수를 이용해 MM(월)자리를 가져와 따로 저장한다.
install.packages("stringr")  
library(stringr) 

fun1<-function(){
  month<-vector("character",nrow(small_data)) #월 정보가 들어갈 빈공간을 미리 만들어 둔다.
  for(i in 1:nrow(small_data)){
    month[i] <- substr(small_data$구매일자[i],5,6) 
    #구매일자에서 5번재 6번째 위치, MM(월)을 가져온다.
  }
  return(month)
}
CMP1<- cmpfun(fun1)
month<-CMP1()

small_data<-cbind(small_data,as.numeric(month)) 
#MM을 "01" 과 같이 문자형에서 숫자형 1 로 바꾼뒤 계절을 분류하는데 이용된다.
colnames(small_data)[4]<-"month"


#month를 문자형에서 숫자형으로 바꾸는 이유

###############################################################################################
###############################################################################################   
##                                                                                           ##
## install.packages("microbenchmark")                                                        ##
## library(microbenchmark)                                                                   ##
##                                                                                           ## 
## char_type <- sample(c("01","02","03","04","05","06","07","08","09","10","11","12"),100,T) ##
## num_type <- sample(1:12,100,T)                                                            ##
##                                                                                           ##
## season1<-vector("character",100);season2<-vector("numeric",100)                           ##
## char<-function(x){                                                                        ##
##   for(i in 1:length(x))                                                                   ##
##     if(x[i]=="03" | x[i]=="04" | x[i]=="05"){                                             ##
##       season1[i]<-"봄"                                                                    ##
##     }                                                                                     ##
## }                                                                                         ##
##                                                                                           ##
## numeric<-function(x){                                                                     ##
##   for(i in 1:length(x)){                                                                  ##
##     if(x[i]>=3 & x[i]<=5){                                                                ##
##       season2[i]<-1                                                                       ##
##     }                                                                                     ##
##   }                                                                                       ##
## }                                                                                         ##
## res <- microbenchmark(char(char_type),numeric(num_type),times=1000)                       ##
## install.packages("ggplot2")                                                               ##
## library(ggplot2)                                                                          ##
## autoplot(res)                                                                             ##
##                                                                                           ##
## 같은 결과를 가져오더라도 if문에 들어가는 내용이 짧을 수록 연산이 빨라지기 때문이다.       ## 
##                                                                                           ##
###############################################################################################
###############################################################################################

#season변수에 month가 3~5이면 봄, 6~8이면 여름, 9~11이면 가을, 12~2이면 겨울을 넣어준다.
fun2<-function(){
  season <- vector("character",nrow(small_data)) 
  #계절이 들어갈 빈공간을 미리 만들어 둔다.
  
  for(i in 1:nrow(small_data)){
    x<-small_data$month[i]
    if(x>=03 & x<=5){
      season[i] <- "봄"
    }else if(x>=6 & x<=8){
      season[i] <- "여름"
    }else if(x>=9 & x<=11){
      season[i] <- "가을"
    }else if(x==1| x==2| x==12){
      season[i] <- "겨울"
    }
  }
  return(season)
}  

CMP2<- cmpfun(fun2) #complie하여 반복문이 빠르게 실행되도록 한다.
season<-CMP2()

data_season <- cbind(small_data,season)

data_season <- as.data.frame(data_season)

season_table <- table(data_season$소분류코드,data_season$season)
season_table <- season_table[,c("봄","여름","가을","겨울")] 



#계절별로 팔리는 물건이 차이가 있는지 그래프로 확인합니다.

plot(season_table[811,1],type="n",xlim=c(1,4),ylim=c(0,3700),main="계절별 구매 횟수 차이",ylab="",xlab="")
lines(season_table[811,],col="aquamarine3",lwd=4)  #수입메로
text(1.5,950,"수입메론",cex=2)
lines(season_table[819,],col="pink",lwd=4)  #굴비
text(1.6,1350,"굴비",col=2,cex=2)
lines(season_table[2019,],col="peachpuff",lwd=4) #찌개두부
text(1.5,400,"찌개두부",col=3,cex=2)
lines(season_table[2512,],col="lightsteelblue2",lwd=4) #온라인베이커리
text(2,150,"온라인베이커리",col=4,cex=2)
lines(season_table[2639,],lwd=4,col="orchid2") #디저트류
text(1.5,2000,"디저트류",cex=2)
lines(season_table[2648,],lwd=4,col="ivory3") #국산삼치
text(1.5,3400,"국산삼치",cex=2)


#통계적으로 유의한지 ANOVA 분석을 실시합니다.
scale_season<-apply(season_table,1,scale) #구매횟수를 정규화 하여 정규분포를 따르게 합니다.

scale_season<-t(scale_season) #apply적용뒤 transpose를 시켜줘야 합니다.

scale_season<-as.data.frame(scale_season) #ANOVA분석을 위한 data.frame으로 변환 작업을 합니다.

colnames(scale_season)<-c("봄","여름","가을","겨울") #다시한번 계절을 입력하고

scale_season<-matrix(c(scale_season[,1],scale_season[,2],scale_season[,3],scale_season[,4]),ncol=1)
## 계절별 데이터를 일렬로 입력을 합니다.
group<-as.factor(rep(c("봄","여름","가을","겨울"),each=4386))
##계절을 factor형으로 입력합니다.
scale_season<-cbind(scale_season,group)
##분석 준비가 끝낫습니다.
colnames(scale_season)<-c("y","group")
##변수명을 y 와 group으로 정의합니다.
scale_season<-as.data.frame(scale_season)
group_df <- transform(scale_season, group = factor(group))

summary(aov(y ~ group, data = group_df))
#마지막으로 ANOVA분석을 실시하여 검정결과를 살펴봅니다.

################################################################################
################################################################################
##                                                                            ##
##                 Df Sum Sq  Mean Sq  F value     Pr(>F)                     ##
## group           3     26     8.535     11.  4 1.83e-07 ***                 ##
## Residuals   17540  13132     0.749                                         ##
## ---                                                                        ##
##  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1            ##
##                                                                            ##
################################################################################
################################################################################


################################################################################
################################################################################

#####겨울데이터 뽑기#####

#계절별 차이가 있음을 확인했으니, 겨울 데이터만 가져오기
win_index<-which(season=="겨울")

data_win <-data[win_index,]


#잘 가져와졌는지 확인한다.
colnames(data_win)
dim(data_win) #28593030-->6850615 약24%로 데이터가 줌
data_win<-as.data.frame(data_win)



################################################################################
################################################################################

#####구매목록 데이터 만들기#####
#물품기반 협업필터링 알고리즘과 Word2Vec알고리즘에서 사용될 구매목록 데이터 만들기
#고객별 전체 기간 중, 겨울동안 구매한 구매목록을 나열한 데이터

index<-unique(data_win$고객번호) #고객번호를 중복되지 않게 저장한다

Sample<-data_win[data_win[,"고객번호"]==index[1],"소분류코드"] #index 1번이 구매한 물건 목록들의 예시
Sample

win_buy_data<-matrix(NA,nrow=length(index),ncol=10000) #고객들의 구매목록을 담을 공간이다. 
#최대구매횟수를 모르기에 1만개의 공간을 만들었다.

x<-vector("numeric",length(index)) #'index i'번 고객이 몇개 샀는지 저장한다.

for(i in 1:max(index)){
  if(is.na(index[i]))next() #겨울에 구매이력이 없는 사람은 걸러내기 위해 조건문을 추가한다.
  li<-data_win[data_win[,"고객번호"]==index[i],"소분류코드"]
  x[i]<-length(li)
  for(j in 1:length(li)){
    win_buy_data[i,j]<-li[j] #'index i'번은 i번고객이 아니라서 
    #win_buy_data[i,]에 index[i]번 고객정보가 들어간다.
  }
}

win_buy_data<-win_buy_data[,1:max(x)] #고객들중 가장 많이 산 고객에 맞춰 win_buy_data의 사이즈를 줄인다.


win_buy_data<-cbind(index,win_buy_data) #마지막으로 고객번호들을 추가하면 구매목록데이터는 완성된다

save(win_buy_data,file="win_buy_data.Rdata")
save(x,file="data_win_짝궁2.Rdata") #워드투백에 다시한번 쓰이기 때문에 저장합니다.





#최종적으로 중복되는 물품들을 제거하고 하나씩만 남겨, 구매목록 데이터를 완성한다.
win_buy_data_unique <- as.data.frame(matrix(NA,ncol=ncol(win_buy_data),nrow=nrow(win_buy_data)))

for(i in 1:nrow(win_buy_data))
{
  a <- unique(win_buy_data[i,])
  win_buy_data_unique[i,1:length(a)] <- a
}

save(win_buy_data_unique, file="win_buy_data_unique.Rdata")




################################################################################
################################################################################

##### Word2Vec 알고리즘 학습시킬 데이터 만들기 #####
#고객이 구매한 물품들을 단어처럼 나열한다면 문장이 되어 Word2Vec에 학습시킬 수 있게 된다.

Word2Vec<-c()
for(i in 1:nrow(win_buy_data)){
  Word2Vec<-rbind(Word2Vec,paste(win_buy_data[i,1:(which(is.na(win_buy_data[i,]))[1]-1)],collapse = " "))
}

save(Word2Vec,file="data_win_word2vec.Rdata")







################################################################################
################################################################################

##### 구매 횟수 데이터 만들기 #####

#물품기반 협업필터링과 워드투벡 알고리즘에서 고객이 각 물품에 매긴 선호도가 필요한데,
#여기에 쓰일 선호도를 생성하기 위해, 전체 겨울기간 동안 고객이 각 물품을 몇개 구매했는지를 담고있는
#구매횟수 데이터를 생성한다.

#상품번호를 중복되지 않게 저장한다.
item<-unique(data_win$소분류코드) 
item<-sort(item) #알아보기 쉽게 정렬을 한다


final3<-matrix(0,nrow=length(li),ncol=length(item)) #구매횟수정보를 담을 공간이다.

for(i in 1:max(li)){ 
  li<-data_win[data_win[,"고객번호"]==index[i],"소분류코드"]
  
  #겨울에 구매이력이 없는 사람은 걸러내기 위해 조건문을 추가한다.
  if(is.na(li[i]==T))next() 
  
  for(j in 1:length(li)){
    final3[i,which(item==li[j])]<-final3[i,which(item==li[j])]+1
    #0으로 초기화된 구매횟수 데이터에서 구매한 목록이 나오면 1씩 더해주어서 구매횟수 정보를 만든다.
  }
}


#고객번호와 물품이름을 행과 열에 추가해준다.
rownames(final3)<-index
colnames(final3)<-item

save(final3,file="win_구매횟수.Rdata")









################################################################################
################################################################################

##### 물품점수화 데이터 만들기 #####
#물품별 고객의 선호도를 구매횟수 데이터와 TF-IDF 이론으로 생성


idf<-vector("numeric",length=nrow(final3)) #물품점수가 들어갈 공간

for(i in 1:nrow(final3)){
  idf[i]<- log(length(index)/length(which(final3[i,]==0)))
  
  #TF-IDF = TF/DF
  #DF값이 너무 크면 TF-IDF값 자체가 너무 작아져, 점수에 차이가 없어 보이므로
  #보기 좋게 하이 위하여 log를 취해줍니다.
  
  #TF-IDF는 상대적인 값이기 때문에 전체적으로 log를 취해주어도 문제 없음.
  
  #DF = (해당 물건을 구매한 고객의 수) = (전체 고객 수) - (해당 물건을 구매하지 않은 고객 수)
}

win_data_TFmat<-final3*idf
win_data_TFmat<-round(win_data_TFmat,2) #유효숫자를 소수점 둘째자리 로 지정합니다.


save(win_data_TFmat,file="win_data_TFmat.Rdata")

