#####################################
######## L.Point - 상어초밥 #########
#####################################

#1. Word2Vec 학습
#2. 고객별 물품 추천 좌표계 & 물품 추천
#3. 학습한 Word2Vec 시각화


################################################################################
################################################################################

##### Word2Vec 학습 #####

#먼저 패키지를 설치합니다
install.packages("devtools")
library(devtools) #wordVectors라는 패키지는 깃허브에서 직접 다운받아 와야하기 때문에 'devtools'패키지를 다운받습니다.

install_github("bmschmidt/wordVectors") #Word2Vec 용 패키지를 설치합니다 .
library(wordVectors)

load("data_win_word2vec.Rdata") 
#학습시킬 데이터를 불러옵니다.
#변수명 word2vec

write(Word2Vec,"Word2Vec.txt") 
#word2vec 데이터를 워드투벡에 학습시키기 위에 txt파일로 저장합니다.


#Word2Vec을 20차원으로 학습시킵니다.
#vectors = 20 : 20차원, window = 8 : 기준 단어 앞,뒤로 8개씩 확인
model = train_word2vec("Word2Vec.txt",output="Word2Vec20.bin",threads = 3,vectors = 20,window=8)
# model<-read.vectors("Word2Vec20.bin") #한번 만든 모델은 read.vectors 함수로 다시 불러올 수 있습니다.


set.seed(1) #씨드넘버를 주어서 항상 같은 그래프가 그려지도록 했습니다.
plot(model)

nearest_to(model,model[[c("C160406")]],10) #C160406는 유아용기저귀 입니다.
#유아용 기저기와 가까운 물품 10가지를 확인해 보겠습니다.

## 결과물들이 코드명이라 해석하기 어려운 형태로 나왔습니다.

#############################################################################################################
#############################################################################################################
##                                                                                                         ##
## C160406      C130401      C160407      C160206      C160409      C150509      C170305      C130403      ##
## 2.220446e-16 3.149391e-02 4.159744e-02 4.937160e-02 5.762460e-02 5.970940e-02 6.450369e-02 6.650422e-02 ##  
## C160404      C170159                                                                                    ##
## 6.733528e-02 6.820762e-02                                                                               ##
##                                                                                                         ##
#############################################################################################################
#############################################################################################################

#소분류코드명을 소분류명으로 바꾸어주어 해석에 용이하도록 하겠습니다.
clasfic<-read.table("제3회 Big Data Competition-분석용데이터-03.상품분류.txt",sep=",",header=T,stringsAsFactors=F)

clasfic<-clasfic[,c("소분류코드","소분류명")]
#'소분류코드' 와 '소분류명'을 가져옵니다.

name<-function(x){
  index=which(clasfic[,1]%in%names(nearest_to(model,model[[c(x)]]))) 
  y<-nearest_to(model,model[[c(x)]])#x위치에 소분류명이 들어온다면 소분류코드와 일치하는 이름을 가져옵니다.
  names(y)<-clasfic[index,2]
  return(y)
}
name("C160406") #유아용기저귀와 코사인 거리가 가까운 순으로 나열한 결과입니다.

#######################################################################################################
#######################################################################################################
##                                                                                                   ##
##  분유            유아과자        토이캔디        유아용물티슈    유아/아동용칫솔    유아용기저귀  ##
##  2.220446e-16    3.149391e-02    4.159744e-02    4.937160e-02    5.762460e-02        5.970940e-02 ##
##  유아용세척용품  젖병/젖꼭지     종이컵     건전지                                                ##
##  6.450369e-02    6.650422e-02    6.733528e-02    6.820762e-02                                     ##
##                                                                                                   ##
#######################################################################################################
#######################################################################################################




################################################################################
################################################################################

##### 고객별 물품 추천 좌표계 & 물품 추천 #####

#각 유저가 구매한 물품을 점수 순으로 X축에 나열하기 위해 "고객별 물품 점수 행렬" 을 불러옵니다.
load("win_data_TFmat.Rdata")


#"중요도"를 나타내는 Z축을 생성합니다.
TFlist<-names(sort(apply(win_data_TFmat,2,sum),decreasing=T)) 

TFlist[1] #1등은 일반흰우유가 나왔습니다.


#고객번호 와 소분류코드를 불러옵니다.
index<-rownames(win_data_TFmat)
item_index<-colnames(win_data_TFmat)


#구매한 물건갯수의 제곱근 만큼 가져옵니다.
#i개를 i의 제곱근(개)���로 줄여 추천좌표계에서 불필요한 좌표를 줄입니다.

load("data_win_짝궁2.Rdata") 
#"구매목록 데이터"를 만들 때 고객별 총 구매횟수를 저장해두었던 Rdata입니다.


n=5 #Y축에 나열할 유사한 물품의 수를 5개로 지정합니다.

##최종적으로 추천할 세가지를 final_recommend_word2vec에 저장합니다.
final_recommend_word2vec <- matrix(NA,nrow=length(index),ncol=3)

for(i in 1:length(index)){
  recomlist<-sort(win_data_TFmat[i,],decreasing = T)[1:sqrt(x[i])] 
  #recomlist 에서 차례대로 n(5)개씩 추천
  
  csim<-matrix(0,ncol=n,nrow=length(recomlist)) #코사인 유사도를 저장할 공간입니다.
  csim_name<-matrix(0,ncol=n,nrow=length(recomlist)) #소분류코드를 저장할 공간입니다.
  
  for(j in 1:length(recomlist)){
    csim[j,]<-nearest_to(model,model[[names(recomlist[j])]])[2:(n+1)] 
    #j번째 물건과 가장 유사한 5가지 물품의 거리를 csim에 저장합니다.
    
    csim_name[j,]<-names(nearest_to(model,model[[names(recomlist[j])]])[2:(n+1)])
    #j번째 물건과 가장 유사한 5가지 물품의 소분류 코드를 csim_name에 저장합니다.
  } 
  
  TFlist_each<-which(TFlist%in%names(recomlist)) 
  #나열한 루트(x[i])개(- x축) 의 중요도를 비교하여 z축에 나열합니다.  
  
  scale_pro<-vector("numeric",length(TFlist_each))
  #5점 만점으로 TF-IDF 점수를 환산하고 저장할 공간입니다.
  
  for(k in 1:length(scale_pro)){
    scale_pro[k] <-5*(TFlist_each[k]-min(TFlist_each))/(max(TFlist_each)-min(TFlist_each))
  }
  
  final_recommend_word2vec[i,]<-csim_name[order(scale_pro^2+((csim*10**3)^2))[1:3]]
  #구한 점수중 상위 3개의 물건을 대입하여 추천리스트를 완성합니다.
}

rownames(final_recommend_word2vec)<-index #고객번호를 추가합니다.

save(final_recommend_word2vec,file="final_recommend_word2vec.Rdata")


################################################################################
################################################################################

##### 학습한 Word2Vec 시각화 #####

#3차원으로 학습한 word2Vec을 3차원plot으로 확인해보는 작업을 하기위해 패키지를 설치합니다.
install.packages("rgl")
library(rgl)

model3 = train_word2vec("Word2Vec.txt",output="Word2Vec3.bin",threads = 3,vectors = 3,window=8)
#3차원으로 학습을 시킵니다.
# model3<-read.vectors("Word2Vec3.bin") #한번 만든 모델은 read.vectors 함수로 다시 불러올 수 있습니다.

install.packages("stringr")
library(stringr) #소분류코드에 A B C D 가 들어가면 다른색을 주기 위해 stringr 패키지를 설치합니다.

A<-str_detect(rownames(model3@.Data),"A")
B<-str_detect(rownames(model3@.Data),"B")
C<-str_detect(rownames(model3@.Data),"C")
D<-str_detect(rownames(model3@.Data),"D")

data_df<-cbind(model3@.Data[,1:3],0) #색을 넣을 빈 공간을 만듭니다.
data_df[A,4]<-"red"             #A사는 red
data_df[B,4]<-"darkgoldenrod1"  #B사는 darkgoldenrod1
data_df[C,4]<-"deepskyblue"     #C사는 deepskyblue
data_df[D,4]<-"darkviolet"      #D사는 darkviolet
data_df[1,4]<-1                 #공백문자는 검은색으로 표시합니다.
plot3d(data_df,xlab="X축",ylab="Y축",zlab="Z축",col=data_df[,4],box=F)
