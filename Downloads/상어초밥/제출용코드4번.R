#####################################
######## L.Point - 상어초밥 #########
#####################################

#1. 물건기반 협업필터링 결과와 Word2Vec 결과 통합
#2. 모델 검증 시트에 맞게 최종추천


################################################################################
################################################################################

##### 물건기반 협업필터링 결과와 Word2Vec 결과 통합 #####

#물품기반 협업필터링의 결과와 word2Vec결과를 불러옵니다.
load("final_recommend_cf.Rdata")
load("final_recommend_word2vec.Rdata")


#고객번호를 숫자형으로 바꿔줍니다.
index1<-as.numeric(rownames(final_recommend))
index2<-as.numeric(rownames(final_recommend_word2vec))


final_recommend<-apply(final_recommend,1,as.character)
final_recommend<-t(final_recommend)


#두 고객번호가 실제로 같은지 검사를 합니다
identical(sort(index1),sort(index2))


#고객번호열을 추가하여 번호순으로 나열을 합니다.
final_recommend<-cbind(final_recommend,index1)
final_recommend_word2vec<-cbind(final_recommend_word2vec,index2)


#변수명을 잘 지정해 줍니다.
#첫번째 추천, 두번째 추천, 세번째 추천, 고객번호
colnames(final_recommend)<-c("1","2","3","index")
colnames(final_recommend_word2vec)<-c("1","2","3","index")


#matrix로 통합을 해줍니다.
final_recommend<-as.matrix(final_recommend)
final_recommend_word2vec<-as.matrix(final_recommend_word2vec)

#이제 고객번호를 sort해준 순서로 새로운 데이터 셋을 만듭니다.
final_recommend2<-final_recommend[order(index1),]
final_recommend_word2vec2<-final_recommend_word2vec[order(index2),]


## 만약 물품기반 협업필터링과 word2Vec 결과가 3가지 모두 동일하다면 그 세가지를 추천합니다.
## 만약 2가지가 같다면  그 2가지와 더 정답률이 높았던 Word2Vec에서 한가지를 추천합니다.
## 만약 1가지가 같다면  같은것 하나와 word2Vec 결과 1순위 물품기반 협업필터링 1순위를 추천합니다.
## 만약 동일하게 추천한 물품이 없다면 word2Vec 결과 1순위 2순위, 물품기반 협업필터링 1순위를 x행렬에 대입합니다.

#final에 최종 추천 목록을 저장합니다.
final <- matrix(NA,ncol=3,nrow=length(index1))

for(i in 1:length(index1)){
  if(sum(final_recommend_word2vec2[i,-4]%in%final_recommend2[i,-4])==3){
    final[i,] <- final_recommend_word2vec2[i,-4] 
    
  }else if(sum(final_recommend_word2vec2[i,-4]%in%final_recommend2[i,-4])==2){
    final[i,] <- final_recommend_word2vec2[i,-4]
    
  }else if(sum(final_recommend_word2vec2[i,-4]%in%final_recommend2[i,-4])==1){
    final[i,] <- c(final_recommend_word2vec2[i,-4][final_recommend_word2vec2[i,-4]%in%final_recommend2[i,-4]],
              final_recommend2[i,-4][!final_recommend_word2vec2[i,-4]%in%final_recommend2[i,-4]][1],
              final_recommend_word2vec2[i,-4][!final_recommend_word2vec2[i,-4]%in%final_recommend2[i,-4]][1])
    
  }else if(sum(final_recommend_word2vec2[i,-4]%in%final_recommend2[i,-4])==0){
    final[i,] <- c(final_recommend_word2vec2[i,-4][1:2], final_recommend2[i,-4][1])
  }
}
#고객번호를 마지막으로 넣어줍니다.
rownames(final) <- final_recommend_word2vec2[,4]



################################################################################
################################################################################

##### 모델 검증 시트에 맞게 최종추천 #####

final_sheet <- as.data.frame(matrix(NA,nrow=19383,ncol=3))
#최종적으로 추천할 물품들을 final_sheet에 저장합니다.


final <- as.matrix(final)

#고객번호가 맞는 부분에 추천목록을 넣습니다.
#겨울에 구매건수가 없는 고객에게는 물품을 추천하지 않습니다.

index <- which(rownames(final_sheet)%in%rownames(final))
final_sheet[index,] <- final


#저장을 한뒤 검정시트에 작성 합니다.
write.csv(final_sheet,"final_sheet.csv")
