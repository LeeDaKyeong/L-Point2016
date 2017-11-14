#####################################
######## L.Point - 상어초밥 #########
#####################################

#1. 물품별 유사도 행렬 데이터
#2. 고객별 물품 추천 점수 행렬
#3. 최종 물품 세가지 추천


################################################################################
################################################################################

##### 물품별 유사도 행렬 #####

#물품 점수 데이터(고객x물품)
load("win_data_TFmat.Rdata") 

#물품점수 데이터를 물품x고객 데이터로
pro <- t(win_data_TFmat)
dim(pro) #4137개의 물품, 19372명의 고객


##물품별 유사도 행렬을 mat에 저장
#물품x물품
mat <- matrix(ncol=nrow(pro),nrow=nrow(pro),0)
colnames(mat) <- rownames(pro)
rownames(mat) <- rownames(pro)


#코사인 유사도 : (x · y) / (||x|| * ||y||)

for(i in 1:dim(mat)[1])
{
  pi <- pro[i,]
  for(j in i:dim(mat)[1])
  {
    pj <- pro[j,]
    mat[i,j] <- sum(pi* pj)/sqrt(sum(pi^2) * sum(pj^2))
  }
}

mat[lower.tri(mat)] <- mat[upper.tri(mat)]

mat_cos <- mat

save(mat_cos,file="mat_cos_win_final.Rdata")


################################################################################
################################################################################

##### 고객별 물품 추천 점수 행렬 #####


#겨울 구매목록 데이터 
load("win_buy_data_unique.Rdata") 
data <- win_buy_data_unique

#첫번째 열이 고객번호이므로 고객번호를 행이름으로 변환
rownames(data) <- data[,1]
data <- data[,-1]


##각 구매자 별 상품 추천 점수를 저장할 fmat
#고객x물품
fmat <- matrix(ncol=dim(mat_cos)[1],nrow=nrow(data),0)
colnames(fmat) <- colnames(mat_cos)
rownames(fmat) <- rownames(data)



#내가 산 물품들과 fmat의 상품들 순서대로 mat의 점수를 가져와 비교
#score(i, x) = (itemsSimilarity(x, record(i, 1)) + … + itemsSimilarity(x, record(i, M(i)))) / (itemsSimilarity(x, item(1)) + … + itemsSimilarity(x, item(K)))

for(i in 1:dim(fmat)[1])
{
  x <- which(colnames(mat_cos)%in%data[i,]) #유사도 매트릭스에서 i번째 고객이 구매한 물품들의 위치
  for(j in 1:dim(fmat)[2]) 
  {
    fmat[i,j] <- sum(mat_cos[x,j])/sum(mat_cos[,j]) 
    #(j번째 물품과 i번째 고객이 구매한 물품들의 유사도 합)/(j번째 물품과 다른 모든 물건들의 유사도 합)
  }
}

save(fmat,file="fmat_win_구매횟수_cos_final.Rdata")


################################################################################
################################################################################

##### 최종 물품 세가지 추천 #####

##최종 물품 세가지를 저장할 final_recommend
#물품 추천 점수 행렬에서 점수가 가장 높은 3가지 물품 추천

sort_fun <- function(x)
{
  return(names(sort(x,decreasing = T)[1:3]))
}

final_recommend <- t(apply(fmat,1,sort_fun))

save(final_recommend,file="final_recommend_cf.Rdata")


