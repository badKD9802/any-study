
 ## (1) Find mean vector, covariance matrix S, correlation matrix R
setwd("C:/Users/Rprogram/Desktop/다변량1 실습1/Rdata")
Klpga <-read.table("klpga.txt", header=T)
X<-Klpga
X
class(X)
X<-as.matrix(X)			# 자료행렬
xn<-nrow(X)			# 행 개수
xbar<-t(X)%*%matrix(1,n,1)/n 	# 평균벡터
I<-diag(n)			
J<-matrix(1,n,n)
H<-I-1/n*J			# 중심화행렬
Y<-H%*%X			# 중심화 자료행렬
S<-t(Y)%*%Y/(n-1)		# 공분산행렬 
D<-diag(1/sqrt(diag(S)))		# 표준편차행렬의 역
Z<-H%*%X%*%D		# 표준화자료행렬
colnames(Z)<-colnames(X)
R<-t(Z)%*%Z/(n-1)		# 상관행렬
R_S<-D%*%S%*%D		# 상관행렬과 공분산행렬의 대수적 관계
detS <- det(S)			# 일반화 분산
detR <- det(R)
trS <- sum(diag(S))		# 총 분산
trR <- sum(diag(R))

xbar;S;R #평균벡터    #공분산행렬    #상관행렬

#2.
S<-cov(X)
R<-cor(X)
X <- scale(X)

variation <- matrix(NA,1,4)
colnames(variation) <- c('detS','detR','trS','trR')

variation[,1] <-det(S)
variation[,2] <-det(R)
variation[,3] <-sum(diag(S))
variation[,4] <-sum(diag(R))
  
variation

 #3.
n<-nrow(X)
xbar<-t(X)%*%matrix(1,n,1)/n # 평균벡터
I<-diag(n)
J<-matrix(1,n,n)
H<-I-1/n*J                  # 중심화행렬
Y<-H%*%X                 # 중심화 자료행렬
S<-t(Y)%*%Y/(n-1)          # 공분산행렬 
D<-diag(1/sqrt(diag(S)))     # 표준편차행렬의 역수 (1/s_ii)
Z<-Y%*%D                # 표준화자료행렬
colnames(Z)<-colnames(X)

boxplot(X)
boxplot(Y)
boxplot(Z)

4.
plot(Klpga)
Klpga[,5]
summary(Klpga)
5.
# Chi-squre Plot for Checking MVN
x=Klpga
n=dim(x)[1]
p=dim(x)[2]
S=cov(x)
xbar=colMeans(x)
m=mahalanobis(x, xbar, S)
m=sort(m)
id=seq(1, n)
pt=(id-0.5)/n
q=qchisq(pt, p)

plot(q, m, pch="*", xlab="Quantile", ylab="Ordered Squared Distance")
abline(0, 1)

rq=cor(cbind(q, m))[1,2]
rq

library("MVN") # for mardia test

Klpga

result_Klpga = mvn(Klpga, mvnTest = "mardia", multivariatePlot =  "qq")

result_Klpga


