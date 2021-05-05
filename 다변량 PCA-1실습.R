setwd("C:/Users/Rprogram/Desktop/다변량1 실습4/Rdata")

## Practice Time 4 for PCA

setwd("E:/1학기 다변량1 실습자료/Rdata")

### 5subjects data

# Steps for PCA

#[Step 1] Data Matrix X
library("MVT")
data(examScor)
X=examScor
head(X)
dim(X)

#[Step 2] Covariance Matrix S(or Correlation Matix R)
S=round(cov(X),3)
S

#[Step 3] Spectrla Decompositoin 
eigen.S=eigen(S)
round(eigen.S$values, 3) # Eigenvalus
V=round(eigen.S$vectors, 3) # Eigenvaectors
V

#[Step 4] Choice of Eigenvalues and Eigenvectors
gof=eigen.S$values/sum(eigen.S$values)*100 # Goodness-of fit
round(gof, 2)
plot(eigen.S$values, type="b", main="Scree Graph", xlab="Component Number", ylab="Eigenvalue")

#[Step 5] PCs : liner combination of original variables 
V2=V[,1:2]
V2

#[Step 6] PCS, PCs Scores and New Data Matrix P
Y=scale(X, scale=F) # Centred Data Matrix
P=Y%*%V2            # PCs Scores
head(P)

#[Step 7] Plot of PCs Scores
plot(P[,1], P[, 2], main="Plot of PCs Scores", xlab="1st PC", ylab="2nd PC")
text(P[,1], P[, 2]+2, labels=rownames(P), cex=0.8, col="blue")
abline(v=0, h=0)

# PC Biplots for 5 Subjects Exam
library("MVT")
data(examScor)
X=examScor
n <- nrow(X) 
rownames(X)
colnames(X)
joinnames=c(rownames(X),colnames(X))

Y <- scale(X,scale=F)

# Biplot based on the Singular Value Decomposition
svd.Y <- svd(Y) 
U <- svd.Y$u    
V <- svd.Y$v 
D <- diag(svd.Y$d)
G <- (sqrt(n-1)*U)[,1:2]
H <- (sqrt(1/(n-1))*V%*%D)[,1:2] 
C<- rbind(G, H)
rownames(G)<-rownames(X)
rownames(H)<-colnames(X)
rownames(C)<-joinnames

# Godness-of-fit
eig <- (svd.Y$d)^2 
per <- eig/sum(eig)*100
gof <- sum(per[1:2])

# Biplots
par(mfrow=c(2,2))
par(pty="s")
lim1 <- range(pretty(H))
plot(H[,1],H[,2],xlab="1st PC",ylab="2nd PC", main="(a) 5 Subjects",
     xlim=lim1,ylim=lim1,pch=15,col=2, type="n") #화살표의 길이가 분산의 크기
abline(v=0,h=0)
text(H[,1], H[,2],colnames(X),cex=0.8,col=1,pos=3)
arrows(0,0,H[,1],H[,2],col=2,code=2, length=0.1)

lim2 <- range(pretty(G))
plot(G[,1],G[,2],xlab="1st PC",ylab="2nd PC", main="(b) 88 Students",
     xlim=lim2,ylim=lim2,pch=16, type="n")
abline(v=0,h=0)
text(G[,1],G[,2],rownames(X),cex=0.8,pos=3)

lim3 <- range(pretty(C))
plot(C[,1],C[,2],xlab="1st PC",ylab="2nd PC",  main="(c) 5 Subjects and 88 Students",
     xlim=lim3,ylim=lim3,pch=16,  type="n")
abline(v=0,h=0)
text(C[,1],C[,2],joinnames,cex=0.8,pos=3)
arrows(0,0,C[89:93,1],C[89:93,2],col=2,code=2, length=0.1)

biplot(G,H, xlab="1st PC",ylab="2nd PC", main="(d) biplot function",
       xlim=lim2,ylim=lim2,cex=0.8,pch=16) #화살표의 방향에 있는 친구들은 성적이 높고 반대방향은 성적이 낮다.
abline(v=0,h=0)

### klpga data

# PCA Steps for KLPGA

#[Step 1] Data Matrix X
Data1.3.2<-read.table("klpga.txt", header=T)
X=Data1.3.2
rownames<-rownames(X) 

#[Step 2] Covariance Matrix S(or Correlation Matix R)
R=round(cor(X),3)
R

#[Step 3] Spectral Decomposition 
eigen.R=eigen(R)
round(eigen.R$values, 2) # Eigenvalues
V=round(eigen.R$vectors, 2) # Eigenvectors

#[Step 4] Choice of Eigenvalues and Eigenvectors
gof=eigen.R$values/sum(eigen.R$values)*100 # Goodness-of fit
round(gof, 2)
par(mfrow=c(1,1))
plot(eigen.R$values, type="b", main="Scree Graph", xlab="Component Number", ylab="Eigenvalue")

#[Step 5] PCs : liner combination of original variables 
V2=V[,1:2]
V2

#[Step 6] PCS, PCs Scores and New Data Matrix P
Z=scale(X, scale=T) # Standardized Data Matrix
head(Z)
P=Z%*%V2            # PCs Scores
head(round(P, 3))

#[Step 7] Plot of PCs Scores
plot(P[,1], P[, 2], main="Plot of PCs Scores", xlab="1st PC", ylab="2nd PC")
text(P[,1], P[, 2], labels=rownames, cex=0.8, col="blue", pos=3)
abline(v=0, h=0)

### KLPGA PCbiplot
Data1.3.2<-read.table("klpga.txt", header=T)
X=Data1.3.2
n <- nrow(X) 
rownames(X)
colnames(X)

Y <- scale(X,scale=T)

# Biplot based on the Singular Value Decomposition
svd.Y <- svd(Y) 
U <- svd.Y$u    
V <- svd.Y$v 
D <- diag(svd.Y$d)
G <- (sqrt(n-1)*U)[,1:2]
H <- (sqrt(1/(n-1))*V%*%D)[,1:2]
rownames(G)<-rownames(X)
rownames(H)<-colnames(X) 

# Godness-of-fit
eig <- (svd.Y$d)^2 
per <- eig/sum(eig)*100
gof <- sum(per[1:2])
round(per, 2)
round(gof, 2)

# PC Biplot
lim<-range(pretty(G))
biplot(G,H, xlab="1st PC(71.83%)",ylab="2nd PC(18.64%)", main="Biplot for KLPGA Data ",
       xlim=lim,ylim=lim,cex=0.8,pch=16) 
#화살표와 같은방향이면 그 화살표가 높은점수 반대면 낮은점수
#화살표들이 같은방향을 가르키면 상관관계가 높다고 판단.
abline(v=0,h=0)


### skull data
# PCA Steps based on the SD for Skull Data

#[Step 1] Data Matrix X(standized data)
Data1.3.2<-read.table("skull.txt", header=T)
Z=as.matrix(Data1.3.2)
rownames<-rownames(Z) 
colnames<-colnames(Z)
n=nrow(Z)

#[Step 2] Covariance Matrix S(or Correlation Matix R)
R=(t(Z)%*%Z/(n-1))
R# =cov(Z)

#[Step 3] Spectral Decomposition 
eigen.R=eigen(R)
round(eigen.R$values, 2) # Eigenvalues
V=eigen.R$vectors # Eigenvectors
round(V, 2)

#[Step 4] Choice of Eigenvalues and Eigenvectors
gof=eigen.R$values/sum(eigen.R$values)*100 # Goodness-of fit
round(gof, 2)
plot(eigen.R$values, type="b", main="Scree Graph", xlab="Component Number", ylab="Eigenvalue")

#[Step 5] PCs : liner combination of original variables 
V3=V[,1:3]
round(t(V3), 2)
#t(x) <- x벡터의 transpose

#[Step 6] PCS, PCs Scores and New Data Matrix P
Z # Standardized Data Matrix
P=Z%*%V3            # PCs Scores
head(round(P, 3))

#[Step 7] Plot of PCs Scores
par(mfrow=c(2,2))
plot(P[,1], P[, 2], main="(a) Plot of PCs Scores", xlab="1st PC", ylab="2nd PC")
text(P[,1], P[, 2], labels=rownames, cex=0.8, col="blue", pos=1)
abline(v=0, h=0)

plot(P[,1], P[, 3], main="(b) Plot of PCs Scores", xlab="1st PC", ylab="3rd PC")
text(P[,1], P[, 3], labels=rownames, cex=0.8, col="blue", pos=1)
abline(v=0, h=0)

plot(P[,2], P[, 3], main="(c) Plot of PCs Scores", xlab="2nd PC", ylab="3rd PC")
text(P[,2], P[, 3], labels=rownames, cex=0.8, col="blue", pos=1)
abline(v=0, h=0)

Data1.3.2<-read.table("klpga.txt", header=T)
X<-Data1.3.2

# PCA based on the SD using princomp( )
pca.R<-princomp(X, cor=T)
summary(pca.R, loadings=T) # explanation, coefficient
round(pca.R$scores, 3)  # PC score
par(mfrow=c(1,1))
screeplot(pca.R, type="lines")

# Principle component biplot (SD)
biplot(pca.R, scale=0, xlab="1st PC",ylab="2nd PC",
       main="PC Biplot for KLPGA Data ")   
abline(v=0, h=0)


# PCA on the SVD using prcomp( )
pcasvd.Z<-prcomp(X, scale=T) 
summary(pcasvd.Z)  # explanation
round(pcasvd.Z$rotation, 3) # PC coefficient
pcasvd.Z$scale
screeplot(pcasvd.Z, type="lines")

# Principle component biplot (SVD)
biplot(pcasvd.Z, scale=0,  xlab="1st PC",ylab="2nd PC",
       main="PC Biplot for KLPGA Data ")
abline(v=0, h=0)

