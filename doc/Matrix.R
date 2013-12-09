### R code from vignette source 'Matrix.Rnw'
### Encoding: CP1250

###################################################
### code chunk number 1: Author:
###################################################
###################################################
##                                               ##
## (c) Andrej Blejec (andrej.blejec@nib.si)      ##
##                                               ##
###################################################


###################################################
### code chunk number 2: initialize
###################################################
options(width=65)
library(Hmisc)
#library(xlsReadWrite)
if(!interactive()) options(prompt=" ")


###################################################
### code chunk number 3: lfn
###################################################
lfn <- "Podatki2012.txt"
fpath <- "http://bit.ly/16oBVpR"


###################################################
### code chunk number 4: Preberi podatke
###################################################
fpath <- file.path("../data",lfn)
#fpath <- "http://bit.ly/16oBVpR"
data <- read.table(fpath,header=TRUE,sep="\t")
names(data)


###################################################
### code chunk number 5: Matrix.Rnw:102-103
###################################################
data <- data[data$starost<30,]


###################################################
### code chunk number 6: Matrix.Rnw:107-109
###################################################
data[data$mesec==0,"mesec"] <- NA
table(data$mesec)


###################################################
### code chunk number 7: majica
###################################################
table(data$majica)
data$majica[data$majica=="XL"] <- "L"
data$majica[data$majica=="XS"] <- "S"
data$majica <- ordered(data$majica,levels =c("S","M","L"))
str(data$majica)
table(data$majica)


###################################################
### code chunk number 8: Matrix.Rnw:136-141
###################################################
# vektorja x in y
x <- c(1,2,-1)
y <- c(2,1,6)
x
y


###################################################
### code chunk number 9: Matrix.Rnw:144-145
###################################################
length(x)


###################################################
### code chunk number 10: Matrix.Rnw:155-161
###################################################
2*x
x+y
x-y
x*y
x/y
x^y


###################################################
### code chunk number 11: make.matrix (eval = FALSE)
###################################################
## if(interactive()) {
##     X0<-make.matrix(n=5,m=2)
##     X0
##     }


###################################################
### code chunk number 12: Matrix.Rnw:191-197
###################################################
(B <- cbind(x,y))
(A <- matrix(c(1,2,-1,0),2,2))
(C <- (B%*%A))
t(C)
B+C



###################################################
### code chunk number 13: Matrix.Rnw:207-214
###################################################
X0 <- cbind(x,y)
X0
X <- X0
dim(X)
n<-dim(X)[1]
dimnames(X)



###################################################
### code chunk number 14: povprečja spremenljivk
###################################################
M<-apply(X0,2,mean)
M



###################################################
### code chunk number 15: matrika povprečij
###################################################
t(t(rep(1,n)))%*%t(M)



###################################################
### code chunk number 16: vsredinjena matrika podatkov
###################################################
X<-scale(X,scale=F)
X



###################################################
### code chunk number 17: vektor povprečij izvlečemo s funkcijo attr
###################################################
attr(X,"scaled:center")



###################################################
### code chunk number 18: Matrix.Rnw:256-258
###################################################
t(X)
X


###################################################
### code chunk number 19: SSP
###################################################
# SSP
C<-t(X)%*%X
C



###################################################
### code chunk number 20: Matrix.Rnw:280-286
###################################################
# kovariančna matrika
S<-C/(n-1)
S
cov(X)
diag(S)



###################################################
### code chunk number 21: Matrix.Rnw:296-301
###################################################
# matrika varianc
diag(diag(S))
SD1<-sqrt(diag(1/diag(S)))
SD1



###################################################
### code chunk number 22: Matrix.Rnw:311-315
###################################################
# korelacijska matrika
R<-SD1%*%S%*%SD1
R



###################################################
### code chunk number 23: Matrix.Rnw:325-331
###################################################
SX<-scale(X)
# pa še enkrat R
t(SX)%*%SX/(n-1)
# pa še enkrat
cor(X)



###################################################
### code chunk number 24: colMeans
###################################################
(X <- cbind(x,y))
(colMeans(X))
(attr(scale(X),"scaled:center"))
(apply(X,2,mean))



###################################################
### code chunk number 25: sd
###################################################
(sqrt(diag(var(X))))
(attr(scale(X),"scaled:scale"))
(apply(X,2,sd))



###################################################
### code chunk number 26: lastni vektorji
###################################################
S <- matrix(c( 10, 3, 3, 2 ),2,2)
S
eigen(S)
det(S)


###################################################
### code chunk number 27: Matrix.Rnw:407-410
###################################################
S1 <- solve(S)
S%*%S1



###################################################
### code chunk number 28: Matrix.Rnw:414-416
###################################################
zapsmall(S%*%S1)



###################################################
### code chunk number 29: Matrix.Rnw:421-422
###################################################
sum(diag(S))


###################################################
### code chunk number 30: Matrix.Rnw:430-432
###################################################
names(data)



###################################################
### code chunk number 31: Matrix.Rnw:450-453
###################################################
X <- data[,c("masa","visina","mesec","roke","cevelj")]
head(X)
tail(X)


###################################################
### code chunk number 32: Matrix.Rnw:461-462
###################################################
cor(X,use="complete.obs")


###################################################
### code chunk number 33: velikost
###################################################
t.test(visina~spol,data=data)


###################################################
### code chunk number 34: ttest
###################################################
student <- function(x,y){
# funkcija za dva vzorca
}


###################################################
### code chunk number 35: Matrix.Rnw:499-501
###################################################
n
sample(10,4)


###################################################
### code chunk number 36: Matrix.Rnw:511-521
###################################################
set.seed(555)
n1 <- 5; mu1 <- 10; sd1 <- 1
n2 <- 5; mu2 <- 10; sd2 <- 1
n <- n1+n2
X1 <- round(rnorm(n1,mu1,sd1),1)
X2 <- round(rnorm(n2,mu2,sd2),1)
X1
X2
X <- c(X1,X2)
ind <- c(rep(1,n1),rep(2,n2))


###################################################
### code chunk number 37: Matrix.Rnw:530-532
###################################################
dx <- function(x,y) mean(y)-mean(x)
dx(X1,X2)


###################################################
### code chunk number 38: Matrix.Rnw:540-544
###################################################
set.seed(432)
(smp1 <- sample(n,n1))
(smp2 <- (1:n)[-smp1])



###################################################
### code chunk number 39: vzorca
###################################################
plot(X,rep(0,n),col=ind,pch=16,ylab="")
points(X[smp1],rep(-1,n1))
points(X[smp2],rep(1,n2))
arrows(X[smp1],rep(0,n1),X[smp1],rep(-1,n1),col=ind[smp1])
arrows(X[smp2],rep(0,n2),X[smp2],rep(1,n2),col=ind[smp2])
abline(v=mean(X[smp1]),col=4)
abline(v=mean(X[smp2]),col=4)
points(c(mean(X[smp1]),mean(X[smp2])),c(-1,1),pch=16,col=4)
points(c(mean(X1),mean(X2)),c(0,0),col=4,cex=2)
dx(X[smp1],X[smp2])


###################################################
### code chunk number 40: Matrix.Rnw:570-580
###################################################
plot(X,rep(0,n),col=ind,pch=16,ylab="")
points(X[smp1],rep(-1,n1))
points(X[smp2],rep(1,n2))
arrows(X[smp1],rep(0,n1),X[smp1],rep(-1,n1),col=ind[smp1])
arrows(X[smp2],rep(0,n2),X[smp2],rep(1,n2),col=ind[smp2])
abline(v=mean(X[smp1]),col=4)
abline(v=mean(X[smp2]),col=4)
points(c(mean(X[smp1]),mean(X[smp2])),c(-1,1),pch=16,col=4)
points(c(mean(X1),mean(X2)),c(0,0),col=4,cex=2)
dx(X[smp1],X[smp2])


###################################################
### code chunk number 41: Matrix.Rnw:588-608
###################################################
perm.test <- function(x,X1,X2){
n1 <- length(X1)
n2 <- length(X2)
#
ind <- c(rep(1,n1),rep(2,n2))
#
(smp1 <- sample(n,n1))
(smp2 <- (1:n)[-smp1])
#
plot(X,rep(0,n),col=ind,pch=16,ylab="")
points(X[smp1],rep(-1,n1))
points(X[smp2],rep(1,n2))
arrows(X[smp1],rep(0,n1),X[smp1],rep(-1,n1),col=ind[smp1])
arrows(X[smp2],rep(0,n2),X[smp2],rep(1,n2),col=ind[smp2])
abline(v=mean(X[smp1]),col=4)
abline(v=mean(X[smp2]),col=4)
points(c(mean(X[smp1]),mean(X[smp2])),c(-1,1),pch=16,col=4)
points(c(mean(X1),mean(X2)),c(0,0),col=4,cex=2)
return(dx(X[smp1],X[smp2]))
}


###################################################
### code chunk number 42: Matrix.Rnw:617-620
###################################################
perm.test(x,X1,X2)
sapply(1:8,FUN=perm.test,X1=X1,X2=X2)
dx(X1,X2)


