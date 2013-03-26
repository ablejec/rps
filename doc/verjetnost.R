### R code from vignette source 'verjetnost.Rnw'
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
options(width=60)
library(Hmisc)
#library(xlsReadWrite)


###################################################
### code chunk number 3: verjetnost.Rnw:108-112
###################################################
n <- 80
k <- 350
p <- 0.2
choose(k-1,n-1)*p^n*(1-p)^(k-n)


###################################################
### code chunk number 4: verjetnost.Rnw:115-117
###################################################
dnbinom(k-n,n,p)
pnbinom(k-n,n,p)-pnbinom(k-n-1,n,p)


###################################################
### code chunk number 5: verjetnost.Rnw:120-121
###################################################
pnbinom(k-n,n,p)


###################################################
### code chunk number 6: verjetnost.Rnw:135-136
###################################################
pnorm(350,400,40)


###################################################
### code chunk number 7: verjetnost.Rnw:145-146
###################################################
n <- 80


###################################################
### code chunk number 8: verjetnost.Rnw:152-156
###################################################
set.seed(789)
X <- rgeom(n,p=0.2)+1
X[1:5]
(Y <- sum(X))


###################################################
### code chunk number 9: verjetnost.Rnw:166-171
###################################################
par (mar=c(3,3,2,1), mgp=c(2,.7,0))
X <- c(0,X)
plot(cumsum(X),0:n,type="s", ylab="Biserov", xlab="Število potopov")
points(cumsum(X[-1]),1:n,col="red",pch=16,cex=.5)



###################################################
### code chunk number 10: verjetnost.Rnw:179-183
###################################################
n <- 350
skoljka <- c("prazna","biser")
izid <- sample(skoljka,n,replace=TRUE)
table(izid)


###################################################
### code chunk number 11: verjetnost.Rnw:191-196
###################################################
n <- 350
skoljka <- c("prazna","biser")
izid <- sample(skoljka,n,replace=TRUE,prob=c(8,2))
(t <- table(izid))
t["biser"]/sum(t)


###################################################
### code chunk number 12: verjetnost.Rnw:205-209
###################################################
p0 <- 0.2
izid <- runif(n)<=p0
(t <- table(izid))
t[2]/sum(t)


###################################################
### code chunk number 13: verjetnost.Rnw:212-214
###################################################
sum(izid)
sum(izid)/length(izid)


###################################################
### code chunk number 14: verjetnost.Rnw:223-228
###################################################
N <- 100
n <- 350
p <- 0.2
X <- matrix(runif(N*n)<=p,N,n)
X[1:4,1:10]+0


###################################################
### code chunk number 15: verjetnost.Rnw:230-232
###################################################
biserov <- apply(X,1,sum)
head(biserov)


###################################################
### code chunk number 16: verjetnost.Rnw:240-241
###################################################
hist(biserov,col="lightblue")


###################################################
### code chunk number 17: verjetnost.Rnw:250-255
###################################################
N <- 10000
n <- 80
p <- 0.2
X <- matrix(rgeom(N*n,p=p),N,n)+1
X[1:4,1:10]


###################################################
### code chunk number 18: verjetnost.Rnw:258-260
###################################################
potopov <- apply(X,1,sum)
head(potopov)


###################################################
### code chunk number 19: verjetnost.Rnw:269-272
###################################################
p <- sum(potopov<=350)/length(potopov)
hist(potopov,col="lightblue",main=p)
abline(v=350,col="red")


