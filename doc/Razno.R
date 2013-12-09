### R code from vignette source 'Razno.Rnw'
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
if(!interactive()) options(prompt=" ",continue=" ")


###################################################
### code chunk number 3: Razno.Rnw:105-106
###################################################
if(interactive()) help("if")


###################################################
### code chunk number 4: Razno.Rnw:116-121
###################################################
x  <-  3
meja <- 5
if (x < meja) cat(x, "je manj od", meja,"\n")
if (x  < meja) cat(x, "je manj od", meja,"\n")  else
cat( "Večje\n" )


###################################################
### code chunk number 5: Razno.Rnw:132-141
###################################################
x <- 3
meja <- 5
if (x  < meja) {
lbl <- "Vsota: "
y <- x+meja
} else
{ lbl <- "Produkt: "
y <- x*meja}
cat(lbl,y,"\n")


###################################################
### code chunk number 6: Razno.Rnw:150-153
###################################################
x <- c(2:-2)
ifelse(x >= 0, x, NA)
sqrt(ifelse(x >= 0, x, NA))  # no warning


###################################################
### code chunk number 7: Razno.Rnw:160-171
###################################################
centre <- function(x, type) {
  switch(type,
         mean = mean(x),
         median = median(x),
         trimmed = mean(x, trim = .1))
}
x <- rcauchy(10)
centre(x, "mean")
centre(x, "median")
centre(x, "trimmed")



###################################################
### code chunk number 8: Razno.Rnw:181-186
###################################################
for (x in 10:13){
y <- x^2
print(y)
}
for(lbl in c("a","e","i","o")) cat(paste("šal",lbl,", ",sep=""))


###################################################
### code chunk number 9: Razno.Rnw:194-201 (eval = FALSE)
###################################################
## n <- seq(1,30,.01)
## x <- seq(-5,5,length=100)
## for (df in n) {
## plot(x,dt(x,df),type="l",lwd=7,col=4,main=df,ylim=c(0,0.4))
## lines(x,dnorm(x),type="l",lwd=3,col=2)
##  }
## df


###################################################
### code chunk number 10: Razno.Rnw:211-217
###################################################
set.seed(123)
(x <- as.numeric(runif(20)<0.2))
i <- 1
while(x[i]!=1){
i <- i+1}
i


###################################################
### code chunk number 11: Razno.Rnw:221-223
###################################################
which(x==1)
which(x==1)[1]


###################################################
### code chunk number 12: Razno.Rnw:231-238
###################################################
x
i <- 1
repeat{
if( x[i] == 1) break
i <- i+1
}
i


###################################################
### code chunk number 13: Razno.Rnw:246-256
###################################################
stars <- function(p){
alfa <- c(0.05,0.01,0.001,0)
st <- ""
i <- 1
while(p <= alfa[i]){
st <- paste(st,"*",sep="")
i <- i+1
}
if(st=="") st <- "NS"
return(st)}


###################################################
### code chunk number 14: Razno.Rnw:259-262
###################################################
p <- 0.002
cat("p =",p,stars(p),"\n")
sapply(c(0.5,0.05,0.005,0.0005),stars)


###################################################
### code chunk number 15: Razno.Rnw:274-275
###################################################
.libPaths()


###################################################
### code chunk number 16: Razno.Rnw:293-356
###################################################
mleMean <- function(n=20,As=NULL, mu=3,sd=1){
par(mar=c(4,4,1,1),oma=c(0,0,0,0),mfrow=c(2,1))
x <- mu+scale(rnorm(n))*sd
maxl <- log(prod(dnorm(x,mu,sd)))
minl <- log(prod(dnorm(x,mu-3*sd,sd)))
maxl <- maxl+0.1*(maxl-minl)
maxd <- max(dnorm(x,mu,sd))
xlim <- c(mu-3*sd,mu+3*sd)
#
if(!is.null(As)) As <- sort(As)
A <- NULL
L <- NULL
plot(x,dnorm(x,mu,sd),type="n",xlim=xlim,
ylim=c(0,maxd),ylab="Density")
axis(3,at=x,lab=NULL,lwd=3,tick=0.05,col="blue")
#
plot(x,dnorm(x,mu,sd),type="n",xlim=c(0,6),
ylim=c(minl,maxl),ylab="log Likelihood")
i <- 1
if(is.null(As)) a <- locator(1)$x else a <- As[i]
#
while(!is.null(a)){

plot(x,dnorm(x,mu,sd),type="n",xlim=xlim,
ylim=c(0,maxd),ylab="Density")
axis(3,at=x,lab=NULL,lwd=3,tick=0.05,col="blue")
#
segments(x,0,x,dnorm(x,mean=a,sd=sd),lwd=3)
points(a,par("usr")[4],pch=16,cex=2,col="red")
plot(x,dnorm(x,mu,sd),type="n",xlim=xlim,
ylim=c(minl,maxl),ylab="log Likelihood")
A <- c(A,a)
L <- c(L,log(prod(dnorm(x,a,sd))))
points(A,L,cex=1.5,lwd=3)
points(a,rev(L)[1],pch=21,bg="red",cex=1.5)
i <- i+1
if(is.null(As)) a <- locator(1)$x else {
if (i<length(As))
 a <- As[i] else a <- NULL
 }
}
id <- which(L==max(L))[1]
a <- A[id]
A <- c(A,a)
L <- c(L,log(prod(dnorm(x,a,sd))))
## replot
plot(x,dnorm(x,mu,sd),type="n",xlim=xlim,
ylim=c(0,maxd),ylab="Density")
axis(3,at=x,lab=NULL,lwd=3,tick=0.05,col="blue")
#
segments(x,0,x,dnorm(x,mean=a,sd=sd),lwd=3)
points(a,par("usr")[4],pch=16,cex=2,col="red")
plot(x,dnorm(x,mu,sd),type="n",xlim=xlim,
ylim=c(minl,maxl),ylab="log Likelihood")
points(A,L,cex=1.5,lwd=3)
points(a,rev(L)[1],pch=21,bg="red",cex=1.5)
##

points(mu,par("usr")[3],col="green",xpd=TRUE,cex=2,pch=16)
abline(v=A[id],col="blue",lwd=2)

invisible(list(x=x,A=A,L=L))
}


###################################################
### code chunk number 17: Razno.Rnw:359-360
###################################################
source("../../animatoR/doc/animatoR-functions.r")


###################################################
### code chunk number 18: mleMean
###################################################
set.seed(1222)
mleMean(mu=10,sd=3,As=seq(0,20,length=101))

includeLatex(scale=.45,poster="first")


