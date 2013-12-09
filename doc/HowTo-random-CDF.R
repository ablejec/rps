### R code from vignette source 'HowTo-random-CDF.Rnw'
### Encoding: CP1250

###################################################
### code chunk number 1: Author
###################################################
###############################################
##                                           ##
## (c) Andrej Blejec (andrej.blejec@nib.si)  ##
##                                           ##
###############################################


###################################################
### code chunk number 2: initialize
###################################################
options(width=70)
#library(Hmisc)
#library(xlsReadWrite)


###################################################
### code chunk number 3: HowTo-random-CDF.Rnw:70-75
###################################################
x <- rnorm(100)
hist(x,prob=TRUE)
d <- density(x,from=min(x),to=max(x))
lines(d)



###################################################
### code chunk number 4: HowTo-random-CDF.Rnw:78-82
###################################################
F <- cumsum(d$y)
P <- F/max(F)
plot(d$x,P,type="l")



###################################################
### code chunk number 5: HowTo-random-CDF.Rnw:85-93
###################################################
n <- 100
u <- runif(n)
plot(d$x,P,type="l")
abline(h=u)
xout <- approx(P,d$x,u)
points(xout$y,xout$x)
abline(v=xout$y)
smpl <- xout$y


###################################################
### code chunk number 6: HowTo-random-CDF.Rnw:96-97
###################################################
hist(smpl)


###################################################
### code chunk number 7: rcdf
###################################################
rcdf <- function(n=1,x=rnorm(100),from=min(x),to=max(x),density=TRUE){
# n number of generated numbers or a vector of quantile ranks
N <- length(x)
if(density) {
d <- density(x,from=from,to=to)
F <- cumsum(d$y)
P <- F/max(F)
P[1] <- 0
x <- d$x
}
else
{
# kvantilni rangi posameznih vrednosti s popravkom za zveznost: 
# [0.5, N-0.5]/N
P <- (rank(x)-0.5)/N
}
#
if(length(n)==1) {
u <- runif(n, min(P),max(P) )
} else {
u <- n
}
#
xout <- approx(P,x,u)$y
return(xout)
}


###################################################
### code chunk number 8: HowTo-random-CDF.Rnw:130-131
###################################################
P <- function(x) (rank(x)-0.5)/length(x)


###################################################
### code chunk number 9: HowTo-random-CDF.Rnw:134-135
###################################################
set.seed(1234)


###################################################
### code chunk number 10: HowTo-random-CDF.Rnw:140-163
###################################################
par(mfrow=c(2,2),mar=c(4,4,0.5,0.5))
seed <- 670 #round(runif(1,1,1000))
n <-1000
x <- rnorm(50)
set.seed(seed)
rdens <- rcdf(n,x,density=TRUE)
set.seed(seed)
rpoly <- rcdf(n,x,density=FALSE)
plot(rdens,rpoly)
abline(c(0,1))
qqplot(x,rdens)
abline(c(0,1))
qqplot(x,rpoly)
abline(c(0,1))
x <- sort(x)
rdens <- sort(rdens)
rpoly <- sort(rpoly)
plot(x,P(x),type="l",lwd=5)
lines(rdens,P(rdens),type="l",col="red",lwd=4)
#lines(x,P(x),type="p",lwd=5)
lines(rpoly,P(rpoly),type="l",col="green",lwd=2)
legend("topleft",legend=c("x","density","polygon"),col=c("black","red","green"),
lwd=4,bty="n")


###################################################
### code chunk number 11: HowTo-random-CDF.Rnw:166-172
###################################################
par(mfrow=c(1,2))
xdens <- rcdf(P(x),x,density=TRUE)
plot(x,xdens)
xpoly <- rcdf(P(x),x,density=FALSE)
plot(x,xpoly)



###################################################
### code chunk number 12: HowTo-random-CDF.Rnw:176-180
###################################################
n <- 10000
x <- rchisq(1000,1)
smpl <- rcdf(n,x)
qqplot(x,smpl)


###################################################
### code chunk number 13: HowTo-random-CDF.Rnw:183-187
###################################################
n <- 10000
x <- rnorm(1000)
smpl <- rcdf(n,x)
qqplot(x,smpl)


###################################################
### code chunk number 14: sessionInfo
###################################################
cat(win.version(),"\n")
toLatex(sessionInfo())
cat("Project path:\\verb'",dirname(getwd()),"'\n")


###################################################
### code chunk number 15: projectFiles
###################################################
mainFile <- commandArgs(trailingOnly = TRUE)
mainFile <- strsplit(mainFile,'.',fixed=TRUE)[[1]][1]
projectName <- rev((strsplit(dirname(getwd()), "/"))[[1]])[1]
cat('> projectName <-"',projectName,'"; ',sep="")
cat(' mainFile <-"',mainFile,'"',sep="")


###################################################
### code chunk number 16: vignette (eval = FALSE)
###################################################
## commandArgs()
## library(tkWidgets)
## # getrootpath <- function() {
## # fp <- (strsplit(getwd(), "/"))[[1]]
## # file <- file.path(paste(fp[-length(fp)], collapse = "/"))
## # return(file)
## # }
## # fileName <- function(name="bla",ext="PDF") paste(name,ext,sep=".")
##  openPDF(file.path(dirname(getwd()),"doc",paste(mainFile,"PDF",sep=".")))
##  viewVignette("viewVignette", projectName, file.path("../doc",paste(mainFile,"RNW",sep=".")))
## 


