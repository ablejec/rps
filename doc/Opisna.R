### R code from vignette source 'Opisna.Rnw'
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
if(!interactive()) options(width=60,prompt="  ",cont="  ")
#library(Hmisc)
#library(xlsReadWrite)


###################################################
### code chunk number 3: lfn
###################################################
lfn <- "Podatki2012.txt"



###################################################
### code chunk number 4: Preberi podatke
###################################################
#fpath <- file.path("../data",lfn)
fpath <- "http://bit.ly/16oBVpR"
data <- read.table(fpath,header=TRUE,sep="\t")
names(data)


###################################################
### code chunk number 5: povzetek
###################################################
summary(data[,1:6])


###################################################
### code chunk number 6: Opisna.Rnw:129-130
###################################################
data <- data[data$starost<30,]


###################################################
### code chunk number 7: Opisna.Rnw:134-136
###################################################
data[data$mesec==0,"mesec"] <- NA
table(data$mesec)


###################################################
### code chunk number 8: opisna
###################################################
summary(data[,7:dim(data)[2]])


###################################################
### code chunk number 9: Opisna.Rnw:161-164
###################################################
summary(data$mati)
by(data$visina,data$spol,summary)
summary(data$oce)


###################################################
### code chunk number 10: Opisna.Rnw:185-188
###################################################
attach(data)
length(visina)
visina[1:5]


###################################################
### code chunk number 11: Opisna.Rnw:199-200
###################################################
plot(visina)


###################################################
### code chunk number 12: Opisna.Rnw:208-209
###################################################
plot(visina, pch=16,col=spol)


###################################################
### code chunk number 13: Opisna.Rnw:217-219
###################################################
x <- visina
plot(x,rank(x),pch=16,col=spol)


###################################################
### code chunk number 14: Opisna.Rnw:228-233
###################################################
x <- visina
plot(x,rank(x),pch=16,col=spol)
q <- qnorm((rank(x)-0.5)/length(x),mean(x),sd(x))
points(q,rank(x),col=4)
segments(x,rank(x),q,rank(x))


###################################################
### code chunk number 15: Opisna.Rnw:241-243
###################################################
qqnorm(visina,col=spol,pch=16)
qqline(visina)


###################################################
### code chunk number 16: Opisna.Rnw:251-254
###################################################
boxplot(visina~spol,col=c("pink","lightblue"))
rug(jitter(visina[spol=="F"]),side=2)
rug(jitter(visina[spol=="M"]),side=4)


###################################################
### code chunk number 17: Opisna.Rnw:266-267
###################################################
t.test(visina~spol)


###################################################
### code chunk number 18: Opisna.Rnw:270-271 (eval = FALSE)
###################################################
## t.test(visina[spol=="F"],visina[spol=="M"])


###################################################
### code chunk number 19: Opisna.Rnw:292-293
###################################################
t.test(masa~spol)


###################################################
### code chunk number 20: Opisna.Rnw:296-299
###################################################
boxplot(masa~spol,col=c("pink","lightblue"))
rug(jitter(masa[spol=="F"]),side=2)
rug(jitter(masa[spol=="M"]),side=4)


###################################################
### code chunk number 21: sl1 (eval = FALSE)
###################################################
## with(data, plot(oce, visina, col=spol, pch=16,xlim=range(visina)))
## abline(c(0,1),col="blue")
## abline(lm(visina~oce, data=data), col=3,lwd=3)
## abline(lm(visina~oce, data=data[data$spol=="M",]), col="red", lwd=3)
## abline(lm(visina~oce, data=data[data$spol=="F",]), lwd=3)


###################################################
### code chunk number 22: moski
###################################################
with(data, plot(oce, visina, col=spol, pch=16,xlim=range(visina)))
abline(c(0,1),col="blue")
abline(lm(visina~oce, data=data), col=3,lwd=3)
abline(lm(visina~oce, data=data[data$spol=="M",]), col="red", lwd=3)
abline(lm(visina~oce, data=data[data$spol=="F",]), lwd=3)


###################################################
### code chunk number 23: sl2 (eval = FALSE)
###################################################
## with(data, plot(mati, visina, col=spol, pch=16,xlim=range(visina)))
## abline(c(0,1),col="blue")
## abline(lm(visina~mati, data=data), col=3,lwd=3)
## abline(lm(visina~mati, data=data[data$spol=="M",]), col="red", lwd=3)
## abline(lm(visina~mati, data=data[data$spol=="F",]),  lwd=3)


###################################################
### code chunk number 24: zenske
###################################################
with(data, plot(mati, visina, col=spol, pch=16,xlim=range(visina)))
abline(c(0,1),col="blue")
abline(lm(visina~mati, data=data), col=3,lwd=3)
abline(lm(visina~mati, data=data[data$spol=="M",]), col="red", lwd=3)
abline(lm(visina~mati, data=data[data$spol=="F",]),  lwd=3)


###################################################
### code chunk number 25: moski2
###################################################
par(mfrow=c(1,2),mar=c(4,3,1,0))
with(data, plot(oce, visina, col=spol, pch=16,xlim=range(visina)))
abline(c(0,1),col="blue")
abline(lm(visina~oce, data=data), col=3,lwd=3)
abline(lm(visina~oce, data=data[data$spol=="M",]), col="red", lwd=3)
abline(lm(visina~oce, data=data[data$spol=="F",]), lwd=3)
with(data, plot(mati, visina, col=spol, pch=16,xlim=range(visina)))
abline(c(0,1),col="blue")
abline(lm(visina~mati, data=data), col=3,lwd=3)
abline(lm(visina~mati, data=data[data$spol=="M",]), col="red", lwd=3)
abline(lm(visina~mati, data=data[data$spol=="F",]),  lwd=3)


###################################################
### code chunk number 26: Opisna.Rnw:375-377
###################################################
fit <- lm(visina~oce,data=data)
summary(fit)


###################################################
### code chunk number 27: Opisna.Rnw:385-387
###################################################
par(mfrow=c(2,2))
plot(fit,col=spol)


###################################################
### code chunk number 28: Opisna.Rnw:393-396
###################################################
fit <- lm(visina~spol*oce,data=data)
summary(fit)



###################################################
### code chunk number 29: Opisna.Rnw:399-401
###################################################
par(mfrow=c(2,2))
plot(fit,col=spol)


###################################################
### code chunk number 30: Opisna.Rnw:408-410
###################################################
plot(visina,masa)
abline(lm(masa~visina))


###################################################
### code chunk number 31: Opisna.Rnw:418-421
###################################################
cor(masa,visina)
fit <- lm(masa~visina)
summary(fit)


###################################################
### code chunk number 32: Opisna.Rnw:429-431
###################################################
fit <- lm(masa~visina*spol)
summary(fit)


###################################################
### code chunk number 33: Opisna.Rnw:439-442
###################################################
plot(visina,masa,col=spol)
abline(lm(masa~visina))
points(visina,predict(fit),pch=16,col=spol)


###################################################
### code chunk number 34: Opisna.Rnw:450-453
###################################################
fvis <- cut(visina,breaks=c(155,165,175,200), labels=c("M","S","V"))
table(fvis)
(m <- by(masa,fvis,mean))


###################################################
### code chunk number 35: figaov
###################################################
par(mfrow=c(1,2))
plot(fvis,masa)
points(1:3,m,pch=16,cex=2)
plot(as.numeric(fvis), jitter(masa))
points(1:3,m,pch=16,cex=2,type="b")
abline(h=mean(masa))


###################################################
### code chunk number 36: Opisna.Rnw:475-476 (eval = FALSE)
###################################################
## par(mfrow=c(1,2))
## plot(fvis,masa)
## points(1:3,m,pch=16,cex=2)
## plot(as.numeric(fvis), jitter(masa))
## points(1:3,m,pch=16,cex=2,type="b")
## abline(h=mean(masa))


###################################################
### code chunk number 37: Opisna.Rnw:486-488
###################################################
fit <- lm(masa~0+fvis)
summary(fit)


###################################################
### code chunk number 38: Opisna.Rnw:496-498
###################################################
fit <- lm(I(masa-mean(masa))~0+fvis)
summary(fit)


###################################################
### code chunk number 39: sessionInfo
###################################################
cat(win.version(),"\n")
toLatex(sessionInfo())
cat("Project path:\\verb'",dirname(getwd()),"'\\\\\n")
mainFile <- commandArgs(trailingOnly = TRUE)
#
mainFilePath <- file.path("../doc", mainFile[1])
cat("Main file :\\verb'", mainFilePath, "'\n")
#
mainFile <- strsplit(mainFile,'.',fixed=TRUE)[[1]][1]
projectName <- rev((strsplit(dirname(getwd()), "/"))[[1]])[1]
#


###################################################
### code chunk number 40: projectFiles
###################################################
cat('> projectName <-"',projectName,'"; ',sep="")
cat(' mainFile <-"',mainFile,'"',sep="")
#


###################################################
### code chunk number 41: vignette (eval = FALSE)
###################################################
## commandArgs()
## library(tkWidgets)
## openPDF(file.path(dirname(getwd()),"doc",
## paste(mainFile,"PDF",sep=".")))
## viewVignette("viewVignette", projectName, #
## file.path("../doc",paste(mainFile,"Rnw",sep=".")))
## #


###################################################
### code chunk number 42: Opisna.Rnw:555-561 (eval = FALSE)
###################################################
## # getrootpath <- function() {
## # fp <- (strsplit(getwd(), "/"))[[1]]
## # file <- file.path(paste(fp[-length(fp)], collapse = "/"))
## # return(file)
## # }
## # fileName <- function(name="bla",ext="PDF") paste(name,ext,sep=".")


