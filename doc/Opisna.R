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
options(width=60)
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
### code chunk number 6: opisna
###################################################
summary(data[,7:dim(data)[2]])


###################################################
### code chunk number 7: Opisna.Rnw:144-147
###################################################
summary(data$mati)
by(data$visina,data$spol,summary)
summary(data$oce)


###################################################
### code chunk number 8: Opisna.Rnw:168-171
###################################################
attach(data)
length(visina)
visina[1:5]


###################################################
### code chunk number 9: Opisna.Rnw:182-183
###################################################
plot(visina)


###################################################
### code chunk number 10: Opisna.Rnw:191-192
###################################################
plot(visina, pch=16,col=spol)


###################################################
### code chunk number 11: Opisna.Rnw:200-202
###################################################
x <- visina
plot(x,rank(x),pch=16,col=spol)


###################################################
### code chunk number 12: Opisna.Rnw:211-216
###################################################
x <- visina
plot(x,rank(x),pch=16,col=spol)
q <- qnorm((rank(x)-0.5)/length(x),mean(x),sd(x))
points(q,rank(x),col=4)
segments(x,rank(x),q,rank(x))


###################################################
### code chunk number 13: Opisna.Rnw:224-226
###################################################
qqnorm(visina,col=spol,pch=16)
qqline(visina)


###################################################
### code chunk number 14: Opisna.Rnw:234-237
###################################################
boxplot(visina~spol,col=c("pink","lightblue"))
rug(visina[spol=="F"],side=2)
rug(visina[spol=="M"],side=4)


###################################################
### code chunk number 15: Opisna.Rnw:249-250
###################################################
t.test(visina~spol)


###################################################
### code chunk number 16: Opisna.Rnw:253-254 (eval = FALSE)
###################################################
## t.test(visina[spol=="F"],visina[spol=="M"])


###################################################
### code chunk number 17: sl1 (eval = FALSE)
###################################################
## with(data, plot(oce, visina, col=spol, pch=16,xlim=range(visina)))
## abline(c(0,1),col="blue")
## abline(lm(visina~oce, data=data), col=3,lwd=3)
## abline(lm(visina~oce, data=data[data$spol=="M",]), col="red", lwd=3)
## abline(lm(visina~oce, data=data[data$spol=="F",]),  lwd=3)


###################################################
### code chunk number 18: moski
###################################################
with(data, plot(oce, visina, col=spol, pch=16,xlim=range(visina)))
abline(c(0,1),col="blue")
abline(lm(visina~oce, data=data), col=3,lwd=3)
abline(lm(visina~oce, data=data[data$spol=="M",]), col="red", lwd=3)
abline(lm(visina~oce, data=data[data$spol=="F",]),  lwd=3)


###################################################
### code chunk number 19: sl2 (eval = FALSE)
###################################################
## with(data, plot(mati, visina, col=spol, pch=16,xlim=range(visina)))
## abline(c(0,1),col="blue")
## abline(lm(visina~mati, data=data), col=3,lwd=3)
## abline(lm(visina~mati, data=data[data$spol=="M",]), col="red", lwd=3)
## abline(lm(visina~mati, data=data[data$spol=="F",]),  lwd=3)
## 


###################################################
### code chunk number 20: zenske
###################################################
with(data, plot(mati, visina, col=spol, pch=16,xlim=range(visina)))
abline(c(0,1),col="blue")
abline(lm(visina~mati, data=data), col=3,lwd=3)
abline(lm(visina~mati, data=data[data$spol=="M",]), col="red", lwd=3)
abline(lm(visina~mati, data=data[data$spol=="F",]),  lwd=3)



###################################################
### code chunk number 21: moski2
###################################################
par(mfrow=c(1,2),mar=c(4,3,1,0))
with(data, plot(oce, visina, col=spol, pch=16,xlim=range(visina)))
abline(c(0,1),col="blue")
abline(lm(visina~oce, data=data), col=3,lwd=3)
abline(lm(visina~oce, data=data[data$spol=="M",]), col="red", lwd=3)
abline(lm(visina~oce, data=data[data$spol=="F",]),  lwd=3)
with(data, plot(mati, visina, col=spol, pch=16,xlim=range(visina)))
abline(c(0,1),col="blue")
abline(lm(visina~mati, data=data), col=3,lwd=3)
abline(lm(visina~mati, data=data[data$spol=="M",]), col="red", lwd=3)
abline(lm(visina~mati, data=data[data$spol=="F",]),  lwd=3)



###################################################
### code chunk number 22: sessionInfo
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
### code chunk number 23: projectFiles
###################################################
cat('> projectName <-"',projectName,'"; ',sep="")
cat(' mainFile <-"',mainFile,'"',sep="")
#


###################################################
### code chunk number 24: vignette (eval = FALSE)
###################################################
## commandArgs()
## library(tkWidgets)
## openPDF(file.path(dirname(getwd()),"doc",
## paste(mainFile,"PDF",sep=".")))
## viewVignette("viewVignette", projectName, #
## file.path("../doc",paste(mainFile,"Rnw",sep=".")))
## #


