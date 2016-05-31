#导入数据分析统计量
algae<-read.table('C:/HZTAO/course/dataMining/R/Analysis.txt',header=T,dec='.',na.strings=c('XXXXXXX'))
summary(algae)

#画直方图
hist(algae$mxPH)
hist(algae$mnO2)
hist(algae$Cl)
hist(algae$NO3)
hist(algae$NH4)
hist(algae$oPO4)
hist(algae$PO4)
hist(algae$Chla)

#画QQ图
library(car)
qqPlot(algae$mxPH,main='Normal QQ plot of maximum pH')
qqPlot(algae$mnO2,main='Normal QQ plot of mnO2')
qqPlot(algae$Cl,main='Normal QQ plot of Cl')
qqPlot(algae$NO3,main='Normal QQ plot of NO3')
qqPlot(algae$NH4,main='Normal QQ plot of NH4')
qqPlot(algae$oPO4,main='Normal QQ plot of oPO4')
qqPlot(algae$PO4,main='Normal QQ plot of PO4')
qqPlot(algae$Chla,main='Normal QQ plot of Chla')

#画盒图
boxplot(algae$mxPH,ylab='mxPH')
rug(algae$mxPH,side=4)
abline(h=mean(algae$mxPH,na.rm=T),lty=2)

boxplot(algae$mnO2,ylab='mnO2')
rug(algae$mnO2,side=4)
abline(h=mean(algae$mnO2,na.rm=T),lty=2)

boxplot(algae$Cl,ylab='Cl')
rug(algae$Cl,side=4)
abline(h=mean(algae$Cl,na.rm=T),lty=2)

boxplot(algae$NO3,ylab='NO3')
rug(algae$NO3,side=4)
abline(h=mean(algae$NO3,na.rm=T),lty=2)

boxplot(algae$NH4,ylab='NH4')
rug(algae$NH4,side=4)
abline(h=mean(algae$NH4,na.rm=T),lty=2)

boxplot(algae$oPO4,ylab='oPO4')
rug(algae$oPO4,side=4)
abline(h=mean(algae$oPO4,na.rm=T),lty=2)

boxplot(algae$PO4,ylab='PO4')
rug(algae$PO4,side=4)
abline(h=mean(algae$PO4,na.rm=T),lty=2)

boxplot(algae$Chla,ylab='Chla')
rug(algae$Chla,side=4)
abline(h=mean(algae$Chla,na.rm=T),lty=2)

#画a1-a7的条件盒图
library(lattice)
bwplot(size~a1,data=algae,ylab='river size',xlab='Alga A1')
bwplot(size~a2,data=algae,ylab='river size',xlab='Alga A2')
bwplot(size~a3,data=algae,ylab='river size',xlab='Alga A3')
bwplot(size~a4,data=algae,ylab='river size',xlab='Alga A4')
bwplot(size~a5,data=algae,ylab='river size',xlab='Alga A5')
bwplot(size~a6,data=algae,ylab='river size',xlab='Alga A6')
bwplot(size~a7,data=algae,ylab='river size',xlab='Alga A7')

#画a1-a7的分段箱图，作业没做要求，但是已画
library(Hmisc)
bwplot(size~a1,data=algae,panel=panel.bpplot,probs=seq(.01,.49,by=.01),datadensity=T,ylab='river size',xlab='Alga A1')
bwplot(size~a2,data=algae,panel=panel.bpplot,probs=seq(.01,.49,by=.01),datadensity=T,ylab='river size',xlab='Alga A2')
bwplot(size~a3,data=algae,panel=panel.bpplot,probs=seq(.01,.49,by=.01),datadensity=T,ylab='river size',xlab='Alga A3')
bwplot(size~a4,data=algae,panel=panel.bpplot,probs=seq(.01,.49,by=.01),datadensity=T,ylab='river size',xlab='Alga A4')
bwplot(size~a5,data=algae,panel=panel.bpplot,probs=seq(.01,.49,by=.01),datadensity=T,ylab='river size',xlab='Alga A5')
bwplot(size~a6,data=algae,panel=panel.bpplot,probs=seq(.01,.49,by=.01),datadensity=T,ylab='river size',xlab='Alga A6')
bwplot(size~a7,data=algae,panel=panel.bpplot,probs=seq(.01,.49,by=.01),datadensity=T,ylab='river size',xlab='Alga A7')


#数据处理

#将缺失部分剔除
algae[!complete.cases(algae),]
nrow(algae[!complete.cases(algae),])
algae<-na.omit(algae)


#将最高频率值来填补缺失值
algae<-read.table('C:/HZTAO/course/dataMining/R/Analysis.txt',header=T,dec='.',na.strings=c('XXXXXXX'))
nrow(algae[!complete.cases(algae),])
algae[is.na(algae$season),'season']<-"winter"
algae[is.na(algae$size),'size']<-"medium"
algae[is.na(algae$speed),'speed']<-"high"
algae[is.na(algae$mxPH),'mxPH']<-median(algae$mxPH,na.rm=T)
algae[is.na(algae$mnO2),'mnO2']<-median(algae$mnO2,na.rm=T)
algae[is.na(algae$Cl),'Cl']<-median(algae$Cl,na.rm=T)
algae[is.na(algae$NO3),'NO3']<-median(algae$NO3,na.rm=T)
algae[is.na(algae$NH4),'NH4']<-median(algae$NH4,na.rm=T)
algae[is.na(algae$oPO4),'oPO4']<-median(algae$oPO4,na.rm=T)
algae[is.na(algae$PO4),'PO4']<-median(algae$PO4,na.rm=T)
algae[is.na(algae$Chla),'Chla']<-median(algae$Chla,na.rm=T)
algae[is.na(algae$a1),'a1']<-median(algae$a1,na.rm=T)
algae[is.na(algae$a2),'a2']<-median(algae$a2,na.rm=T)
algae[is.na(algae$a3),'a3']<-median(algae$a3,na.rm=T)
algae[is.na(algae$a4),'a4']<-median(algae$a4,na.rm=T)
algae[is.na(algae$a5),'a5']<-median(algae$a5,na.rm=T)
algae[is.na(algae$a6),'a6']<-median(algae$a6,na.rm=T)
algae[is.na(algae$a7),'a7']<-median(algae$a7,na.rm=T)
nrow(algae[!complete.cases(algae),])

#相关性
library(DMwR)
algae<-read.table('C:/HZTAO/course/dataMining/R/Analysis.txt',header=T,dec='.',na.strings=c('XXXXXXX'))
nrow(algae[!complete.cases(algae),])
symnum(cor(algae[,4:18],use="complete.obs"))
lm(formula=PO4~oPO4,data=algae)
algae[28,"PO4"]<-42.897+1.293*algae[28,"oPO4"]


#相似性
library(DMwR)
clean.algae<-knnImputation(algae,k=10)