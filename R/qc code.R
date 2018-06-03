##############
##QC1
setwd('N:/크레이데이터')

t.data<-read.csv('usn(data)_time.csv')
colnames(t.data)
colnames(t.data)<-c('id','year','mon','day','date','s_30','s_60','s_90'
                    ,'s_temp','light','rh','temp','light.1','ws','wd','prec','s_trans')

t.data$date2<-as.Date(paste0(t.data$year,'-',t.data$mon,'-',t.data$day))
t.data$time<-as.numeric(substr(t.data$date,12,13))

t.data2<-t.data[,-c(5,13,15)]
colnames(t.data)
t.data2$light<-(t.data2$light*3600/10^6)#w/m^2->mj/m^2

#1 MJ / m2 = 10^6 J / m2 = 10^6 W · s / m2 = 10^6/3600 W · h / m2 = 277.778 Wh / m2
#기상청 Mj/m^2 이고 ㅋㅋㅋusn W/m^2
t.data<-t.data2
rm(list=ls(pattern='t.data2'))
t.data[is.na(t.data$s_trans),'s_trans']<-0
###############
#기기 정지로 추측되는 비율
#(del<-table(apply(t.data[,-c(1:4,15:16)],1,sum)==0))
library(DUcj)
qc1_2<-apply(t.data,2,qc1)
qc1_2<-as.data.frame(qc1_2)

t.data2<-t.data

t.data2[(qc1_2$s_30>5)|apply(t.data[,-c(1:4,15,16)],1,sum)==0,5]<-NA
t.data2[(qc1_2$s_60>5)|apply(t.data[,-c(1:4,15,16)],1,sum)==0,6]<-NA
t.data2[(qc1_2$s_90>5)|apply(t.data[,-c(1:4,15,16)],1,sum)==0,7]<-NA
t.data2[(qc1_2$s_temp>10)|apply(t.data[,-c(1:4,15,16)],1,sum)==0,8]<-NA

t.data2[(qc1_2$light>10)|apply(t.data[,-c(1:4,15,16)],1,sum)==0,8]<-NA

t.data2[(qc1_2$rh>5)|apply(t.data[,-c(1:4,15,16)],1,sum)==0|t.data$rh==0,10]<-NA
t.data2[(qc1_2$temp>5)|apply(t.data[,-c(1:4,15,16)],1,sum)==0,11]<-NA
t.data2[(qc1_2$ws>5)|apply(t.data[,-c(1:4,15,16)],1,sum)==0,9]<-NA

t.data2[(qc1_2$prec>5)|apply(t.data[,-c(1:4,15,16)],1,sum)==0,9]<-NA
t.data2[(qc1_2$s_trans>5)|apply(t.data[,-c(1:4,15,16)],1,sum)==0,9]<-NA

###################
#t.data2는 qc1이 끝난 자료

##################
#QC2
setwd('N:/')
load('qc2data.rda')
location <- ("
             id lat  lon alt
             1 37.614735 128.740809 1057
             2 37.617677 128.738604 1052
             3 37.620780 128.738312 1006
             4 37.336853 129.006529 953
             5 37.2235018 128.9653731 1156
             ")
loc <- read.table(textConnection(location), header=T)
loc<-rbind(loc,rbind(head(s.data20[s.data20$id==100,],1),head(s.data20[s.data20$id==216,],1))[,c(1,10,11,12)])
# 1,2,3같은곳 100 태백
# 4,5 같은곳 216 대관령

plot(loc$lat,loc$lon,col=c(rep(1,5),2,3),pch=16)
plot(loc$lat,loc$lon,col=c(2,2,2,3,3,1,1),pch=16)
#100, 216 지점만 추출
s.data20_1<-s.data20[s.data20$id%in%c(100,216),]
rm(list=ls(pattern='s.data20')[1])

#즉
# 1,2,3 의 경우 100의 분포를 보고 QC
colnames(s.data20_1)
par(mfrow=c(2,3))
for(i in c(3:5,7:9)){
  hist(s.data20_1[s.data20_1$id==100,i],nclass=100,xlab=colnames(s.data20_1)[i],main='')}
par(mfrow=c(1,1))

s.data20_2<-s.data20_1[s.data20_1$id==100,]
qc2_temp_range<-c(lower=mean(s.data20_2$temp,na.rm=T)-5*sd(s.data20_2$temp,na.rm=T),upper=mean(s.data20_2$temp,na.rm=T)+5*sd(s.data20_2$temp,na.rm=T))
qc2_prec_range<-mad_range(s.data20_2$prec,k=5)
qc2_wd_range<-c(lower=mean(s.data20_2$wd,na.rm=T)-5*sd(s.data20_2$wd,na.rm=T),upper=mean(s.data20_2$wd,na.rm=T)+5*sd(s.data20_2$wd,na.rm=T))
qc2_rh_range<-mad_range(s.data20_2$rh,k=5)
qc2_light_range<-mad_range(s.data20_2$light,k=5)
qc2_s_temp_range<-c(lower=mean(s.data20_2$s_temp,na.rm=T)-5*sd(s.data20_2$s_temp,na.rm=T),upper=mean(s.data20_2$s_temp,na.rm=T)+5*sd(s.data20_2$s_temp,na.rm=T))

###############
# 4,5의 경우 216의 분포를 보고 QC

par(mfrow=c(2,3))
for(i in c(3:5,7:9)){
  hist(s.data20_1[s.data20_1$id==216,i],nclass=100,xlab=colnames(s.data20_1)[i],main='')}
par(mfrow=c(1,1))

s.data20_3<-s.data20_1[s.data20_1$id==216,]
qc2_temp_range2<-c(lower=mean(s.data20_3$temp,na.rm=T)-5*sd(s.data20_3$temp,na.rm=T),upper=mean(s.data20_3$temp,na.rm=T)+5*sd(s.data20_3$temp,na.rm=T))
qc2_prec_range2<-mad_range(s.data20_3$prec,k=5)
qc2_wd_range2<-mad_range(s.data20_3$wd,k=5)
qc2_rh_range2<-mad_range(s.data20_3$rh,k=5)
qc2_light_range2<-mad_range(s.data20_3$light,k=5)
qc2_s_temp_range2<-c(lower=mean(s.data20_3$s_temp,na.rm=T)-5*sd(s.data20_3$s_temp,na.rm=T),upper=mean(s.data20_3$s_temp,na.rm=T)+5*sd(s.data20_3$s_temp,na.rm=T))



###########
##QC3










###########
##QC4
#예측한것의 범위 내에 있으면 괜찮 벗어나면 이상치
setwd('N:/')
load(file='qc4data.rda')
