##############
##QC1
setwd('N:/크레이데이터')
t.data<-read.csv('usn(data)_time.csv')
colnames(t.data)
colnames(t.data)<-c('id','year','mon','day','date','s_30','s_60','s_90'
                    ,'s_temp','light','rh','temp','light.1','ws','wd','prec','s_trans')
t.data$time<-as.numeric(substr(t.data$date,12,13))
t.data$date2<-(paste0(t.data$year,'-',t.data$mon,'-',t.data$day,' ',paste0(ifelse(nchar(t.data$time)==1,paste0('0',t.data$time),t.data$time),':00')))


t.data2<-t.data[,-c(5,13,15)]
colnames(t.data2)
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
colnames(t.data)
t.data2[(qc1_2$s_30>5)|apply(t.data[,-c(1:4,15,16)],1,sum)==0,5]<-NA
t.data2[(qc1_2$s_60>5)|apply(t.data[,-c(1:4,15,16)],1,sum)==0,6]<-NA
t.data2[(qc1_2$s_90>5)|apply(t.data[,-c(1:4,15,16)],1,sum)==0,7]<-NA
t.data2[(qc1_2$s_temp>10)|apply(t.data[,-c(1:4,15,16)],1,sum)==0,8]<-NA
t.data2[(qc1_2$rh>5)|apply(t.data[,-c(1:4,15,16)],1,sum)==0|t.data$rh==0,10]<-NA
t.data2[(qc1_2$temp>5)|apply(t.data[,-c(1:4,15,16)],1,sum)==0,11]<-NA
t.data2[(qc1_2$ws>5)|apply(t.data[,-c(1:4,15,16)],1,sum)==0,12]<-NA



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
for(i in c(3:4,6:9)){
  hist(s.data20_1[s.data20_1$id==100,i],nclass=100,xlab=colnames(s.data20_1)[i],main='')}
par(mfrow=c(1,1))

s.data20_2<-s.data20_1[s.data20_1$id==100,]
qc2_temp_range<-c(lower=mean(s.data20_2$temp,na.rm=T)-5*sd(s.data20_2$temp,na.rm=T),upper=mean(s.data20_2$temp,na.rm=T)+5*sd(s.data20_2$temp,na.rm=T))
qc2_prec_range<-mad_range(s.data20_2$prec,k=5)
qc2_ws_range<-mad_range(s.data20_2$ws,k=5)
qc2_rh_range<-mad_range(s.data20_2$rh,k=5)
qc2_light_range<-mad_range(s.data20_2$light,k=5)
qc2_s_temp_range<-c(lower=mean(s.data20_2$s_temp,na.rm=T)-5*sd(s.data20_2$s_temp,na.rm=T),upper=mean(s.data20_2$s_temp,na.rm=T)+5*sd(s.data20_2$s_temp,na.rm=T))

###############
# 4,5의 경우 216의 분포를 보고 QC

par(mfrow=c(2,3))
for(i in c(3:4,6:9)){
  hist(s.data20_1[s.data20_1$id==216,i],nclass=100,xlab=colnames(s.data20_1)[i],main='')}
par(mfrow=c(1,1))

s.data20_3<-s.data20_1[s.data20_1$id==216,]
qc2_temp_range2<-c(lower=mean(s.data20_3$temp,na.rm=T)-5*sd(s.data20_3$temp,na.rm=T),upper=mean(s.data20_3$temp,na.rm=T)+5*sd(s.data20_3$temp,na.rm=T))
qc2_prec_range2<-mad_range(s.data20_3$prec,k=5)
qc2_ws_range2<-mad_range(s.data20_3$ws,k=5)
qc2_rh_range2<-mad_range(s.data20_3$rh,k=5)
qc2_light_range2<-mad_range(s.data20_3$light,k=5)
qc2_s_temp_range2<-c(lower=mean(s.data20_3$s_temp,na.rm=T)-5*sd(s.data20_3$s_temp,na.rm=T),upper=mean(s.data20_3$s_temp,na.rm=T)+5*sd(s.data20_3$s_temp,na.rm=T))

#############
t.data3<-t.data2
t.data4<-t.data3[t.data3$id%in%c(1,2,3),]
t.data4[!(qc2_temp_range[1]<t.data4$temp&t.data4$temp<qc2_temp_range[2])&!is.na(t.data4$temp),'temp']<-NA
t.data4[!(qc2_prec_range[1]<t.data4$prec&t.data4$prec<qc2_prec_range[2])&!is.na(t.data4$prec),'prec']<-NA
t.data4[!(qc2_ws_range[1]<t.data4$ws&t.data4$ws<qc2_ws_range[2])&!is.na(t.data4$ws),'ws']<-NA
t.data4[!(qc2_rh_range[1]<t.data4$rh&t.data4$rh<qc2_rh_range[2])&!is.na(t.data4$rh),'rh']<-NA
t.data4[!(qc2_light_range[1]<t.data4$light&t.data4$light<qc2_light_range[2])&!is.na(t.data4$light),'light']<-NA
t.data4[!(qc2_s_temp_range[1]<t.data4$s_temp&t.data4$s_temp<qc2_s_temp_range[2])&!is.na(t.data4$s_temp),'s_temp']<-NA

t.data5<-t.data3[t.data3$id%in%c(4,5),]

t.data5[!(qc2_temp_range2[1]<t.data5$temp&t.data5$temp<qc2_temp_range2[2])&!is.na(t.data5$temp),'temp']<-NA
t.data5[!(qc2_prec_range2[1]<t.data5$prec&t.data5$prec<qc2_prec_range2[2])&!is.na(t.data5$prec),'prec']<-NA
t.data5[!(qc2_ws_range2[1]<t.data5$ws&t.data5$ws<qc2_ws_range2[2])&!is.na(t.data5$ws),'ws']<-NA
t.data5[!(qc2_rh_range2[1]<t.data5$rh&t.data5$rh<qc2_rh_range2[2])&!is.na(t.data5$rh),'rh']<-NA
t.data5[!(qc2_light_range2[1]<t.data5$light&t.data5$light<qc2_light_range2[2])&!is.na(t.data5$light),'light']<-NA
t.data5[!(qc2_s_temp_range2[1]<t.data5$s_temp&t.data5$s_temp<qc2_s_temp_range2[2])&!is.na(t.data5$s_temp),'s_temp']<-NA

t.data6<-rbind(t.data4,t.data5)
summary(t.data2)
summary(t.data6)
apply(apply(t.data2,2,is.na),2,sum)
apply(apply(t.data6,2,is.na),2,sum)

###########
##QC3

qc_usn<-t.data6
#qc_usn$date2<-as.Date(qc_usn$date2)
qc_usn<-qc_usn[order(qc_usn$time),]
qc_usn<-qc_usn[order(as.Date(qc_usn$date2)),]


for( i in 1:5)
  assign(paste0('qc_usn_',i),qc_usn[qc_usn$id==i,])

qc1<-qc_usn_1[-1,]
for(j in 5:14)qc1[,j]<-NA

for(j in 5:14){
  for(i in 1:(nrow(qc_usn_1)-1)){
    if(is.na(qc_usn_1[-1,][i,j])|is.na(qc_usn_1[i,j])!=T
    )qc1[i,j]<-qc_usn_1[-1,][i,j]-qc_usn_1[i,j] else if(is.na(qc_usn_1[i,j])==T
    ) qc1[i,j]<-NA else if(is.na(qc_usn_1[i,j])!=T) {n=0
    while(is.na(qc_usn_1[-1,][i+n,j])==T){if(i+n==nrow(qc_usn_1))break;n=n+1}
    qc1[i,j]<-abs((qc_usn_1[-1,][i+n,j]-qc_usn_1[i,j])/(n+1))
    }
  }
  qc1[,j]<-abs(qc1[,j])
}

##################################
qc2<-qc_usn_2[-1,]
for(j in 5:14)qc2[,j]<-NA

for(j in 5:14){
  for(i in 1:(nrow(qc_usn_2)-1)){
    if(is.na(qc_usn_2[-1,][i,j])|is.na(qc_usn_2[i,j])!=T
    )qc2[i,j]<-qc_usn_2[-1,][i,j]-qc_usn_2[i,j] else if(is.na(qc_usn_2[i,j])==T
    ) qc2[i,j]<-NA else if(is.na(qc_usn_2[i,j])!=T) {n=0
    while(is.na(qc_usn_2[-1,][i+n,j])==T){if(i+n==nrow(qc_usn_2))break;n=n+1}
    qc2[i,j]<-abs((qc_usn_2[-1,][i+n,j]-qc_usn_2[i,j])/(n+1))
    }
  }
  qc2[,j]<-abs(qc2[,j])
}

#################
qc3<-qc_usn_3[-1,]
for(j in 5:14)qc3[,j]<-NA

for(j in 5:14){
  for(i in 1:(nrow(qc_usn_3)-1)){
    if(is.na(qc_usn_3[-1,][i,j])|is.na(qc_usn_3[i,j])!=T
    )qc3[i,j]<-qc_usn_3[-1,][i,j]-qc_usn_3[i,j] else if(is.na(qc_usn_3[i,j])==T
    ) qc3[i,j]<-NA else if(is.na(qc_usn_3[i,j])!=T) {n=0
    while(is.na(qc_usn_3[-1,][i+n,j])==T){if(i+n==nrow(qc_usn_3))break;n=n+1}
    qc3[i,j]<-abs((qc_usn_3[-1,][i+n,j]-qc_usn_3[i,j])/(n+1))
    }
  }
  qc3[,j]<-abs(qc3[,j])
}
#################
qc4<-qc_usn_4[-1,]
for(j in 5:14)qc4[,j]<-NA

for(j in 5:14){
  for(i in 1:(nrow(qc_usn_4)-1)){
    if(is.na(qc_usn_4[-1,][i,j])|is.na(qc_usn_4[i,j])!=T
    )qc4[i,j]<-qc_usn_4[-1,][i,j]-qc_usn_4[i,j] else if(is.na(qc_usn_4[i,j])==T
    ) qc4[i,j]<-NA else if(is.na(qc_usn_4[i,j])!=T) {n=0
    while(is.na(qc_usn_4[-1,][i+n,j])==T){if(i+n==nrow(qc_usn_4))break;n=n+1}
    qc4[i,j]<-abs((qc_usn_4[-1,][i+n,j]-qc_usn_4[i,j])/(n+1))
    }
  }
  qc4[,j]<-abs(qc4[,j])
}
#################
qc5<-qc_usn_5[-1,]
for(j in 5:14)qc5[,j]<-NA

for(j in 5:14){
  for(i in 1:(nrow(qc_usn_5)-1)){
    if(is.na(qc_usn_5[-1,][i,j])|is.na(qc_usn_5[i,j])!=T
    )qc5[i,j]<-qc_usn_5[-1,][i,j]-qc_usn_5[i,j] else if(is.na(qc_usn_5[i,j])==T
    ) qc5[i,j]<-NA else if(is.na(qc_usn_5[i,j])!=T) {n=0
    while(is.na(qc_usn_5[-1,][i+n,j])==T){if(i+n==nrow(qc_usn_5))break;n=n+1}
    qc5[i,j]<-abs((qc_usn_5[-1,][i+n,j]-qc_usn_5[i,j])/(n+1))
    }
  }
  qc5[,j]<-abs(qc5[,j])
}
usnqc<-rbind(qc_usn_1[1,],qc1,qc_usn_2[1,],qc2,qc_usn_3[1,],qc3,qc_usn_4[1,],qc4,qc_usn_5[1,],qc5)
usnqc<-usnqc[order(usnqc$time),]
usnqc<-usnqc[order(as.Date(usnqc$date2)),]
usnqc2<-usnqc

for(j in 5:14){print(mad_range(usnqc[quantile(usnqc[,j],.95,na.rm=T)<usnqc[,j],j],k=5)[2])
usnqc2[,j]<-usnqc[,j]<mad_range(usnqc[quantile(usnqc[,j],.95,na.rm=T)<usnqc[,j],j],k=5)[2]
usnqc2[is.na(usnqc2[,j]),j]<-F
}

t.data7<-qc_usn
for(j in 5:14)
  t.data7[!usnqc2[,j]&!is.na(t.data7[,j]),j]<-NA

apply(apply(t.data7,2,is.na),2,sum)
apply(apply(t.data6,2,is.na),2,sum)
apply(apply(t.data2,2,is.na),2,sum)
###########
##QC4
#예측한것의 범위 내에 있으면 괜찮 벗어나면 이상치
setwd('N:/')
load(file='qc4data.rda')
qc4data$date<-as.character(qc4data$date)


qc4data2<-qc4data[qc4data$date%in%unique(t.data7$date2),]
head(qc4data2)
summary(qc4data2)
