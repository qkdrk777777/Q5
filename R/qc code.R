##############
##QC1
setwd('N:/크레이데이터')
t.data<-read.csv('usn(data)_time.csv')
t.data<-t.data[t.data$월%in%c(7:10),]
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

setwd('D:/packages/Q5/R')
source('sorce_qc1_1.R',encoding='utf-8')
setwd('N:/크레이데이터')



#############

library(DUcj)
qc1_2<-apply(t.data,2,qc1)
qc1_2<-as.data.frame(qc1_2)
t.data2<-t.data

colnames(t.data2)
t.data2[(qc1_2$s_30>5)|apply(t.data[,-c(1:4,15,16)],1,sum)==0,5]<-NA
t.data2[(qc1_2$s_60>5)|apply(t.data[,-c(1:4,15,16)],1,sum)==0,6]<-NA
t.data2[(qc1_2$s_90>5)|apply(t.data[,-c(1:4,15,16)],1,sum)==0,7]<-NA
t.data2[(qc1_2$s_temp>10)|apply(t.data[,-c(1:4,15,16)],1,sum)==0,8]<-NA
t.data2[apply(t.data[,-c(1:4,15,16)],1,sum)==0,9]<-NA
t.data2[(qc1_2$rh>5)|apply(t.data[,-c(1:4,15,16)],1,sum)==0|t.data$rh==0,10]<-NA
t.data2[(qc1_2$temp>5)|apply(t.data[,-c(1:4,15,16)],1,sum)==0,11]<-NA
t.data2[(qc1_2$ws>5)|apply(t.data[,-c(1:4,15,16)],1,sum)==0,12]<-NA
t.data2[apply(t.data[,-c(1:4,15,16)],1,sum)==0,13]<-NA
t.data2[apply(t.data[,-c(1:4,15,16)],1,sum)==0,14]<-NA

setwd('D:/packages/Q5/R')
source(encoding='utf-8','source_qc1_2.R')
setwd('N:/크레이데이터')

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
#
# summary(s.data20_1[s.data20_1$id==216,])
# sd(s.data20_1[s.data20_1$id==216,'s_temp'],na.rm=T)
# hist(s.data20_1[s.data20_1$id==100,'s_temp'],xlab='',ylab=''
#      ,main='20년간 태백 asos의 토양온도 분포',cex.main=3)

plot(loc$lat,loc$lon,col=c(rep(1,5),2,3),pch=16)
plot(loc$lat,loc$lon,col=c(2,2,2,3,3,1,1),pch=16)
#100, 216 지점만 추출
s.data20_1<-s.data20[s.data20$id%in%c(100,216),]
rm(list=ls(pattern='s.data20')[1])

#즉
# 1,2,3 의 경우 100의 분포를 보고 QC
colnames(s.data20_1)

# par(mfrow=c(1,1))
# for(i in c(3:4,6:9)){
# png(paste0('C:/Users/qkdrk/Desktop/크레이 output/100/',colnames(s.data20_1)[i],'.png'))
# par(bg='transparent')
#     hist(s.data20_1[s.data20_1$id==100,i],nclass=50,xlab=c(NA,NA,'온도','강수량',NA,'풍속','습도','일사량','토양온도')[i],ylab='',main='')
#     dev.off()
#     }

par(mfrow=c(2,3))
for(i in c(3:4,6:9)){
  hist(s.data20_1[s.data20_1$id==100,i],nclass=50,xlab=c(NA,NA,'온도','강수량',NA,'풍속','습도','일사량','토양온도')[i],ylab='',main='')
  }
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

# for(i in c(3:4,6:9)){
#   png(paste0('C:/Users/qkdrk/Desktop/크레이 output/216/',colnames(s.data20_1)[i],'.png'))
#   par(bg='transparent')
#     hist(s.data20_1[s.data20_1$id==216,i],nclass=50,xlab=c(NA,NA,'온도','강수량',NA,'풍속','습도','일사량','토양온도')[i],main='')
#     dev.off()}
# par(mfrow=c(1,1))

par(mfrow=c(2,3))
for(i in c(3:4,6:9)){
  hist(s.data20_1[s.data20_1$id==216,i],nclass=50,xlab=colnames(s.data20_1)[i],main='')}
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
setwd('D:/packages/Q5/R')
source('source_qc2.R',encoding='utf-8')
setwd('N:/크레이데이터')
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
#
# hist(usnqc$s_30,xlab='토양수분30cm',main='',ylab='')
# hist(usnqc$s_60,xlab='토양수분60cm',main='',ylab='')
# hist(usnqc$s_90,xlab='토양수분90cm',main='',ylab='')
# hist(usnqc$s_temp,xlab='토양온도',main='',ylab='')
# hist(usnqc$temp,xlab='온도',main='',ylab='')
# hist(usnqc$rh,xlab='습도',main='',ylab='')
# hist(usnqc$prec,xlab='강수량',main='',ylab='')
# hist(usnqc$light,xlab='일사량',main='',ylab='')
# hist(usnqc$ws,xlab='풍속',main='',ylab='')
# hist(usnqc$s_trans,xlab='토양전도율',main='',ylab='')

usnqc<-usnqc[order(usnqc$time),]
usnqc<-usnqc[order(as.Date(usnqc$date2)),]
usnqc2<-usnqc

colnames(usnqc)
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
setwd('D:/packages/Q5/R')
source('source_qc3.R',encoding='utf-8')
setwd('N:/크레이데이터')
###########
##QC4
#예측한것의 범위 내에 있으면 괜찮 벗어나면 이상치
setwd('N:/')
load(file='qc4raw.rda')

qc4_raw<-qc4raw[,setdiff(2:47,c(grep(c('h95'),colnames(qc4raw)),grep(c('l95'),colnames(qc4raw))))]

t.data8<-t.data7
colnames(qc4_raw)
qc4_temp_range1<-c(lower=mean(qc4_raw$pred_temp_100,na.rm=T)-5*sd(qc4_raw$pred_temp_100,na.rm=T),upper=mean(qc4_raw$pred_temp_100,na.rm=T)+5*sd(qc4_raw$pred_temp_100,na.rm=T))
qc4_prec_range1<-c(lower=mean(qc4_raw$pred_prec_100,na.rm=T)-5*sd(qc4_raw$pred_prec_100,na.rm=T),upper=mean(qc4_raw$pred_prec_100,na.rm=T)+5*sd(qc4_raw$pred_prec_100,na.rm=T))
qc4_rh_range1<-c(lower=mean(qc4_raw$pred_rh_100,na.rm=T)-5*sd(qc4_raw$pred_rh_100,na.rm=T),upper=mean(qc4_raw$pred_rh_100,na.rm=T)+5*sd(qc4_raw$pred_rh_100,na.rm=T))
qc4_light_range1<-c(lower=mean(qc4_raw$pred_light_100,na.rm=T)-5*sd(qc4_raw$pred_light_100,na.rm=T),upper=mean(qc4_raw$pred_light_100,na.rm=T)+5*sd(qc4_raw$pred_light_100,na.rm=T))
qc4_s_temp_range1<-c(lower=mean(qc4_raw$pred_s_temp_100,na.rm=T)-5*sd(qc4_raw$pred_s_temp_100,na.rm=T),upper=mean(qc4_raw$pred_s_temp_100,na.rm=T)+5*sd(qc4_raw$pred_s_temp_100,na.rm=T))

qc4_temp_range2<-c(lower=mean(qc4_raw$pred_temp_216,na.rm=T)-5*sd(qc4_raw$pred_temp_216,na.rm=T),upper=mean(qc4_raw$pred_temp_216,na.rm=T)+5*sd(qc4_raw$pred_temp_216,na.rm=T))
qc4_prec_range2<-c(lower=mean(qc4_raw$pred_prec_216,na.rm=T)-5*sd(qc4_raw$pred_prec_216,na.rm=T),upper=mean(qc4_raw$pred_prec_216,na.rm=T)+5*sd(qc4_raw$pred_prec_216,na.rm=T))
qc4_rh_range2<-c(lower=mean(qc4_raw$pred_rh_216,na.rm=T)-5*sd(qc4_raw$pred_rh_216,na.rm=T),upper=mean(qc4_raw$pred_rh_216,na.rm=T)+5*sd(qc4_raw$pred_rh_216,na.rm=T))
qc4_light_range2<-c(lower=mean(qc4_raw$pred_light_216,na.rm=T)-5*sd(qc4_raw$pred_light_216,na.rm=T),upper=mean(qc4_raw$pred_light_216,na.rm=T)+5*sd(qc4_raw$pred_light_216,na.rm=T))
qc4_s_temp_range2<-c(lower=mean(qc4_raw$pred_s_temp_216,na.rm=T)-5*sd(qc4_raw$pred_s_temp_216,na.rm=T),upper=mean(qc4_raw$pred_s_temp_216,na.rm=T)+5*sd(qc4_raw$pred_s_temp_216,na.rm=T))

range1<-data.frame(qc4_temp_range1,qc4_prec_range1,qc4_rh_range1,qc4_light_range1,qc4_s_temp_range1)
range2<-data.frame(qc4_temp_range2,qc4_prec_range2,qc4_rh_range2,qc4_light_range2,qc4_s_temp_range2)

del4_1<-range1[2,]-range1[1,]
del4_2<-range2[2,]-range2[1,]
colnames(del4_1)<-c('temp','prec','rh','light','s_temp')
colnames(del4_2)<-c('temp','prec','rh','light','s_temp')

del4_4=NULL
for(i in 1:5){
del4_3<-c(del4_1[1,i],del4_2[1,i])%in%apply(rbind(del4_1,del4_2),2,max)[i]
del4_4<-rbind(del4_4,del4_3)}


del4_5<-rbind(t(range1)[del4_4[,1],],t(range2)[del4_4[,2],,drop=F])
del4_6<-del4_5[order(rownames(del4_5)),]
qc4_light_range<-del4_6[1,]
qc4_prec_range<-del4_6[2,]
qc4_rh_range<-del4_6[3,]
qc4_s_temp_range<-del4_6[4,]
qc4_temp_range<-del4_6[5,]

range<-data.frame(qc4_light_range,qc4_prec_range,qc4_rh_range,qc4_s_temp_range,qc4_temp_range)
t.data8[!(qc4_light_range[1]<t.data8$light&t.data8$light<qc4_light_range[2])&!is.na(t.data8$light),'light']<-NA
t.data8[!(qc4_prec_range[1]<t.data8$prec&t.data8$prec<qc4_prec_range[2])&!is.na(t.data8$prec),'prec']<-NA
t.data8[!(qc4_rh_range[1]<t.data8$rh&t.data8$rh<qc4_rh_range[2])&!is.na(t.data8$rh),'rh']<-NA
t.data8[!(qc4_s_temp_range[1]<t.data8$s_temp&t.data8$s_temp<qc4_s_temp_range[2])&!is.na(t.data8$s_temp),'s_temp']<-NA
t.data8[!(qc4_temp_range[1]<t.data8$temp&t.data8$temp<qc4_temp_range[2])&!is.na(t.data8$temp),'temp']<-NA



setwd('D:/packages/Q5/R')
source('source_qc4.R',encoding='utf-8')
setwd('N:/크레이데이터')

