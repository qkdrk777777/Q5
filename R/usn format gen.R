
# data<-read.csv('qc1&qc2&qc3_data.csv')[,-1]

data<-t.data8


cor(data[,-c(1:5,16)],use='complete.obs')



setwd('N:/크레이데이터/qc logit')
setwd('..\\')
d2015 <- read.csv("2015.csv", header=T)
d2016 <- read.csv("2016.csv", header=T)
d2017 <- read.csv("2017.csv", header=T)

d2015_2<-d2015[d2015$date != "" ,1:9]
d2016_2<-d2016[d2016$date != "" ,1:9]
d2017_2<-d2017[d2017$date != "" ,1:9]

a.numeric <- function(data){
  for(i in 3:9)
    data[,i]<-as.numeric(paste(data[,i]))
  return(data)
}

d2015_2 <-a.numeric(d2015_2)
d2016_2 <-a.numeric(d2016_2)
d2017_2 <-a.numeric(d2017_2)

d2017_2$wei<- d2017_2$wei*1000

d.data <- rbind(d2015_2, d2016_2, d2017_2)
d.data$date <- as.Date(d.data$date)

head(d.data)
head(data)
colnames(d.data)[2]<-'id'

# data$date2<-as.Date(gsub('\\.','-',data$date))
data$date2<-as.Date(paste0(data$year,'-',data$mon,'-',data$day))
data$s_30_count<-as.numeric(is.na(data$s_30))
data$s_60_count<-as.numeric(is.na(data$s_60))
data$s_90_count<-as.numeric(is.na(data$s_90))
data$s_temp_count<-as.numeric(is.na(data$s_temp))
data$light_count<-as.numeric(is.na(data$light))
data$rh_count<-as.numeric(is.na(data$rh))
data$temp_count<-as.numeric(is.na(data$temp))
# data$light.1_count<-as.numeric(is.na(data$light.1))
data$ws_count<-as.numeric(is.na(data$ws))
data$prec_count<-as.numeric(is.na(data$prec))
data$s_trans_count<-as.numeric(is.na(data$s_trans))



library(plyr)


data2<-ddply(data,~date2+id,summarise,s_30=mean(s_30,na.rm=T)
,s_60=mean(s_60,na.rm=T),
s_90=mean(s_90,na.rm=T),s_temp=mean(s_temp,na.rm=T),light=mean(light,na.rm=T)
,rh=mean(rh,na.rm=T),temp=mean(temp,na.rm=T),
# light.1=mean(light.1,na.rm=T)
ws=mean(ws,na.rm=T),
prec=mean(prec,na.rm=T),s_trans=mean(s_trans,na.rm=T),
s_30_count=sum(s_30_count,na.rm=T),s_60_count=sum(s_60_count,na.rm=T),s_90_count=sum(s_90_count,na.rm=T)
,s_temp_count=sum(s_temp_count,na.rm=T),light_count=sum(light_count,na.rm=T),
rh_count=sum(rh_count,na.rm=T),temp_count=sum(temp_count,na.rm=T),
# light.1_count=sum(light.1_count,na.rm=T)
ws_count=sum(ws_count,na.rm=T)
,prec_count=sum(prec_count,na.rm=T),
prec_count=sum(prec,na.rm=T),s_trans_count=sum(s_trans_count,na.rm=T))


data3_1<-data2[data2$date2%in%(unique(d.data$date)-1),]
data3_1$date2<-data3_1$date2+1
colnames(data3_1)[3:22]<-paste0(colnames(data3_1)[3:22],'_m1')
data3_2<-data2[data2$date2%in%(unique(d.data$date)-2),]
data3_2$date2<-data3_2$date2+2
colnames(data3_2)[3:22]<-paste0(colnames(data3_2)[3:22],'_m2')



data3<-(data2[data2$date2%in%unique(d.data$date),])
for(i in 1:ncol(data3))
  data3[is.nan(data3[,i]),i]<-NA



library(plyr)

data4<-NULL
for( i in 1:length(unique(data3$date2))){
del<-data.frame(date2=unique(data3$date2)[i],ddply(data2[data2$date2%in%c(unique(data3$date2)[i]-c(1:3)),],~id,summarise,s_30_3=mean(s_30,na.rm=T)
,s_60_3=mean(s_60,na.rm=T),s_90_3=mean(s_90,na.rm=T),s_temp_3=mean(s_temp,na.rm=T),
light_3=mean(light,na.rm=T),rh_3=mean(rh,na.rm=T),temp_3=mean(temp,na.rm = T),
ws_3=mean(ws,na.rm=T),
#light.1_3=mean(light.1,na.rm=T),
prec_3=mean(prec,na.rm=T),s_trans_3=mean(s_trans,na.rm=T),s_30_3_count=sum(s_30_count,na.rm=T),
s_60_3_count=sum(s_60_count,na.rm=T),s_90_3_count=sum(s_90_count,na.rm=T),s_temp_3_count=sum(s_temp_count,na.rm=T)
,light_3_count=sum(light_count,na.rm=T),rh_3_count=sum(rh_count,na.rm=T),temp_3_count=sum(temp_count,na.rm=T),
ws_3_count=sum(ws_count,na.rm=T),
#light.1_3_count=sum(light.1_count,na.rm=T),
prec_3_count=sum(prec_count,na.rm=T),
s_trans_3_count=sum(s_trans_count,na.rm=T)))
data4<-rbind(data4,del)
}


data5<-NULL
for( i in 1:length(unique(data3$date2))){
del<-data.frame(date2=unique(data3$date2)[i],ddply(data2[data2$date2%in%c(unique(data3$date2)[i]-c(1:7)),],~id,summarise,s_30_7=mean(s_30,na.rm=T)
,s_60_7=mean(s_60,na.rm=T),s_90_7=mean(s_90,na.rm=T),s_temp_7=mean(s_temp,na.rm=T),
light_7=mean(light,na.rm=T),rh_7=mean(rh,na.rm=T),temp_7=mean(temp,na.rm = T),
ws_7=mean(ws,na.rm=T),
#light.1_7=mean(light.1,na.rm=T),
prec_7=mean(prec,na.rm=T),s_trans_7=mean(s_trans,na.rm=T),s_30_7_count=sum(s_30_count,na.rm=T),
s_60_7_count=sum(s_60_count,na.rm=T),s_90_7_count=sum(s_90_count,na.rm=T),s_temp_7_count=sum(s_temp_count,na.rm=T)
,light_7_count=sum(light_count,na.rm=T),rh_7_count=sum(rh_count,na.rm=T),temp_7_count=sum(temp_count,na.rm=T),
ws_7_count=sum(ws_count,na.rm=T),
#light.1_7_count=sum(light.1_count,na.rm=T),
prec_7_count=sum(prec_count,na.rm=T),
s_trans_7_count=sum(s_trans_count,na.rm=T)))
data5<-rbind(data5,del)
}


data6<-merge(merge(data3,data4,by=c('date2','id'),all=T),data5,by=c('date2','id'),all=T)
data7<-merge(data6,merge(data3_1,data3_2,by=c('date2','id'),all=T),by=c('date2','id'),all=T)


head(data7)

data8<-data7[,c(1,2,grep('count',colnames(data7)))]
data9<-data7[,-grep('count',colnames(data7))]
#data7<-cbind(data6[,-c(13:23,33:42,53:62)],round((24-data6[,13:23])/24*100,3),round((24*3-data6[,33:42])/(24*3)*100,3),round((24*7-data6[,53:62])/(24*7)*100,3))
head(data9)


# colnames(data7)[grep('count',colnames(data7))]<-gsub('count','per',colnames(data7)[grep('count',colnames(data7))])
# colnames(data7)
# head(data7)
# data7<-data7[,c(1:12,23:42,13:22,43:62)]
# data8<-merge(data7,data3_1,by=c('date2','id'))

head(data8)

setwd('G:/')
# write.csv(data7,file='qc1~3_포전포맷.csv')
getwd()

for(i in 1:ncol(data9)){
data9[is.nan(data9[,i]),i]<-NA}

unique(data9$date2)

colnames(data9)
grep('s_trans',colnames(data9))
grep('s_trans',colnames(data9),value=T)
data9<-data9[,c(1:2,3,33,43,13,23,4,34,44,14,24,5,35,45,15,25,6,36,46,16,26,7,37,47,17,27,
8,38,48,18,28,9,39,49,19,29,10,40,50,20,30,11,41,51,21,31,12,42,52,22,32)]




# setwd('C:/Users/USER/Desktop')
setwd('C:/Users/qkdrk/Desktop/생육')
data10<-read.csv('생육.csv')[,-1]

colnames(data10)[1:2]<-c('date2','id')
data10$date2<-as.Date(data10$date2)
str(data9)
data11<-merge(data10,data9,by=c('date2','id'),all=T)
getwd()
write.csv(as.data.frame(cor(data11[-c(1:2)],use='complete.obs')),'상관분석.csv')

cor(data[,c(6:15)],use='complete.obs')
cor(data3[,c(3:12)],use='complete.obs')

head(data3)
pairs(data[,c(6:15)])
pairs(data3[,c(3:12)])

