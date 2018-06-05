del2<-as.data.frame(apply(apply(t.data2,2,is.na),2,sum))

del2<-data.frame(dim(t.data2)[1]-del[2]-del2,del2)
colnames(del2)<-c('정상','비정상')


for(i in 1:5){
  assign(paste0('del2_',i),as.data.frame(apply(apply(t.data2[t.data2$id==i,],2,is.na),2,sum)))
assign(paste0('del2_',i),data.frame(nrow(t.data2[t.data2$id==i,])-get(paste0('del2_',i)),get(paste0('del2_',i))))
}
colnames(del2_1)<-c('정상','비정상')
colnames(del2_2)<-c('정상','비정상')
colnames(del2_3)<-c('정상','비정상')
colnames(del2_4)<-c('정상','비정상')
colnames(del2_5)<-c('정상','비정상')
for(j in 1:2)
  del2_1[,j]<-paste0(del2_1[,j],'(',round(100*del2_1[,j]/nrow(t.data2[t.data2$id==1,]),2),'%)')
for(j in 1:2)
  del2_2[,j]<-paste0(del2_2[,j],'(',round(100*del2_2[,j]/nrow(t.data2[t.data2$id==2,]),2),'%)')
for(j in 1:2)
  del2_3[,j]<-paste0(del2_3[,j],'(',round(100*del2_3[,j]/nrow(t.data2[t.data2$id==3,]),2),'%)')
for(j in 1:2)
  del2_4[,j]<-paste0(del2_4[,j],'(',round(100*del2_4[,j]/nrow(t.data2[t.data2$id==4,]),2),'%)')
for(j in 1:2)
  del2_5[,j]<-paste0(del2_5[,j],'(',round(100*del2_5[,j]/nrow(t.data2[t.data2$id==5,]),2),'%)')


for(j in 1:2)
  del2[,j]<-paste0(del2[,j],'(',round(100*del2[,j]/nrow(t.data2),2),'%)')


del1_2<-data.frame(del2,del2_1,del2_2,del2_3,del2_4,del2_5)[-c(1:4,15:16),]

qc_logit_1_2<-data.frame(t.data2[,1:4],
s_30=(qc1_2$s_30>5)&apply(t.data[,-c(1:4,15,16)],1,sum)!=0,
s_60=(qc1_2$s_60>5)&apply(t.data[,-c(1:4,15,16)],1,sum)!=0,
s_90=(qc1_2$s_90>5)&apply(t.data[,-c(1:4,15,16)],1,sum)!=0,
s_temp=(qc1_2$s_temp>10)&apply(t.data[,-c(1:4,15,16)],1,sum)!=0,
rh=(qc1_2$rh>5)&apply(t.data[,-c(1:4,15,16)],1,sum)!=0,
temp=(qc1_2$temp>5)&apply(t.data[,-c(1:4,15,16)],1,sum)!=0,
ws=(qc1_2$ws>5)&apply(t.data[,-c(1:4,15,16)],1,sum)!=0)

qc_logit_1<-data.frame(t.data2[,1:4],
s_30=(qc1_2$s_30>5)|apply(t.data[,-c(1:4,15,16)],1,sum)==0,
s_60=(qc1_2$s_60>5)|apply(t.data[,-c(1:4,15,16)],1,sum)==0,
s_90=(qc1_2$s_90>5)|apply(t.data[,-c(1:4,15,16)],1,sum)==0,
light=apply(t.data[,-c(1:4,15,16)],1,sum)==0,
s_temp=(qc1_2$s_temp>10)|apply(t.data[,-c(1:4,15,16)],1,sum)==0,
rh=(qc1_2$rh>5)|apply(t.data[,-c(1:4,15,16)],1,sum)==0|t.data$rh==0,
temp=(qc1_2$temp>5)|apply(t.data[,-c(1:4,15,16)],1,sum)==0,
ws=(qc1_2$ws>5)|apply(t.data[,-c(1:4,15,16)],1,sum)==0,
prec=apply(t.data[,-c(1:4,15,16)],1,sum)==0,
s_trans=apply(t.data[,-c(1:4,15,16)],1,sum)==0
)



#############

#기기가 안꺼졌고 여러번 반복
#s_30, 60, 90 은 5번 반복, s_temp 10번반복 rh, temp, ws 5번 반복
qc.data1_2<-t.data
qc.data1_2[qc_logit_1_2$s_30,'s_30']<-NA
qc.data1_2[qc_logit_1_2$s_60,'s_60']<-NA
qc.data1_2[qc_logit_1_2$s_90,'s_90']<-NA
qc.data1_2[qc_logit_1_2$s_temp,'s_temp']<-NA
qc.data1_2[qc_logit_1_2$rh,'rh']<-NA
qc.data1_2[qc_logit_1_2$temp,'temp']<-NA
qc.data1_2[qc_logit_1_2$ws,'ws']<-NA

name<-names(apply(apply(qc.data1_2,2,is.na),2,sum))
qc1_table=data.frame(전체=paste0(apply(apply(qc.data1_2,2,is.na),2,sum)
,'(',round(apply(apply(qc.data1_2,2,is.na),2,sum)/dim(qc.data1_2)[1]*100,2),'%)'),
지점1=paste0(apply(apply(qc.data1_2[qc.data1_2$id==1,],2,is.na),2,sum),
'(',round(apply(apply(qc.data1_2[qc.data1_2$id==1,],2,is.na),2,sum)/dim(qc.data1_2[qc.data1_2$id==1,])[1]*100,2),'%)')
,지점2=paste0(apply(apply(qc.data1_2[qc.data1_2$id==2,],2,is.na),2,sum),
'(',round(apply(apply(qc.data1_2[qc.data1_2$id==2,],2,is.na),2,sum)/dim(qc.data1_2[qc.data1_2$id==2,])[1]*100,2),'%)')
,지점3=paste0(apply(apply(qc.data1_2[qc.data1_2$id==3,],2,is.na),2,sum),
'(',round(apply(apply(qc.data1_2[qc.data1_2$id==3,],2,is.na),2,sum)/dim(qc.data1_2[qc.data1_2$id==3,])[1]*100,2),'%)')
,지점4=paste0(apply(apply(qc.data1_2[qc.data1_2$id==4,],2,is.na),2,sum),
'(',round(apply(apply(qc.data1_2[qc.data1_2$id==4,],2,is.na),2,sum)/dim(qc.data1_2[qc.data1_2$id==4,])[1]*100,2),'%)')
,지점5=paste0(apply(apply(qc.data1_2[qc.data1_2$id==5,],2,is.na),2,sum),
'(',round(apply(apply(qc.data1_2[qc.data1_2$id==5,],2,is.na),2,sum)/dim(qc.data1_2[qc.data1_2$id==5,])[1]*100,2),'%)')
,stringsAsFactors=F
)

rownames(qc1_table)<-name


qc1_table=rbind(qc1_table,
기기정지=data.frame(전체=paste0(apply(qc_logit_1_1[,-c(1:4,15:16)],2,sum)[1]
,'(',round(apply(qc_logit_1_1[,-c(1:4,15:16)],2,sum)[1]/dim(qc.data1_2)[1]*100,2),'%)')
,지점1=paste0(apply(qc_logit_1_1[qc_logit_1_1$id==1,-c(1:4,15:16)],2,sum)[1]
,'(',round(apply(qc_logit_1_1[qc_logit_1_1$id==1,-c(1:4,15:16)],2,sum)[1]/dim(qc.data1_2[qc.data1_2$id==1,])[1]*100,2),'%)')
,지점2=paste0(apply(qc_logit_1_1[qc_logit_1_1$id==2,-c(1:4,15:16)],2,sum)[1]
,'(',round(apply(qc_logit_1_1[qc_logit_1_1$id==2,-c(1:4,15:16)],2,sum)[1]/dim(qc.data1_2[qc.data1_2$id==2,])[1]*100,2),'%)')
,지점3=paste0(apply(qc_logit_1_1[qc_logit_1_1$id==3,-c(1:4,15:16)],2,sum)[1]
,'(',round(apply(qc_logit_1_1[qc_logit_1_1$id==3,-c(1:4,15:16)],2,sum)[1]/dim(qc.data1_2[qc.data1_2$id==3,])[1]*100,2),'%)')
,지점4=paste0(apply(qc_logit_1_1[qc_logit_1_1$id==4,-c(1:4,15:16)],2,sum)[1]
,'(',round(apply(qc_logit_1_1[qc_logit_1_1$id==4,-c(1:4,15:16)],2,sum)[1]/dim(qc.data1_2[qc.data1_2$id==4,])[1]*100,2),'%)')
,지점5=paste0(apply(qc_logit_1_1[qc_logit_1_1$id==5,-c(1:4,15:16)],2,sum)[1]
,'(',round(apply(qc_logit_1_1[qc_logit_1_1$id==5,-c(1:4,15:16)],2,sum)[1]/dim(qc.data1_2[qc.data1_2$id==5,])[1]*100,2),'%)')))

qc.data1<-t.data
head(qc_logit_1_2)
qc.data1[qc_logit_1$s_30,'s_30']<-NA
qc.data1[qc_logit_1$s_60,'s_60']<-NA
qc.data1[qc_logit_1$s_90,'s_90']<-NA
qc.data1[qc_logit_1$light,'light']<-NA
qc.data1[qc_logit_1$s_temp,'s_temp']<-NA
qc.data1[qc_logit_1$rh,'rh']<-NA
qc.data1[qc_logit_1$temp,'temp']<-NA
qc.data1[qc_logit_1$ws,'ws']<-NA
qc.data1[qc_logit_1$prec,'prec']<-NA
qc.data1[qc_logit_1$s_trans,'s_trans']<-NA

apply(apply(qc.data1_1,2,is.na),2,sum)
apply(apply(qc.data1_2,2,is.na),2,sum)
apply(apply(qc.data1,2,is.na),2,sum)

qc1_table<-rbind(qc1_table,기기오류=data.frame(전체=paste0(table(as.logical(apply(qc_logit_1_2[,-c(1:4)],1,sum)))[2]
,'(',round(100*table(as.logical(apply(qc_logit_1_2[,-c(1:4)],1,sum)))[2]/sum(table(as.logical(apply(qc_logit_1_2[,-c(1:4)],1,sum)))),2),'%)')
,지점1=paste0(table(as.logical(apply(qc_logit_1_2[qc_logit_1_2$id==1,][,-c(1:4)],1,sum)))[2]
,'(',round(100*table(as.logical(apply(qc_logit_1_2[qc_logit_1_2$id==1,][,-c(1:4)],1,sum)))[2]/sum(table(as.logical(apply(qc_logit_1_2[qc_logit_1_2$id==1,][,-c(1:4)],1,sum)))),2),'%)')
,지점2=paste0(table(as.logical(apply(qc_logit_1_2[qc_logit_1_2$id==2,][,-c(1:4)],1,sum)))[2]
,'(',round(100*table(as.logical(apply(qc_logit_1_2[qc_logit_1_2$id==2,][,-c(1:4)],1,sum)))[2]/sum(table(as.logical(apply(qc_logit_1_2[qc_logit_1_2$id==2,][,-c(1:4)],1,sum)))),2),'%)')
,지점3=paste0(table(as.logical(apply(qc_logit_1_2[qc_logit_1_2$id==3,][,-c(1:4)],1,sum)))[2]
,'(',round(100*table(as.logical(apply(qc_logit_1_2[qc_logit_1_2$id==3,][,-c(1:4)],1,sum)))[2]/sum(table(as.logical(apply(qc_logit_1_2[qc_logit_1_2$id==3,][,-c(1:4)],1,sum)))),2),'%)')
,지점4=paste0(table(as.logical(apply(qc_logit_1_2[qc_logit_1_2$id==4,][,-c(1:4)],1,sum)))[2]
,'(',round(100*table(as.logical(apply(qc_logit_1_2[qc_logit_1_2$id==4,][,-c(1:4)],1,sum)))[2]/sum(table(as.logical(apply(qc_logit_1_2[qc_logit_1_2$id==4,][,-c(1:4)],1,sum)))),2),'%)')
,지점5=paste0(table(as.logical(apply(qc_logit_1_2[qc_logit_1_2$id==5,][,-c(1:4)],1,sum)))[2]
,'(',round(100*table(as.logical(apply(qc_logit_1_2[qc_logit_1_2$id==5,][,-c(1:4)],1,sum)))[2]/sum(table(as.logical(apply(qc_logit_1_2[qc_logit_1_2$id==5,][,-c(1:4)],1,sum)))),2),'%)')
))



qc1_table<-rbind(qc1_table,`기기정지 및 오류`=data.frame(전체=paste0(table(as.logical(apply(qc_logit_1[,-c(1:4)],1,sum)))[2]
,'(',round(100*table(as.logical(apply(qc_logit_1[,-c(1:4)],1,sum)))[2]/sum(table(as.logical(apply(qc_logit_1[,-c(1:4)],1,sum)))),2),'%)')
,지점1=paste0(table(as.logical(apply(qc_logit_1[qc_logit_1$id==1,][,-c(1:4)],1,sum)))[2]
,'(',round(100*table(as.logical(apply(qc_logit_1[qc_logit_1$id==1,][,-c(1:4)],1,sum)))[2]/sum(table(as.logical(apply(qc_logit_1[qc_logit_1$id==1,][,-c(1:4)],1,sum)))),2),'%)')
,지점2=paste0(table(as.logical(apply(qc_logit_1[qc_logit_1$id==2,][,-c(1:4)],1,sum)))[2]
,'(',round(100*table(as.logical(apply(qc_logit_1[qc_logit_1$id==2,][,-c(1:4)],1,sum)))[2]/sum(table(as.logical(apply(qc_logit_1[qc_logit_1$id==2,][,-c(1:4)],1,sum)))),2),'%)')
,지점3=paste0(table(as.logical(apply(qc_logit_1[qc_logit_1$id==3,][,-c(1:4)],1,sum)))[2]
,'(',round(100*table(as.logical(apply(qc_logit_1[qc_logit_1$id==3,][,-c(1:4)],1,sum)))[2]/sum(table(as.logical(apply(qc_logit_1[qc_logit_1$id==3,][,-c(1:4)],1,sum)))),2),'%)')
,지점4=paste0(table(as.logical(apply(qc_logit_1[qc_logit_1$id==4,][,-c(1:4)],1,sum)))[2]
,'(',round(100*table(as.logical(apply(qc_logit_1[qc_logit_1$id==4,][,-c(1:4)],1,sum)))[2]/sum(table(as.logical(apply(qc_logit_1[qc_logit_1$id==4,][,-c(1:4)],1,sum)))),2),'%)')
,지점5=paste0(table(as.logical(apply(qc_logit_1[qc_logit_1$id==5,][,-c(1:4)],1,sum)))[2]
,'(',round(100*table(as.logical(apply(qc_logit_1[qc_logit_1$id==5,][,-c(1:4)],1,sum)))[2]/sum(table(as.logical(apply(qc_logit_1[qc_logit_1$id==5,][,-c(1:4)],1,sum)))),2),'%)')
))
qc1_table<-qc1_table[-c(1:4,9,13:16),]

write.csv(qc1_table,'C:/Users/qkdrk/Desktop/크레이 output/qc1_table.csv')
# del2_1<-as.data.frame(apply(apply(t.data2[t.data2$id==1,],2,is.na),2,sum))
# del2_1<-data.frame(nrow(t.data2[t.data2$id==1,])-del2_1,del2_1)
# colnames(del2_1)<-c('정상','비정상')
# for(j in 1:2)
#   del2_1[,j]<-paste0(del2_1[,j],'(',round(100*del2_1[,j]/nrow(t.data2[t.data2$id==1,]),2),'%)')
