#기기정지로 추정되는 비율
(del<-table(apply(t.data[,-c(1:4,15:16)],1,sum)==0))
names(del)<-c('정상','비정상')

qc_logit_1_1<-apply(t.data[,-c(1:4,15:16)],1,sum)==0

t.data1_1<-t.data
for(i in 5:14)
  t.data1_1[qc_logit_1_1,i]<-NA

qc.data1_1<-t.data1_1
qc_logit_1_1<-as.data.frame(apply(qc.data1_1,2,is.na))
qc_logit_1_1[,c(1:4,15:16)]<-t.data[,c(1:4,15:16)]

summary(qc_logit_1_1)
summary(qc.data1_1)
