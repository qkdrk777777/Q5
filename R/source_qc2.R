(total2<-dim(t.data2)[1]-apply(apply(t.data2,2,is.na),2,sum))
del2<-apply(apply(t.data6,2,is.na),2,sum)-apply(apply(t.data2,2,is.na),2,sum)
del2_1<-data.frame(N=total2,전체=paste0(del2,'(',round(del2/total2*100,2),'%)'))

for(i in 1:5){
(total2<-8856-apply(apply(t.data2[t.data2$id==i,],2,is.na),2,sum))
del2<-apply(apply(t.data6[t.data6$id==i,],2,is.na),2,sum)-
  apply(apply(t.data2[t.data2$id==i,],2,is.na),2,sum)
del2_2<-data.frame(N=total2,지점=paste0(del2,'(',round(del2/total2*100,2),'%)'))

del2_1<-cbind(del2_1,del2_2)
}
rownames(del2_1)<-names(del2)
qc2_table<-del2_1[-c(1:4,15:16),]


write.csv(qc2_table,'C:/Users/qkdrk/Desktop/크레이 output/qc2_table.csv')
