
N<-data.frame(전체N=qc3_table[,1]-as.numeric(substr(qc3_table[,2],1,regexpr('\\(',qc3_table[,2])-1))
                ,지점1N=qc3_table[,3]-as.numeric(substr(qc3_table[,4],1,regexpr('\\(',qc3_table[,4])-1))
                ,지점2N=qc3_table[,5]-as.numeric(substr(qc3_table[,6],1,regexpr('\\(',qc3_table[,6])-1))
                ,지점3N=qc3_table[,7]-as.numeric(substr(qc3_table[,8],1,regexpr('\\(',qc3_table[,8])-1))
                ,지점4N=qc3_table[,9]-as.numeric(substr(qc3_table[,10],1,regexpr('\\(',qc3_table[,10])-1))
                ,지점5N=qc3_table[,11]-as.numeric(substr(qc3_table[,12],1,regexpr('\\(',qc3_table[,12])-1)))
rownames(N)<-rownames(qc3_table)
#qc3 로 걸러진 비정상 데이터의 개수(전체지점)
del3<-data.frame(전체=apply(apply(t.data8,2,is.na),2,sum)[-c(1:4,15,16)]-(dim(t.data8)[1]-N[,1])
                   ,지점1=apply(apply(t.data8[t.data8$id==1,],2,is.na),2,sum)[-c(1:4,15,16)]-(dim(t.data8[t.data8$id==1,])[1]-N[,2])
                   ,지점2=apply(apply(t.data8[t.data8$id==2,],2,is.na),2,sum)[-c(1:4,15,16)]-(dim(t.data8[t.data8$id==2,])[1]-N[,3])
                   ,지점3=apply(apply(t.data8[t.data8$id==3,],2,is.na),2,sum)[-c(1:4,15,16)]-(dim(t.data8[t.data8$id==3,])[1]-N[,4])
                   ,지점4=apply(apply(t.data8[t.data8$id==4,],2,is.na),2,sum)[-c(1:4,15,16)]-(dim(t.data8[t.data8$id==4,])[1]-N[,5])
                   ,지점5=apply(apply(t.data8[t.data8$id==5,],2,is.na),2,sum)[-c(1:4,15,16)]-(dim(t.data8[t.data8$id==5,])[1]-N[,6])
)


del3_1<-cbind(del3,N)
colnames(del3_1)
qc4_table<-del3_1[,c(7,1,8,2,9,3,10,4,11,5,12,6)]

for(i in seq(2,12,2))
  qc4_table[,i]=paste0(qc4_table[,i],'(',round(qc4_table[,i]/qc4_table[,(i-1)]*100,2),'%)')

write.csv(qc4_table,'C:/Users/qkdrk/Desktop/크레이 output/qc4_table.csv')
