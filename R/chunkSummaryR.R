chunkSummaryR<- function(data){
  n<-nrow(data)
  n1<-ceiling(n/4)
  n2<-ceiling(n/2)
  n3<-ceiling(3*n/4)
  n4<-n
  dat1<-data[1:n1,,]
  dat2<-data[(n1+1):n2,,]
  dat3<-data[(n2+1):n3,,]
  dat4<-data[(n3+1):n4,,]

  mean1<-round(apply(dat1,2,mean),2)
  sd1<-round(apply(dat1,2,sd),2)
  mean2<-round(apply(dat2,2,mean),2)
  sd2<-round(apply(dat2,2,sd),2)
  mean3<-round(apply(dat3,2,mean),2)
  sd3<-round(apply(dat3,2,sd),2)
  mean4<-round(apply(dat4,2,mean),2)
  sd4<-round(apply(dat4,2,sd),2)


names<-colnames(data)
out<-data.frame(Variable = names,
                Chunk1 = data.frame(mean1,sd1),
                Chunk2 = data.frame(mean2,sd2),
                Chunk3 = data.frame(mean3,sd3),
                Chunk4 = data.frame(mean4,sd4))
out

}
