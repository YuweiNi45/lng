library(MASS)
data<-birthwt
class(birthwt)
chunkSummary<- function(data){
  n<-nrow(data)
  n1<-ceiling(n/4)
  n2<-ceiling(n/2)
  n3<-ceiling(3*n/4)
  n4<-n
  dat1<-data[1:n1,,]
  dat2<-data[(n1+1):n2,,]
  dat3<-data[(n2+1):n3,,]
  dat4<-data[(n3+1):n4,,]

  nvar<-ncol(data)
  mean<-numeric(nvar)
  sd<-numeric(nvar)
  for(i in 1:nvar){
    mean[i]<-mean(data[,i])
    sd[i]<-sd(data[,i])
  }

names<-colnames(data)

out<-data.frame(Variable = names,
                Mean = round(mean,2),
                SD=round(sd,2))
names(out)[3] <- "Standard Deviation"
out

}
