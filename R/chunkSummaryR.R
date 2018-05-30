#' Chunk Summary
#'
#' Divide the whole dataset into four chunks and calculate the mean and standard deviation of each chunks.
#'
#'
#' @param data  A data frame
#'
#' @return A table is provided, which include the names of each column and the results.
#'
#' @examples
#' library(MASS)
#' data<-birthwt
#' chunkSummaryR(data)
#'
#' @seealso \code{\link[lng]{chunkSummaryC}}
#'
#' @references Hadley Wickham (2015) R package.
#'
#'@export
chunkSummaryR<- function(data){
  #Get the total numbers of rows
  n<-nrow(data)

  #Divide the rows into four parts
  n1<-ceiling(n/4)
  n2<-ceiling(n/2)
  n3<-ceiling(3*n/4)
  n4<-n

  #Divide the whole dataset into four chunks based on rows
  dat1<-data[1:n1,,]
  dat2<-data[(n1+1):n2,,]
  dat3<-data[(n2+1):n3,,]
  dat4<-data[(n3+1):n4,,]

  #Calculate the mean and standard deviation of each chunk
  mean1<-round(apply(dat1,2,mean),2)
  sd1<-round(apply(dat1,2,sd),2)
  mean2<-round(apply(dat2,2,mean),2)
  sd2<-round(apply(dat2,2,sd),2)
  mean3<-round(apply(dat3,2,mean),2)
  sd3<-round(apply(dat3,2,sd),2)
  mean4<-round(apply(dat4,2,mean),2)
  sd4<-round(apply(dat4,2,sd),2)

  #Combine four chunks and the names of each column
  names<-colnames(data)
  out<-data.frame(Variables = names,
                  Chunk1 = data.frame(mean1,sd1),
                  Chunk2 = data.frame(mean2,sd2),
                  Chunk3 = data.frame(mean3,sd3),
                  Chunk4 = data.frame(mean4,sd4),
                  row.names = NULL)

  #Export the final dataframe
  out

}
