#' Univariate analysis and summary table
#'
#' Generate summary table proving the univariate analysis for each independent variabls and dependent variable from the whole dataset
#'
#'
#' @param y   dependent variable which selected by user
#' @param data  a data frame, list the variables to be summarized (contain the variable Y)
#' @param alternative	  a character string specifying the alternative approaches, must be one of "linear" (default), "logistic"
#' @param digits     the digits of the data values (default is 3)
#' @param ...	  further arguments to be passed to or from methods
#'
#' @return  For univariate analysis, it returns a summary table based on the type of dependent variables. For continuous Y, the table contains parameters, coefficient, p-value, CI for coefficient; while for categorical variable, the table contains the parameters, Odds ratio, p-value, CI for odds ratio.
#'
#' @references Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988) The New S Language. Wadsworth & Brooks/Cole.
#'
#'
#'@import tidyverse
#'
#'@export

smruni<-function(y,
                 data,
                 alternative = c("linear","logistic"),
                 digit = 3,
                 ...){
  alternative <- match.arg(alternative)

  if(!missing(alternative)){
    if(alternative=="logistic"){
      #logistic

      #get the name and the number of the Y
      y_length <- length(y)
      data_names <- colnames(data)
      name_length <- length(data_names)
      result <- numeric(name_length)
      for (i in 1:name_length) {
        result[i] <- sum(y %in% factor(unlist(data[data_names[i]])))
      }
      ncol_y <- which(result == y_length)
      y_group <- data_names[ncol_y]

      #re-arrange the whole dataset
      data1<-data[,-ncol_y]
      data2<-cbind(data[,ncol_y],data1)
      data2<-as.data.frame(data2)



      #do the logistic regression and get the coefficient/p-value/standard error

      coe <- numeric(ncol(data2)-1)
      pva<-numeric(ncol(data2)-1)
      se <-numeric(ncol(data2)-1)
      for(i in 2:ncol(data2)){
        se[i]<-summary(glm(data2[,1]~data2[,i],family=binomial))$coefficients[2,2]
        coe[i]<-summary(glm(data2[,1]~data2[,i],family=binomial))$coefficients[2,1]
        pva[i]<-summary(glm(data2[,1]~data2[,i],family=binomial))$coefficients[2,4]
      }

      se <-se[-1]
      coe <-coe[-1]
      pva <-pva[-1]

      #calculate the coffience interval and the or
      ci1 <- numeric(length(se))
      ci2 <-numeric(length(se))

      for(i in 1:length(se)){
        ci1[i]<- coe[i]-1.96*se[i]
        ci2[i]<- coe[i]+1.96*se[i]
      }

      or<-exp(cbind(OR=coe, ci1,ci2))

      #change the form of the data
      test<-function(q){
        if(q<0.001) q.txt<-"<0.001"
        else if(q<0.05) q.txt<-"<0.05"
        else q.txt <-format(q,digits = digit)
      }

      #change the form of p-value
      pvalue <- numeric(nrow(or))
      for(i in 1:nrow(or)){
        pvalue[i]<-test(pva[i])
      }




      #add the name of rows
      data_names <- colnames(data2)
      names<-data_names[-1]
      out <- data.frame(Parameters=names,
                        Coefficient = format(coe,digits = digit),
                        p.value = pvalue,
                        OR = format(or[,1],digits = digit),
                        CI= paste0("(",
                                   format(or[,2],digits = digit),
                                   ",",
                                   format(or[,3],digits = digit),
                                   ")"))
      names(out)[5] <- "CI(95%)"
      out

    }else{
      #linear

      #get the name and the number of the Y
      y_length <- length(y)
      data_names <- colnames(data)
      name_length <- length(data_names)
      result <- numeric(name_length)
      for (i in 1:name_length) {
        result[i] <- sum(y %in% factor(unlist(data[data_names[i]])))
      }
      ncol_y <- which(result == y_length)
      y_group <- data_names[ncol_y]

      #re-arrange the whole dataset
      data1<-data[,-ncol_y]
      data2<-cbind(data[,ncol_y],data1)
      data2<-as.data.frame(data2)




      #do the logistic regression and get the coefficient/p-value/standard error
      coe1 <- numeric(ncol(data2)-1)
      pva1<-numeric(ncol(data2)-1)
      se1 <-numeric(ncol(data2)-1)

      for(i in 2:ncol(data2)){
        se1[i]<-summary(lm(data2[,1]~data2[,i]))$coefficients[2,2]
        coe1[i]<-summary(lm(data2[,1]~data2[,i]))$coefficients[2,1]
        pva1[i]<-summary(lm(data2[,1]~data2[,i]))$coefficients[2,4]
      }
      se1 <-se1[-1]
      coe1 <-coe1[-1]
      pva1 <-pva1[-1]

      #calculate the coffience interval and the or
      ci21 <- numeric(length(se1))
      ci22 <-numeric(length(se1))

      for(i in 1:length(se1)){
        ci21[i]<- coe1[i]-1.96*se1[i]
        ci22[i]<- coe1[i]+1.96*se1[i]
      }



      #change the form of the data
      test<-function(q){
        if(q<0.001) q.txt<-"<0.001"
        else if(q<0.05) q.txt<-"<0.05"
        else q.txt <-format(q,digits = digit)
      }

      #change the form of p-value
      pvalue1 <- numeric(length(pva1))
      for(i in 1:length(pva1)){
        pvalue1[i]<-test(pva1[i])
      }


      #add the name of rows
      data_names <- colnames(data2)
      names<-data_names[-1]
      out1 <- data.frame(Parameters=names,
                         Coefficient = format(coe1,digits = digit),
                         p.value = pvalue1,
                         CI= paste0("(",
                                    format(ci21,digits = digit),
                                    ",",
                                    format(ci22,digits = digit),
                                    ")"))
      names(out1)[4] <- "CI(95%)"
      out1
    }
  }else{

    #linear

    #get the name and the number of the Y
    y_length <- length(y)
    data_names <- colnames(data)
    name_length <- length(data_names)
    result <- numeric(name_length)
    for (i in 1:name_length) {
      result[i] <- sum(y %in% factor(unlist(data[data_names[i]])))
    }
    ncol_y <- which(result == y_length)
    y_group <- data_names[ncol_y]

    #re-arrange the whole dataset
    data1<-data[,-ncol_y]
    data2<-cbind(data[,ncol_y],data1)
    data2<-as.data.frame(data2)




    #do the logistic regression and get the coefficient/p-value/standard error
    coe1 <- numeric(ncol(data2)-1)
    pva1<-numeric(ncol(data2)-1)
    se1 <-numeric(ncol(data2)-1)

    for(i in 2:ncol(data2)){
      se1[i]<-summary(lm(data2[,1]~data2[,i]))$coefficients[2,2]
      coe1[i]<-summary(lm(data2[,1]~data2[,i]))$coefficients[2,1]
      pva1[i]<-summary(lm(data2[,1]~data2[,i]))$coefficients[2,4]
    }
    se1 <-se1[-1]
    coe1 <-coe1[-1]
    pva1 <-pva1[-1]

    #calculate the coffience interval and the or
    ci21 <- numeric(length(se1))
    ci22 <-numeric(length(se1))

    for(i in 1:length(se1)){
      ci21[i]<- coe1[i]-1.96*se1[i]
      ci22[i]<- coe1[i]+1.96*se1[i]
    }




    #change the form of the data
    test<-function(q){
      if(q<0.001) q.txt<-"<0.001"
      else if(q<0.05) q.txt<-"<0.05"
      else q.txt <-format(q,digits = digit)
    }

    #change the form of p-value
    pvalue1 <- numeric(length(pva1))
    for(i in 1:length(pva1)){
      pvalue1[i]<-test(pva1[i])
    }


    #add the name of rows
    data_names <- colnames(data2)
    names<-data_names[-1]
    out1 <- data.frame(Parameters=names,
                       Coefficient = format(coe1,digits = digit),
                       p.value = pvalue1,
                       CI= paste0("(",
                                  format(ci21,digits = digit),
                                  ",",
                                  format(ci22,digits = digit),
                                  ")"))
    names(out1)[4] <- "CI(95%)"
    out1

  }


}
