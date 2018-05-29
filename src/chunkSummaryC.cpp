#include <Rcpp.h>
using namespace Rcpp;



// [[Rcpp::export]]
DataFrame chunkSummaryC(DataFrame x) {
  int nrow = x.nrow();
  int ncol = x.ncol();
  double n11 = float(nrow)/4;
  double n21 = float(nrow)/2;
  double n31 = float(nrow)*3/4;
  double n41 = float(nrow);
  int n1 = ceil(n11);
  int n2 = ceil(n21);
  int n3 = ceil(n31);
  int n4 = ceil(n41);


  NumericMatrix dataall = internal::convert_using_rfunction(x, "as.matrix");


 NumericMatrix dat1 =dataall(Range(0,n1-1),Range(0,ncol-1));
 NumericMatrix dat2 =dataall(Range(n1,n2-1),Range(0,ncol-1));
 NumericMatrix dat3 =dataall(Range(n2,n3-1),Range(0,ncol-1));
 NumericMatrix dat4 =dataall(Range(n3,n4-1),Range(0,ncol-1));

 NumericVector ave1 = no_init(ncol);
 NumericVector std1 = no_init(ncol);
 for(int i = 0; i <ncol; i++){
   NumericMatrix::Column result1 = dat1(_,i);
   ave1[i] = mean(result1);
   std1[i] = sd(result1);
 }


 NumericVector ave2 = no_init(ncol);
 NumericVector std2 = no_init(ncol);
 for(int i = 0; i <ncol; i++){
   NumericMatrix::Column result2 = dat2(_,i);
   ave2[i] = mean(result2);
   std2[i] = sd(result2);
 }


 NumericVector ave3 = no_init(ncol);
 NumericVector std3 = no_init(ncol);
 for(int i = 0; i <ncol; i++){
   NumericMatrix::Column result3 = dat3(_,i);
   ave3[i] = mean(result3);
   std3[i] = sd(result3);
 }


 NumericVector ave4 = no_init(ncol);
 NumericVector std4 = no_init(ncol);
 for(int i = 0; i <ncol; i++){
   NumericMatrix::Column result4 = dat4(_,i);
   ave4[i] = mean(result4);
   std4[i] = sd(result4);
 }


 CharacterVector names = Rf_getAttrib(x, R_NamesSymbol);

 List final;
 final["Variables"] = names;
 final["Chunk1.mean1"]=round(ave1,2);
 final["Chunk1.sd1"]=round(std1,2);
 final["Chunk2.mean2"]=round(ave2,2);
 final["Chunk2.sd2"]=round(std2,2);
 final["Chunk3.mean3"]=round(ave3,2);
 final["Chunk3.sd3"]=round(std3,2);
 final["Chunk4.mean4"]=round(ave4,2);
 final["Chunk4.sd4"]=round(std4,2);


 return final;

}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.
//

/*** R
library(MASS)
chunkSummaryC(birthwt)

*/
