#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]

int LsumC(IntegerVector y, double r) {
  int n=y.size();    
  int l = 0;	
  for (int i = 0; i < n; i++){
    if(y[i] > 0){
      for(int j=0; j < y[i]; j++){
        double ps = r / (r + j);
        l = l + R::rbinom(1, ps);
      }
    }
  }
  return(l);
}


// [[Rcpp::export]]
int LsumC_new(IntegerVector y, double r, double cutoff=0.0) {
  double l = 0;
  int n = y.size();
  if(cutoff == 0.0){
    for (int i = 0; i < n; i++){
      if(y[i]>0){
        for(int j=0; j < y[i]; j++){
          double ps = r / (r + j);
          l = l + R::rbinom(1, ps);
        }
      }
    }
  }
  else { //cutoff != 0.0
    int end = floor((r*(1-cutoff)/cutoff)+1);
    double rend = r + end;
    long double err = 1.0;
    for (int i = 0; i < n; i++){ 
      if(y[i]>0){  
        for(int j=0; j<std::min(y[i],end); j++){
          double ps = r / (r + j);
          l = l + R::rbinom(1, ps);}
        if(y[i]>end){
          err = err * ((r+y[i])/rend);
        } 
      }
    }
    l = l + r*log(err);
  }
  return(std::round(l));
}