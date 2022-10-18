#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double sign(double val){
  if (val > 0) return 1.0;
  if (val < 0) return -1.0;
  return 0;
  
}
// [[Rcpp::export]]
Rcpp::List checkPairs(NumericVector x,NumericVector y) {
  int Con=0;
  int Dis=0;
  int Tie=0;
  int Left=0;
  int Right=0;
  double tau=0;
  int n=x.size();
  for(int i=0; i<n-1; ++i){
    for(int j=i+1; j<n; ++j){
      tau += sign(x[i]-x[j]) * sign(y[i]-y[j]) * 1/();
    }
  }
  return tau;
}


