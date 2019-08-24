#include <RcppArmadillo.h>
#include <algorithm>
#include <Rcpp.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;
using namespace arma;
using namespace std;

NumericVector callrpg(int num, NumericVector h, NumericVector z) {
  Environment package_env("package:BayesLogit");
  Function rfun = package_env["rpg"];    
  return rfun(num,h, z);
}

// [[Rcpp::export]]
Rcpp::NumericVector subset_range(Rcpp::NumericVector x, int start, int end) {
  return x[Rcpp::Range(start, end)];
}

// [[Rcpp::export]]
NumericVector rep_adv(NumericVector x, NumericVector y) {
  std::vector<double> myvector(sum(y));
  int ind=0;
  for (int i=0; i < y.size(); ++i) {
    std::fill(myvector.begin()+ind, myvector.begin()+ind+y[i], x[i]);
    ind += y[i];
  }
  return Rcpp::wrap(myvector);
}

// [[Rcpp::export]]
double innerProduct(NumericMatrix x, NumericVector y,int i) {
  NumericVector xk = x(i,_);
  y.erase(i);
  return std::inner_product(xk.begin(), xk.end(), y.begin(), 0.0);
}

//[[Rcpp::export]]
arma::mat matmult_sp(const arma::mat X, const arma::sp_mat Y){
  arma::mat ret = X.t() * Y * X; 
  return ret;
} 

// [[Rcpp::export]]
Rcpp::List GibbsC_Andrew(NumericVector y, NumericVector logE, 
                  int ne, arma::sp_mat L, NumericVector Ldiag, NumericMatrix L_b,
                  int nsim, int nburn, int thin, NumericVector nkid,
                  double d0, double g0, double e0, double f0){
  
  int nb=nkid.length(); 
  int N = y.length();
  double gam = 1.0;
  double  r = 1.0; 
  NumericVector b(nb);   
  NumericVector bid = rep_adv(b, nkid);

  int niter = (nsim - nburn) / thin;
  NumericMatrix bsave(niter, nb);
  NumericVector rsave(niter), gamsave(niter);
  List res;

  for (int g = nburn+1; g <= nsim; g++){

    if(g % thin == 0){
      NumericVector om = callrpg(N, y+r, bid+logE);
      NumericVector z=(((y-r)/2)/om)-logE; 
      NumericVector omz = om*z;

      int start = 0;
      for(int k=0; k < nb; k++){
          //update b
        int end = start + nkid[k] -1; 
        double v = 1/(sum(subset_range(om, start, end)) + gam*Ldiag(k));
        double lbk=innerProduct(L_b, b, k);
        double m = v*(sum(subset_range(omz,start, end)) - gam*lbk);
        b[k]=R::rnorm(m, sqrt(v));
        start = start + nkid[k];

      }

      bid = rep_adv(b, nkid);
      NumericVector bminuslog = -bid - logE;
      NumericVector p = 1/(1+exp(bminuslog));

        //update gam
      arma::mat bLb= matmult_sp(b, L); //sumpairb=(t(b)%*%L%*%b)= bLb
      gam=R::rgamma(g0+ne/2,2*d0/(2+d0*bLb(0,0)));
      
      int l = 0;	
      for (int i = 0; i < N; i++){
        if(y[i] > 0){
          for(int j=0; j < y[i]; j++){
            double ps = r / (r + j);
            l = l + R::rbinom( 1, ps);
          }
        }
      }
      r = R::rgamma(e0+l, 1/(f0-sum(log(1-p))));;

      int ii = ((g - nburn) / thin) -1;
      rsave(ii) = r;
      bsave(ii,_) = b;
      gamsave(ii)= gam;
    } 
  }
  return Rcpp::List::create(Rcpp::Named("bsave")=bsave,
                            Rcpp::Named("rsave")=rsave,
                            Rcpp::Named("gamsave")=gamsave);
} 



