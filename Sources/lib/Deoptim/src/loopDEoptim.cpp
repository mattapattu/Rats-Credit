#include <RcppArmadillo.h>  

Rcpp::List DEoptim_impl(const arma::colvec & minbound,                  // user-defined lower bounds
                        const arma::colvec & maxbound,                  // user-defined upper bounds
                        SEXP fnS,                                       // function to be optimized, either R or C++
                        const Rcpp::List & control,                     // parameters 
                        SEXP rhoS);

// [[Rcpp::export]]
Rcpp::List loopDEoptim(const arma::colvec & minbound,                  // user-defined lower bounds
                       const arma::colvec & maxbound,                  // user-defined upper bounds
                       SEXP fnS,                                       // )
                       const Rcpp::List & control,                     // parameters 
                       SEXP rhoS1,SEXP rhoS2,SEXP rhoS3) {
  
  Rcpp::List res1 = DEoptim_impl(minbound,maxbound,fnS,control,rhoS1);
  Rcpp::List res2 = DEoptim_impl(minbound,maxbound,fnS,control,rhoS2);
  Rcpp::List res3 = DEoptim_impl(minbound,maxbound,fnS,control,rhoS3);
  return Rcpp::List::create(Rcpp::Named("res1") = res1,
                            Rcpp::Named("res2") = res2,
                            Rcpp::Named("res3") = res3);
                            
}
