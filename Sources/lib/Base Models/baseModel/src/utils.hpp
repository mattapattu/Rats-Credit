#ifndef __UTILS__
#define __UTILS__

#include <vector>
#include <RcppArmadillo.h>

#include <regex>
#include <RcppArmadilloExtensions/sample.h>

inline double softmax_cpp3(int A,int S,arma::mat &H){
  //Rcpp::Rcout <<  "S="<< S<<std::endl;
  arma::rowvec v = H.row(S);
  //Rcpp::Rcout <<  v<< std::endl;
  double m=arma::max(v);
  double exp_sum  = std::exp(H(S,0)-m)+std::exp(H(S,1)-m)+std::exp(H(S,2)-m)+std::exp(H(S,3)-m)+std::exp(H(S,4)-m)+std::exp(H(S,5)-m) ;
  double pr_A = (std::exp(H(S,A)-m))/exp_sum;
  
  //  float m=arma::max(v);
  //  v=exp(v-m);
  // // //Rcpp::Rcout << "m=" << m<< std::endl;
  //  double exp_sum  = arma::accu(v) ;
  //  v=v/exp_sum;
  //  double pr_A=v[A];
  
  if(pr_A<0){
    Rcpp::Rcout <<"A="<<A<< ", S=" <<S << std::endl;
    Rcpp::Rcout << H << std::endl;
    Rcpp::Rcout << "m=" <<m << std::endl;
    Rcpp::Rcout << "Numerator=" << std::exp(H(S,A)-m)  << std::endl;
    Rcpp::Rcout << "exp_sum=" << exp_sum  << std::endl;
    // Rcpp::Rcout << "std::exp(H(S,0))-m)=" << std::exp(H(S,0)-m)  << std::endl;
    // Rcpp::Rcout << "std::exp(H(S,1))-m)=" << std::exp(H(S,1)-m)  << std::endl;
    // Rcpp::Rcout << "std::exp(H(S,2))-m)=" << std::exp(H(S,2)-m)  << std::endl;
    // Rcpp::Rcout << "std::exp(H(S,3))-m)=" << std::exp(H(S,3)-m)  << std::endl;
    // Rcpp::Rcout << "std::exp(H(S,4))-m)=" << std::exp(H(S,4)-m)  << std::endl;
    // Rcpp::Rcout << "std::exp(H(S,5))-m)=" << std::exp(H(S,5)-m)  << std::endl;
    Rcpp::Rcout << v  << std::endl;
    
    //stop("logProb is NAN");
    
  }else if(pr_A>1){
    Rcpp::Rcout <<"pr_A="<<pr_A<< " is > 1" << std::endl;
    Rcpp::Rcout <<"A="<<A<< ", S=" <<S << std::endl;
    Rcpp::Rcout << H << std::endl;
    Rcpp::Rcout << "m=" <<m << std::endl;
    Rcpp::Rcout << "Numerator=" << std::exp(H(S,A)-m)  << std::endl;
    Rcpp::Rcout << "exp_sum=" << exp_sum  << std::endl;
    Rcpp::Rcout << "(std::exp(H(S,0))-m)=" << std::exp(H(S,0)-m)  << std::endl;
    Rcpp::Rcout << "(std::exp(H(S,1))-m)=" << std::exp(H(S,1)-m)  << std::endl;
    Rcpp::Rcout << "(std::exp(H(S,2))-m)=" << std::exp(H(S,2)-m)  << std::endl;
    Rcpp::Rcout << "(std::exp(H(S,3))-m)=" << std::exp(H(S,3)-m)  << std::endl;
    Rcpp::Rcout << "(std::exp(H(S,4))-m)=" << std::exp(H(S,4)-m)  << std::endl;
    Rcpp::Rcout << "(std::exp(H(S,5))-m)=" << std::exp(H(S,5)-m)  << std::endl;
  }
  if(R_IsNaN((pr_A))){
    
    Rcpp::Rcout << "exp_sum=" << exp_sum  << std::endl;
    Rcpp::Rcout << "numerator=" << (std::exp(H(S,A)-m))  << std::endl;
    Rcpp::Rcout <<  "pr_A="<< pr_A << std::endl;
    Rcpp::Rcout <<  H<< std::endl;
    Rcpp::Rcout <<"A=" <<A<< ", S=" <<S <<", m="<<m<< std::endl;
    Rcpp::Rcout << "Numerator=" << std::exp(H(S,A)-m)  << std::endl;
    Rcpp::Rcout << "(std::exp(H(S,0))-m)=" << std::exp(H(S,0)-m)  << std::endl;
    Rcpp::Rcout << "(std::exp(H(S,1))-m)=" << std::exp(H(S,1)-m)  << std::endl;
    Rcpp::Rcout << "(std::exp(H(S,2))-m)=" << std::exp(H(S,2)-m)  << std::endl;
    Rcpp::Rcout << "(std::exp(H(S,3))-m)=" << std::exp(H(S,3)-m)  << std::endl;
    Rcpp::Rcout << "(std::exp(H(S,4))-m)=" << std::exp(H(S,4)-m)  << std::endl;
    Rcpp::Rcout << "(std::exp(H(S,5))-m)=" << std::exp(H(S,5)-m)  << std::endl;
    //stop("logProb is NAN");
  }
  
  // if(Rcpp::traits::is_infinite<REALSXP>(prob_a)){
  //   Rcpp::Rcout <<  "Prb of action seclection = 1" << std::endl;;
  // }
  //Rcpp::Rcout <<  "pr_A="<< pr_A<<std::endl;
  return(pr_A);
}


#endif