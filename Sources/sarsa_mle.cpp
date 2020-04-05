// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::depends(BH)]]
#include <vector>
#include <RcppArmadillo.h>
#include <regex>




using namespace Rcpp;




double softmax_cpp(int A,int S,arma::mat &H){
  //Rcpp::Rcout <<  "S="<< S<<std::endl;
  arma::rowvec v = H.row(S);
  //Rcpp::Rcout <<  v<< std::endl;
  float m=arma::max(v);
  double exp_sum  = 0;
  for(int j=0;j<5;j++){
    double denom_a = (H(S,j)-m);
    double exp_action= std::exp(denom_a); 
    //Rcpp::Rcout << "tau="<<tau << ", denom_a="<<denom_a<<", exp_action=" << exp_action<< std::endl;
    exp_sum = exp_sum+exp_action;
  }
  double pr_A = (std::exp((H(S,A)-m)))/exp_sum;
  if(pr_A<0){
    Rcpp::Rcout << "S=" <<S << std::endl;
    Rcpp::Rcout << H(0,0)  << std::endl;
    Rcpp::Rcout << "Numerator=" << std::exp(H(S,A)-m)  << std::endl;
    Rcpp::Rcout << "(std::exp(H(S,0))-m)=" << (H(S,0))-m  << std::endl;
    Rcpp::Rcout << "(std::exp(H(S,0))-m)=" << (std::exp(H(S,1))-m)  << std::endl;
    Rcpp::Rcout << "(std::exp(H(S,0))-m)=" << (std::exp(H(S,2))-m)  << std::endl;
    Rcpp::Rcout << "(std::exp(H(S,0))-m)=" << (std::exp(H(S,3))-m)  << std::endl;
    Rcpp::Rcout << "(std::exp(H(S,0))-m)=" << (std::exp(H(S,4))-m)  << std::endl;
    Rcpp::Rcout << "(std::exp(H(S,0))-m)=" << (std::exp(H(S,5))-m)  << std::endl;
    
  }
  if(R_IsNaN((pr_A))){
    
    Rcpp::Rcout << "exp_sum=" << exp_sum  << std::endl;
    Rcpp::Rcout << "numerator=" << (std::exp(H(S,A)-m))  << std::endl;
    Rcpp::Rcout <<  "pr_A="<< pr_A << std::endl;
    
    Rcpp::Rcout <<  H<< std::endl;
    //stop("logProb is NAN");
  }
  
  // if(Rcpp::traits::is_infinite<REALSXP>(prob_a)){
  //   Rcpp::Rcout <<  "Prb of action seclection = 1" << std::endl;;
  // }    
  //Rcpp::Rcout <<  "pr_A="<< pr_A<<std::endl;
  return(pr_A);
}



// [[Rcpp::export("sarsa_mle_enreg")]]
double sarsa_mle_enreg(Rcpp::StringMatrix allpaths,float alpha,float gamma,float lambda, arma::mat &H){

  //Rcpp::Rcout <<  "alpha="<<alpha<<", epsLim="<<epsLim<< std::endl;
  //Rcpp::Rcout <<  H<< std::endl;
  //Rcpp::Rcout <<  Visits<< std::endl;
  
  double log_lik=0;
  arma::mat Elig_trace = arma::zeros(2,6);
  arma::mat Q_vals = arma::zeros(2,6);
  int episode=1;
  int S=0;
  int A=0;
  arma::vec actions(1);
  actions.fill(-1);
  arma::vec states(1);
  states.fill(-1);
  arma::vec time_taken_for_trial(1);
  time_taken_for_trial.fill(-1);
  std::regex a("^.*e$"); 
  std::regex b("^.*i$");
  std::cmatch base_match;
  
  S=std::atoi(allpaths(0,4))-1;
  A=std::atoi(allpaths(0,2))-1;
  int initState=0;
  bool changeState = false;
  bool returnToInitState = false;
  //int score_episode=0;
  //int episodeFin=0;
  //int prev_ses=-1;
  //int trial_pos=0;
  int nrow = allpaths.nrow();
  //int enreg_rows = enreg_pos.nrow();
  //int trial=0;
  int i;
  //int avg_score = 0;
  bool resetVector = true;
  for ( i = 0; i < (nrow-1); i++) {
    
    if(resetVector){
      initState=S;
      //Rcpp::Rcout <<"initState="<<initState<<std::endl;
      resetVector= false;
    }
    
    
    
    int R=std::atoi(allpaths(i,3));
    
    if(R>0){
      R=1;
    }
    int S_prime=std::atoi(allpaths((i+1),4))-1;
    int A_prime=std::atoi(allpaths((i+1),2))-1;
    //int S_prime=getNextState_cpp(allpaths,i)-1;
    if(S_prime<0){
      continue;
    }
    Elig_trace(S,A)=Elig_trace(S,A)+1;
    float delta=R+(gamma* Q_vals(S_prime,A_prime)) - Q_vals(S,A);
    Q_vals=Q_vals+ (alpha*delta*Elig_trace);
    Elig_trace=Elig_trace*gamma*lambda;
    
    //Rcpp::Rcout << "trial="<<trial <<  ", A="<< A << ", S=" << S << ", S_prime="<< S_prime << std::endl;
    int sz = actions.n_elem;
    actions.resize(sz+1);
    actions(sz) = A;
    
    states.resize(sz+1);
    states(sz)=S;
    
    //Rcpp::Rcout << "i="<<i<<", trial="<<trial <<  ", ses="<< ses << ", time_taken_for_trial=" << (allpaths(i,4)) << std::endl;

    
    if(S_prime!=initState){
      changeState = true;
    }else if(S_prime==initState && changeState){
      returnToInitState = true;
    }
    //Rcpp::Rcout <<  "changeState="<< changeState <<  ", returnToInitState="<< returnToInitState<<std::endl;
    
    double prob_a = softmax_cpp(A,S,H);
    
    //Rcpp::Rcout << "i=" << i  << std::endl;
    //Rcpp::Rcout << "prob_a=" << prob_a  << std::endl;
    
    double logProb = log(prob_a);
    if(R_IsNaN((logProb))){
      
      Rcpp::Rcout <<"alpha=" << alpha  << std::endl;
      Rcpp::Rcout << "i=" << i  << std::endl;
      Rcpp::Rcout <<  "A="<< A << ", S=" << S << ", S_prime="<< S_prime << std::endl;
      Rcpp::Rcout << "prob_a=" << prob_a << std::endl;
      Rcpp::Rcout << "logProb=" << logProb << std::endl;
      
      Rcpp::Rcout <<  H<< std::endl;
      //Rcpp::Rcout <<  Visits<< std::endl;
      //stop("logProb is NAN");
      return(100000);
    }
    
    log_lik=log_lik+ logProb;
    
    S=S_prime;
    A=A_prime;
    //Check if episode ended
    if(returnToInitState ){
      //Rcpp::Rcout <<  "Inside end episode"<<std::endl;
      changeState = false;
      returnToInitState = false;
      Elig_trace=Elig_trace*0;
      episode  = episode+1;
      actions=arma::vec();
      actions.fill(-1);
      states=arma::vec();
      states.fill(-1);
     
      
    }
    
    

  }
  return(log_lik*(-1));
  
  
}  
