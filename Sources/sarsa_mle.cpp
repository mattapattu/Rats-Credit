// [[Rcpp::depends(RcppArmadillo)]]
#include <vector>
#include <RcppArmadillo.h>
#include <regex>
#include <RcppArmadilloExtensions/sample.h>


  

using namespace Rcpp;


double softmax_cpp4(int A,int S,arma::mat &H){
  arma::rowvec v = H.row(S);
  //Rcpp::Rcout <<  v<< std::endl;
  // float m=arma::max(v);
  // double exp_sum  = std::exp(H(S,0)-m)+std::exp(H(S,1)-m)+std::exp(H(S,2)-m)+std::exp(H(S,3)-m)+std::exp(H(S,4)-m)+std::exp(H(S,5)-m) ;
  // double pr_A = (std::exp(H(S,A)-m))/exp_sum;
  
  double m=arma::max(v);
  v=exp(v-m);
  //Rcpp::Rcout << "m=" << m<< std::endl;
  double exp_sum  = arma::accu(v) ;
  v=v/exp_sum;
  double pr_A=v[A];
  
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

int sarsa_getNextState(int curr_state,int action){
  int new_state=-1;
  if(action == 4){
    new_state=curr_state;
  }else if(curr_state==0){
    new_state=1;
  }else if(curr_state==1){
    new_state=0;
  }
  
  return(new_state);
}

int softmax_action_sel(arma::mat &H,int S){
  
  arma::rowvec v = H.row(S);
  //Rcpp::Rcout <<  H<< std::endl;
  double m=arma::max(v);
  v=exp(v-m);
  //Rcpp::Rcout << "m=" << m<< std::endl;
  double exp_sum  = arma::accu(v) ;
  v=v/exp_sum;
  IntegerVector actions = seq(0, 5);
  //Rcpp::Rcout << "v=" << v<< std::endl;
  //Rcpp::Rcout << "actions=" << actions<< std::endl;
  int action_selected = Rcpp::RcppArmadillo::sample(actions, 1, false, v.as_col())[0] ;
  //Rcpp::Rcout << "action_selected=" << action_selected<< std::endl;
  return(action_selected);
  
}

// [[Rcpp::export("sarsa_gen_sim")]]
arma::mat sarsa_gen_sim(arma::mat &Q_vals,float alpha,float gamma,float lambda,int total_trials,int init_state){
  //NumericMatrix likelihood(10,1298);
  //int d;
  //for(d =0;d<9;d++){
  
  //float alpha=alphas[d];
  //arma::mat H = arma::zeros(2,6);
  arma::mat R = arma::zeros(2,6);
  R(0,3)=1;
  R(1,3)=1;
  arma::mat Elig_trace = arma::zeros(2,6);
  //Rcpp::NumericMatrix H(2,6);
  //Rcpp::NumericMatrix Visits(2,6);
  
  //Rcpp::Rcout <<  H<< std::endl;
  //Rcpp::Rcout <<  Visits<< std::endl;
  arma::mat allpaths_sarsa = arma::zeros(total_trials,3);
  
  //double log_lik=0;
  
  int episode=1;
  int S=init_state;
  int A=softmax_action_sel(Q_vals,S);;
  arma::vec actions(1);
  actions.fill(-1);
  arma::vec states(1);
  states.fill(-1);
  
  
  int initState=0;
  bool changeState = false;
  bool returnToInitState = false;
  //int prev_ses=-1;
  //int trial=0;
  int i;
  //int avg_score = 0;
  bool resetVector = true;
  for ( i = 0; i < (total_trials-1); i++) {
    
    if(resetVector){
      initState=S;
      //Rcpp::Rcout <<"initState="<<initState<<std::endl;
      resetVector= false;
    }
    
    
    //  trial=std::atoi(enreg_pos(trial_pos,5));
    //Rcpp::Rcout <<"i="<<i<<  ", trial="<<trial<<std::endl;
    
    //Rcpp::Rcout <<  "trial_pos="<<trial_pos << ", enreg_trial="<< std::atoi(enreg_pos(trial_pos,5)) <<std::endl;
    //Rcpp::Rcout <<"H="<<H<<std::endl;

    // if(R(S,A)>0){
    //   score_episode = score_episode+1;
    // }
    //Rcpp::Rcout <<"i="<<i<<  ", A="<<A<<", S="<<S<<std::endl;
    
    allpaths_sarsa(i,0)=A;
    allpaths_sarsa(i,1)=S;
    allpaths_sarsa(i,2)=R(S,A);
    
    int S_prime=sarsa_getNextState(S,A);
    int A_prime=softmax_action_sel(Q_vals,S);
    
    Elig_trace(S,A)=Elig_trace(S,A)+1;
    double delta=R(S,A)+(gamma* Q_vals(S_prime,A_prime)) - Q_vals(S,A);
    Q_vals=Q_vals + (alpha*delta*Elig_trace);
    Elig_trace=Elig_trace*gamma*lambda;
    
    //Rcpp::Rcout << "A="<< A << ", S=" << S << ", S_prime="<< S_prime << std::endl;
    int sz = actions.n_elem;
    actions.resize(sz+1);
    actions(sz) = A;
    
    states.resize(sz+1);
    states(sz)=S;
    
    if(S_prime!=initState){
      changeState = true;
    }else if(S_prime==initState && changeState){
      returnToInitState = true;
    }
    //Rcpp::Rcout <<  "changeState="<< changeState <<  ", returnToInitState="<< returnToInitState<<std::endl;
    S=S_prime;
    A=A_prime;
    if(returnToInitState ){
      //Rcpp::Rcout <<  "Inside end episode"<<std::endl;
      changeState = false;
      returnToInitState = false;
      //Elig_trace=Elig_trace*0;
      if(episode==2){
        Elig_trace=Elig_trace*0;
        episode=0;
      }
      episode  = episode+1;
      actions=arma::vec(1);
      actions.fill(-1);
      states=arma::vec(1);
      states.fill(-1);
      //Rcpp::Rcout <<"actions="<<actions<<std::endl;
      //Rcpp::Rcout << "states="<<states<<std::endl;
      resetVector = true;
      
    }
  }
  
  return(allpaths_sarsa);
  
  
}

// [[Rcpp::export("sarsa_mle")]]
arma::vec sarsa_mle(Rcpp::NumericMatrix allpaths,float alpha,float gamma,float lambda, arma::mat &Q_vals,int sim){
  
  double log_lik=0;
  int nrow = allpaths.nrow();
  arma::vec mseMatrix=arma::zeros(nrow);
  arma::mat Elig_trace = arma::zeros(2,6);
  //arma::mat Q_vals = arma::zeros(2,6);
  int episode=1;
  int S=0;
  int A=0;
  arma::vec actions(1);
  actions.fill(-1);
  arma::vec states(1);
  states.fill(-1);
  
  if(sim==1){
    S=allpaths(0,1);
    A=allpaths(0,0);
  }else{
    S=allpaths(0,1)-1;
    A=allpaths(0,0)-1;
  }
  
  
  int initState=0;
  bool changeState = false;
  bool returnToInitState = false;
  
  int i;
  bool resetVector = true;
  for ( i = 0; i < (nrow-1); i++) {
    
    if(resetVector){
      initState=S;
      resetVector= false;
    }
    int R=allpaths(i,2);
    if(R > 0){
      R=1;
    }
    //Rcpp::Rcout << "S=" << S <<", A=" <<A  << std::endl;
    int S_prime=0;
    int A_prime=0;
    
    if(sim==1){
      S_prime=allpaths((i+1),1);
      A_prime=allpaths((i+1),0);
    }else{
      S_prime=allpaths((i+1),1)-1;
      A_prime=allpaths((i+1),0)-1;
    }
    
    //Rcpp::Rcout << "HERE1"<< std::endl;
    Elig_trace(S,A)=Elig_trace(S,A)+1;
    double delta=R+(gamma* Q_vals(S_prime,A_prime))-Q_vals(S,A);
    Q_vals=Q_vals+ (alpha*delta*Elig_trace);
    Elig_trace=Elig_trace*gamma*lambda;
    
    //Rcpp::Rcout << "HERE2"<< std::endl;
    int sz = actions.n_elem;
    actions.resize(sz+1);
    actions(sz) = A;
    
    states.resize(sz+1);
    states(sz)=S;
    
    if(S_prime!=initState){
      changeState = true;
    }else if(S_prime==initState && changeState){
      returnToInitState = true;
    }
    
    double prob_a = softmax_cpp4(A,S,Q_vals);
    
    
    double logProb = log(prob_a);
    if(R_IsNaN((logProb))){
      
      Rcpp::Rcout <<"alpha=" << alpha  << std::endl;
      Rcpp::Rcout << "i=" << i  << std::endl;
      Rcpp::Rcout <<  "A="<< A << ", S=" << S << ", S_prime="<< S_prime << std::endl;
      Rcpp::Rcout << "prob_a=" << prob_a << std::endl;
      Rcpp::Rcout << "logProb=" << logProb << std::endl;
      // 
      Rcpp::Rcout <<  Q_vals<< std::endl;
      ////Rcpp::Rcout <<  Visits<< std::endl;
      stop("logProb is NAN");
      //return(100000);
    }
    
    mseMatrix(i)=logProb;
    log_lik=log_lik+ logProb;
    
    S=S_prime;
    A=A_prime;
    //Check if episode ended
    if(returnToInitState ){
      //Rcpp::Rcout <<  "Inside end episode"<<std::endl;
      changeState = false;
      returnToInitState = false;
      //Elig_trace=Elig_trace*0;
      if(episode==2){
        Elig_trace=Elig_trace*0;
        episode=0;
      }
      episode  = episode+1;
      actions=arma::vec(1);
      actions.fill(-1);
      states=arma::vec(1);
      states.fill(-1);
      
    }
  }
  return(mseMatrix);
  
  
}  


// [[Rcpp::export("sarsaGetProbMatrix")]]
arma::mat sarsaGetProbMatrix(Rcpp::NumericMatrix allpaths,float alpha,float gamma,float lambda, arma::mat &Q_vals,int sim){
  arma::mat Elig_trace = arma::zeros(2,6);
  //arma::mat Q_vals = arma::zeros(2,6);
  int episode=1;
  int S=0;
  int A=0;
  arma::vec actions(1);
  actions.fill(-1);
  arma::vec states(1);
  states.fill(-1);
  
  if(sim==1){
    S=allpaths(0,1);
    A=allpaths(0,0);
  }else{
    S=allpaths(0,1)-1;
    A=allpaths(0,0)-1;
  }
  
  
  int initState=0;
  bool changeState = false;
  bool returnToInitState = false;
  int nrow = allpaths.nrow();
  int i;
  bool resetVector = true;
  arma::mat probMatrix_sarsa=arma::zeros(nrow,12);
  
  for ( i = 0; i < (nrow-1); i++) {
    
    if(resetVector){
      initState=S;
      resetVector= false;
    }
    int R=allpaths(i,2);
    if(R > 0){
      R=1;
    }
    //Rcpp::Rcout << "S=" << S <<", A=" <<A  << std::endl;
    int S_prime=0;
    int A_prime=0;
    
    if(sim==1){
      S_prime=allpaths((i+1),1);
      A_prime=allpaths((i+1),0);
    }else{
      S_prime=allpaths((i+1),1)-1;
      A_prime=allpaths((i+1),0)-1;
    }
    
    //Rcpp::Rcout << "HERE1"<< std::endl;
    Elig_trace(S,A)=Elig_trace(S,A)+1;
    double delta=R+(gamma* Q_vals(S_prime,A_prime))-Q_vals(S,A);
    Q_vals=Q_vals+ (alpha*delta*Elig_trace);
    Elig_trace=Elig_trace*gamma*lambda;
    
    //Rcpp::Rcout << "HERE2"<< std::endl;
    int sz = actions.n_elem;
    actions.resize(sz+1);
    actions(sz) = A;
    
    states.resize(sz+1);
    states(sz)=S;
    
    if(S_prime!=initState){
      changeState = true;
    }else if(S_prime==initState && changeState){
      returnToInitState = true;
    }
    
    if(S==0){
      probMatrix_sarsa.submat(i,6,i,11)=arma::zeros(1,6);
      //Rcpp::Rcout << "probMatrix_aca="<< probMatrix_aca << std::endl;
      for(int act=0;act<6;act++){
        double x = softmax_cpp4(act,0,Q_vals);
        probMatrix_sarsa(i,act)=x;
      }
    }else if(S==1){
      //Rcpp::Rcout << "i=" <<i<< std::endl;
      probMatrix_sarsa.submat(i,0,i,5)=arma::zeros(1,6);
      //Rcpp::Rcout << "probMatrix_aca="<< probMatrix_aca << std::endl;
      for(int act=0;act<6;act++){
        double x = softmax_cpp4(act,1,Q_vals);
        probMatrix_sarsa(i,(6+act))=x;
      }
    }
    
    S=S_prime;
    A=A_prime;
    //Check if episode ended
    if(returnToInitState ){
      //Rcpp::Rcout <<  "Inside end episode"<<std::endl;
      changeState = false;
      returnToInitState = false;
      //Elig_trace=Elig_trace*0;
      if(episode==2){
        Elig_trace=Elig_trace*0;
        episode=0;
      }
      episode  = episode+1;
      actions=arma::vec(1);
      actions.fill(-1);
      states=arma::vec(1);
      states.fill(-1);
      
    }
  }
  return(probMatrix_sarsa);
}

