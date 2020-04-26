// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::depends(BH)]]
#include <vector>
#include <RcppArmadillo.h>
#include <regex>
#include <RcppArmadilloExtensions/sample.h>




using namespace Rcpp;




double softmax_cpp3(int A,int S,arma::mat &H){
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
    stop("logProb is NAN");
  }
  
  // if(Rcpp::traits::is_infinite<REALSXP>(prob_a)){
  //   Rcpp::Rcout <<  "Prb of action seclection = 1" << std::endl;;
  // }    
  //Rcpp::Rcout <<  "pr_A="<< pr_A<<std::endl;
  return(pr_A);
}

int aca_getNextState(int curr_state,int action){
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
  double m=arma::max(v);
  v=exp(v-m);
  double exp_sum  = arma::accu(v) ;
  v=v/exp_sum;
  IntegerVector actions = seq(0, 5);
  int action_selected = Rcpp::RcppArmadillo::sample(actions, 1, true, v.as_col())[0] ;
  return(action_selected);
  
}

arma::mat updateHMat(arma::mat &H,arma::vec actions, arma::vec states, arma::vec time_taken_for_trial, double alpha,float score_episode,double avg_score, int model){

    if(model==1){
    arma::uvec state1_idx = find(states==0);
    arma::vec uniq_state1 = arma::unique(actions.elem(state1_idx));
    for(unsigned int l=0;l< uniq_state1.n_elem;l++){
      if(uniq_state1(l)==-1){
        continue;
      }
      double  curr_action = uniq_state1(l);
      arma::uvec a=arma::find(actions.elem(state1_idx)==curr_action);
      arma::vec time_s1 =  time_taken_for_trial.elem(state1_idx);
      double total_time_spent_in_state1 = arma::accu(time_s1);
      double activity= a.n_elem*1000/total_time_spent_in_state1;
      //Rcpp::Rcout <<  "activity="<<activity<< std::endl;
      
      H(0,curr_action)= H(0,curr_action)+alpha*(score_episode*activity);
      if(R_IsNaN((H(0,curr_action)))){
        Rcpp::Rcout <<  "state="<<0<<", action="<<curr_action<< std::endl;
        stop("H is NAN");
      }else if(H(0,curr_action) == R_PosInf){
        Rcpp::Rcout <<  "state="<<0<<", action="<<curr_action<<std::endl;
        stop("H is Inf");
      }else if(H(0,curr_action) < 0){
      }
      
    }
    
    
    
    arma::uvec state2_idx = find(states==1);
    arma::vec uniq_state2 = arma::unique(actions.elem(state2_idx));
    
    for(unsigned int l=0;l< uniq_state2.n_elem;l++){
      if(uniq_state2(l)==-1){
        continue;
      }
      double  curr_action = uniq_state2(l);
      arma::uvec a=arma::find(actions.elem(state2_idx)==curr_action);
      arma::vec time_s2 =  time_taken_for_trial.elem(state2_idx);
      double total_time_spent_in_state2 = arma::accu(time_s2);
      double activity=0;
      if(total_time_spent_in_state2>0){
        activity = a.n_elem*1000/total_time_spent_in_state2;
      }else{
        activity = a.n_elem/(1.0*state2_idx.n_elem);
      }
      
      H(1,curr_action)= H(1,curr_action)+alpha*(score_episode*activity);
      if(R_IsNaN((H(1,curr_action)))){
        Rcpp::Rcout <<  "state="<<1<<", action="<<curr_action<< std::endl;
        stop("H is NAN");
      }else if(H(1,curr_action) == R_PosInf){
        Rcpp::Rcout <<  "state="<<1<<", action="<<curr_action<<std::endl;
        stop("H is Inf");
      }else if(H(1,curr_action) < 0){
      }
    }
  }else if(model==2){
    
    // UPDATE CREDIT OF STATE 1 ACTIONS
    IntegerVector all_actions =  seq(0, 5);
    arma::uvec state1_idx = find(states==0);
    arma::vec uniq_state1 = arma::unique(actions.elem(state1_idx));
    
    //Rcpp::Rcout <<  "state1_idx="<< state1_idx<<std::endl;
    
    
    // UPDATE CREDIT OF STATE 1 ACTIONS SECLECTED DURING LAST N EPISODS
    for(unsigned int l=0;l< uniq_state1.n_elem;l++){
      if(uniq_state1(l)==-1){
        continue;
      }
      double  curr_action = uniq_state1(l)*1.00;
      double delta_H = alpha*(score_episode-avg_score)*(1-softmax_cpp3(curr_action,0,H));
      H(0,curr_action)= H(0,curr_action)+delta_H;
      if(R_IsNaN((H(0,curr_action)))){
        Rcpp::Rcout <<  "state="<<0<<", action="<<curr_action<< std::endl;
        //Rcpp::Rcout <<  "H="<< H(0,curr_action)<<std::endl;
        //Rcpp::Rcout <<  "Visits="<< Visits(0,curr_action)<<std::endl;
        stop("H is NAN");
      }else if(H(0,curr_action) == R_PosInf){
        Rcpp::Rcout <<  "state="<<0<<", action="<<curr_action<<std::endl;
        stop("H is Inf");
      }
      
    }
    
    // UPDATE CREDIT OF STATE 1 ACTIONS NOT SELECTED DURING LAST N EPISODS
    
    IntegerVector setdiff_state1 = Rcpp::setdiff(all_actions,IntegerVector(uniq_state1.begin(),uniq_state1.end()));
    //Rcpp::Rcout <<  "setdiff_state1="<<setdiff_state1 << std::endl;
    
    
    for(unsigned int l=0;l< setdiff_state1.size();l++){
      double  curr_action = setdiff_state1(l);
      H(0,curr_action)= H(0,curr_action)-(alpha*(score_episode-avg_score)*(softmax_cpp3(curr_action,0,H)));
      if(R_IsNaN((H(0,curr_action)))){
        Rcpp::Rcout <<  "state="<<0<<", action="<<curr_action<< std::endl;
        stop("H is NAN");
      }else if(H(0,curr_action) == R_PosInf){
        Rcpp::Rcout <<  "state="<<0<<", action="<<curr_action <<std::endl;
        stop("H is Inf");
      }
    }
    
    // UPDATE CREDIT OF STATE 2 ACTIONS
    
    arma::uvec state2_idx = find(states==1);
    arma::vec uniq_state2 = arma::unique(actions.elem(state2_idx));
    
    // UPDATE CREDIT OF STATE 2 ACTIONS SECLECTED DURING LAST N EPISODS
    
    for(unsigned int l=0;l< uniq_state2.n_elem;l++){
      if(uniq_state2(l)==-1){
        continue;
      }
      double  curr_action = uniq_state2(l);

      H(1,curr_action)= H(1,curr_action)+(alpha*(score_episode-avg_score)*(1-softmax_cpp3(curr_action,1,H)));
      if(R_IsNaN((H(1,curr_action)))){
        Rcpp::Rcout <<  "state="<<1<<", action="<<curr_action<< std::endl;
        //Rcpp::Rcout <<"epsLim=" <<epsLim<< "activity="<< activity<<std::endl;
        stop("H is NAN");
      }else if(H(1,curr_action) == R_PosInf){
        Rcpp::Rcout <<  "state="<<1<<", action="<<curr_action <<std::endl;
        //Rcpp::Rcout <<"epsLim=" <<epsLim<< ", activity="<< activity<<", score_episode="<<score_episode<<std::endl;
        stop("H is Inf");
      }
    }
    
    // UPDATE CREDIT OF STATE 2 NOT  ACTIONS SECLECTED DURING LAST N EPISODS
    
    
    IntegerVector setdiff_state2 = Rcpp::setdiff(all_actions,IntegerVector(uniq_state2.begin(),uniq_state2.end()));

    for(unsigned int l=0;l< setdiff_state2.size();l++){
      double  curr_action = setdiff_state2(l);
      
      H(1,curr_action)= H(1,curr_action)-(alpha*(score_episode-avg_score)*(softmax_cpp3(curr_action,1,H)));
      if(R_IsNaN((H(1,curr_action)))){
        Rcpp::Rcout <<  "state="<<1<<", action="<<curr_action <<std::endl;
        stop("H is NAN");
      }else if(H(1,curr_action) == R_PosInf){
        Rcpp::Rcout <<  "state="<<1<<", action="<<curr_action <<std::endl;
        stop("H is Inf");
      }
    }
    
  } else if(model==3){
    
    // UPDATE CREDIT OF STATE 1 ACTIONS
    IntegerVector all_actions =  seq(0, 5);
    arma::uvec state1_idx = find(states==0);
    arma::vec uniq_state1 = arma::unique(actions.elem(state1_idx));
    
    //Rcpp::Rcout <<  "state1_idx="<< state1_idx<<std::endl;
    
    
    // UPDATE CREDIT OF STATE 1 ACTIONS SECLECTED DURING LAST N EPISODS
    for(unsigned int l=0;l< uniq_state1.n_elem;l++){
      if(uniq_state1(l)==-1){
        continue;
      }
      double  curr_action = uniq_state1(l)*1.00;
      arma::uvec a=arma::find(actions.elem(state1_idx)==curr_action);
      arma::vec time_s1 =  time_taken_for_trial.elem(state1_idx);
      double total_time_spent_in_state1 = arma::accu(time_s1);
      double activity=0;
      if(total_time_spent_in_state1>0){
        activity = a.n_elem*1000/total_time_spent_in_state1;
      }else{
        activity = a.n_elem/(1.0*state1_idx.n_elem);
      }
      
      double delta_H = alpha*(score_episode-avg_score)*(1-activity);
      H(0,curr_action)= H(0,curr_action)+delta_H;
      if(R_IsNaN((H(0,curr_action)))){
        Rcpp::Rcout <<  "state="<<0<<", action="<<curr_action<< std::endl;
        stop("H is NAN");
      }else if(H(0,curr_action) == R_PosInf){
        Rcpp::Rcout <<  "state="<<0<<", action="<<curr_action<<std::endl;
        stop("H is Inf");
      }
      
    }
    
    // UPDATE CREDIT OF STATE 1 ACTIONS NOT SELECTED DURING LAST N EPISODS
    
    IntegerVector setdiff_state1 = Rcpp::setdiff(all_actions,IntegerVector(uniq_state1.begin(),uniq_state1.end()));

    for(unsigned int l=0;l< setdiff_state1.size();l++){
      double  curr_action = setdiff_state1(l);
      arma::uvec a=arma::find(actions.elem(state1_idx)==curr_action);
      arma::vec time_s1 =  time_taken_for_trial.elem(state1_idx);
      double total_time_spent_in_state1 = arma::accu(time_s1);
      double activity=0;
      if(total_time_spent_in_state1>0){
        activity = a.n_elem*1000/total_time_spent_in_state1;
      }else{
        activity = a.n_elem/(1.0*state1_idx.n_elem);
      }
      
      H(0,curr_action)= H(0,curr_action)-(alpha*(score_episode-avg_score)*activity);
      if(R_IsNaN((H(0,curr_action)))){
        Rcpp::Rcout <<  "state="<<0<<", action="<<curr_action<< std::endl;
        stop("H is NAN");
      }else if(H(0,curr_action) == R_PosInf){
        Rcpp::Rcout <<  "state="<<0<<", action="<<curr_action <<std::endl;
        stop("H is Inf");
      }
    }
    
    // UPDATE CREDIT OF STATE 2 ACTIONS
    
    arma::uvec state2_idx = find(states==1);
    arma::vec uniq_state2 = arma::unique(actions.elem(state2_idx));
    
    // UPDATE CREDIT OF STATE 2 ACTIONS SECLECTED DURING LAST N EPISODS
    
    for(unsigned int l=0;l< uniq_state2.n_elem;l++){
      if(uniq_state2(l)==-1){
        continue;
      }
      double  curr_action = uniq_state2(l);
      arma::uvec a=arma::find(actions.elem(state2_idx)==curr_action);
      arma::vec time_s2 =  time_taken_for_trial.elem(state2_idx);
      double total_time_in_state_2 = arma::accu(time_s2);
      double activity=0;
      if(total_time_in_state_2>0){
        activity = a.n_elem*1000/total_time_in_state_2;
      }else{
        activity = a.n_elem/(1.0*state2_idx.n_elem);
      }
      
      H(1,curr_action)= H(1,curr_action)+(alpha*(score_episode-avg_score)*(1-activity));
      if(R_IsNaN((H(1,curr_action)))){
        Rcpp::Rcout <<  "state="<<1<<", action="<<curr_action<< std::endl;
        stop("H is NAN");
      }else if(H(1,curr_action) == R_PosInf){
        Rcpp::Rcout <<  "state="<<1<<", action="<<curr_action <<std::endl;
        stop("H is Inf");
      }
    }
    
    // UPDATE CREDIT OF STATE 2 NOT  ACTIONS SECLECTED DURING LAST N EPISODS
    
    
    IntegerVector setdiff_state2 = Rcpp::setdiff(all_actions,IntegerVector(uniq_state2.begin(),uniq_state2.end()));

    for(unsigned int l=0;l< setdiff_state2.size();l++){
      double  curr_action = setdiff_state2(l);
      arma::uvec a=arma::find(actions.elem(state2_idx)==curr_action);
      arma::vec time_s2 =  time_taken_for_trial.elem(state2_idx);
      double total_time_in_state_2 = arma::accu(time_s2);
      double activity=0;
      if(total_time_in_state_2>0){
        activity = a.n_elem*1000/total_time_in_state_2;
      }else{
        activity = a.n_elem/(1.0*state2_idx.n_elem);
      }
      H(1,curr_action)= H(1,curr_action)-(alpha*(score_episode-avg_score)*activity);
      if(R_IsNaN((H(1,curr_action)))){
        Rcpp::Rcout <<  "state="<<1<<", action="<<curr_action <<std::endl;
        stop("H is NAN");
      }else if(H(1,curr_action) == R_PosInf){
        Rcpp::Rcout <<  "state="<<1<<", action="<<curr_action <<std::endl;
        stop("H is Inf");
      }
    }
    
  }
  return(H);
}

// [[Rcpp::export("aca_gen_sim")]]
arma::mat aca_gen_sim(arma::mat &allpaths,double alpha,int N,int total_trials,int init_state,int model){
  arma::mat H = arma::zeros(2,6);
  arma::mat R = arma::zeros(2,6);
  R(0,3)=1;
  R(1,3)=1;
  arma::mat allpaths_aca_model2 = arma::zeros(total_trials,5);
  Rcpp::CharacterMatrix H_vals(total_trials,2);
  Rcpp::NumericVector episodeIndices;
  
  int episode=1;
  int S=init_state;
  int A=0;
  
  
  
  int initState=0;
  bool changeState = false;
  bool returnToInitState = false;
  int score_episode=0;
  //int episodeFin=0;
  //int prev_ses=-1;
  //int trial=0;
  int i;
  float avg_score = 0;
  bool resetVector = true;
  for ( i = 0; i < (total_trials-1); i++) {
    
    if(resetVector){
      initState=S;
      resetVector= false;
    }
    
    
    A=softmax_action_sel(H,S);
    
    
    if(R(S,A)>0){
      score_episode = score_episode+1;
    }

    allpaths_aca_model2(i,0)=A;
    allpaths_aca_model2(i,1)=S;
    allpaths_aca_model2(i,2)=R(S,A);
    
    
    arma::uvec state_idx = arma::find(allpaths_aca_model2.col(1)==S && allpaths_aca_model2.col(0)==A);
    arma::uvec allpaths_state_idx = arma::find(allpaths.col(1)==(S+1) && allpaths.col(0)==(A+1));

    if(state_idx.n_elem <= allpaths_state_idx.n_elem){
      allpaths_aca_model2(i,3) = allpaths(allpaths_state_idx(state_idx.n_elem-1),3);
    }else{
      allpaths_aca_model2(i,3) = allpaths(allpaths_state_idx(allpaths_state_idx.n_elem-1),3);
    }
    
    allpaths_aca_model2(i,4)=episode;
    //Rcpp::Rcout <<"i="<<i<<", A="<<A<<", S="<<S<<", state_idx.n_elem="<<state_idx.n_elem<<", allpaths_aca_model2(i,3)="<<allpaths_aca_model2(i,3)<<std::endl;
    
    int S_prime=aca_getNextState(S,A);
    
    if(S_prime!=initState){
      changeState = true;
    }else if(S_prime==initState && changeState){
      returnToInitState = true;
    }
    //Rcpp::Rcout <<  "changeState="<< changeState <<  ", returnToInitState="<< returnToInitState<<std::endl;
    
    if(returnToInitState ){
      //Rcpp::Rcout <<  "Inside end episode"<<std::endl;
      changeState = false;
      returnToInitState = false;
      
      
      //arma::vec time_taken_for_trial=arma::zeros(actions.n_elem);
      avg_score = avg_score + (score_episode-avg_score)/episode;
      
      arma::vec actions;
      arma::vec states;
      arma::vec time_taken_for_trial; 
      
      if(episode > N){
        arma::uvec episodeIdx = arma::find(allpaths_aca_model2.col(4) > (episode-N));
        arma::vec allpath_actions = allpaths_aca_model2.col(0);
        actions=allpath_actions.elem(episodeIdx);
        arma::vec allpath_states = allpaths_aca_model2.col(1);
        states=allpath_states.elem(episodeIdx);
        arma::vec allpath_times = allpaths_aca_model2.col(3);
        time_taken_for_trial=allpath_times.elem(episodeIdx);
      }else{
        actions=allpaths_aca_model2.col(0);
        states=allpaths_aca_model2.col(1);
        time_taken_for_trial=allpaths_aca_model2.col(3);
      }
      
      H=updateHMat(H,actions, states, time_taken_for_trial, alpha, score_episode, avg_score, model);
        //Rcpp::Rcout << "H="<< H << std::endl;
        
      score_episode=0;
      episode = episode+1;
      resetVector = true;
    }
    
    S=S_prime; 
  }
  
  //Rcpp::Rcout <<"episodeIndices="<<episodeIndices<<std::endl;
 //return(Rcpp::List::create(Rcpp::Named("allpaths")=allpaths_aca_model2,Rcpp::Named("H_vals")=H_vals));
 return(allpaths_aca_model2);
  
  
}

// [[Rcpp::export("aca_mle_lik")]]
arma::vec aca_mle_lik(arma::mat allpaths,double alpha,int N, arma::mat &H,int model,int sim){

  int nrow = allpaths.n_rows;
  if(sim !=1){
    allpaths.col(4).fill(0);
  }
  //Rcpp::Rcout <<  "allpaths.col(4)="<<allpaths.col(4) <<std::endl;
  
  arma::vec mseMatrix=arma::zeros(nrow);
  
  int episode=1;
  int S=0;
  if(sim==1){
    S=allpaths(0,1);
  }else{
    S=allpaths(0,1)-1;
  }
  int A=0;

  int initState=0;
  bool changeState = false;
  bool returnToInitState = false;
  int score_episode=0;
  int i;
  float avg_score = 0;
  bool resetVector = true;
  for ( i = 0; i < (nrow-1); i++) {
    
    if(resetVector){
      initState=S;
      //Rcpp::Rcout <<"initState="<<initState<<std::endl;
      resetVector= false;
    }
    
    
    int R=allpaths(i,2);
    
    if(R>0){
      score_episode = score_episode+1;
    }         
    
    if(sim==1){
      A=allpaths(i,0);
    }else{
      A=allpaths(i,0)-1;
    }
    
    
    int S_prime=0;
    if(sim==1){
      S_prime=allpaths((i+1),1);
    }else{
      S_prime=allpaths((i+1),1)-1;
    }
    
    if(sim !=1){
      allpaths(i,4)=episode;  
    }
    
    
    if(S_prime!=initState){
      changeState = true;
    }else if(S_prime==initState && changeState){
      returnToInitState = true;
    }
    double prob_a = softmax_cpp3(A,S,H);
    double logProb = log(prob_a);
    mseMatrix(i)=logProb;
    //log_lik=log_lik+ logProb;

    //Check if episode ended
    if(returnToInitState ){
      //Rcpp::Rcout <<  "Inside end episode"<<std::endl;
      changeState = false;
      returnToInitState = false;

      
      avg_score = avg_score + (score_episode-avg_score)/episode;
      
      arma::vec actions;
      arma::vec states;
      arma::vec time_taken_for_trial; 
      
      if(episode > N){
        arma::uvec episodeIdx = arma::find(allpaths.col(4) > (episode-N));
        arma::vec allpath_actions = allpaths.col(0);
        actions=allpaths.elem(episodeIdx);
        arma::vec allpath_states = allpaths.col(1);
        states=allpath_states.elem(episodeIdx);
        arma::vec allpath_times = allpaths.col(3);
        time_taken_for_trial=allpath_times.elem(episodeIdx);
      }else{
        actions=allpaths(arma::span(0,i),0);
        states=allpaths(arma::span(0,i),1);
        time_taken_for_trial=allpaths(arma::span(0,i),3);
      }
      
      if(sim!=1){
        actions=actions-1;
        states=states-1;
      }
      
      // Rcpp::Rcout <<  "actions="<<actions<<std::endl;
      // Rcpp::Rcout <<  "states="<<states<<std::endl;
      // Rcpp::Rcout <<  "time_taken_for_trial="<<time_taken_for_trial<<std::endl;
      // Rcpp::Rcout <<  "H="<<H<<std::endl;
      
      
      
      H=updateHMat(H,actions, states, time_taken_for_trial, alpha, score_episode, avg_score, model);
      // Rcpp::Rcout <<  "H="<<H<<std::endl;
      score_episode=0;
      episode = episode+1;
      resetVector = true;
      
    }
    
    
    S=S_prime; 
    //trial=trial+1;
    
  }
  return(mseMatrix);
}  


// [[Rcpp::export("acaGetProbMatrix")]]
arma::mat acaGetProbMatrix(arma::mat allpaths,double alpha,int N, arma::mat &H,int model,int sim){
  //int sim=1;
  int episode=1;
  if(sim !=1){
    allpaths.col(4).fill(0);
  }
  int S=0;
  if(sim==1){
    S=allpaths(0,1);
  }else{
    S=allpaths(0,1)-1;
  }
  //int A=0;
  
  int initState=0;
  bool changeState = false;
  bool returnToInitState = false;
  int score_episode=0;
  int nrow = allpaths.n_rows;
  int i;
  float avg_score = 0;
  bool resetVector = true;
  
  arma::mat probMatrix_aca=arma::zeros(nrow,12);
  arma::mat mseMatrix=arma::zeros(nrow,4);
  Rcpp::CharacterMatrix H_vals(nrow,2);
  
  for ( i = 0; i < (nrow-1); i++) {
    
    if(resetVector){
      initState=S;
      //Rcpp::Rcout <<"initState="<<initState<<std::endl;
      resetVector= false;
    }

    int R=allpaths(i,2);
    if(R>0){
      score_episode = score_episode+1;
    }         
    
    // if(sim==1){
    //   A=allpaths(i,0);
    // }else{
    //   A=allpaths(i,0)-1;
    // }
    
    int S_prime=0;
    if(sim==1){
      S_prime=allpaths((i+1),1);
    }else{
      S_prime=allpaths((i+1),1)-1;
    }
    
    if(sim !=1){
      allpaths(i,4)=episode;  
    }
    
    if(S==0){
      probMatrix_aca.submat(i,6,i,11)=arma::zeros(1,6);
      //Rcpp::Rcout << "probMatrix_aca="<< probMatrix_aca << std::endl;
      for(int act=0;act<6;act++){
        double x = softmax_cpp3(act,0,H);
        probMatrix_aca(i,act)=x;
      }
    }else if(S==1){
      //Rcpp::Rcout << "i=" <<i<< std::endl;
      probMatrix_aca.submat(i,0,i,5)=arma::zeros(1,6);
      //Rcpp::Rcout << "probMatrix_aca="<< probMatrix_aca << std::endl;
      for(int act=0;act<6;act++){
        double x = softmax_cpp3(act,1,H);
        probMatrix_aca(i,(6+act))=x;
      }
    }
    
    
    if(S_prime!=initState){
      changeState = true;
    }else if(S_prime==initState && changeState){
      returnToInitState = true;
    }

    //Check if episode ended
    if(returnToInitState ){
      //Rcpp::Rcout <<  "Inside end episode"<<std::endl;
      changeState = false;
      returnToInitState = false;
      
      
      avg_score = avg_score + (score_episode-avg_score)/episode;
      
      arma::vec actions;
      arma::vec states;
      arma::vec time_taken_for_trial; 
      
      if(episode > N){
        arma::uvec episodeIdx = arma::find(allpaths.col(4) > (episode-N));
        arma::vec allpath_actions = allpaths.col(0);
        actions=allpaths.elem(episodeIdx);
        arma::vec allpath_states = allpaths.col(1);
        states=allpath_states.elem(episodeIdx);
        arma::vec allpath_times = allpaths.col(3);
        time_taken_for_trial=allpath_times.elem(episodeIdx);
      }else{
        actions=allpaths(arma::span(0,i),0);
        states=allpaths(arma::span(0,i),1);
        time_taken_for_trial=allpaths(arma::span(0,i),3);
      }
      
      if(sim!=1){
        actions=actions-1;
        states=states-1;
      }
      
      H=updateHMat(H,actions, states, time_taken_for_trial, alpha, score_episode, avg_score, model);
      
      score_episode=0;
      episode = episode+1;
      resetVector = true;
    }
    S=S_prime; 
    //trial=trial+1;
  }
  return(probMatrix_aca);
}


// [[Rcpp::export("computeMSE")]]
arma::mat computeMSE(arma::mat allpaths,arma::mat probMatrix_aca,int sim){

  //int start_index = allpaths.n_rows/2;
  //int max_index=allpaths.n_rows-start_index;
  arma::mat mseMatrix=arma::zeros(12,allpaths.n_rows);
  //start_index= start_index+1;
  int trial=0;

  for(unsigned int i=0;i<(allpaths.n_rows);i++){
    int action=allpaths(i,0);
    if(sim==1){
      if(allpaths(i,1)==0){

        mseMatrix(0,trial)=pow(probMatrix_aca(i,0)-(0==action),2);
        mseMatrix(1,trial)=pow(probMatrix_aca(i,1)-(1==action),2);
        mseMatrix(2,trial)=pow(probMatrix_aca(i,2)-(2==action),2);
        mseMatrix(3,trial)=pow(probMatrix_aca(i,3)-(3==action),2);
        mseMatrix(4,trial)=pow(probMatrix_aca(i,4)-(4==action),2);
        mseMatrix(5,trial)=pow(probMatrix_aca(i,5)-(5==action),2);

      }else if(allpaths(i,1)==1){
        mseMatrix(6,trial)=pow(probMatrix_aca(i,6)-(0==action),2);
        mseMatrix(7,trial)=pow(probMatrix_aca(i,7)-(1==action),2);
        mseMatrix(8,trial)=pow(probMatrix_aca(i,8)-(2==action),2);
        mseMatrix(9,trial)=pow(probMatrix_aca(i,9)-(3==action),2);
        mseMatrix(10,trial)=pow(probMatrix_aca(i,10)-(4==action),2);
        mseMatrix(11,trial)=pow(probMatrix_aca(i,11)-(5==action),2);
        //Rcpp::Rcout << "S=2, mseMatrix(3,trial)="<<  mseMatrix(3,trial)<< std::endl;
      }
      
    }else{
      if(allpaths(i,1)==1){

        mseMatrix(0,trial)=pow(probMatrix_aca(i,0)-(1==action),2);
        mseMatrix(1,trial)=pow(probMatrix_aca(i,1)-(2==action),2);
        mseMatrix(2,trial)=pow(probMatrix_aca(i,2)-(3==action),2);
        mseMatrix(3,trial)=pow(probMatrix_aca(i,3)-(4==action),2);
        mseMatrix(4,trial)=pow(probMatrix_aca(i,4)-(5==action),2);
        mseMatrix(5,trial)=pow(probMatrix_aca(i,5)-(6==action),2);

      }else if(allpaths(i,1)==2){
        mseMatrix(6,trial)=pow(probMatrix_aca(i,6)-(1==action),2);
        mseMatrix(7,trial)=pow(probMatrix_aca(i,7)-(2==action),2);
        mseMatrix(8,trial)=pow(probMatrix_aca(i,8)-(3==action),2);
        mseMatrix(9,trial)=pow(probMatrix_aca(i,9)-(4==action),2);
        mseMatrix(10,trial)=pow(probMatrix_aca(i,10)-(5==action),2);
        mseMatrix(11,trial)=pow(probMatrix_aca(i,11)-(6==action),2);
      }
    }
    
    trial=trial+1;
  }
  return(mseMatrix);
}


// [[Rcpp::export("computeMSE2")]]
arma::vec computeMSE2(arma::mat allpaths,arma::mat probMatrix_aca,int sim){
  
  arma::mat mseMatrix=arma::zeros(allpaths.n_rows);
  int trial=0;

  for(unsigned int i=0;i<(allpaths.n_rows);i++){
    int action=allpaths(i,0);
    int state=allpaths(i,1);

    if(sim != 1){
      state=state-1;
      action=action-1;
    }
    
    mseMatrix(i)=pow(1-probMatrix_aca(i,((6*state)+action)),2);
    trial=trial+1;
  }
  return(mseMatrix);
}


// [[Rcpp::export("updateAllpaths1")]]
arma::vec updateAllpaths1(NumericVector allpaths,Rcpp::NumericMatrix enreg_pos){
  
  int nrow = allpaths.size();
  //Rcpp::Rcout <<"nrow="<<nrow<<std::endl;
  arma::colvec y(nrow);
  //arma::mat enreg_arma(enreg_pos.begin(), enreg_pos.nrow(), enreg_pos.ncol(), false);
  arma::mat enreg_arma = Rcpp::as<arma::mat>(enreg_pos);
  arma::vec allpaths_arma = Rcpp::as<arma::vec>(allpaths);
  //Rcpp::Rcout <<enreg_arma<<std::endl;
  int prev_ses=-1;
  int trial = 0;
  for(int i=0;i<nrow;i++){
    
    //Rcpp::Rcout <<"trial="<<trial<<", allpaths_arma(i)="<<allpaths_arma(i)<<std::endl;
    if(prev_ses!=allpaths_arma(i)){
      trial=1;
      prev_ses=allpaths_arma(i);
    }
    //Rcpp::Rcout <<"i="<<i<<std::endl;
    arma::uvec ids = arma::find((enreg_arma.col(1)==trial) && (enreg_arma.col(2)==allpaths_arma(i)));
    int start_pos_id = ids(0);
    //Rcpp::Rcout <<"ids.size="<<ids.n_elem<<std::endl;
    
    int max_pos_id = ids(ids.n_elem-1);
    //Rcpp::Rcout <<"start_pos_id="<<start_pos_id<<", max_pos_id="<<max_pos_id<<std::endl;
    double time_taken_for_trial=(enreg_arma((max_pos_id-1),0))-enreg_arma(start_pos_id,0);
    if(time_taken_for_trial==0){
      time_taken_for_trial=20;
    }
    y(i)=time_taken_for_trial;
    //Rcpp::Rcout <<"trial="<<trial<<", ses="<<allpaths_arma(i)<<", time_taken_for_trial=" <<time_taken_for_trial<<std::endl;
    trial= trial+1;
  }
  //=arma::join_horiz(allpaths_arma,y);
  return(y);
}