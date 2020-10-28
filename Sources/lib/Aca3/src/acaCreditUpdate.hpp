#ifndef __ACACREDITUPDATE__
#define __ACACREDITUPDATE__

#include <vector>
#include <RcppArmadillo.h>

#include <regex>
#include <RcppArmadilloExtensions/sample.h>


//    H = Aca3CreditUpdate(H, actions, states, time_taken_for_trial,  alpha, score_episode);

inline arma::mat Aca3CreditUpdate(arma::mat H, arma::vec actions, arma::vec states, arma::vec trialTimes,  double alpha,float score_episode){
  
  arma::uvec state1_idx = arma::find(states==0);
  arma::vec uniq_action1 = arma::unique(actions.elem(state1_idx));
  
  for(unsigned int l=0;l< uniq_action1.n_elem;l++){
    
    double  curr_action = uniq_action1(l);
    arma::vec last_ep_time_s1 = trialTimes.elem(state1_idx);
    arma::vec last_ep_actions_s1 = actions.elem(state1_idx);
    arma::uvec curr_act_idx = arma::find(last_ep_actions_s1==curr_action);
    double activity = arma::accu(last_ep_time_s1.elem(curr_act_idx))/arma::accu(trialTimes);
    
    H(0,curr_action)= (H(0,curr_action)+(alpha*(score_episode)*(activity)));
    if(R_IsNaN((H(0,curr_action)))){
      Rcpp::Rcout <<  "state="<<0<<", action="<<curr_action<< std::endl;
    }else if(H(0,curr_action) == R_PosInf){
      Rcpp::Rcout <<  "state="<<0<<", action="<<curr_action<<std::endl;
    }else if(H(0,curr_action) < 0){
      
    }
    
  }
  
  arma::uvec state2_idx = find(states==1);
  arma::vec uniq_action2 = arma::unique(actions.elem(state2_idx));
  
  for(unsigned int l=0;l< uniq_action2.n_elem;l++){
    if(uniq_action2(l)==-1){
      continue;
    }
    double  curr_action = uniq_action2(l);
    arma::vec last_ep_time_s2 = trialTimes.elem(state2_idx);
    arma::vec last_ep_actions_s2 = actions.elem(state2_idx);
    arma::uvec curr_act_idx= arma::find(last_ep_actions_s2==curr_action);
    double activity= arma::accu(last_ep_time_s2.elem(curr_act_idx))/arma::accu(trialTimes);
    
    H(1,curr_action)= (H(1,curr_action)+(alpha*score_episode*(activity)));
    
    if(R_IsNaN((H(1,curr_action)))){
      Rcpp::Rcout <<  "state="<<1<<", action="<<curr_action<< std::endl;
    }else if(H(1,curr_action) == R_PosInf){
      Rcpp::Rcout <<  "state="<<1<<", action="<<curr_action<<std::endl;
    }else if(H(1,curr_action) < 0){
    }
  }
  
  
  return(H);
}

#endif