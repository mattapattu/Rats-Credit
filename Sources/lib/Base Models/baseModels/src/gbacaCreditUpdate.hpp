#ifndef __GBACACREDITUPDATE__
#define __GBACACREDITUPDATE__

#include <vector>
#include <RcppArmadillo.h>

#include <regex>
#include <RcppArmadilloExtensions/sample.h>
#include "utils.hpp"


inline arma::mat GbAcaCreditUpdate(arma::mat H,arma::vec actions, arma::vec states,  double alpha, float score_episode,double avg_score, arma::mat activityMatrix){
  
    Rcpp::IntegerVector all_actions =  Rcpp::seq(0, 5);
    // int last_episode = episodes(episodes.n_elem-1);
    // arma::uvec epsIdx = find(episodes==last_episode);
    // arma::vec last_ep_actions = actions.elem(epsIdx);
    // arma::vec last_ep_states = states.elem(epsIdx);
    arma::uvec state1_idx = find(states==0);
    arma::vec uniq_action1 = arma::unique(actions.elem(state1_idx));
    
    //Rcpp::Rcout <<  "state1_idx="<< state1_idx<<std::endl;
    
    
    // UPDATE CREDIT OF STATE 1 ACTIONS SECLECTED DURING LAST N EPISODS
    for(unsigned int l=0;l< uniq_action1.n_elem;l++){
      if(uniq_action1(l)==-1){
        continue;
      }
      double  curr_action = uniq_action1(l);
      // // arma::uvec state1_all_idx = find(states==0);
      double activity = activityMatrix(0,curr_action);
      
      double delta_H = alpha*(score_episode-avg_score)*(1-activity);
      H(0,curr_action)= H(0,curr_action)+delta_H;
      if(R_IsNaN((H(0,curr_action)))){
        Rcpp::Rcout <<  "state="<<0<<", action="<<curr_action<< std::endl;
      }else if(H(0,curr_action) == R_PosInf){
        Rcpp::Rcout <<  "state="<<0<<", action="<<curr_action<<std::endl;
      }
      
    }
    
    //## UPDATE CREDIT OF STATE 1 ACTIONS NOT SELECTED DURING LAST N EPISODES #########################
    
    Rcpp::IntegerVector setdiff_state1 = Rcpp::setdiff(all_actions,Rcpp::IntegerVector(uniq_action1.begin(),uniq_action1.end()));
    
    for(unsigned int l=0;l< setdiff_state1.size();l++){
      double  curr_action = setdiff_state1(l);
      double activity = activityMatrix(0,curr_action);
      
      H(0,curr_action)= H(0,curr_action)-(alpha*(score_episode-avg_score)*(activity));
      if(R_IsNaN((H(0,curr_action)))){
        Rcpp::Rcout <<  "state="<<0<<", action="<<curr_action<< std::endl;
      }else if(H(0,curr_action) == R_PosInf){
        Rcpp::Rcout <<  "state="<<0<<", action="<<curr_action <<std::endl;
      }
    }
    
    // ######### UPDATE CREDIT OF STATE 2 ACTIONS ##############################################################################
    
    arma::uvec state2_idx = find(states==1);
    arma::vec uniq_action2 = arma::unique(actions.elem(state2_idx));
    
    // UPDATE CREDIT OF STATE 2 ACTIONS SECLECTED DURING LAST N EPISODS
    
    for(unsigned int l=0;l< uniq_action2.n_elem;l++){
      if(uniq_action2(l)==-1){
        continue;
      }
      double  curr_action = uniq_action2(l);

      double activity = activityMatrix(1,curr_action);
      
      H(1,curr_action)= H(1,curr_action)+(alpha*(score_episode-avg_score)*(1-activity));
      if(R_IsNaN((H(1,curr_action)))){
        Rcpp::Rcout <<  "state="<<1<<", action="<<curr_action<< std::endl;
      }else if(H(1,curr_action) == R_PosInf){
        Rcpp::Rcout <<  "state="<<1<<", action="<<curr_action <<std::endl;
      }
    }
    
    // UPDATE CREDIT OF STATE 2 NOT  ACTIONS SECLECTED DURING LAST N EPISODS
    
    
    Rcpp::IntegerVector setdiff_state2 = Rcpp::setdiff(all_actions,Rcpp::IntegerVector(uniq_action2.begin(),uniq_action2.end()));
    
    for(unsigned int l=0;l< setdiff_state2.size();l++){
      double  curr_action = setdiff_state2(l);
      double activity = activityMatrix(1,curr_action);
      
      H(1,curr_action)= H(1,curr_action)-(alpha*(score_episode-avg_score)*(activity));
      if(R_IsNaN((H(1,curr_action)))){
        Rcpp::Rcout <<  "state="<<1<<", action="<<curr_action <<std::endl;
      }else if(H(1,curr_action) == R_PosInf){
        Rcpp::Rcout <<  "state="<<1<<", action="<<curr_action <<std::endl;
      }
    }
    
 
  return(H);
}

#endif