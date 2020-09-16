#ifndef __GBCREDITUPDATE__
#define __GBCREDITUPDATE__

#include <vector>
#include <RcppArmadillo.h>

#include <regex>
#include <RcppArmadilloExtensions/sample.h>
#include "utils.hpp"


arma::mat GbCreditUpdate(arma::mat &H,arma::vec actions, arma::vec states, double alpha, float score_episode,double avg_score){
  
    // UPDATE CREDIT OF STATE 1 ACTIONS
    Rcpp::IntegerVector all_actions =  Rcpp::seq(0, 5);
    arma::uvec state1_idx = find(states==0);
    arma::vec uniq_action1 = arma::unique(actions.elem(state1_idx));
    
    // UPDATE CREDIT OF STATE 1 ACTIONS SECLECTED DURING LAST N EPISODS
    for(unsigned int l=0;l< uniq_action1.n_elem;l++){
      if(uniq_action1(l)==-1){
        continue;
      }
      double  curr_action = uniq_action1(l);
      double delta_H = alpha*(score_episode-avg_score)*(1-softmax_cpp3(curr_action,0,H));
      H(0,curr_action)= H(0,curr_action)+delta_H;
      if(R_IsNaN((H(0,curr_action)))){
        Rcpp::Rcout <<  "state="<<0<<", action="<<curr_action<< std::endl;
        //Rcpp::Rcout <<  "H="<< H(0,curr_action)<<std::endl;
        //Rcpp::Rcout <<  "Visits="<< Visits(0,curr_action)<<std::endl;
      }else if(H(0,curr_action) == R_PosInf){
        Rcpp::Rcout <<  "state="<<0<<", action="<<curr_action<<std::endl;
      }
      
    }
    
    // UPDATE CREDIT OF STATE 1 ACTIONS NOT SELECTED DURING LAST N EPISODS
    
    Rcpp::IntegerVector setdiff_state1 = Rcpp::setdiff(all_actions,Rcpp::IntegerVector(uniq_action1.begin(),uniq_action1.end()));
    //Rcpp::Rcout <<  "setdiff_state1="<<setdiff_state1 << std::endl;
    
    
    for(unsigned int l=0;l< setdiff_state1.size();l++){
      double  curr_action = setdiff_state1(l);
      H(0,curr_action)= H(0,curr_action)-(alpha*(score_episode-avg_score)*(softmax_cpp3(curr_action,0,H)));
      if(R_IsNaN((H(0,curr_action)))){
        Rcpp::Rcout <<  "state="<<0<<", action="<<curr_action<< std::endl;
      }else if(H(0,curr_action) == R_PosInf){
        Rcpp::Rcout <<  "state="<<0<<", action="<<curr_action <<std::endl;
      }
    }
    
    // UPDATE CREDIT OF STATE 2 ACTIONS
    
    arma::uvec state2_idx = find(states==1);
    arma::vec uniq_action2 = arma::unique(actions.elem(state2_idx));
    
    // UPDATE CREDIT OF STATE 2 ACTIONS SECLECTED DURING LAST N EPISODS
    
    for(unsigned int l=0;l< uniq_action2.n_elem;l++){
      if(uniq_action2(l)==-1){
        continue;
      }
      double  curr_action = uniq_action2(l);
      
      H(1,curr_action)= H(1,curr_action)+(alpha*(score_episode-avg_score)*(1-softmax_cpp3(curr_action,1,H)));
      if(R_IsNaN((H(1,curr_action)))){
        Rcpp::Rcout <<  "state="<<1<<", action="<<curr_action<< std::endl;
        //Rcpp::Rcout <<"epsLim=" <<epsLim<< "activity="<< activity<<std::endl;
      }else if(H(1,curr_action) == R_PosInf){
        Rcpp::Rcout <<  "state="<<1<<", action="<<curr_action <<std::endl;
        //Rcpp::Rcout <<"epsLim=" <<epsLim<< ", activity="<< activity<<", score_episode="<<score_episode<<std::endl;
      }
    }
    
    // UPDATE CREDIT OF STATE 2 NOT  ACTIONS SECLECTED DURING LAST N EPISODS
    
    
    Rcpp::IntegerVector setdiff_state2 = Rcpp::setdiff(all_actions,Rcpp::IntegerVector(uniq_action2.begin(),uniq_action2.end()));
    
    for(unsigned int l=0;l< setdiff_state2.size();l++){
      double  curr_action = setdiff_state2(l);
      
      H(1,curr_action)= H(1,curr_action)-(alpha*(score_episode-avg_score)*(softmax_cpp3(curr_action,1,H)));
      if(R_IsNaN((H(1,curr_action)))){
        Rcpp::Rcout <<  "state="<<1<<", action="<<curr_action <<std::endl;
      }else if(H(1,curr_action) == R_PosInf){
        Rcpp::Rcout <<  "state="<<1<<", action="<<curr_action <<std::endl;
      }
    }
    
  return(H);
}

#endif