#ifndef __ACACREDITUPDATE__
#define __ACACREDITUPDATE__

#include <vector>
#include <RcppArmadillo.h>

#include <regex>
#include <RcppArmadilloExtensions/sample.h>
#include "utils.hpp"

//updateHMat(H,actions, states, trialTimes, alpha,N, score_episode, avg_score, model);
inline arma::mat AcaCreditUpdate(arma::mat H, arma::vec actions, arma::vec states, arma::vec trialTimes, double alpha, float score_episode)
{

  arma::uvec state1_idx = arma::find(states == 0);
  arma::vec uniq_action1 = arma::unique(actions.elem(state1_idx));

  //Rcpp::Rcout <<  "actions = " << actions <<", states=" << states << ", trialTimes=" << trialTimes  <<std::endl;

  for (unsigned int l = 0; l < uniq_action1.n_elem; l++)
  {
    if(uniq_action1(l) != 6 )
    {
      double curr_action = uniq_action1(l);
      arma::vec last_ep_time_s1 = trialTimes.elem(state1_idx);
      arma::vec last_ep_actions_s1 = actions.elem(state1_idx);
      arma::uvec curr_act_idx = arma::find(last_ep_actions_s1 == curr_action);
      double activity = 0;
      if (curr_action != 6)
      {
        activity = arma::accu(last_ep_time_s1.elem(curr_act_idx)) / arma::accu(trialTimes);
      }

      //Rcpp::Rcout <<"state=0" << ", curr_action=" <<curr_action <<", activity="<<activity<<", score_episode="<<score_episode<< std::endl;

      H(0, curr_action) = (H(0, curr_action) + (alpha * score_episode * (activity)));
      if (R_IsNaN((H(0, curr_action))))
      {
        Rcpp::Rcout << "state=" << 0 << ", action=" << curr_action << std::endl;
      }
      else if (H(0, curr_action) == R_PosInf)
      {
        Rcpp::Rcout << "state=" << 0 << ", action=" << curr_action << std::endl;
      }
      else if (H(0, curr_action) < 0)
      {
      }
    }
    
    //double partialCredit = score_episode * activity;
    //Rcpp::Rcout << "State=1" << ", Path="<< curr_action  << ", credit received=" << partialCredit <<std::endl;
  }

  arma::uvec state2_idx = find(states == 1);
  arma::vec uniq_action2 = arma::unique(actions.elem(state2_idx));
  //Rcpp::Rcout <<"uniq_action2=" << uniq_action2 <<", trialTimes=" <<trialTimes.elem(state2_idx) <<", activity="<<activity<<", score_episode="<<score_episode<< std::endl;

  for (unsigned int l = 0; l < uniq_action2.n_elem; l++)
  {
    
    //Rcpp::Rcout <<"l=" << l << ", uniq_action2.n_elem=" <<uniq_action2.n_elem<< std::endl;
    //Rcpp::Rcout <<"state=1" << ", curr_action=" <<uniq_action2(l)<< std::endl;

    if(uniq_action2(l) != 6 )
    {
      double curr_action = uniq_action2(l);
      arma::vec last_ep_time_s2 = trialTimes.elem(state2_idx);
      arma::vec last_ep_actions_s2 = actions.elem(state2_idx);
      arma::uvec curr_act_idx = arma::find(last_ep_actions_s2 == curr_action);

      double activity = 0;
      if (curr_action != 6)
      {
        activity = arma::accu(last_ep_time_s2.elem(curr_act_idx)) / arma::accu(trialTimes);
      }

      //Rcpp::Rcout <<"state=1" << ", curr_action=" <<curr_action <<", activity="<<activity<<", score_episode="<<score_episode<< std::endl;
      H(1, curr_action) = (H(1, curr_action) + (alpha * score_episode * (activity)));

      if (R_IsNaN((H(1, curr_action))))
      {
        Rcpp::Rcout << "state=" << 1 << ", action=" << curr_action << std::endl;
      }
      else if (H(1, curr_action) == R_PosInf)
      {
        Rcpp::Rcout << "state=" << 1 << ", action=" << curr_action << std::endl;
      }
      else if (H(1, curr_action) < 0)
      {
      }
    }
    
    //double partialCredit = score_episode * activity;
    //Rcpp::Rcout << "State=2" << ", Path="<< curr_action  << ", credit received=" << partialCredit <<std::endl;

  }
  //Rcpp::Rcout << "Returning H" << std::endl;
  return (H);
}

#endif