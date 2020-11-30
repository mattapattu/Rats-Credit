#ifndef __ACACREDITUPDATE__
#define __ACACREDITUPDATE__

#include <vector>
#include <RcppArmadillo.h>

#include <regex>
#include <RcppArmadilloExtensions/sample.h>
#include "utils.hpp"

//updateHMat(H,actions, states, trialTimes, alpha,N, score_episode, avg_score, model);
inline void Aca3CreditUpdate(std::vector<std::shared_ptr<TreeNode>> episodeTurns, std::vector<int> episodeTurnStates, std::vector<double> episodeTurnTimes, double alpha, float score_episode)
{
 
  
  arma::vec episodeTurnStates_arma = arma::conv_to<arma::vec>::from(episodeTurnStates);
  //Rcpp::Rcout <<  "episodeTurnStates=" << episodeTurnStates_arma <<std::endl;

  arma::vec episodeTurnTimes_arma(episodeTurnTimes);
  for (int state = 0; state < 2; state++)
  {
    //get turns in state 0/1
    std::set<std::string> turns;
    //std::vector<double> turnTimes;
    std::vector<std::shared_ptr<TreeNode>> episodeTurns_state;

    //identify unique turns corresponding to each state
    for (unsigned int index = 0; index < episodeTurnStates.size(); ++index)
    {
        if (episodeTurnStates[index] == state)
      {
        turns.insert(episodeTurns[index]->turn);
        //turnTimes.push_back(episodeTurnTimes[index]);
      }
    }
   
    //Rcpp::Rcout <<  "state= " <<state <<std::endl;
      
    //turns - contains all unique turns in an episode corresponding to one state
    //Next: Loop through turns
    //Next: get the pointer of curr_turn from episodeTurns
    //Next: update credit of curr_turn in the tree
    //for each unique turn - get all instances of that in current episode

    for(auto curr_turn = std::begin(turns); curr_turn != std::end(turns); ++curr_turn) {
      Rcpp::Rcout << "curr_turn=" <<*curr_turn << " in state=" << state<<std::endl;
      arma::uvec turnIdx; // to store the indices of all instances of curr_turn 
      unsigned int turnIndex = 0;
      std::shared_ptr<TreeNode> currNode;
      
      for(auto node = std::begin(episodeTurns); node != std::end(episodeTurns); ++node)
      {
        //Rcpp::Rcout <<"state =" <<episodeTurnStates[turnIndex] <<  ", node->turn="<< (*node)->turn  <<std::endl;
        if (episodeTurnStates[turnIndex] == state)
        {
          
          if ((*node)->turn == *curr_turn)
          {
            //Rcpp::Rcout <<  "Turn="<< *curr_turn  << " is found in episodeTurns" <<std::endl;
            turnIdx.insert_rows(turnIdx.n_rows, turnIndex);
            currNode = *node;
   
          }
          
        }
        turnIndex++;
      }

      if (!currNode)
      {
        //Rcpp::Rcout <<"state=" <<state <<  ", turn="<< *curr_turn  << " not found in episodeTurns" <<std::endl;
      } 
      double turnTime = arma::accu(episodeTurnTimes_arma.elem(turnIdx));
      double activity = turnTime/arma::accu(episodeTurnTimes_arma);
      currNode->credit = currNode->credit + (alpha * score_episode * activity) ;
      Rcpp::Rcout <<  "Turn="<< *curr_turn  << ", credit=" << currNode->credit <<std::endl;
    }


          
  } 
}
#endif