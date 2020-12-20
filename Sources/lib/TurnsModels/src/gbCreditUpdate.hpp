#ifndef __GBCREDITUPDATE__
#define __GBCREDITUPDATE__

#include <vector>
#include <RcppArmadillo.h>

#include <regex>
#include <RcppArmadilloExtensions/sample.h>
#include "utils.hpp"

void GbCreditUpdate(std::vector<std::shared_ptr<TreeNode>> episodeTurns, std::vector<int> episodeTurnStates, double alpha, float score_episode, double avg_score)
{
 
  
  arma::vec episodeTurnStates_arma = arma::conv_to<arma::vec>::from(episodeTurnStates);
  Rcpp::Rcout <<  "score_episode=" << score_episode << ", avg_score=" <<avg_score <<std::endl;

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
      //Rcpp::Rcout << "curr_turn=" <<*curr_turn << " in state=" << state<<std::endl;
      //Rcpp::IntegerVector turnIdx; // to store the indices of all instances of curr_turn 
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
            //turnIdx.push_back(turnIndex);
            currNode = *node;
   
          }
          
        }
        turnIndex++;
      }

      if (!currNode)
      {
        Rcpp::Rcout <<"state=" <<state <<  ", turn="<< *curr_turn  << " not found in episodeTurns" <<std::endl;
      } 
      
      currNode->credit = currNode->credit + (alpha * (score_episode-avg_score) * (1 - softmax(currNode)));
      if (!currNode->siblings.empty())
      {
        for (auto sibling = currNode->siblings.begin(); sibling != currNode->siblings.end(); sibling++)
        {
          (*sibling)->credit = (*sibling)->credit + (alpha * (score_episode-avg_score) * softmax((*sibling)));
        }
      }
      //Rcpp::Rcout <<  "Turn="<< *curr_turn  << ", turnTime=" << turnTime <<std::endl;
    }


          
  } 
}
#endif