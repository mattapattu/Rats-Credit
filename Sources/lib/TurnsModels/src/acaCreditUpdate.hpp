#ifndef __ACACREDITUPDATE__
#define __ACACREDITUPDATE__

#include <vector>
#include <RcppArmadillo.h>

#include <regex>
#include <RcppArmadilloExtensions/sample.h>
#include "utils.hpp"

//updateHMat(H,actions, states, trialTimes, alpha,N, score_episode, avg_score, model);
inline void AcaCreditUpdate(std::vector<std::shared_ptr<TreeNode>> episodeTurns, std::vector<int> episodeTurnStates, std::vector<double> episodeTurnTimes, double alpha, float score_episode)
{
  arma::vec episodeTurnTimes_arma(episodeTurnTimes);
  for (int state = 0; state < 1; state++)
  {
    //get turns in state 0/1
    std::vector<std::string> turns;
    std::vector<double> turnTimes;

    //identify unique turns
    for (unsigned int index = 0; index < episodeTurnStates.size(); ++index)
    {
        if (episodeTurnStates[index] == state)
      {
        turns.push_back(episodeTurns[index]->turn);
        turnTimes.push_back(episodeTurnTimes[index]);
      }
    }

    arma::vec turnTimes_arma(turnTimes);

    std::sort(turns.begin(), turns.end());
    turns.erase(std::unique(turns.begin(), turns.end()), turns.end());

    //for each unique turn - get all instances of that in current episode
    for (const auto &curr_turn : turns)
    {
      arma::uvec idx; // to store the indices of all instances of curr_turn
      arma::uword index = 0;
      std::shared_ptr<TreeNode> currNode;
      
      for (const auto node : episodeTurns)
      {
        if (episodeTurnStates[index] == state)
        {
          if (node->turn == curr_turn)
          {
            idx.insert_rows(idx.n_rows, index);
            currNode = node;
          }
          index++;
        }
    
      }
      double turnTime = arma::accu(turnTimes_arma.elem(idx));
      double activity = turnTime/arma::accu(episodeTurnTimes_arma);
      currNode->credit = currNode->credit + (alpha * score_episode * activity) ;
    }
      
  }
}
#endif