#ifndef __GBCREDITUPDATE__
#define __GBCREDITUPDATE__

#include <vector>
#include <RcppArmadillo.h>

#include <regex>
#include <RcppArmadilloExtensions/sample.h>
#include "utils.hpp"

void GbCreditUpdate(std::vector<std::shared_ptr<TreeNode>> episodeTurns, std::vector<int> episodeTurnStates, double alpha, float score_episode, double avg_score)
{

  for (int state = 0; state < 1; state++)
  {
    //get turns in state 0/1
    std::vector<std::string> turns;

    //identify unique turns
    for (unsigned int index = 0; index < episodeTurnStates.size(); ++index)
    {
        if (episodeTurnStates[index] == state)
      {
        turns.push_back(episodeTurns[index]->turn);
      }
    }

    std::sort(turns.begin(), turns.end());
    turns.erase(std::unique(turns.begin(), turns.end()), turns.end());

    //get the idx of each unique turn in episodeTurnTimes_state
    //update turn credits
    for (const auto &curr_turn : turns)
    {
      arma::uvec idx; //ids of elements same as curr_turn in episodeTurns
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
      currNode->credit = currNode->credit + (alpha * score_episode - avg_score * (1 - softmax(currNode)));
      if (!currNode->siblings.empty())
      {
        for (auto i = currNode->siblings.begin(); i != currNode->siblings.end(); i++)
        {
          (*i)->credit = (*i)->credit + (alpha * (score_episode-avg_score) * softmax((*i)));
        }
      }
    }
  }
}


#endif