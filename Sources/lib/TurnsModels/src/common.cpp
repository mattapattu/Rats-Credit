// [[Rcpp::depends(RcppArmadillo)]]
#include <vector>
#include <RcppArmadillo.h>

#include <regex>
#include <RcppArmadilloExtensions/sample.h>

#include "acaCreditUpdate.hpp"
#include "gbCreditUpdate.hpp"
#include "utils.hpp"
#include "tree.hpp"

using namespace Rcpp;

int aca_getNextState(int curr_state, int action)
{
  int new_state = -1;
  if (action == 4)
  {
    new_state = curr_state;
  }
  else if (curr_state == 0)
  {
    new_state = 1;
  }
  else if (curr_state == 1)
  {
    new_state = 0;
  }

  return (new_state);
}

int softmax_action_sel(arma::vec H)
{

  // Rcpp::Rcout <<"S="<<S<<", H="<<H<<std::endl;
  double m = arma::max(H);

  //Rcpp::Rcout <<"m="<<m<<", v="<<v<<std::endl;

  H = exp(H - m);
  double exp_sum = arma::accu(H);
  H = H / exp_sum;
  IntegerVector actions = seq(0, 12);
  int action_selected = Rcpp::RcppArmadillo::sample(actions, 1, true, H)[0];
  //Rcpp::Rcout <<"action_selected="<<action_selected<<std::endl;

  return (action_selected);
}

void updateCreditMatrix(std::vector<std::shared_ptr<TreeNode>> episodeTurns, std::vector<int> episodeTurnStates, std::vector<double> episodeTurnTimes, double alpha, float score_episode, double avg_score, int model)
{
  if (model == 1)
  {
    AcaCreditUpdate(episodeTurns, episodeTurnStates, episodeTurnTimes, alpha, score_episode);
  }
  else if (model == 2)
  {
    GbCreditUpdate(episodeTurns, episodeTurnStates, alpha, score_episode, avg_score);
  }
  return;
}

// [[Rcpp::export()]]
std::vector<double> getTurnsLikelihood(arma::mat allpaths, arma::mat turnTimes, int turnMethod, double alpha, double rewardVal, int sim, int model)
{

  if (sim != 1)
  {
    arma::mat v = arma::zeros(allpaths.n_rows, 1);
    allpaths = arma::join_horiz(allpaths, v);
  }
  //Rcpp::Rcout <<  "allpaths.col(4)="<<allpaths.col(4) <<std::endl;

  std::vector<double> mseMatrix;
  //int mseRowIdx = 0;

  arma::vec allpath_actions = allpaths.col(0);
  arma::vec allpath_states = allpaths.col(1);
  arma::vec allpath_rewards = allpaths.col(2);
  arma::vec sessionVec = allpaths.col(4);
  arma::vec uniqSessIdx = arma::unique(sessionVec);

  arma::vec turnTime_method;
  if (turnMethod == 0)
  {
    turnTime_method = turnTimes.col(4);
  }
  else if (turnMethod == 1)
  {
    turnTime_method = turnTimes.col(5);
  }
  else if (turnMethod == 2)
  {
    turnTime_method = turnTimes.col(6);
  }

  int episode = 1;

  std::shared_ptr<TreeNode> rootS1 = initS1();
  std::shared_ptr<TreeNode> rootS2 = initS2();

  //Rcpp::Rcout <<"rootS1.turn ="<<rootS1->turn<<std::endl;
  //Rcpp::Rcout <<"rootS2.turn ="<<rootS2->turn<<std::endl;

  for (unsigned int session = 0; session < (uniqSessIdx.n_elem); session++)
  {

    int sessId = uniqSessIdx(session);
    //Rcpp::Rcout <<"sessId="<<sessId<<std::endl;
    arma::uvec sessionIdx = arma::find(sessionVec == sessId);
    arma::vec actions_sess = allpath_actions.elem(sessionIdx);
    arma::vec states_sess = allpath_states.elem(sessionIdx);
    arma::vec rewards_sess = allpath_rewards.elem(sessionIdx);

    arma::uvec turnTimes_idx = arma::find(turnTimes.col(4) == sessId);
    arma::vec turn_times_session = turnTime_method.elem(turnTimes_idx);
    arma::uword session_turn_count = 0;

    int initState = 0;
    bool changeState = false;
    bool returnToInitState = false;
    int score_episode = 0;
    float avg_score = 0;
    bool resetVector = true;
    int nrow = actions_sess.n_rows;
    int S = 0;
    int A = 0;
    std::vector<std::shared_ptr<TreeNode>> episodeTurns;
    std::vector<int> episodeTurnStates;
    std::vector<double> episodeTurnTimes;

    for (int i = 0; i < (nrow - 1); i++)
    {

      if (resetVector)
      {
        initState = S;
        //Rcpp::Rcout <<"initState="<<initState<<std::endl;
        resetVector = false;
      }

      int R = rewards_sess(i);

      if (R > 0)
      {
        score_episode = score_episode + rewardVal;
      }

      if (sim == 1)
      {
        A = actions_sess(i);
      }
      else
      {
        A = actions_sess(i) - 1;
      }

      int S_prime = 0;
      if (sim == 1)
      {
        S_prime = states_sess(i + 1);
      }
      else
      {
        S_prime = states_sess(i + 1) - 1;
      }

      if (S_prime != initState)
      {
        changeState = true;
      }
      else if (S_prime == initState && changeState)
      {
        returnToInitState = true;
      }

      //Rcpp::Rcout <<"i="<< i << ", S=" << S <<", A=" << A<<std::endl;

      Rcpp::StringVector turns;
      turns = getTurnsFromPaths(A, S);
      int nbOfTurns = turns.length();
      //Rcpp::Rcout <<"Path="<< A << ", nbOfTurns=" << nbOfTurns<<std::endl;
      std::shared_ptr<TreeNode> currNode;
      for (int j = 0; j < nbOfTurns; j++)
      {
        std::string currTurn = Rcpp::as<std::string>(turns(j));
        //Rcpp::Rcout <<"j=" <<j <<", currTurn="<< currTurn<<std::endl;
        if (j == 0)
        {
          if (S == 0)
          {
            currNode = getChildNode(rootS1, currTurn);
            if (!currNode)
            {
              //Rcpp::Rcout <<"currTurn="<< currTurn <<" is not child of "<< rootS1->turn <<std::endl;
            }
          }
          else
          {
            currNode = getChildNode(rootS2, currTurn);
            if (!currNode)
            {
              //Rcpp::Rcout <<"currTurn="<< currTurn <<" is not child of "<< rootS2->turn <<std::endl;
            }
          }
        }
        else
        {
          currNode = getChildNode(currNode, currTurn);
          if (!currNode)
          {
            //Rcpp::Rcout <<"currNode is null"<<std::endl;
          }
        }

        if (!currNode)
        {
          //Rcpp::Rcout <<"currNode is null"<<std::endl;
        }
        //Rcpp::Rcout <<"currNode.turn="<< currNode->turn<<std::endl;
        episodeTurns.push_back(currNode);
        episodeTurnStates.push_back(S);
        //Rcpp::Rcout <<"session_turn_count="<< session_turn_count<<std::endl;
        //Rcpp::Rcout <<"turn_time=" << turn_times_session(session_turn_count) <<std::endl;
        episodeTurnTimes.push_back(turn_times_session(session_turn_count));

        //Change softmax function - input row of credits, first element is always the selected turn, return prob of turn

        double prob_a = softmax(currNode);
        //Rcpp::Rcout <<"prob_a="<< prob_a<<std::endl;
        double logProb = log(prob_a);
        mseMatrix.push_back(logProb);
        session_turn_count++;
      }

      //log_lik=log_lik+ logProb;

      //Check if episode ended
      if (returnToInitState)
      {
        //Rcpp::Rcout <<  "Inside end episode"<<std::endl;
        changeState = false;
        returnToInitState = false;

        avg_score = avg_score + (score_episode - avg_score) / episode;

        updateCreditMatrix(episodeTurns, episodeTurnStates, episodeTurnTimes, alpha, score_episode, avg_score, model);
        //Rcpp::Rcout <<  "H="<<H<<std::endl;
        score_episode = 0;
        episode = episode + 1;
        resetVector = true;
        episodeTurns.clear();
        episodeTurnStates.clear();
        episodeTurnTimes.clear();
      }
      S = S_prime;
      //trial=trial+1;
    }
  }

  return (mseMatrix);
}

// [[Rcpp::export()]]
arma::mat getProbMatrix(arma::mat allpaths, arma::mat turnTimes, int turnMethod, double alpha, double rewardVal, int sim, int model)
{

  if (sim != 1)
  {
    arma::mat v = arma::zeros(allpaths.n_rows, 1);
    allpaths = arma::join_horiz(allpaths, v);
  }
  //Rcpp::Rcout <<  "allpaths.col(4)="<<allpaths.col(4) <<std::endl;

  arma::mat mseMatrix;
  int total_paths = 0;
  int total_turns = 0;
  //int mseRowIdx = 0;

  arma::vec allpath_actions = allpaths.col(0);
  arma::vec allpath_states = allpaths.col(1);
  arma::vec allpath_rewards = allpaths.col(2);
  arma::vec sessionVec = allpaths.col(4);
  arma::vec uniqSessIdx = arma::unique(sessionVec);
  //Rcpp::Rcout <<  "uniqSessIdx.n_elem="<<uniqSessIdx.n_elem <<std::endl;

  arma::vec turnTime_method;
  if (turnMethod == 0)
  {
    turnTime_method = turnTimes.col(5);
  }
  else if (turnMethod == 1)
  {
    turnTime_method = turnTimes.col(6);
  }
  else if (turnMethod == 2)
  {
    turnTime_method = turnTimes.col(7);
  }

  int episode = 1;

  std::shared_ptr<TreeNode> rootS1 = initS1();
  std::shared_ptr<TreeNode> rootS2 = initS2();

  //Rcpp::Rcout <<"rootS1.turn ="<<rootS1->turn<<std::endl;
  //Rcpp::Rcout <<"rootS2.turn ="<<rootS2->turn<<std::endl;

  for (unsigned int session = 0; session < uniqSessIdx.n_elem; session++)
  {

    int sessId = uniqSessIdx(session);
    //Rcpp::Rcout <<"sessId="<<sessId<<std::endl;
    arma::uvec sessionIdx = arma::find(sessionVec == sessId);
    arma::vec actions_sess = allpath_actions.elem(sessionIdx);
    arma::vec states_sess = allpath_states.elem(sessionIdx);
    arma::vec rewards_sess = allpath_rewards.elem(sessionIdx);

    arma::uvec turnTimes_idx = arma::find(turnTimes.col(4) == sessId);
    arma::vec turn_times_session = turnTime_method.elem(turnTimes_idx);
    arma::uword session_turn_count = 0;

    int initState = 0;
    bool changeState = false;
    bool returnToInitState = false;
    int score_episode = 0;
    float avg_score = 0;
    bool resetVector = true;
    int nrow = actions_sess.n_elem;
    int S = 0;
    int A = 0;
    std::vector<std::shared_ptr<TreeNode>> episodeTurns;
    std::vector<int> episodeTurnStates;
    std::vector<double> episodeTurnTimes;
    //Rcpp::Rcout <<"sessId="<<sessId<< ", actions=" << nrow <<std::endl;
    for (int i = 0; i < (nrow - 1); i++)
    {
      total_paths++;
      if (resetVector)
      {
        initState = S;
        //Rcpp::Rcout <<"initState="<<initState<<std::endl;
        resetVector = false;
      }

      int R = rewards_sess(i);

      if (R > 0)
      {
        score_episode = score_episode + rewardVal;
      }

      if (sim == 1)
      {
        A = actions_sess(i);
      }
      else
      {
        A = actions_sess(i) - 1;
      }

      int S_prime = 0;
      if (sim == 1)
      {
        S_prime = states_sess(i + 1);
      }
      else
      {
        S_prime = states_sess(i + 1) - 1;
      }

      if (S_prime != initState)
      {
        changeState = true;
      }
      else if (S_prime == initState && changeState)
      {
        returnToInitState = true;
      }

      //Rcpp::Rcout <<"i="<< i << ", S=" << S <<", A=" << A<<std::endl;
      if (A != 5)
      {
        Rcpp::StringVector turns;
        turns = getTurnsFromPaths(A, S);
        int nbOfTurns = turns.length();
        //Rcpp::Rcout <<"Path="<< A << ", nbOfTurns=" << nbOfTurns<<std::endl;
        std::shared_ptr<TreeNode> currNode;
        for (int j = 0; j < nbOfTurns; j++)
        {
          total_turns++;
          std::string currTurn = Rcpp::as<std::string>(turns(j));
          //Rcpp::Rcout <<"j=" <<j <<", currTurn="<< currTurn<<std::endl;
          if (j == 0)
          {
            if (S == 0)
            {
              currNode = getChildNode(rootS1, currTurn);
              if (!currNode)
              {
                //Rcpp::Rcout <<"currTurn="<< currTurn <<" is not child of "<< rootS1->turn <<std::endl;
              }
            }
            else
            {
              currNode = getChildNode(rootS2, currTurn);
              if (!currNode)
              {
                //Rcpp::Rcout <<"currTurn="<< currTurn <<" is not child of "<< rootS2->turn <<std::endl;
              }
            }
          }
          else
          {
            currNode = getChildNode(currNode, currTurn);
            if (!currNode)
            {
              //Rcpp::Rcout <<"currNode is null"<<std::endl;
            }
          }

          if (!currNode)
          {
            //Rcpp::Rcout <<"currNode is null"<<std::endl;
          }
          //Rcpp::Rcout <<"currNode.turn="<< currNode->turn<<std::endl;
          episodeTurns.push_back(currNode);
          //Rcpp::Rcout <<"Adding state="<< S << " to episodeTurnStates"<<std::endl;
          episodeTurnStates.push_back(S);
          //Rcpp::Rcout <<"session_turn_count="<< session_turn_count<<std::endl;
          //Rcpp::Rcout <<"turn_time=" << turn_times_session(session_turn_count) <<std::endl;
          episodeTurnTimes.push_back(turn_times_session(session_turn_count));

          //Change softmax function - input row of credits, first element is always the selected turn, return prob of turn

          arma::rowvec probRow(16);
          probRow.fill(-1);
          double prob_a = softmax(currNode);
          unsigned int idx = getTurnIdx(currNode->turn, S);
          probRow(idx) = prob_a;
          for (auto sibling = currNode->siblings.begin(); sibling != currNode->siblings.end(); sibling++)
          {
            unsigned int idx = getTurnIdx((*sibling)->turn, S);
            probRow(idx) = softmax((*sibling));
          }

          //Rcpp::Rcout <<"prob_a="<< prob_a<<std::endl;
          mseMatrix = arma::join_vert(mseMatrix, probRow);
          session_turn_count++;
          //Rcpp::Rcout << "mseMatrix.n_rows=" << mseMatrix.n_rows << ", session_turn_count=" << session_turn_count << std::endl;
        }
      }

      //log_lik=log_lik+ logProb;

      //Check if episode ended
      if (returnToInitState)
      {
        //Rcpp::Rcout <<  "Inside end episode"<<std::endl;
        changeState = false;
        returnToInitState = false;

        avg_score = avg_score + (score_episode - avg_score) / episode;

        updateCreditMatrix(episodeTurns, episodeTurnStates, episodeTurnTimes, alpha, score_episode, avg_score, model);
        //Rcpp::Rcout <<  "H="<<H<<std::endl;
        score_episode = 0;
        episode = episode + 1;
        resetVector = true;
        episodeTurns.clear();
        episodeTurnStates.clear();
        episodeTurnTimes.clear();
      }
      S = S_prime;
      //trial=trial+1;
    }
    
  }
  //Rcpp::Rcout << "total_paths=" << total_paths << std::endl;
  //Rcpp::Rcout << "total_turns=" << total_turns << std::endl;

  return (mseMatrix);
}
