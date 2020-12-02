// [[Rcpp::depends(RcppArmadillo)]]
#include <vector>
#include <RcppArmadillo.h>

#include <regex>
#include <RcppArmadilloExtensions/sample.h>

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


// [[Rcpp::export()]]
std::vector<double> getTurnsLikelihood(arma::mat allpaths, double alpha, double gamma, double lambda, int sim)
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

    arma::uword session_turn_count = 0;

    int initState = 0;
    bool changeState = false;
    bool returnToInitState = false;
    bool resetVector = true;
    int nrow = actions_sess.n_rows;
    int S = 0;
    int A = 0;
    if (sim == 1)
    {
      S = states_sess(0);
      A = actions_sess(0);
    }
    else
    {
      S = states_sess(0) - 1;
      A = actions_sess(0) - 1;
    }
    std::vector<std::shared_ptr<TreeNode>> episodeTurns;
    std::vector<int> episodeTurnStates;

    for (int i = 0; i < (nrow - 1); i++)
    {

      if (resetVector)
      {
        initState = S;
        //Rcpp::Rcout <<"initState="<<initState<<std::endl;
        resetVector = false;
      }

     int R = rewards_sess(i);

      int S_prime = 0;
      int A_prime = 0;

      if (sim == 1)
      {
        A_prime = actions_sess(i+1);
        S_prime = states_sess(i+1);
      }
      else
      {
        A_prime = actions_sess(i+1) - 1;
        S_prime = states_sess(i+1) - 1;
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

        
        //Change softmax function - input row of credits, first element is always the selected turn, return prob of turn
        
        double prob_a = softmax(currNode);
        //Rcpp::Rcout <<"prob_a="<< prob_a<<std::endl;
        double logProb = log(prob_a);
        mseMatrix.push_back(logProb);

        double prediction = 0;
        if(j<nbOfTurns-1)
        {
          std::string nextTurn = Rcpp::as<std::string>(turns(j+1));
          std::shared_ptr<TreeNode> nextNode = getChildNode(currNode, nextTurn);
          prediction = gamma * nextNode->qval;
        }
        int turn_reward= 0;
        if(j==(nbOfTurns-1) && (R==1))
        {
          turn_reward = 1;
        }
        double td_err = turn_reward + prediction - currNode->qval;
        currNode->etrace = currNode->etrace + 1;
        
        updateQvals(rootS1, alpha, td_err, gamma);
        updateQvals(rootS2, alpha, td_err, gamma);
        
        session_turn_count++;
      }

      //log_lik=log_lik+ logProb;

      //Check if episode ended
      if (returnToInitState)
      {
        //Rcpp::Rcout <<  "Inside end episode"<<std::endl;
        changeState = false;
        returnToInitState = false;

        episode = episode + 1;
        resetVector = true;
        
      }

      S = S_prime;
      A = A_prime;
      //trial=trial+1;
    }
  }

  return (mseMatrix);
}


// [[Rcpp::export()]]
arma::mat getProbMatrix(arma::mat allpaths, double alpha, double gamma, double lambda, int sim)
{

  if (sim != 1)
  {
    arma::mat v = arma::zeros(allpaths.n_rows, 1);
    allpaths = arma::join_horiz(allpaths, v);
  }
  //Rcpp::Rcout <<  "allpaths.col(4)="<<allpaths.col(4) <<std::endl;

  arma::mat mseMatrix;
  //int mseRowIdx = 0;

  arma::vec allpath_actions = allpaths.col(0);
  arma::vec allpath_states = allpaths.col(1);
  arma::vec allpath_rewards = allpaths.col(2);
  arma::vec sessionVec = allpaths.col(4);
  arma::vec uniqSessIdx = arma::unique(sessionVec);

  
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

    arma::uword session_turn_count = 0;

    int initState = 0;
    bool changeState = false;
    bool returnToInitState = false;
    int score_episode = 0;
    bool resetVector = true;
    int nrow = actions_sess.n_rows;
    int S = 0;
    int A = 0;
    if (sim == 1)
    {
      S = states_sess(0);
      A = actions_sess(0);
    }
    else
    {
      S = states_sess(0) - 1;
      A = actions_sess(0) - 1;
    }
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
        score_episode = score_episode + 1;
      }

      
      int S_prime = 0;
      int A_prime = 0;

      if (sim == 1)
      {
        A_prime = actions_sess(i+1);
        S_prime = states_sess(i+1);
      }
      else
      {
        A_prime = actions_sess(i+1) - 1;
        S_prime = states_sess(i+1) - 1;
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

        double prediction = 0;
        if(j<nbOfTurns-1)
        {
          std::string nextTurn = Rcpp::as<std::string>(turns(j+1));
          std::shared_ptr<TreeNode> nextNode = getChildNode(currNode, nextTurn);
          prediction = gamma * nextNode->qval;
        }
        int turn_reward= 0;
        if(j==(nbOfTurns-1) && (R==1))
        {
          turn_reward = 1;
        }
        double td_err = turn_reward + prediction - currNode->qval;
        currNode->etrace = currNode->etrace + 1;
        
        updateQvals(rootS1, alpha, td_err, gamma);
        updateQvals(rootS2, alpha, td_err, gamma);
        
        session_turn_count++;
      }

      //log_lik=log_lik+ logProb;

      //Check if episode ended
       if (returnToInitState)
      {
        //Rcpp::Rcout <<  "Inside end episode"<<std::endl;
        changeState = false;
        returnToInitState = false;

        episode = episode + 1;
        resetVector = true;
        
      }
      S = S_prime;
      A = A_prime;
      //trial=trial+1;
    }
  }

  return (mseMatrix);
}
