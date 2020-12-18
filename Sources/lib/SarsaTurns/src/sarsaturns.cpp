// [[Rcpp::depends(RcppArmadillo)]]
#include <vector>
#include <RcppArmadillo.h>

#include <regex>
#include <RcppArmadilloExtensions/sample.h>

#include "utils.hpp"
#include "tree.hpp"

using namespace Rcpp;

int getNextState(int curr_state, int action)
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

std::shared_ptr<TreeNode> softmax_action_sel(std::vector<std::shared_ptr<TreeNode>> nodes)
{

  std::vector<double> qvalVec;
  for (auto node = nodes.begin(); node != nodes.end(); node++)
  {
    qvalVec.push_back((*node)->qval);
  }
  arma::vec qvalVec_arma(qvalVec);
  double m = arma::max(qvalVec_arma);

  //Rcpp::Rcout <<"m="<<m<<", v="<<v<<std::endl;

  qvalVec_arma = exp(qvalVec_arma - m);
  double exp_sum = arma::accu(qvalVec_arma);
  qvalVec_arma = qvalVec_arma / exp_sum;
  IntegerVector actions = seq(0, nodes.size());
  int action_selected = Rcpp::RcppArmadillo::sample(actions, 1, true, qvalVec_arma)[0];
  //Rcpp::Rcout <<"action_selected="<<action_selected<<std::endl;

  return (nodes[action_selected]);
}

//allpaths, A,S, sessId, pathCount
int getPathIndex(arma::mat allpaths, int action, int state, int sessId, int pathCount)
{
  arma::vec sessionVec = allpaths.col(3);
  arma::uvec sessionIdx = arma::find(sessionVec == (sessId));
  arma::vec allpath_actions = allpaths.col(0);
  arma::vec allpath_states = allpaths.col(1);
  arma::vec actions_sess = allpath_actions.elem(sessionIdx);
  arma::vec states_sess = allpath_states.elem(sessionIdx);

  arma::uvec act_idx = arma::find(actions_sess == action && states_sess == state);

  int pathIndex = -1;

  if (act_idx.n_elem == 0)
  {
    pathIndex = -1;
  }
  else if (act_idx.n_elem >= pathCount)
  {
    pathIndex = act_idx(pathCount - 1);
  }
  else if (act_idx.n_elem <= pathCount)
  {
    pathIndex = act_idx(act_idx.n_elem - 1);
  }

  return (pathIndex);
}

//allpaths, A,S, sessId, pathCount
double getPathTime(arma::mat allpaths, int action, int state, int sessId, int pathCount)
{
  arma::vec sessionVec = allpaths.col(3);
  arma::uvec sessionIdx = arma::find(sessionVec == (sessId));
  arma::vec allpath_actions = allpaths.col(0);
  arma::vec allpath_states = allpaths.col(1);
  arma::vec allpath_times = allpaths.col(3);
  arma::vec actions_sess = allpath_actions.elem(sessionIdx);
  arma::vec states_sess = allpath_states.elem(sessionIdx);
  arma::vec trialTimes_sess = allpath_times.elem(sessionIdx);

  arma::uvec act_idx = arma::find(actions_sess == action && states_sess == state);

  double pathTime = 0;

  if (act_idx.n_elem == 0)
  {
    arma::uvec allpaths_actIdx = arma::find(allpath_actions == action && allpath_states == state);
    if (allpaths_actIdx.n_elem == 0)
    {
      pathTime = 100; //Since this path is never taken, assign a path time
    }
    else
    {
      pathTime = arma::mean(allpath_times.elem(allpaths_actIdx));
    }
  }
  else if (act_idx.n_elem >= pathCount)
  {
    pathTime = trialTimes_sess(act_idx(pathCount - 1));
  }
  else if (act_idx.n_elem <= pathCount)
  {
    pathTime = trialTimes_sess(act_idx(act_idx.n_elem - 1));
  }

  return (pathTime);
}

//turnTimes, pathIndex, generated_TurnsData_sess, turnIds, 
arma::mat updateTurnTime(arma::mat turnTimes, int pathIndex, arma::mat generated_PathData_sess, Rcpp::IntegerVector generatedTurnIds, int turnMethod)
{
  arma::vec pathIds = turnTimes.col(0);
  arma::uvec sessionIdx = arma::find(pathIds == pathIndex);
  arma::vec turns = turnTimes.col(3);
  arma::vec turn_states = turnTimes.col(2);
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

  arma::vec turns_currPath = turns.elem(sessionIdx);
  arma::vec turnTime_currPath = turnTime_method.elem(sessionIdx);
  
  for(int i=0; i< generatedTurnIds.length();i++)
  {
    int turn = generated_PathData_sess(generatedTurnIds[i],0);

    for(int j=0; j< turns_currPath.n_elem;j++)
    {
      if(turns_currPath(j) == generatedTurnIds[i])
      {
        generated_PathData_sess(generatedTurnIds[i],3) = turnTime_currPath(j);
      }
    }
  
  
  }

  return (generated_PathData_sess);
}

// [[Rcpp::export()]]
Rcpp::List simulateSarsa(arma::mat allpaths, arma::mat turnTimes, double alpha, double gamma, double lambda, int turnMethod)
{

  arma::mat R = arma::zeros(2, 6);
  R(0, 3) = 1;
  R(1, 3) = 1;
  arma::mat generated_PathData;
  arma::mat generated_TurnData;
  arma::vec allpath_actions = allpaths.col(0);
  arma::vec allpath_states = allpaths.col(1);
  //arma::vec allpath_rewards = allpaths.col(2);
  arma::vec sessionVec = allpaths.col(3);
  arma::vec uniqSessIdx = arma::unique(sessionVec);

  arma::vec all_turns = turnTimes.col(1);
  arma::vec turns_sessions = turnTimes.col(4);

  //Rcpp::Rcout << "sessionVec=" << sessionVec << std::endl;
  //Rcpp::Rcout << "uniqSessIdx=" << uniqSessIdx << std::endl;
  std::shared_ptr<TreeNode> rootS1 = initS1();
  std::shared_ptr<TreeNode> rootS2 = initS2();

  // Loop through each session
  for (unsigned int session = 0; session < (uniqSessIdx.n_elem); session++)
  {

    int sessId = uniqSessIdx(session);
    //Rcpp::Rcout << "session=" <<session <<", sessId=" << sessId << std::endl;
    arma::uvec sessionIdx = arma::find(sessionVec == (sessId));
    arma::vec actions_sess = allpath_actions.elem(sessionIdx);
    arma::vec states_sess = allpath_states.elem(sessionIdx);

    arma::uvec turns_sessIdx = arma::find(turns_sessions == (sessId));
    arma::vec turns_sess = all_turns.elem(turns_sessIdx);

    int initState = 0;
    bool changeState = false;
    bool returnToInitState = false;
    bool resetVector = true;
    int nrow = actions_sess.n_rows;

    int S = states_sess(0) - 1;

    arma::mat etrace(2, 6, arma::fill::zeros);
    arma::mat generated_PathData_sess(nrow, 5);
    arma::mat generated_TurnsData_sess((nrow * 2), 5);
    generated_PathData_sess.fill(-1);
    int turnIdx = 0;
        //All episodes in new session

    for (int i = 0; i < nrow; i++)
    {
      std::shared_ptr<TreeNode> rootNode;
      Rcpp::StringVector turns;
      if (S == 0)
      {
        rootNode = rootS1;
      }
      else
      {
        rootNode = rootS2;
      }
      std::vector<std::shared_ptr<TreeNode>> childNodes = rootNode->child;

      Rcpp::IntegerVector turnIds;
      while (!childNodes.empty())
      {
        std::shared_ptr<TreeNode> turnSelected = softmax_action_sel(childNodes);
        turns.push_back(turnSelected->turn);
        int turnIndex = getTurnIdx(turnSelected->turn, S);
        generated_TurnsData_sess(turnIdx, 0) = turnIndex;
        generated_TurnsData_sess(turnIdx, 1) = S;
        generated_TurnsData_sess(turnIdx, 2) = 0;

        arma::uvec turn_idx = arma::find(generated_TurnsData_sess.col(0) == turnIndex && generated_TurnsData_sess.col(1) == S);
        int turnCount = turn_idx.n_elem;

        //generated_TurnsData_sess(i,3) = getTurnTime(turnTimes, turnIndex, S, sessId, turnCount);
        generated_TurnsData_sess(turnIdx, 4) = sessId;
        childNodes = turnSelected->child;
        turnIds.push_back(turnIdx);
        turnIdx++;
      }

      int A = getPathFromTurns(turns, S);
      S = getNextState(S, A);
      generated_PathData_sess(i, 0) = A;
      generated_PathData_sess(i, 1) = S;
      generated_PathData_sess(i, 2) = R(S, A);

      if (R(S, A) == 1)
      {
        generated_TurnsData_sess(turnIdx, 2) = 1;
      }
      arma::uvec act_idx = arma::find(generated_PathData_sess.col(0) == A && generated_PathData_sess.col(1) == S);
      int pathCount = act_idx.n_elem;
      generated_PathData_sess(i, 3) = getPathTime(allpaths, A, S, sessId, pathCount);
      int pathIndex = getPathIndex(allpaths, A, S, sessId, pathCount);
      generated_PathData_sess(i, 4) = sessId;
      generated_PathData_sess = updateTurnTime(turnTimes, pathIndex, generated_TurnsData_sess, turnIds, turnMethod);
    }

    if (turnIdx < (nrow * 2) - 1)
    {
      generated_TurnsData_sess.shed_rows((turnIdx + 1), ((nrow * 2) - 1));
    }
    generated_TurnData = arma::join_cols(generated_TurnData, generated_TurnsData_sess);
    //Rcpp::Rcout <<  "H after session=" << H<<std::endl;
    generated_PathData = arma::join_cols(generated_PathData, generated_PathData_sess);
    //Rcpp::Rcout <<  "likelihoodVec=" << likelihoodVec<<std::endl;
  }
  return (Rcpp::List::create(Named("PathData") = generated_PathData , _["turnData"] = generated_TurnData));
}

// [[Rcpp::export()]]
std::vector<double> getTurnsLikelihood(arma::mat allpaths, double alpha, double gamma, double lambda, double rewardVal, int sim)
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
        A_prime = actions_sess(i + 1);
        S_prime = states_sess(i + 1);
      }
      else
      {
        A_prime = actions_sess(i + 1) - 1;
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
          Rcpp::Rcout << "currNode is null" << std::endl;
        }
        //Rcpp::Rcout <<"currNode.turn="<< currNode->turn<<std::endl;

        //Change softmax function - input row of credits, first element is always the selected turn, return prob of turn

        double prob_a = softmax(currNode);
        //Rcpp::Rcout <<"prob_a="<< prob_a<<std::endl;
        double logProb = log(prob_a);
        mseMatrix.push_back(logProb);

        double prediction = 0;
        if (j < nbOfTurns - 1)
        {
          std::string nextTurn = Rcpp::as<std::string>(turns(j + 1));
          std::shared_ptr<TreeNode> nextNode = getChildNode(currNode, nextTurn);
          prediction = gamma * nextNode->qval;
        }
        else if (j == (nbOfTurns - 1))
        {
          //Rcpp::Rcout <<"A_prime=" << A_prime << ", S_prime=" <<S_prime<< std::endl;
          if (A_prime == 5)
          {
            prediction = 0;
          }
          else
          {
            Rcpp::StringVector next_turns = getTurnsFromPaths(A_prime, S_prime);
            std::string nextTurn_prime = Rcpp::as<std::string>(next_turns(0));
            //Rcpp::Rcout <<"nextTurn_prime="<< nextTurn_prime << std::endl;
            std::shared_ptr<TreeNode> nextNode_prime;
            if (S_prime == 0)
            {
              nextNode_prime = getChildNode(rootS1, nextTurn_prime);
            }
            else
            {
              nextNode_prime = getChildNode(rootS2, nextTurn_prime);
            }
            //Rcpp::Rcout <<"nextTurn_prime="<< nextTurn_prime << ", nextNode_prime=" << nextNode_prime->turn<<std::endl;
            prediction = gamma * nextNode_prime->qval;
          }
        }

        int turn_reward = 0;
        if (j == (nbOfTurns - 1) && (R > 0))
        {
          turn_reward = rewardVal;
        }
        double td_err = turn_reward + prediction - currNode->qval;
        currNode->etrace = currNode->etrace + 1;

        updateQvals(rootS1, alpha, td_err, gamma, lambda);
        updateQvals(rootS2, alpha, td_err, gamma, lambda);

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
arma::mat getProbMatrix(arma::mat allpaths, double alpha, double gamma, double lambda, double rewardVal, int sim)
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

      int S_prime = 0;
      int A_prime = 0;

      if (sim == 1)
      {
        A_prime = actions_sess(i + 1);
        S_prime = states_sess(i + 1);
      }
      else
      {
        A_prime = actions_sess(i + 1) - 1;
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
      //Rcpp::Rcout <<"A_prime=" << A_prime << ", S_prime=" <<S_prime<< std::endl;
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
        if (j < nbOfTurns - 1)
        {
          std::string nextTurn = Rcpp::as<std::string>(turns(j + 1));
          std::shared_ptr<TreeNode> nextNode = getChildNode(currNode, nextTurn);
          prediction = gamma * nextNode->qval;
        }
        else if (j == (nbOfTurns - 1))
        {
          //Rcpp::Rcout <<"A_prime=" << A_prime << ", S_prime=" <<S_prime<< std::endl;
          if (A_prime == 5)
          {
            prediction = 0;
          }
          else
          {
            Rcpp::StringVector next_turns = getTurnsFromPaths(A_prime, S_prime);
            std::string nextTurn_prime = Rcpp::as<std::string>(next_turns(0));
            //Rcpp::Rcout <<"nextTurn_prime="<< nextTurn_prime << std::endl;
            std::shared_ptr<TreeNode> nextNode_prime;
            if (S_prime == 0)
            {
              nextNode_prime = getChildNode(rootS1, nextTurn_prime);
            }
            else
            {
              nextNode_prime = getChildNode(rootS2, nextTurn_prime);
            }
            //Rcpp::Rcout <<"nextTurn_prime="<< nextTurn_prime << ", nextNode_prime=" << nextNode_prime->turn<<std::endl;
            prediction = gamma * nextNode_prime->qval;
          }
        }
        int turn_reward = 0;
        if (j == (nbOfTurns - 1) && (R > 0))
        {
          turn_reward = rewardVal;
        }
        //Rcpp::Rcout <<"turn=" <<currNode->turn << ", turn_reward="<< turn_reward << ", A=" << A << ", S=" << S<<std::endl;

        double td_err = turn_reward + prediction - currNode->qval;
        currNode->etrace = currNode->etrace + 1;

        // if(idx == 7 || idx ==8)
        // {
        //   Rcpp::Rcout <<"turn=" << currNode->turn << ", td_error="<< td_err << ", qval=" << currNode->qval<<std::endl;
        // }

        updateQvals(rootS1, alpha, td_err, gamma, lambda);
        updateQvals(rootS2, alpha, td_err, gamma, lambda);

        // if(idx == 7 || idx ==8)
        // {
        //   Rcpp::Rcout <<"turn=" << currNode->turn <<", qval after update=" << currNode->qval<<std::endl;
        // }

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
