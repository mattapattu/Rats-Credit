// [[Rcpp::depends(RcppArmadillo)]]
#include <vector>
#include <RcppArmadillo.h>

#include <regex>
#include <RcppArmadilloExtensions/sample.h>

#include "aca3CreditUpdate.hpp"
#include "utils.hpp"
#include "tree.hpp"

using namespace Rcpp;

//Function simulateTurnTimeFromR = Environment::global_env()["simulateTurnTime"];


int aca_getNextState(int curr_state, int action, int last_turn)
{
  //Rcpp::Rcout << "curr_state=" << curr_state << ", action=" << action << ", last_turn=" << last_turn << std::endl;
  int new_state = -1;
  if (action == 4 || action == 5)
  {
    new_state = curr_state;
  }
  else if (action == 6)
  {
    if (last_turn == 4 || last_turn == 7 || last_turn == 12 || last_turn == 15)
    {
      new_state = 1;
    }
    else if (last_turn == 5 || last_turn == 6 || last_turn == 13 || last_turn == 14)
    {
      new_state = 0;
    }
  }
  else if (curr_state == 0)
  {
    new_state = 1;
  }
  else if (curr_state == 1)
  {
    new_state = 0;
  }

  //Rcpp::Rcout << "new_state=" << new_state << std::endl;

  return (new_state);
}

std::shared_ptr<TreeNode> softmax_action_sel(std::vector<std::shared_ptr<TreeNode>> nodes)
{

  std::vector<double> creditVec;
  for (auto node = nodes.begin(); node != nodes.end(); node++)
  {
    creditVec.push_back((*node)->credit);
  }
  arma::vec creditVec_arma(creditVec);
  //Rcpp::Rcout <<"creditVec_arma="<<creditVec_arma<<std::endl;

  double m = arma::max(creditVec_arma);

  //Rcpp::Rcout <<"m="<<m<<", v="<<v<<std::endl;

  creditVec_arma = exp(creditVec_arma - m);
  double exp_sum = arma::accu(creditVec_arma);
  creditVec_arma = creditVec_arma / exp_sum;
  IntegerVector actions = seq(0, (nodes.size() - 1));
  int action_selected = Rcpp::RcppArmadillo::sample(actions, 1, true, creditVec_arma)[0];
  //Rcpp::Rcout <<"action_selected="<<action_selected<<std::endl;

  return (nodes[action_selected]);
}


//allpaths_num,turnTimes,0,1,model=1,turnMethod = 1
// [[Rcpp::export()]]
Rcpp::List simulateTurnsModels(arma::mat allpaths, arma::mat turnTimes, double alpha, double gamma1, double gamma2, arma::vec turnStages)
{

  //Rcpp::Rcout << "model=" << model << ", turnMethod=" << turnMethod << std::endl;
  arma::mat R = arma::zeros(2, 6);
  R(0, 3) = 1;
  R(1, 3) = 1;
  arma::mat generated_PathData;
  arma::mat generated_TurnData;
  arma::vec allpath_actions = allpaths.col(0);
  arma::vec allpath_states = allpaths.col(1);
  //arma::vec allpath_rewards = allpaths.col(2);
  arma::vec allpath_duration = allpaths.col(3);
  arma::vec sessionVec = allpaths.col(4);
  arma::vec uniqSessIdx = arma::unique(sessionVec);
  arma::vec pathNb = allpaths.col(5);

  arma::vec all_turns = turnTimes.col(3);
  arma::vec turns_sessions = turnTimes.col(4);

  //Rcpp::Rcout << "sessionVec=" << sessionVec << std::endl;
  //Rcpp::Rcout << "uniqSessIdx=" << uniqSessIdx << std::endl;
  std::shared_ptr<TreeNode> rootS1 = initS1();
  std::shared_ptr<TreeNode> rootS2 = initS2();
  int actionNb=0;

  // Loop through each session
  for (unsigned int session = 0; session < (uniqSessIdx.n_elem); session++)
  {

    int sessId = uniqSessIdx(session);
    //Rcpp::Rcout << "session=" << session << ", sessId=" << sessId << std::endl;
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
    double avg_score = 0;
    double score_episode = 0;
    int episode = 1;

    int S = states_sess(0) - 1;
    std::vector<std::shared_ptr<TreeNode>> episodeTurns;
    std::vector<int> episodeTurnStates;
    std::vector<double> episodeTurnTimes;

    arma::mat generated_PathData_sess(nrow, 6);
    arma::mat generated_TurnsData_sess((nrow * 3), 6);
    generated_PathData_sess.fill(-1);
    generated_TurnsData_sess.fill(-1);
    unsigned int turnIdx = 0;
    //All episodes in new session
    //Rcpp::Rcout << "nrow=" << nrow << std::endl;
    for (int i = 0; i < nrow; i++)
    {
      std::shared_ptr<TreeNode> rootNode;
      actionNb++;
      if (resetVector)
      {
        initState = S;
        //Rcpp::Rcout <<"initState="<<initState<<std::endl;
        resetVector = false;
      }

      if (S == 0)
      {
        rootNode = rootS1;
      }
      else
      {
        rootNode = rootS2;
      }
      std::vector<std::shared_ptr<TreeNode>> childNodes = rootNode->child;

      std::vector<unsigned int> turns_index;
      Rcpp::StringVector turnNames;
      while (!childNodes.empty())
      {
        std::shared_ptr<TreeNode> turnSelected = softmax_action_sel(childNodes);
        //Rcpp::Rcout << "turnSelected=" << turnSelected->turn << std::endl;
        int turnNb = getTurnIdx(turnSelected->turn, S);
        //Rcpp::Rcout << "S=" << S << ", turnNb=" << turnNb << std::endl;
        generated_TurnsData_sess(turnIdx, 0) = turnNb;
        generated_TurnsData_sess(turnIdx, 1) = S;
        generated_TurnsData_sess(turnIdx, 2) = 0;
        //Rcpp::Rcout << "turnNb=" << turnNb << ", turnIdx=" << (turnIdx+1) << std::endl;
        int turncount = turnIdx+1;
        double turnTime = simulateTurnDuration(turnTimes,allpaths,turnNb,turncount, turnStages);
        //Rcpp::Rcout << "turnTime=" << turnTime << std::endl;
        generated_TurnsData_sess(turnIdx, 3) = turnTime;
        generated_TurnsData_sess(turnIdx, 4) = sessId;
        generated_TurnsData_sess(turnIdx, 5) = actionNb;
        episodeTurns.push_back(turnSelected);
        turnNames.push_back(turnSelected->turn);
        episodeTurnStates.push_back(S);
        //Rcpp::Rcout << "selected turn=" << turnSelected->turn << std::endl;
        episodeTurnTimes.push_back(turnTime);
        childNodes = turnSelected->child;
        turns_index.push_back(turnIdx);
        turnIdx++;
      }

      int A = getPathFromTurns(turnNames, S);
      //Rcpp::Rcout <<"i=" <<i << ", A=" << A << ", S=" << S << ", sessId=" <<sessId<< std::endl;
      generated_PathData_sess(i, 0) = A;
      generated_PathData_sess(i, 1) = S;
      generated_PathData_sess(i, 2) = R(S, A);
      arma::vec arma_episodeTurnTimes(episodeTurnTimes);
      double pathTime = arma::accu(arma_episodeTurnTimes);
      generated_PathData_sess(i, 3) = pathTime;
      generated_PathData_sess(i, 4) = sessId;
      generated_PathData_sess(i, 5) = actionNb;

      if (R(S, A) == 1)
      {
        //Rcpp::Rcout << "turnNb=" << generated_TurnsData_sess((turnIdx - 1), 0) << ", receives reward"<< std::endl;
        generated_TurnsData_sess((turnIdx - 1), 2) = 1;
        score_episode = score_episode + 1;
      }

      int last_turn = generated_TurnsData_sess((turnIdx - 1), 0);
      
      int S_prime = aca_getNextState(S, A, last_turn);
      //Rcpp::Rcout <<"last_turn=" <<last_turn << ", S=" <<S <<", A=" <<A << ", S_prime=" << S_prime << std::endl;

      if (S_prime != initState)
      {
        changeState = true;
      }
      else if (S_prime == initState && changeState)
      {
        returnToInitState = true;
      }

      if (returnToInitState)
      {
        //Rcpp::Rcout << "Inside end episode" << std::endl;
        changeState = false;
        returnToInitState = false;

        avg_score = (avg_score * (episode - 1) + (score_episode - avg_score)) / episode;

        Aca3CreditUpdate(episodeTurns, episodeTurnStates, episodeTurnTimes, alpha, score_episode);
        //Rcpp::Rcout <<  "H="<<H<<std::endl;
        score_episode = 0;
        episode = episode + 1;
        resetVector = true;
        episodeTurns.clear();
        episodeTurnStates.clear();
        episodeTurnTimes.clear();
      }

      //Rcpp::Rcout << "Here1" << std::endl;
      decayCredits(rootS1, gamma1);
      decayCredits(rootS2, gamma1);
      //Rcpp::Rcout << "Here2" << std::endl;

      S = S_prime;
    }

   //Rcpp::Rcout << "Here3" << std::endl;
    decayCredits(rootS1, gamma2);
    decayCredits(rootS2, gamma2);

    // if (turnIdx < (nrow * 2) - 1)
    // {
    //   generated_TurnsData_sess.shed_rows((turnIdx), ((nrow * 2) - 1));
    // }
    //Rcpp::Rcout << "Here4" << std::endl;
    generated_TurnData = arma::join_cols(generated_TurnData, generated_TurnsData_sess.rows(0,(turnIdx-1)));
    //Rcpp::Rcout <<  "H after session=" << H<<std::endl;
    //Rcpp::Rcout << "Here5" << std::endl;
    generated_PathData = arma::join_cols(generated_PathData, generated_PathData_sess);
    //Rcpp::Rcout << "Here6" << std::endl;
    //Rcpp::Rcout <<  "likelihoodVec=" << likelihoodVec<<std::endl;
  }
  return (Rcpp::List::create(Named("PathData") = generated_PathData, _["TurnData"] = generated_TurnData));
}

// [[Rcpp::export()]]
std::vector<double> getTurnsLikelihood(arma::mat allpaths, arma::mat turnTimes, double alpha, double gamma1, double gamma2, double rewardVal, int sim)
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
  if (sim == 1)
  {
    turnTime_method = turnTimes.col(3);
  }
  else
  {
     turnTime_method = turnTimes.col(5);
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
      std::shared_ptr<TreeNode> parentNode;
      for (int j = 0; j < nbOfTurns; j++)
      {
        std::string currTurn = Rcpp::as<std::string>(turns(j));
        //Rcpp::Rcout <<"j=" <<j <<", currTurn="<< currTurn<<std::endl;
        if (j == 0)
        {
          if (S == 0)
          {
            currNode = getChildNode(rootS1, currTurn);
            parentNode = rootS1;
            if (!currNode)
            {
              //Rcpp::Rcout <<"currTurn="<< currTurn <<" is not child of "<< rootS1->turn <<std::endl;
            }
          }
          else
          {
            currNode = getChildNode(rootS2, currTurn);
            parentNode = rootS2;
            if (!currNode)
            {
              //Rcpp::Rcout <<"currTurn="<< currTurn <<" is not child of "<< rootS2->turn <<std::endl;
            }
          }
        }
        else
        {
          parentNode = currNode;
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
        //Rcpp::Rcout <<"session_turn_count="<< session_turn_count <<", turn_time=" << turn_times_session(session_turn_count) <<std::endl;
        episodeTurnTimes.push_back(turn_times_session(session_turn_count));

        //Change softmax function - input row of credits, first element is always the selected turn, return prob of turn

        double prob_a = softmax(currNode,parentNode);
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
        //Rcpp::Rcout <<  "score_episode=" << score_episode<<std::endl;
        Aca3CreditUpdate(episodeTurns, episodeTurnStates, episodeTurnTimes, alpha, score_episode);
        //Rcpp::Rcout <<  "H="<<H<<std::endl;
        score_episode = 0;
        episode = episode + 1;
        resetVector = true;
        episodeTurns.clear();
        episodeTurnStates.clear();
        episodeTurnTimes.clear();
      }
      decayCredits(rootS1, gamma1);
      decayCredits(rootS2, gamma1);
      S = S_prime;
      //trial=trial+1;
    }
    decayCredits(rootS1, gamma2);
    decayCredits(rootS2, gamma2);
  }

  return (mseMatrix);
}

// [[Rcpp::export()]]
arma::mat getProbMatrix(arma::mat allpaths, arma::mat turnTimes, double alpha, double gamma1, double gamma2, double rewardVal, int sim)
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
  arma::vec pathIds;
  if(sim == 1)
  {
    pathIds = turnTimes.col(5);
  }
  else
  {
    pathIds = turnTimes.col(0);
  }

  arma::vec turnTime_method;
  if (sim == 1)
  {
    turnTime_method = turnTimes.col(3);
  }
  else
  {
   turnTime_method = turnTimes.col(5);
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
    arma::vec pathIds_session = pathIds.elem(turnTimes_idx);
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
      //Rcpp::Rcout <<"Path="<< A << ", turns=" << turns<<std::endl;
      std::shared_ptr<TreeNode> currNode;
      std::shared_ptr<TreeNode> parentNode;
      for (int j = 0; j < nbOfTurns; j++)
      {
        std::string currTurn = Rcpp::as<std::string>(turns(j));
        //Rcpp::Rcout <<"j=" <<j <<", currTurn="<< currTurn<<std::endl;
        if (j == 0)
        {
          if (S == 0)
          {
            currNode = getChildNode(rootS1, currTurn);
            parentNode = rootS1;
            if (!currNode)
            {
              //Rcpp::Rcout <<"currTurn="<< currTurn <<" is not child of "<< rootS1->turn <<std::endl;
            }
          }
          else
          {
            currNode = getChildNode(rootS2, currTurn);
            parentNode = rootS2;
            if (!currNode)
            {
              //Rcpp::Rcout <<"currTurn="<< currTurn <<" is not child of "<< rootS2->turn <<std::endl;
            }
          }
        }
        else
        {
          parentNode = currNode;
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

        arma::rowvec probRow(17);
        probRow.fill(-1);
        probRow(16) = pathIds_session(session_turn_count);
        // double prob_a = softmax(currNode,parentNode);
        // unsigned int idx = getTurnIdx(currNode->turn, S);
        // probRow(idx) = prob_a;
        for (auto child = parentNode->child.begin(); child != parentNode->child.end(); child++)
        {
          std::shared_ptr<TreeNode> stgPtr = (*child);
          unsigned int idx = getTurnIdx(stgPtr->turn, S);
          probRow(idx) = softmax(stgPtr, parentNode);
          //Rcpp::Rcout <<"probRow(idx)="<< probRow(idx) << ", idx=" <<idx<<std::endl;
        }

        //Rcpp::Rcout <<"prob_a="<< prob_a<<std::endl;
        mseMatrix = arma::join_vert(mseMatrix, probRow);
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

        Aca3CreditUpdate(episodeTurns, episodeTurnStates, episodeTurnTimes, alpha, score_episode);
        //Rcpp::Rcout <<  "H="<<H<<std::endl;
        score_episode = 0;
        episode = episode + 1;
        resetVector = true;
        episodeTurns.clear();
        episodeTurnStates.clear();
        episodeTurnTimes.clear();
      }

      decayCredits(rootS1, gamma1);
      decayCredits(rootS2, gamma1);
      S = S_prime;

      //trial=trial+1;
    }
    decayCredits(rootS1, gamma2);
    decayCredits(rootS2, gamma2);
  }

  return (mseMatrix);
}
