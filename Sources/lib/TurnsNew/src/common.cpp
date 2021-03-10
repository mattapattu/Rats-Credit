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

Edge softmax_action_sel(Graph graph, std::vector<Edge> edges)
{

  std::vector<double> probVec;
  for (auto edge = edges.begin(); edge != edges.end(); edge++)
  {
    probVec.push_back((*edge).probability);
  }
  arma::vec probVec_arma(probVec);
  //Rcpp::Rcout <<"creditVec_arma="<<creditVec_arma<<std::endl;

  IntegerVector actions = seq(0, (edges.size() - 1));
  int action_selected = Rcpp::RcppArmadillo::sample(actions, 1, true, probVec_arma)[0];
  //Rcpp::Rcout <<"action_selected="<<action_selected<<std::endl;

  return (edges[action_selected]);
}

//allpaths_num,turnTimes,0,1,model=1,turnMethod = 1
// [[Rcpp::export()]]
Rcpp::List simulateTurnsModels(arma::mat allpaths, arma::mat turnTimes, double alpha, double gamma1, double gamma2, Rcpp::S4 turnModel, arma::vec turnStages)
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
  Graph S0(turnModel, 0);
  Graph S1(turnModel, 1);

  int actionNb = 0;

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
    std::vector<std::string> episodeTurns;
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
      actionNb++;
      if (resetVector)
      {
        initState = S;
        //Rcpp::Rcout <<"initState="<<initState<<std::endl;
        resetVector = false;
      }

      Node *currNode;
      std::vector<Edge> *edges;
      Rcpp::StringVector turnNames;
      Graph graph;
      if (S == 0)
      {
        graph = S0;
        edges = graph.getOutgoingEdges("E");
      }
      else
      {
        graph = S1;
        edges = graph.getOutgoingEdges("I");
      }

      while (!edges->empty())
      {
        Edge edgeSelected = softmax_action_sel(graph, *edges);
        std::string turnSelected = edgeSelected.dest->node;
        currNode = edgeSelected.dest;

        
        int turnNb;
        if (S==0)
        {
          turnNb = S0.getNodeIndex(currNode->node); 
        }
        else
        {
          turnNb = S1.getNodeIndex(currNode->node); 
        }
         
        int turncount = turnIdx + 1;
        double turnTime = simulateTurnDuration(turnTimes, allpaths, turnNb, turncount, turnStages);

        //Rcpp::Rcout << "S=" << S << ", turnNb=" << turnNb << std::endl;
        generated_TurnsData_sess(turnIdx, 0) = turnNb;
        generated_TurnsData_sess(turnIdx, 1) = S;
        generated_TurnsData_sess(turnIdx, 2) = 0;
        generated_TurnsData_sess(turnIdx, 3) = turnTime;
        generated_TurnsData_sess(turnIdx, 4) = sessId;
        generated_TurnsData_sess(turnIdx, 5) = actionNb;

        turnNames.push_back(turnSelected);
        episodeTurns.push_back(currNode->node);
        episodeTurnStates.push_back(S);
        episodeTurnTimes.push_back(turnTime);
        //turns_index.push_back(turnIdx);

        turnIdx++;
        edges = graph.getOutgoingEdges(currNode->node);
      }

      int A;
      if(S == 0)
      {
        A = S0.getPathFromTurns(turnNames);
      }
      else
      {
       A = S1.getPathFromTurns(turnNames); 
      }
     
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
        Aca3CreditUpdate(episodeTurns, episodeTurnStates, episodeTurnTimes, alpha, score_episode, &S0, &S1);
        S0.updateEdgeProbs();
        S1.updateEdgeProbs();
        //Rcpp::Rcout <<  "H="<<H<<std::endl;
        score_episode = 0;
        episode = episode + 1;
        resetVector = true;
        episodeTurns.clear();
        episodeTurnStates.clear();
        episodeTurnTimes.clear();
      }

      //Rcpp::Rcout << "Here1" << std::endl;
      decayCredits(S0, gamma1);
      decayCredits(S1, gamma1);
      //Rcpp::Rcout << "Here2" << std::endl;

      S = S_prime;
    }

    //Rcpp::Rcout << "Here3" << std::endl;
    decayCredits(S0, gamma2);
    decayCredits(S1, gamma2);

    // if (turnIdx < (nrow * 2) - 1)
    // {
    //   generated_TurnsData_sess.shed_rows((turnIdx), ((nrow * 2) - 1));
    // }
    //Rcpp::Rcout << "Here4" << std::endl;
    generated_TurnData = arma::join_cols(generated_TurnData, generated_TurnsData_sess.rows(0, (turnIdx - 1)));
    //Rcpp::Rcout <<  "H after session=" << H<<std::endl;
    //Rcpp::Rcout << "Here5" << std::endl;
    generated_PathData = arma::join_cols(generated_PathData, generated_PathData_sess);
    //Rcpp::Rcout << "Here6" << std::endl;
    //Rcpp::Rcout <<  "likelihoodVec=" << likelihoodVec<<std::endl;
  }
  return (Rcpp::List::create(Named("PathData") = generated_PathData, _["TurnData"] = generated_TurnData));
}

// [[Rcpp::export()]]
std::vector<double> getTurnsLikelihood(arma::mat allpaths, arma::mat turnTimes, double alpha, double gamma1, double gamma2, double rewardVal, Rcpp::S4 turnModel, int sim)
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

  Graph S0(turnModel, 0);
  //S0.printGraph();
  Graph S1(turnModel, 1);
  //S1.printGraph();

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
    std::vector<std::string> episodeTurns;
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
      if(S==0)
      {
        turns = S0.getTurnsFromPaths(A);
      } 
      else
      {
        turns = S1.getTurnsFromPaths(A);
      }
      int nbOfTurns = turns.length();
      //Rcpp::Rcout <<"turns="<< turns << std::endl;

      Node *prevNode;
      Node *currNode;
      Graph graph;
      if (S == 0)
      {
        graph = S0;
        prevNode = graph.getNode("E");
      }
      else
      {
        graph = S1;
        prevNode = graph.getNode("I");
      }

      for (int j = 0; j < nbOfTurns; j++)
      {
        std::string currTurn = Rcpp::as<std::string>(turns(j));
        currNode = graph.getNode(currTurn);
        //currNode->credit = currNode->credit + 1; //Test
        //Rcpp::Rcout <<"currNode="<< currNode->node<<std::endl;
        episodeTurns.push_back(currNode->node);
        episodeTurnStates.push_back(S);
        episodeTurnTimes.push_back(turn_times_session(session_turn_count));

        Edge edge = graph.getEdge(prevNode->node, currNode->node);
        double prob_a = edge.probability;
        //Rcpp::Rcout <<"prob_a="<< prob_a<<std::endl;
        double logProb = log(prob_a);
        mseMatrix.push_back(logProb);

        session_turn_count++;
        prevNode = currNode;
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
        // for (unsigned int index = 0; index < episodeTurns.size(); ++index)
        // {
        //   Rcpp::Rcout << "index=" <<index << ", episodeTurns[index]= " <<episodeTurns[index]->node <<std::endl;
        // }
        Aca3CreditUpdate(episodeTurns, episodeTurnStates, episodeTurnTimes, alpha, score_episode, &S0, &S1);
        //Rcpp::Rcout << "Update S0 probabilities"<<std::endl;
        S0.updateEdgeProbs();
        //Rcpp::Rcout << "Update S1 probabilities"<<std::endl;
        S1.updateEdgeProbs();
        //Rcpp::Rcout <<  "H="<<H<<std::endl;
        score_episode = 0;
        episode = episode + 1;
        resetVector = true;
        episodeTurns.clear();
        episodeTurnStates.clear();
        episodeTurnTimes.clear();
      }
      decayCredits(S0, gamma1);
      decayCredits(S1, gamma1);
      S = S_prime;
      //trial=trial+1;
    }
    decayCredits(S0, gamma2);
    decayCredits(S1, gamma2);
  }

  return (mseMatrix);
}

// [[Rcpp::export()]]
arma::mat getProbMatrix(arma::mat allpaths, arma::mat turnTimes, double alpha, double gamma1, double gamma2, double rewardVal, Rcpp::S4 turnModel, int sim)
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
  arma::vec allpaths_pathNb = allpaths.col(5);
  arma::vec uniqSessIdx = arma::unique(sessionVec);
  arma::vec pathIds;
  if (sim == 1)
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

  Graph S0(turnModel, 0);
  Graph S1(turnModel, 1);
  //Rcpp::Rcout <<"Print S0"<<std::endl;
  //S0.printGraph();
  //Rcpp::Rcout <<"Print S1"<<std::endl;
  //S1.printGraph();

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
    arma::vec allpaths_pathNb_sess = allpaths_pathNb.elem(sessionIdx);

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
    std::vector<std::string> episodeTurns;
    std::vector<int> episodeTurnStates;
    std::vector<double> episodeTurnTimes;
    //Rcpp::Rcout <<"nrow="<<nrow<<std::endl;
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
      if(S==0)
      {
        turns = S0.getTurnsFromPaths(A);
      } 
      else
      {
        turns = S1.getTurnsFromPaths(A);
      }
      int nbOfTurns = turns.length();
      //Rcpp::Rcout <<"Path="<< A << ", turns=" << turns<<std::endl;
      Node *currNode;
      Graph graph;
      if (S == 0)
      {
        graph = S0;
      }
      else
      {
        graph = S1;
      }

      for (int j = 0; j < nbOfTurns; j++)
      {
        std::string currTurn = Rcpp::as<std::string>(turns(j));
        currNode = graph.getNode(currTurn);
        //Edge edge = graph.getEdge(prevNode->node, currNode->node);

        episodeTurns.push_back(currNode->node);
        episodeTurnStates.push_back(S);
        episodeTurnTimes.push_back(turn_times_session(session_turn_count));

        session_turn_count++;
      }
      //Rcpp::Rcout << "Here5" << std::endl;
      arma::rowvec probRow(13);
      probRow.fill(-1);
      probRow(12) = allpaths_pathNb_sess(i);

      for (int path = 0; path < 6; path++)
      {
        //Rcpp::Rcout << "path=" << path << ", state=" << S << std::endl;
        
        Rcpp::StringVector turnVec;
        if (S == 0)
        {
          turnVec = S0.getTurnsFromPaths(path);
          turnVec.push_front("E");
        }
        else
        {
          turnVec = S1.getTurnsFromPaths(path);
          turnVec.push_front("I");
        }
        //Rcpp::Rcout << "turnVec=" << turnVec << std::endl;
        double pathProb = 1;
        for (int k = 0; k < (turnVec.length() - 1); k++)
        {
          std::string turn1 = Rcpp::as<std::string>(turnVec[k]);
          std::string turn2 = Rcpp::as<std::string>(turnVec[k + 1]);
          //Rcpp::Rcout << "turn1=" << turn1 << ", turn2=" << turn2 << std::endl;
          Edge e;
          if (S == 0)
          {
            e = S0.getEdge(turn1, turn2);
          }
          else
          {
            e = S1.getEdge(turn1, turn2);
          }

          //Rcpp::Rcout << "Edge prob=" << e.probability << std::endl;
          pathProb = e.probability * pathProb;
        }
        int index = path + (6 * S);
        //Rcpp::Rcout << "index=" << index << ", pathProb=" << pathProb << std::endl;
        probRow[index] = pathProb;
      }
      //Rcpp::Rcout << "probRow=" << probRow << std::endl;
      mseMatrix = arma::join_vert(mseMatrix, probRow);

      //log_lik=log_lik+ logProb;

      //Check if episode ended
      if (returnToInitState)
      {
        //Rcpp::Rcout <<  "Inside end episode"<<std::endl;
        changeState = false;
        returnToInitState = false;

        avg_score = avg_score + (score_episode - avg_score) / episode;

        Aca3CreditUpdate(episodeTurns, episodeTurnStates, episodeTurnTimes, alpha, score_episode, &S0, &S1);
        S0.updateEdgeProbs();
        S1.updateEdgeProbs();
        score_episode = 0;
        episode = episode + 1;
        resetVector = true;
        episodeTurns.clear();
        episodeTurnStates.clear();
        episodeTurnTimes.clear();
      }

      decayCredits(S0, gamma1);
      decayCredits(S1, gamma1);
      S = S_prime;
      //Rcpp::Rcout <<  "Here1"<<std::endl;
      //trial=trial+1;
    }
    //Rcpp::Rcout <<  "Here2"<<std::endl;
    decayCredits(S0, gamma2);
    decayCredits(S1, gamma2);
    //Rcpp::Rcout <<  "Here3"<<std::endl;
  }

  return (mseMatrix);
}
