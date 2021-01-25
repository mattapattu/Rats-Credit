// [[Rcpp::depends(RcppArmadillo)]]
#include <vector>
#include <RcppArmadillo.h>
#include <regex>
#include <RcppArmadilloExtensions/sample.h>
#include "acaCreditUpdate.hpp"
#include "utils.hpp"

using namespace Rcpp;

inline double softmax_cpp3(int A, int S, arma::mat H)
{
  //Rcpp::Rcout <<  "S="<< S<<std::endl;
  arma::rowvec v = H.row(S);
  //Rcpp::Rcout <<  v<< std::endl;
  double m = arma::max(v);
  double exp_sum = std::exp(H(S, 0) - m) + std::exp(H(S, 1) - m) + std::exp(H(S, 2) - m) + std::exp(H(S, 3) - m) + std::exp(H(S, 4) - m) + std::exp(H(S, 5) - m);
  double pr_A = (std::exp(H(S, A) - m)) / exp_sum;

  //  float m=arma::max(v);
  //  v=exp(v-m);
  // // //Rcpp::Rcout << "m=" << m<< std::endl;
  //  double exp_sum  = arma::accu(v) ;
  //  v=v/exp_sum;
  //  double pr_A=v[A];

  if (pr_A < 0)
  {
    Rcpp::Rcout << "A=" << A << ", S=" << S << std::endl;
    Rcpp::Rcout << H << std::endl;
    Rcpp::Rcout << "m=" << m << std::endl;
    Rcpp::Rcout << "Numerator=" << std::exp(H(S, A) - m) << std::endl;
    Rcpp::Rcout << "exp_sum=" << exp_sum << std::endl;
    // Rcpp::Rcout << "std::exp(H(S,0))-m)=" << std::exp(H(S,0)-m)  << std::endl;
    // Rcpp::Rcout << "std::exp(H(S,1))-m)=" << std::exp(H(S,1)-m)  << std::endl;
    // Rcpp::Rcout << "std::exp(H(S,2))-m)=" << std::exp(H(S,2)-m)  << std::endl;
    // Rcpp::Rcout << "std::exp(H(S,3))-m)=" << std::exp(H(S,3)-m)  << std::endl;
    // Rcpp::Rcout << "std::exp(H(S,4))-m)=" << std::exp(H(S,4)-m)  << std::endl;
    // Rcpp::Rcout << "std::exp(H(S,5))-m)=" << std::exp(H(S,5)-m)  << std::endl;
    Rcpp::Rcout << v << std::endl;

    //stop("logProb is NAN");
  }
  else if (pr_A > 1)
  {
    Rcpp::Rcout << "pr_A=" << pr_A << " is > 1" << std::endl;
    Rcpp::Rcout << "A=" << A << ", S=" << S << std::endl;
    Rcpp::Rcout << H << std::endl;
    Rcpp::Rcout << "m=" << m << std::endl;
    Rcpp::Rcout << "Numerator=" << std::exp(H(S, A) - m) << std::endl;
    Rcpp::Rcout << "exp_sum=" << exp_sum << std::endl;
    Rcpp::Rcout << "(std::exp(H(S,0))-m)=" << std::exp(H(S, 0) - m) << std::endl;
    Rcpp::Rcout << "(std::exp(H(S,1))-m)=" << std::exp(H(S, 1) - m) << std::endl;
    Rcpp::Rcout << "(std::exp(H(S,2))-m)=" << std::exp(H(S, 2) - m) << std::endl;
    Rcpp::Rcout << "(std::exp(H(S,3))-m)=" << std::exp(H(S, 3) - m) << std::endl;
    Rcpp::Rcout << "(std::exp(H(S,4))-m)=" << std::exp(H(S, 4) - m) << std::endl;
    Rcpp::Rcout << "(std::exp(H(S,5))-m)=" << std::exp(H(S, 5) - m) << std::endl;
  }
  if (R_IsNaN((pr_A)))
  {

    Rcpp::Rcout << "exp_sum=" << exp_sum << std::endl;
    Rcpp::Rcout << "numerator=" << (std::exp(H(S, A) - m)) << std::endl;
    Rcpp::Rcout << "pr_A=" << pr_A << std::endl;
    Rcpp::Rcout << H << std::endl;
    Rcpp::Rcout << "A=" << A << ", S=" << S << ", m=" << m << std::endl;
    Rcpp::Rcout << "Numerator=" << std::exp(H(S, A) - m) << std::endl;
    Rcpp::Rcout << "(std::exp(H(S,0))-m)=" << std::exp(H(S, 0) - m) << std::endl;
    Rcpp::Rcout << "(std::exp(H(S,1))-m)=" << std::exp(H(S, 1) - m) << std::endl;
    Rcpp::Rcout << "(std::exp(H(S,2))-m)=" << std::exp(H(S, 2) - m) << std::endl;
    Rcpp::Rcout << "(std::exp(H(S,3))-m)=" << std::exp(H(S, 3) - m) << std::endl;
    Rcpp::Rcout << "(std::exp(H(S,4))-m)=" << std::exp(H(S, 4) - m) << std::endl;
    Rcpp::Rcout << "(std::exp(H(S,5))-m)=" << std::exp(H(S, 5) - m) << std::endl;
    //stop("logProb is NAN");
  }

  // if(Rcpp::traits::is_infinite<REALSXP>(prob_a)){
  //   Rcpp::Rcout <<  "Prb of action seclection = 1" << std::endl;;
  // }
  //Rcpp::Rcout <<  "pr_A="<< pr_A<<std::endl;
  return (pr_A);
}

inline double episolonGreedyProb(int A, int S, arma::mat H, double epsilon)
{
  //Rcpp::Rcout <<  "S="<< S<<std::endl;
  arma::rowvec v = H.row(S);
  //Rcpp::Rcout <<  v<< std::endl;
  double m = arma::max(v);
  double pr_A = 0;
  if (m == H(S, A))
  {
    pr_A = 1 - epsilon + epsilon / 6;
  }
  else
  {
    pr_A = 1 / 6;
  }

  return (pr_A);
}

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

int softmax_action_sel(arma::mat H, int S)
{

  // Rcpp::Rcout <<"S="<<S<<", H="<<H<<std::endl;
  arma::rowvec v = H.row(S);
  double m = arma::max(v);

  //Rcpp::Rcout <<"m="<<m<<", v="<<v<<std::endl;

  v = exp(v - m);
  double exp_sum = arma::accu(v);
  v = v / exp_sum;
  IntegerVector actions = seq(0, 5);
  int action_selected = Rcpp::RcppArmadillo::sample(actions, 1, true, v.as_col())[0];
  //Rcpp::Rcout <<"action_selected="<<action_selected<<std::endl;

  return (action_selected);
}

int epsGreedyActionSel(arma::mat H, int S, double epsilon)
{

  // Rcpp::Rcout <<"S="<<S<<", H="<<H<<std::endl;
  arma::rowvec v = H.row(S);
  //Rcpp::Rcout <<"m="<<m<<", v="<<v<<std::endl;

  arma::uword max_idx = v.index_max();
  arma::vec probVector(6);
  probVector.fill(epsilon / 6);
  probVector(max_idx) = epsilon / 6 + 1 - epsilon;
  IntegerVector actions = seq(0, 5);
  int action_selected = Rcpp::RcppArmadillo::sample(actions, 1, true, probVector)[0];
  //Rcpp::Rcout <<"action_selected="<<action_selected<<std::endl;

  return (action_selected);
}

int actionSelection(arma::mat H, int S, int method, double epsilon = 0)
{
  int selected_action = -1;
  if (method == 1)
  {
    selected_action = softmax_action_sel(H, S);
  }
  else if (method == 2)
  {
    selected_action = epsGreedyActionSel(H, S, epsilon);
  }
  return (selected_action);
}

double actionProb(int A, int S, arma::mat H, int method, double epsilon = 0)
{
  double actionProb = -1;
  if (method == 1)
  {
    actionProb = softmax_cpp3(A, S, H);
  }
  else if (method == 2)
  {
    actionProb = episolonGreedyProb(A, S, H, epsilon);
  }
  return (actionProb);
}

double getPathIndex(arma::mat allpaths, int action, int state, int sessId, unsigned int pathCount)
{
  arma::vec sessionVec = allpaths.col(4);
  arma::uvec sessionIdx = arma::find(sessionVec == (sessId));
  arma::vec allpath_actions = allpaths.col(0);
  arma::vec allpath_states = allpaths.col(1);
  arma::vec pathNb = allpaths.col(5);
  arma::vec actions_sess = allpath_actions.elem(sessionIdx);
  arma::vec states_sess = allpath_states.elem(sessionIdx);
  arma::vec pathNb_sess = pathNb.elem(sessionIdx);

  arma::uvec act_idx = arma::find(actions_sess == (action + 1) && states_sess == (state + 1));

  //Rcpp::Rcout << "pathCount=" << pathCount << std::endl;
  //Rcpp::Rcout << "act_idx=" << act_idx << std::endl;
  double pathIndex = -1;

  if (act_idx.n_elem == 0)
  {
    arma::uvec all_acts_idx = arma::find(allpath_actions == (action + 1) && allpath_states == (state + 1));
    arma::mat X = allpaths.rows(all_acts_idx);
    arma::uvec idx_after_sess = arma::find(X.col(4) > sessId);
    arma::uvec idx_before_sess = arma::find(X.col(4) < sessId);
    //Rcpp::Rcout <<"idx_after_sess=" <<idx_after_sess << std::endl;
    if (!idx_after_sess.empty())
    {
      arma::mat X_after = X.rows(idx_after_sess);
      pathIndex = X_after(0, 5);
      //Rcpp::Rcout <<"pathIndex=" <<pathIndex << ", X_after=" << X_after << std::endl;
    }
    else if (!idx_before_sess.empty())
    {
      arma::mat X_before = X.rows(idx_before_sess);
      pathIndex = X_before(0, 5);
      //Rcpp::Rcout << "X_before=" << X_before << std::endl;
    }
    else
    {
      Rcpp::Rcout << "Action=" << action << ", state=" << state << ", is not found" << std::endl;
    }
  }
  else if (act_idx.n_elem >= pathCount)
  {
    pathIndex = pathNb_sess(act_idx(pathCount - 1));
  }
  else if (act_idx.n_elem <= pathCount)
  {
    pathIndex = pathNb_sess(act_idx(act_idx.n_elem - 1));
  }

  arma::uvec rowid = arma::find(pathNb == pathIndex);
  arma::rowvec row = allpaths.row(rowid(0));
  //Rcpp::Rcout << "row=" << row << std::endl;

  return (pathIndex);
}

//allpaths, A,S, sessId, pathCount
double getPathTime(arma::mat allpaths, int action, int state, int sessId, unsigned int pathCount)
{
  arma::vec sessionVec = allpaths.col(4);
  arma::uvec sessionIdx = arma::find(sessionVec == (sessId));
  //Rcpp::Rcout << "Action=" << action << ", state=" << state << ", sessId=" << sessId << std::endl;

  arma::vec allpath_actions = allpaths.col(0);
  arma::vec allpath_states = allpaths.col(1);
  arma::vec allpath_times = allpaths.col(3);

  //Rcpp::Rcout << "sessionIdx=" << sessionIdx << std::endl;
  arma::vec actions_sess = allpath_actions.elem(sessionIdx);
  arma::vec states_sess = allpath_states.elem(sessionIdx);
  arma::vec trialTimes_sess = allpath_times.elem(sessionIdx);

  //Rcpp::Rcout << "pathCount=" << pathCount << std::endl;
  arma::uvec act_idx = arma::find(actions_sess == (action + 1) && states_sess == (state + 1));
  //Rcpp::Rcout << "act_idx.n_elem=" << act_idx.n_elem << std::endl;
  double pathTime = 0;

  if (act_idx.n_elem == 0)
  {
    arma::uvec all_acts_idx = arma::find(allpath_actions == (action + 1) && allpath_states == (state + 1));
    //Rcpp::Rcout << "all_acts_idx=" << all_acts_idx << std::endl;
    arma::mat X = allpaths.rows(all_acts_idx);
    //Rcpp::Rcout << "X=" << X << std::endl;
    arma::uvec idx_after_sess = arma::find(X.col(4) > sessId);
    arma::uvec idx_before_sess = arma::find(X.col(4) < sessId);
    //Rcpp::Rcout <<"idx_after_sess=" <<idx_after_sess.empty() << ", idx_before_sess=" <<idx_before_sess.empty()<< std::endl;
    if (!idx_after_sess.empty())
    {
      arma::mat X_after = allpaths.rows(idx_after_sess);
      pathTime = X_after(0, 3);
      //Rcpp::Rcout <<"pathTime=" <<pathTime << ", X_after=" << X_after << std::endl;
    }
    else if (!idx_before_sess.empty())
    {
      arma::mat X_before = allpaths.rows(idx_before_sess);
      pathTime = X_before(0, 3);
      //Rcpp::Rcout <<"pathTime=" <<pathTime << ", X_before=" << X_before << std::endl;
    }
    else
    {
      Rcpp::Rcout << "Action=" << action << ", state=" << state << ", is not found" << std::endl;
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

//updateTurnTime(turnTimes, pathIndex, generated_TurnsData_sess, turns_index, turnMethod);
arma::mat updateTurnTime(arma::mat turnTimes, int allpaths_idx, arma::mat generated_TurnsData_sess, Rcpp::IntegerVector generatedTurnIds, int turnMethod)
{
  arma::vec pathIds = turnTimes.col(0);
  arma::uvec turnTimes_idx = arma::find(pathIds == allpaths_idx);
  arma::vec turns = turnTimes.col(3);
  arma::vec turn_states = turnTimes.col(2);
  arma::vec turnTime_method;

  //Rcpp::Rcout << "allpaths_idx=" << allpaths_idx << std::endl;
  //Rcpp::Rcout << "turnTimes_idx=" << turnTimes_idx << std::endl;
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

  arma::vec turns_currPath = turns.elem(turnTimes_idx);
  arma::vec turnTime_currPath = turnTime_method.elem(turnTimes_idx);

 

  for (unsigned int i = 0; i < generatedTurnIds.length(); i++)
  {
    //Rcpp::Rcout << "i=" << i << ", generatedTurnIds=" << generatedTurnIds[i] << std::endl;
    std::string turnName = getTurnString(generated_TurnsData_sess(generatedTurnIds[i], 0));
    int curr_turn = generated_TurnsData_sess(generatedTurnIds[i], 0);
    //Rcpp::Rcout << "turn to update=" << turnName << std::endl;

    for (unsigned int j = 0; j < turns_currPath.n_elem; j++)
    {
      std::string turnName = getTurnString(turns_currPath(j));
      //Rcpp::Rcout << "turns from rat data=" << turns_currPath(j) << std::endl;

      if (turns_currPath(j) == curr_turn)
      {
        
        //Rcpp::Rcout << "Turn=" << turnName << ", index=" <<  generatedTurnIds[i] << ", turntime=" <<turnTime_currPath(j) << std::endl;
        generated_TurnsData_sess(generatedTurnIds[i], 3) = turnTime_currPath(j);
        //Rcpp::Rcout << "turnTime_currPath=" << turnTime_currPath(j) << std::endl;
        break;
      }
    }
  }

  return (generated_TurnsData_sess);
}

// [[Rcpp::export()]]
Rcpp::List simulateTrials(arma::mat allpaths, arma::mat turnTimes, double alpha, double gamma1, int turnMethod)
{

  arma::mat H = arma::zeros(2,6);
  arma::mat R = arma::zeros(2,6);
  R(0, 3) = 1;
  R(1, 3) = 1;
  arma::mat generated_PathData;
  arma::mat generated_TurnData;
  
  arma::vec allpath_actions = allpaths.col(0);
  arma::vec allpath_states = allpaths.col(1);
  arma::vec allpath_rewards = allpaths.col(2);
  arma::vec allpath_times = allpaths.col(3);
  arma::vec sessionVec = allpaths.col(4);
  arma::vec uniqSessIdx = arma::unique(sessionVec);
  //Rcpp::Rcout << "sessionVec=" << sessionVec << std::endl;
  //Rcpp::Rcout << "uniqSessIdx=" << uniqSessIdx << std::endl;
  int episode = 1;
  int score_episode = 0;
  int actionNb=0;

  // Loop through each session
  for (unsigned int session = 0; session < (uniqSessIdx.n_elem); session++)
  {

    int sessId = uniqSessIdx(session);
    arma::uvec sessionIdx = arma::find(sessionVec == sessId);
    arma::vec actions_sess = allpath_actions.elem(sessionIdx);
    arma::vec states_sess = allpath_states.elem(sessionIdx);
    arma::vec rewards_sess = allpath_rewards.elem(sessionIdx);
    arma::vec time_taken_for_trial_sess = allpath_times.elem(sessionIdx);

    //Rcpp::Rcout << "session=" <<session <<", sessId=" << sessId << std::endl;

    int initState = 0;
    bool changeState = false;
    bool returnToInitState = false;
    bool resetVector = true;
    int nrow = actions_sess.n_rows;
    int A = 0;
    int turnIdx = 0;
    //int A_prime=0;
    std::vector<int> episodeActions;
    std::vector<int> episodeStates;
    std::vector<double> episodePathTimes;


    int S = states_sess(0) - 1;
    arma::mat generated_PathData_sess(nrow, 5);
    generated_PathData_sess.fill(-1);
    arma::mat generated_TurnsData_sess((nrow * 2), 6);
    generated_TurnsData_sess.fill(-1);

    //All episodes in new session
    for (int act = 0; act < (nrow); act++)
    {
      actionNb++;
      // Rcpp::Rcout << "episode=" << i  << std::endl;
      if (resetVector)
      {
        initState = S;
        //Rcpp::Rcout <<"initState="<<initState<<std::endl;
        resetVector = false;
      }

      A = softmax_action_sel(H,S);
      
      //Rcpp::Rcout << "S=" << S << ", A=" << A << ", episode=" << episode <<std::endl;

      score_episode = score_episode + R(S, A);
      generated_PathData_sess(act, 0) = A;
      generated_PathData_sess(act, 1) = S;
      generated_PathData_sess(act, 2) = R(S, A);
      generated_PathData_sess(act, 3) = 0;
      generated_PathData_sess(act, 4) = sessId;

      Rcpp::StringVector turns = getTurnsFromPaths(A, S);
      int nbOfTurns = turns.length();
      //Rcpp::Rcout <<"Path="<< A << ", nbOfTurns=" << nbOfTurns<<std::endl;
      Rcpp::IntegerVector turns_index;
      for (int j = 0; j < nbOfTurns; j++)
      {
        std::string currTurn = Rcpp::as<std::string>(turns(j));
        int turnNb = getTurnIdx(currTurn, S);
        generated_TurnsData_sess(turnIdx, 0) = turnNb;
        generated_TurnsData_sess(turnIdx, 1) = S;
        generated_TurnsData_sess(turnIdx, 2) = 0;
        generated_TurnsData_sess(turnIdx, 4) = sessId;
        generated_TurnsData_sess(turnIdx, 5) = actionNb;
        turns_index.push_back(turnIdx);
        turnIdx++;
      }

      if (R(S, A) == 1)
      {
        generated_TurnsData_sess((turnIdx - 1), 2) = 1;
      }

      episodeActions.push_back(A);
      episodeStates.push_back(S);
      
      if (A == 5)
      {
        episodePathTimes.push_back(0);  
      }
      else
      {
        arma::uvec act_idx = arma::find(generated_PathData_sess.col(0) == A && generated_PathData_sess.col(1) == S);
        int pathCount = act_idx.n_elem;
        double pathTime = getPathTime(allpaths, A, S, sessId, pathCount);
        episodePathTimes.push_back(pathTime);  
        generated_PathData_sess(act, 3) = pathTime;
        int allpaths_idx = getPathIndex(allpaths, A, S, sessId, pathCount);
        //Rcpp::Rcout << "Update turn times for turns=" << turnNames << ", A=" <<A << ", S=" << S << std::endl;
        generated_TurnsData_sess = updateTurnTime(turnTimes, allpaths_idx, generated_TurnsData_sess, turns_index, turnMethod);
      }
     
      int S_prime = aca_getNextState(S, A);

      if (S_prime != initState)
      {
        changeState = true;
      }
      else if (S_prime == initState && changeState)
      {
        returnToInitState = true;
      }

      //Rcpp::Rcout << "logProb=" << logProb <<std::endl;
      //log_lik=log_lik+ logProb;

      //Check if episode ended
      if (returnToInitState)
      {
        //Rcpp::Rcout <<  "Inside end episode"<<std::endl;
        changeState = false;
        returnToInitState = false;

        arma::vec actions = arma::conv_to<arma::vec>::from(episodeActions);
        arma::vec states = arma::conv_to<arma::vec>::from(episodeStates);
        arma::vec time_taken_for_trial = arma::conv_to<arma::vec>::from(episodePathTimes);

        episode = episode + 1;
        resetVector = true;

        H = Aca3CreditUpdate(H, actions, states, time_taken_for_trial, alpha, score_episode);

        score_episode = 0;
        episodeActions.clear();
        episodeStates.clear();
        episodePathTimes.clear();
      }

      H = gamma1 * H;
      S = S_prime;
      //trial=trial+1;
    }
    // if (turnIdx < (nrow * 2) - 1)
    // {
    //   generated_TurnsData_sess.shed_rows((turnIdx), ((nrow * 2) - 1));
    // }
    generated_TurnData = arma::join_cols(generated_TurnData, generated_TurnsData_sess.rows(0,(turnIdx-1)));
    //Rcpp::Rcout <<  "H after session=" << H<<std::endl;
    generated_PathData = arma::join_cols(generated_PathData, generated_PathData_sess);
    //Rcpp::Rcout <<  "likelihoodVec=" << likelihoodVec<<std::endl;
  }
  
    //Rcpp::Rcout <<  "likelihoodVec=" << likelihoodVec<<std::endl;
  
  return (Rcpp::List::create(Named("PathData") = generated_PathData, _["TurnData"] = generated_TurnData));
}



// [[Rcpp::export()]]
arma::vec getPathLikelihood(arma::mat allpaths, double alpha, double gamma1, arma::mat H, int sim, int model, int policyMethod, double epsilon = 0, int endTrial = 0)
{

  if (sim != 1)
  {
    arma::mat v = arma::zeros(allpaths.n_rows, 1);
    allpaths = arma::join_horiz(allpaths, v);
  }
  //Rcpp::Rcout <<  "allpaths.col(4)="<<allpaths.col(4) <<std::endl;

  arma::vec likelihoodVec;

  arma::vec allpath_actions = allpaths.col(0);
  arma::vec allpath_states = allpaths.col(1);
  arma::vec allpath_rewards = allpaths.col(2);
  arma::vec allpath_times = allpaths.col(3);
  arma::vec sessionVec = allpaths.col(4);
  arma::vec uniqSessIdx = arma::unique(sessionVec);
  //Rcpp::Rcout << "sessionVec=" << sessionVec << std::endl;
  //Rcpp::Rcout << "uniqSessIdx=" << uniqSessIdx << std::endl;
  int episode = 1;
  int score_episode = 0;
  float avg_score = 0;

  // Loop through each session
  for (unsigned int session = 0; session < (uniqSessIdx.n_elem); session++)
  {

    int sessId = uniqSessIdx(session);
    //Rcpp::Rcout << "session=" <<session <<", sessId=" << sessId << std::endl;
    arma::uvec sessionIdx = arma::find(allpaths.col(4) == (sessId));
    arma::vec actions_sess = allpath_actions.elem(sessionIdx);
    arma::vec states_sess = allpath_states.elem(sessionIdx);
    arma::vec rewards_sess = allpath_rewards.elem(sessionIdx);
    arma::vec time_taken_for_trial_sess = allpath_times.elem(sessionIdx);

    int initState = 0;
    bool changeState = false;
    bool returnToInitState = false;
    bool resetVector = true;
    int nrow = actions_sess.n_rows;
    int A = 0;
    //int A_prime=0;
    

    int S = 0;
    if (sim == 1)
    {
      S = states_sess(0);
    }
    else
    {
      S = states_sess(0) - 1;
    }

    arma::vec likelihoodVec_sess(nrow - 1);
    arma::vec episodeVec(nrow - 1);
    //All episodes in new session
    for (int i = 0; i < (nrow - 1); i++)
    {

      //Rcpp::Rcout << "S=" << S << ", A=" << A << ", episode=" << episode <<std::endl;
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

      episodeVec(i) = episode;

      if (S_prime != initState)
      {
        changeState = true;
      }
      else if (S_prime == initState && changeState)
      {
        returnToInitState = true;
      }

      double prob_a = 0;
      if (policyMethod == 1)
      {
        prob_a = actionProb(A, S, H, 1, 0);
      }
      else if (policyMethod == 2)
      {
        if (i > endTrial)
        {
          epsilon = 1;
        }
        prob_a = actionProb(A, S, H, 2, epsilon);
      }

      double logProb = log(prob_a);

      likelihoodVec_sess(i) = logProb;
      //Rcpp::Rcout << "logProb=" << logProb <<std::endl;
      //log_lik=log_lik+ logProb;

      //Check if episode ended
      if (returnToInitState)
      {
        //Rcpp::Rcout <<  "Inside end episode"<<std::endl;
        changeState = false;
        returnToInitState = false;

        arma::uvec episodeIdx = arma::find(episodeVec == episode);
        arma::vec actions = actions_sess.elem(episodeIdx);
        arma::vec states = states_sess.elem(episodeIdx);
        arma::vec time_taken_for_trial = time_taken_for_trial_sess.elem(episodeIdx);

        if (sim != 1)
        {
          actions = actions - 1;
          states = states - 1;
        }

        episode = episode + 1;
        resetVector = true;
        avg_score = (avg_score * (episode - 1) + (score_episode - avg_score)) / episode;

        // Rcpp::Rcout << "episodeIdx=" << episodeIdx<<std::endl;
        // Rcpp::Rcout << "actions=" << actions<<std::endl;
        // Rcpp::Rcout << "states=" << states<<std::endl;
        // Rcpp::Rcout << "time_taken_for_trial=" << time_taken_for_trial<<std::endl;
        //Rcpp::Rcout << "activityMatrix=" <<activityMatrix<<std::endl;
        H = Aca3CreditUpdate(H, actions, states, time_taken_for_trial, alpha, score_episode);
        //Rcpp::Rcout <<  "H after episode=" << H << std::endl;

        //H = updateCreditMatrix(H,actions, states, time_taken_for_trial, alpha, score_episode, avg_score,model);
        score_episode = 0;
      }

      H = gamma1 * H;
      S = S_prime;
      //trial=trial+1;
    }

    //Rcpp::Rcout <<  "H after session=" << H<<std::endl;
    likelihoodVec = arma::join_cols(likelihoodVec, likelihoodVec_sess);
    //Rcpp::Rcout <<  "likelihoodVec=" << likelihoodVec<<std::endl;
  }
  return (likelihoodVec);
}

// [[Rcpp::export()]]
arma::mat getProbMatrix(arma::mat allpaths, double alpha, double gamma1, arma::mat H, int sim, int model, int policyMethod, double epsilon = 0)
{

  if (sim != 1)
  {
    arma::mat v = arma::zeros(allpaths.n_rows, 1);
    allpaths = arma::join_horiz(allpaths, v);
  }
  //Rcpp::Rcout <<  "allpaths.col(4)="<<allpaths.col(4) <<std::endl;

  arma::mat probMatrix_aca;

  arma::vec allpath_actions = allpaths.col(0);
  arma::vec allpath_states = allpaths.col(1);
  arma::vec allpath_rewards = allpaths.col(2);
  arma::vec allpath_times = allpaths.col(3);
  arma::vec sessionVec = allpaths.col(4);
  arma::vec uniqSessIdx = arma::unique(sessionVec);
  arma::vec pathIdx = allpaths.col(5);
  //Rcpp::Rcout << "sessionVec=" << sessionVec << std::endl;
  //Rcpp::Rcout << "uniqSessIdx=" << uniqSessIdx << std::endl;
  int episode = 1;
  int score_episode = 0;
  float avg_score = 0;


  // Loop through each session
  for (unsigned int session = 0; session < (uniqSessIdx.n_elem); session++)
  {

    int sessId = uniqSessIdx(session);
    //Rcpp::Rcout << "session=" <<session <<", sessId=" << sessId << std::endl;
    arma::uvec sessionIdx = arma::find(allpaths.col(4) == (sessId));
    arma::vec actions_sess = allpath_actions.elem(sessionIdx);
    arma::vec states_sess = allpath_states.elem(sessionIdx);
    arma::vec rewards_sess = allpath_rewards.elem(sessionIdx);
    arma::vec time_taken_for_trial_sess = allpath_times.elem(sessionIdx);
    arma::vec pathIdx_sess = pathIdx.elem(sessionIdx);

    int initState = 0;
    bool changeState = false;
    bool returnToInitState = false;
    bool resetVector = true;
    int nrow = actions_sess.n_rows;
    //int A = 0;
    //int A_prime=0;

    int S = 0;
    if (sim == 1)
    {
      S = states_sess(0);
    }
    else
    {
      S = states_sess(0) - 1;
    }

    arma::mat probMatrix_sess((nrow - 1), 13);
    arma::vec episodeVec(nrow - 1);

    //All episodes in new session
    for (int i = 0; i < (nrow - 1); i++)
    {

      //Rcpp::Rcout << "S=" << S << ", A=" << A << ", episode=" << episode <<std::endl;
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
      if (sim == 1)
      {
        S_prime = states_sess(i + 1);
      }
      else
      {
        S_prime = states_sess(i + 1) - 1;
      }

      episodeVec(i) = episode;

      if (S_prime != initState)
      {
        changeState = true;
      }
      else if (S_prime == initState && changeState)
      {
        returnToInitState = true;
      }

      probMatrix_sess(i,12) = pathIdx_sess(i);
      if (S == 0)
      {
        probMatrix_sess.submat(i, 6, i, 11) = arma::zeros(1, 6);
        for (int act = 0; act < 6; act++)
        {
          double x = 0;
          if (policyMethod == 1)
          {
            x = actionProb(act, 0, H, 1);
          }
          else if (policyMethod == 2)
          {
            x = actionProb(act, 0, H, 2, epsilon);
          }
          probMatrix_sess(i, act) = x;
          //Rcpp::Rcout <<"S=0" << ", i="<< i << ", x=" << x << std::endl;
        }
      }
      else if (S == 1)
      {
        //Rcpp::Rcout << "i=" <<i<< std::endl;
        probMatrix_sess.submat(i, 0, i, 5) = arma::zeros(1, 6);
        //Rcpp::Rcout << "probMatrix_aca="<< probMatrix_aca << std::endl;
        for (int act = 0; act < 6; act++)
        {
          double x = 0;
          if (policyMethod == 1)
          {
            x = actionProb(act, 1, H, 1);
          }
          else if (policyMethod == 2)
          {
            x = actionProb(act, 1, H, 2, epsilon);
          }
          probMatrix_sess(i, (6 + act)) = x;
          //Rcpp::Rcout << "S=1" << ", i="<< i << ", x=" << x << std::endl;
        }
      }

      //Check if episode ended
      if (returnToInitState)
      {
        //Rcpp::Rcout <<  "Inside end episode"<<std::endl;
        changeState = false;
        returnToInitState = false;

        arma::uvec episodeIdx = arma::find(episodeVec == episode);
        arma::vec actions = actions_sess.elem(episodeIdx);
        arma::vec states = states_sess.elem(episodeIdx);
        arma::vec time_taken_for_trial = time_taken_for_trial_sess.elem(episodeIdx);

        if (sim != 1)
        {
          actions = actions - 1;
          states = states - 1;
        }

        episode = episode + 1;
        resetVector = true;
        avg_score = (avg_score * (episode - 1) + (score_episode - avg_score)) / episode;

        //Rcpp::Rcout << "i=" <<i<< ", episode="<<episode<<std::endl;
        //Rcpp::Rcout << "activityMatrix=" <<activityMatrix<<std::endl;
        H = Aca3CreditUpdate(H, actions, states, time_taken_for_trial, alpha, score_episode);
        //H = updateCreditMatrix(H,actions, states, time_taken_for_trial, alpha, score_episode, avg_score, model);
        score_episode = 0;
        H = gamma1 * H;
      }

      S = S_prime;
      //trial=trial+1;
    }

    probMatrix_aca = arma::join_cols(probMatrix_aca, probMatrix_sess);
    //Rcpp::Rcout <<  "likelihoodVec=" << likelihoodVec<<std::endl;
  }
  return (probMatrix_aca);
}