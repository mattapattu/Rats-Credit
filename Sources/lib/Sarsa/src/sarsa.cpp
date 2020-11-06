// [[Rcpp::depends(RcppArmadillo)]]
#include <vector>
#include <RcppArmadillo.h>

#include <regex>
#include <RcppArmadilloExtensions/sample.h>

using namespace Rcpp;

inline double softmaxProb(int A, int S, arma::mat H)
{
  //Rcpp::Rcout <<  "S="<< S<<std::endl;
  arma::rowvec v = H.row(S);
  //Rcpp::Rcout <<  v<< std::endl;
  double m = arma::max(v);
  double exp_sum = std::exp(H(S, 0) - m) + std::exp(H(S, 1) - m) + std::exp(H(S, 2) - m) + std::exp(H(S, 3) - m) + std::exp(H(S, 4) - m) + std::exp(H(S, 5) - m);
  double pr_A = (std::exp(H(S, A) - m)) / exp_sum;

  return (pr_A);
}

inline double epsilonGreedyProb(int A, int S, arma::mat H, double epsilon)
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

double actionProb(int A, int S, arma::mat H, int method, double epsilon = 0)
{
  double actionProb = -1;
  if (method == 1)
  {
    actionProb = softmaxProb(A, S, H);
  }
  else if (method == 2)
  {
    actionProb = epsilonGreedyProb(A, S, H, epsilon);
  }
  return (actionProb);
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

// [[Rcpp::export()]]
arma::mat simulateSarsa(arma::mat allpaths, arma::mat Q, double alpha, double gamma, double lambda, int model, int policyMethod, double epsilon = 0)
{

  arma::mat R = arma::zeros(2, 6);
  R(0, 3) = 1;
  R(1, 3) = 1;
  arma::mat generated_data;

  arma::vec allpath_actions = allpaths.col(0);
  arma::vec allpath_states = allpaths.col(1);
  arma::vec allpath_rewards = allpaths.col(2);
  arma::vec allpath_times = allpaths.col(3);
  arma::vec sessionVec = allpaths.col(4);
  arma::vec uniqSessIdx = arma::unique(sessionVec);
  //Rcpp::Rcout << "sessionVec=" << sessionVec << std::endl;
  //Rcpp::Rcout << "uniqSessIdx=" << uniqSessIdx << std::endl;
  int episode = 1;

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

    int score_episode = 0;
    int S = states_sess(0) - 1;
    int A = 0;
    if (policyMethod == 1)
    {
      A = actionSelection(Q, S, 1);
    }
    else if (policyMethod == 2)
    {
      A = actionSelection(Q, S, 2, epsilon);
    }
    arma::mat etrace(2, 6, arma::fill::zeros);
    arma::mat generated_data_sess(nrow, 5);
    //All episodes in new session
    for (int i = 0; i < (nrow); i++)
    {
      //Rcpp::Rcout << "episode=" << i  << std::endl;
      if (resetVector)
      {
        initState = S;
        //Rcpp::Rcout <<"initState="<<initState<<std::endl;
        resetVector = false;
      }

      //Rcpp::Rcout << "S=" << S << ", A=" << A << ", episode=" << episode <<std::endl;

      score_episode = score_episode + R(S, A);
      generated_data_sess(i, 0) = A;
      generated_data_sess(i, 1) = S;
      generated_data_sess(i, 2) = R(S, A);

      arma::uvec act_idx = arma::find(generated_data_sess.col(1) == S && generated_data_sess.col(0) == A);
      arma::uvec sess_act_idx = arma::find(states_sess == (S + 1) && actions_sess == (A + 1));

      if (sess_act_idx.n_elem == 0)
      {
        //Rcpp::Rcout << "Here2" <<std::endl;
        arma::uvec act_idx = arma::find(allpath_states == (S + 1) && allpath_actions == (A + 1));
        arma::vec act_times = allpath_times.elem(act_idx);
        //Rcpp::Rcout << "act_idx=" << act_idx << ", act_times=" << act_times <<std::endl;
        generated_data_sess(i, 3) = arma::mean(act_times);
        // Rcpp::Rcout << "generated_data_sess(i,3)=" << generated_data_sess(i,3) <<std::endl;
      }
      else
      {
        //Rcpp::Rcout << "Here3" <<std::endl;
        //arma::uword last_act = act_idx(act_idx.n_elem-1);
        arma::vec act_times = time_taken_for_trial_sess.elem(sess_act_idx);
        // Rcpp::Rcout << "last_act=" << last_act <<std::endl;
        generated_data_sess(i, 3) = arma::mean(act_times);
      }

      //generated_data_sess(i,4) = episode;
      generated_data_sess(i, 4) = session + 1;

      int S_prime = aca_getNextState(S, A);
      int A_prime = 0;
      if (policyMethod == 1)
      {
        A_prime = actionSelection(Q, S_prime, 1);
      }
      else if (policyMethod == 2)
      {
        A_prime = actionSelection(Q, S_prime, 2, epsilon);
      }

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

      if (returnToInitState)
      {
        //Rcpp::Rcout <<  "Inside end episode"<<std::endl;
        changeState = false;
        returnToInitState = false;
        episode = episode + 1;
        resetVector = true;
      }

      double prediction = gamma * Q(S_prime, A_prime);
      double td_err = R(S, A) + prediction - Q(S, A);

      etrace(S, A) = etrace(S, A) + 1;

      for (int state = 0; state < 2; state++)
      {
        for (int act = 0; act < 5; act++)
        {
          Q(state, act) = Q(state, act) + (alpha * td_err * etrace(state, act));
          etrace(state, act) = lambda * gamma * etrace(state, act);
        }
      }
      S = S_prime;
      A = A_prime;
      //trial=trial+1;
    }

    //Rcpp::Rcout <<  "H after session=" << H<<std::endl;
    generated_data = arma::join_cols(generated_data, generated_data_sess);
    //Rcpp::Rcout <<  "likelihoodVec=" << likelihoodVec<<std::endl;
  }
  return (generated_data);
}

// [[Rcpp::export()]]
arma::vec getPathLikelihood(arma::mat allpaths, double alpha, double gamma, double lambda, arma::mat Q, int sim, int policyMethod, double epsilon = 0)
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

    arma::vec likelihoodVec_sess(nrow - 1);
    arma::mat etrace(2, 6, arma::fill::zeros);
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

      int S_prime = 0;
      int A_prime = 0;

      if (sim == 1)
      {
        S_prime = states_sess(i + 1);
        A_prime = actions_sess(i + 1);
      }
      else
      {
        S_prime = states_sess(i + 1) - 1;
        A_prime = actions_sess(i + 1) - 1;
      }

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
        prob_a = actionProb(A, S, Q, 1);
      }
      else if (policyMethod == 2)
      {
        prob_a = actionProb(A, S, Q, 2, epsilon);
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
        episode = episode + 1;
        resetVector = true;
      }
      // else{
      //   //Rcpp::Rcout << "S_prime=" << S_prime << ", S_prime=" << S_prime <<std::endl;
      //   arma::rowvec v = Q.row(S_prime);
      //   arma::uword idx = v.index_max();
      //   prediction = gamma*Q(S_prime, idx);
      // }

      double prediction = gamma * Q(S_prime, A_prime);
      double td_err = R + prediction - Q(S, A);

      //Rcpp::Rcout <<  "prediction=" << prediction<<std::endl;
      // Rcpp::Rcout <<  "S=" << S << ", A =" << A << ", Q(S,A) = " << Q(S,A) <<std::endl;
      // Rcpp::Rcout <<  "S_prime = " << S_prime << ", A_prime = " << A_prime  << ", Q(S',A') = " << Q(S_prime, A_prime) <<std::endl;
      // Rcpp::Rcout <<  "R = " << R << ", td_err = " << td_err <<std::endl;

      etrace(S, A) = etrace(S, A) + 1;

      for (int state = 0; state < 2; state++)
      {
        for (int act = 0; act < 5; act++)
        {
          Q(state, act) = Q(state, act) + (alpha * td_err * etrace(state, act));
          etrace(state, act) = lambda * gamma * etrace(state, act);
        }
      }

      S = S_prime;
      A = A_prime;
      //trial=trial+1;
    }
    likelihoodVec = arma::join_cols(likelihoodVec, likelihoodVec_sess);
    //Rcpp::Rcout <<  "likelihoodVec=" << likelihoodVec<<std::endl;
  }
  return (likelihoodVec);
}

// [[Rcpp::export()]]
arma::mat getProbMatrix(arma::mat allpaths, double alpha, double gamma, double lambda, arma::mat Q, int sim, int policyMethod, double epsilon = 0)
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

  int episode = 1;

  // Loop through each session
  for (unsigned int session = 0; session < (uniqSessIdx.n_elem); session++)
  {

    int sessId = uniqSessIdx(session);
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

    arma::mat probMatrix_sess((nrow - 1), 12);
    arma::mat etrace(2, 6, arma::fill::zeros);
    //All episode in one session
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
        S_prime = states_sess(i + 1);
        A_prime = actions_sess(i + 1);
      }
      else
      {
        S_prime = states_sess(i + 1) - 1;
        A_prime = actions_sess(i + 1) - 1;
      }

      if (S_prime != initState)
      {
        changeState = true;
      }
      else if (S_prime == initState && changeState)
      {
        returnToInitState = true;
      }

      //arma::rowvec new_row = arma::zeros(12);

      if (S == 0)
      {
        probMatrix_sess.submat(i, 6, i, 11) = arma::zeros(1, 6);
        for (int act = 0; act < 6; act++)
        {
          double x = 0;
          if (policyMethod == 1)
          {
            x = actionProb(act, 0, Q, 1);
          }
          else if (policyMethod == 2)
          {
            x = actionProb(act, 0, Q, 2, epsilon);
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
            x = actionProb(act, 1, Q, 1);
          }
          else if (policyMethod == 2)
          {
            x = actionProb(act, 1, Q, 2, epsilon);
          }
          probMatrix_sess(i, (6 + act)) = x;
          //Rcpp::Rcout << "S=1" << ", i="<< i << ", x=" << x << std::endl;
        }
      }

      // probMatrix_aca.insert_rows(probMatrix_aca.n_rows, new_row);
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
      // else{
      //   //Rcpp::Rcout << "S_prime=" << S_prime << ", A_prime=" << A_prime <<std::endl;

      // }

      double prediction = gamma * Q(S_prime, A_prime);
      double td_err = R + prediction - Q(S, A);

      //Rcpp::Rcout <<  "prediction=" << prediction<<std::endl;
      // Rcpp::Rcout <<  "S=" << S << ", A =" << A << ", Q(S,A) = " << Q(S,A) <<std::endl;
      // Rcpp::Rcout <<  "S_prime = " << S_prime << ", A_prime = " << A_prime  << ", Q(S',A') = " << Q(S_prime, A_prime) <<std::endl;
      // Rcpp::Rcout <<  "R = " << R << ", td_err = " << td_err <<std::endl;

      etrace(S, A) = etrace(S, A) + 1;

      for (int state = 0; state < 2; state++)
      {
        for (int act = 0; act < 5; act++)
        {
          Q(state, act) = Q(state, act) + (alpha * td_err * etrace(state, act));
          etrace(state, act) = lambda * gamma * etrace(state, act);
        }
      }

      S = S_prime;
      A = A_prime;
      //trial=trial+1;
    }

    probMatrix_aca = arma::join_cols(probMatrix_aca, probMatrix_sess);
  }
  return (probMatrix_aca);
}
