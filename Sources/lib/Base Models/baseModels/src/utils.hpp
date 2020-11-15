#ifndef __UTILS__
#define __UTILS__

#include <vector>
#include <algorithm>
#include <string>
#include <regex>
#include <RcppArmadillo.h>
#include <RcppArmadilloExtensions/sample.h>

inline double softmax_cpp3(int A, int S, arma::mat H)
{
  //Rcpp::Rcout <<  "softmax_cpp3"<<std::endl;
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
  Rcpp::Rcout << "episolonGreedyProb"
              << ", epsilon=" << epsilon << std::endl;
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
    pr_A = (float)1 / (float)6;
  }

  return (pr_A);
}

inline double softmaxEps(int A, int S, arma::mat H, double epsilon)
{
  //Rcpp::Rcout <<  "softmaxEps" << ", epsilon=" << epsilon<<std::endl;
  arma::rowvec v = H.row(S);
  float m = arma::max(v);
  v = exp((v - m) / epsilon);
  // //Rcpp::Rcout << "m=" << m<< std::endl;
  double exp_sum = arma::accu(v);
  v = v / exp_sum;
  double pr_A = v[A];

  return (pr_A);
}

// [[Rcpp::export()]]
arma::vec getPathTimes(Rcpp::NumericVector allpaths, Rcpp::NumericMatrix enreg_pos)
{

  int nrow = allpaths.size();
  //Rcpp::Rcout <<"nrow="<<nrow<<std::endl;
  arma::colvec y(nrow);
  //arma::mat enreg_arma(enreg_pos.begin(), enreg_pos.nrow(), enreg_pos.ncol(), false);
  arma::mat enreg_arma = Rcpp::as<arma::mat>(enreg_pos);
  arma::vec allpaths_arma = Rcpp::as<arma::vec>(allpaths);
  //Rcpp::Rcout <<enreg_arma<<std::endl;
  int prev_ses = -1;
  int trial = 0;
  for (int i = 0; i < nrow; i++)
  {

    //Rcpp::Rcout <<"trial="<<trial<<", allpaths_arma(i)="<<allpaths_arma(i)<<std::endl;
    if (prev_ses != allpaths_arma(i))
    {
      trial = 1;
      prev_ses = allpaths_arma(i);
    }
    //Rcpp::Rcout <<"i="<<i<<std::endl;
    arma::uvec ids = arma::find((enreg_arma.col(1) == trial) && (enreg_arma.col(2) == allpaths_arma(i)));
    int start_pos_id = ids(0);
    //Rcpp::Rcout <<"ids.size="<<ids.n_elem<<std::endl;

    int max_pos_id = ids(ids.n_elem - 1);
    //Rcpp::Rcout <<"start_pos_id="<<start_pos_id<<", max_pos_id="<<max_pos_id<<std::endl;
    double time_taken_for_trial = (enreg_arma((max_pos_id - 1), 0)) - enreg_arma(start_pos_id, 0);
    if (time_taken_for_trial == 0)
    {
      time_taken_for_trial = 20;
    }
    y(i) = time_taken_for_trial;
    //Rcpp::Rcout <<"trial="<<trial<<", ses="<<allpaths_arma(i)<<", time_taken_for_trial=" <<time_taken_for_trial<<std::endl;
    trial = trial + 1;
  }
  //=arma::join_horiz(allpaths_arma,y);
  return (y);
}

// [[Rcpp::export()]]
arma::mat empiricalProbMat(arma::mat allpaths, int window)
{

  int nrow = allpaths.n_rows;
  arma::mat probMatrix = arma::zeros(nrow, 12);

  for (int i = 0; i < nrow; i++)
  {

    int S = allpaths(i, 1);
    arma::vec states;
    arma::vec actions;
    if (i >= window)
    {
      states = allpaths(arma::span((i - window), i), 1);
      actions = allpaths(arma::span((i - window), i), 0);
    }
    else
    {
      states = allpaths(arma::span(0, i), 1);
      actions = allpaths(arma::span(0, i), 0);
    }

    arma::uvec state_idx = arma::find(states == S);

    arma::vec actions_in_state = actions.elem(state_idx);

    //Rcpp::Rcout <<"act_idx.n_elem="<<act_idx.n_elem<<", s1_idx.n_elem="<<s1_idx.n_elem<<std::endl;

    //Rcpp::Rcout <<"states="<<states<<std::endl;
    if (S == 1)
    {
      probMatrix.submat(i, 6, i, 11) = arma::zeros(1, 6);
      for (int act = 1; act < 7; act++)
      {
        arma::uvec act_idx = arma::find(actions_in_state == act);
        double x = act_idx.n_elem / (1.0 * state_idx.n_elem);
        probMatrix(i, (act - 1)) = x;
      }
    }
    else if (S == 2)
    {
      probMatrix.submat(i, 0, i, 5) = arma::zeros(1, 6);
      for (int act = 1; act < 7; act++)
      {

        arma::uvec act_idx = arma::find(actions_in_state == act);
        double x = act_idx.n_elem / (1.0 * state_idx.n_elem);
        probMatrix(i, (5 + act)) = x;
      }
    }
  }

  return (probMatrix);
}

// [[Rcpp::export()]]
arma::vec mseEmpirical(arma::mat allpaths, arma::mat probMatrix_m1, arma::vec movAvg, int sim)
{

  arma::vec mseMatrix = arma::zeros(allpaths.n_rows);
  //arma::mat movAvg=arma::zeros(allpaths.n_rows);
  //arma::vec allpath_actions = allpaths.col(0);
  //arma::vec allpath_states = allpaths.col(1);

  Rcpp::Rcout << "allpaths.n_rows=" << allpaths.n_rows << std::endl;

  for (unsigned int i = 0; i < (allpaths.n_rows); i++)
  {
    int action = allpaths(i, 0);
    int state = allpaths(i, 1);

    if (sim != 1)
    {
      state = state - 1;
      action = action - 1;
    }

    //mseMatrix(i)=pow(movAvg(i)-probMatrix_m1(i,((6*state)+action)),2);
    mseMatrix(i) = abs(movAvg(i) - probMatrix_m1(i, ((6 * state) + action)));

    //Rcpp::Rcout <<"movAvg(i)="<<movAvg(i)<<", prob=" <<probMatrix_m1(i,((6*state)+action))<<", mseMatrix(i)="<<mseMatrix(i)<<std::endl;
  }
  //Rcpp::Rcout <<"movAvg="<<movAvg<<std::endl;
  return (mseMatrix);
}

// [[Rcpp::export()]]
arma::vec pathProbability(arma::mat allpaths, arma::mat probMatrix_m1, int sim)
{

  arma::vec mseMatrix = arma::zeros(allpaths.n_rows);
  //arma::mat movAvg=arma::zeros(allpaths.n_rows);
  //arma::vec allpath_actions = allpaths.col(0);
  //arma::vec allpath_states = allpaths.col(1);

  //Rcpp::Rcout <<"allpaths.n_rows="<<allpaths.n_rows<<std::endl;

  for (unsigned int i = 0; i < (allpaths.n_rows); i++)
  {
    int action = allpaths(i, 0);
    int state = allpaths(i, 1);

    if (sim != 1)
    {
      state = state - 1;
      action = action - 1;
    }

    //mseMatrix(i)=pow(movAvg(i)-probMatrix_m1(i,((6*state)+action)),2);
    mseMatrix(i) = probMatrix_m1(i, ((6 * state) + action));

    //Rcpp::Rcout <<"movAvg(i)="<<movAvg(i)<<", prob=" <<probMatrix_m1(i,((6*state)+action))<<", mseMatrix(i)="<<mseMatrix(i)<<std::endl;
  }
  //Rcpp::Rcout <<"movAvg="<<movAvg<<std::endl;
  return (mseMatrix);
}

Rcpp::IntegerVector cumsum1(Rcpp::IntegerVector x)
{
  // initialize an accumulator variable
  double acc = 0;
  // initialize the result vector
  Rcpp::IntegerVector res(x.size());
  for (int i = 0; i < x.size(); i++)
  {
    acc += x[i];
    res[i] = acc;
  }
  return res;
}

// [[Rcpp::export]]
arma::vec getBoxTimes(arma::vec enregPosTimes, Rcpp::IntegerVector rleLengths)
{
  Rcpp::IntegerVector indexVec = cumsum1(rleLengths);
  int nrow = rleLengths.size();
  arma::vec boxTimes = arma::zeros(nrow);
  for (int i = 0; i < nrow; i++)
  {
    arma::uvec idx;
    if (i > 0)
    {
      idx = arma::regspace<arma::uvec>((indexVec[i - 1]), (indexVec[i] - 1));
    }
    else
    {
      idx = arma::regspace<arma::uvec>(0, (indexVec[i] - 1));
    }

    double time_in_box = arma::sum(enregPosTimes.elem(idx));
    boxTimes(i) = time_in_box;
  }
  return (boxTimes);
}

// [[Rcpp::export]]
Rcpp::StringVector getTurns(int path, int state)
{

  Rcpp::StringVector turns;
  if (state == 1)
  {
    if (path == 1)
    {
      turns.push_back("dch");
    }
    else if (path == 2)
    {
      turns.push_back("gak");
    }
    else if (path == 3)
    {
      turns.push_back("dcb");
      turns.push_back("bak");
    }
    else if (path == 4)
    {
      turns.push_back("gab");
      turns.push_back("bch");
    }
    else if (path == 5)
    {
      turns.push_back("gab");
      turns.push_back("bcd");
    }
  }
  else if (state == 2)
  {
    if (path == 1)
    {
      turns.push_back("hcd");
    }
    else if (path == 2)
    {
      turns.push_back("kag");
    }
    else if (path == 3)
    {
      turns.push_back("hcb");
      turns.push_back("bag");
    }
    else if (path == 4)
    {
      turns.push_back("kab");
      turns.push_back("bcd");
    }
    else if (path == 5)
    {
      turns.push_back("kab");
      turns.push_back("bch");
    }
  }

  return (turns);
}

// [[Rcpp::export]]
std::string getTurnString(int turnNb)
{
  std::string turn;
  if (turnNb == 1)
  {
    turn = "dch";
  }
  else if (turnNb == 2)
  {
    turn = "gak";
  }
  else if (turnNb == 3)
  {
    turn = "dcb";
  }
  else if (turnNb == 4)
  {
    turn = "bak";
  }
  else if (turnNb == 5)
  {
    turn = "gab";
  }
  else if (turnNb == 6)
  {
    turn = "bch";
  }
  else if (turnNb == 7)
  {
    turn = "bcd";
  }
  else if (turnNb == 8)
  {
    turn = "hcd";
  }
  else if (turnNb == 9)
  {
    turn = "kag";
  }
  else if (turnNb == 10)
  {
    turn = "hcb";
  }
  else if (turnNb == 11)
  {
    turn = "bag";
  }
  else if (turnNb == 12)
  {
    turn = "kab";
  }
  return (turn);
}

// [[Rcpp::export]]
int getTurnIdx(std::string turn)
{
  int turnNb = -1;
  if (turn.compare("dch") == 0)
  {
    turnNb = 1;
  }
  else if (turn.compare("gak") == 0)
  {
    turnNb = 2;
  }
  else if (turn.compare("dcb") == 0)
  {
    turnNb = 3;
  }
  else if (turn.compare("bak") == 0)
  {
    turnNb = 4;
  }
  else if (turn.compare("gab") == 0)
  {
    turnNb = 5;
  }
  else if (turn.compare("bch") == 0)
  {
    turnNb = 6;
  }
  else if (turn.compare("bcd") == 0)
  {
    turnNb = 7;
  }
  else if (turn.compare("hcd") == 0)
  {
    turnNb = 8;
  }
  else if (turn.compare("kag") == 0)
  {
    turnNb = 9;
  }
  else if (turn.compare("hcb") == 0)
  {
    turnNb = 10;
  }
  else if (turn.compare("bag") == 0)
  {
    turnNb = 11;
  }
  else if (turn.compare("kab") == 0)
  {
    turnNb = 12;
  }
  return (turnNb);
}

// [[Rcpp::export]]
arma::mat getTurnTimes(Rcpp::CharacterMatrix allpaths, arma::vec boxTimes)
{

  int totalPaths = allpaths.nrow();
  int currBoxIdx = -1; // Since index starts from 0
  arma::mat res_mat;
  //Rcpp::Rcout <<  "totalPaths="<< totalPaths<<std::endl;
  for (int i = 0; i < totalPaths; i++)
  {
    std::string path_string = Rcpp::as<std::string>(allpaths(i, 2));
    std::string state_string = Rcpp::as<std::string>(allpaths(i, 4));
    std::string sessionNb_string = Rcpp::as<std::string>(allpaths(i, 1));
    //Rcpp::Rcout << "i=" << i << ", path_string =" << path_string <<std::endl;
    int path = std::stoi(path_string);
    int state = std::stoi(state_string);
    int sessionNb = std::stoi(sessionNb_string);
    Rcpp::StringVector turns;
    turns = getTurns(path, state);
    int nbOfTurns = turns.length();
    //Rcpp::Rcout << "i=" << i << ", path =" << path << " ,state=" << state << std::endl;
    //Rcpp::Rcout << "turns=" << turns << std::endl;

    for (int j = 0; j < nbOfTurns; j++)
    {

      std::string currTurn = Rcpp::as<std::string>(turns(j));
      // currTurn.erase(std::remove(turns(j).begin(), currTurn.end(), ','), currTurn.end());
      // currTurn.erase(std::remove(turns(j).begin(), currTurn.end(), ' '), currTurn.end());
      int turnIdx = getTurnIdx(currTurn);

      arma::rowvec new_row = {(i + 1), path, state, turnIdx, sessionNb, 0, 0, 0};

      for (int method = 0; method < 3; method++)
      {

        std::string s = Rcpp::as<std::string>(allpaths(i, 0));
        s.erase(std::remove(s.begin(), s.end(), ','), s.end());
        s.erase(std::remove(s.begin(), s.end(), ' '), s.end());
        //Rcpp::Rcout <<"s=" <<s << ", method=" << method << std::endl;

        if (method == 0)
        {
          std::smatch m;
          std::string pattern = "(";
          pattern.push_back(currTurn.at(0));
          pattern += "(";
          pattern.push_back(currTurn.at(1));
          pattern += ".*";
          pattern.push_back(currTurn.at(2));
          pattern += ")+)";
          std::regex e(pattern);
          std::regex_search(s, m, e);

          int turnStartIdx = currBoxIdx + m.position(1) + 1;
          int turnEndIdx = currBoxIdx + m.position(1) + m[1].length(); //m[1].length() counts turnStartIdx also
          arma::uvec ids = arma::conv_to<arma::uvec>::from(arma::regspace(turnStartIdx, turnEndIdx));

          double turnTime = arma::accu(boxTimes.elem(ids));
          //Rcpp::Rcout << "boxTimes=" << boxTimes.elem(ids)<<std::endl;
          new_row(5 + method) = turnTime;
        }
        else if (method == 1)
        {
          std::smatch m;
          std::string pattern = "((";
          pattern.push_back(currTurn.at(0));
          pattern += ".*";
          pattern.push_back(currTurn.at(1));
          pattern += ")+)";
          pattern.push_back(currTurn.at(2));
          std::regex e(pattern);
          std::regex_search(s, m, e);

          int turnStartIdx = currBoxIdx + m.position(1) + 1;
          int turnEndIdx = currBoxIdx + m.position(1) + m[1].length(); //m[1].length() counts turnStartIdx also
          arma::uvec ids = arma::conv_to<arma::uvec>::from(arma::regspace(turnStartIdx, turnEndIdx));
          double turnTime = arma::accu(boxTimes.elem(ids));

          new_row(5 + method) = turnTime;
        }
        else if (method == 2)
        {
          std::smatch m;
          std::string pattern;
          pattern.push_back(currTurn.at(0));
          pattern += "((";
          pattern.push_back(currTurn.at(1));
          pattern += ".*";
          pattern.push_back(currTurn.at(2));
          pattern += ")+)";
          std::regex e(pattern);
          std::regex_search(s, m, e);

          int turnStartIdx = currBoxIdx + m.position(1) + 1;
          int turnEndIdx = currBoxIdx + m.position(1) + m[1].length(); //m[1].length() counts turnStartIdx also
          arma::uvec ids = arma::conv_to<arma::uvec>::from(arma::regspace(turnStartIdx, turnEndIdx));
          double turnTime = arma::accu(boxTimes.elem(ids));

          new_row(5 + method) = turnTime;
        }
        else if (method == 3)
        {
          std::smatch m;
          std::string pattern = ".*(";
          pattern.push_back(currTurn.at(1));
          pattern.push_back(')');
          std::regex e(pattern);
          std::regex_search(s, m, e);

          int turnStartIdx = currBoxIdx + m.position(1) + 1;
          arma::uvec ids(1);
          ids = turnStartIdx;
          double turnTime = arma::accu(boxTimes.elem(ids));

          new_row(5 + method) = turnTime;
        }
      }
      res_mat = arma::join_vert(res_mat, new_row);
    }

    std::string currPath = Rcpp::as<std::string>(allpaths(i, 0));
    currPath.erase(std::remove(currPath.begin(), currPath.end(), ','), currPath.end());
    currPath.erase(std::remove(currPath.begin(), currPath.end(), ' '), currPath.end());
    currBoxIdx = currBoxIdx + currPath.length();
    //Rcpp::Rcout << "currBoxIdx=" << currBoxIdx << ", currPath=" << currPath << ", currPath.length=" << currPath.length() << std::endl;
  }
  return (res_mat);
}

// [[Rcpp::export]]
arma::vec getComputationalActivity(arma::mat allpaths, arma::mat probabilityMatrix)
{
  arma::vec episodes = allpaths.col(5);
  int max_episode = arma::max(episodes);
  //Rcpp::Rcout << "max_episode=" <<max_episode<<std::endl;
  int last_ep_idx = 0;
  arma::vec computationalActivity = arma::zeros(max_episode);
  arma::rowvec means = arma::mean(probabilityMatrix, 0);
  //Rcpp::Rcout << "means=" <<means<<std::endl;
  for (int i = 1; i <= (max_episode - 1); i++)
  {
    arma::uvec episodeIdx = arma::find(allpaths.col(5) == i);
    int episode_end_idx = arma::max(episodeIdx);
    //Rcpp::Rcout << "episode_end_idx=" <<episode_end_idx<< ", last_ep_idx=" <<last_ep_idx << std::endl;
    arma::rowvec probDiff = arma::abs(probabilityMatrix.row(episode_end_idx) - probabilityMatrix.row(last_ep_idx));
    //Rcpp::Rcout << "probDiff=" <<probDiff<<std::endl;
    probDiff = probDiff / means;
    probDiff.replace(arma::datum::nan, 0);
    computationalActivity(i) = arma::accu(probDiff);
    //Rcpp::Rcout << "i=" <<i << ", computationalActivity(i)=" <<computationalActivity(i)<<std::endl;
    last_ep_idx = i;
  }
  return (computationalActivity);
}

#endif