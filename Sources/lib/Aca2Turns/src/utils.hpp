#ifndef __UTILS__
#define __UTILS__

#include <vector>
#include<set>
#include <algorithm>
#include <string>
#include <regex>
#include <cmath>
#include <RcppArmadillo.h>
#include <RcppArmadilloExtensions/sample.h>
#include "tree.hpp"


inline double softmax(std::shared_ptr<TreeNode> currNode)
{
  //Rcpp::Rcout <<  "S="<< S<<std::endl;
  //Rcpp::Rcout <<  v<< std::endl;

  std::vector<double> credits;
  credits.push_back(currNode->credit);
  std::vector<double> siblingCredits = getSiblingCredits(currNode);
  credits.insert(std::end(credits), std::begin(siblingCredits), std::end(siblingCredits));
  arma::vec v(credits);

  if(!v.is_finite())
  {
    Rcpp::Rcout <<  "v="<<v <<std::endl;
  }
  double m = arma::max(v);
  v = exp(v - m);
  // //Rcpp::Rcout << "m=" << m<< std::endl;
  double exp_sum = arma::accu(v);
  v = v / exp_sum;
  double pr_A = v[0];
  
  

  return (pr_A);
}

// [[Rcpp::export()]]
arma::vec getTrialTimes(Rcpp::NumericVector allpaths, Rcpp::NumericMatrix enreg_pos)
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
Rcpp::StringVector getTurnsFromPaths(int path, int state)
{

  Rcpp::StringVector turns;
  if (state == 0)
  {
    if (path == 0)
    {
      turns.push_back("dch");
    }
    else if (path == 1)
    {
      turns.push_back("gak");
    }
    else if (path == 2)
    {
      turns.push_back("dcb");
      turns.push_back("bak");
    }
    else if (path == 3)
    {
      turns.push_back("gab");
      turns.push_back("bch");
    }
    else if (path == 4)
    {
      turns.push_back("gab");
      turns.push_back("bcd");
    }
    else if (path == 5)
    {
      turns.push_back("dcb");
      turns.push_back("bag");
    }
  }
  else if (state == 1)
  {
    if (path == 0)
    {
      turns.push_back("hcd");
    }
    else if (path == 1)
    {
      turns.push_back("kag");
    }
    else if (path == 2)
    {
      turns.push_back("hcb");
      turns.push_back("bag");
    }
    else if (path == 3)
    {
      turns.push_back("kab");
      turns.push_back("bcd");
    }
    else if (path == 4)
    {
      turns.push_back("kab");
      turns.push_back("bch");
    }
    else if (path == 5)
    {
      turns.push_back("hcb");
      turns.push_back("bak");
    }
    
  }

  return (turns);
}

// [[Rcpp::export]]
std::string getTurnString(int turnNb)
{
  std::string turn;
  if (turnNb == 0)
  {
    turn = "dcb";
  }
  else if (turnNb == 1)
  {
    turn = "dch";
  }
  else if (turnNb == 2)
  {
    turn = "gak";
  }
  else if (turnNb == 3)
  {
    turn = "gab";
  }
  else if (turnNb == 4)
  {
    turn = "bak";
  }
  else if (turnNb == 5)
  {
    turn = "bag";
  }
  else if (turnNb == 6)
  {
    turn = "bcd";
  }
  else if (turnNb == 7)
  {
    turn = "bch";
  }
  else if (turnNb == 8)
  {
    turn = "hcb";
  }
  else if (turnNb == 9)
  {
    turn = "hcd";
  }
  else if (turnNb == 10)
  {
    turn = "kag";
  }
  else if (turnNb == 11)
  {
    turn = "kab";
  }
  else if (turnNb == 12)
  {
    turn = "bak";
  }
  else if (turnNb == 13)
  {
    turn = "bag";
  }
  else if (turnNb == 14)
  {
    turn = "bcd";
  }
  else if (turnNb == 15)
  {
    turn = "bch";
  }
  return (turn);
}

// [[Rcpp::export]]
unsigned int getTurnIdx(std::string turn, int state)
{
  unsigned int turnNb = 100;
  if (state == 0)
  {
    if (turn == "dcb")
    {
      turnNb = 0;
    }
    else if (turn == "dch")
    {
      turnNb = 1;
    }
    else if (turn == "gak")
    {
      turnNb = 2;
    }
    else if (turn == "gab")
    {
      turnNb = 3;
    }
    else if (turn == "bak")
    {
      turnNb = 4;
    }
    else if (turn == "bag")
    {
      turnNb = 5;
    }
    else if (turn == "bcd")
    {
      turnNb = 6;
    }
    else if (turn == "bch")
    {
      turnNb = 7;
    }
  }
  else if (state == 1)
  {
    if (turn == "hcb")
    {
      turnNb = 8;
    }
    else if (turn == "hcd")
    {
      turnNb = 9;
    }
    else if (turn == "kag")
    {
      turnNb = 10;
    }
    else if (turn == "kab")
    {
      turnNb = 11;
    }
    else if (turn == "bak")
    {
      turnNb = 12;
    }
    else if (turn == "bag")
    {
      turnNb = 13;
    }
    else if (turn == "bcd")
    {
      turnNb = 14;
    }
    else if (turn == "bch")
    {
      turnNb = 15;
    }
  }

  return turnNb;
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
    //Rcpp::Rcout << "i=" << i << ", path_string =" << path_string << " ,state_string=" <<state_string <<std::endl;
    int path = std::stoi(path_string);
    int state = std::stoi(state_string);
    Rcpp::StringVector turns;
    turns = getTurnsFromPaths(path, state);
    int nbOfTurns = turns.length();
    //Rcpp::Rcout << "i=" << i << ", path =" << path << " ,state=" <<state <<std::endl;
    //Rcpp::Rcout << "turns=" << turns << std::endl;

    for (int j = 0; j < nbOfTurns; j++)
    {

      std::string currTurn = Rcpp::as<std::string>(turns(j));
      // currTurn.erase(std::remove(turns(j).begin(), currTurn.end(), ','), currTurn.end());
      // currTurn.erase(std::remove(turns(j).begin(), currTurn.end(), ' '), currTurn.end());
      unsigned int turnIdx = getTurnIdx(currTurn, state);

      arma::rowvec new_row = {static_cast<double>(i + 1), static_cast<double>(path), static_cast<double>(state), static_cast<double>(turnIdx), 0, 0, 0, 0};

      for (int method = 0; method < 4; method++)
      {
        //Rcpp::Rcout <<  "method="<< method<<std::endl;
        if (method == 0)
        {
          std::string s = Rcpp::as<std::string>(allpaths(i, 0));
          s.erase(std::remove(s.begin(), s.end(), ','), s.end());
          s.erase(std::remove(s.begin(), s.end(), ' '), s.end());

          std::smatch m;
          std::string pattern = ".*(";
          pattern.push_back(currTurn.at(0));
          pattern += ".*?";
          pattern.push_back(currTurn.at(2));
          pattern.push_back(')');
          std::regex e(pattern);
          std::regex_search(s, m, e);

          int turnStartIdx = currBoxIdx + m.position(1) + 1;
          int turnEndIdx = currBoxIdx + m.position(1) + m[1].length(); //m[1].length() counts turnStartIdx also
          arma::uvec ids = arma::conv_to<arma::uvec>::from(arma::regspace(turnStartIdx, turnEndIdx));
          double turnTime = arma::accu(boxTimes.elem(ids));

          new_row(4 + method) = turnTime;
        }
        else if (method == 1)
        {
          std::string s = Rcpp::as<std::string>(allpaths(i, 0));
          s.erase(std::remove(s.begin(), s.end(), ','), s.end());
          s.erase(std::remove(s.begin(), s.end(), ' '), s.end());
          std::smatch m;
          std::string pattern = ".*(";
          pattern.push_back(currTurn.at(0));
          pattern += ".*?";
          pattern.push_back(currTurn.at(1));
          pattern.push_back(')');
          std::regex e(pattern);
          std::regex_search(s, m, e);

          int turnStartIdx = currBoxIdx + m.position(1) + 1;
          int turnEndIdx = currBoxIdx + m.position(1) + m[1].length(); //m[1].length() counts turnStartIdx also
          arma::uvec ids = arma::conv_to<arma::uvec>::from(arma::regspace(turnStartIdx, turnEndIdx));
          double turnTime = arma::accu(boxTimes.elem(ids));

          new_row(4 + method) = turnTime;
        }
        else if (method == 2)
        {
          std::string s = Rcpp::as<std::string>(allpaths(i, 0));
          s.erase(std::remove(s.begin(), s.end(), ','), s.end());
          s.erase(std::remove(s.begin(), s.end(), ' '), s.end());
          std::smatch m;
          std::string pattern = ".*(";
          pattern.push_back(currTurn.at(1));
          pattern += ".*?";
          pattern.push_back(currTurn.at(2));
          pattern.push_back(')');
          std::regex e(pattern);
          std::regex_search(s, m, e);

          int turnStartIdx = currBoxIdx + m.position(1) + 1;
          int turnEndIdx = currBoxIdx + m.position(1) + m[1].length(); //m[1].length() counts turnStartIdx also
          arma::uvec ids = arma::conv_to<arma::uvec>::from(arma::regspace(turnStartIdx, turnEndIdx));
          double turnTime = arma::accu(boxTimes.elem(ids));

          new_row(4 + method) = turnTime;
        }
        else if (method == 3)
        {
          std::string s = Rcpp::as<std::string>(allpaths(i, 0));
          s.erase(std::remove(s.begin(), s.end(), ','), s.end());
          s.erase(std::remove(s.begin(), s.end(), ' '), s.end());
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

          new_row(4 + method) = turnTime;
        }
      }
      res_mat = arma::join_vert(res_mat, new_row);
    }

    std::string currPath = Rcpp::as<std::string>(allpaths(i, 0));
    currPath.erase(std::remove(currPath.begin(), currPath.end(), ','), currPath.end());
    currPath.erase(std::remove(currPath.begin(), currPath.end(), ' '), currPath.end());
    currBoxIdx = currBoxIdx + currPath.length();
  }
  return (res_mat);
}

void decayCredits(std::shared_ptr<TreeNode> root, double gamma)
{
    for (auto i = root->child.begin(); i != root->child.end(); i++)
    {
        (*i)->credit = (*i)->credit * gamma;
        //Rcpp::Rcout << "turns=" << (*i)->turn << " credits after decay = " <<(*i)->credit << std::endl;
        std::vector<std::shared_ptr<TreeNode>> childNodes = (*i)->child;
        if (!childNodes.empty())
        {
             for (auto child = childNodes.begin(); child != childNodes.end(); child++)
             {
                (*child)->credit = (*child)->credit * gamma;
                //Rcpp::Rcout << "turns=" << (*child)->turn << " credits after decay = " <<(*child)->credit << std::endl;
             }
        }  
    }
}

bool elementFound(Rcpp::StringVector turns, std::string turn)
{
  bool elementFound = false;
  for(int i=0; i<turns.size(); i++)
  {
    if(turns[i] == turn)
    {
      elementFound = true;
      break;
    }
  }
  return(elementFound);
}

// [[Rcpp::export]]
int getPathFromTurns(Rcpp::StringVector turns, int state)
{
  //Rcpp::Rcout << "turns=" << turns << ", state=" << state << std::endl;
  int path = 5;
  if (state == 0)
  {
    if (elementFound(turns,"dch"))
    {
      path = 0;
    }
    else if (elementFound(turns,"gak"))
    {
      path = 1;
    }
    else if (elementFound(turns,"dcb") && elementFound(turns,"bak"))
    {
      path = 2;
    }
    else if (elementFound(turns,"gab") && elementFound(turns,"bch"))
    {
      path = 3;
    }
    else if (elementFound(turns,"gab") && elementFound(turns,"bcd"))
    {
      path = 4;
    }
    else if (elementFound(turns,"dcb") && elementFound(turns,"bag"))
    {
      path = 5;
    }
  }
  else if (state == 1)
  {
    if (elementFound(turns,"hcd"))
    {
      path = 0;
    }
    else if (elementFound(turns,"kag"))
    {
      path = 1;
    }
    else if (elementFound(turns,"hcb") && elementFound(turns,"bag"))
    {
      path = 2;
    }
    else if (elementFound(turns,"kab") && elementFound(turns,"bcd"))
    {
      path = 3;
    }
    else if (elementFound(turns,"kab") && elementFound(turns,"bch"))
    {
      path = 4;
    }
    else if (elementFound(turns,"hcb") && elementFound(turns,"bak"))
    {
      path = 5;
    }
  }

  return (path);
}
#endif