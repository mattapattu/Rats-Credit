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


inline double softmax(std::shared_ptr<TreeNode> currNode, std::shared_ptr<TreeNode> parentNode)
{
  //Rcpp::Rcout <<  "S="<< S<<std::endl;
  //Rcpp::Rcout <<  v<< std::endl;

  double nodeCredits = currNode->credit;
  std::vector<double> childrenCredits = getChildrenCredits(parentNode);
  arma::vec v(childrenCredits);

  if(!v.is_finite())
  {
    Rcpp::Rcout <<  "v="<<v <<std::endl;
  }
  double m = arma::max(v);
  v = exp(v - m);
  // //Rcpp::Rcout << "m=" << m<< std::endl;
  double exp_sum = arma::accu(v);
  double pr_A = exp(nodeCredits)/exp_sum;
  
  

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
      turns.push_back("dc1");
      turns.push_back("c2h");
    }
    else if (path == 1)
    {
      turns.push_back("fga1");
      turns.push_back("a2kj");
    }
    else if (path == 2)
    {
      turns.push_back("dc1");
      turns.push_back("c2ba1");
      turns.push_back("a2kj");
    }
    else if (path == 3)
    {
      turns.push_back("fga1");
      turns.push_back("a2bc1");
      turns.push_back("c2h");
    }
    else if (path == 4)
    {
      turns.push_back("fga1");
      turns.push_back("a2bc1");
      turns.push_back("c2d");
    }
    else if (path == 5)
    {
      turns.push_back("dc1");
      turns.push_back("c2ba1");
      turns.push_back("a2gf");
    }
  }
  else if (state == 1)
  {
    if (path == 0)
    {
      turns.push_back("hc1");
      turns.push_back("c2d");
    }
    else if (path == 1)
    {
      turns.push_back("jka1");
      turns.push_back("a2gf");
    }
    else if (path == 2)
    {
      turns.push_back("hc1");
      turns.push_back("c2ba1");
      turns.push_back("a2gf");
    }
    else if (path == 3)
    {
      turns.push_back("jka1");
      turns.push_back("a2bc1");
      turns.push_back("c2d");
    }
    else if (path == 4)
    {
      turns.push_back("jka1");
      turns.push_back("a2bc1");
      turns.push_back("c2h");
    }
    else if (path == 5)
    {
      turns.push_back("hc1");
      turns.push_back("c2ba1");
      turns.push_back("a2kj");
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
    turn = "dc1";
  }
  else if (turnNb == 1)
  {
    turn = "fga1";
  }
  else if (turnNb == 2)
  {
    turn = "c2h";
  }
  else if (turnNb == 3)
  {
    turn = "c2ba1";
  }
  else if (turnNb == 4)
  {
    turn = "a2bc1";
  }
  else if (turnNb == 5)
  {
    turn = "a2kj";
  }
  else if (turnNb == 6)
  {
    turn = "a2gf";
  }
  else if (turnNb == 7)
  {
    turn = "c2d";
  }
  else if (turnNb == 8)
  {
    turn = "hc1";
  }
  else if (turnNb == 9)
  {
    turn = "jka1";
  }
  else if (turnNb == 10)
  {
    turn = "c2d";
  }
  else if (turnNb == 11)
  {
    turn = "c2ba1";
  }
  else if (turnNb == 12)
  {
    turn = "a2bc1";
  }
  else if (turnNb == 13)
  {
    turn = "a2gf";
  }
  else if (turnNb == 14)
  {
    turn = "a2kj";
  }
  else if (turnNb == 15)
  {
    turn = "c2h";
  }
   
  return (turn);
}

// [[Rcpp::export]]
unsigned int getTurnIdx(std::string turn, int state)
{
  unsigned int turnNb = 100;
  if (state == 0)
  {
    if (turn == "dc1")
    {
      turnNb = 0;
    }
    else if (turn == "fga1")
    {
      turnNb = 1;
    }
    else if (turn == "c2h")
    {
      turnNb = 2;
    }
    else if (turn == "c2ba1")
    {
      turnNb = 3;
    }
    else if (turn == "a2bc1")
    {
      turnNb = 4;
    }
    else if (turn == "a2kj")
    {
      turnNb = 5;
    }
    else if (turn == "a2gf")
    {
      turnNb = 6;
    }
    else if (turn == "c2d")
    {
      turnNb = 7;
    }
  }
  else if (state == 1)
  {
    if (turn == "hc1")
    {
      turnNb = 8;
    }
    else if (turn == "jka1")
    {
      turnNb = 9;
    }
    else if (turn == "c2d")
    {
      turnNb = 10;
    }
    else if (turn == "c2ba1")
    {
      turnNb = 11;
    }
    else if (turn == "a2bc1")
    {
      turnNb = 12;
    }
    else if (turn == "a2gf")
    {
      turnNb = 13;
    }
    else if (turn == "a2kj")
    {
      turnNb = 14;
    }
    else if (turn == "c2h")
    {
      turnNb = 15;
    }
  }

  return turnNb;
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
    if (elementFound(turns,"dc1") && elementFound(turns,"c2h"))
    {
      path = 0;
    }
    else if (elementFound(turns,"fga1") && elementFound(turns,"a2kj"))
    {
      path = 1;
    }
    else if (elementFound(turns,"dc1") && elementFound(turns,"c2ba1") && elementFound(turns,"a2kj"))
    {
      path = 2;
    }
    else if (elementFound(turns,"fga1") && elementFound(turns,"a2bc1") && elementFound(turns,"c2h"))
    {
      path = 3;
    }
    else if (elementFound(turns,"fga1") && elementFound(turns,"a2bc1") && elementFound(turns,"c2d"))
    {
      path = 4;
    }
    else if (elementFound(turns,"dc1") && elementFound(turns,"c2ba1") && elementFound(turns,"a2gf"))
    {
      path = 5;
    }
  }
  else if (state == 1)
  {
     if (elementFound(turns,"hc1") && elementFound(turns,"c2d"))
    {
      path = 0;
    }
    else if (elementFound(turns,"jka1") && elementFound(turns,"a2gf"))
    {
      path = 1;
    }
    else if (elementFound(turns,"hc1") && elementFound(turns,"c2ba1") && elementFound(turns,"a2gf"))
    {
      path = 2;
    }
    else if (elementFound(turns,"jka1") && elementFound(turns,"a2bc1") && elementFound(turns,"c2d"))
    {
      path = 3;
    }
    else if (elementFound(turns,"jka1") && elementFound(turns,"a2bc1") && elementFound(turns,"c2h"))
    {
      path = 4;
    }
    else if (elementFound(turns,"hc1") && elementFound(turns,"c2ba1") && elementFound(turns,"a2kj"))
    {
      path = 5;
    }
  }

  return (path);
}

// [[Rcpp::export]]
std::vector<double> quartiles(std::vector<double> samples)
{
    // return as vector containing {first quartile, median, third quartile}
    std::vector<double> answer;
    int size = samples.size();
    std::sort(samples.begin(), samples.end());
    // First Quartile
    answer.push_back(samples[size/4]);
    // Second Quartile = Median
    if (size % 2 == 0)
        answer.push_back((samples[size / 2 - 1] + samples[size / 2]) / 2);
    else
        answer.push_back(samples[size / 2]);
    // Third Quartile
    answer.push_back(samples[size*3/4]);
    return answer;
}

// [[Rcpp::export]]
double simulateTurnDuration(arma::mat turnTimes, arma::mat allpaths, int turnId, int turnNb, arma::vec turnStages)
{
  std::vector<int> grp1 = {0,2,7,8,10,15};
  std::vector<int> grp2 = {1,5,6,9,13,14};
  std::vector<int> grp3 = {3,4,11,12};

  int start = -1;
  int end = 0;
  if(turnNb < turnStages(1))
  {
    start = 1;
    end = turnStages(1)-1;
  }
  else if(turnNb >= turnStages(1) && turnNb < turnStages(2))
  {
    start = turnStages(1);
    end = turnStages(2)-1;
  }
  else if(turnNb >= turnStages(2))
  {
    start = turnStages(2);
    end = turnStages(3);
  }

  start = start-1;
  end = end-1;
  //Rcpp::Rcout << "start=" << start << ", end=" << end << std::endl;
  
  arma::mat turnTimes_submat = turnTimes.rows(start,end);
  arma::vec turnId_submat = turnTimes_submat.col(3);
  arma::uvec idx;

  //Rcpp::Rcout << "turnTimes_submat=" << turnTimes_submat << std::endl;

  if(std::find(grp1.begin(), grp1.end(), turnId) != grp1.end())
  {
    idx = arma::find(turnId_submat == 0 || turnId_submat == 2|| turnId_submat == 7||turnId_submat == 8 || turnId_submat == 10|| turnId_submat == 15);
  }
  else if(std::find(grp2.begin(), grp2.end(), turnId) != grp2.end())
  {
    idx = arma::find(turnId_submat == 1 || turnId_submat == 5|| turnId_submat == 6||turnId_submat == 9 || turnId_submat == 13|| turnId_submat == 14);
  }
  else if(std::find(grp3.begin(), grp3.end(), turnId) != grp3.end())
  {
    idx = arma::find(turnId_submat == 3 || turnId_submat == 4|| turnId_submat == 11||turnId_submat == 12);
  }
  arma::vec turndurations_submat = turnTimes_submat.col(5);
  arma::vec sample = turndurations_submat.elem(idx);
  std::vector<double> q = quartiles(arma::conv_to<std::vector<double>>::from(sample));
  arma::uvec final_sample_ids = arma::find(sample >= q[0] && sample <= q[2]);
  arma::vec fin_sample = sample.elem(final_sample_ids);
  arma::vec pvec(fin_sample.n_elem); 
  double probability = (double) 1/(double) fin_sample.n_elem;
  pvec.fill(probability);
  double duration = Rcpp::RcppArmadillo::sample(fin_sample, 1, true, pvec)[0];
  return(duration);
}


#endif