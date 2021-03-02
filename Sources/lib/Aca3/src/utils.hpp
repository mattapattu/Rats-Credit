#ifndef __UTILS__
#define __UTILS__

#include <vector>
#include <algorithm>
#include <string>
#include <regex>
#include <RcppArmadillo.h>
#include <RcppArmadilloExtensions/sample.h>



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
arma::mat simulatePathTime(arma::mat turnTimes, arma::mat allpaths, int pathNb, int path, arma::vec pathStages)
{
  std::vector<int> grp1 = {0,1};
  std::vector<int> grp2 = {2,3,4,5};

  int start = -1;
  int end = 0;
  if(pathNb < pathStages(1))
  {
    start = 1;
    end = pathStages(1)-1;
  }
  else if(pathNb >= pathStages(1) && pathNb < pathStages(2))
  {
    start = pathStages(1);
    end = pathStages(2)-1;
  }
  else if(pathNb >= pathStages(2))
  {
    start = pathStages(2);
    end = pathStages(3);
  }

  start = start-1;
  end = end-1;
  //Rcpp::Rcout << "start=" << start << ", end=" << end << std::endl;
  arma::mat allpaths_submat = allpaths.rows(start,end);
  arma::vec path_submat = allpaths_submat.col(0);
  arma::uvec allpathsubmat_idx;
  if(std::find(grp1.begin(), grp1.end(), path) != grp1.end())
  {
    allpathsubmat_idx = arma::find(path_submat == 0 || path_submat == 1);
  }
  else if(std::find(grp2.begin(), grp2.end(), path) != grp2.end())
  {
    allpathsubmat_idx = arma::find(path_submat == 2|| path_submat == 3||path_submat == 4 || path_submat == 5);
  }
  

  arma::mat allpath_submat2 = allpaths_submat.rows(allpathsubmat_idx);
  arma::vec sample = allpath_submat2.col(3);
  std::vector<double> q = quartiles(arma::conv_to<std::vector<double>>::from(sample));
  arma::uvec final_sample_ids = arma::find(sample >= q[0] && sample <= q[2]);
  double probability = (double) 1/(double) final_sample_ids.n_elem;
  arma::vec pvec(final_sample_ids.n_elem); 
  pvec.fill(probability);
  arma::uword sampled_id = Rcpp::RcppArmadillo::sample(final_sample_ids, 1, true, pvec)[0];
  int actionNb = allpath_submat2(sampled_id,5);
  arma::uvec turnIdx = arma::find(turnTimes.col(0) == actionNb);
  arma::mat turn_submat = turnTimes.rows(turnIdx);
  arma::uvec colIds = {3,5};
  arma::mat turnDurations = turn_submat.cols(colIds);
  return(turnDurations);
}
#endif