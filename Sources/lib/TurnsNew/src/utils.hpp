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
#include "Debugger.hpp"





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
arma::vec simulateTurnDuration(arma::mat turnTimes, arma::mat allpaths, int turnId, int turnNb, arma::vec turnStages, Rcpp::List nodeGroups, bool debug = false)
{
  Debugger logger;
  logger.setDebug(debug);
  std::ostringstream msg; 

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

  //Rcpp::Rcout << "turnTimes_submat=" << turnTimes_submat << std::endl;
  Rcpp::IntegerVector idx;
  for(int i=0;i<nodeGroups.size();i++)
  {
    Rcpp::CharacterVector vecGrp = Rcpp::as<Rcpp::CharacterVector>(nodeGroups[i]);
    Rcpp::CharacterVector turnIds = Rcpp::as<Rcpp::CharacterVector>(Rcpp::wrap(turnId_submat));
    
     Rcpp::CharacterVector table(1);
     table(0) = turnId;
     Rcpp::IntegerVector vec =  Rcpp::match(vecGrp , table ) ;
     //Rcpp::Rcout <<"vecGrp=" <<vecGrp << ", turnId=" <<turnId << ", vec=" <<vec << std::endl;
     bool res = Rcpp::any(!Rcpp::is_na(vec));
     if(res)
     {
       Rcpp::IntegerVector v = Rcpp::seq(0, turnId_submat.size()-1);
       Rcpp::IntegerVector vec =  Rcpp::match(turnIds, vecGrp ) ;
       idx = v[!Rcpp::is_na(vec)];
       break;
     }
  }
  
  arma::uvec arma_idx = Rcpp::as<arma::uvec>(idx);
  //Rcpp::Rcout <<"arma_idx=" <<arma_idx << std::endl;
 
  //arma::vec turndurations_submat = turnTimes_submat.col(5);
  arma::mat submat_sample = turnTimes_submat.rows(arma_idx);
  msg.str("");
  msg << "submat_sample=";
   logger.PrintArmaMat(msg.str(),submat_sample); 
  arma::vec sample = submat_sample.col(5);
  std::vector<double> q = quartiles(arma::conv_to<std::vector<double>>::from(sample));
  arma::uvec final_sample_ids = arma::find(sample >= q[0] && sample <= q[2]);
  //arma::vec fin_sample = sample.elem(final_sample_ids);
  arma::vec pvec(final_sample_ids.n_elem); 
  double probability = (double) 1/(double) final_sample_ids.n_elem;
  pvec.fill(probability);
  arma::uword sampled_id = Rcpp::RcppArmadillo::sample(final_sample_ids, 1, true, pvec)[0];
    msg.str("");
    msg << "sampled_id=" <<sampled_id;
   logger.Print(msg.str()); 

  arma::rowvec turnRow = submat_sample.row(sampled_id);
  arma::uvec cols = {0,5}; //3 = ActionNb, 5 = actionNb

   msg.str("");
   msg << "turnRow=";
   logger.PrintArmaRowVec(msg.str(),turnRow); 
  arma::vec turnDurations = turnRow.elem(cols);

    msg.str("");
   msg << "turnDurations=";
   logger.PrintArmaVec(msg.str(),turnDurations); 

  return(turnDurations);
}



arma::mat simulatePathTime(arma::mat turnTimes, arma::mat allpaths, int pathNb, int path, arma::vec pathStages)
{
  std::vector<int> grp1 = {0};
  std::vector<int> grp2 = {1};
  std::vector<int> grp3 = {3,4};
  std::vector<int> grp4 = {2,5};


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
  Rcpp::Rcout << "start=" << start << ", end=" << end << std::endl;
  arma::mat allpaths_submat = allpaths.rows(start,end);
  arma::vec path_submat = allpaths_submat.col(0) - 1;
  arma::uvec allpathsubmat_idx;
  if(std::find(grp1.begin(), grp1.end(), path) != grp1.end())
  {
    Rcpp::Rcout << "Here1" << std::endl;
    allpathsubmat_idx = arma::find(path_submat == 0);
  }
  else if(std::find(grp2.begin(), grp2.end(), path) != grp2.end())
  {
    Rcpp::Rcout << "Here2" << std::endl;
    allpathsubmat_idx = arma::find(path_submat == 1);
  }
  else if(std::find(grp3.begin(), grp3.end(), path) != grp3.end())
  {
    Rcpp::Rcout << "Here3" << std::endl;
    allpathsubmat_idx = arma::find( path_submat == 3||path_submat == 4 );
  }
  else if(std::find(grp4.begin(), grp4.end(), path) != grp4.end())
  {
    Rcpp::Rcout << "Here4" << std::endl;
    allpathsubmat_idx = arma::find(path_submat == 2 || path_submat == 5);
  }
  
 Rcpp::Rcout << "allpathsubmat_idx.n_elem=" << allpathsubmat_idx.n_elem << std::endl;
  
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
  arma::uvec colIds = {0,5}; //3 = ActionNb, 5 = actionNb
  arma::mat turnDurations = turn_submat.cols(colIds);
  return(turnDurations);
}





#endif