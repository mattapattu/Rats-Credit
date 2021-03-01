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
  //Rcpp::Rcout << "m=" << m << ", exp_sum=" <<exp_sum << std::endl;
  //Rcpp::Rcout <<  v<< std::endl;
  double pr_A = (std::exp(H(S, A) - m)) / exp_sum;
  //Rcpp::Rcout << "pr_A=" << pr_A << std::endl;

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
arma::mat empiricalProbMat2(arma::mat allpaths, int window)
{

  arma::mat probMatrix;

  arma::vec allpath_actions = allpaths.col(0);
  arma::vec allpath_states = allpaths.col(1);
  arma::vec sessionVec = allpaths.col(4);
  arma::vec uniqSessIdx = arma::unique(sessionVec);
  //Rcpp::Rcout << "sessionVec=" << sessionVec << std::endl;
  //Rcpp::Rcout << "uniqSessIdx=" << uniqSessIdx << std::endl;
 
  // Loop through each session
  for (unsigned int session = 0; session < (uniqSessIdx.n_elem); session++)
  {
     int sessId = uniqSessIdx(session);
    //Rcpp::Rcout << "session=" <<session <<", sessId=" << sessId << std::endl;
    arma::uvec sessionIdx = arma::find(allpaths.col(4) == (sessId));
    arma::vec actions_sess = allpath_actions.elem(sessionIdx);
    arma::vec states_sess = allpath_states.elem(sessionIdx);
    int nrow = actions_sess.n_rows;
    int size = nrow/window;
    int remaining = nrow % window;

    if(size == 0 && remaining > 0){
      size = 1;
    }
    arma::mat probMatrix_sess(size,15);  
    for(int i=0;i<size;i++)
    {
      probMatrix_sess(i,12) = sessId;
      

      arma::uword start_idx = i * window;
      arma::uword end_idx = (i+1)*window - 1;
      
      if(nrow/window < 1)
      {
        end_idx = remaining-1;
      }
      else if(i == size-1)
      {
        end_idx = end_idx + remaining;
      }
      probMatrix_sess(i,13) =  start_idx;
      probMatrix_sess(i,14) =  end_idx;
      arma::uvec range = arma::regspace<arma::uvec>(start_idx,end_idx);
      //Rcpp::Rcout << "start_idx=" << start_idx << ", end_idx=" << end_idx << std::endl;
      arma::vec states_chunk = states_sess.elem(range);
      arma::vec actions_chunk = actions_sess.elem(range);
      //Rcpp::Rcout << "states_chunk=" << states_chunk << std::endl;
      //Rcpp::Rcout << "actions_chunk=" << actions_chunk << std::endl;
      for(int state = 0; state < 2; state++)
      {
        
        arma::uvec state_idx = arma::find(states_chunk == (state+1));
        arma::vec actions_in_state = actions_chunk.elem(state_idx);
        //Rcpp::Rcout << "state_idx=" << state_idx << std::endl;
        //probMatrix_sess.submat(i, 6, i, 11) = arma::zeros(1, 6);
        for(int act = 0; act < 6; act++)
          {
            arma::uvec act_idx = arma::find(actions_in_state == (act+1));
            //Rcpp::Rcout << "act_idx=" << act_idx << std::endl;
            double x = act_idx.n_elem / (1.0 * state_idx.n_elem);
            probMatrix_sess(i, ((state*6) + act)) = x;
          }      
      }
    }
    probMatrix = arma::join_cols(probMatrix, probMatrix_sess);
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

    arma::uword start_idx = idx(0);
    arma::uword end_idx = idx(idx.n_elem-1);
    double time_in_box = enregPosTimes(end_idx)-enregPosTimes(start_idx);
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