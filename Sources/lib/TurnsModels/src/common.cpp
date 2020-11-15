// [[Rcpp::depends(RcppArmadillo)]]
#include <vector>
#include <RcppArmadillo.h>

#include <regex>
#include <RcppArmadilloExtensions/sample.h>

#include "acaCreditUpdate.hpp"
#include "gbCreditUpdate.hpp"
#include "utils.hpp"
#include "tree.hpp"

using namespace Rcpp;


int aca_getNextState(int curr_state,int action){
  int new_state=-1;
  if(action == 4){
    new_state=curr_state;
  }else if(curr_state==0){
    new_state=1;
  }else if(curr_state==1){
    new_state=0;
  }

  return(new_state);
}

int softmax_action_sel(arma::vec H,turn S){

  // Rcpp::Rcout <<"S="<<S<<", H="<<H<<std::endl;
  double m=arma::max(H);

  //Rcpp::Rcout <<"m="<<m<<", v="<<v<<std::endl;

  H=exp(H-m);
  double exp_sum  = arma::accu(v) ;
  H=H/exp_sum;
  IntegerVector actions = seq(0, 12);
  int action_selected = Rcpp::RcppArmadillo::sample(actions, 1, true, H)[0] ;
  //Rcpp::Rcout <<"action_selected="<<action_selected<<std::endl;

  return(action_selected);

}

arma::mat updateCreditMatrix(arma::mat H,arma::vec actions, arma::vec states, arma::vec time_taken_for_trial, double alpha, float score_episode, double avg_score, arma::mat activityMatrix, int model){

  if(model==1){
    H = AcaCreditUpdate(H,actions, states, time_taken_for_trial,  alpha, score_episode);

  }else if(model==2){
    H = GbCreditUpdate(H, actions, states, alpha, score_episode, avg_score);

  }
  return(H);
}



// [[Rcpp::export()]]
arma::vec getTurnsLikelihood(arma::mat allpaths,double alpha, arma::vec H, int sim, int model){

  if(sim != 1){
    arma::mat v = arma::zeros(allpaths.n_rows,1);
    allpaths=arma::join_horiz(allpaths,v);
  }
  //Rcpp::Rcout <<  "allpaths.col(4)="<<allpaths.col(4) <<std::endl;

  arma::vec mseMatrix;
  int mseRowIdx = 0;
 
  arma::vec allpath_actions = allpaths.col(0);
  arma::vec allpath_states = allpaths.col(1);
  arma::vec allpath_rewards = allpaths.col(2);
  arma::vec allpath_times = allpaths.col(3);
  arma::vec sessionVec = allpaths.col(4);
  arma::vec uniqSessIdx = arma::unique(sessionVec);

  int episode=1;

  std::shared_ptr<TreeNode> rootS1 = initS1();
  std::shared_ptr<TreeNode> rootS2 = initS2();

  for(unsigned int session = 0; session < (uniqSessIdx.n_elem-1); session++){
    
    int sessId = uniqSessIdx(session);
    arma::uvec episodeIdx = arma::find(allpaths.col(4) == (sessId));
    arma::vec actions = allpath_actions.elem(episodeIdx);
    arma::vec states = allpath_states.elem(episodeIdx);
    arma::vec rewards = allpath_rewards.elem(episodeIdx);
    arma::vec time_taken_for_trial = allpath_times.elem(episodeIdx);
    

    int initState=0;
    bool changeState = false;
    bool returnToInitState = false;
    int score_episode=0;
    float avg_score = 0;
    bool resetVector = true;
    int nrow = actions.n_rows;
    int S = 0;
    int A = 0;
 
    for (int i = 0; i < (nrow-1) ; i++) {

      if(resetVector){
        initState=S;
        //Rcpp::Rcout <<"initState="<<initState<<std::endl;
        resetVector= false;
      }

      int R=allpaths(i,2);

      if(R > 0){
        score_episode = score_episode+1;
      }

      if(sim==1){
        A=allpaths(i,0);
      }else{
        A=allpaths(i,0)-1;
      }

      int S_prime=0;
      if(sim==1){
        S_prime=allpaths((i+1),1);
      }else{
        S_prime=allpaths((i+1),1)-1;
      }

      if(S_prime!=initState){
        changeState = true;
      }else if(S_prime==initState && changeState){
        returnToInitState = true;
      }

      Rcpp::StringVector turns;
      turns = getTurnsFromPaths(path,state);
      int nbOfTurns = turns.length();
    
    for(int j = 0; j< nbOfTurns;j++){
      
      std::string currTurn = Rcpp::as<std::string>(turns(j));
      std::shared_ptr<TreeNode> currNode;
      if(j==0)
      {
        if(sim==1){
          if(isChild(rootS1,currTurn)){
            currNode = getChildNode(rootS1,currTurn);
          }
        }else{
          if(isChild(rootS2,currTurn)){
            currNode = getChildNode(rootS1,currTurn);
          }
        }
        
      }
      // currTurn.erase(std::remove(turns(j).begin(), currTurn.end(), ','), currTurn.end());
      // currTurn.erase(std::remove(turns(j).begin(), currTurn.end(), ' '), currTurn.end());
      //int turnIdx = getTurnIdx(currTurn);

      std::vector<double> credits; 
      credits.push_back(currNode->credit);
      std::vector<double> siblingCredits = getSiblingCredits(currNode);
      credits.insert(std::end(a), std::begin(siblingCredits), std::end(siblingCredits));

      //Change softmax function - input row of credits, first element is always the selected turn, return prob of turn
      arma::vec Y(credits);
      double prob_a = softmax(Y);
      double logProb = log(prob_a);
      mseMatrix.insert_rows(mseMatrix.n_rows, logProb);

    }  
  

      //log_lik=log_lik+ logProb;

      //Check if episode ended
      if(returnToInitState ){
        //Rcpp::Rcout <<  "Inside end episode"<<std::endl;
        changeState = false;
        returnToInitState = false;


        arma::uvec episodeIdx = arma::find(allpaths.col(4) == (episode));
        arma::vec allpath_actions = allpaths.col(0);
        arma::vec actions=allpath_actions.elem(episodeIdx);

        arma::vec allpath_states = allpaths.col(1);
        arma::vec states=allpath_states.elem(episodeIdx);

        arma::vec allpath_times = allpaths.col(3);
        arma::vec time_taken_for_trial=allpath_times.elem(episodeIdx);


        if(sim!=1){
          actions=actions-1;
          allpath_actions = allpath_actions-1;
          states=states-1;
          allpath_states = allpath_states-1;
        }

        avg_score = avg_score + (score_episode-avg_score)/episode;

        H = updateCreditMatrix(H,actions, states, time_taken_for_trial, alpha, score_episode, avg_score,activityMatrix, model);
        //Rcpp::Rcout <<  "H="<<H<<std::endl;
        score_episode=0;
        episode = episode+1;
        resetVector = true;

      }
      S=S_prime;
      //trial=trial+1;

    }
  }

  return(mseMatrix);
}

// [[Rcpp::export()]]
arma::mat getProbMatrix(arma::mat allpaths,double alpha,arma::mat H,int sim, int model){
  //int sim=1;
  int episode=1;
  int nrow = allpaths.n_rows;
  if(sim !=1){
    arma::mat v = arma::zeros(nrow,1);
    allpaths=arma::join_horiz(allpaths,v);
  }
  int S=0;
  if(sim==1){
    S=allpaths(0,1);
  }else{
    S=allpaths(0,1)-1;
  }
  //int A=0;

  int initState=0;
  bool changeState = false;
  bool returnToInitState = false;
  int score_episode=0;

  int i;
  float avg_score = 0;
  bool resetVector = true;

  arma::mat probMatrix_aca=arma::zeros(nrow,12);
  arma::mat mseMatrix=arma::zeros(nrow,4);
  Rcpp::CharacterMatrix H_vals(nrow,2);

  for ( i = 0; i < (nrow-1); i++) {

    if(resetVector){
      initState=S;
      //Rcpp::Rcout <<"initState="<<initState<<std::endl;
      resetVector= false;
    }

    int R=allpaths(i,2);
    if(R>0){
      score_episode = score_episode+1;
    }

    int S_prime=0;
    if(sim==1){
      S_prime=allpaths((i+1),1);
    }else{
      S_prime=allpaths((i+1),1)-1;
    }

    if(sim !=1){
      allpaths(i,4)=episode;
    }

    if(S==0){
      probMatrix_aca.submat(i,6,i,11)=arma::zeros(1,6);
      //Rcpp::Rcout << "probMatrix_aca="<< probMatrix_aca << std::endl;
      for(int act=0;act<6;act++){
        double x = softmax_cpp3(act,0,H);
        probMatrix_aca(i,act)=x;
      }
    }else if(S==1){
      //Rcpp::Rcout << "i=" <<i<< std::endl;
      probMatrix_aca.submat(i,0,i,5)=arma::zeros(1,6);
      //Rcpp::Rcout << "probMatrix_aca="<< probMatrix_aca << std::endl;
      for(int act=0;act<6;act++){
        double x = softmax_cpp3(act,1,H);
        probMatrix_aca(i,(6+act))=x;
      }
    }


    if(S_prime!=initState){
      changeState = true;
    }else if(S_prime==initState && changeState){
      returnToInitState = true;
    }

    //Check if episode ended
    if(returnToInitState ){
      //Rcpp::Rcout <<  "Inside end episode"<<std::endl;
      changeState = false;
      returnToInitState = false;



      arma::uvec episodeIdx = arma::find(allpaths.col(4) == (episode));
      arma::vec allpath_actions = allpaths.col(0);
      arma::vec actions=allpath_actions.elem(episodeIdx);

      arma::vec allpath_states = allpaths.col(1);
      arma::vec states=allpath_states.elem(episodeIdx);

      arma::vec allpath_times = allpaths.col(3);
      arma::vec time_taken_for_trial=allpath_times.elem(episodeIdx);


      if(sim!=1){
        actions=actions-1;
        states=states-1;
        allpath_states=allpath_states-1;
        allpath_actions=allpath_actions-1;
      }

      // rewardIdx= arma::find(actions ==3);
      // score_episode = rewardIdx.n_elem;
      avg_score = avg_score + (score_episode-avg_score)/episode;

      arma::mat activityMatrix = arma::zeros(2,6);


      for(unsigned int state=0;state<2;state++){
        for(unsigned int acts=0;acts<6;acts++){
          arma::uvec state_idx = arma::find(allpath_states(arma::span(0,i))==state);
          arma::uvec act_idx = arma::find(allpath_actions.elem(state_idx)==acts);
          arma::vec time_in_state = allpath_times.elem(state_idx);
          activityMatrix(state,acts)=arma::accu(time_in_state.elem(act_idx))/arma::accu(time_in_state);
        }
      }


      //Rcpp::Rcout << "episode="<<episode<< ", score_episode="<<score_episode<<", avg_score="<<avg_score<<std::endl;

      H = updateCreditMatrix(H, actions, states, time_taken_for_trial, alpha, score_episode, avg_score,activityMatrix, model);

      score_episode=0;
      episode = episode+1;
      resetVector = true;
    }
    S=S_prime;
    //trial=trial+1;
  }
  return(probMatrix_aca);
}
