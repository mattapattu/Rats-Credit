// [[Rcpp::depends(RcppArmadillo)]]
#include <vector>
#include <RcppArmadillo.h>

#include <regex>
#include <RcppArmadilloExtensions/sample.h>

#include "acaCreditUpdate.hpp"
#include "gbCreditUpdate.hpp"
#include "gbacaCreditUpdate.hpp"
#include "utils.hpp"


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

int softmax_action_sel(arma::mat H,int S){

  // Rcpp::Rcout <<"S="<<S<<", H="<<H<<std::endl;
  arma::rowvec v = H.row(S);
  double m=arma::max(v);

  //Rcpp::Rcout <<"m="<<m<<", v="<<v<<std::endl;

  v=exp(v-m);
  double exp_sum  = arma::accu(v) ;
  v=v/exp_sum;
  IntegerVector actions = seq(0, 5);
  int action_selected = Rcpp::RcppArmadillo::sample(actions, 1, true, v.as_col())[0] ;
  //Rcpp::Rcout <<"action_selected="<<action_selected<<std::endl;

  return(action_selected);

}

int epsGreedyActionSel(arma::mat H, int S, double epsilon){

  // Rcpp::Rcout <<"S="<<S<<", H="<<H<<std::endl;
  arma::rowvec v = H.row(S);
  //Rcpp::Rcout <<"m="<<m<<", v="<<v<<std::endl;

  arma::uword max_idx = v.index_max();
  arma::vec probVector(6);
  probVector.fill(epsilon/6);
  probVector(max_idx) = epsilon/6 + 1-epsilon;
  IntegerVector actions = seq(0, 5);
  int action_selected = Rcpp::RcppArmadillo::sample(actions, 1, true, probVector)[0] ;
  //Rcpp::Rcout <<"action_selected="<<action_selected<<std::endl;

  return(action_selected);

}


int actionSelection(arma::mat H, int S, int method, double epsilon){
  int selected_action = -1;
  if(method == 1){
    selected_action = softmax_action_sel(H, S);
  }else if(method == 2){
    selected_action = epsGreedyActionSel(H, S, epsilon);
  }
  return(selected_action);
}

double actionProb(int A, int S, arma::mat H, int method, double epsilon){
  //Rcpp::Rcout <<"epsilon="<<epsilon<<std::endl;
  double actionProb = -1;
  if(method == 1){
    actionProb = softmax_cpp3(A, S, H);
  }else if(method == 2){
    actionProb = softmaxEps(A, S, H, epsilon);
  }
  return(actionProb);
}

arma::mat updateCreditMatrix(arma::mat H,arma::vec actions, arma::vec states, arma::vec time_taken_for_trial, double alpha, float score_episode, double avg_score, arma::mat activityMatrix, int model){

  if(model==1){
    H = AcaCreditUpdate(H,actions, states, time_taken_for_trial,  alpha, score_episode);

  }else if(model==2){
    H = GbCreditUpdate(H, actions, states, alpha, score_episode, avg_score);

  }else if(model==3){
    H = GbAcaCreditUpdate(H, actions, states, alpha, score_episode, avg_score, activityMatrix);

  }
  return(H);
}


// [[Rcpp::export()]]
arma::mat simulateTrials(arma::mat allpaths, arma::mat H, double alpha,int total_trials,int init_state, int model, int policyMethod, double epsilon=0){
  //arma::mat H = arma::zeros(2,6);
  //H(0,0)=3;
  //H(1,0)=3;
   //Rcpp::Rcout <<"epsilon="<<epsilon<<std::endl;
  arma::mat R= arma::zeros(2,6);
  //R.fill(-1);
  R(0,3)=1;
  R(1,3)=1;
  arma::mat allpaths_aca_model2(total_trials,5);
  allpaths_aca_model2.fill(-1);
  Rcpp::CharacterMatrix H_vals(total_trials,2);
  Rcpp::NumericVector episodeIndices;

  int episode=1;
  int S=init_state;
  int A=0;


  int initState=0;
  bool changeState = false;
  bool returnToInitState = false;
  int score_episode=0;
  //int episodeFin=0;
  //int prev_ses=-1;
  //int trial=0;
  int i;
  float avg_score = 0;
  bool resetVector = true;
  for ( i = 0; i < (total_trials); i++) {

    if(resetVector){
      initState=S;
      resetVector= false;
    }
    //Rcpp::Rcout <<"i="<<i<<", S="<<S<<", H="<<H<<std::endl;

    if(policyMethod == 1){
      A = actionSelection(H,S,1,0);
    }else if(policyMethod == 2){
      A = actionSelection(H,S,2,epsilon);
    }
    
    //Rcpp::Rcout <<"i="<<i<<", A="<<A<<", S="<<S<<std::endl;


    if(R(S,A)==1){
      score_episode = score_episode+1;
    }

    allpaths_aca_model2(i,0)=A;
    allpaths_aca_model2(i,1)=S;
    allpaths_aca_model2(i,2)=R(S,A);


    arma::uvec state_idx = arma::find(allpaths_aca_model2.col(1)==S && allpaths_aca_model2.col(0)==A);
    arma::uvec allpaths_state_idx = arma::find(allpaths.col(1)==(S+1) && allpaths.col(0)==(A+1));

    if(state_idx.n_elem <= allpaths_state_idx.n_elem){
      allpaths_aca_model2(i,3) = allpaths(allpaths_state_idx(state_idx.n_elem-1),3);
    }else{
      allpaths_aca_model2(i,3) = allpaths(allpaths_state_idx(allpaths_state_idx.n_elem-1),3);
    }

    allpaths_aca_model2(i,4)=episode;
    //Rcpp::Rcout <<"i="<<i<<", A="<<A<<", S="<<S<<", state_idx.n_elem="<<state_idx.n_elem<<", allpaths_aca_model2(i,3)="<<allpaths_aca_model2(i,3)<<std::endl;

    int S_prime=aca_getNextState(S,A);

    if(S_prime!=initState){
      changeState = true;
    }else if(S_prime==initState && changeState){
      returnToInitState = true;
    }
    //Rcpp::Rcout <<  "changeState="<< changeState <<  ", returnToInitState="<< returnToInitState<<std::endl;

    if(returnToInitState ){
      //Rcpp::Rcout <<  "Inside end episode"<<std::endl;
      changeState = false;
      returnToInitState = false;

      arma::uvec episodeIdx = arma::find(allpaths_aca_model2.col(4) == (episode));
      arma::vec allpath_actions = allpaths_aca_model2.col(0);
      arma::vec actions=allpath_actions.elem(episodeIdx);

      arma::vec allpath_states = allpaths_aca_model2.col(1);
      arma::vec states=allpath_states.elem(episodeIdx);

      arma::vec allpath_times = allpaths_aca_model2.col(3);
      arma::vec time_taken_for_trial=allpath_times.elem(episodeIdx);

      //rewardIdx= arma::find(actions ==3);
      //score_episode = rewardIdx.n_elem;
      avg_score = avg_score + (score_episode-avg_score)/episode;

      arma::mat activityMatrix = arma::zeros(2,6);

      for(unsigned int state=0;state<2;state++){
        for(unsigned int acts=0;acts<6;acts++){
          arma::vec allpaths_curr_states = allpath_states(arma::span(0,i));
          arma::uvec state_idx = arma::find(allpaths_curr_states==state);
          arma::uvec act_idx = arma::find(allpath_actions.elem(state_idx)==acts);
          arma::vec time_in_state = allpath_times.elem(state_idx);
          activityMatrix(state,acts)=arma::accu(time_in_state.elem(act_idx))/arma::accu(time_in_state);
        }
      }


      //Rcpp::Rcout << "episode="<<episode<< ", score_episode="<<score_episode<<", avg_score="<<avg_score<<std::endl;

      H = updateCreditMatrix(H,actions, states, time_taken_for_trial, alpha, score_episode, avg_score, activityMatrix, model);
      //Rcpp::Rcout << "H="<< H << std::endl;

      score_episode=0;
      episode = episode+1;
      resetVector = true;
    }

    S=S_prime;
  }

    return(allpaths_aca_model2);
}

// [[Rcpp::export()]]
arma::vec getPathLikelihood(arma::mat allpaths,double alpha, arma::mat H,int sim, int model, int policyMethod, double epsilon=0, int endTrial=0){
  //Rcpp::Rcout << "endTrial=" <<endTrial<< ", policyMethod=" <<policyMethod <<", epsilon="<<epsilon<<std::endl;
  int nrow = allpaths.n_rows;
  if(sim !=1){
    arma::mat v = arma::zeros(nrow,1);
    allpaths=arma::join_horiz(allpaths,v);
  }
  //Rcpp::Rcout <<  "allpaths.col(4)="<<allpaths.col(4) <<std::endl;

  arma::vec mseMatrix=arma::zeros(nrow);

  int episode=1;
  int S=0;
  if(sim==1){
    S=allpaths(0,1);
  }else{
    S=allpaths(0,1)-1;
  }
  int A=0;

  int initState=0;
  bool changeState = false;
  bool returnToInitState = false;
  int score_episode=0;
  int i;
  float avg_score = 0;
  bool resetVector = true;
  for ( i = 0; i < (nrow-1); i++) {

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

    if(sim !=1){
      allpaths(i,5)=episode;
    }


    if(S_prime!=initState){
      changeState = true;
    }else if(S_prime==initState && changeState){
      returnToInitState = true;
    }
    
    double prob_a = 0;
    if(policyMethod == 1){
      prob_a = actionProb(A, S, H, 1, 0);
    }else if(policyMethod == 2){
      if(i > endTrial){
            epsilon = 1;
      }
      prob_a = actionProb(A, S, H, 2, epsilon);
    }

    double logProb = log(prob_a);
    mseMatrix(i)=logProb;
    //log_lik=log_lik+ logProb;

    //Check if episode ended
    if(returnToInitState ){
      //Rcpp::Rcout <<  "Inside end episode"<<std::endl;
      changeState = false;
      returnToInitState = false;


      arma::uvec episodeIdx = arma::find(allpaths.col(5) == (episode));
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


      arma::mat activityMatrix = arma::zeros(2,6);

      for(unsigned int state=0;state<2;state++){
        for(unsigned int acts=0;acts<6;acts++){
          arma::vec allpaths_curr_states = allpath_states(arma::span(0,i));
          arma::uvec state_idx = arma::find(allpaths_curr_states==state);
          arma::uvec act_idx = arma::find(allpath_actions.elem(state_idx)==acts);
          arma::vec time_in_state = allpath_times.elem(state_idx);
          //Rcpp::Rcout << "allpaths_curr_states=" <<allpaths_curr_states<<", state_idx="<<state_idx<<std::endl;
          activityMatrix(state,acts)=arma::accu(time_in_state.elem(act_idx))/arma::accu(time_in_state);
        }
      }

      //Rcpp::Rcout << "i=" <<i<< ", episode="<<episode<<std::endl;
      //Rcpp::Rcout << "activityMatrix=" <<activityMatrix<<std::endl;

      H = updateCreditMatrix(H,actions, states, time_taken_for_trial, alpha, score_episode, avg_score,activityMatrix, model);
      //Rcpp::Rcout <<  "H="<<H<<std::endl;
      score_episode=0;
      episode = episode+1;
      resetVector = true;

    }


    S=S_prime;
    //trial=trial+1;

  }
  return(mseMatrix);
}

/* // [[Rcpp::export()]]
arma::vec getPathLikelihood(arma::mat allpaths,double alpha, arma::mat H,int sim, int model, int policyMethod, double epsilon=0, int endTrial=0){

   if(sim != 1){
    arma::mat v = arma::zeros(allpaths.n_rows,1);
    allpaths=arma::join_horiz(allpaths,v);
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
  int episode=1;
  
  // Loop through each session
  for(unsigned int session = 0; session < (uniqSessIdx.n_elem-1); session++){
    
    int sessId = uniqSessIdx(session);
    //Rcpp::Rcout << "session=" <<session <<", sessId=" << sessId << std::endl;  
    arma::uvec sessionIdx = arma::find(allpaths.col(4) == (sessId));
    arma::vec actions_sess = allpath_actions.elem(sessionIdx);
    arma::vec states_sess = allpath_states.elem(sessionIdx);
    arma::vec rewards_sess = allpath_rewards.elem(sessionIdx);
    arma::vec time_taken_for_trial_sess = allpath_times.elem(sessionIdx);

    int initState=0;
    bool changeState = false;
    bool returnToInitState = false;
    bool resetVector = true;
    int nrow = actions_sess.n_rows;
    int A = 0;
    int A_prime=0;
    int score_episode=0;
    float avg_score = 0;

    int S = 0;
    if (sim == 1){
      S = states_sess(0);
    }else{
      S = states_sess(0) - 1;
    }

    arma::vec likelihoodVec_sess(nrow-1);
    //All episodes in new session
    for (int i = 0; i < (nrow-1) ; i++) {
    
    //Rcpp::Rcout << "S=" << S << ", A=" << A << ", episode=" << episode <<std::endl;
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

      if(sim !=1){
        allpaths(i,5)=episode;
      }


      if(S_prime!=initState){
        changeState = true;
      }else if(S_prime==initState && changeState){
        returnToInitState = true;
      }
      
      double prob_a = 0;
      if(policyMethod == 1){
        prob_a = actionProb(A, S, H, 1, 0);
      }else if(policyMethod == 2){
        if(i > endTrial){
              epsilon = 1;
        }
        prob_a = actionProb(A, S, H, 2, epsilon);
      }

      double logProb = log(prob_a);
   
      likelihoodVec_sess(i) = logProb;
      //Rcpp::Rcout << "logProb=" << logProb <<std::endl;
      //log_lik=log_lik+ logProb;

     //Check if episode ended
      if(returnToInitState ){
      //Rcpp::Rcout <<  "Inside end episode"<<std::endl;
        changeState = false;
        returnToInitState = false;


        arma::uvec episodeIdx = arma::find(allpaths.col(5) == (episode));
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

        score_episode=0;
        episode = episode+1;
        resetVector = true;
        avg_score = (avg_score *(episode-1) + (score_episode-avg_score))/episode;

        arma::mat activityMatrix = arma::zeros(2,6);

        for(unsigned int state=0;state<2;state++){
          for(unsigned int acts=0;acts<6;acts++){
            arma::vec allpaths_curr_states = allpath_states(arma::span(0,i));
            arma::uvec state_idx = arma::find(allpaths_curr_states==state);
            arma::uvec act_idx = arma::find(allpath_actions.elem(state_idx)==acts);
            arma::vec time_in_state = allpath_times.elem(state_idx);
            //Rcpp::Rcout << "allpaths_curr_states=" <<allpaths_curr_states<<", state_idx="<<state_idx<<std::endl;
            activityMatrix(state,acts)=arma::accu(time_in_state.elem(act_idx))/arma::accu(time_in_state);
          }
        }

        //Rcpp::Rcout << "i=" <<i<< ", episode="<<episode<<std::endl;
        //Rcpp::Rcout << "activityMatrix=" <<activityMatrix<<std::endl;

        H = updateCreditMatrix(H,actions, states, time_taken_for_trial, alpha, score_episode, avg_score,activityMatrix, model);
        //Rcpp::Rcout <<  "H="<<H<<std::endl;

      }


      S = S_prime;
      //trial=trial+1;
    }
    likelihoodVec = arma::join_cols(likelihoodVec, likelihoodVec_sess);
    //Rcpp::Rcout <<  "likelihoodVec=" << likelihoodVec<<std::endl;
  }
    return (likelihoodVec);
} */

// [[Rcpp::export()]]
arma::mat getProbMatrix(arma::mat allpaths,double alpha,arma::mat H,int sim, int model, int policyMethod, double epsilon=0, int endTrial=0){
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
      allpaths(i,5)=episode;
    }

    if(S==0){
      probMatrix_aca.submat(i,6,i,11)=arma::zeros(1,6);
      //Rcpp::Rcout << "probMatrix_aca="<< probMatrix_aca << std::endl;
      for(int act=0;act<6;act++){
        double x = 0;
        if(policyMethod == 1){
          x = actionProb(act, 0, H, 1, 0);
        }else if(policyMethod == 2){
          if(i > endTrial){
            epsilon = 1;
          }
          x = actionProb(act, 0, H, 2, epsilon);
        }
        probMatrix_aca(i,act)=x;
      }
    }else if(S==1){
      //Rcpp::Rcout << "i=" <<i<< std::endl;
      probMatrix_aca.submat(i,0,i,5)=arma::zeros(1,6);
      //Rcpp::Rcout << "probMatrix_aca="<< probMatrix_aca << std::endl;
      for(int act=0;act<6;act++){
        double x = 0;
        if(policyMethod == 1){
          x = actionProb(act, 1, H, 1, 0);
        }else if(policyMethod == 2){
          if(i > endTrial){
            epsilon = 1;
          }
          x = actionProb(act, 1, H, 2, epsilon);
        }
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



      arma::uvec episodeIdx = arma::find(allpaths.col(5) == (episode));
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


// [[Rcpp::export()]]
arma::mat getEpisodes(arma::mat allpaths){
  int sim = 2;
  int nrow = allpaths.n_rows;
  if(sim !=1){
    arma::mat v = arma::zeros(nrow,1);
    allpaths=arma::join_horiz(allpaths,v);
  }


  int episode=1;
  int S=allpaths(0,1)-1;

  int initState=0;
  bool changeState = false;
  bool returnToInitState = false;
  int i;
  bool resetVector = true;

  for ( i = 0; i < (nrow-1); i++) {

    if(resetVector){
      initState=S;
      //Rcpp::Rcout <<"initState="<<initState<<std::endl;
      resetVector= false;
    }


    //Rcpp::Rcout << "i=" << i << ", episode="<<episode<<std::endl;

    allpaths(i,5)=episode;
    

    int S_prime=allpaths((i+1),1)-1;;
    

    if(S_prime!=initState){
      changeState = true;
    }else if(S_prime==initState && changeState){
      returnToInitState = true;
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
      episode = episode+1;
      resetVector = true;

    }


    S=S_prime;
    //trial=trial+1;

  }
  return(allpaths);
}