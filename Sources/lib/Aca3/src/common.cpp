// [[Rcpp::depends(RcppArmadillo)]]
#include <vector>
#include <RcppArmadillo.h>

#include <regex>
#include <RcppArmadilloExtensions/sample.h>

#include "acaCreditUpdate.hpp"


using namespace Rcpp;

inline double softmax_cpp3(int A,int S,arma::mat H){
  //Rcpp::Rcout <<  "S="<< S<<std::endl;
  arma::rowvec v = H.row(S);
  //Rcpp::Rcout <<  v<< std::endl;
  double m=arma::max(v);
  double exp_sum  = std::exp(H(S,0)-m)+std::exp(H(S,1)-m)+std::exp(H(S,2)-m)+std::exp(H(S,3)-m)+std::exp(H(S,4)-m)+std::exp(H(S,5)-m) ;
  double pr_A = (std::exp(H(S,A)-m))/exp_sum;
  
  //  float m=arma::max(v);
  //  v=exp(v-m);
  // // //Rcpp::Rcout << "m=" << m<< std::endl;
  //  double exp_sum  = arma::accu(v) ;
  //  v=v/exp_sum;
  //  double pr_A=v[A];
  
  if(pr_A<0){
    Rcpp::Rcout <<"A="<<A<< ", S=" <<S << std::endl;
    Rcpp::Rcout << H << std::endl;
    Rcpp::Rcout << "m=" <<m << std::endl;
    Rcpp::Rcout << "Numerator=" << std::exp(H(S,A)-m)  << std::endl;
    Rcpp::Rcout << "exp_sum=" << exp_sum  << std::endl;
    // Rcpp::Rcout << "std::exp(H(S,0))-m)=" << std::exp(H(S,0)-m)  << std::endl;
    // Rcpp::Rcout << "std::exp(H(S,1))-m)=" << std::exp(H(S,1)-m)  << std::endl;
    // Rcpp::Rcout << "std::exp(H(S,2))-m)=" << std::exp(H(S,2)-m)  << std::endl;
    // Rcpp::Rcout << "std::exp(H(S,3))-m)=" << std::exp(H(S,3)-m)  << std::endl;
    // Rcpp::Rcout << "std::exp(H(S,4))-m)=" << std::exp(H(S,4)-m)  << std::endl;
    // Rcpp::Rcout << "std::exp(H(S,5))-m)=" << std::exp(H(S,5)-m)  << std::endl;
    Rcpp::Rcout << v  << std::endl;
    
    //stop("logProb is NAN");
    
  }else if(pr_A>1){
    Rcpp::Rcout <<"pr_A="<<pr_A<< " is > 1" << std::endl;
    Rcpp::Rcout <<"A="<<A<< ", S=" <<S << std::endl;
    Rcpp::Rcout << H << std::endl;
    Rcpp::Rcout << "m=" <<m << std::endl;
    Rcpp::Rcout << "Numerator=" << std::exp(H(S,A)-m)  << std::endl;
    Rcpp::Rcout << "exp_sum=" << exp_sum  << std::endl;
    Rcpp::Rcout << "(std::exp(H(S,0))-m)=" << std::exp(H(S,0)-m)  << std::endl;
    Rcpp::Rcout << "(std::exp(H(S,1))-m)=" << std::exp(H(S,1)-m)  << std::endl;
    Rcpp::Rcout << "(std::exp(H(S,2))-m)=" << std::exp(H(S,2)-m)  << std::endl;
    Rcpp::Rcout << "(std::exp(H(S,3))-m)=" << std::exp(H(S,3)-m)  << std::endl;
    Rcpp::Rcout << "(std::exp(H(S,4))-m)=" << std::exp(H(S,4)-m)  << std::endl;
    Rcpp::Rcout << "(std::exp(H(S,5))-m)=" << std::exp(H(S,5)-m)  << std::endl;
  }
  if(R_IsNaN((pr_A))){
    
    Rcpp::Rcout << "exp_sum=" << exp_sum  << std::endl;
    Rcpp::Rcout << "numerator=" << (std::exp(H(S,A)-m))  << std::endl;
    Rcpp::Rcout <<  "pr_A="<< pr_A << std::endl;
    Rcpp::Rcout <<  H<< std::endl;
    Rcpp::Rcout <<"A=" <<A<< ", S=" <<S <<", m="<<m<< std::endl;
    Rcpp::Rcout << "Numerator=" << std::exp(H(S,A)-m)  << std::endl;
    Rcpp::Rcout << "(std::exp(H(S,0))-m)=" << std::exp(H(S,0)-m)  << std::endl;
    Rcpp::Rcout << "(std::exp(H(S,1))-m)=" << std::exp(H(S,1)-m)  << std::endl;
    Rcpp::Rcout << "(std::exp(H(S,2))-m)=" << std::exp(H(S,2)-m)  << std::endl;
    Rcpp::Rcout << "(std::exp(H(S,3))-m)=" << std::exp(H(S,3)-m)  << std::endl;
    Rcpp::Rcout << "(std::exp(H(S,4))-m)=" << std::exp(H(S,4)-m)  << std::endl;
    Rcpp::Rcout << "(std::exp(H(S,5))-m)=" << std::exp(H(S,5)-m)  << std::endl;
    //stop("logProb is NAN");
  }
  
  // if(Rcpp::traits::is_infinite<REALSXP>(prob_a)){
  //   Rcpp::Rcout <<  "Prb of action seclection = 1" << std::endl;;
  // }
  //Rcpp::Rcout <<  "pr_A="<< pr_A<<std::endl;
  return(pr_A);
}

inline double episolonGreedyProb(int A,int S,arma::mat H,double epsilon){
  //Rcpp::Rcout <<  "S="<< S<<std::endl;
  arma::rowvec v = H.row(S);
  //Rcpp::Rcout <<  v<< std::endl;
  double m=arma::max(v);
  double pr_A = 0;
  if(m==H(S,A)){
    pr_A = 1 - epsilon + epsilon/6;
  }else{
    pr_A = 1/6;
  }
  
  return(pr_A);
}

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


int actionSelection(arma::mat H, int S, int method, double epsilon=0){
  int selected_action = -1;
  if(method == 1){
    selected_action = softmax_action_sel(H, S);
  }else if(method == 2){
    selected_action = epsGreedyActionSel(H, S, epsilon);
  }
  return(selected_action);
}

double actionProb(int A, int S, arma::mat H, int method, double epsilon=0){
  double actionProb = -1;
  if(method == 1){
    actionProb = softmax_cpp3(A, S, H);
  }else if(method == 2){
    actionProb = episolonGreedyProb(A, S, H, epsilon);
  }
  return(actionProb);
}

// [[Rcpp::export()]]
arma::mat simulateTrials(arma::mat allpaths, arma::mat H, double alpha, double gamma1, double gamma2, int total_trials, int model, int policyMethod, double epsilon =0){
  
  
  arma::mat R = arma::zeros(2,6);
  R(0,3)=1;
  R(1,3)=1;
  arma::mat generated_data;
  generated_data.fill(-1);
  Rcpp::CharacterMatrix H_vals(total_trials,2);
  Rcpp::NumericVector episodeIndices;


  arma::vec allpath_actions = allpaths.col(0);
  arma::vec allpath_states = allpaths.col(1);
  arma::vec allpath_rewards = allpaths.col(2);
  arma::vec allpath_times = allpaths.col(3);
  arma::vec sessionVec = allpaths.col(4);
  arma::vec uniqSessIdx = arma::unique(sessionVec);
  //Rcpp::Rcout << "sessionVec=" << sessionVec << std::endl;  
  //Rcpp::Rcout << "uniqSessIdx=" << uniqSessIdx << std::endl;  
  int episode=0;
  
  // Loop through each session
  for(unsigned int session = 0; session < (uniqSessIdx.n_elem-1); session++){
    
    int sessId = uniqSessIdx(session);
    // Rcpp::Rcout << "session=" <<session <<", sessId=" << sessId << std::endl;  
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
    //int A_prime=0;
    int score_episode=0;
    float avg_score = 0;

    int S = states_sess(0) - 1;
    arma::mat generated_data_sess(nrow,5);
    //All episodes in new session
    for (int i = 0; i < (nrow) ; i++) {
      // Rcpp::Rcout << "episode=" << i  << std::endl;  
      if(resetVector){
        initState=S;
        //Rcpp::Rcout <<"initState="<<initState<<std::endl;
        resetVector= false;
      }

      if(policyMethod == 1){
        A = actionSelection(H,S,1);
      }else if(policyMethod == 2){
        A = actionSelection(H,S,2,epsilon);
      }
      // Rcpp::Rcout << "S=" << S << ", A=" << A << ", episode=" << episode <<std::endl;

      score_episode = score_episode + R(S,A);
      generated_data_sess(i,0)=A;
      generated_data_sess(i,1)=S;
      generated_data_sess(i,2)=R(S,A);

      arma::uvec act_idx = arma::find(generated_data_sess.col(1)==S && generated_data_sess.col(0)==A);
      arma::uvec actual_act_idx = arma::find(states_sess==(S+1) && actions_sess==(A+1));
      // Rcpp::Rcout << "act_idx.n_elem=" << act_idx.n_elem << ", actual_act_idx.n_elem=" << actual_act_idx.n_elem <<std::endl;

      if(act_idx.n_elem <= actual_act_idx.n_elem){
        // Rcpp::Rcout << "Here1" <<std::endl; 
        generated_data_sess(i,3) = allpaths(actual_act_idx(act_idx.n_elem-1),3);
      }else{
        if(actual_act_idx.n_elem == 0){
          // Rcpp::Rcout << "Here2" <<std::endl; 
          arma::uvec act_idx = arma::find(allpath_states==(S+1) && allpath_actions==(A+1));
          arma::vec act_times = allpath_times.elem(act_idx);
          //Rcpp::Rcout << "act_idx=" << act_idx << ", act_times=" << act_times <<std::endl;
          generated_data_sess(i,3) = arma::mean(act_times);
          // Rcpp::Rcout << "generated_data_sess(i,3)=" << generated_data_sess(i,3) <<std::endl;
        }else{
          // Rcpp::Rcout << "Here3" <<std::endl; 
          arma::uword last_act = act_idx(act_idx.n_elem-1);
          // Rcpp::Rcout << "last_act=" << last_act <<std::endl;
          generated_data_sess(i,3) = generated_data(last_act,3);
        }
      }

      generated_data_sess(i,4) = episode;

      int S_prime=aca_getNextState(S,A);

  
      if(S_prime!=initState){
        changeState = true;
      }else if(S_prime==initState && changeState){
        returnToInitState = true;
      }
      
      //Rcpp::Rcout << "logProb=" << logProb <<std::endl;
      //log_lik=log_lik+ logProb;

     //Check if episode ended
      if(returnToInitState){
        // Rcpp::Rcout <<  "Inside end episode"<<std::endl;
        changeState = false;
        returnToInitState = false;

        arma::vec action_sess = generated_data_sess.col(0);
        arma::vec state_sess = generated_data_sess.col(1);
        arma::vec time_taken_for_trial_sess = generated_data_sess.col(3);
        
          
        arma::uvec episodeIdx = arma::find(generated_data_sess.col(4) == episode);
        arma::vec actions = action_sess.elem(episodeIdx);
        arma::vec states = state_sess.elem(episodeIdx);
        arma::vec time_taken_for_trial = time_taken_for_trial_sess.elem(episodeIdx);

        episode = episode+1;
        resetVector = true;
        avg_score = (avg_score *(episode-1) + (score_episode-avg_score))/episode;

        // Rcpp::Rcout << "episodeIdx=" << episodeIdx<<std::endl;
        // Rcpp::Rcout << "actions=" << actions<<std::endl;
        // Rcpp::Rcout << "states=" << states<<std::endl;
        // Rcpp::Rcout << "time_taken_for_trial=" << time_taken_for_trial<<std::endl;
        //Rcpp::Rcout << "activityMatrix=" <<activityMatrix<<std::endl;
        H = Aca3CreditUpdate(H, actions, states, time_taken_for_trial,  alpha, score_episode);
        //Rcpp::Rcout <<  "H after episode=" << H << std::endl;

        score_episode=0;
      }

      H = gamma1 * H;
      S = S_prime;
      //trial=trial+1;
    }

    H = gamma2*H;
    //Rcpp::Rcout <<  "H after session=" << H<<std::endl;
    generated_data = arma::join_cols(generated_data_sess, generated_data);
    //Rcpp::Rcout <<  "likelihoodVec=" << likelihoodVec<<std::endl;
  }
    return (generated_data);
}


// [[Rcpp::export()]]
arma::vec getPathLikelihood(arma::mat allpaths,double alpha, double gamma1, double gamma2, arma::mat H,int sim, int model, int policyMethod, double epsilon=0, int endTrial=0){

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
  int episode=0;
  
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
    //int A_prime=0;
    int score_episode=0;
    float avg_score = 0;

    int S = 0;
    if (sim == 1){
      S = states_sess(0);
    }else{
      S = states_sess(0) - 1;
    }

    arma::vec likelihoodVec_sess(nrow-1);
    arma::vec episodeVec(nrow-1);
    //All episodes in new session
    for (int i = 0; i < (nrow-1) ; i++) {
    
    //Rcpp::Rcout << "S=" << S << ", A=" << A << ", episode=" << episode <<std::endl;
        if(resetVector){
      initState=S;
      //Rcpp::Rcout <<"initState="<<initState<<std::endl;
      resetVector= false;
      }

      int R=rewards_sess(i);

      if(R > 0){
        score_episode = score_episode+1;
      }

      if(sim==1){
        A=actions_sess(i);
      }else{
        A=actions_sess(i)-1;
      }

      int S_prime=0;
      if(sim==1){
        S_prime=states_sess(i+1);
      }else{
        S_prime=states_sess(i+1)-1;
      }

      episodeVec(i)=episode;
     
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
      if(returnToInitState){
        //Rcpp::Rcout <<  "Inside end episode"<<std::endl;
        changeState = false;
        returnToInitState = false;


        arma::uvec episodeIdx = arma::find(episodeVec == episode);
        arma::vec actions=actions_sess.elem(episodeIdx);
        arma::vec states=states_sess.elem(episodeIdx);
        arma::vec time_taken_for_trial=time_taken_for_trial_sess.elem(episodeIdx);

        if(sim!=1){
          actions=actions-1;
          states=states-1;
        }

        
        episode = episode+1;
        resetVector = true;
        avg_score = (avg_score *(episode-1) + (score_episode-avg_score))/episode;

        // Rcpp::Rcout << "episodeIdx=" << episodeIdx<<std::endl;
        // Rcpp::Rcout << "actions=" << actions<<std::endl;
        // Rcpp::Rcout << "states=" << states<<std::endl;
        // Rcpp::Rcout << "time_taken_for_trial=" << time_taken_for_trial<<std::endl;
        //Rcpp::Rcout << "activityMatrix=" <<activityMatrix<<std::endl;
        H = Aca3CreditUpdate(H, actions, states, time_taken_for_trial,  alpha, score_episode);
        //Rcpp::Rcout <<  "H after episode=" << H << std::endl;

        //H = updateCreditMatrix(H,actions, states, time_taken_for_trial, alpha, score_episode, avg_score,model);
        score_episode=0;
      }

      H = gamma1 * H;
      S = S_prime;
      //trial=trial+1;
    }

    H = gamma2*H;
    //Rcpp::Rcout <<  "H after session=" << H<<std::endl;
    likelihoodVec = arma::join_cols(likelihoodVec, likelihoodVec_sess);
    //Rcpp::Rcout <<  "likelihoodVec=" << likelihoodVec<<std::endl;
  }
    return (likelihoodVec);
}


// [[Rcpp::export()]]
arma::mat getProbMatrix(arma::mat allpaths,double alpha, double gamma1, double gamma2, arma::mat H,int sim, int model, int policyMethod, double epsilon =0){
  
   if(sim != 1){
    arma::mat v = arma::zeros(allpaths.n_rows,1);
    allpaths=arma::join_horiz(allpaths,v);
  }
  //Rcpp::Rcout <<  "allpaths.col(4)="<<allpaths.col(4) <<std::endl;

  arma::mat probMatrix_aca;

  arma::vec allpath_actions = allpaths.col(0);
  arma::vec allpath_states = allpaths.col(1);
  arma::vec allpath_rewards = allpaths.col(2);
  arma::vec allpath_times = allpaths.col(3);
  arma::vec sessionVec = allpaths.col(4);
  arma::vec uniqSessIdx = arma::unique(sessionVec);
  //Rcpp::Rcout << "sessionVec=" << sessionVec << std::endl;  
  //Rcpp::Rcout << "uniqSessIdx=" << uniqSessIdx << std::endl;  
  int episode=0;
  
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
    //int A = 0;
   //int A_prime=0;
    int score_episode=0;
    float avg_score = 0;

    int S = 0;
    if (sim == 1){
      S = states_sess(0);
    }else{
      S = states_sess(0) - 1;
    }

    arma::mat probMatrix_sess((nrow-1),12);
    arma::vec episodeVec(nrow-1);
    
    //All episodes in new session
    for (int i = 0; i < (nrow-1) ; i++) {
    
    //Rcpp::Rcout << "S=" << S << ", A=" << A << ", episode=" << episode <<std::endl;
      if(resetVector){
        initState=S;
        //Rcpp::Rcout <<"initState="<<initState<<std::endl;
        resetVector= false;
      }

      int R=rewards_sess(i);

      if(R > 0){
        score_episode = score_episode+1;
      }

      int S_prime=0;
      if(sim==1){
        S_prime=states_sess(i+1);
      }else{
        S_prime=states_sess(i+1)-1;
      }

      episodeVec(i)=episode;


      if(S_prime!=initState){
        changeState = true;
      }else if(S_prime==initState && changeState){
        returnToInitState = true;
      }

      if(S==0){
        probMatrix_sess.submat(i,6,i,11)=arma::zeros(1,6);
        for(int act=0;act<6;act++){
          double x = 0;
          if(policyMethod == 1){
            x = actionProb(act, 0, H, 1);
          }else if(policyMethod == 2){
            x = actionProb(act, 0, H, 2, epsilon);
          }
          probMatrix_sess(i,act)=x;
          //Rcpp::Rcout <<"S=0" << ", i="<< i << ", x=" << x << std::endl;
        }
      }else if(S==1){
        //Rcpp::Rcout << "i=" <<i<< std::endl;
        probMatrix_sess.submat(i,0,i,5)=arma::zeros(1,6);
        //Rcpp::Rcout << "probMatrix_aca="<< probMatrix_aca << std::endl;
        for(int act=0;act<6;act++){
          double x = 0;
          if(policyMethod == 1){
            x = actionProb(act, 1, H, 1);
          }else if(policyMethod == 2){
            x = actionProb(act, 1, H, 2, epsilon);
          }
          probMatrix_sess(i,(6+act))=x;
          //Rcpp::Rcout << "S=1" << ", i="<< i << ", x=" << x << std::endl;
        }
      }
      
      //Check if episode ended
      if(returnToInitState ){
      //Rcpp::Rcout <<  "Inside end episode"<<std::endl;
        changeState = false;
        returnToInitState = false;


        arma::uvec episodeIdx = arma::find(episodeVec == episode);
        arma::vec actions=actions_sess.elem(episodeIdx);
        arma::vec states=states_sess.elem(episodeIdx);
        arma::vec time_taken_for_trial=time_taken_for_trial_sess.elem(episodeIdx);


        if(sim!=1){
          actions=actions-1;
          states=states-1;
        }

        
        episode = episode+1;
        resetVector = true;
        avg_score = (avg_score *(episode-1) + (score_episode-avg_score))/episode;

        //Rcpp::Rcout << "i=" <<i<< ", episode="<<episode<<std::endl;
        //Rcpp::Rcout << "activityMatrix=" <<activityMatrix<<std::endl;
        H = Aca3CreditUpdate(H, actions, states, time_taken_for_trial,  alpha, score_episode);
        //H = updateCreditMatrix(H,actions, states, time_taken_for_trial, alpha, score_episode, avg_score, model);
        score_episode=0;
        H = gamma1 * H;
      }

      
      S = S_prime;
      //trial=trial+1;
    }

    H = gamma2*H;
    probMatrix_aca = arma::join_cols(probMatrix_aca,probMatrix_sess);
    //Rcpp::Rcout <<  "likelihoodVec=" << likelihoodVec<<std::endl;
  }
    return (probMatrix_aca);
}