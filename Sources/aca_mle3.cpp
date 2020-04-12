// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::depends(BH)]]
#include <vector>
#include <RcppArmadillo.h>
#include <regex>
#include <RcppArmadilloExtensions/sample.h>




using namespace Rcpp;




double softmax_cpp3(int A,int S,arma::mat &H){
  //Rcpp::Rcout <<  "S="<< S<<std::endl;
  arma::rowvec v = H.row(S);
  //Rcpp::Rcout <<  v<< std::endl;
  float m=arma::max(v);
  double exp_sum  = std::exp(H(S,0)-m)+std::exp(H(S,1)-m)+std::exp(H(S,2)-m)+std::exp(H(S,3)-m)+std::exp(H(S,4)-m)+std::exp(H(S,5)-m) ;
  double pr_A = (std::exp(H(S,A)-m))/exp_sum;
  
  // float m=arma::max(v);
  // v=exp(v-m);
  // //Rcpp::Rcout << "m=" << m<< std::endl;
  // double exp_sum  = arma::accu(v) ;
  // v=v/exp_sum;
  // double pr_A=v[A];
  
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
    Rcpp::Rcout << "(std::exp(H(S,0))-m)=" << (H(S,0))-m  << std::endl;
    Rcpp::Rcout << "(std::exp(H(S,0))-m)=" << (std::exp(H(S,1)-m))  << std::endl;
    Rcpp::Rcout << "(std::exp(H(S,0))-m)=" << (std::exp(H(S,2)-m))  << std::endl;
    Rcpp::Rcout << "(std::exp(H(S,0))-m)=" << (std::exp(H(S,3)-m))  << std::endl;
    Rcpp::Rcout << "(std::exp(H(S,0))-m)=" << (std::exp(H(S,4)-m))  << std::endl;
    Rcpp::Rcout << "(std::exp(H(S,0))-m)=" << (std::exp(H(S,5)-m))  << std::endl;
    //stop("logProb is NAN");
  }
  
  // if(Rcpp::traits::is_infinite<REALSXP>(prob_a)){
  //   Rcpp::Rcout <<  "Prb of action seclection = 1" << std::endl;;
  // }    
  //Rcpp::Rcout <<  "pr_A="<< pr_A<<std::endl;
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

int softmax_action_sel(arma::mat &H,int S){
  
  arma::rowvec v = H.row(S);
  //Rcpp::Rcout <<  H<< std::endl;
  float m=arma::max(v);
  v=exp(v-m);
  //Rcpp::Rcout << "m=" << m<< std::endl;
  double exp_sum  = arma::accu(v) ;
  v=v/exp_sum;
  IntegerVector actions = seq(0, 5);
  //Rcpp::Rcout << "v=" << v<< std::endl;
  //Rcpp::Rcout << "actions=" << actions<< std::endl;
  int action_selected = Rcpp::RcppArmadillo::sample(actions, 1, false, v.as_col())[0] ;
  //Rcpp::Rcout << "action_selected=" << action_selected<< std::endl;
  return(action_selected);
  
}

// [[Rcpp::export("aca_gen_sim")]]
arma::mat aca_gen_sim(arma::mat &H,float alpha,int epsLim,int total_trials,int init_state,int model){
  //NumericMatrix likelihood(10,1298);
  //int d;
  //for(d =0;d<9;d++){
  
  //float alpha=alphas[d];
  //arma::mat H = arma::zeros(2,6);
  arma::mat R = arma::zeros(2,6);
  R(0,3)=1;
  R(1,3)=1;
  //Rcpp::NumericMatrix H(2,6);
  //Rcpp::NumericMatrix Visits(2,6);
  
  //Rcpp::Rcout <<  H<< std::endl;
  //Rcpp::Rcout <<  Visits<< std::endl;
  arma::mat allpaths_aca_model2 = arma::zeros(total_trials,4);
  
  //double log_lik=0;
  
  int episode=1;
  int S=init_state;
  int A=0;
  arma::vec actions(1);
  actions.fill(-1);
  arma::vec states(1);
  states.fill(-1);
  
  
  int initState=0;
  bool changeState = false;
  bool returnToInitState = false;
  int score_episode=0;
  int episodeFin=0;
  //int prev_ses=-1;
  //int trial=0;
  int i;
  int avg_score = 0;
  bool resetVector = true;
  for ( i = 0; i < (total_trials-1); i++) {
    
    if(resetVector){
      initState=S;
      //Rcpp::Rcout <<"initState="<<initState<<std::endl;
      resetVector= false;
    }
    
    
    //  trial=std::atoi(enreg_pos(trial_pos,5));
    //Rcpp::Rcout <<"i="<<i<<  ", trial="<<trial<<std::endl;
    
    //Rcpp::Rcout <<  "trial_pos="<<trial_pos << ", enreg_trial="<< std::atoi(enreg_pos(trial_pos,5)) <<std::endl;
    //Rcpp::Rcout <<"H="<<H<<std::endl;
    A=softmax_action_sel(H,S);
    
    
    if(R(S,A)>0){
      score_episode = score_episode+1;
    }
    //Rcpp::Rcout <<"i="<<i<<  ", A="<<A<<", S="<<S<<std::endl;
    
    allpaths_aca_model2(i,0)=A;
    allpaths_aca_model2(i,1)=S;
    allpaths_aca_model2(i,2)=R(S,A);
    allpaths_aca_model2(i,3)=-1;
    
    int S_prime=aca_getNextState(S,A);
    
    //Rcpp::Rcout << "A="<< A << ", S=" << S << ", S_prime="<< S_prime << std::endl;
    int sz = actions.n_elem;
    actions.resize(sz+1);
    actions(sz) = A;
    
    states.resize(sz+1);
    states(sz)=S;
    
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
      episodeFin=episodeFin+1;
      IntegerVector all_actions =  seq(0, 5);
      if((episodeFin == epsLim) || (i==(total_trials-1))){
        episode = episode+1;
        //Rcpp::Rcout <<  "episodeFin="<< episodeFin<<", episodeFin-epsLim="<<(episodeFin-epsLim)<<std::endl;
        
        //Rcpp::Rcout <<  "Inside update H"<<std::endl;
        //int total_actions= actions.n_elem;
        //Rcpp::Rcout <<  "total_actions="<< total_actions<<std::endl;
        //Rcpp::Rcout <<  "actions="<< actions<<std::endl;
        //Rcpp::Rcout <<  "time_taken_for_trial="<< time_taken_for_trial<<std::endl;
        
        if(model==1){
          arma::uvec state1_idx = find(states==0);
          arma::vec uniq_state1 = arma::unique(actions.elem(state1_idx));
          
          //Rcpp::Rcout << "state1 actions="<<actions.elem(state1_idx)<<std::endl;
          
          for(unsigned int l=0;l< uniq_state1.n_elem;l++){
            if(uniq_state1(l)==-1){
              continue;
            }
            double  curr_action = uniq_state1(l);
            //Rcpp::Rcout << "state1 actions="<<actions.elem(state1_idx)<<std::endl;
            //Rcpp::Rcout <<  "episode="<<episode<<", state="<<1<<", curr_action="<<curr_action<<std::endl;
            arma::uvec a=arma::find(actions.elem(state1_idx)==curr_action);
            double activity = a.n_elem/(1.0*state1_idx.n_elem);
            
            
            H(0,curr_action)= H(0,curr_action)+alpha*(score_episode*activity);
            //double delta_H = alpha*(score_episode-avg_score)*(1-softmax_cpp(curr_action,0,H))*activity;
            //H(0,curr_action)= H(0,curr_action)+delta_H;
            //Rcpp::Rcout << "H(0,curr_action)="<<H(0,curr_action) <<", curr_action="<<curr_action<<", score_episode="<<score_episode<< ", a.n_elem="<< a.n_elem<<", state1_idx.n_elem="<<state1_idx.n_elem<<std::endl;
            // if(H(0,curr_action) <0){
            //   Rcpp::Rcout <<  "H(0,curr_action)="<<H(0,curr_action)<<", curr_action="<<Visits(0,curr_action)<< std::endl;
            // }
            if(R_IsNaN((H(0,curr_action)))){
              Rcpp::Rcout <<  "state="<<0<<", action="<<curr_action<< std::endl;
              //Rcpp::Rcout <<  "H="<< H(0,curr_action)<<std::endl;
              //Rcpp::Rcout <<  "Visits="<< Visits(0,curr_action)<<std::endl;
              stop("H is NAN");
            }else if(H(0,curr_action) == R_PosInf){
              Rcpp::Rcout <<  "state="<<0<<", action="<<curr_action <<std::endl;
              stop("H is Inf");
            }else if(H(0,curr_action) < 0){
              // Rcpp::Rcout <<  "episode="<<episode<<", state="<<1<<", curr_action="<<curr_action<<", score_episode="<<score_episode<<", activity="<<activity<<std::endl;
              // Rcpp::Rcout <<  H<< std::endl;
              
            }
            
          }
          
          
          
          arma::uvec state2_idx = find(states==1);
          arma::vec uniq_state2 = arma::unique(actions.elem(state2_idx));
          
          //Rcpp::Rcout << "state2 actions="<<actions.elem(state2_idx)<<std::endl;
          
          for(unsigned int l=0;l< uniq_state2.n_elem;l++){
            if(uniq_state2(l)==-1){
              continue;
            }
            double  curr_action = uniq_state2(l);
            //Rcpp::Rcout << "state2 actions="<<actions.elem(state2_idx)<<std::endl;
            //Rcpp::Rcout <<  "episode="<<episode<<", state="<<2<<", curr_action="<<curr_action<<std::endl;
            arma::uvec a=arma::find(actions.elem(state2_idx)==curr_action);
            double activity = a.n_elem/(1.0*state2_idx.n_elem);
            
            H(1,curr_action)= H(1,curr_action)+alpha*(score_episode*activity);
            //Rcpp::Rcout << "H(1,curr_action)="<<H(1,curr_action) <<", curr_action="<<curr_action<<", score_episode="<<score_episode<< ", a.n_elem="<< a.n_elem<<", state2_idx.n_elem="<<state2_idx.n_elem<<std::endl;
            //H(1,curr_action)= H(1,curr_action)+(alpha*(score_episode-avg_score)*(1-softmax_cpp(curr_action,1,H))*activity);
            // if(H(1,curr_action) <0){
            //   Rcpp::Rcout <<  "H(1,curr_action)="<<H(1,curr_action)<<", Visits(1,curr_action)="<<Visits(1,curr_action)<< std::endl;
            // }
            if(R_IsNaN((H(1,curr_action)))){
              Rcpp::Rcout <<  "state="<<1<<", action="<<curr_action<< std::endl;
              //Rcpp::Rcout <<"epsLim=" <<epsLim<< "activity="<< activity<<std::endl;
              stop("H is NAN");
            }else if(H(1,curr_action) == R_PosInf){
              Rcpp::Rcout <<  "state="<<1<<", action="<<curr_action <<std::endl;
              //Rcpp::Rcout <<"epsLim=" <<epsLim<< ", activity="<< activity<<", score_episode="<<score_episode<<std::endl;
              stop("H is Inf");
            }else if(H(1,curr_action) < 0){
              // Rcpp::Rcout <<  "episode="<<episode<<", state="<<2<<", curr_action="<<curr_action<<", score_episode="<<score_episode<<", activity="<<activity<<std::endl;
              // Rcpp::Rcout <<  H<< std::endl;
              
            }
          }
        }else if(model==2){
          arma::uvec state1_idx = find(states==0);
          arma::vec uniq_state1 = arma::unique(actions.elem(state1_idx));
          
          //Rcpp::Rcout <<  "states="<< states<<std::endl;
          avg_score = avg_score + (score_episode-avg_score)/episode;
          
          for(unsigned int l=0;l< uniq_state1.n_elem;l++){
            if(uniq_state1(l)==-1){
              continue;
            }
            double  curr_action = uniq_state1(l)*1.00;
            //Rcpp::Rcout << "state1 actions="<<actions.elem(state1_idx)<<std::endl;
            //Rcpp::Rcout <<  "episode="<<episode<<", state="<<1<<", curr_action="<<curr_action<<std::endl;
            arma::uvec a=arma::find(actions.elem(state1_idx)==curr_action);
            //Rcpp::Rcout << "a.n_elem="<<a.n_elem<<std::endl;
            double activity = a.n_elem/(1.0*state1_idx.n_elem);
            
            //Rcpp::Rcout << "H(0,5)="<<H(0,5) <<", reward=" <<score_episode-avg_score<<", activity="<<activity<<std::endl;
            
            //H(0,curr_action)= H(0,curr_action)+alpha*(score_episode*activity/Visits(0,curr_action));
            double delta_H = alpha*(score_episode-avg_score)*(1-softmax_cpp3(curr_action,0,H))*activity;
            H(0,curr_action)= H(0,curr_action)+delta_H;
            //Rcpp::Rcout << "H(0,5)="<<H(0,5) <<", reward=" <<score_episode-avg_score<< ", delta_H="<< delta_H<<", activity="<<activity<<std::endl;
            // if(H(0,curr_action) <0){
            //   Rcpp::Rcout <<  "H(0,curr_action)="<<H(0,curr_action)<<", curr_action="<<Visits(0,curr_action)<< std::endl;
            // }
            if(R_IsNaN((H(0,curr_action)))){
              Rcpp::Rcout <<  "state="<<0<<", action="<<curr_action<< std::endl;
              //Rcpp::Rcout <<  "H="<< H(0,curr_action)<<std::endl;
              //Rcpp::Rcout <<  "Visits="<< Visits(0,curr_action)<<std::endl;
              stop("H is NAN");
            }else if(H(0,curr_action) == R_PosInf||H(0,curr_action) == R_NegInf){
              Rcpp::Rcout << "H(0,curr_action)="<<H(0,curr_action) <<", reward=" <<score_episode-avg_score<< ", delta_H="<< delta_H<<", activity="<<activity<<std::endl;
              stop("H is Inf");
            }
            
          }
          
          IntegerVector setdiff_state1 = Rcpp::setdiff(all_actions,IntegerVector(uniq_state1.begin(),uniq_state1.end()));
          //Rcpp::Rcout <<  "setdiff_state1="<<setdiff_state1 << std::endl;
          
          for(unsigned int l=0;l< setdiff_state1.size();l++){
            double  curr_action = setdiff_state1(l);
            H(0,curr_action)= H(0,curr_action)-(alpha*(score_episode-avg_score)*(softmax_cpp3(curr_action,0,H))/state1_idx.n_elem);
            
            //Rcpp::Rcout << "curr_action="<<curr_action<<" ,H(0,5)="<<H(0,5) <<", reward=" <<score_episode-avg_score<< ", softmax_cpp3(curr_action,0,H)="<< softmax_cpp3(curr_action,0,H)<<", state1_idx.n_elem="<<state1_idx.n_elem<<std::endl;
            // if(H(0,curr_action) < 0){
            //   Rcpp::Rcout <<  "H(0,curr_action)="<<H(0,curr_action)<<", Visits(0,curr_action)="<<Visits(0,curr_action)<< std::endl;
            // }
            if(R_IsNaN((H(0,curr_action)))){
              Rcpp::Rcout <<  "state="<<0<<", action="<<curr_action<< std::endl;
              stop("H is NAN");
            }else if(H(0,curr_action) == R_PosInf||H(0,curr_action) == R_NegInf){
              Rcpp::Rcout << "H(0,curr_action)="<<H(0,curr_action) <<", reward=" <<score_episode-avg_score<<", state1_idx.n_elem="<<state1_idx.n_elem<<std::endl;
              stop("H is Inf");
            }
          }
          
          
          
          arma::uvec state2_idx = find(states==1);
          arma::vec uniq_state2 = arma::unique(actions.elem(state2_idx));
          
          for(unsigned int l=0;l< uniq_state2.n_elem;l++){
            if(uniq_state2(l)==-1){
              continue;
            }
            double  curr_action = uniq_state2(l);
            //Rcpp::Rcout << "state2 actions="<<actions.elem(state2_idx)<<std::endl;
            //Rcpp::Rcout <<  "episode="<<episode<<", state="<<2<<", curr_action="<<curr_action<<std::endl;
            arma::uvec a=arma::find(actions.elem(state2_idx)==curr_action);
            double activity = a.n_elem/(1.0*state2_idx.n_elem);
            
            //H(1,curr_action)= H(1,curr_action)+alpha*(score_episode*activity);
            double delta_H = alpha*(score_episode-avg_score)*(1-softmax_cpp3(curr_action,1,H))*activity;
            H(1,curr_action)= H(1,curr_action)+delta_H;
            // if(H(1,curr_action) <0){
            //   Rcpp::Rcout <<  "H(1,curr_action)="<<H(1,curr_action)<<", Visits(1,curr_action)="<<Visits(1,curr_action)<< std::endl;
            // }
            if(R_IsNaN((H(1,curr_action)))){
              Rcpp::Rcout <<  "state="<<1<<", action="<<curr_action<< std::endl;
              //Rcpp::Rcout <<"epsLim=" <<epsLim<< "activity="<< activity<<std::endl;
              stop("H is NAN");
            }else if(H(1,curr_action) == R_PosInf||H(1,curr_action) == R_NegInf){
              Rcpp::Rcout << "H(1,curr_action)="<<H(1,curr_action) <<", reward=" <<score_episode-avg_score<< ", delta_H="<< delta_H<<", activity="<<activity<<std::endl;
              //Rcpp::Rcout <<"epsLim=" <<epsLim<< ", activity="<< activity<<", score_episode="<<score_episode<<std::endl;
              stop("H is Inf");
            }
          }
          
          IntegerVector setdiff_state2 = Rcpp::setdiff(all_actions,IntegerVector(uniq_state2.begin(),uniq_state2.end()));
          //Rcpp::Rcout <<  "setdiff_state2="<<setdiff_state2 << std::endl;
          
          for(unsigned int l=0;l< setdiff_state2.size();l++){
            double  curr_action = setdiff_state2(l);
            
            H(1,curr_action)= H(1,curr_action)-(alpha*(score_episode-avg_score)*(softmax_cpp3(curr_action,1,H))/state2_idx.n_elem);
            
            // if(H(1,curr_action) < 0){
            //   Rcpp::Rcout <<  "H(1,curr_action)="<<H(1,curr_action)<<", Visits(1,curr_action)="<<Visits(1,curr_action)<< std::endl;
            //  }
            if(R_IsNaN((H(1,curr_action)))){
              Rcpp::Rcout <<  "state="<<1<<", action="<<curr_action <<std::endl;
              stop("H is NAN");
            }else if(H(1,curr_action) == R_PosInf){
              Rcpp::Rcout << "H(0,curr_action)="<<H(0,curr_action) <<", reward=" <<score_episode-avg_score<<", state2_idx.n_elem="<<state2_idx.n_elem<<std::endl;
              stop("H is Inf");
            }
          }
          
        }
        score_episode=0;
        episodeFin=0;
        
        actions=arma::vec(1);
        actions.fill(-1);
        states=arma::vec(1);
        states.fill(-1);
        //Rcpp::Rcout <<"actions="<<actions<<std::endl;
        //Rcpp::Rcout << "states="<<states<<std::endl;
        resetVector = true;
        
      }
    }
    S=S_prime; 
  }
  return(allpaths_aca_model2);
  
  
}

// [[Rcpp::export("aca_mle_lik")]]
double aca_mle_lik(Rcpp::NumericMatrix allpaths,float alpha,int epsLim, arma::mat &H,int model,int sim){
  //NumericMatrix likelihood(10,1298);
  //int d;
  //for(d =0;d<9;d++){
  
  //float alpha=alphas[d];
  //arma::mat H = arma::zeros(2,6);
  //Rcpp::NumericMatrix H(2,6);
  //Rcpp::NumericMatrix Visits(2,6);
  
  //Rcpp::Rcout <<  H<< std::endl;
  //Rcpp::Rcout <<  Visits<< std::endl;
  
  double log_lik=0;
  
  int episode=1;
  int S=0;
  if(sim==1){
    S=allpaths(0,1);
  }else{
    S=allpaths(0,1)-1;
  }
  int A=0;
  arma::vec actions(1);
  actions.fill(-1);
  arma::vec states(1);
  states.fill(-1);
  arma::vec time_taken_for_trial(1);
  time_taken_for_trial.fill(-1);
  
  int initState=0;
  bool changeState = false;
  bool returnToInitState = false;
  int score_episode=0;
  int episodeFin=0;
  //int prev_ses=-1;
  int nrow = allpaths.nrow();
  //int trial=0;
  int i;
  int avg_score = 0;
  bool resetVector = true;
  for ( i = 1; i < (nrow-1); i++) {
    
    if(resetVector){
      initState=S;
      //Rcpp::Rcout <<"initState="<<initState<<std::endl;
      resetVector= false;
    }
    
    
    //  trial=std::atoi(enreg_pos(trial_pos,5));
    //Rcpp::Rcout <<"i="<<i<<  ", trial="<<trial<<std::endl;
    int R=allpaths(i,2);
    if(R>0){
      score_episode = score_episode+1;
    }         
    
    //Rcpp::Rcout <<  "trial_pos="<<trial_pos << ", enreg_trial="<< std::atoi(enreg_pos(trial_pos,5)) <<std::endl;
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
    
    if(S_prime<0){
      continue;
    }
    //Rcpp::Rcout << "A="<< A << ", S=" << S << ", S_prime="<< S_prime << std::endl;
    int sz = actions.n_elem;
    actions.resize(sz+1);
    actions(sz) = A;
    
    states.resize(sz+1);
    states(sz)=S;
    
    time_taken_for_trial.resize(sz+1);
    float time = allpaths(i,3);
    time_taken_for_trial(sz)= time;
    
    if(S_prime!=initState){
      changeState = true;
    }else if(S_prime==initState && changeState){
      returnToInitState = true;
    }
    //Rcpp::Rcout <<  "changeState="<< changeState <<  ", returnToInitState="<< returnToInitState<<std::endl;
    
    double prob_a = softmax_cpp3(A,S,H);
    
    //Rcpp::Rcout << "i=" << i  << std::endl;
    //Rcpp::Rcout << "prob_a=" << prob_a  << std::endl;
    
    double logProb = log(prob_a);
    //Rcpp::Rcout << "i=" << i  << std::endl;
    //Rcpp::Rcout << "prob_a=" << prob_a  <<", i=" << i  << std::endl;
    if(logProb==R_PosInf){
      // Rcpp::Rcout <<  "logProb==R_PosInf"<< std::endl;
      // Rcpp::Rcout <<"epsLim=" <<epsLim<<", alpha=" << alpha  << std::endl;
      // Rcpp::Rcout << "i=" << i  << std::endl;
      // Rcpp::Rcout <<  "A="<< A << ", S=" << S << ", S_prime="<< S_prime << std::endl;
      // Rcpp::Rcout << "prob_a=" << prob_a << std::endl;
      // Rcpp::Rcout << "logProb=" << logProb << std::endl;
      // 
      // Rcpp::Rcout <<  H<< std::endl;
      // //Rcpp::Rcout <<  Visits<< std::endl;
      // stop("logProb==R_PosInf");
      //return(100000);
    }else if( prob_a >1){
      // Rcpp::Rcout <<  "prob_a >1"<< std::endl;
      // Rcpp::Rcout <<"epsLim=" <<epsLim<<", alpha=" << alpha  << std::endl;
      // Rcpp::Rcout << "i=" << i  << std::endl;
      // Rcpp::Rcout <<  "A="<< A << ", S=" << S << ", S_prime="<< S_prime << std::endl;
      // Rcpp::Rcout << "prob_a=" << prob_a << std::endl;
      // Rcpp::Rcout << "logProb=" << logProb << std::endl;
      // 
      // Rcpp::Rcout <<  H<< std::endl;
      // stop("prob_a >1");
      //return(100000);
    }
    
    log_lik=log_lik+ logProb;
    //Rcpp::Rcout << "log_lik=" << log_lik << std::endl;
    if(log_lik == R_NegInf){
      // Rcpp::Rcout <<  "log_lik == R_NegInf"<< std::endl;
      // Rcpp::Rcout <<"epsLim=" <<epsLim<<", alpha=" << alpha  << std::endl;
      // Rcpp::Rcout << "i=" << i  << std::endl;
      // Rcpp::Rcout <<  "A="<< A << ", S=" << S << ", S_prime="<< S_prime << std::endl;
      // Rcpp::Rcout << "prob_a=" << prob_a << std::endl;
      // Rcpp::Rcout << "logProb=" << logProb << std::endl;
      // 
      // Rcpp::Rcout <<  H<< std::endl;
      // stop("log_lik == R_NegInf");
    }
    //Rcpp::Rcout << "log_lik=" <<log_lik<<", prob_a=" << prob_a  <<", i=" << i  << std::endl;
    //Check if episode ended
    if(returnToInitState ){
      //Rcpp::Rcout <<  "Inside end episode"<<std::endl;
      changeState = false;
      returnToInitState = false;
      episodeFin=episodeFin+1;
      
      //Rcpp::Rcout <<  "episode="<< episode<<", actions="<<actions<<std::endl;
      IntegerVector all_actions =  seq(0, 5);
      if(episodeFin == epsLim || i==(nrow-1)){
        episode = episode+1;
        
        if(model==1){
          arma::uvec state1_idx = find(states==0);
          arma::vec uniq_state1 = arma::unique(actions.elem(state1_idx));
          
          //Rcpp::Rcout <<  "state1_idx="<< state1_idx<<std::endl;
          for(unsigned int l=0;l< uniq_state1.n_elem;l++){
            if(uniq_state1(l)==-1){
              continue;
            }
            double  curr_action = uniq_state1(l);
            //Rcpp::Rcout << "state1 actions="<<actions.elem(state1_idx)<<std::endl;
            //Rcpp::Rcout <<  "episode="<<episode<<", state="<<1<<", curr_action="<<curr_action<<std::endl;
            arma::uvec a=arma::find(actions.elem(state1_idx)==curr_action);
            arma::vec time_s1 =  time_taken_for_trial.elem(state1_idx);
            double total_time_spent_in_state1 = arma::accu(time_s1);
            double activity=0;
            if(total_time_spent_in_state1>0){
              activity = a.n_elem*1000/total_time_spent_in_state1;
            }else{
              activity = a.n_elem/(1.0*state1_idx.n_elem);
            }
            
            
            
            H(0,curr_action)= H(0,curr_action)+alpha*(score_episode*activity);
            //double delta_H = alpha*(score_episode-avg_score)*(1-softmax_cpp(curr_action,0,H))*activity;
            //H(0,curr_action)= H(0,curr_action)+delta_H;
            //Rcpp::Rcout << "H(0,curr_action)="<<H(0,curr_action) <<", reward=" <<score_episode-avg_score<< ", delta_H="<< delta_H<<std::endl;
            // if(H(0,curr_action) <0){
            //   Rcpp::Rcout <<  "H(0,curr_action)="<<H(0,curr_action)<<", curr_action="<<Visits(0,curr_action)<< std::endl;
            // }
            if(R_IsNaN((H(0,curr_action)))){
              Rcpp::Rcout <<  "state="<<0<<", action="<<curr_action<< std::endl;
              //Rcpp::Rcout <<  "H="<< H(0,curr_action)<<std::endl;
              //Rcpp::Rcout <<  "Visits="<< Visits(0,curr_action)<<std::endl;
              stop("H is NAN");
            }else if(H(0,curr_action) == R_PosInf){
              Rcpp::Rcout <<  "state="<<0<<", action="<<curr_action<<std::endl;
              stop("H is Inf");
            }else if(H(0,curr_action) < 0){
              // Rcpp::Rcout <<  "episode="<<episode<<", state="<<1<<", curr_action="<<curr_action<<", score_episode="<<score_episode<<", activity="<<activity<<std::endl;
              // Rcpp::Rcout <<  H<< std::endl;
              
            }
            
          }
          
          
          
          arma::uvec state2_idx = find(states==1);
          arma::vec uniq_state2 = arma::unique(actions.elem(state2_idx));
          
          for(unsigned int l=0;l< uniq_state2.n_elem;l++){
            if(uniq_state2(l)==-1){
              continue;
            }
            double  curr_action = uniq_state2(l);
            //Rcpp::Rcout << "state2 actions="<<actions.elem(state2_idx)<<std::endl;
            //Rcpp::Rcout <<  "episode="<<episode<<", state="<<2<<", curr_action="<<curr_action<<std::endl;
            arma::uvec a=arma::find(actions.elem(state2_idx)==curr_action);
            arma::vec time_s2 =  time_taken_for_trial.elem(state2_idx);
            double total_time_spent_in_state2 = arma::accu(time_s2);
            double activity=0;
            if(total_time_spent_in_state2>0){
              activity = a.n_elem*1000/total_time_spent_in_state2;
            }else{
              activity = a.n_elem/(1.0*state2_idx.n_elem);
            }
            
            H(1,curr_action)= H(1,curr_action)+alpha*(score_episode*activity);
            //H(1,curr_action)= H(1,curr_action)+(alpha*(score_episode-avg_score)*(1-softmax_cpp(curr_action,1,H))*activity);
            // if(H(1,curr_action) <0){
            //   Rcpp::Rcout <<  "H(1,curr_action)="<<H(1,curr_action)<<", Visits(1,curr_action)="<<Visits(1,curr_action)<< std::endl;
            // }
            if(R_IsNaN((H(1,curr_action)))){
              Rcpp::Rcout <<  "state="<<1<<", action="<<curr_action<< std::endl;
              //Rcpp::Rcout <<"epsLim=" <<epsLim<< "activity="<< activity<<std::endl;
              stop("H is NAN");
            }else if(H(1,curr_action) == R_PosInf){
              Rcpp::Rcout <<  "state="<<1<<", action="<<curr_action<<std::endl;
              //Rcpp::Rcout <<"epsLim=" <<epsLim<< ", activity="<< activity<<", score_episode="<<score_episode<<std::endl;
              stop("H is Inf");
            }else if(H(1,curr_action) < 0){
              // Rcpp::Rcout <<  "episode="<<episode<<", state="<<2<<", curr_action="<<curr_action<<", score_episode="<<score_episode<<", activity="<<activity<<std::endl;
              // Rcpp::Rcout <<  H<< std::endl;
              
            }
          }
        }else if(model==2){
          
          // UPDATE CREDIT OF STATE 1 ACTIONS
          
          arma::uvec state1_idx = find(states==0);
          arma::vec uniq_state1 = arma::unique(actions.elem(state1_idx));
          
          //Rcpp::Rcout <<  "state1_idx="<< state1_idx<<std::endl;
          avg_score = avg_score + (score_episode-avg_score)/episode;
          
          // UPDATE CREDIT OF STATE 1 ACTIONS SECLECTED DURING LAST N EPISODS
          for(unsigned int l=0;l< uniq_state1.n_elem;l++){
            if(uniq_state1(l)==-1){
              continue;
            }
            double  curr_action = uniq_state1(l)*1.00;
            //Rcpp::Rcout << "state1 actions="<<actions.elem(state1_idx)<<std::endl;
            //Rcpp::Rcout <<  "episode="<<episode<<", state="<<1<<", curr_action="<<curr_action<<std::endl;
            arma::uvec a=arma::find(actions.elem(state1_idx)==curr_action);
            //Rcpp::Rcout << "a.n_elem="<<a.n_elem<<std::endl;
            arma::vec time_s1 =  time_taken_for_trial.elem(state1_idx);
            double total_time_spent_in_state1 = arma::accu(time_s1);
            double activity=0;
            if(total_time_spent_in_state1>0){
              activity = a.n_elem*1000/total_time_spent_in_state1;
            }else{
              activity = a.n_elem/(1.0*state1_idx.n_elem);
            }
            
            //Rcpp::Rcout << "H(0,5)="<<H(0,5) <<", reward=" <<score_episode-avg_score<<", activity="<<activity<<std::endl;
            
            //H(0,curr_action)= H(0,curr_action)+alpha*(score_episode*activity/Visits(0,curr_action));
            double delta_H = alpha*(score_episode-avg_score)*(1-softmax_cpp3(curr_action,0,H))*activity;
            H(0,curr_action)= H(0,curr_action)+delta_H;
            //Rcpp::Rcout << "H(0,5)="<<H(0,5) <<", reward=" <<score_episode-avg_score<< ", delta_H="<< delta_H<<", activity="<<activity<<std::endl;
            // if(H(0,curr_action) <0){
            //   Rcpp::Rcout <<  "H(0,curr_action)="<<H(0,curr_action)<<", curr_action="<<Visits(0,curr_action)<< std::endl;
            // }
            if(R_IsNaN((H(0,curr_action)))){
              Rcpp::Rcout <<  "state="<<0<<", action="<<curr_action<< std::endl;
              //Rcpp::Rcout <<  "H="<< H(0,curr_action)<<std::endl;
              //Rcpp::Rcout <<  "Visits="<< Visits(0,curr_action)<<std::endl;
              stop("H is NAN");
            }else if(H(0,curr_action) == R_PosInf){
              Rcpp::Rcout <<  "state="<<0<<", action="<<curr_action<<std::endl;
              stop("H is Inf");
            }
            
          }
          
          // UPDATE CREDIT OF STATE 1 ACTIONS NOT SELECTED DURING LAST N EPISODS
          
          IntegerVector setdiff_state1 = Rcpp::setdiff(all_actions,IntegerVector(uniq_state1.begin(),uniq_state1.end()));
          //Rcpp::Rcout <<  "setdiff_state1="<<setdiff_state1 << std::endl;
          
          
          for(unsigned int l=0;l< setdiff_state1.size();l++){
            double  curr_action = setdiff_state1(l);
            H(0,curr_action)= H(0,curr_action)-(alpha*(score_episode-avg_score)*(softmax_cpp3(curr_action,0,H))/state1_idx.n_elem);
            
            //Rcpp::Rcout << "curr_action="<<curr_action<<" ,H(0,5)="<<H(0,5) <<", reward=" <<score_episode-avg_score<< ", softmax_cpp3(curr_action,0,H)="<< softmax_cpp3(curr_action,0,H)<<", state1_idx.n_elem="<<state1_idx.n_elem<<std::endl;
            // if(H(0,curr_action) < 0){
            //   Rcpp::Rcout <<  "H(0,curr_action)="<<H(0,curr_action)<<", Visits(0,curr_action)="<<Visits(0,curr_action)<< std::endl;
            // }
            if(R_IsNaN((H(0,curr_action)))){
              Rcpp::Rcout <<  "state="<<0<<", action="<<curr_action<< std::endl;
              stop("H is NAN");
            }else if(H(0,curr_action) == R_PosInf){
              Rcpp::Rcout <<  "state="<<0<<", action="<<curr_action <<std::endl;
              stop("H is Inf");
            }
          }
          
          // UPDATE CREDIT OF STATE 2 ACTIONS
          
          arma::uvec state2_idx = find(states==1);
          arma::vec uniq_state2 = arma::unique(actions.elem(state2_idx));
          
          // UPDATE CREDIT OF STATE 2 ACTIONS SECLECTED DURING LAST N EPISODS
          
          for(unsigned int l=0;l< uniq_state2.n_elem;l++){
            if(uniq_state2(l)==-1){
              continue;
            }
            double  curr_action = uniq_state2(l);
            //Rcpp::Rcout << "state2 actions="<<actions.elem(state2_idx)<<std::endl;
            //Rcpp::Rcout <<  "episode="<<episode<<", state="<<2<<", curr_action="<<curr_action<<std::endl;
            arma::uvec a=arma::find(actions.elem(state2_idx)==curr_action);
            arma::vec time_s2 =  time_taken_for_trial.elem(state2_idx);
            double total_time_in_state_2 = arma::accu(time_s2);
            double activity=0;
            if(total_time_in_state_2>0){
              activity = a.n_elem*1000/total_time_in_state_2;
            }else{
              activity = a.n_elem/(1.0*state2_idx.n_elem);
            }
            
            //H(1,curr_action)= H(1,curr_action)+alpha*(score_episode*activity);
            H(1,curr_action)= H(1,curr_action)+(alpha*(score_episode-avg_score)*(1-softmax_cpp3(curr_action,1,H))*activity);
            // if(H(1,curr_action) <0){
            //   Rcpp::Rcout <<  "H(1,curr_action)="<<H(1,curr_action)<<", Visits(1,curr_action)="<<Visits(1,curr_action)<< std::endl;
            // }
            if(R_IsNaN((H(1,curr_action)))){
              Rcpp::Rcout <<  "state="<<1<<", action="<<curr_action<< std::endl;
              //Rcpp::Rcout <<"epsLim=" <<epsLim<< "activity="<< activity<<std::endl;
              stop("H is NAN");
            }else if(H(1,curr_action) == R_PosInf){
              Rcpp::Rcout <<  "state="<<1<<", action="<<curr_action <<std::endl;
              //Rcpp::Rcout <<"epsLim=" <<epsLim<< ", activity="<< activity<<", score_episode="<<score_episode<<std::endl;
              stop("H is Inf");
            }
          }
          
          // UPDATE CREDIT OF STATE 2 NOT  ACTIONS SECLECTED DURING LAST N EPISODS
          
          
          IntegerVector setdiff_state2 = Rcpp::setdiff(all_actions,IntegerVector(uniq_state2.begin(),uniq_state2.end()));
          //Rcpp::Rcout <<  "setdiff_state2="<<setdiff_state2 << std::endl;
          
          for(unsigned int l=0;l< setdiff_state2.size();l++){
            double  curr_action = setdiff_state2(l);
            
            H(1,curr_action)= H(1,curr_action)-(alpha*(score_episode-avg_score)*(softmax_cpp3(curr_action,1,H))/state2_idx.n_elem);
            
            // if(H(1,curr_action) < 0){
            //   Rcpp::Rcout <<  "H(1,curr_action)="<<H(1,curr_action)<<", Visits(1,curr_action)="<<Visits(1,curr_action)<< std::endl;
            //  }
            if(R_IsNaN((H(1,curr_action)))){
              Rcpp::Rcout <<  "state="<<1<<", action="<<curr_action <<std::endl;
              stop("H is NAN");
            }else if(H(1,curr_action) == R_PosInf){
              Rcpp::Rcout <<  "state="<<1<<", action="<<curr_action <<std::endl;
              stop("H is Inf");
            }
          }
          
        }
        score_episode=0;
        episodeFin=0;
        
        actions=arma::vec(1);
        actions.fill(-1);
        states=arma::vec(1);
        states.fill(-1);
        time_taken_for_trial = arma::vec(1);
        time_taken_for_trial.fill(-1);
        
        //Rcpp::Rcout <<  "Restting vectors"<<std::endl;
        resetVector = true;
      }
      
      
      
    }
    
    
    S=S_prime; 
    //trial=trial+1;
    
  }
  return(log_lik*(-1));
  
  
}  


// [[Rcpp::export("acaGetProbMatrix")]]
arma::mat acaGetProbMatrix(Rcpp::NumericMatrix allpaths,float alpha,int epsLim, arma::mat &H,int model,int sim){
  //int sim=1;
  //NumericMatrix likelihood(10,1298);
  //int d;
  //for(d =0;d<9;d++){
  
  //float alpha=alphas[d];
  //arma::mat H = arma::zeros(2,6);
  //Rcpp::NumericMatrix H(2,6);
  //Rcpp::NumericMatrix Visits(2,6);
  
  //Rcpp::Rcout <<  H<< std::endl;
  //Rcpp::Rcout <<  Visits<< std::endl;
  
  //double log_lik=0;
  
  int episode=1;
  int S=0;
  if(sim==1){
    S=allpaths(0,1);
  }else{
    S=allpaths(0,1)-1;
  }
  int A=0;
  arma::vec actions(1);
  actions.fill(-1);
  arma::vec states(1);
  states.fill(-1);
  arma::vec time_taken_for_trial(1);
  time_taken_for_trial.fill(-1);
  
  
  
  int initState=0;
  bool changeState = false;
  bool returnToInitState = false;
  int score_episode=0;
  int episodeFin=0;
  //int prev_ses=-1;
  int nrow = allpaths.nrow();
  //int trial=0;
  int i;
  int avg_score = 0;
  bool resetVector = true;
  
  arma::mat probMatrix_aca=arma::zeros(nrow,12);
  
  for ( i = 0; i < (nrow-1); i++) {
    
    if(resetVector){
      initState=S;
      //Rcpp::Rcout <<"initState="<<initState<<std::endl;
      resetVector= false;
    }
    
    
    //  trial=std::atoi(enreg_pos(trial_pos,5));
    //Rcpp::Rcout <<"i="<<i<<  ", trial="<<trial<<std::endl;
    int R=allpaths(i,2);
    if(R>0){
      score_episode = score_episode+1;
    }         
    
    //Rcpp::Rcout <<  "trial_pos="<<trial_pos << ", enreg_trial="<< std::atoi(enreg_pos(trial_pos,5)) <<std::endl;
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
    
    if(S_prime<0){
      continue;
    }
    //Rcpp::Rcout << "A="<< A << ", S=" << S << ", S_prime="<< S_prime << std::endl;
    int sz = actions.n_elem;
    actions.resize(sz+1);
    actions(sz) = A;
    
    states.resize(sz+1);
    states(sz)=S;
    
    time_taken_for_trial.resize(sz+1);
    float time = allpaths(i,3);
    time_taken_for_trial(sz)= time;
    
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
    //Rcpp::Rcout <<  "changeState="<< changeState <<  ", returnToInitState="<< returnToInitState<<std::endl;
    
    //Check if episode ended
    if(returnToInitState ){
      //Rcpp::Rcout <<  "Inside end episode"<<std::endl;
      changeState = false;
      returnToInitState = false;
      episodeFin=episodeFin+1;
      
      //Rcpp::Rcout <<  "episode="<< episode<<", actions="<<actions<<std::endl;
      IntegerVector all_actions =  seq(0, 5);
      if(episodeFin == epsLim || i==(nrow-1)){
        episode = episode+1;
        
        if(model==1){
          arma::uvec state1_idx = find(states==0);
          arma::vec uniq_state1 = arma::unique(actions.elem(state1_idx));
          
          //Rcpp::Rcout <<  "state1_idx="<< state1_idx<<std::endl;
          for(unsigned int l=0;l< uniq_state1.n_elem;l++){
            if(uniq_state1(l)==-1){
              continue;
            }
            double  curr_action = uniq_state1(l);
            //Rcpp::Rcout << "state1 actions="<<actions.elem(state1_idx)<<std::endl;
            //Rcpp::Rcout <<  "episode="<<episode<<", state="<<1<<", curr_action="<<curr_action<<std::endl;
            arma::uvec a=arma::find(actions.elem(state1_idx)==curr_action);
            arma::vec time_s1 =  time_taken_for_trial.elem(state1_idx);
            double total_time_spent_in_state1 = arma::accu(time_s1);
            double activity=0;
            if(total_time_spent_in_state1>0){
              activity = a.n_elem*1000/total_time_spent_in_state1;
            }else{
              activity = a.n_elem/(1.0*state1_idx.n_elem);
            }
            
            
            
            H(0,curr_action)= H(0,curr_action)+alpha*(score_episode*activity);
            //double delta_H = alpha*(score_episode-avg_score)*(1-softmax_cpp(curr_action,0,H))*activity;
            //H(0,curr_action)= H(0,curr_action)+delta_H;
            //Rcpp::Rcout << "H(0,curr_action)="<<H(0,curr_action) <<", reward=" <<score_episode-avg_score<< ", delta_H="<< delta_H<<std::endl;
            // if(H(0,curr_action) <0){
            //   Rcpp::Rcout <<  "H(0,curr_action)="<<H(0,curr_action)<<", curr_action="<<Visits(0,curr_action)<< std::endl;
            // }
            if(R_IsNaN((H(0,curr_action)))){
              Rcpp::Rcout <<  "state="<<0<<", action="<<curr_action<< std::endl;
              //Rcpp::Rcout <<  "H="<< H(0,curr_action)<<std::endl;
              //Rcpp::Rcout <<  "Visits="<< Visits(0,curr_action)<<std::endl;
              stop("H is NAN");
            }else if(H(0,curr_action) == R_PosInf){
              Rcpp::Rcout <<  "state="<<0<<", action="<<curr_action<<std::endl;
              stop("H is Inf");
            }else if(H(0,curr_action) < 0){
              // Rcpp::Rcout <<  "episode="<<episode<<", state="<<1<<", curr_action="<<curr_action<<", score_episode="<<score_episode<<", activity="<<activity<<std::endl;
              // Rcpp::Rcout <<  H<< std::endl;
              
            }
            
          }
          
          
          
          arma::uvec state2_idx = find(states==1);
          arma::vec uniq_state2 = arma::unique(actions.elem(state2_idx));
          
          for(unsigned int l=0;l< uniq_state2.n_elem;l++){
            if(uniq_state2(l)==-1){
              continue;
            }
            double  curr_action = uniq_state2(l);
            //Rcpp::Rcout << "state2 actions="<<actions.elem(state2_idx)<<std::endl;
            //Rcpp::Rcout <<  "episode="<<episode<<", state="<<2<<", curr_action="<<curr_action<<std::endl;
            arma::uvec a=arma::find(actions.elem(state2_idx)==curr_action);
            arma::vec time_s2 =  time_taken_for_trial.elem(state2_idx);
            double total_time_spent_in_state2 = arma::accu(time_s2);
            double activity=0;
            if(total_time_spent_in_state2>0){
              activity = a.n_elem*1000/total_time_spent_in_state2;
            }else{
              activity = a.n_elem/(1.0*state2_idx.n_elem);
            }
            
            H(1,curr_action)= H(1,curr_action)+alpha*(score_episode*activity);
            //H(1,curr_action)= H(1,curr_action)+(alpha*(score_episode-avg_score)*(1-softmax_cpp(curr_action,1,H))*activity);
            // if(H(1,curr_action) <0){
            //   Rcpp::Rcout <<  "H(1,curr_action)="<<H(1,curr_action)<<", Visits(1,curr_action)="<<Visits(1,curr_action)<< std::endl;
            // }
            if(R_IsNaN((H(1,curr_action)))){
              Rcpp::Rcout <<  "state="<<1<<", action="<<curr_action<< std::endl;
              //Rcpp::Rcout <<"epsLim=" <<epsLim<< "activity="<< activity<<std::endl;
              stop("H is NAN");
            }else if(H(1,curr_action) == R_PosInf){
              Rcpp::Rcout <<  "state="<<1<<", action="<<curr_action<<std::endl;
              //Rcpp::Rcout <<"epsLim=" <<epsLim<< ", activity="<< activity<<", score_episode="<<score_episode<<std::endl;
              stop("H is Inf");
            }else if(H(1,curr_action) < 0){
              // Rcpp::Rcout <<  "episode="<<episode<<", state="<<2<<", curr_action="<<curr_action<<", score_episode="<<score_episode<<", activity="<<activity<<std::endl;
              // Rcpp::Rcout <<  H<< std::endl;
              
            }
          }
        }else if(model==2){
          
          // UPDATE CREDIT OF STATE 1 ACTIONS
          
          arma::uvec state1_idx = find(states==0);
          arma::vec uniq_state1 = arma::unique(actions.elem(state1_idx));
          
          //Rcpp::Rcout <<  "state1_idx="<< state1_idx<<std::endl;
          avg_score = avg_score + (score_episode-avg_score)/episode;
          
          // UPDATE CREDIT OF STATE 1 ACTIONS SECLECTED DURING LAST N EPISODS
          for(unsigned int l=0;l< uniq_state1.n_elem;l++){
            if(uniq_state1(l)==-1){
              continue;
            }
            double  curr_action = uniq_state1(l)*1.00;
            //Rcpp::Rcout << "state1 actions="<<actions.elem(state1_idx)<<std::endl;
            //Rcpp::Rcout <<  "episode="<<episode<<", state="<<1<<", curr_action="<<curr_action<<std::endl;
            arma::uvec a=arma::find(actions.elem(state1_idx)==curr_action);
            //Rcpp::Rcout << "a.n_elem="<<a.n_elem<<std::endl;
            arma::vec time_s1 =  time_taken_for_trial.elem(state1_idx);
            double total_time_spent_in_state1 = arma::accu(time_s1);
            double activity=0;
            if(total_time_spent_in_state1>0){
              activity = a.n_elem*1000/total_time_spent_in_state1;
            }else{
              activity = a.n_elem/(1.0*state1_idx.n_elem);
            }
            
            //Rcpp::Rcout << "H(0,5)="<<H(0,5) <<", reward=" <<score_episode-avg_score<<", activity="<<activity<<std::endl;
            
            //H(0,curr_action)= H(0,curr_action)+alpha*(score_episode*activity/Visits(0,curr_action));
            double delta_H = alpha*(score_episode-avg_score)*(1-softmax_cpp3(curr_action,0,H))*activity;
            H(0,curr_action)= H(0,curr_action)+delta_H;
            //Rcpp::Rcout << "H(0,5)="<<H(0,5) <<", reward=" <<score_episode-avg_score<< ", delta_H="<< delta_H<<", activity="<<activity<<std::endl;
            // if(H(0,curr_action) <0){
            //   Rcpp::Rcout <<  "H(0,curr_action)="<<H(0,curr_action)<<", curr_action="<<Visits(0,curr_action)<< std::endl;
            // }
            if(R_IsNaN((H(0,curr_action)))){
              Rcpp::Rcout <<  "state="<<0<<", action="<<curr_action<< std::endl;
              //Rcpp::Rcout <<  "H="<< H(0,curr_action)<<std::endl;
              //Rcpp::Rcout <<  "Visits="<< Visits(0,curr_action)<<std::endl;
              stop("H is NAN");
            }else if(H(0,curr_action) == R_PosInf){
              Rcpp::Rcout <<  "state="<<0<<", action="<<curr_action<<std::endl;
              stop("H is Inf");
            }
            
          }
          
          // UPDATE CREDIT OF STATE 1 ACTIONS NOT SELECTED DURING LAST N EPISODS
          
          IntegerVector setdiff_state1 = Rcpp::setdiff(all_actions,IntegerVector(uniq_state1.begin(),uniq_state1.end()));
          //Rcpp::Rcout <<  "setdiff_state1="<<setdiff_state1 << std::endl;
          
          
          for(unsigned int l=0;l< setdiff_state1.size();l++){
            double  curr_action = setdiff_state1(l);
            H(0,curr_action)= H(0,curr_action)-(alpha*(score_episode-avg_score)*(softmax_cpp3(curr_action,0,H))/state1_idx.n_elem);
            
            //Rcpp::Rcout << "curr_action="<<curr_action<<" ,H(0,5)="<<H(0,5) <<", reward=" <<score_episode-avg_score<< ", softmax_cpp3(curr_action,0,H)="<< softmax_cpp3(curr_action,0,H)<<", state1_idx.n_elem="<<state1_idx.n_elem<<std::endl;
            // if(H(0,curr_action) < 0){
            //   Rcpp::Rcout <<  "H(0,curr_action)="<<H(0,curr_action)<<", Visits(0,curr_action)="<<Visits(0,curr_action)<< std::endl;
            // }
            if(R_IsNaN((H(0,curr_action)))){
              Rcpp::Rcout <<  "state="<<0<<", action="<<curr_action<< std::endl;
              stop("H is NAN");
            }else if(H(0,curr_action) == R_PosInf){
              Rcpp::Rcout <<  "state="<<0<<", action="<<curr_action <<std::endl;
              stop("H is Inf");
            }
          }
          
          // UPDATE CREDIT OF STATE 2 ACTIONS
          
          arma::uvec state2_idx = find(states==1);
          arma::vec uniq_state2 = arma::unique(actions.elem(state2_idx));
          
          // UPDATE CREDIT OF STATE 2 ACTIONS SECLECTED DURING LAST N EPISODS
          
          for(unsigned int l=0;l< uniq_state2.n_elem;l++){
            if(uniq_state2(l)==-1){
              continue;
            }
            double  curr_action = uniq_state2(l);
            //Rcpp::Rcout << "state2 actions="<<actions.elem(state2_idx)<<std::endl;
            //Rcpp::Rcout <<  "episode="<<episode<<", state="<<2<<", curr_action="<<curr_action<<std::endl;
            arma::uvec a=arma::find(actions.elem(state2_idx)==curr_action);
            arma::vec time_s2 =  time_taken_for_trial.elem(state2_idx);
            double total_time_in_state_2 = arma::accu(time_s2);
            double activity=0;
            if(total_time_in_state_2>0){
              activity = a.n_elem*1000/total_time_in_state_2;
            }else{
              activity = a.n_elem/(1.0*state2_idx.n_elem);
            }
            
            //H(1,curr_action)= H(1,curr_action)+alpha*(score_episode*activity);
            H(1,curr_action)= H(1,curr_action)+(alpha*(score_episode-avg_score)*(1-softmax_cpp3(curr_action,1,H))*activity);
            // if(H(1,curr_action) <0){
            //   Rcpp::Rcout <<  "H(1,curr_action)="<<H(1,curr_action)<<", Visits(1,curr_action)="<<Visits(1,curr_action)<< std::endl;
            // }
            if(R_IsNaN((H(1,curr_action)))){
              Rcpp::Rcout <<  "state="<<1<<", action="<<curr_action<< std::endl;
              //Rcpp::Rcout <<"epsLim=" <<epsLim<< "activity="<< activity<<std::endl;
              stop("H is NAN");
            }else if(H(1,curr_action) == R_PosInf){
              Rcpp::Rcout <<  "state="<<1<<", action="<<curr_action <<std::endl;
              //Rcpp::Rcout <<"epsLim=" <<epsLim<< ", activity="<< activity<<", score_episode="<<score_episode<<std::endl;
              stop("H is Inf");
            }
          }
          
          // UPDATE CREDIT OF STATE 2 NOT  ACTIONS SECLECTED DURING LAST N EPISODS
          
          
          IntegerVector setdiff_state2 = Rcpp::setdiff(all_actions,IntegerVector(uniq_state2.begin(),uniq_state2.end()));
          //Rcpp::Rcout <<  "setdiff_state2="<<setdiff_state2 << std::endl;
          
          for(unsigned int l=0;l< setdiff_state2.size();l++){
            double  curr_action = setdiff_state2(l);
            
            H(1,curr_action)= H(1,curr_action)-(alpha*(score_episode-avg_score)*(softmax_cpp3(curr_action,1,H))/state2_idx.n_elem);
            
            // if(H(1,curr_action) < 0){
            //   Rcpp::Rcout <<  "H(1,curr_action)="<<H(1,curr_action)<<", Visits(1,curr_action)="<<Visits(1,curr_action)<< std::endl;
            //  }
            if(R_IsNaN((H(1,curr_action)))){
              Rcpp::Rcout <<  "state="<<1<<", action="<<curr_action <<std::endl;
              stop("H is NAN");
            }else if(H(1,curr_action) == R_PosInf){
              Rcpp::Rcout <<  "state="<<1<<", action="<<curr_action <<std::endl;
              stop("H is Inf");
            }
          }
          
        }
        score_episode=0;
        episodeFin=0;
        
        actions=arma::vec(1);
        actions.fill(-1);
        states=arma::vec(1);
        states.fill(-1);
        time_taken_for_trial = arma::vec(1);
        time_taken_for_trial.fill(-1);
        
        //Rcpp::Rcout <<  "Restting vectors"<<std::endl;
        resetVector = true;
      }
      
      
      
    }
    
    
    S=S_prime; 
    //trial=trial+1;
    
  }
  return(probMatrix_aca);
}


// [[Rcpp::export("computeMSE")]]
arma::mat computeMSE(arma::mat allpaths,arma::mat probMatrix_aca,int sim){

  //int start_index = allpaths.n_rows/2;
  //int max_index=allpaths.n_rows-start_index;
  arma::mat mseMatrix=arma::zeros(12,allpaths.n_rows);
  //start_index= start_index+1;
  int trial=0;
  //Rcpp::Rcout<< "pow(2,3)=" <<pow(2,3)<< ", pow(3,2)=" << pow(3,2) <<std::endl;
  //Rcpp::Rcout <<"start_index="<<start_index<<", allpaths.n_rows=" <<allpaths.n_rows<< ", action=" <<allpaths(start_index,0)<< " ,state=" << allpaths(start_index,1)<<std::endl;
  //arma::rowvec v = probMatrix_aca.row(trial);
  //Rcpp::Rcout <<  v<< std::endl;
  
  for(unsigned int i=0;i<(allpaths.n_rows);i++){
    int action=allpaths(i,0);
    //int sub=(1==action);
     //Rcpp::Rcout<<"i="<<i<<", action=" <<action<< " ,state=" << allpaths(i,1)<<std::endl;
    //Rcpp::Rcout<<"i="<< i<< ", action=" <<action <<std::endl;
    if(sim==1){
      if(allpaths(i,1)==0){
        
        mseMatrix(0,trial)=pow(probMatrix_aca(i,0)-(0==action),2);
        mseMatrix(1,trial)=pow(probMatrix_aca(i,1)-(1==action),2);
        mseMatrix(2,trial)=pow(probMatrix_aca(i,2)-(2==action),2);
        mseMatrix(3,trial)=pow(probMatrix_aca(i,3)-(3==action),2);
        mseMatrix(4,trial)=pow(probMatrix_aca(i,4)-(4==action),2);
        mseMatrix(5,trial)=pow(probMatrix_aca(i,5)-(5==action),2);
        
      }else if(allpaths(i,1)==1){
        mseMatrix(6,trial)=pow(probMatrix_aca(i,6)-(0==action),2);
        mseMatrix(7,trial)=pow(probMatrix_aca(i,7)-(1==action),2);
        mseMatrix(8,trial)=pow(probMatrix_aca(i,8)-(2==action),2);
        mseMatrix(9,trial)=pow(probMatrix_aca(i,9)-(3==action),2);
        mseMatrix(10,trial)=pow(probMatrix_aca(i,10)-(4==action),2);
        mseMatrix(11,trial)=pow(probMatrix_aca(i,11)-(5==action),2);
        //Rcpp::Rcout << "S=2, mseMatrix(3,trial)="<<  mseMatrix(3,trial)<< std::endl;
      }
    }else{
      if(allpaths(i,1)==1){
        
        mseMatrix(0,trial)=pow(probMatrix_aca(i,0)-(1==action),2);
        mseMatrix(1,trial)=pow(probMatrix_aca(i,1)-(2==action),2);
        mseMatrix(2,trial)=pow(probMatrix_aca(i,2)-(3==action),2);
        mseMatrix(3,trial)=pow(probMatrix_aca(i,3)-(4==action),2);
        mseMatrix(4,trial)=pow(probMatrix_aca(i,4)-(5==action),2);
        mseMatrix(5,trial)=pow(probMatrix_aca(i,5)-(6==action),2);
        
      }else if(allpaths(i,1)==2){
        mseMatrix(6,trial)=pow(probMatrix_aca(i,6)-(1==action),2);
        mseMatrix(7,trial)=pow(probMatrix_aca(i,7)-(2==action),2);
        mseMatrix(8,trial)=pow(probMatrix_aca(i,8)-(3==action),2);
        mseMatrix(9,trial)=pow(probMatrix_aca(i,9)-(4==action),2);
        mseMatrix(10,trial)=pow(probMatrix_aca(i,10)-(5==action),2);
        mseMatrix(11,trial)=pow(probMatrix_aca(i,11)-(6==action),2);
        //Rcpp::Rcout << "S=2, mseMatrix(3,trial)="<<  mseMatrix(3,trial)<< std::endl;
      }
    }
    
     // arma::colvec v = mseMatrix.col(trial);
     // Rcpp::Rcout <<  v.as_row()<< std::endl;

    // if(action==4 && allpaths(i,1)==2){
    //   Rcpp::Rcout <<"i="<<i<<", mseMatrix(9,trial)=" <<mseMatrix(9,trial)<< ", probMatrix_aca(i,9)=" <<probMatrix_aca(i,9)<< ", (4==action)="<<(4==action)<< std::endl;
    // }
    trial=trial+1;
  }
  //Rcpp::Rcout<< accu(mseMatrix)<<std::endl;
  //double total_mse=accu(mseMatrix)/(1.0*max_index);
  return(mseMatrix);
}

// [[Rcpp::export("updateAllpaths1")]]
arma::vec updateAllpaths1(NumericVector allpaths,Rcpp::NumericMatrix enreg_pos){
  
  int nrow = allpaths.size();
  //Rcpp::Rcout <<"nrow="<<nrow<<std::endl;
  arma::colvec y(nrow);
  //arma::mat enreg_arma(enreg_pos.begin(), enreg_pos.nrow(), enreg_pos.ncol(), false);
  arma::mat enreg_arma = Rcpp::as<arma::mat>(enreg_pos);
  arma::vec allpaths_arma = Rcpp::as<arma::vec>(allpaths);
  //Rcpp::Rcout <<enreg_arma<<std::endl;
  int prev_ses=-1;
  int trial = 0;
  for(int i=0;i<nrow;i++){
    
    //Rcpp::Rcout <<"trial="<<trial<<", allpaths_arma(i)="<<allpaths_arma(i)<<std::endl;
    if(prev_ses!=allpaths_arma(i)){
      trial=1;
      prev_ses=allpaths_arma(i);
    }
    //Rcpp::Rcout <<"i="<<i<<std::endl;
    arma::uvec ids = arma::find((enreg_arma.col(1)==trial) && (enreg_arma.col(2)==allpaths_arma(i)));
    int start_pos_id = ids(0);
    //Rcpp::Rcout <<"ids.size="<<ids.n_elem<<std::endl;
    
    int max_pos_id = ids(ids.n_elem-1);
    //Rcpp::Rcout <<"start_pos_id="<<start_pos_id<<", max_pos_id="<<max_pos_id<<std::endl;
    double time_taken_for_trial=(enreg_arma((max_pos_id-1),0))-enreg_arma(start_pos_id,0);
    if(time_taken_for_trial==0){
      time_taken_for_trial=20;
    }
    y(i)=time_taken_for_trial;
    //Rcpp::Rcout <<"trial="<<trial<<", ses="<<allpaths_arma(i)<<", time_taken_for_trial=" <<time_taken_for_trial<<std::endl;
    trial= trial+1;
  }
  //=arma::join_horiz(allpaths_arma,y);
  return(y);
}