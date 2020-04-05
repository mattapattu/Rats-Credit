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
  arma::vec y = arma::zeros<arma::vec>(6);
  arma::vec x = arma::zeros<arma::vec>(6);
  //Rcpp::Rcout <<  v<< std::endl;
  float m=arma::max(v);
  //Rcpp::Rcout << "m=" << m<< std::endl;
  double exp_sum  = std::exp(H(S,0)-m)+std::exp(H(S,1)-m)+std::exp(H(S,2)-m)+std::exp(H(S,3)-m)+std::exp(H(S,4)-m)+std::exp(H(S,5)-m) ;
  
  double pr_A = (std::exp((H(S,A)-m)))/exp_sum;
  if(pr_A<0){
    Rcpp::Rcout <<"pr_A="<<pr_A<< " is < 0" << std::endl;
    Rcpp::Rcout <<"A="<<A<< ", S=" <<S << std::endl;
    Rcpp::Rcout << "Numerator=" << std::exp(H(S,A)-m)  << std::endl;
    Rcpp::Rcout << "(std::exp(H(S,0))-m)=" << std::exp(H(S,0)-m)  << std::endl;
    Rcpp::Rcout << "(std::exp(H(S,1))-m)=" << std::exp(H(S,1)-m)  << std::endl;
    Rcpp::Rcout << "(std::exp(H(S,2))-m)=" << std::exp(H(S,2)-m)  << std::endl;
    Rcpp::Rcout << "(std::exp(H(S,3))-m)=" << std::exp(H(S,3)-m)  << std::endl;
    Rcpp::Rcout << "(std::exp(H(S,4))-m)=" << std::exp(H(S,4)-m)  << std::endl;
    Rcpp::Rcout << "(std::exp(H(S,5))-m)=" << std::exp(H(S,5)-m)  << std::endl;
    
  }else if(pr_A>1){
    Rcpp::Rcout <<"pr_A="<<pr_A<< " is > 1" << std::endl;
    Rcpp::Rcout <<"A="<<A<< ", S=" <<S << std::endl;
    Rcpp::Rcout << H << std::endl;
    Rcpp::Rcout << "m=" <<m << std::endl;
    Rcpp::Rcout << "Numerator=" << std::exp(H(S,A)-m)  << std::endl;
    Rcpp::Rcout << "exp_sum=" << exp_sum  << std::endl;
    Rcpp::Rcout << "x=" << x  << std::endl;
    Rcpp::Rcout << "y=" << y  << std::endl;
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
  Rcpp::Rcout << "v=" << v<< std::endl;
  //Rcpp::Rcout << "actions=" << actions<< std::endl;
  int action_selected = Rcpp::RcppArmadillo::sample(actions, 1, false, v.as_col())[0] ;
  Rcpp::Rcout << "action_selected=" << action_selected<< std::endl;
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
  arma::mat allpaths_aca_model2 = arma::zeros(total_trials,3);
  
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
          
          //Rcpp::Rcout <<  "state1_idx="<< state1_idx<<std::endl;
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
            }else if(H(0,curr_action) == R_PosInf){
              Rcpp::Rcout <<  "state="<<0<<", action="<<curr_action<<std::endl;
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
            }else if(H(0,curr_action) == R_PosInf){
              Rcpp::Rcout <<  "state="<<0<<", action="<<curr_action <<std::endl;
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
            double activity = a.n_elem/(1.0*state1_idx.n_elem);
            
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
              Rcpp::Rcout <<  "state="<<1<<", action="<<curr_action<<std::endl;
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
    
  }
  return(allpaths_aca_model2);
  
  
}

// [[Rcpp::export("aca_mle_lik")]]
double aca_mle_lik(Rcpp::NumericMatrix allpaths,float alpha,int epsLim, arma::mat &H,int model){
  //NumericMatrix likelihood(10,1298);
  //int d;
  //for(d =0;d<9;d++){
  
  //float alpha=alphas[d];
  //arma::mat H = arma::zeros(2,6);
  arma::mat Visits = arma::zeros(2,6);
  //Rcpp::NumericMatrix H(2,6);
  //Rcpp::NumericMatrix Visits(2,6);
  
  //Rcpp::Rcout <<  H<< std::endl;
  //Rcpp::Rcout <<  Visits<< std::endl;
  
  double log_lik=0;
  
  int episode=1;
  int S=((allpaths(0,1))-1);
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
    
    A=((allpaths(i,0))-1);
    
    int S_prime=((allpaths((i+1),1))-1);
    if(S_prime<0){
      continue;
    }
    //Rcpp::Rcout << "trial="<<trial <<  ", A="<< A << ", S=" << S << ", S_prime="<< S_prime << std::endl;
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
    if(R_IsNaN((logProb))){
      
      Rcpp::Rcout <<"epsLim=" <<epsLim<<", alpha=" << alpha  << std::endl;
      Rcpp::Rcout << "i=" << i  << std::endl;
      Rcpp::Rcout <<  "A="<< A << ", S=" << S << ", S_prime="<< S_prime << std::endl;
      Rcpp::Rcout << "prob_a=" << prob_a << std::endl;
      Rcpp::Rcout << "logProb=" << logProb << std::endl;
      
      Rcpp::Rcout <<  H<< std::endl;
      //Rcpp::Rcout <<  Visits<< std::endl;
      //stop("logProb is NAN");
      return(100000);
    }else if( prob_a >1){
      return(100000);
    }
    
    log_lik=log_lik+ logProb;
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
            double activity = a.n_elem/(1.0*state1_idx.n_elem);
            
            
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
              Rcpp::Rcout <<  "state="<<0<<", action="<<curr_action<<", Visits(0,curr_action)="<< Visits(0,curr_action) <<std::endl;
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
            double activity = a.n_elem/(1.0*state2_idx.n_elem);
            
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
              Rcpp::Rcout <<  "state="<<1<<", action="<<curr_action<<", Visits(1,curr_action)="<< Visits(1,curr_action) <<std::endl;
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
          
          //Rcpp::Rcout <<  "state1_idx="<< state1_idx<<std::endl;
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
            arma::vec time_s1 =  time_taken_for_trial.elem(state1_idx);
            double total_time_spent_in_state1 = arma::accu(time_s1);
            double activity = a.n_elem*10000/total_time_spent_in_state1;
            
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
              Rcpp::Rcout <<  "state="<<0<<", action="<<curr_action<<", Visits(0,curr_action)="<< Visits(0,curr_action) <<std::endl;
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
            }else if(H(0,curr_action) == R_PosInf){
              Rcpp::Rcout <<  "state="<<0<<", action="<<curr_action<<", Visits(0,curr_action)="<< Visits(0,curr_action) <<std::endl;
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
            arma::vec time_s2 =  time_taken_for_trial.elem(state2_idx);
            double total_time_in_state_2 = arma::accu(time_s2);
            double activity = a.n_elem*10000/total_time_in_state_2;
            
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
              Rcpp::Rcout <<  "state="<<1<<", action="<<curr_action<<", Visits(1,curr_action)="<< Visits(1,curr_action) <<std::endl;
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
              Rcpp::Rcout <<  "state="<<1<<", action="<<curr_action<<", Visits(1,curr_action)="<< Visits(1,curr_action) <<std::endl;
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

