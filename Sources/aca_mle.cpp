// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::depends(BH)]]
#include <vector>
#include <RcppArmadillo.h>
#include <regex>




using namespace Rcpp;


int getNextState_cpp(Rcpp::StringMatrix allpaths,int i){
  int nrow = allpaths.nrow();
  //Rcpp::Rcout <<  "nrow="<<nrow<< std::endl;
  int next_state=-2;
  
  std::regex a("^.*e$"); 
  std::regex b("^.*i$");
  std::regex f("^f.*"); 
  std::regex d("^d.*"); 
  std::regex j("^j.*");
  std::regex h("^h.*");
  std::cmatch base_match;
  
  std::string s = Rcpp::as<std::string>(allpaths(i,0));
  //Rcpp::Rcout <<  "i="<<i<<", allpaths(i)="<< s << std::endl;
  if(std::regex_match(s,a)){
    next_state=1;
    //Rcpp::Rcout <<  "Here22"<< std::endl;
  }else if(std::regex_match(s,b)){
    next_state=2;
  }else{
    std::string s1 = Rcpp::as<std::string>(allpaths(i+1,0));
    
    if(i < (nrow-1)){
      //Rcpp::Rcout <<   "i+1="<<(i+1)<<", allpaths(i+1)="<< s1 << std::endl;
      if(std::regex_match(s1,f)||std::regex_match(s1,d)){
        next_state = 1;
      }else if(std::regex_match(s1,j)||std::regex_match(s1,h)){
        next_state = 2;
      }
    }
  }
  //Rcpp::Rcout <<  "next_state="<<next_state <<std::endl;
  return(next_state);
}

double softmax_cpp(int A,int S,arma::mat &H){
  //Rcpp::Rcout <<  "S="<< S<<std::endl;
  arma::rowvec v = H.row(S);
  //Rcpp::Rcout <<  v<< std::endl;
  float m=arma::max(v);
  double exp_sum  = std::exp(H(S,0)-m)+std::exp(H(S,1)-m)+std::exp(H(S,2)-m)+std::exp(H(S,3)-m)+std::exp(H(S,4)-m)+std::exp(H(S,5)-m) ;
  double pr_A = (std::exp((H(S,A)-m)))/exp_sum;
  if(pr_A<=0){
    Rcpp::Rcout <<"A="<<A<< ", S=" <<S << std::endl;
    Rcpp::Rcout << H << std::endl;
    Rcpp::Rcout << "m=" <<m << std::endl;
    Rcpp::Rcout << "Numerator=" << std::exp(H(S,A)-m)  << std::endl;
    Rcpp::Rcout << "exp_sum=" << exp_sum  << std::endl;
    Rcpp::Rcout << "(std::exp(H(S,0))-m)=" << (std::exp(H(S,0))-m)  << std::endl;
    Rcpp::Rcout << "(std::exp(H(S,1))-m)=" << (std::exp(H(S,1))-m)  << std::endl;
    Rcpp::Rcout << "(std::exp(H(S,2))-m)=" << (std::exp(H(S,2))-m)  << std::endl;
    Rcpp::Rcout << "(std::exp(H(S,3))-m)=" << (std::exp(H(S,3))-m)  << std::endl;
    Rcpp::Rcout << "(std::exp(H(S,4))-m)=" << (std::exp(H(S,4))-m)  << std::endl;
    Rcpp::Rcout << "(std::exp(H(S,5))-m)=" << (std::exp(H(S,5))-m)  << std::endl;
    stop("logProb is NAN");
    
  }else if(pr_A>1){
    Rcpp::Rcout << "S=" <<S << std::endl;
    Rcpp::Rcout << H << std::endl;
    Rcpp::Rcout << "m=" <<m << std::endl;
    Rcpp::Rcout << "Numerator=" << std::exp(H(S,A)-m)  << std::endl;
    Rcpp::Rcout << "exp_sum=" << exp_sum  << std::endl;
    Rcpp::Rcout << "(std::exp(H(S,0))-m)=" << (std::exp(H(S,0))-m)  << std::endl;
    Rcpp::Rcout << "(std::exp(H(S,1))-m)=" << (std::exp(H(S,1))-m)  << std::endl;
    Rcpp::Rcout << "(std::exp(H(S,2))-m)=" << (std::exp(H(S,2))-m)  << std::endl;
    Rcpp::Rcout << "(std::exp(H(S,3))-m)=" << (std::exp(H(S,3))-m)  << std::endl;
    Rcpp::Rcout << "(std::exp(H(S,4))-m)=" << (std::exp(H(S,4))-m)  << std::endl;
    Rcpp::Rcout << "(std::exp(H(S,5))-m)=" << (std::exp(H(S,5))-m)  << std::endl;
  }
  if(R_IsNaN((pr_A))){
    
    Rcpp::Rcout << "exp_sum=" << exp_sum  << std::endl;
    Rcpp::Rcout << "numerator=" << (std::exp(H(S,A)-m))  << std::endl;
    Rcpp::Rcout <<  "pr_A="<< pr_A << std::endl;
 
    Rcpp::Rcout <<  H<< std::endl;
    //stop("logProb is NAN");
  }
  
  // if(Rcpp::traits::is_infinite<REALSXP>(prob_a)){
  //   Rcpp::Rcout <<  "Prb of action seclection = 1" << std::endl;;
  // }    
  //Rcpp::Rcout <<  "pr_A="<< pr_A<<std::endl;
  return(pr_A);
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


// [[Rcpp::export("aca_mle_cpp")]]
double aca_mle_cpp(Rcpp::StringMatrix allpaths,float alpha,int epsLim, arma::mat &H){
  //NumericMatrix likelihood(10,1298);
  //int d;
  //for(d =0;d<9;d++){
  
  //float alpha=alphas[d];
  //arma::mat H = arma::zeros(2,6);
  arma::mat Visits = arma::zeros(2,6);
  //Rcpp::NumericMatrix H(2,6);
  //Rcpp::NumericMatrix Visits(2,6);
  
  //Rcpp::Rcout <<  "alpha="<<alpha<<", epsLim="<<epsLim<< std::endl;
  //Rcpp::Rcout <<  H<< std::endl;
  //Rcpp::Rcout <<  Visits<< std::endl;
  
  double log_lik=0;
  
  int episode=1;
  int S=0;
  int A=0;
  arma::vec actions(1);
  actions.fill(-1);
  arma::vec states(1);
  states.fill(-1);
  arma::vec time_taken_for_trial(1);
  time_taken_for_trial.fill(-1);
  std::regex a("^.*e$"); 
  std::regex b("^.*i$");
  std::cmatch base_match;
  
  std::string s = Rcpp::as<std::string>(allpaths(0,0));
  if(std::regex_match(s,a)){
    S=0;
  }else if(std::regex_match(s,b)){
    S=1;
  }else{
    Rcpp::Rcout <<  "Unknown intial state. Check"<< std::endl;
  }
  
  int initState=allpaths(0,1);
  bool changeState = false;
  bool returnToInitState = false;
  int score_episode=0;
  int episodeFin=0;
  //int prev_ses=-1;
  //int trial_pos=0;
  int nrow = allpaths.nrow();
  //int enreg_rows = enreg_pos.nrow();
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
    

    int R=std::atoi(allpaths(i,3));
    
    if(R>0){
      score_episode=score_episode+1;
    }
    
    A=std::atoi(allpaths(i,2))-1;
    
    //int S_prime=getNextState_cpp(allpaths,i)-1;
    int S_prime=std::atoi(allpaths((i+1),4))-1;
    if(S_prime<0){
      continue;
    }
    //Rcpp::Rcout << "trial="<<trial <<  ", A="<< A << ", S=" << S << ", S_prime="<< S_prime << std::endl;
    int sz = actions.n_elem;
    actions.resize(sz+1);
    actions(sz) = A;
    
    states.resize(sz+1);
    states(sz)=S;
    
    //Rcpp::Rcout << "i="<<i<<", trial="<<trial <<  ", ses="<< ses << ", time_taken_for_trial=" << (allpaths(i,4)) << std::endl;
    time_taken_for_trial.resize(sz+1);
    float time = std::atof(allpaths(i,5));
    time_taken_for_trial(sz)= time;
    
    
    Visits(S,A) = Visits(S,A)+1;
    
    if(S_prime!=initState){
      changeState = true;
    }else if(S_prime==initState && changeState){
      returnToInitState = true;
    }
    //Rcpp::Rcout <<  "changeState="<< changeState <<  ", returnToInitState="<< returnToInitState<<std::endl;
    
    double prob_a = softmax_cpp(A,S,H);
    
    //Rcpp::Rcout << "i=" << i  << std::endl;
    //Rcpp::Rcout << "prob_a=" << prob_a  << std::endl;
    
    double logProb = log(prob_a);
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
    }
    
    log_lik=log_lik+ logProb;
    
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
        //Rcpp::Rcout <<  "Inside update H"<<std::endl;
        //int total_actions= actions.n_elem;
        //Rcpp::Rcout <<  "total_actions="<< total_actions<<std::endl;
        //Rcpp::Rcout <<  "actions="<< actions<<std::endl;
        //Rcpp::Rcout <<  "time_taken_for_trial="<< time_taken_for_trial<<std::endl;
        
        arma::uvec state1_idx = find(states==0);
        arma::vec uniq_state1 = arma::unique(actions.elem(state1_idx));
        
        //Rcpp::Rcout <<  "state1_idx="<< state1_idx<<std::endl;
        avg_score = avg_score + (score_episode-avg_score)/episode;
        
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
          double activity = a.n_elem*10000/total_time_spent_in_state1;
          
          
          //H(0,curr_action)= H(0,curr_action)+alpha*(score_episode*activity/Visits(0,curr_action));
          double delta_H = alpha*(score_episode-avg_score)*(1-softmax_cpp(curr_action,0,H))*activity;
          H(0,curr_action)= H(0,curr_action)+delta_H;
          //Rcpp::Rcout << "H(0,curr_action)="<<H(0,curr_action) <<", reward=" <<score_episode-avg_score<< ", delta_H="<< delta_H<<", activity="<<activity<<std::endl;
          // if(H(0,curr_action) <0){
          //   Rcpp::Rcout <<  "H(0,curr_action)="<<H(0,curr_action)<<", curr_action="<<Visits(0,curr_action)<< std::endl;
          // }
          if(R_IsNaN((H(0,curr_action)))){
            Rcpp::Rcout <<  "state="<<0<<", action="<<curr_action<< std::endl;
            //Rcpp::Rcout <<  "H="<< H(0,curr_action)<<std::endl;
            //Rcpp::Rcout <<  "Visits="<< Visits(0,curr_action)<<std::endl;
            stop("H is NAN");
          }else if(H(0,curr_action) == R_PosInf){
            Rcpp::Rcout <<  "state="<<0<<", action="<<curr_action<<", i="<< i <<", epsLim="<<epsLim<<std::endl;
            Rcpp::Rcout << "time_taken_for_trial" <<time_taken_for_trial<<std::endl;
            Rcpp::Rcout << "state1_idx" <<state1_idx<<std::endl;
            //Rcpp::Rcout <<"total_time_spent_in_state1="<<total_time_spent_in_state1<< ", H(0,curr_action)="<<H(0,curr_action) <<", reward=" <<score_episode-avg_score<< ", delta_H="<< delta_H<<std::endl;
            stop("H is Inf");
          }
          
        }
        
        IntegerVector setdiff_state1 = Rcpp::setdiff(all_actions,IntegerVector(uniq_state1.begin(),uniq_state1.end()));
        //Rcpp::Rcout <<  "setdiff_state1="<<setdiff_state1 << std::endl;
        
        for(unsigned int l=0;l< setdiff_state1.size();l++){
          double  curr_action = setdiff_state1(l);
          H(0,curr_action)= H(0,curr_action)-(alpha*(score_episode-avg_score)*(softmax_cpp(curr_action,0,H))/state1_idx.n_elem);
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
          H(1,curr_action)= H(1,curr_action)+(alpha*(score_episode-avg_score)*(1-softmax_cpp(curr_action,1,H))*activity);
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
          
          H(1,curr_action)= H(1,curr_action)-(alpha*(score_episode-avg_score)*(softmax_cpp(curr_action,1,H))/state2_idx.n_elem);
                                                      
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
        
        
        score_episode=0;
        episodeFin=0;
        
        actions=arma::vec();
        actions.fill(-1);
        states=arma::vec();
        states.fill(-1);
        time_taken_for_trial = arma::vec();
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




