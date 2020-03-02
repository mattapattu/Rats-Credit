// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::depends(BH)]]
#include <vector>
#include <RcppArmadillo.h>
#include <boost/regex.hpp>
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
  double exp_sum  = 0;
  for(int j=0;j<5;j++){
    exp_sum = exp_sum + (std::exp(H(S,j))-m); 
  }
  double pr_A = (std::exp(H(S,A)-m))/exp_sum;
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


// [[Rcpp::export("aca_mle_cpp")]]
double aca_mle_cpp(Rcpp::StringMatrix allpaths,Rcpp::StringMatrix enreg_pos,float alpha,int epsLim){
  //NumericMatrix likelihood(10,1298);
  //int d;
  //for(d =0;d<9;d++){
    
      //float alpha=alphas[d];
      arma::mat H = arma::zeros(2,6);
      arma::mat Visits = arma::zeros(2,6);
      //Rcpp::NumericMatrix H(2,6);
      //Rcpp::NumericMatrix Visits(2,6);
      
      //Rcpp::Rcout <<  H<< std::endl;
      //Rcpp::Rcout <<  Visits<< std::endl;
      
      double log_lik=0;
      
      int episode=1;
      int S=0;
      int A=0;
      Rcpp::NumericVector actions=NumericVector::create(-1);
      Rcpp::NumericVector states=NumericVector::create(-1);
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

      int initState=0;
      bool changeState = false;
      bool returnToInitState = false;
      int score_episode=0;
      int episodeFin=0;
      int prev_ses=-1;
      int trial_pos=0;
      int nrow = allpaths.nrow();
      int trial=0;
      int i;
      for ( i = 0; i < nrow; i++) {
        
        if(actions.length()==1){
          initState=S;
        }
        int ses=std::atoi(allpaths(i,1))-1;
        if(prev_ses!=ses){
          //Rcpp::List enreg_cpp=Rcpp::as<Rcpp::List>(enreg);
          // Rcpp::List enreg_cpp_ses = Rcpp::as<Rcpp::List>(enreg_cpp(ses));
          // pos_ses_mat = Rcpp::as<Rcpp::CharacterMatrix>(enreg_cpp_ses(0));
          prev_ses = ses;
          trial_pos=0;
        }
        //Rcpp::Rcout <<  "A="<< A << ", S=" << S << ", S_prime="<< S_prime << std::endl;
        //Rcpp::Rcout <<  "trial="<<trial<<std::endl;
        trial=std::atoi(enreg_pos(i,5));
        int R=0;
        
        while(trial_pos<nrow && std::atoi(enreg_pos(trial_pos,5))==trial){
          R=R+std::atoi(enreg_pos(trial_pos,3));
          trial_pos=trial_pos+1;
        }

        if(R>0){
          score_episode=score_episode+1;
        }
        A=(std::atoi(allpaths(i,2))-1);
        
        int S_prime=getNextState_cpp(allpaths,i)-1;
        if(S_prime<0){
          continue;
        }
        //Rcpp::Rcout <<  "S_prime="<<S_prime<<", A="<<A<< std::endl;
        actions.push_back(A);
        states.push_back(S);
        
        Visits(S,A) = Visits(S,A)+1;
        
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
          episodeFin=episodeFin+1;
          
          if(episodeFin==epsLim||i<=(nrow-epsLim)){
            
            int total_actions= actions.size();
            //Rcpp::Rcout <<  "total_actions="<< total_actions<<std::endl;
            
            //ignore first elm of every episode(=-1)
            for(int x=1;x<total_actions;x++){
              NumericVector a=actions[actions==actions(x)];
              //Rcpp::Rcout <<  "a="<< a<std::endl;
              double activity=a.size()/(double) total_actions;
                
                //Rcpp::Rcout <<  "H="<< H(states(x),actions(x))<<std::endl;
                //Rcpp::Rcout <<  "Visits="<< Visits<<std::endl;  
              //H(states(x),actions(x))=H(states(x),actions(x))+alpha*(score_episode*activity/Visits(states(x),actions(x)));
              H(states(x),actions(x))=H(states(x),actions(x))+alpha*(score_episode*activity/Visits(states(x),actions(x)));
              if(R_IsNaN((H(states(x),actions(x))))){
                  Rcpp::Rcout <<  "state="<<states(x)<<", action="<<actions(x)<< std::endl;
                  Rcpp::Rcout <<  "activity="<< activity<<std::endl;
                  Rcpp::Rcout <<  "H="<< H(states(x),actions(x))<<std::endl;
                  Rcpp::Rcout <<  "Visits="<< Visits(states(x),actions(x))<<std::endl;
                  stop("H is NAN");
                }
                
            }
            //Rcpp::Rcout <<  H<< std::endl;
            //Rcpp::Rcout <<  Visits<< std::endl;
            
            score_episode=0;
            episodeFin=0;
            episode = episode+1;
            
            actions= NumericVector::create(-1);
            states= NumericVector::create(-1);
            
          }
          
        }
        
        double prob_a = softmax_cpp(A,S,H);
        
        //Rcpp::Rcout << "i=" << i  << std::endl;
        //Rcpp::Rcout << "prob_a=" << prob_a  << std::endl;

        double logProb = log(prob_a);
        if(R_IsNaN((logProb))){
          
          Rcpp::Rcout << "alpha=" << alpha  << std::endl;
          Rcpp::Rcout << "i=" << i  << std::endl;
          Rcpp::Rcout <<  "A="<< A << ", S=" << S << ", S_prime="<< S_prime << std::endl;
          Rcpp::Rcout << "prob_a=" << prob_a << std::endl;
          Rcpp::Rcout << "logProb=" << logProb << std::endl;

          Rcpp::Rcout <<  H<< std::endl;
          Rcpp::Rcout <<  Visits<< std::endl;
          //stop("logProb is NAN");
          return(100000);
        }
        log_lik=log_lik+ logProb;
        S=S_prime;  
        
      }
      return(log_lik*(-1));

  
}  

// [[Rcpp::export("loop_aca")]]
double loop_aca(Rcpp::StringMatrix allpaths,Rcpp::StringMatrix enreg_pos){
  double min_val=100000;
  double max_alpha=0;
  for(int i=0;i<1000;i++){
    double alpha = 0.001*(i+1);
    
    double res = aca_mle_cpp(allpaths,enreg_pos,alpha,3);
    if(res < min_val){
      max_alpha=alpha;
      min_val = res;
    }
  }
  Rcpp::Rcout <<  min_val<< std::endl;
  return(max_alpha);
}


