#ifndef __UTILS__
#define __UTILS__

#include <vector>
#include <RcppArmadillo.h>

#include <regex>
#include <RcppArmadilloExtensions/sample.h>

inline double softmax_cpp3(int A,int S,arma::mat &H){
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

// [[Rcpp::export()]]
arma::vec getTrialTimes(Rcpp::NumericVector allpaths,Rcpp::NumericMatrix enreg_pos){
  
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

// [[Rcpp::export()]]
arma::mat empiricalProbMat(arma::mat allpaths, int window){
  
  int nrow = allpaths.n_rows;
  arma::mat probMatrix=arma::zeros(nrow,12);
  
  for(int i=0;i<nrow;i++){
    
    int S = allpaths(i,1);
    arma::vec states;
    arma::vec actions;
    if(i >= window){
      states = allpaths(arma::span((i-window),i),1);
      actions = allpaths(arma::span((i-window),i),0);
    }else{
      states = allpaths(arma::span(0,i),1);
      actions = allpaths(arma::span(0,i),0);
    }

    arma::uvec state_idx = arma::find(states == S);
    
    arma::vec actions_in_state = actions.elem(state_idx);
    
    
    //Rcpp::Rcout <<"act_idx.n_elem="<<act_idx.n_elem<<", s1_idx.n_elem="<<s1_idx.n_elem<<std::endl;
    
    
    //Rcpp::Rcout <<"states="<<states<<std::endl;
    if(S == 1){
      probMatrix.submat(i,6,i,11)=arma::zeros(1,6);
      for(int act=1;act<7;act++){
        
        
        arma::uvec act_idx = arma::find(actions_in_state == act);
        double x = act_idx.n_elem/(1.0*state_idx.n_elem);
        probMatrix(i,(act-1)) = x;
      }
    }else if(S == 2){
      probMatrix.submat(i,0,i,5) = arma::zeros(1,6);
      for(int act=1;act<7;act++){
        
        arma::uvec act_idx = arma::find(actions_in_state == act);
        double x = act_idx.n_elem/(1.0*state_idx.n_elem);
        probMatrix(i,(5+act)) = x;
      }
    }
    
  }
  
  return(probMatrix);
}

// [[Rcpp::export()]]
arma::vec mseEmpirical(arma::mat allpaths,arma::mat probMatrix_m1,arma::vec movAvg,int sim){
  
  arma::vec mseMatrix=arma::zeros(allpaths.n_rows);
  //arma::mat movAvg=arma::zeros(allpaths.n_rows);
  //arma::vec allpath_actions = allpaths.col(0);
  //arma::vec allpath_states = allpaths.col(1);
  
  Rcpp::Rcout <<"allpaths.n_rows="<<allpaths.n_rows<<std::endl;
  
  for(unsigned int i=0;i<(allpaths.n_rows);i++){
    int action=allpaths(i,0);
    int state=allpaths(i,1);

    if(sim != 1){
      state=state-1;
      action=action-1;
    }
    
    //mseMatrix(i)=pow(movAvg(i)-probMatrix_m1(i,((6*state)+action)),2);
    mseMatrix(i)=abs(movAvg(i)-probMatrix_m1(i,((6*state)+action)));
    
    //Rcpp::Rcout <<"movAvg(i)="<<movAvg(i)<<", prob=" <<probMatrix_m1(i,((6*state)+action))<<", mseMatrix(i)="<<mseMatrix(i)<<std::endl;
  }
  //Rcpp::Rcout <<"movAvg="<<movAvg<<std::endl;
  return(mseMatrix);
}

// [[Rcpp::export()]]
arma::vec pathProbability(arma::mat allpaths,arma::mat probMatrix_m1,int sim){
  
  arma::vec mseMatrix=arma::zeros(allpaths.n_rows);
  //arma::mat movAvg=arma::zeros(allpaths.n_rows);
  //arma::vec allpath_actions = allpaths.col(0);
  //arma::vec allpath_states = allpaths.col(1);
  
  //Rcpp::Rcout <<"allpaths.n_rows="<<allpaths.n_rows<<std::endl;
  
  for(unsigned int i=0;i<(allpaths.n_rows);i++){
    int action=allpaths(i,0);
    int state=allpaths(i,1);
    
    if(sim != 1){
      state=state-1;
      action=action-1;
    }
    
    //mseMatrix(i)=pow(movAvg(i)-probMatrix_m1(i,((6*state)+action)),2);
    mseMatrix(i)=probMatrix_m1(i,((6*state)+action));
    
    //Rcpp::Rcout <<"movAvg(i)="<<movAvg(i)<<", prob=" <<probMatrix_m1(i,((6*state)+action))<<", mseMatrix(i)="<<mseMatrix(i)<<std::endl;
  }
  //Rcpp::Rcout <<"movAvg="<<movAvg<<std::endl;
  return(mseMatrix);
}





#endif