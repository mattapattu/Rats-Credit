// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::depends(BH)]]
#include <vector>
#include <boost/multiprecision/float128.hpp>
#include <RcppArmadillo.h>
#include <boost/regex.hpp>
#include <regex>



namespace mp = boost::multiprecision;

using namespace Rcpp;


// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//


// cppFunction('NumericVector rowSumsC(NumericMatrix x) {
//   int nrow = x.nrow(), ncol = x.ncol();
//   NumericVector out(nrow);
//   
//   for (int i = 0; i < nrow; i++) {
//     double total = 0;
//     for (int j = 0; j < ncol; j++) {
//       total += x(i, j);
//     }
//     out[i] = total;
//   }
//   return out;
// }')
// // [[Rcpp::export("checkNull")]]
// bool checkNull(Nullable<NumericVector> x) {
//   if (x.isNotNull()) {
//     // do something
//     NumericVector xx(x);
//     Rcpp::Rcout << "Sum is " << sum(xx) << std::endl;
//     return true;
//   } else {
//     // do nothing
//     Rcpp::Rcout << "Nothing to see" << std::endl;
//     return false;
//   }
// }



// [[Rcpp::export("aca_mle_cpp")]]
int aca_mle_cpp(StringMatrix allpaths,Rcpp::List enreg){
  try {
    float H_mat[2][6]={};
    float Score_mat[2][6]={};
    int Visit_mat[2][6]={};
    std::vector<int> activations;
    
    std::vector<mp::float128> QProb;
    
    int episode=1;
    int S=0;
    int A=0;
    Rcpp::NumericVector actions;
    Rcpp::NumericVector states;
    Rcpp::Rcout <<  "Here1"<< std::endl;
    std::regex a("^.*e$"); 
    std::regex b("^.*i$");
    std::cmatch base_match;
    
    std::string s = Rcpp::as<std::string>(allpaths(1,1));
    if(std::regex_match(s,a)){
      S=1;
    }else if(std::regex_match(s,b)){
      S=2;
    }else{
      // printf("Unknown intial state. Check");
    }
    Rcpp::Rcout <<  "Here2"<< std::endl;
    
    int initState=0;
    bool changeState = false;
    bool returnToInitState = false;
    int score_episode=0;
    int episodeFin=0;
    int prev_ses=0;
    int start_ses_pos=0;
    int trial_pos=0
    
    int nrow = allpaths.nrow(), ncol = allpaths.ncol();
    Rcpp::CharacterMatrix allpaths_cpp = Rcpp::as<Rcpp::CharacterMatrix>(allpaths);
    for (int i = 1; i < 2; i++) {
      
      if(actions.length()==0){
        initState=S;
      }
      Rcpp::Rcout <<  "Here3"<< std::endl;
      std::string s1 = Rcpp::as<std::string>(allpaths(i,2));
      int ses=std::atoi(s1.c_str())-1;
      Rcpp::Rcout <<  "ses=" << ses<< std::endl;
      Rcpp::CharacterVector allpaths_ses = allpaths(_,2);
      
      if(prev_ses!=ses){
        start_ses_pos=i;
      }

      int trial=i-start_ses_pos+1;
      Rcpp::Rcout <<  "trial="<< trial << std::endl;
      Rcpp::List enreg_cpp=Rcpp::as<Rcpp::List>(enreg);
      Rcout << "Nb of sessions =" << enreg_cpp.size() << std::endl;
      
      Rcpp::List enreg_cpp_ses = Rcpp::as<Rcpp::List>(enreg_cpp[ses]);
      int n = Rcpp::as<Rcpp::List>(enreg_cpp_ses).size();
      Rcout << "Nb of matrices =" << n << std::endl;
      
      int max_trials=Rcpp::as<Rcpp::CharacterMatrix>(enreg_cpp_ses(0)).nrow();
      Rf_PrintValue(enreg_cpp_ses(0));
       
      
      
      //print(enreg_cpp_ses[3]);
      //int n2=Rcpp::as<Rcpp::CharacterMatrix>(enreg_cpp_ses[2]).nrow();
      //int n3=Rcpp::as<Rcpp::CharacterMatrix>(enreg_cpp_ses[3]).nrow();
      Rcout << "Nb of rows =" << n1 << std::endl;
       //Rcout << "Nb of rows2 =" << enreg_cpp_ses[2] << std::endl;
       //Rcout << "Nb of rows3 =" << enreg_cpp_ses[3]  << std::endl;
      
      Rcpp::CharacterMatrix pos_ses_mat = Rcpp::as<Rcpp::CharacterMatrix>(enreg_cpp_ses(0));
      std::string s = Rcpp::as<std::string>(pos_ses_mat(1,2));
      //Rcout << "POS matrix is" << std::endl << pos_ses_mat << std::endl;
      Rcpp::Rcout <<  "s="<<  s << std::endl;
      Rcpp::Rcout <<  "Here5"<< std::endl;
      
      
      int R=0;
      while(std::atoi(pos_ses_mat(trial_pos,5).c_str())==trial){
        R=R+std::atoi(pos_ses_mat(trial_pos,3).c_str());
        trial_pos=trial_pos+1;
      }
      
      if(R>0){
          score_episode=score_episode+1;
        }
      A=std::atoi(allpaths_cpp(i,2).c_str());
     int S_prime=getNextState(allpaths_cpp,i);  
      if(A == 4 && S == 1){
          actions.push_back(51);
        }else if(A == 4 && S == 2){
          actions.push_back(49);
        }else{
          actions.push_back(A);
        }
      states.push_back(S);
      Visits(S,A) = Visits(S,A)+1;
      if(S_prime!=initState){
          changeState = true;
        }else if(S_prime==initState && changeState){
          returnToInitState = true;
        }
        
        
        
        
      prev_ses = ses;
    }
    
    return 0; 
  } catch(std::exception &ex) {	
    Rcpp::Rcout <<  ex.what() << std::endl;
    return(-1);
  }catch(Rcpp::exception &e) {
    return(-2);
  }
  catch(Rcpp::internal::InterruptedException &e) {
    return(-3);
  } catch(...) { 
    Rcpp::Rcout <<  "Unknown error" << std::endl;
    return(-1);
  }
  
}  



// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//


