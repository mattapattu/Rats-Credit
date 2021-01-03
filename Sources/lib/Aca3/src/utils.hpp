#ifndef __UTILS__
#define __UTILS__

#include <vector>
#include <algorithm>
#include <string>
#include <regex>
#include <RcppArmadillo.h>
#include <RcppArmadilloExtensions/sample.h>


// [[Rcpp::export]]
Rcpp::StringVector getTurnsFromPaths(int path, int state)
{

  Rcpp::StringVector turns;
  if (state == 0)
  {
    if (path == 0)
    {
      turns.push_back("dch");
    }
    else if (path == 1)
    {
      turns.push_back("gak");
    }
    else if (path == 2)
    {
      turns.push_back("dcb");
      turns.push_back("bak");
    }
    else if (path == 3)
    {
      turns.push_back("gab");
      turns.push_back("bch");
    }
    else if (path == 4)
    {
      turns.push_back("gab");
      turns.push_back("bcd");
    }
  }
  else if (state == 1)
  {
    if (path == 0)
    {
      turns.push_back("hcd");
    }
    else if (path == 1)
    {
      turns.push_back("kag");
    }
    else if (path == 2)
    {
      turns.push_back("hcb");
      turns.push_back("bag");
    }
    else if (path == 3)
    {
      turns.push_back("kab");
      turns.push_back("bcd");
    }
    else if (path == 4)
    {
      turns.push_back("kab");
      turns.push_back("bch");
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
    turn = "dcb";
  }
  else if (turnNb == 1)
  {
    turn = "dch";
  }
  else if (turnNb == 2)
  {
    turn = "gak";
  }
  else if (turnNb == 3)
  {
    turn = "gab";
  }
  else if (turnNb == 4)
  {
    turn = "bak";
  }
  else if (turnNb == 5)
  {
    turn = "bag";
  }
  else if (turnNb == 6)
  {
    turn = "bcd";
  }
  else if (turnNb == 7)
  {
    turn = "bch";
  }
  else if (turnNb == 8)
  {
    turn = "hcb";
  }
  else if (turnNb == 9)
  {
    turn = "hcd";
  }
  else if (turnNb == 10)
  {
    turn = "kag";
  }
  else if (turnNb == 11)
  {
    turn = "kab";
  }
  else if (turnNb == 12)
  {
    turn = "bak";
  }
  else if (turnNb == 13)
  {
    turn = "bag";
  }
  else if (turnNb == 14)
  {
    turn = "bcd";
  }
  else if (turnNb == 15)
  {
    turn = "bch";
  }
  return (turn);
}

// [[Rcpp::export]]
unsigned int getTurnIdx(std::string turn, int state)
{
  unsigned int turnNb = 100;
  if (state == 0)
  {
    if (turn == "dcb")
    {
      turnNb = 0;
    }
    else if (turn == "dch")
    {
      turnNb = 1;
    }
    else if (turn == "gak")
    {
      turnNb = 2;
    }
    else if (turn == "gab")
    {
      turnNb = 3;
    }
    else if (turn == "bak")
    {
      turnNb = 4;
    }
    else if (turn == "bag")
    {
      turnNb = 5;
    }
    else if (turn == "bcd")
    {
      turnNb = 6;
    }
    else if (turn == "bch")
    {
      turnNb = 7;
    }
  }
  else if (state == 1)
  {
    if (turn == "hcb")
    {
      turnNb = 8;
    }
    else if (turn == "hcd")
    {
      turnNb = 9;
    }
    else if (turn == "kag")
    {
      turnNb = 10;
    }
    else if (turn == "kab")
    {
      turnNb = 11;
    }
    else if (turn == "bak")
    {
      turnNb = 12;
    }
    else if (turn == "bag")
    {
      turnNb = 13;
    }
    else if (turn == "bcd")
    {
      turnNb = 14;
    }
    else if (turn == "bch")
    {
      turnNb = 15;
    }
  }

  return turnNb;
}


// [[Rcpp::export]]
arma::mat getTurnTimes(Rcpp::CharacterMatrix allpaths, arma::vec boxTimes, int sim)
{
  
  int totalPaths = allpaths.nrow();
  int currBoxIdx = -1; // Since index starts from 0
  arma::mat res_mat;
  //Rcpp::Rcout <<  "totalPaths="<< totalPaths<<std::endl;
  for (int i = 0; i < totalPaths; i++)
  {
    std::string path_string = Rcpp::as<std::string>(allpaths(i, 2));
    std::string state_string = Rcpp::as<std::string>(allpaths(i, 4));
    std::string sessionNb_string = Rcpp::as<std::string>(allpaths(i, 1));
    //Rcpp::Rcout << "i=" << i << ", path_string =" << path_string <<std::endl;
    int path = std::stoi(path_string);
    int state = std::stoi(state_string);
    if (sim == 2)
    {
      path = path - 1;
      state = state - 1;
    }
    int sessionNb = std::stoi(sessionNb_string);
    Rcpp::StringVector turns;
    turns = getTurnsFromPaths(path, state);
    int nbOfTurns = turns.length();
    //Rcpp::Rcout << "i=" << i << ", path =" << path << " ,state=" << state << ", nbOfTurns=" <<nbOfTurns<<std::endl;
    //Rcpp::Rcout << "turns=" << turns << std::endl;
    
    for (int j = 0; j < nbOfTurns; j++)
    {
      
      std::string currTurn = Rcpp::as<std::string>(turns(j));
      // currTurn.erase(std::remove(turns(j).begin(), currTurn.end(), ','), currTurn.end());
      // currTurn.erase(std::remove(turns(j).begin(), currTurn.end(), ' '), currTurn.end());
      int turnIdx = getTurnIdx(currTurn, state);
      
      arma::rowvec new_row = {static_cast<double>(i + 1), static_cast<double>(path), static_cast<double>(state), static_cast<double>(turnIdx), static_cast<double>(sessionNb), 0, 0, 0};
      
      for (int method = 0; method < 3; method++)
      {
        
        std::string s = Rcpp::as<std::string>(allpaths(i, 0));
        s.erase(std::remove(s.begin(), s.end(), ','), s.end());
        s.erase(std::remove(s.begin(), s.end(), ' '), s.end());
        //Rcpp::Rcout <<"s=" <<s << ", method=" << method << std::endl;
        
        if (method == 0)
        {
          std::smatch m;
          std::string pattern = "(";
          pattern.push_back(currTurn.at(0));
          pattern += "(";
          pattern.push_back(currTurn.at(1));
          pattern += ".*";
          pattern.push_back(currTurn.at(2));
          pattern += ")+)";
          std::regex e(pattern);
          std::regex_search(s, m, e);
          
          int turnStartIdx = currBoxIdx + m.position(1) + 1;
          int turnEndIdx = currBoxIdx + m.position(1) + m[1].length(); //m[1].length() counts turnStartIdx also
          arma::uvec ids = arma::conv_to<arma::uvec>::from(arma::regspace(turnStartIdx, turnEndIdx));
          
          double turnTime = arma::accu(boxTimes.elem(ids));
          //Rcpp::Rcout << "boxTimes=" << boxTimes.elem(ids)<<std::endl;
          new_row(5 + method) = turnTime;
        }
        else if (method == 1)
        {
          std::smatch m;
          std::string pattern = "((";
          pattern.push_back(currTurn.at(0));
          pattern += ".*";
          pattern.push_back(currTurn.at(1));
          pattern += ")+)";
          pattern.push_back(currTurn.at(2));
          std::regex e(pattern);
          std::regex_search(s, m, e);
          
          int turnStartIdx = currBoxIdx + m.position(1) + 1;
          int turnEndIdx = currBoxIdx + m.position(1) + m[1].length(); //m[1].length() counts turnStartIdx also
          arma::uvec ids = arma::conv_to<arma::uvec>::from(arma::regspace(turnStartIdx, turnEndIdx));
          double turnTime = arma::accu(boxTimes.elem(ids));
          
          new_row(5 + method) = turnTime;
        }
        else if (method == 2)
        {
          std::smatch m;
          std::string pattern;
          pattern.push_back(currTurn.at(0));
          pattern += "((";
          pattern.push_back(currTurn.at(1));
          pattern += ".*";
          pattern.push_back(currTurn.at(2));
          pattern += ")+)";
          std::regex e(pattern);
          std::regex_search(s, m, e);
          
          int turnStartIdx = currBoxIdx + m.position(1) + 1;
          int turnEndIdx = currBoxIdx + m.position(1) + m[1].length(); //m[1].length() counts turnStartIdx also
          arma::uvec ids = arma::conv_to<arma::uvec>::from(arma::regspace(turnStartIdx, turnEndIdx));
          double turnTime = arma::accu(boxTimes.elem(ids));
          
          new_row(5 + method) = turnTime;
        }
        else if (method == 3)
        {
          std::smatch m;
          std::string pattern = ".*(";
          pattern.push_back(currTurn.at(1));
          pattern.push_back(')');
          std::regex e(pattern);
          std::regex_search(s, m, e);
          
          int turnStartIdx = currBoxIdx + m.position(1) + 1;
          arma::uvec ids(1);
          ids = turnStartIdx;
          double turnTime = arma::accu(boxTimes.elem(ids));
          
          new_row(5 + method) = turnTime;
        }
      }
      res_mat = arma::join_vert(res_mat, new_row);
    }
    
    std::string currPath = Rcpp::as<std::string>(allpaths(i, 0));
    currPath.erase(std::remove(currPath.begin(), currPath.end(), ','), currPath.end());
    currPath.erase(std::remove(currPath.begin(), currPath.end(), ' '), currPath.end());
    currBoxIdx = currBoxIdx + currPath.length();
    //Rcpp::Rcout << "currBoxIdx=" << currBoxIdx << ", currPath=" << currPath << ", currPath.length=" << currPath.length() << std::endl;
  }
  return (res_mat);
}

#endif