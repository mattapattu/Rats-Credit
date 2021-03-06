// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// simulateTrials
Rcpp::List simulateTrials(arma::mat allpaths, arma::mat turnTimes, double alpha, int model, int turnMethod);
RcppExport SEXP _baseModels_simulateTrials(SEXP allpathsSEXP, SEXP turnTimesSEXP, SEXP alphaSEXP, SEXP modelSEXP, SEXP turnMethodSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type allpaths(allpathsSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type turnTimes(turnTimesSEXP);
    Rcpp::traits::input_parameter< double >::type alpha(alphaSEXP);
    Rcpp::traits::input_parameter< int >::type model(modelSEXP);
    Rcpp::traits::input_parameter< int >::type turnMethod(turnMethodSEXP);
    rcpp_result_gen = Rcpp::wrap(simulateTrials(allpaths, turnTimes, alpha, model, turnMethod));
    return rcpp_result_gen;
END_RCPP
}
// getPathLikelihood
arma::vec getPathLikelihood(arma::mat allpaths, double alpha, arma::mat H, int sim, int model, int policyMethod, double epsilon, int endTrial);
RcppExport SEXP _baseModels_getPathLikelihood(SEXP allpathsSEXP, SEXP alphaSEXP, SEXP HSEXP, SEXP simSEXP, SEXP modelSEXP, SEXP policyMethodSEXP, SEXP epsilonSEXP, SEXP endTrialSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type allpaths(allpathsSEXP);
    Rcpp::traits::input_parameter< double >::type alpha(alphaSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type H(HSEXP);
    Rcpp::traits::input_parameter< int >::type sim(simSEXP);
    Rcpp::traits::input_parameter< int >::type model(modelSEXP);
    Rcpp::traits::input_parameter< int >::type policyMethod(policyMethodSEXP);
    Rcpp::traits::input_parameter< double >::type epsilon(epsilonSEXP);
    Rcpp::traits::input_parameter< int >::type endTrial(endTrialSEXP);
    rcpp_result_gen = Rcpp::wrap(getPathLikelihood(allpaths, alpha, H, sim, model, policyMethod, epsilon, endTrial));
    return rcpp_result_gen;
END_RCPP
}
// getProbMatrix
arma::mat getProbMatrix(arma::mat allpaths, double alpha, arma::mat H, int sim, int model, int policyMethod, double epsilon, int endTrial);
RcppExport SEXP _baseModels_getProbMatrix(SEXP allpathsSEXP, SEXP alphaSEXP, SEXP HSEXP, SEXP simSEXP, SEXP modelSEXP, SEXP policyMethodSEXP, SEXP epsilonSEXP, SEXP endTrialSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type allpaths(allpathsSEXP);
    Rcpp::traits::input_parameter< double >::type alpha(alphaSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type H(HSEXP);
    Rcpp::traits::input_parameter< int >::type sim(simSEXP);
    Rcpp::traits::input_parameter< int >::type model(modelSEXP);
    Rcpp::traits::input_parameter< int >::type policyMethod(policyMethodSEXP);
    Rcpp::traits::input_parameter< double >::type epsilon(epsilonSEXP);
    Rcpp::traits::input_parameter< int >::type endTrial(endTrialSEXP);
    rcpp_result_gen = Rcpp::wrap(getProbMatrix(allpaths, alpha, H, sim, model, policyMethod, epsilon, endTrial));
    return rcpp_result_gen;
END_RCPP
}
// getEpisodes
arma::mat getEpisodes(arma::mat allpaths);
RcppExport SEXP _baseModels_getEpisodes(SEXP allpathsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type allpaths(allpathsSEXP);
    rcpp_result_gen = Rcpp::wrap(getEpisodes(allpaths));
    return rcpp_result_gen;
END_RCPP
}
// rcpparma_hello_world
arma::mat rcpparma_hello_world();
RcppExport SEXP _baseModels_rcpparma_hello_world() {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    rcpp_result_gen = Rcpp::wrap(rcpparma_hello_world());
    return rcpp_result_gen;
END_RCPP
}
// rcpparma_outerproduct
arma::mat rcpparma_outerproduct(const arma::colvec& x);
RcppExport SEXP _baseModels_rcpparma_outerproduct(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::colvec& >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpparma_outerproduct(x));
    return rcpp_result_gen;
END_RCPP
}
// rcpparma_innerproduct
double rcpparma_innerproduct(const arma::colvec& x);
RcppExport SEXP _baseModels_rcpparma_innerproduct(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::colvec& >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpparma_innerproduct(x));
    return rcpp_result_gen;
END_RCPP
}
// rcpparma_bothproducts
Rcpp::List rcpparma_bothproducts(const arma::colvec& x);
RcppExport SEXP _baseModels_rcpparma_bothproducts(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::colvec& >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpparma_bothproducts(x));
    return rcpp_result_gen;
END_RCPP
}
// getPathTimes
arma::vec getPathTimes(Rcpp::NumericVector allpaths, Rcpp::NumericMatrix enreg_pos);
RcppExport SEXP _baseModels_getPathTimes(SEXP allpathsSEXP, SEXP enreg_posSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type allpaths(allpathsSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type enreg_pos(enreg_posSEXP);
    rcpp_result_gen = Rcpp::wrap(getPathTimes(allpaths, enreg_pos));
    return rcpp_result_gen;
END_RCPP
}
// empiricalProbMat
arma::mat empiricalProbMat(arma::mat allpaths, int window);
RcppExport SEXP _baseModels_empiricalProbMat(SEXP allpathsSEXP, SEXP windowSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type allpaths(allpathsSEXP);
    Rcpp::traits::input_parameter< int >::type window(windowSEXP);
    rcpp_result_gen = Rcpp::wrap(empiricalProbMat(allpaths, window));
    return rcpp_result_gen;
END_RCPP
}
// empiricalProbMat2
arma::mat empiricalProbMat2(arma::mat allpaths, int window);
RcppExport SEXP _baseModels_empiricalProbMat2(SEXP allpathsSEXP, SEXP windowSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type allpaths(allpathsSEXP);
    Rcpp::traits::input_parameter< int >::type window(windowSEXP);
    rcpp_result_gen = Rcpp::wrap(empiricalProbMat2(allpaths, window));
    return rcpp_result_gen;
END_RCPP
}
// mseEmpirical
arma::vec mseEmpirical(arma::mat allpaths, arma::mat probMatrix_m1, arma::vec movAvg, int sim);
RcppExport SEXP _baseModels_mseEmpirical(SEXP allpathsSEXP, SEXP probMatrix_m1SEXP, SEXP movAvgSEXP, SEXP simSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type allpaths(allpathsSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type probMatrix_m1(probMatrix_m1SEXP);
    Rcpp::traits::input_parameter< arma::vec >::type movAvg(movAvgSEXP);
    Rcpp::traits::input_parameter< int >::type sim(simSEXP);
    rcpp_result_gen = Rcpp::wrap(mseEmpirical(allpaths, probMatrix_m1, movAvg, sim));
    return rcpp_result_gen;
END_RCPP
}
// pathProbability
arma::vec pathProbability(arma::mat allpaths, arma::mat probMatrix_m1, int sim);
RcppExport SEXP _baseModels_pathProbability(SEXP allpathsSEXP, SEXP probMatrix_m1SEXP, SEXP simSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type allpaths(allpathsSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type probMatrix_m1(probMatrix_m1SEXP);
    Rcpp::traits::input_parameter< int >::type sim(simSEXP);
    rcpp_result_gen = Rcpp::wrap(pathProbability(allpaths, probMatrix_m1, sim));
    return rcpp_result_gen;
END_RCPP
}
// getBoxTimes
arma::vec getBoxTimes(arma::vec enregPosTimes, Rcpp::IntegerVector rleLengths);
RcppExport SEXP _baseModels_getBoxTimes(SEXP enregPosTimesSEXP, SEXP rleLengthsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type enregPosTimes(enregPosTimesSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type rleLengths(rleLengthsSEXP);
    rcpp_result_gen = Rcpp::wrap(getBoxTimes(enregPosTimes, rleLengths));
    return rcpp_result_gen;
END_RCPP
}
// getTurnsFromPaths
Rcpp::StringVector getTurnsFromPaths(int path, int state);
RcppExport SEXP _baseModels_getTurnsFromPaths(SEXP pathSEXP, SEXP stateSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type path(pathSEXP);
    Rcpp::traits::input_parameter< int >::type state(stateSEXP);
    rcpp_result_gen = Rcpp::wrap(getTurnsFromPaths(path, state));
    return rcpp_result_gen;
END_RCPP
}
// getTurnString
std::string getTurnString(int turnNb);
RcppExport SEXP _baseModels_getTurnString(SEXP turnNbSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type turnNb(turnNbSEXP);
    rcpp_result_gen = Rcpp::wrap(getTurnString(turnNb));
    return rcpp_result_gen;
END_RCPP
}
// getTurnIdx
unsigned int getTurnIdx(std::string turn, int state);
RcppExport SEXP _baseModels_getTurnIdx(SEXP turnSEXP, SEXP stateSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::string >::type turn(turnSEXP);
    Rcpp::traits::input_parameter< int >::type state(stateSEXP);
    rcpp_result_gen = Rcpp::wrap(getTurnIdx(turn, state));
    return rcpp_result_gen;
END_RCPP
}
// getTurnTimes
arma::mat getTurnTimes(Rcpp::CharacterMatrix allpaths, arma::vec boxTimes, int sim);
RcppExport SEXP _baseModels_getTurnTimes(SEXP allpathsSEXP, SEXP boxTimesSEXP, SEXP simSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::CharacterMatrix >::type allpaths(allpathsSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type boxTimes(boxTimesSEXP);
    Rcpp::traits::input_parameter< int >::type sim(simSEXP);
    rcpp_result_gen = Rcpp::wrap(getTurnTimes(allpaths, boxTimes, sim));
    return rcpp_result_gen;
END_RCPP
}
// getComputationalActivity
arma::vec getComputationalActivity(arma::mat allpaths, arma::mat probabilityMatrix);
RcppExport SEXP _baseModels_getComputationalActivity(SEXP allpathsSEXP, SEXP probabilityMatrixSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type allpaths(allpathsSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type probabilityMatrix(probabilityMatrixSEXP);
    rcpp_result_gen = Rcpp::wrap(getComputationalActivity(allpaths, probabilityMatrix));
    return rcpp_result_gen;
END_RCPP
}
// getPathFromTurns
int getPathFromTurns(Rcpp::StringVector turns, int state);
RcppExport SEXP _baseModels_getPathFromTurns(SEXP turnsSEXP, SEXP stateSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::StringVector >::type turns(turnsSEXP);
    Rcpp::traits::input_parameter< int >::type state(stateSEXP);
    rcpp_result_gen = Rcpp::wrap(getPathFromTurns(turns, state));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_baseModels_simulateTrials", (DL_FUNC) &_baseModels_simulateTrials, 5},
    {"_baseModels_getPathLikelihood", (DL_FUNC) &_baseModels_getPathLikelihood, 8},
    {"_baseModels_getProbMatrix", (DL_FUNC) &_baseModels_getProbMatrix, 8},
    {"_baseModels_getEpisodes", (DL_FUNC) &_baseModels_getEpisodes, 1},
    {"_baseModels_rcpparma_hello_world", (DL_FUNC) &_baseModels_rcpparma_hello_world, 0},
    {"_baseModels_rcpparma_outerproduct", (DL_FUNC) &_baseModels_rcpparma_outerproduct, 1},
    {"_baseModels_rcpparma_innerproduct", (DL_FUNC) &_baseModels_rcpparma_innerproduct, 1},
    {"_baseModels_rcpparma_bothproducts", (DL_FUNC) &_baseModels_rcpparma_bothproducts, 1},
    {"_baseModels_getPathTimes", (DL_FUNC) &_baseModels_getPathTimes, 2},
    {"_baseModels_empiricalProbMat", (DL_FUNC) &_baseModels_empiricalProbMat, 2},
    {"_baseModels_empiricalProbMat2", (DL_FUNC) &_baseModels_empiricalProbMat2, 2},
    {"_baseModels_mseEmpirical", (DL_FUNC) &_baseModels_mseEmpirical, 4},
    {"_baseModels_pathProbability", (DL_FUNC) &_baseModels_pathProbability, 3},
    {"_baseModels_getBoxTimes", (DL_FUNC) &_baseModels_getBoxTimes, 2},
    {"_baseModels_getTurnsFromPaths", (DL_FUNC) &_baseModels_getTurnsFromPaths, 2},
    {"_baseModels_getTurnString", (DL_FUNC) &_baseModels_getTurnString, 1},
    {"_baseModels_getTurnIdx", (DL_FUNC) &_baseModels_getTurnIdx, 2},
    {"_baseModels_getTurnTimes", (DL_FUNC) &_baseModels_getTurnTimes, 3},
    {"_baseModels_getComputationalActivity", (DL_FUNC) &_baseModels_getComputationalActivity, 2},
    {"_baseModels_getPathFromTurns", (DL_FUNC) &_baseModels_getPathFromTurns, 2},
    {NULL, NULL, 0}
};

RcppExport void R_init_baseModels(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
