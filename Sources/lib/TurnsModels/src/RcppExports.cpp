// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// simulateTurnsModels
Rcpp::List simulateTurnsModels(arma::mat allpaths, arma::mat turnTimes, double alpha, int model, int turnMethod);
RcppExport SEXP _TurnsModels_simulateTurnsModels(SEXP allpathsSEXP, SEXP turnTimesSEXP, SEXP alphaSEXP, SEXP modelSEXP, SEXP turnMethodSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type allpaths(allpathsSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type turnTimes(turnTimesSEXP);
    Rcpp::traits::input_parameter< double >::type alpha(alphaSEXP);
    Rcpp::traits::input_parameter< int >::type model(modelSEXP);
    Rcpp::traits::input_parameter< int >::type turnMethod(turnMethodSEXP);
    rcpp_result_gen = Rcpp::wrap(simulateTurnsModels(allpaths, turnTimes, alpha, model, turnMethod));
    return rcpp_result_gen;
END_RCPP
}
// getTurnsLikelihood
Rcpp::NumericVector getTurnsLikelihood(arma::mat allpaths, arma::mat turnTimes, int turnMethod, double alpha, double rewardVal, int sim, int model);
RcppExport SEXP _TurnsModels_getTurnsLikelihood(SEXP allpathsSEXP, SEXP turnTimesSEXP, SEXP turnMethodSEXP, SEXP alphaSEXP, SEXP rewardValSEXP, SEXP simSEXP, SEXP modelSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type allpaths(allpathsSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type turnTimes(turnTimesSEXP);
    Rcpp::traits::input_parameter< int >::type turnMethod(turnMethodSEXP);
    Rcpp::traits::input_parameter< double >::type alpha(alphaSEXP);
    Rcpp::traits::input_parameter< double >::type rewardVal(rewardValSEXP);
    Rcpp::traits::input_parameter< int >::type sim(simSEXP);
    Rcpp::traits::input_parameter< int >::type model(modelSEXP);
    rcpp_result_gen = Rcpp::wrap(getTurnsLikelihood(allpaths, turnTimes, turnMethod, alpha, rewardVal, sim, model));
    return rcpp_result_gen;
END_RCPP
}
// getProbMatrix
arma::mat getProbMatrix(arma::mat allpaths, arma::mat turnTimes, int turnMethod, double alpha, double rewardVal, int sim, int model);
RcppExport SEXP _TurnsModels_getProbMatrix(SEXP allpathsSEXP, SEXP turnTimesSEXP, SEXP turnMethodSEXP, SEXP alphaSEXP, SEXP rewardValSEXP, SEXP simSEXP, SEXP modelSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type allpaths(allpathsSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type turnTimes(turnTimesSEXP);
    Rcpp::traits::input_parameter< int >::type turnMethod(turnMethodSEXP);
    Rcpp::traits::input_parameter< double >::type alpha(alphaSEXP);
    Rcpp::traits::input_parameter< double >::type rewardVal(rewardValSEXP);
    Rcpp::traits::input_parameter< int >::type sim(simSEXP);
    Rcpp::traits::input_parameter< int >::type model(modelSEXP);
    rcpp_result_gen = Rcpp::wrap(getProbMatrix(allpaths, turnTimes, turnMethod, alpha, rewardVal, sim, model));
    return rcpp_result_gen;
END_RCPP
}
// rcpparma_hello_world
arma::mat rcpparma_hello_world();
RcppExport SEXP _TurnsModels_rcpparma_hello_world() {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    rcpp_result_gen = Rcpp::wrap(rcpparma_hello_world());
    return rcpp_result_gen;
END_RCPP
}
// rcpparma_outerproduct
arma::mat rcpparma_outerproduct(const arma::colvec& x);
RcppExport SEXP _TurnsModels_rcpparma_outerproduct(SEXP xSEXP) {
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
RcppExport SEXP _TurnsModels_rcpparma_innerproduct(SEXP xSEXP) {
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
RcppExport SEXP _TurnsModels_rcpparma_bothproducts(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::colvec& >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpparma_bothproducts(x));
    return rcpp_result_gen;
END_RCPP
}
// getTrialTimes
arma::vec getTrialTimes(Rcpp::NumericVector allpaths, Rcpp::NumericMatrix enreg_pos);
RcppExport SEXP _TurnsModels_getTrialTimes(SEXP allpathsSEXP, SEXP enreg_posSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type allpaths(allpathsSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type enreg_pos(enreg_posSEXP);
    rcpp_result_gen = Rcpp::wrap(getTrialTimes(allpaths, enreg_pos));
    return rcpp_result_gen;
END_RCPP
}
// empiricalProbMat
arma::mat empiricalProbMat(arma::mat allpaths, int window);
RcppExport SEXP _TurnsModels_empiricalProbMat(SEXP allpathsSEXP, SEXP windowSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type allpaths(allpathsSEXP);
    Rcpp::traits::input_parameter< int >::type window(windowSEXP);
    rcpp_result_gen = Rcpp::wrap(empiricalProbMat(allpaths, window));
    return rcpp_result_gen;
END_RCPP
}
// mseEmpirical
arma::vec mseEmpirical(arma::mat allpaths, arma::mat probMatrix_m1, arma::vec movAvg, int sim);
RcppExport SEXP _TurnsModels_mseEmpirical(SEXP allpathsSEXP, SEXP probMatrix_m1SEXP, SEXP movAvgSEXP, SEXP simSEXP) {
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
RcppExport SEXP _TurnsModels_pathProbability(SEXP allpathsSEXP, SEXP probMatrix_m1SEXP, SEXP simSEXP) {
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
RcppExport SEXP _TurnsModels_getBoxTimes(SEXP enregPosTimesSEXP, SEXP rleLengthsSEXP) {
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
RcppExport SEXP _TurnsModels_getTurnsFromPaths(SEXP pathSEXP, SEXP stateSEXP) {
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
RcppExport SEXP _TurnsModels_getTurnString(SEXP turnNbSEXP) {
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
RcppExport SEXP _TurnsModels_getTurnIdx(SEXP turnSEXP, SEXP stateSEXP) {
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
RcppExport SEXP _TurnsModels_getTurnTimes(SEXP allpathsSEXP, SEXP boxTimesSEXP, SEXP simSEXP) {
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
// getPathFromTurns
int getPathFromTurns(Rcpp::StringVector turns, int state);
RcppExport SEXP _TurnsModels_getPathFromTurns(SEXP turnsSEXP, SEXP stateSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::StringVector >::type turns(turnsSEXP);
    Rcpp::traits::input_parameter< int >::type state(stateSEXP);
    rcpp_result_gen = Rcpp::wrap(getPathFromTurns(turns, state));
    return rcpp_result_gen;
END_RCPP
}
// getPathProbMatrix
arma::mat getPathProbMatrix(arma::mat turnProbMat, arma::mat allpaths, int sim);
RcppExport SEXP _TurnsModels_getPathProbMatrix(SEXP turnProbMatSEXP, SEXP allpathsSEXP, SEXP simSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type turnProbMat(turnProbMatSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type allpaths(allpathsSEXP);
    Rcpp::traits::input_parameter< int >::type sim(simSEXP);
    rcpp_result_gen = Rcpp::wrap(getPathProbMatrix(turnProbMat, allpaths, sim));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_TurnsModels_simulateTurnsModels", (DL_FUNC) &_TurnsModels_simulateTurnsModels, 5},
    {"_TurnsModels_getTurnsLikelihood", (DL_FUNC) &_TurnsModels_getTurnsLikelihood, 7},
    {"_TurnsModels_getProbMatrix", (DL_FUNC) &_TurnsModels_getProbMatrix, 7},
    {"_TurnsModels_rcpparma_hello_world", (DL_FUNC) &_TurnsModels_rcpparma_hello_world, 0},
    {"_TurnsModels_rcpparma_outerproduct", (DL_FUNC) &_TurnsModels_rcpparma_outerproduct, 1},
    {"_TurnsModels_rcpparma_innerproduct", (DL_FUNC) &_TurnsModels_rcpparma_innerproduct, 1},
    {"_TurnsModels_rcpparma_bothproducts", (DL_FUNC) &_TurnsModels_rcpparma_bothproducts, 1},
    {"_TurnsModels_getTrialTimes", (DL_FUNC) &_TurnsModels_getTrialTimes, 2},
    {"_TurnsModels_empiricalProbMat", (DL_FUNC) &_TurnsModels_empiricalProbMat, 2},
    {"_TurnsModels_mseEmpirical", (DL_FUNC) &_TurnsModels_mseEmpirical, 4},
    {"_TurnsModels_pathProbability", (DL_FUNC) &_TurnsModels_pathProbability, 3},
    {"_TurnsModels_getBoxTimes", (DL_FUNC) &_TurnsModels_getBoxTimes, 2},
    {"_TurnsModels_getTurnsFromPaths", (DL_FUNC) &_TurnsModels_getTurnsFromPaths, 2},
    {"_TurnsModels_getTurnString", (DL_FUNC) &_TurnsModels_getTurnString, 1},
    {"_TurnsModels_getTurnIdx", (DL_FUNC) &_TurnsModels_getTurnIdx, 2},
    {"_TurnsModels_getTurnTimes", (DL_FUNC) &_TurnsModels_getTurnTimes, 3},
    {"_TurnsModels_getPathFromTurns", (DL_FUNC) &_TurnsModels_getPathFromTurns, 2},
    {"_TurnsModels_getPathProbMatrix", (DL_FUNC) &_TurnsModels_getPathProbMatrix, 3},
    {NULL, NULL, 0}
};

RcppExport void R_init_TurnsModels(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
