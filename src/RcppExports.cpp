// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// getPIP
IntegerVector getPIP(IntegerVector seeds, IntegerVector scans, NumericVector mzs, IntegerVector clu, double mztol, int gap);
RcppExport SEXP KPIC_getPIP(SEXP seedsSEXP, SEXP scansSEXP, SEXP mzsSEXP, SEXP cluSEXP, SEXP mztolSEXP, SEXP gapSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type seeds(seedsSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type scans(scansSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type mzs(mzsSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type clu(cluSEXP);
    Rcpp::traits::input_parameter< double >::type mztol(mztolSEXP);
    Rcpp::traits::input_parameter< int >::type gap(gapSEXP);
    rcpp_result_gen = Rcpp::wrap(getPIP(seeds, scans, mzs, clu, mztol, gap));
    return rcpp_result_gen;
END_RCPP
}
// getPIP_kmeans
IntegerVector getPIP_kmeans(IntegerVector seeds, IntegerVector scans, NumericVector mzs, NumericVector ints, IntegerVector clu, double mztol, int gap, int min_width, int max_width, double alpha);
RcppExport SEXP KPIC_getPIP_kmeans(SEXP seedsSEXP, SEXP scansSEXP, SEXP mzsSEXP, SEXP intsSEXP, SEXP cluSEXP, SEXP mztolSEXP, SEXP gapSEXP, SEXP min_widthSEXP, SEXP max_widthSEXP, SEXP alphaSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type seeds(seedsSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type scans(scansSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type mzs(mzsSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type ints(intsSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type clu(cluSEXP);
    Rcpp::traits::input_parameter< double >::type mztol(mztolSEXP);
    Rcpp::traits::input_parameter< int >::type gap(gapSEXP);
    Rcpp::traits::input_parameter< int >::type min_width(min_widthSEXP);
    Rcpp::traits::input_parameter< int >::type max_width(max_widthSEXP);
    Rcpp::traits::input_parameter< double >::type alpha(alphaSEXP);
    rcpp_result_gen = Rcpp::wrap(getPIP_kmeans(seeds, scans, mzs, ints, clu, mztol, gap, min_width, max_width, alpha));
    return rcpp_result_gen;
END_RCPP
}
// findCandidate
NumericVector findCandidate(int ind, double mzmin, double mzmax, double rtmin, double rtmax, NumericVector mz, NumericVector rt, NumericVector group);
RcppExport SEXP KPIC_findCandidate(SEXP indSEXP, SEXP mzminSEXP, SEXP mzmaxSEXP, SEXP rtminSEXP, SEXP rtmaxSEXP, SEXP mzSEXP, SEXP rtSEXP, SEXP groupSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type ind(indSEXP);
    Rcpp::traits::input_parameter< double >::type mzmin(mzminSEXP);
    Rcpp::traits::input_parameter< double >::type mzmax(mzmaxSEXP);
    Rcpp::traits::input_parameter< double >::type rtmin(rtminSEXP);
    Rcpp::traits::input_parameter< double >::type rtmax(rtmaxSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type mz(mzSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type rt(rtSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type group(groupSEXP);
    rcpp_result_gen = Rcpp::wrap(findCandidate(ind, mzmin, mzmax, rtmin, rtmax, mz, rt, group));
    return rcpp_result_gen;
END_RCPP
}
// waveft
NumericMatrix waveft(NumericVector omega, NumericVector scales);
RcppExport SEXP KPIC_waveft(SEXP omegaSEXP, SEXP scalesSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type omega(omegaSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type scales(scalesSEXP);
    rcpp_result_gen = Rcpp::wrap(waveft(omega, scales));
    return rcpp_result_gen;
END_RCPP
}
// cwtft
List cwtft(NumericVector val);
RcppExport SEXP KPIC_cwtft(SEXP valSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type val(valSEXP);
    rcpp_result_gen = Rcpp::wrap(cwtft(val));
    return rcpp_result_gen;
END_RCPP
}
// localMax
LogicalMatrix localMax(NumericMatrix cwt2d);
RcppExport SEXP KPIC_localMax(SEXP cwt2dSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type cwt2d(cwt2dSEXP);
    rcpp_result_gen = Rcpp::wrap(localMax(cwt2d));
    return rcpp_result_gen;
END_RCPP
}
// localMin
LogicalMatrix localMin(NumericMatrix cwt2d);
RcppExport SEXP KPIC_localMin(SEXP cwt2dSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type cwt2d(cwt2dSEXP);
    rcpp_result_gen = Rcpp::wrap(localMin(cwt2d));
    return rcpp_result_gen;
END_RCPP
}
// ridgesDetection
List ridgesDetection(NumericMatrix cwt2d, NumericVector val);
RcppExport SEXP KPIC_ridgesDetection(SEXP cwt2dSEXP, SEXP valSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type cwt2d(cwt2dSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type val(valSEXP);
    rcpp_result_gen = Rcpp::wrap(ridgesDetection(cwt2d, val));
    return rcpp_result_gen;
END_RCPP
}
// peaksPosition
NumericVector peaksPosition(NumericVector val, List ridges, NumericMatrix cwt2d);
RcppExport SEXP KPIC_peaksPosition(SEXP valSEXP, SEXP ridgesSEXP, SEXP cwt2dSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type val(valSEXP);
    Rcpp::traits::input_parameter< List >::type ridges(ridgesSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type cwt2d(cwt2dSEXP);
    rcpp_result_gen = Rcpp::wrap(peaksPosition(val, ridges, cwt2d));
    return rcpp_result_gen;
END_RCPP
}
// getSignal
List getSignal(NumericMatrix cwt2d, List ridges, NumericVector peaks);
RcppExport SEXP KPIC_getSignal(SEXP cwt2dSEXP, SEXP ridgesSEXP, SEXP peaksSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type cwt2d(cwt2dSEXP);
    Rcpp::traits::input_parameter< List >::type ridges(ridgesSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type peaks(peaksSEXP);
    rcpp_result_gen = Rcpp::wrap(getSignal(cwt2d, ridges, peaks));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"KPIC_getPIP", (DL_FUNC) &KPIC_getPIP, 6},
    {"KPIC_getPIP_kmeans", (DL_FUNC) &KPIC_getPIP_kmeans, 10},
    {"KPIC_findCandidate", (DL_FUNC) &KPIC_findCandidate, 8},
    {"KPIC_waveft", (DL_FUNC) &KPIC_waveft, 2},
    {"KPIC_cwtft", (DL_FUNC) &KPIC_cwtft, 1},
    {"KPIC_localMax", (DL_FUNC) &KPIC_localMax, 1},
    {"KPIC_localMin", (DL_FUNC) &KPIC_localMin, 1},
    {"KPIC_ridgesDetection", (DL_FUNC) &KPIC_ridgesDetection, 2},
    {"KPIC_peaksPosition", (DL_FUNC) &KPIC_peaksPosition, 3},
    {"KPIC_getSignal", (DL_FUNC) &KPIC_getSignal, 3},
    {NULL, NULL, 0}
};

RcppExport void R_init_KPIC(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
