// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include "../inst/include/specialfunctions.h"
#include <Rcpp.h>
#include <string>
#include <set>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// ei_bh
double ei_bh(double u, double a);
static SEXP _specialfunctions_ei_bh_try(SEXP uSEXP, SEXP aSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< double >::type u(uSEXP);
    Rcpp::traits::input_parameter< double >::type a(aSEXP);
    rcpp_result_gen = Rcpp::wrap(ei_bh(u, a));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _specialfunctions_ei_bh(SEXP uSEXP, SEXP aSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_specialfunctions_ei_bh_try(uSEXP, aSEXP));
    }
    Rboolean rcpp_isInterrupt_gen = Rf_inherits(rcpp_result_gen, "interrupted-error");
    if (rcpp_isInterrupt_gen) {
        UNPROTECT(1);
        Rf_onintr();
    }
    bool rcpp_isLongjump_gen = Rcpp::internal::isLongjumpSentinel(rcpp_result_gen);
    if (rcpp_isLongjump_gen) {
        Rcpp::internal::resumeJump(rcpp_result_gen);
    }
    Rboolean rcpp_isError_gen = Rf_inherits(rcpp_result_gen, "try-error");
    if (rcpp_isError_gen) {
        SEXP rcpp_msgSEXP_gen = Rf_asChar(rcpp_result_gen);
        UNPROTECT(1);
        Rf_error("%s", CHAR(rcpp_msgSEXP_gen));
    }
    UNPROTECT(1);
    return rcpp_result_gen;
}
// bessel_k_bh
double bessel_k_bh(double x, int nu);
static SEXP _specialfunctions_bessel_k_bh_try(SEXP xSEXP, SEXP nuSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< double >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type nu(nuSEXP);
    rcpp_result_gen = Rcpp::wrap(bessel_k_bh(x, nu));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _specialfunctions_bessel_k_bh(SEXP xSEXP, SEXP nuSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_specialfunctions_bessel_k_bh_try(xSEXP, nuSEXP));
    }
    Rboolean rcpp_isInterrupt_gen = Rf_inherits(rcpp_result_gen, "interrupted-error");
    if (rcpp_isInterrupt_gen) {
        UNPROTECT(1);
        Rf_onintr();
    }
    bool rcpp_isLongjump_gen = Rcpp::internal::isLongjumpSentinel(rcpp_result_gen);
    if (rcpp_isLongjump_gen) {
        Rcpp::internal::resumeJump(rcpp_result_gen);
    }
    Rboolean rcpp_isError_gen = Rf_inherits(rcpp_result_gen, "try-error");
    if (rcpp_isError_gen) {
        SEXP rcpp_msgSEXP_gen = Rf_asChar(rcpp_result_gen);
        UNPROTECT(1);
        Rf_error("%s", CHAR(rcpp_msgSEXP_gen));
    }
    UNPROTECT(1);
    return rcpp_result_gen;
}
// factorial_bh
double factorial_bh(int x);
static SEXP _specialfunctions_factorial_bh_try(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< int >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(factorial_bh(x));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _specialfunctions_factorial_bh(SEXP xSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_specialfunctions_factorial_bh_try(xSEXP));
    }
    Rboolean rcpp_isInterrupt_gen = Rf_inherits(rcpp_result_gen, "interrupted-error");
    if (rcpp_isInterrupt_gen) {
        UNPROTECT(1);
        Rf_onintr();
    }
    bool rcpp_isLongjump_gen = Rcpp::internal::isLongjumpSentinel(rcpp_result_gen);
    if (rcpp_isLongjump_gen) {
        Rcpp::internal::resumeJump(rcpp_result_gen);
    }
    Rboolean rcpp_isError_gen = Rf_inherits(rcpp_result_gen, "try-error");
    if (rcpp_isError_gen) {
        SEXP rcpp_msgSEXP_gen = Rf_asChar(rcpp_result_gen);
        UNPROTECT(1);
        Rf_error("%s", CHAR(rcpp_msgSEXP_gen));
    }
    UNPROTECT(1);
    return rcpp_result_gen;
}
// bessel_k
Rcpp::NumericVector bessel_k(const Rcpp::NumericVector& x, double nu, bool expon_scaled);
static SEXP _specialfunctions_bessel_k_try(SEXP xSEXP, SEXP nuSEXP, SEXP expon_scaledSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< const Rcpp::NumericVector& >::type x(xSEXP);
    Rcpp::traits::input_parameter< double >::type nu(nuSEXP);
    Rcpp::traits::input_parameter< bool >::type expon_scaled(expon_scaledSEXP);
    rcpp_result_gen = Rcpp::wrap(bessel_k(x, nu, expon_scaled));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _specialfunctions_bessel_k(SEXP xSEXP, SEXP nuSEXP, SEXP expon_scaledSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_specialfunctions_bessel_k_try(xSEXP, nuSEXP, expon_scaledSEXP));
    }
    Rboolean rcpp_isInterrupt_gen = Rf_inherits(rcpp_result_gen, "interrupted-error");
    if (rcpp_isInterrupt_gen) {
        UNPROTECT(1);
        Rf_onintr();
    }
    bool rcpp_isLongjump_gen = Rcpp::internal::isLongjumpSentinel(rcpp_result_gen);
    if (rcpp_isLongjump_gen) {
        Rcpp::internal::resumeJump(rcpp_result_gen);
    }
    Rboolean rcpp_isError_gen = Rf_inherits(rcpp_result_gen, "try-error");
    if (rcpp_isError_gen) {
        SEXP rcpp_msgSEXP_gen = Rf_asChar(rcpp_result_gen);
        UNPROTECT(1);
        Rf_error("%s", CHAR(rcpp_msgSEXP_gen));
    }
    UNPROTECT(1);
    return rcpp_result_gen;
}
// bessel_i
Rcpp::NumericVector bessel_i(const Rcpp::NumericVector& x, double nu, bool expon_scaled);
static SEXP _specialfunctions_bessel_i_try(SEXP xSEXP, SEXP nuSEXP, SEXP expon_scaledSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< const Rcpp::NumericVector& >::type x(xSEXP);
    Rcpp::traits::input_parameter< double >::type nu(nuSEXP);
    Rcpp::traits::input_parameter< bool >::type expon_scaled(expon_scaledSEXP);
    rcpp_result_gen = Rcpp::wrap(bessel_i(x, nu, expon_scaled));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _specialfunctions_bessel_i(SEXP xSEXP, SEXP nuSEXP, SEXP expon_scaledSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_specialfunctions_bessel_i_try(xSEXP, nuSEXP, expon_scaledSEXP));
    }
    Rboolean rcpp_isInterrupt_gen = Rf_inherits(rcpp_result_gen, "interrupted-error");
    if (rcpp_isInterrupt_gen) {
        UNPROTECT(1);
        Rf_onintr();
    }
    bool rcpp_isLongjump_gen = Rcpp::internal::isLongjumpSentinel(rcpp_result_gen);
    if (rcpp_isLongjump_gen) {
        Rcpp::internal::resumeJump(rcpp_result_gen);
    }
    Rboolean rcpp_isError_gen = Rf_inherits(rcpp_result_gen, "try-error");
    if (rcpp_isError_gen) {
        SEXP rcpp_msgSEXP_gen = Rf_asChar(rcpp_result_gen);
        UNPROTECT(1);
        Rf_error("%s", CHAR(rcpp_msgSEXP_gen));
    }
    UNPROTECT(1);
    return rcpp_result_gen;
}
// bessel_j
Rcpp::NumericVector bessel_j(const Rcpp::NumericVector& x, double nu, bool expon_scaled);
static SEXP _specialfunctions_bessel_j_try(SEXP xSEXP, SEXP nuSEXP, SEXP expon_scaledSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< const Rcpp::NumericVector& >::type x(xSEXP);
    Rcpp::traits::input_parameter< double >::type nu(nuSEXP);
    Rcpp::traits::input_parameter< bool >::type expon_scaled(expon_scaledSEXP);
    rcpp_result_gen = Rcpp::wrap(bessel_j(x, nu, expon_scaled));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _specialfunctions_bessel_j(SEXP xSEXP, SEXP nuSEXP, SEXP expon_scaledSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_specialfunctions_bessel_j_try(xSEXP, nuSEXP, expon_scaledSEXP));
    }
    Rboolean rcpp_isInterrupt_gen = Rf_inherits(rcpp_result_gen, "interrupted-error");
    if (rcpp_isInterrupt_gen) {
        UNPROTECT(1);
        Rf_onintr();
    }
    bool rcpp_isLongjump_gen = Rcpp::internal::isLongjumpSentinel(rcpp_result_gen);
    if (rcpp_isLongjump_gen) {
        Rcpp::internal::resumeJump(rcpp_result_gen);
    }
    Rboolean rcpp_isError_gen = Rf_inherits(rcpp_result_gen, "try-error");
    if (rcpp_isError_gen) {
        SEXP rcpp_msgSEXP_gen = Rf_asChar(rcpp_result_gen);
        UNPROTECT(1);
        Rf_error("%s", CHAR(rcpp_msgSEXP_gen));
    }
    UNPROTECT(1);
    return rcpp_result_gen;
}
// bessel_y
Rcpp::NumericVector bessel_y(const Rcpp::NumericVector& x, double nu, bool expon_scaled);
static SEXP _specialfunctions_bessel_y_try(SEXP xSEXP, SEXP nuSEXP, SEXP expon_scaledSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< const Rcpp::NumericVector& >::type x(xSEXP);
    Rcpp::traits::input_parameter< double >::type nu(nuSEXP);
    Rcpp::traits::input_parameter< bool >::type expon_scaled(expon_scaledSEXP);
    rcpp_result_gen = Rcpp::wrap(bessel_y(x, nu, expon_scaled));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _specialfunctions_bessel_y(SEXP xSEXP, SEXP nuSEXP, SEXP expon_scaledSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_specialfunctions_bessel_y_try(xSEXP, nuSEXP, expon_scaledSEXP));
    }
    Rboolean rcpp_isInterrupt_gen = Rf_inherits(rcpp_result_gen, "interrupted-error");
    if (rcpp_isInterrupt_gen) {
        UNPROTECT(1);
        Rf_onintr();
    }
    bool rcpp_isLongjump_gen = Rcpp::internal::isLongjumpSentinel(rcpp_result_gen);
    if (rcpp_isLongjump_gen) {
        Rcpp::internal::resumeJump(rcpp_result_gen);
    }
    Rboolean rcpp_isError_gen = Rf_inherits(rcpp_result_gen, "try-error");
    if (rcpp_isError_gen) {
        SEXP rcpp_msgSEXP_gen = Rf_asChar(rcpp_result_gen);
        UNPROTECT(1);
        Rf_error("%s", CHAR(rcpp_msgSEXP_gen));
    }
    UNPROTECT(1);
    return rcpp_result_gen;
}
// bessel_k_cplx
Rcpp::ComplexMatrix bessel_k_cplx(const Rcpp::ComplexVector& x, double nu, bool expon_scaled, size_t nseq);
static SEXP _specialfunctions_bessel_k_cplx_try(SEXP xSEXP, SEXP nuSEXP, SEXP expon_scaledSEXP, SEXP nseqSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< const Rcpp::ComplexVector& >::type x(xSEXP);
    Rcpp::traits::input_parameter< double >::type nu(nuSEXP);
    Rcpp::traits::input_parameter< bool >::type expon_scaled(expon_scaledSEXP);
    Rcpp::traits::input_parameter< size_t >::type nseq(nseqSEXP);
    rcpp_result_gen = Rcpp::wrap(bessel_k_cplx(x, nu, expon_scaled, nseq));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _specialfunctions_bessel_k_cplx(SEXP xSEXP, SEXP nuSEXP, SEXP expon_scaledSEXP, SEXP nseqSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_specialfunctions_bessel_k_cplx_try(xSEXP, nuSEXP, expon_scaledSEXP, nseqSEXP));
    }
    Rboolean rcpp_isInterrupt_gen = Rf_inherits(rcpp_result_gen, "interrupted-error");
    if (rcpp_isInterrupt_gen) {
        UNPROTECT(1);
        Rf_onintr();
    }
    bool rcpp_isLongjump_gen = Rcpp::internal::isLongjumpSentinel(rcpp_result_gen);
    if (rcpp_isLongjump_gen) {
        Rcpp::internal::resumeJump(rcpp_result_gen);
    }
    Rboolean rcpp_isError_gen = Rf_inherits(rcpp_result_gen, "try-error");
    if (rcpp_isError_gen) {
        SEXP rcpp_msgSEXP_gen = Rf_asChar(rcpp_result_gen);
        UNPROTECT(1);
        Rf_error("%s", CHAR(rcpp_msgSEXP_gen));
    }
    UNPROTECT(1);
    return rcpp_result_gen;
}
// bessel_i_cplx
Rcpp::ComplexMatrix bessel_i_cplx(const Rcpp::ComplexVector& x, double nu, bool expon_scaled, size_t nseq);
static SEXP _specialfunctions_bessel_i_cplx_try(SEXP xSEXP, SEXP nuSEXP, SEXP expon_scaledSEXP, SEXP nseqSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< const Rcpp::ComplexVector& >::type x(xSEXP);
    Rcpp::traits::input_parameter< double >::type nu(nuSEXP);
    Rcpp::traits::input_parameter< bool >::type expon_scaled(expon_scaledSEXP);
    Rcpp::traits::input_parameter< size_t >::type nseq(nseqSEXP);
    rcpp_result_gen = Rcpp::wrap(bessel_i_cplx(x, nu, expon_scaled, nseq));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _specialfunctions_bessel_i_cplx(SEXP xSEXP, SEXP nuSEXP, SEXP expon_scaledSEXP, SEXP nseqSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_specialfunctions_bessel_i_cplx_try(xSEXP, nuSEXP, expon_scaledSEXP, nseqSEXP));
    }
    Rboolean rcpp_isInterrupt_gen = Rf_inherits(rcpp_result_gen, "interrupted-error");
    if (rcpp_isInterrupt_gen) {
        UNPROTECT(1);
        Rf_onintr();
    }
    bool rcpp_isLongjump_gen = Rcpp::internal::isLongjumpSentinel(rcpp_result_gen);
    if (rcpp_isLongjump_gen) {
        Rcpp::internal::resumeJump(rcpp_result_gen);
    }
    Rboolean rcpp_isError_gen = Rf_inherits(rcpp_result_gen, "try-error");
    if (rcpp_isError_gen) {
        SEXP rcpp_msgSEXP_gen = Rf_asChar(rcpp_result_gen);
        UNPROTECT(1);
        Rf_error("%s", CHAR(rcpp_msgSEXP_gen));
    }
    UNPROTECT(1);
    return rcpp_result_gen;
}
// bessel_j_cplx
Rcpp::ComplexMatrix bessel_j_cplx(const Rcpp::ComplexVector& x, double nu, bool expon_scaled, size_t nseq);
static SEXP _specialfunctions_bessel_j_cplx_try(SEXP xSEXP, SEXP nuSEXP, SEXP expon_scaledSEXP, SEXP nseqSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< const Rcpp::ComplexVector& >::type x(xSEXP);
    Rcpp::traits::input_parameter< double >::type nu(nuSEXP);
    Rcpp::traits::input_parameter< bool >::type expon_scaled(expon_scaledSEXP);
    Rcpp::traits::input_parameter< size_t >::type nseq(nseqSEXP);
    rcpp_result_gen = Rcpp::wrap(bessel_j_cplx(x, nu, expon_scaled, nseq));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _specialfunctions_bessel_j_cplx(SEXP xSEXP, SEXP nuSEXP, SEXP expon_scaledSEXP, SEXP nseqSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_specialfunctions_bessel_j_cplx_try(xSEXP, nuSEXP, expon_scaledSEXP, nseqSEXP));
    }
    Rboolean rcpp_isInterrupt_gen = Rf_inherits(rcpp_result_gen, "interrupted-error");
    if (rcpp_isInterrupt_gen) {
        UNPROTECT(1);
        Rf_onintr();
    }
    bool rcpp_isLongjump_gen = Rcpp::internal::isLongjumpSentinel(rcpp_result_gen);
    if (rcpp_isLongjump_gen) {
        Rcpp::internal::resumeJump(rcpp_result_gen);
    }
    Rboolean rcpp_isError_gen = Rf_inherits(rcpp_result_gen, "try-error");
    if (rcpp_isError_gen) {
        SEXP rcpp_msgSEXP_gen = Rf_asChar(rcpp_result_gen);
        UNPROTECT(1);
        Rf_error("%s", CHAR(rcpp_msgSEXP_gen));
    }
    UNPROTECT(1);
    return rcpp_result_gen;
}
// bessel_y_cplx
Rcpp::ComplexMatrix bessel_y_cplx(const Rcpp::ComplexVector& x, double nu, bool expon_scaled, size_t nseq);
static SEXP _specialfunctions_bessel_y_cplx_try(SEXP xSEXP, SEXP nuSEXP, SEXP expon_scaledSEXP, SEXP nseqSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< const Rcpp::ComplexVector& >::type x(xSEXP);
    Rcpp::traits::input_parameter< double >::type nu(nuSEXP);
    Rcpp::traits::input_parameter< bool >::type expon_scaled(expon_scaledSEXP);
    Rcpp::traits::input_parameter< size_t >::type nseq(nseqSEXP);
    rcpp_result_gen = Rcpp::wrap(bessel_y_cplx(x, nu, expon_scaled, nseq));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _specialfunctions_bessel_y_cplx(SEXP xSEXP, SEXP nuSEXP, SEXP expon_scaledSEXP, SEXP nseqSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_specialfunctions_bessel_y_cplx_try(xSEXP, nuSEXP, expon_scaledSEXP, nseqSEXP));
    }
    Rboolean rcpp_isInterrupt_gen = Rf_inherits(rcpp_result_gen, "interrupted-error");
    if (rcpp_isInterrupt_gen) {
        UNPROTECT(1);
        Rf_onintr();
    }
    bool rcpp_isLongjump_gen = Rcpp::internal::isLongjumpSentinel(rcpp_result_gen);
    if (rcpp_isLongjump_gen) {
        Rcpp::internal::resumeJump(rcpp_result_gen);
    }
    Rboolean rcpp_isError_gen = Rf_inherits(rcpp_result_gen, "try-error");
    if (rcpp_isError_gen) {
        SEXP rcpp_msgSEXP_gen = Rf_asChar(rcpp_result_gen);
        UNPROTECT(1);
        Rf_error("%s", CHAR(rcpp_msgSEXP_gen));
    }
    UNPROTECT(1);
    return rcpp_result_gen;
}
// erf_vec
std::vector<double> erf_vec(std::vector<double> x);
static SEXP _specialfunctions_erf_vec_try(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< std::vector<double> >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(erf_vec(x));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _specialfunctions_erf_vec(SEXP xSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_specialfunctions_erf_vec_try(xSEXP));
    }
    Rboolean rcpp_isInterrupt_gen = Rf_inherits(rcpp_result_gen, "interrupted-error");
    if (rcpp_isInterrupt_gen) {
        UNPROTECT(1);
        Rf_onintr();
    }
    bool rcpp_isLongjump_gen = Rcpp::internal::isLongjumpSentinel(rcpp_result_gen);
    if (rcpp_isLongjump_gen) {
        Rcpp::internal::resumeJump(rcpp_result_gen);
    }
    Rboolean rcpp_isError_gen = Rf_inherits(rcpp_result_gen, "try-error");
    if (rcpp_isError_gen) {
        SEXP rcpp_msgSEXP_gen = Rf_asChar(rcpp_result_gen);
        UNPROTECT(1);
        Rf_error("%s", CHAR(rcpp_msgSEXP_gen));
    }
    UNPROTECT(1);
    return rcpp_result_gen;
}
// erfc_vec
std::vector<double> erfc_vec(std::vector<double> x);
static SEXP _specialfunctions_erfc_vec_try(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< std::vector<double> >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(erfc_vec(x));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _specialfunctions_erfc_vec(SEXP xSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_specialfunctions_erfc_vec_try(xSEXP));
    }
    Rboolean rcpp_isInterrupt_gen = Rf_inherits(rcpp_result_gen, "interrupted-error");
    if (rcpp_isInterrupt_gen) {
        UNPROTECT(1);
        Rf_onintr();
    }
    bool rcpp_isLongjump_gen = Rcpp::internal::isLongjumpSentinel(rcpp_result_gen);
    if (rcpp_isLongjump_gen) {
        Rcpp::internal::resumeJump(rcpp_result_gen);
    }
    Rboolean rcpp_isError_gen = Rf_inherits(rcpp_result_gen, "try-error");
    if (rcpp_isError_gen) {
        SEXP rcpp_msgSEXP_gen = Rf_asChar(rcpp_result_gen);
        UNPROTECT(1);
        Rf_error("%s", CHAR(rcpp_msgSEXP_gen));
    }
    UNPROTECT(1);
    return rcpp_result_gen;
}
// gamma_inc_single
double gamma_inc_single(double u, double a);
static SEXP _specialfunctions_gamma_inc_single_try(SEXP uSEXP, SEXP aSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< double >::type u(uSEXP);
    Rcpp::traits::input_parameter< double >::type a(aSEXP);
    rcpp_result_gen = Rcpp::wrap(gamma_inc_single(u, a));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _specialfunctions_gamma_inc_single(SEXP uSEXP, SEXP aSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_specialfunctions_gamma_inc_single_try(uSEXP, aSEXP));
    }
    Rboolean rcpp_isInterrupt_gen = Rf_inherits(rcpp_result_gen, "interrupted-error");
    if (rcpp_isInterrupt_gen) {
        UNPROTECT(1);
        Rf_onintr();
    }
    bool rcpp_isLongjump_gen = Rcpp::internal::isLongjumpSentinel(rcpp_result_gen);
    if (rcpp_isLongjump_gen) {
        Rcpp::internal::resumeJump(rcpp_result_gen);
    }
    Rboolean rcpp_isError_gen = Rf_inherits(rcpp_result_gen, "try-error");
    if (rcpp_isError_gen) {
        SEXP rcpp_msgSEXP_gen = Rf_asChar(rcpp_result_gen);
        UNPROTECT(1);
        Rf_error("%s", CHAR(rcpp_msgSEXP_gen));
    }
    UNPROTECT(1);
    return rcpp_result_gen;
}
// gamma_inc_vec
std::vector<double> gamma_inc_vec(std::vector<double> u, double a);
static SEXP _specialfunctions_gamma_inc_vec_try(SEXP uSEXP, SEXP aSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< std::vector<double> >::type u(uSEXP);
    Rcpp::traits::input_parameter< double >::type a(aSEXP);
    rcpp_result_gen = Rcpp::wrap(gamma_inc_vec(u, a));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _specialfunctions_gamma_inc_vec(SEXP uSEXP, SEXP aSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_specialfunctions_gamma_inc_vec_try(uSEXP, aSEXP));
    }
    Rboolean rcpp_isInterrupt_gen = Rf_inherits(rcpp_result_gen, "interrupted-error");
    if (rcpp_isInterrupt_gen) {
        UNPROTECT(1);
        Rf_onintr();
    }
    bool rcpp_isLongjump_gen = Rcpp::internal::isLongjumpSentinel(rcpp_result_gen);
    if (rcpp_isLongjump_gen) {
        Rcpp::internal::resumeJump(rcpp_result_gen);
    }
    Rboolean rcpp_isError_gen = Rf_inherits(rcpp_result_gen, "try-error");
    if (rcpp_isError_gen) {
        SEXP rcpp_msgSEXP_gen = Rf_asChar(rcpp_result_gen);
        UNPROTECT(1);
        Rf_error("%s", CHAR(rcpp_msgSEXP_gen));
    }
    UNPROTECT(1);
    return rcpp_result_gen;
}
// gamma_inc_rcpp
Rcpp::NumericVector gamma_inc_rcpp(Rcpp::NumericVector u, double a);
static SEXP _specialfunctions_gamma_inc_rcpp_try(SEXP uSEXP, SEXP aSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type u(uSEXP);
    Rcpp::traits::input_parameter< double >::type a(aSEXP);
    rcpp_result_gen = Rcpp::wrap(gamma_inc_rcpp(u, a));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _specialfunctions_gamma_inc_rcpp(SEXP uSEXP, SEXP aSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_specialfunctions_gamma_inc_rcpp_try(uSEXP, aSEXP));
    }
    Rboolean rcpp_isInterrupt_gen = Rf_inherits(rcpp_result_gen, "interrupted-error");
    if (rcpp_isInterrupt_gen) {
        UNPROTECT(1);
        Rf_onintr();
    }
    bool rcpp_isLongjump_gen = Rcpp::internal::isLongjumpSentinel(rcpp_result_gen);
    if (rcpp_isLongjump_gen) {
        Rcpp::internal::resumeJump(rcpp_result_gen);
    }
    Rboolean rcpp_isError_gen = Rf_inherits(rcpp_result_gen, "try-error");
    if (rcpp_isError_gen) {
        SEXP rcpp_msgSEXP_gen = Rf_asChar(rcpp_result_gen);
        UNPROTECT(1);
        Rf_error("%s", CHAR(rcpp_msgSEXP_gen));
    }
    UNPROTECT(1);
    return rcpp_result_gen;
}
// ei_vec
std::vector<double> ei_vec(std::vector<double> u);
static SEXP _specialfunctions_ei_vec_try(SEXP uSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< std::vector<double> >::type u(uSEXP);
    rcpp_result_gen = Rcpp::wrap(ei_vec(u));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _specialfunctions_ei_vec(SEXP uSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_specialfunctions_ei_vec_try(uSEXP));
    }
    Rboolean rcpp_isInterrupt_gen = Rf_inherits(rcpp_result_gen, "interrupted-error");
    if (rcpp_isInterrupt_gen) {
        UNPROTECT(1);
        Rf_onintr();
    }
    bool rcpp_isLongjump_gen = Rcpp::internal::isLongjumpSentinel(rcpp_result_gen);
    if (rcpp_isLongjump_gen) {
        Rcpp::internal::resumeJump(rcpp_result_gen);
    }
    Rboolean rcpp_isError_gen = Rf_inherits(rcpp_result_gen, "try-error");
    if (rcpp_isError_gen) {
        SEXP rcpp_msgSEXP_gen = Rf_asChar(rcpp_result_gen);
        UNPROTECT(1);
        Rf_error("%s", CHAR(rcpp_msgSEXP_gen));
    }
    UNPROTECT(1);
    return rcpp_result_gen;
}
// ei_one
double ei_one(double u);
static SEXP _specialfunctions_ei_one_try(SEXP uSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< double >::type u(uSEXP);
    rcpp_result_gen = Rcpp::wrap(ei_one(u));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _specialfunctions_ei_one(SEXP uSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_specialfunctions_ei_one_try(uSEXP));
    }
    Rboolean rcpp_isInterrupt_gen = Rf_inherits(rcpp_result_gen, "interrupted-error");
    if (rcpp_isInterrupt_gen) {
        UNPROTECT(1);
        Rf_onintr();
    }
    bool rcpp_isLongjump_gen = Rcpp::internal::isLongjumpSentinel(rcpp_result_gen);
    if (rcpp_isLongjump_gen) {
        Rcpp::internal::resumeJump(rcpp_result_gen);
    }
    Rboolean rcpp_isError_gen = Rf_inherits(rcpp_result_gen, "try-error");
    if (rcpp_isError_gen) {
        SEXP rcpp_msgSEXP_gen = Rf_asChar(rcpp_result_gen);
        UNPROTECT(1);
        Rf_error("%s", CHAR(rcpp_msgSEXP_gen));
    }
    UNPROTECT(1);
    return rcpp_result_gen;
}
// factorial_one
double factorial_one(double u);
static SEXP _specialfunctions_factorial_one_try(SEXP uSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< double >::type u(uSEXP);
    rcpp_result_gen = Rcpp::wrap(factorial_one(u));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _specialfunctions_factorial_one(SEXP uSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_specialfunctions_factorial_one_try(uSEXP));
    }
    Rboolean rcpp_isInterrupt_gen = Rf_inherits(rcpp_result_gen, "interrupted-error");
    if (rcpp_isInterrupt_gen) {
        UNPROTECT(1);
        Rf_onintr();
    }
    bool rcpp_isLongjump_gen = Rcpp::internal::isLongjumpSentinel(rcpp_result_gen);
    if (rcpp_isLongjump_gen) {
        Rcpp::internal::resumeJump(rcpp_result_gen);
    }
    Rboolean rcpp_isError_gen = Rf_inherits(rcpp_result_gen, "try-error");
    if (rcpp_isError_gen) {
        SEXP rcpp_msgSEXP_gen = Rf_asChar(rcpp_result_gen);
        UNPROTECT(1);
        Rf_error("%s", CHAR(rcpp_msgSEXP_gen));
    }
    UNPROTECT(1);
    return rcpp_result_gen;
}

// validate (ensure exported C++ functions exist before calling them)
static int _specialfunctions_RcppExport_validate(const char* sig) { 
    static std::set<std::string> signatures;
    if (signatures.empty()) {
        signatures.insert("double(*ei_bh)(double,double)");
        signatures.insert("double(*bessel_k_bh)(double,int)");
        signatures.insert("double(*factorial_bh)(int)");
        signatures.insert("Rcpp::NumericVector(*bessel_k)(const Rcpp::NumericVector&,double,bool)");
        signatures.insert("Rcpp::NumericVector(*bessel_i)(const Rcpp::NumericVector&,double,bool)");
        signatures.insert("Rcpp::NumericVector(*bessel_j)(const Rcpp::NumericVector&,double,bool)");
        signatures.insert("Rcpp::NumericVector(*bessel_y)(const Rcpp::NumericVector&,double,bool)");
        signatures.insert("Rcpp::ComplexMatrix(*bessel_k_cplx)(const Rcpp::ComplexVector&,double,bool,size_t)");
        signatures.insert("Rcpp::ComplexMatrix(*bessel_i_cplx)(const Rcpp::ComplexVector&,double,bool,size_t)");
        signatures.insert("Rcpp::ComplexMatrix(*bessel_j_cplx)(const Rcpp::ComplexVector&,double,bool,size_t)");
        signatures.insert("Rcpp::ComplexMatrix(*bessel_y_cplx)(const Rcpp::ComplexVector&,double,bool,size_t)");
        signatures.insert("std::vector<double>(*erf_vec)(std::vector<double>)");
        signatures.insert("std::vector<double>(*erfc_vec)(std::vector<double>)");
        signatures.insert("double(*gamma_inc_single)(double,double)");
        signatures.insert("std::vector<double>(*gamma_inc_vec)(std::vector<double>,double)");
        signatures.insert("Rcpp::NumericVector(*gamma_inc_rcpp)(Rcpp::NumericVector,double)");
        signatures.insert("std::vector<double>(*ei_vec)(std::vector<double>)");
        signatures.insert("double(*ei_one)(double)");
        signatures.insert("double(*factorial_one)(double)");
    }
    return signatures.find(sig) != signatures.end();
}

// registerCCallable (register entry points for exported C++ functions)
RcppExport SEXP _specialfunctions_RcppExport_registerCCallable() { 
    R_RegisterCCallable("specialfunctions", "_specialfunctions_ei_bh", (DL_FUNC)_specialfunctions_ei_bh_try);
    R_RegisterCCallable("specialfunctions", "_specialfunctions_bessel_k_bh", (DL_FUNC)_specialfunctions_bessel_k_bh_try);
    R_RegisterCCallable("specialfunctions", "_specialfunctions_factorial_bh", (DL_FUNC)_specialfunctions_factorial_bh_try);
    R_RegisterCCallable("specialfunctions", "_specialfunctions_bessel_k", (DL_FUNC)_specialfunctions_bessel_k_try);
    R_RegisterCCallable("specialfunctions", "_specialfunctions_bessel_i", (DL_FUNC)_specialfunctions_bessel_i_try);
    R_RegisterCCallable("specialfunctions", "_specialfunctions_bessel_j", (DL_FUNC)_specialfunctions_bessel_j_try);
    R_RegisterCCallable("specialfunctions", "_specialfunctions_bessel_y", (DL_FUNC)_specialfunctions_bessel_y_try);
    R_RegisterCCallable("specialfunctions", "_specialfunctions_bessel_k_cplx", (DL_FUNC)_specialfunctions_bessel_k_cplx_try);
    R_RegisterCCallable("specialfunctions", "_specialfunctions_bessel_i_cplx", (DL_FUNC)_specialfunctions_bessel_i_cplx_try);
    R_RegisterCCallable("specialfunctions", "_specialfunctions_bessel_j_cplx", (DL_FUNC)_specialfunctions_bessel_j_cplx_try);
    R_RegisterCCallable("specialfunctions", "_specialfunctions_bessel_y_cplx", (DL_FUNC)_specialfunctions_bessel_y_cplx_try);
    R_RegisterCCallable("specialfunctions", "_specialfunctions_erf_vec", (DL_FUNC)_specialfunctions_erf_vec_try);
    R_RegisterCCallable("specialfunctions", "_specialfunctions_erfc_vec", (DL_FUNC)_specialfunctions_erfc_vec_try);
    R_RegisterCCallable("specialfunctions", "_specialfunctions_gamma_inc_single", (DL_FUNC)_specialfunctions_gamma_inc_single_try);
    R_RegisterCCallable("specialfunctions", "_specialfunctions_gamma_inc_vec", (DL_FUNC)_specialfunctions_gamma_inc_vec_try);
    R_RegisterCCallable("specialfunctions", "_specialfunctions_gamma_inc_rcpp", (DL_FUNC)_specialfunctions_gamma_inc_rcpp_try);
    R_RegisterCCallable("specialfunctions", "_specialfunctions_ei_vec", (DL_FUNC)_specialfunctions_ei_vec_try);
    R_RegisterCCallable("specialfunctions", "_specialfunctions_ei_one", (DL_FUNC)_specialfunctions_ei_one_try);
    R_RegisterCCallable("specialfunctions", "_specialfunctions_factorial_one", (DL_FUNC)_specialfunctions_factorial_one_try);
    R_RegisterCCallable("specialfunctions", "_specialfunctions_RcppExport_validate", (DL_FUNC)_specialfunctions_RcppExport_validate);
    return R_NilValue;
}

static const R_CallMethodDef CallEntries[] = {
    {"_specialfunctions_ei_bh", (DL_FUNC) &_specialfunctions_ei_bh, 2},
    {"_specialfunctions_bessel_k_bh", (DL_FUNC) &_specialfunctions_bessel_k_bh, 2},
    {"_specialfunctions_factorial_bh", (DL_FUNC) &_specialfunctions_factorial_bh, 1},
    {"_specialfunctions_bessel_k", (DL_FUNC) &_specialfunctions_bessel_k, 3},
    {"_specialfunctions_bessel_i", (DL_FUNC) &_specialfunctions_bessel_i, 3},
    {"_specialfunctions_bessel_j", (DL_FUNC) &_specialfunctions_bessel_j, 3},
    {"_specialfunctions_bessel_y", (DL_FUNC) &_specialfunctions_bessel_y, 3},
    {"_specialfunctions_bessel_k_cplx", (DL_FUNC) &_specialfunctions_bessel_k_cplx, 4},
    {"_specialfunctions_bessel_i_cplx", (DL_FUNC) &_specialfunctions_bessel_i_cplx, 4},
    {"_specialfunctions_bessel_j_cplx", (DL_FUNC) &_specialfunctions_bessel_j_cplx, 4},
    {"_specialfunctions_bessel_y_cplx", (DL_FUNC) &_specialfunctions_bessel_y_cplx, 4},
    {"_specialfunctions_erf_vec", (DL_FUNC) &_specialfunctions_erf_vec, 1},
    {"_specialfunctions_erfc_vec", (DL_FUNC) &_specialfunctions_erfc_vec, 1},
    {"_specialfunctions_gamma_inc_single", (DL_FUNC) &_specialfunctions_gamma_inc_single, 2},
    {"_specialfunctions_gamma_inc_vec", (DL_FUNC) &_specialfunctions_gamma_inc_vec, 2},
    {"_specialfunctions_gamma_inc_rcpp", (DL_FUNC) &_specialfunctions_gamma_inc_rcpp, 2},
    {"_specialfunctions_ei_vec", (DL_FUNC) &_specialfunctions_ei_vec, 1},
    {"_specialfunctions_ei_one", (DL_FUNC) &_specialfunctions_ei_one, 1},
    {"_specialfunctions_factorial_one", (DL_FUNC) &_specialfunctions_factorial_one, 1},
    {"_specialfunctions_RcppExport_registerCCallable", (DL_FUNC) &_specialfunctions_RcppExport_registerCCallable, 0},
    {NULL, NULL, 0}
};

RcppExport void R_init_specialfunctions(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
