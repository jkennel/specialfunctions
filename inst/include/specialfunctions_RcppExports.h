// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#ifndef RCPP_specialfunctions_RCPPEXPORTS_H_GEN_
#define RCPP_specialfunctions_RCPPEXPORTS_H_GEN_

#include <Rcpp.h>

namespace specialfunctions {

    using namespace Rcpp;

    namespace {
        void validateSignature(const char* sig) {
            Rcpp::Function require = Rcpp::Environment::base_env()["require"];
            require("specialfunctions", Rcpp::Named("quietly") = true);
            typedef int(*Ptr_validate)(const char*);
            static Ptr_validate p_validate = (Ptr_validate)
                R_GetCCallable("specialfunctions", "_specialfunctions_RcppExport_validate");
            if (!p_validate(sig)) {
                throw Rcpp::function_not_exported(
                    "C++ function with signature '" + std::string(sig) + "' not found in specialfunctions");
            }
        }
    }

    inline double ei_bh(double u, double a) {
        typedef SEXP(*Ptr_ei_bh)(SEXP,SEXP);
        static Ptr_ei_bh p_ei_bh = NULL;
        if (p_ei_bh == NULL) {
            validateSignature("double(*ei_bh)(double,double)");
            p_ei_bh = (Ptr_ei_bh)R_GetCCallable("specialfunctions", "_specialfunctions_ei_bh");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_ei_bh(Shield<SEXP>(Rcpp::wrap(u)), Shield<SEXP>(Rcpp::wrap(a)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<double >(rcpp_result_gen);
    }

    inline double bessel_k_bh(double x, int nu) {
        typedef SEXP(*Ptr_bessel_k_bh)(SEXP,SEXP);
        static Ptr_bessel_k_bh p_bessel_k_bh = NULL;
        if (p_bessel_k_bh == NULL) {
            validateSignature("double(*bessel_k_bh)(double,int)");
            p_bessel_k_bh = (Ptr_bessel_k_bh)R_GetCCallable("specialfunctions", "_specialfunctions_bessel_k_bh");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_bessel_k_bh(Shield<SEXP>(Rcpp::wrap(x)), Shield<SEXP>(Rcpp::wrap(nu)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<double >(rcpp_result_gen);
    }

    inline double factorial_bh(int x) {
        typedef SEXP(*Ptr_factorial_bh)(SEXP);
        static Ptr_factorial_bh p_factorial_bh = NULL;
        if (p_factorial_bh == NULL) {
            validateSignature("double(*factorial_bh)(int)");
            p_factorial_bh = (Ptr_factorial_bh)R_GetCCallable("specialfunctions", "_specialfunctions_factorial_bh");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_factorial_bh(Shield<SEXP>(Rcpp::wrap(x)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<double >(rcpp_result_gen);
    }

    inline Rcpp::NumericVector bessel_k(const Rcpp::NumericVector& x, double nu, bool expon_scaled) {
        typedef SEXP(*Ptr_bessel_k)(SEXP,SEXP,SEXP);
        static Ptr_bessel_k p_bessel_k = NULL;
        if (p_bessel_k == NULL) {
            validateSignature("Rcpp::NumericVector(*bessel_k)(const Rcpp::NumericVector&,double,bool)");
            p_bessel_k = (Ptr_bessel_k)R_GetCCallable("specialfunctions", "_specialfunctions_bessel_k");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_bessel_k(Shield<SEXP>(Rcpp::wrap(x)), Shield<SEXP>(Rcpp::wrap(nu)), Shield<SEXP>(Rcpp::wrap(expon_scaled)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<Rcpp::NumericVector >(rcpp_result_gen);
    }

    inline Rcpp::NumericVector bessel_i(const Rcpp::NumericVector& x, double nu, bool expon_scaled) {
        typedef SEXP(*Ptr_bessel_i)(SEXP,SEXP,SEXP);
        static Ptr_bessel_i p_bessel_i = NULL;
        if (p_bessel_i == NULL) {
            validateSignature("Rcpp::NumericVector(*bessel_i)(const Rcpp::NumericVector&,double,bool)");
            p_bessel_i = (Ptr_bessel_i)R_GetCCallable("specialfunctions", "_specialfunctions_bessel_i");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_bessel_i(Shield<SEXP>(Rcpp::wrap(x)), Shield<SEXP>(Rcpp::wrap(nu)), Shield<SEXP>(Rcpp::wrap(expon_scaled)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<Rcpp::NumericVector >(rcpp_result_gen);
    }

    inline Rcpp::NumericVector bessel_j(const Rcpp::NumericVector& x, double nu, bool expon_scaled) {
        typedef SEXP(*Ptr_bessel_j)(SEXP,SEXP,SEXP);
        static Ptr_bessel_j p_bessel_j = NULL;
        if (p_bessel_j == NULL) {
            validateSignature("Rcpp::NumericVector(*bessel_j)(const Rcpp::NumericVector&,double,bool)");
            p_bessel_j = (Ptr_bessel_j)R_GetCCallable("specialfunctions", "_specialfunctions_bessel_j");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_bessel_j(Shield<SEXP>(Rcpp::wrap(x)), Shield<SEXP>(Rcpp::wrap(nu)), Shield<SEXP>(Rcpp::wrap(expon_scaled)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<Rcpp::NumericVector >(rcpp_result_gen);
    }

    inline Rcpp::NumericVector bessel_y(const Rcpp::NumericVector& x, double nu, bool expon_scaled) {
        typedef SEXP(*Ptr_bessel_y)(SEXP,SEXP,SEXP);
        static Ptr_bessel_y p_bessel_y = NULL;
        if (p_bessel_y == NULL) {
            validateSignature("Rcpp::NumericVector(*bessel_y)(const Rcpp::NumericVector&,double,bool)");
            p_bessel_y = (Ptr_bessel_y)R_GetCCallable("specialfunctions", "_specialfunctions_bessel_y");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_bessel_y(Shield<SEXP>(Rcpp::wrap(x)), Shield<SEXP>(Rcpp::wrap(nu)), Shield<SEXP>(Rcpp::wrap(expon_scaled)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<Rcpp::NumericVector >(rcpp_result_gen);
    }

    inline Rcpp::ComplexMatrix bessel_k_cplx(const Rcpp::ComplexVector& x, double nu, bool expon_scaled, size_t nseq) {
        typedef SEXP(*Ptr_bessel_k_cplx)(SEXP,SEXP,SEXP,SEXP);
        static Ptr_bessel_k_cplx p_bessel_k_cplx = NULL;
        if (p_bessel_k_cplx == NULL) {
            validateSignature("Rcpp::ComplexMatrix(*bessel_k_cplx)(const Rcpp::ComplexVector&,double,bool,size_t)");
            p_bessel_k_cplx = (Ptr_bessel_k_cplx)R_GetCCallable("specialfunctions", "_specialfunctions_bessel_k_cplx");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_bessel_k_cplx(Shield<SEXP>(Rcpp::wrap(x)), Shield<SEXP>(Rcpp::wrap(nu)), Shield<SEXP>(Rcpp::wrap(expon_scaled)), Shield<SEXP>(Rcpp::wrap(nseq)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<Rcpp::ComplexMatrix >(rcpp_result_gen);
    }

    inline Rcpp::ComplexMatrix bessel_i_cplx(const Rcpp::ComplexVector& x, double nu, bool expon_scaled, size_t nseq) {
        typedef SEXP(*Ptr_bessel_i_cplx)(SEXP,SEXP,SEXP,SEXP);
        static Ptr_bessel_i_cplx p_bessel_i_cplx = NULL;
        if (p_bessel_i_cplx == NULL) {
            validateSignature("Rcpp::ComplexMatrix(*bessel_i_cplx)(const Rcpp::ComplexVector&,double,bool,size_t)");
            p_bessel_i_cplx = (Ptr_bessel_i_cplx)R_GetCCallable("specialfunctions", "_specialfunctions_bessel_i_cplx");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_bessel_i_cplx(Shield<SEXP>(Rcpp::wrap(x)), Shield<SEXP>(Rcpp::wrap(nu)), Shield<SEXP>(Rcpp::wrap(expon_scaled)), Shield<SEXP>(Rcpp::wrap(nseq)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<Rcpp::ComplexMatrix >(rcpp_result_gen);
    }

    inline Rcpp::ComplexMatrix bessel_j_cplx(const Rcpp::ComplexVector& x, double nu, bool expon_scaled, size_t nseq) {
        typedef SEXP(*Ptr_bessel_j_cplx)(SEXP,SEXP,SEXP,SEXP);
        static Ptr_bessel_j_cplx p_bessel_j_cplx = NULL;
        if (p_bessel_j_cplx == NULL) {
            validateSignature("Rcpp::ComplexMatrix(*bessel_j_cplx)(const Rcpp::ComplexVector&,double,bool,size_t)");
            p_bessel_j_cplx = (Ptr_bessel_j_cplx)R_GetCCallable("specialfunctions", "_specialfunctions_bessel_j_cplx");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_bessel_j_cplx(Shield<SEXP>(Rcpp::wrap(x)), Shield<SEXP>(Rcpp::wrap(nu)), Shield<SEXP>(Rcpp::wrap(expon_scaled)), Shield<SEXP>(Rcpp::wrap(nseq)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<Rcpp::ComplexMatrix >(rcpp_result_gen);
    }

    inline Rcpp::ComplexMatrix bessel_y_cplx(const Rcpp::ComplexVector& x, double nu, bool expon_scaled, size_t nseq) {
        typedef SEXP(*Ptr_bessel_y_cplx)(SEXP,SEXP,SEXP,SEXP);
        static Ptr_bessel_y_cplx p_bessel_y_cplx = NULL;
        if (p_bessel_y_cplx == NULL) {
            validateSignature("Rcpp::ComplexMatrix(*bessel_y_cplx)(const Rcpp::ComplexVector&,double,bool,size_t)");
            p_bessel_y_cplx = (Ptr_bessel_y_cplx)R_GetCCallable("specialfunctions", "_specialfunctions_bessel_y_cplx");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_bessel_y_cplx(Shield<SEXP>(Rcpp::wrap(x)), Shield<SEXP>(Rcpp::wrap(nu)), Shield<SEXP>(Rcpp::wrap(expon_scaled)), Shield<SEXP>(Rcpp::wrap(nseq)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<Rcpp::ComplexMatrix >(rcpp_result_gen);
    }

    inline std::vector<double> erf_vec(std::vector<double> x) {
        typedef SEXP(*Ptr_erf_vec)(SEXP);
        static Ptr_erf_vec p_erf_vec = NULL;
        if (p_erf_vec == NULL) {
            validateSignature("std::vector<double>(*erf_vec)(std::vector<double>)");
            p_erf_vec = (Ptr_erf_vec)R_GetCCallable("specialfunctions", "_specialfunctions_erf_vec");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_erf_vec(Shield<SEXP>(Rcpp::wrap(x)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<std::vector<double> >(rcpp_result_gen);
    }

    inline std::vector<double> erfc_vec(std::vector<double> x) {
        typedef SEXP(*Ptr_erfc_vec)(SEXP);
        static Ptr_erfc_vec p_erfc_vec = NULL;
        if (p_erfc_vec == NULL) {
            validateSignature("std::vector<double>(*erfc_vec)(std::vector<double>)");
            p_erfc_vec = (Ptr_erfc_vec)R_GetCCallable("specialfunctions", "_specialfunctions_erfc_vec");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_erfc_vec(Shield<SEXP>(Rcpp::wrap(x)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<std::vector<double> >(rcpp_result_gen);
    }

    inline double gamma_inc_single(double u, double a) {
        typedef SEXP(*Ptr_gamma_inc_single)(SEXP,SEXP);
        static Ptr_gamma_inc_single p_gamma_inc_single = NULL;
        if (p_gamma_inc_single == NULL) {
            validateSignature("double(*gamma_inc_single)(double,double)");
            p_gamma_inc_single = (Ptr_gamma_inc_single)R_GetCCallable("specialfunctions", "_specialfunctions_gamma_inc_single");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_gamma_inc_single(Shield<SEXP>(Rcpp::wrap(u)), Shield<SEXP>(Rcpp::wrap(a)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<double >(rcpp_result_gen);
    }

    inline std::vector<double> gamma_inc_vec(std::vector<double> u, double a) {
        typedef SEXP(*Ptr_gamma_inc_vec)(SEXP,SEXP);
        static Ptr_gamma_inc_vec p_gamma_inc_vec = NULL;
        if (p_gamma_inc_vec == NULL) {
            validateSignature("std::vector<double>(*gamma_inc_vec)(std::vector<double>,double)");
            p_gamma_inc_vec = (Ptr_gamma_inc_vec)R_GetCCallable("specialfunctions", "_specialfunctions_gamma_inc_vec");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_gamma_inc_vec(Shield<SEXP>(Rcpp::wrap(u)), Shield<SEXP>(Rcpp::wrap(a)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<std::vector<double> >(rcpp_result_gen);
    }

    inline Rcpp::NumericVector gamma_inc_rcpp(Rcpp::NumericVector u, double a) {
        typedef SEXP(*Ptr_gamma_inc_rcpp)(SEXP,SEXP);
        static Ptr_gamma_inc_rcpp p_gamma_inc_rcpp = NULL;
        if (p_gamma_inc_rcpp == NULL) {
            validateSignature("Rcpp::NumericVector(*gamma_inc_rcpp)(Rcpp::NumericVector,double)");
            p_gamma_inc_rcpp = (Ptr_gamma_inc_rcpp)R_GetCCallable("specialfunctions", "_specialfunctions_gamma_inc_rcpp");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_gamma_inc_rcpp(Shield<SEXP>(Rcpp::wrap(u)), Shield<SEXP>(Rcpp::wrap(a)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<Rcpp::NumericVector >(rcpp_result_gen);
    }

    inline std::vector<double> ei_vec(std::vector<double> u) {
        typedef SEXP(*Ptr_ei_vec)(SEXP);
        static Ptr_ei_vec p_ei_vec = NULL;
        if (p_ei_vec == NULL) {
            validateSignature("std::vector<double>(*ei_vec)(std::vector<double>)");
            p_ei_vec = (Ptr_ei_vec)R_GetCCallable("specialfunctions", "_specialfunctions_ei_vec");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_ei_vec(Shield<SEXP>(Rcpp::wrap(u)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<std::vector<double> >(rcpp_result_gen);
    }

    inline double ei_one(double u) {
        typedef SEXP(*Ptr_ei_one)(SEXP);
        static Ptr_ei_one p_ei_one = NULL;
        if (p_ei_one == NULL) {
            validateSignature("double(*ei_one)(double)");
            p_ei_one = (Ptr_ei_one)R_GetCCallable("specialfunctions", "_specialfunctions_ei_one");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_ei_one(Shield<SEXP>(Rcpp::wrap(u)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<double >(rcpp_result_gen);
    }

    inline double factorial_one(double u) {
        typedef SEXP(*Ptr_factorial_one)(SEXP);
        static Ptr_factorial_one p_factorial_one = NULL;
        if (p_factorial_one == NULL) {
            validateSignature("double(*factorial_one)(double)");
            p_factorial_one = (Ptr_factorial_one)R_GetCCallable("specialfunctions", "_specialfunctions_factorial_one");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_factorial_one(Shield<SEXP>(Rcpp::wrap(u)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<double >(rcpp_result_gen);
    }

}

#endif // RCPP_specialfunctions_RCPPEXPORTS_H_GEN_
