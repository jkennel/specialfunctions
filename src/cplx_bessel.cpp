#include <Rcpp.h>

// [[Rcpp::interfaces(cpp)]]



//' @title
 //' bessel_k
 //'
 //' @description
 //' BesselK from base
 //'
 //' @param x \code{numeric} value to evaluate
 //' @param v \code{numeric} value to evaluate
 //'
 //' @return bessel function result
 //'
 //'
 //' @export
 // [[Rcpp::export]]
 Rcpp::NumericVector bessel_k(const Rcpp::NumericVector &x,
                              double nu,
                              bool expon_scaled)
 {
   
   // Obtain environment containing function
   Rcpp::Environment base("package:base");
   
   // Make function callable from C++
   Rcpp::Function bessel_k = base["besselK"];
   
   return(bessel_k(x, nu, expon_scaled));
   
 }


//' @title
 //' bessel_i
 //'
 //' @description
 //' besselI from base
 //'
 //' @param x \code{numeric} value to evaluate
 //' @param v \code{numeric} value to evaluate
 //'
 //' @return bessel function result
 //'
 //'
 //' @export
 // [[Rcpp::export]]
 Rcpp::NumericVector bessel_i(const Rcpp::NumericVector &x,
                              double nu,
                              bool expon_scaled)
 {
   
   // Obtain environment containing function
   Rcpp::Environment base("package:base");
   
   // Make function callable from C++
   Rcpp::Function bessel_k = base["besselI"];
   
   return(bessel_k(x, nu, expon_scaled));
   
 }

//' @title
 //' bessel_j
 //'
 //' @description
 //' besselJ from base
 //'
 //' @param x \code{numeric} value to evaluate
 //' @param v \code{numeric} value to evaluate
 //'
 //' @return bessel function result
 //'
 //'
 //' @export
 // [[Rcpp::export]]
 Rcpp::NumericVector bessel_j(const Rcpp::NumericVector &x,
                              double nu,
                              bool expon_scaled)
 {
   
   // Obtain environment containing function
   Rcpp::Environment base("package:base");
   
   // Make function callable from C++
   Rcpp::Function bessel_k = base["besselJ"];
   
   return(bessel_k(x, nu, expon_scaled));
   
 }


//' @title
 //' bessel_y
 //'
 //' @description
 //' besselY from base
 //'
 //' @param x \code{numeric} value to evaluate
 //' @param v \code{numeric} value to evaluate
 //'
 //' @return bessel function result
 //'
 //'
 //' @export
 // [[Rcpp::export]]
 Rcpp::NumericVector bessel_y(const Rcpp::NumericVector &x,
                              double nu,
                              bool expon_scaled)
 {
   
   // Obtain environment containing function
   Rcpp::Environment base("package:base");
   
   // Make function callable from C++
   Rcpp::Function bessel_k = base["besselY"];
   
   return(bessel_k(x, nu, expon_scaled));
   
 }


//' @title
 //' bessel_k_wrapper
 //'
 //' @description
 //' Modified Bessel function of first kind order 1
 //'
 //' @param x \code{numeric} value to evaluate
 //' @param v \code{numeric} value to evaluate
 //'
 //' @return bessel function result
 //'
 //'
 //' @export
 // [[Rcpp::export]]
 Rcpp::ComplexMatrix bessel_k_cplx(const Rcpp::ComplexVector &x,
                                   double nu,
                                   bool expon_scaled,
                                   size_t nseq)
 {
   
   // Obtain environment containing function
   Rcpp::Environment base("package:Bessel");
   
   // Make function callable from C++
   Rcpp::Function bessel_k = base["BesselK"];
   
   return(bessel_k(x, nu, expon_scaled, nseq));
   
 }

//' @title
 //' bessel_i_wrapper
 //'
 //' @description
 //' Modified Bessel function of first kind order 1
 //'
 //' @param x \code{numeric} value to evaluate
 //' @param v \code{numeric} value to evaluate
 //'
 //' @return bessel function result
 //'
 //'
 //' @export
 // [[Rcpp::export]]
 Rcpp::ComplexMatrix bessel_i_cplx(const Rcpp::ComplexVector &x,
                                   double nu,
                                   bool expon_scaled,
                                   size_t nseq)
 {
   
   // Obtain environment containing function
   Rcpp::Environment base("package:Bessel");
   
   // Make function callable from C++
   Rcpp::Function bessel_i = base["BesselI"];
   
   return (bessel_i(x, nu, expon_scaled, nseq));
 }

//' @title
 //' bessel_j_wrapper
 //'
 //' @description
 //' Modified Bessel function of first kind order 1
 //'
 //' @param x \code{numeric} value to evaluate
 //' @param v \code{numeric} value to evaluate
 //'
 //' @return bessel function result
 //'
 //'
 //' @export
 // [[Rcpp::export]]
 Rcpp::ComplexMatrix bessel_j_cplx(const Rcpp::ComplexVector &x,
                                   double nu,
                                   bool expon_scaled,
                                   size_t nseq)
 {
   
   // Obtain environment containing function
   Rcpp::Environment base("package:Bessel");
   
   // Make function callable from C++
   Rcpp::Function bessel_j = base["BesselJ"];
   
   return (bessel_j(x, nu, expon_scaled, nseq));
 }

//' @title
 //' bessel_j_wrapper
 //'
 //' @description
 //' Modified Bessel function of first kind order 1
 //'
 //' @param x \code{numeric} value to evaluate
 //' @param v \code{numeric} value to evaluate
 //'
 //' @return bessel function result
 //'
 //'
 //' @export
 // [[Rcpp::export]]
 Rcpp::ComplexMatrix bessel_y_cplx(const Rcpp::ComplexVector &x,
                                   double nu,
                                   bool expon_scaled,
                                   size_t nseq)
 {
   
   // Obtain environment containing function
   Rcpp::Environment base("package:Bessel");
   
   // Make function callable from C++
   Rcpp::Function bessel_y = base["BesselY"];
   
   return (bessel_y(x, nu, expon_scaled, nseq));
 }

/*** R
*/
