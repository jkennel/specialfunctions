#include <Rcpp.h>

// [[Rcpp::depends(BH)]]

#include <boost/math/special_functions/gamma.hpp>
#include <boost/math/special_functions/expint.hpp>
#include <boost/math/special_functions/erf.hpp>
#include <boost/math/special_functions/factorials.hpp>
#include <boost/math/special_functions/bessel.hpp>

// [[Rcpp::interfaces(cpp)]]

//==============================================================================
//' @title
//' exp_int_single
//'
//' @description
//' Calculate the exponential integral using Boost
//'
//' @param u value of the Theis u
//' @param a not used (flow dimension)
//'
//' @return exponential integral
//'
//' @importFrom Rcpp evalCpp
//'
//' @export
//'
// [[Rcpp::export]]
double ei_bh(double u, double a) {
  
  if (u == 0){
    u = R_PosInf;
  } else if (u > 40.0){
    u = 0;
  } else {
    u = -boost::math::expint(-u);
  }
  
  return(u);
}

//==============================================================================
//' @title
//' bessel_k_single
//'
//' @description
//' boost function for the Bessel function
//'
//' @param x value for bessel function
//' @param nu order
//'
//' @return result of the bessel function
//'
//'
//' @export
//'
// [[Rcpp::export]]
double bessel_k_bh(double x, int nu) {
  
  return(boost::math::cyl_bessel_k(nu, x));
  
}


// [[Rcpp::export]]
double factorial_bh(int x) {

  return(boost::math::factorial<double>(x));

}




// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

/*** R

bench::mark(
  specialfunctions:::ei_bh(0.1, 0),
  specialfunctions:::gamma_inc_single(0.1, 0)
)


*/
