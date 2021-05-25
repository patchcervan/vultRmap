#include <Rcpp.h>
using namespace Rcpp;

//' Calculate minimum distance
//' @description This function calculates the minimum distance between a matrix
//' of locations r and another matrix of locations x
//' @param r A numeric matrix with each row being a pair of coordinates.
//' @param x A numeric matrix with each row being a pair of coordinates.
//' @return A vector with as many elements as rows in r, corresponding to the minimum distance between the rows in r and all the rows in x.
//' @export
//'
// [[Rcpp::export]]
NumericVector calcMinDist_cpp(NumericMatrix r, NumericMatrix x) {
    int n = r.nrow();
    int m = x.nrow();

    NumericVector d(n);

    for(int i = 0; i < n; ++i) {
        NumericVector cc = r(i,_);
        NumericVector dd(m);

        for(int j = 0; j < m; ++j){
            dd[j] = sqrt(sum(pow(cc - x(j,_), 2.0)));
        }

        d[i] = min(dd);
    }
    return d;
}
