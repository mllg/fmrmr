#include "correlation.h"

using namespace Rcpp;
using namespace Eigen;
using std::sqrt;
using std::max;


MatrixXd scale(const MatrixXd &x) {
    MatrixXd scaled = x.rowwise() - x.colwise().mean();
    return scaled.array().rowwise() / (scaled.colwise().norm() / sqrt(scaled.rows() - 1)).array();
}

SEXP maxcor(SEXP R_x) {
    const Map<MatrixXd> x(as<Map<MatrixXd> >(R_x));
    const size n = x.rows(), p = x.cols();
    ArrayXXd xs = scale(x);
    ArrayXd cor, res = ArrayXd::Zero(p);

    for (size i = 0; i < p - 1; i++) {
        cor = (((xs.rightCols(p-i-1)).colwise() * xs.col(i)).colwise().sum() / (n-1)).abs();
        res(i) = max(res(i), cor.maxCoeff());
        res.tail(cor.size()) = res.tail(cor.size()).max(cor);
    }

    return wrap(res);
}

SEXP meancor(SEXP R_x) {
    const Map<MatrixXd> x(as<Map<MatrixXd> >(R_x));
    const size n = x.rows(), p = x.cols();
    ArrayXXd xs = scale(x);
    ArrayXd cor, res = ArrayXd::Zero(p);

    for (size i = 0; i < p - 1; i++) {
        cor = (((xs.rightCols(p-i-1)).colwise() * xs.col(i)).colwise().sum() / (n-1)).abs();
        res(i) += cor.sum();
        res.tail(cor.size()) += cor;
    }

    res /= p;
    return wrap(res);
}
