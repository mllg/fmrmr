#include "redundance.h"
#include "scale.h"
typedef unsigned int size;
using namespace Rcpp;

SEXP red_mean_abs_pearson(SEXP R_x) {
    const Eigen::Map<Eigen::MatrixXd> x(as<Eigen::Map<Eigen::MatrixXd> >(R_x));
    const size n = x.rows(), p = x.cols();
    Eigen::ArrayXXd xs = scale(x);
    Eigen::ArrayXd cor, res = Eigen::ArrayXd::Zero(p);

    for (size i = 0; i < p - 1; i++) {
        cor = (((xs.rightCols(p-i-1)).colwise() * xs.col(i)).colwise().sum() / (n-1)).abs();
        res(i) += cor.sum();
        res.tail(cor.size()) += cor;
    }

    res /= p;
    return wrap(res);
}
