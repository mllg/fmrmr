#include "redundance.h"
#include "scale.h"
typedef unsigned int size;
using namespace Rcpp;

SEXP red_mean_abs_pearson(SEXP R_x) {
    const Eigen::Map<Eigen::MatrixXd> x(as<Eigen::Map<Eigen::MatrixXd> >(R_x));
    const size n = x.rows(), p = x.cols();
    Eigen::ArrayXXd xs = scale(x);
    Eigen::ArrayXd tmp, res = Eigen::ArrayXd::Zero(p);

    for (size i = 0; i < p - 1; i++) {
        tmp = (((xs.rightCols(p-i-1)).colwise() * xs.col(i)).colwise().sum() / (n-1)).abs();
        res(i) += tmp.sum();
        res.tail(tmp.size()) += tmp;
    }

    res /= p;
    return wrap(res);
}

SEXP red_mean_mim(SEXP R_x) {
    const Eigen::Map<Eigen::MatrixXd> x(as<Eigen::Map<Eigen::MatrixXd> >(R_x));
    const size n = x.rows(), p = x.cols();
    Eigen::ArrayXXd xs = scale(x);
    Eigen::ArrayXd tmp, res = Eigen::ArrayXd::Zero(p);

    for (size i = 0; i < p - 1; i++) {
        tmp = -0.5 * (1 - (((xs.rightCols(p-i-1)).colwise() * xs.col(i)).colwise().sum() / (n-1)).square()).log();
        res(i) += tmp.sum();
        res.tail(tmp.size()) += tmp;
    }

    res /= p;
    return wrap(res);
}
