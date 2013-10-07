#include "redundance.h"
#include "scale.h"
using namespace Rcpp;

SEXP red_mean_abs_pearson(SEXP R_x) {
    const Eigen::Map<Eigen::MatrixXd> x(as<Eigen::Map<Eigen::MatrixXd> >(R_x));
    const size_t nm1 = x.rows() - 1, pm1 = x.cols() - 1;
    Eigen::ArrayXXd xs = scale(x);
    Eigen::ArrayXd tmp, res = Eigen::ArrayXd::Zero(x.cols());

    for (size_t i = 0, j = pm1; i < pm1; i++, j--) {
        tmp = (((xs.rightCols(j)).colwise() * xs.col(i)).colwise().sum() / nm1).abs();
        res(i) += tmp.sum();
        res.tail(j) += tmp;
    }

    res /= res.size();
    return wrap(res);
}

SEXP red_mean_mim(SEXP R_x) {
    const Eigen::Map<Eigen::MatrixXd> x(as<Eigen::Map<Eigen::MatrixXd> >(R_x));
    const size_t nm1 = x.rows() - 1, pm1 = x.cols() - 1;
    Eigen::ArrayXXd xs = scale(x);
    Eigen::ArrayXd tmp, res = Eigen::ArrayXd::Zero(x.cols());

    for (size_t i = 0, j = pm1; i < pm1; i++, j--) {
        tmp = -0.5 * (1 - (((xs.rightCols(j)).colwise() * xs.col(i)).colwise().sum() / nm1).square()).log();
        res(i) += tmp.sum();
        res.tail(j) += tmp;
    }

    res /= res.size();
    return wrap(res);
}
