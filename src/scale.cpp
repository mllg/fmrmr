#include "scale.h"

Eigen::MatrixXd scale(const Eigen::MatrixXd &x) {
    Eigen::MatrixXd scaled = x.rowwise() - x.colwise().mean();
    return scaled.array().rowwise() / (scaled.colwise().norm() / std::sqrt(scaled.rows() - 1)).array();
}
