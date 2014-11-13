#include <RcppArmadillo.h>
#include <vector>
#include <set>
// [[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::export]]
arma::vec cindex(const arma::vec & time, const arma::uvec & status, const arma::mat & x) {
    const arma::uvec order = arma::sort_index(time);
    const size_t p = x.n_cols;
    size_t counter = 0;
    arma::vec result(p, arma::fill::zeros);

    const arma::uvec::const_iterator order_end = order.end();
    for (arma::uvec::const_iterator i = order.begin(); i != order_end; i++) {
        if (!status(*i))
            continue;
        for (arma::uvec::const_iterator j = i + 1; j != order_end; j++) {
            if (time(*i) == time(*j)) {
                break;
            }
            counter++;
            for (size_t k = 0; k < p; k++) {
                if (x(*j, k) > x(*i, k)) {
                    result(k) += 1.0;
                } else if (x(*j, k) == x(*i, k)) {
                    result(k) += 0.5;
                }
            }
        }
    }

    if (counter == 0)
        Rf_error("Unable to calculate concordance index: No events");
    return (result / counter);
}

// [[Rcpp::export]]
Rcpp::List mrmr(const arma::vec & time, const arma::uvec & status, const arma::mat & x, size_t nselect) {
    /* scaled x matrix */
    arma::mat xs(x);
    xs.each_row() -= mean(x, 0);

    /* number of columns */
    const size_t p = x.n_cols;

    /* save indices of selected features and corresponding scores for return */
    std::vector<double> scores; scores.reserve(nselect);
    std::vector<size_t> selected; selected.reserve(nselect);

    /* logical to quickly look up whether x_i has already been selected or not */
    std::vector<bool> is_selected(p, false);

    /* vector to aggregate correlation sums of previous iterations */
    arma::vec cor_aggr(p, arma::fill::zeros);

    /* best_index and best pseudo score (score without considering some constants) */
    arma::uword best_index;
    double best_pscore;

     /* first element does not consider redundancy */
    const arma::vec relevance = cindex(time, status, x);
    scores.push_back(relevance.max(best_index));
    selected.push_back(best_index);
    is_selected[best_index] = true;

    for (size_t i = 1; i < nselect; i++) {
        size_t last_selected = selected.back();
        best_pscore = -1;
        for (size_t j = 0; j < p; j++) {
            if (!is_selected[j]) {
                cor_aggr(j) += fabs(arma::norm_dot(xs.col(last_selected), xs.col(j)));
                double pscore = relevance(j) / cor_aggr(j);
                if (pscore > best_pscore) {
                    best_pscore = pscore;
                    best_index = j;
                }
            }
        }
        is_selected[best_index] = true;
        scores.push_back(relevance(best_index) / (cor_aggr(best_index) / selected.size()));
        selected.push_back(best_index);
    }

    return Rcpp::List::create(Rcpp::Named("index") = selected, Rcpp::Named("score") = scores);
}
