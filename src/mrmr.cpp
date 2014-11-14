#include <RcppArmadillo.h>
#include <vector>
// [[Rcpp::depends(RcppArmadillo)]]

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
        Rcpp::stop("Unable to calculate concordance index: No events");
    return (result / counter);
}

// [[Rcpp::export]]
Rcpp::List mrmr(const arma::vec & time, const arma::uvec & status, const arma::mat & x, const size_t nselect) {
    /* construct a centered matrix to efficiently compute correlations */
    const size_t p = x.n_cols;
    arma::mat xc(x);
    xc.each_row() -= mean(x, 0);

    /* some containers to store results and stuff */
    std::vector<double> scores; scores.reserve(nselect);
    std::vector<size_t> selected; selected.reserve(nselect);
    std::vector<bool> is_selected(p, false);
    arma::vec cor_aggr(p, arma::fill::zeros);
    arma::uword best_index;

     /* first element does not consider redundancy */
    const arma::vec relevance = cindex(time, status, x);
    scores.push_back(relevance.max(best_index));
    selected.push_back(best_index);
    is_selected[best_index] = true;

    while (selected.size() < nselect) {
        double best_pscore = -1;
        const size_t last = selected.back();
        for (size_t i = 0; i < p; i++) {
            if (!is_selected[i]) {
                cor_aggr(i) += fabs(arma::norm_dot(xc.col(last), xc.col(i)));
                double pscore = relevance(i) / cor_aggr(i);
                if (pscore > best_pscore) {
                    best_pscore = pscore;
                    best_index = i;
                }
            }
        }

        scores.push_back(best_pscore * selected.size());
        is_selected[best_index] = true;
        selected.push_back(best_index);
    }

    return Rcpp::List::create(Rcpp::Named("index") = selected, Rcpp::Named("score") = scores);
}
