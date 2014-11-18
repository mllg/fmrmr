#include <RcppArmadillo.h>
#include <vector>
// [[Rcpp::depends(RcppArmadillo)]]


typedef std::pair<int, double> paired;

bool cmp_second(const paired & left, const paired & right) {
    return left.second < right.second;
}

Rcpp::IntegerVector order(const Rcpp::NumericVector & x) {
    const size_t n = x.size();
    std::vector<paired> pairs; pairs.reserve(n);

    for(size_t i = 0; i < n; i++)
        pairs.push_back(std::make_pair(i, x(i)));

    std::sort(pairs.begin(), pairs.end(), cmp_second);

    Rcpp::IntegerVector result(n);
    for(size_t i = 0; i < n; i++)
        result(i) = pairs[i].first;
    return result;
}

Rcpp::NumericVector cindex(const Rcpp::NumericVector & time, const Rcpp::LogicalVector & status, const Rcpp::NumericMatrix & x) {
    const Rcpp::IntegerVector ord = order(time);
    Rcpp::IntegerVector::const_iterator i, j;
    const Rcpp::IntegerVector::const_iterator iend = ord.end() - 1;
    const Rcpp::IntegerVector::const_iterator jend = ord.end();
    const size_t p = x.cols();
    int counter = 0;
    Rcpp::NumericVector res(p);

    for (i = ord.begin(); i != iend; i++) {
        if (status(*i)) {
            for (j = i + 1; j != jend; j++) {
                if (time(*i) == time(*j))
                    break;
                counter++;
                for (size_t k = 0; k < p; k++) {
                    if (x(*j, k) > x(*i, k)) {
                        res(k) += 1.0;
                    } else if (x(*j, k) == x(*i, k)) {
                        res(k) += 0.5;
                    }
                }
            }
        }
    }
    if (counter == 0)
        Rcpp::stop("Internal error: Unable to calculate concordance index. No events or identical survival times.");
    return(res / counter);
}

// [[Rcpp::export]]
Rcpp::List mrmr(const Rcpp::NumericVector & time, const Rcpp::LogicalVector & status, const Rcpp::NumericMatrix & x, const int nselect) {
    /* construct a centered matrix to efficiently compute correlations */
    const size_t p = x.cols();
    arma::mat xc(x.begin(), x.rows(), p, true);
    xc.each_row() -= mean(xc, 0);

    /* some containers to store results and stuff */
    std::vector<bool> is_selected(p, false);
    Rcpp::IntegerVector selected(nselect);
    Rcpp::NumericVector scores(nselect);
    arma::vec cor_aggr(p, arma::fill::zeros);
    size_t best_index;

     /* first element does not consider redundancy */
    const Rcpp::NumericVector relevance = cindex(time, status, x);
    best_index = which_max(relevance);
    is_selected[best_index] = true;
    selected[0] = best_index;
    scores[0] = relevance[best_index];

    for (int j = 1; j < nselect; j++) {
        double best_pscore = -1;
        const size_t last = selected[j-1];
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

        is_selected[best_index] = true;
        selected[j] = best_index;
        scores[j] = best_pscore * j;
    }

    return Rcpp::List::create(Rcpp::Named("index") = selected, Rcpp::Named("score") = scores);
}
