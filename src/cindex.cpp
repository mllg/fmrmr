#include "cindex.h"

using namespace Rcpp;

typedef std::pair<int, double> paired;

bool cmp_second(const paired& left, const paired& right) {
    return left.second < right.second;
}

IntegerVector order(const NumericVector &x) {
    const size n = x.size();
    std::vector<paired> pairs; pairs.reserve(n);
    IntegerVector result(n);

    for(size i = 0; i < n; i++)
        pairs.push_back(std::make_pair(i, x(i)));

    std::sort(pairs.begin(), pairs.end(), cmp_second);

    for(size i = 0; i < n; i++)
        result(i) = pairs[i].first;

    return result;
}

SEXP cindex(SEXP R_time, SEXP R_status, SEXP R_x) {
    const NumericVector time(R_time);
    const LogicalVector status(R_status);
    const NumericMatrix x(R_x);

    if (is_true(any(is_na(time))) || is_true(any(is_na(status)))) {
        exception("Survival time or censoring indicator contain missing values");
        return R_NilValue;
    }
    if (is_true(all(!status))) {
        exception("No uncensored observation");
        return R_NilValue;
    }
    if (is_true(any(is_na(x)))) {
        exception("Feature matrix contains missing values");
        return R_NilValue;
    }

    const IntegerVector ord = order(time);
    IntegerVector::iterator i, j;
    IntegerVector::const_iterator iend = ord.end() - 1;
    const size p = x.ncol();

    size counter = 0;
    NumericVector res(p, 0);

    for (i = ord.begin(); i != iend; i++) {
        if (!status(*i))
            continue;
        for (j = i + 1; j != ord.end(); j++) {
            if (time(*i) == time(*j))
                break;
            counter++;
            for (size k = 0; k < p; k++) {
                if (x(*j, k) > x(*i, k)) {
                    res(k) += 1.0;
                } else if (x(*j, k) == x(*i, k)) {
                    res(k) += 0.5;
                }
            }
        }
    }

    //if (counter == 0) ..
    res = res / counter;
    return wrap(res);
}
