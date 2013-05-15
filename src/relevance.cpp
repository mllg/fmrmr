#include "relevance.h"
#include "order.h"
typedef unsigned int size;
using namespace Rcpp;


SEXP rel_cindex(SEXP R_time, SEXP R_status, SEXP R_x) {
    using namespace Rcpp;
    const NumericVector time(R_time);
    const LogicalVector status(R_status);
    const NumericMatrix x(R_x);
    const size n = x.nrow(), p = x.ncol();

    if (is_true(any(is_na(time))))
        Rf_error("Survival time contains missing values");
    if (is_true(any(is_na(status))))
        Rf_error("Censoring indicator contains missing values");
    if (is_true(all(time(0) == tail(time, n - 1))))
        Rf_error("All times are identical");
    if (is_true(all(!status)))
        Rf_error("No uncensored observation");
    if (is_true(any(is_na(x))))
        Rf_error("Feature matrix contains missing values");

    const IntegerVector ord = order(time);
    IntegerVector::iterator i, j;
    IntegerVector::const_iterator iend = ord.end() - 1;

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

    if (counter == 0)
        Rf_error("Internal error: Unable to calculate concordance index");
    res = res / counter;
    return wrap(res);
}
