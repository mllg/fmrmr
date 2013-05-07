#include "cindex.h"

using namespace Rcpp;

SEXP cindex(SEXP R_time, SEXP R_status, SEXP R_x) {
    // FIXME time, status and x must be ordered!
    NumericVector time(R_time);
    LogicalVector status(R_status);
    NumericMatrix x(R_x);

    const size n = x.nrow(), p = x.ncol();
    size counter = 0;
    NumericVector res(p, 0);

    for (size i = 0; i < n - 1; i++) {
        if (!status(i))
            continue;
        for (size j = i + 1; j < n; j++) {
            if (time(i) == time(j))
                break;
            counter++;
            for (size k = 0; k < p; k++) {
                if (x(j, k) > x(i, k)) {
                    res(k) += 1.0;
                } else if (x(j, k) == x(i, k)) {
                    res(k) += 0.5;
                }
            }
        }
    }

    // FIXME counter == 0
    res = res / counter;
    return wrap(res);
}
