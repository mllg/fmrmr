#include "order.h"
typedef std::pair<int, double> paired;
using namespace Rcpp;

bool cmp_second(const paired &left, const paired &right) {
    return left.second < right.second;
}

IntegerVector order(const NumericVector &x) {
    const size_t n = x.size();
    std::vector<paired> pairs;
    pairs.reserve(n);
    IntegerVector result(n);

    for(size_t i = 0; i < n; i++)
        pairs.push_back(std::make_pair(i, x(i)));

    std::sort(pairs.begin(), pairs.end(), cmp_second);

    for(size_t i = 0; i < n; i++)
        result(i) = pairs[i].first;

    return result;
}
