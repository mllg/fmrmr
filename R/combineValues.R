combineValues = function(combine, relevance, redundancy, alpha) {
  switch(combine,
    "difference" = alpha * relevance - redundancy,
    "quotient" = relevance^alpha / redundancy
  )
}
