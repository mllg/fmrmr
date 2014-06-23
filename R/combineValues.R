combineValues = function(x, combine, relevance, redundancy, alpha) {
  #FIXME: data can come from different sources here. we REALLY need to make sure it is sorted correctly
  # before we combine
  score = switch(combine,
    "difference" = alpha * relevance - redundancy,
    "quotient" = relevance^alpha / redundancy
  )
  res = data.frame(score = score, rel = relevance, redundancy = redundancy)
  rownames(res) = colnames(x)
  return(res)
}
