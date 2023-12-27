source("affn.R")

sqr <- function(q,ve) {
  
  delta = 2^(-2 / (q - 2)) * ve^(q / (q - 2))
  alpha = (ve / 2)^(1/(q - 2))
  
  M <- ceiling(0.5*log2(1 / ve))
  return(c(alpha, delta,M))
}