rm(list= ls())
euclidean<-function(a, b){
  stopifnot(is.integer(a),is.integer(b))
  while (b!=0)
  {t <- b;
  b <- a %% t;
  a <- t;
  }
  return(abs(a))
}
