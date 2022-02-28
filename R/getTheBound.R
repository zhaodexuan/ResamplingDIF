getTheBound <- function(theArray, nTime, replace_sample, sig_sig, delta_bound_method){
  theBound <- matrix(NA, nrow = nTime, ncol = 2)
  if (delta_bound_method == 'time.abs') {
    for (theTime in 1:nTime) {
      tempArray <- abs(sample(theArray, size = length(theArray), replace = replace_sample))
      theBound[theTime, 1] <- -quantile(tempArray, (1 - sig_sig / 2))
      theBound[theTime, 2] <- quantile(tempArray, (1 - sig_sig / 2))
    }
  }else if (delta_bound_method == 'time') {
    for (theTime in 1:nTime) {
      tempArray <- sample(theArray, size = length(theArray), replace = replace_sample)
      theBound[theTime, 1] <- quantile(tempArray, (sig_sig / 2))
      theBound[theTime, 2] <- quantile(tempArray, (1 - sig_sig / 2))
    }
  }
  
  return(theBound)
}