data_bootstrap <- function(theData01,theData02,n_01,n_02,nItem,replace_sample){
  tempData01 <- array(0, dim = c(n_01,nItem))
  tempData02 <- array(0, dim = c(n_02,nItem))
  
  t_sample01 <- sample(1:n_01,size = n_01,replace = replace_sample)
  t_sample02 <- sample(1:n_02,size = n_02,replace = replace_sample)
  tempData01 <- theData01[t_sample01,]
  tempData02 <- theData02[t_sample02,]
  
  reData <- list()
  reData[[1]] <- rbind(tempData01[1:(n_01/2),], tempData02[1:(n_02/2),])
  reData[[2]] <- rbind(tempData01[(n_01/2 + 1):n_01,], tempData02[(n_02/2 + 1):n_02,])
  return(reData)
}
  