data_jackknife <- function(theTime,nTime,theData01,theData02,n_01,n_02,nItem){
  tempData01 <- array(0, dim = c(n_01,nItem))
  tempData02 <- array(0, dim = c(n_02,nItem))
  
  t_sample01 <- (theTime-1)*(n_01/nTime)+1
  t_sample02 <- (theTime-1)*(n_02/nTime)+1
  tempData01 <- theData01[-t_sample01,]
  tempData02 <- theData02[-t_sample02,]
  
  L1 <-length(tempData01[,1])
  L2 <-length(tempData02[,1])
  
  W1 <- round(L1/2)
  W2 <- round(L2/2)
  
  reData <- list()
  reData[[1]] <- rbind(tempData01[1:W1,], tempData02[(W2 + 1):L2,])
  reData[[2]] <- rbind(tempData01[(W1 + 1):L1,], tempData02[1:W2,])
  return(reData)
}