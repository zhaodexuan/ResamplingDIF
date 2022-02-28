data_montecarlo <- function(time_theta,time_beta,n_01,n_02,nItem,trial){
  tempData01 <- array(0, dim = c(n_01,nItem))
  tempData02 <- array(0, dim = c(n_02,nItem))
  
  true_theta_01 <- rnorm(n_01, mean = time_theta[1], sd = time_theta[2])
  true_theta_02 <- rnorm(n_02, mean = time_theta[1], sd = time_theta[2])
  true_beta_01 <- time_beta
  true_beta_02 <- time_beta
  
  group1Bank <- genDichoMatrix(items = nItem, model = "1PL", seed = (trial+100))
  group1Bank[,2] <- true_beta_01
  group2Bank <- group1Bank
  group2Bank[,2] <- true_beta_02
  
  tempData01 <- genPattern(true_theta_01,group1Bank,D=theD)
  tempData02 <- genPattern(true_theta_02,group2Bank,D=theD)
  
  simData <- list()
  simData[[1]] <- tempData01
  simData[[2]] <- tempData02
  simData[[3]] <- true_theta_01
  simData[[4]] <- true_theta_02
  simData[[5]] <- true_beta_01
  simData[[6]] <- true_beta_02
  names(simData) <- c('tempData01','tempData02','true_theta_01','true_theta_02',
                      'true_beta_01','true_beta_02')
  return(simData)
}
  