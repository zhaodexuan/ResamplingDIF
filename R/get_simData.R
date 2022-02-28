get_simData <- function(nExaminee1,nExaminee2,nItem,mean_sd_Theta,dif_Set,theD,trial){
  group1Bank <- genDichoMatrix(items = nItem, model = "1PL", seed = (trial+100))
  group2Bank <- group1Bank
  group2Bank[,2] <- group1Bank[,2] + dif_Set
  
  true_theta_01 <- rnorm(nExaminee1, mean = mean_sd_Theta[1,1], sd = mean_sd_Theta[1,2])
  true_theta_02 <- rnorm(nExaminee2, mean = mean_sd_Theta[2,1], sd = mean_sd_Theta[2,2])
  true_beta_01 <- group1Bank[,2]
  true_beta_02 <- group2Bank[,2]
  
  theData01 <- genPattern(true_theta_01,group1Bank,D=theD)
  theData02 <- genPattern(true_theta_02,group2Bank,D=theD)
  
  simData <- list()
  simData[[1]] <- theData01
  simData[[2]] <- theData02
  simData[[3]] <- true_theta_01
  simData[[4]] <- true_theta_02
  simData[[5]] <- true_beta_01
  simData[[6]] <- true_beta_02
  names(simData) <- c('theData01','theData02','true_theta_01','true_theta_02','true_beta_01','true_beta_02')
  return(simData)
}