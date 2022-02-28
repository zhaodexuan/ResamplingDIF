runDifResampling <- function(resampling,theData01,theData02,n_01,n_02,nItem,theD,nTime,
                             beta_01,beta_02,
                             replace_sample,sort_sample,delta_0_bound,delta_0_Beta,choose_point,
                             delta_bound_method,sig_sig,theClass,trial,nTrial,dif_Set){
  # resampling <- 'jackknife'
  # nTime <- jack_time
  all_beta_01 <- array(0, dim = c(nTime,nItem))
  all_beta_02 <- array(0, dim = c(nTime,nItem))
  for (theTime in 1:nTime) {
    # theTime <- 2
    print(paste0("class", theClass, " trial ", trial, "/", nTrial, ", ", resampling, " ", theTime, "/", nTime, ' ', Sys.time()))
    
    reData <- list()
    simErr1 <- 1
    simErr2 <- 1
    while ((simErr1 + simErr2) > 0) {
      if(resampling == 'jackknife'){
        reData <- data_jackknife(theTime,nTime,theData01,theData02,n_01,n_02,nItem)
      }else if(resampling == 'bootstrap'){
        reData <- data_bootstrap(theData01,theData02,n_01,n_02,nItem,replace_sample)
      }
      
      tempData01 <- reData[[1]]
      tempData02 <- reData[[2]]
      
      colnames(tempData01) <- paste0('i',1:nItem)
      temp_temp <- try(mirt(tempData01,1,itemtype = "Rasch",verbose=F))
      if ("try-error" %in% class(temp_temp))
      {
        simErr1 <- 1
      } else{
        simErr1 <- 0
        temp_BetaAlpha <- coef(temp_temp,simplify = TRUE)
        all_beta_01[theTime,] <- temp_BetaAlpha$items[, 2]
      }
      
      colnames(tempData02) <- paste0('i',1:nItem)
      temp_temp <- mirt(tempData02,1,itemtype = "Rasch",verbose=F)
      if ("try-error" %in% class(temp_temp))
      {
        simErr2 <- 1
      } else{
        simErr2 <- 0
        temp_BetaAlpha <- coef(temp_temp,simplify = TRUE)
        all_beta_02[theTime,] <- temp_BetaAlpha$items[, 2]
      }
      
      if((simErr1 + simErr2) > 0 && resampling == 'jackknife'){
        resampling <- 'bootstrap'
        print("jackknife error, change into bootstrap")
      }
    }
  }
  
  delta_list <- getTheDelta(all_beta_01,all_beta_02,nTime,nItem,delta_0_bound,sort_sample,replace_sample,sig_sig,delta_bound_method)
  all_beta_delta_0 <- delta_list[[1]]
  all_beta_delta <- delta_list[[2]]
  delta_bound_beta <- delta_list[[3]] #1_low 2_high
  
  fileName <- paste0("beta01_", resampling, "_trial", trial,".csv")
  write.table(all_beta_01, fileName, sep = ",")
  
  fileName <- paste0("beta02_", resampling, "_trial", trial,".csv")
  write.table(all_beta_02, fileName, sep = ",")
  
  fileName <- paste0("beta_delta_", resampling, "_trial", trial,".csv")
  write.table(all_beta_delta, fileName, sep = ",")
  
  fileName <- paste0("beta_delta_bound_", resampling, "_trial", trial,".csv")
  write.table(delta_bound_beta, fileName, sep = ",")
  
  # colnames(theData01) <- paste0('i',1:nItem)
  # temp_temp <- mirt(theData01,1,itemtype = "Rasch",verbose=F)
  # temp_BetaAlpha <- coef(temp_temp,simplify = TRUE)
  # beta_01 <- temp_BetaAlpha$items[, 2]
  # 
  # colnames(theData02) <- paste0('i',1:nItem)
  # temp_temp <- mirt(theData02,1,itemtype = "Rasch",verbose=F)
  # temp_BetaAlpha <- coef(temp_temp,simplify = TRUE)
  # beta_02 <- temp_BetaAlpha$items[, 2]
  
  beta_delta <- beta_01 - beta_02
  if(delta_0_Beta){
    beta_delta_sort <- sort(beta_delta)
    beta_delta_0 <- mean(beta_delta_sort[(floor(nItem / 2) - 1):(floor(nItem / 2) + 2)])
    beta_delta <- beta_delta - beta_delta_0
  }
  rank_beta_delta <- rank(beta_delta)
  order_beta_delta <- order(beta_delta)
  sig_temp_beta <- get_sig_temp(choose_point,nItem,sort_sample,beta_delta,delta_bound_beta,rank_beta_delta,order_beta_delta)
  
  theList <- list()
  theList[[1]] <- sig_temp_beta
  names(theList) <- c('beta')
  return(theList)
}