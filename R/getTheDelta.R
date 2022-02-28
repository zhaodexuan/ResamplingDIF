getTheDelta <- function(all_beta_01,all_beta_02,nTime,nItem,delta_0_bound,sort_sample,replace_sample,sig_sig,delta_bound_method){
  all_beta_delta_0 <- array(0, dim = c(nTime, 1))
  for (theTime in 1:nTime) {
    temp_beta_delta <- all_beta_01[theTime, ] - all_beta_02[theTime, ]
    
    if(delta_0_bound){
      temp_beta_delta_sort <- sort(temp_beta_delta)
      all_beta_delta_0[theTime,1] <- mean(temp_beta_delta_sort[(floor(nItem / 2) - 1):(floor(nItem / 2) + 2)])
    }
  }
  
  all_beta_delta <- array(0, dim = c(nTime, nItem))
  for (theTime in 1:nTime) {
    all_beta_delta[theTime,] <- all_beta_01[theTime, ] - all_beta_02[theTime, ]
    all_beta_delta[theTime,] <- all_beta_delta[theTime, ] - all_beta_delta_0[theTime, 1]
    
    if(sort_sample){
      all_beta_delta[theTime, ] <- sort(all_beta_delta[theTime, ])
    }
  }
  
  beta_delta_bound <- array(0, dim = c(2, nItem)) #1_low 2_high
  for (j in 1:nItem) {
    if(delta_bound_method == 'time'){
      tempBound <- getTheBound(all_beta_delta[,j], nTime, replace_sample, sig_sig, delta_bound_method)
      beta_delta_bound[1,j] <- mean(tempBound[,1])
      beta_delta_bound[2,j] <- mean(tempBound[,2])
    }else if(delta_bound_method == 'time.abs'){
      tempBound <- getTheBound(all_beta_delta[,j], nTime, replace_sample, sig_sig, delta_bound_method)
      beta_delta_bound[1,j] <- mean(tempBound[,1])
      beta_delta_bound[2,j] <- mean(tempBound[,2])
    }else if(delta_bound_method == 'qnorm'){
      tempMean <- mean(all_beta_delta[,j])
      tempSd <- sd(all_beta_delta[,j])
      beta_delta_bound[1,j] <- qnorm((sig_sig/2), mean = tempMean, sd = tempSd)
      beta_delta_bound[2,j] <- qnorm((1-sig_sig/2), mean = tempMean, sd = tempSd)
    }else if(delta_bound_method == 'qnorm.abs'){
      tempMean <- mean(abs(all_beta_delta[,j]))
      tempSd <- sd(abs(all_beta_delta[,j]))
      beta_delta_bound[1,j] <- -qnorm((1-sig_sig/2), mean = tempMean, sd = tempSd)
      beta_delta_bound[2,j] <- qnorm((1-sig_sig/2), mean = tempMean, sd = tempSd)
    }else if(delta_bound_method == 'quantile'){
      beta_delta_bound[1,j] <- quantile(all_beta_delta[,j],(sig_sig/2))
      beta_delta_bound[2,j] <- quantile(all_beta_delta[,j],(1-sig_sig/2))
    }else if(delta_bound_method == 'quantile.abs'){
      beta_delta_bound[1,j] <- -quantile(abs(all_beta_delta[,j]),(1-sig_sig/2))
      beta_delta_bound[2,j] <- quantile(abs(all_beta_delta[,j]),(1-sig_sig/2))
    }else if(delta_bound_method == 't.test'){
      T_all <- t.test(all_beta_delta[,j], alternative = 'two.sided', conf.level = (1-sig_sig))
      beta_delta_bound[1,j] <- T_all$conf.int[[1]]
      beta_delta_bound[2,j] <- T_all$conf.int[[2]]
    }else if(delta_bound_method == 't.test.abs'){
      T_all <- t.test(abs(all_beta_delta[,j]), alternative = 'less', conf.level = (1-sig_sig))
      beta_delta_bound[1,j] <- -T_all$conf.int[[2]]
      beta_delta_bound[2,j] <- T_all$conf.int[[2]]
    }
  }
  
  theList <- list()
  theList[[1]] <- all_beta_delta_0
  theList[[2]] <- all_beta_delta
  theList[[3]] <- beta_delta_bound
  return(theList)
}