get_sig_temp <- function (choose_point,nItem,sort_sample,beta_delta,beta_delta_bound,rank_beta_delta,order_beta_delta){
  sig_temp <- array(0,dim = c(nItem))
  
  if(choose_point){
    if(sort_sample){
      j <- 0
      stop_point <- 0
      while (j <= nItem && stop_point == 0) {
        j <- j+1
        if(beta_delta[order_beta_delta[j]]>beta_delta_bound[2,j] || beta_delta[order_beta_delta[j]]<beta_delta_bound[1,j]){
          sig_temp[j] <- 1
        }else{
          stop_point <- 1
        }
      }
      
      j <- nItem + 1
      stop_point <- 0
      while (j >= 1 && stop_point == 0) {
        j <- j - 1
        if(beta_delta[order_beta_delta[j]]>beta_delta_bound[2,j] || beta_delta[order_beta_delta[j]]<beta_delta_bound[1,j]){
          sig_temp[j] <- 1
        }else{
          stop_point <-1
        }
      }
    }else{
      j <- 0
      stop_point <- 0
      while (j <= nItem && stop_point == 0) {
        j <- j+1
        if(beta_delta[order_beta_delta[j]]>beta_delta_bound[2,order_beta_delta[j]] || beta_delta[order_beta_delta[j]]<beta_delta_bound[1,order_beta_delta[j]]){
          sig_temp[j] <- 1
        }else{
          stop_point <-1
        }
      }
      
      j <- nItem + 1
      stop_point <- 0
      while (j >= 1 && stop_point == 0) {
        j <- j - 1
        if(beta_delta[order_beta_delta[j]]>beta_delta_bound[2,order_beta_delta[j]] || beta_delta[order_beta_delta[j]]<beta_delta_bound[1,order_beta_delta[j]]){
          sig_temp[j] <- 1
        }else{
          stop_point <-1
        }
      }
    }
  }else{
    for (j in 1:nItem) {
      if(sort_sample){
        if(beta_delta[j]>beta_delta_bound[2,rank_beta_delta[j]] || beta_delta[j]<beta_delta_bound[1,rank_beta_delta[j]]){
          sig_temp[j] <- 1
        }
      }else{
        if(beta_delta[j]>beta_delta_bound[2,j] || beta_delta[j]<beta_delta_bound[1,j]){
          sig_temp[j] <- 1
        }
      }
    }
  }
  
  return(sig_temp)
}