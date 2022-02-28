runDIF <- function(theData01,theData02,nSize_01 = 0,nSize_02 = 0,
                   resampling = 'bootstrap',theD = 1.7,nTime = 1000,
                   replace_sample = TRUE,sort_sample = FALSE,delta_0_bound = TRUE,delta_0_Beta = TRUE,choose_point = FALSE,
                   delta_bound_method = 'quantile',theSig = 0.05,theClass = 1,trial = 1,nTrial = 1){

  nItem <- length(theData01[1,])
  dif_Set <- 0

  if(nSize_01 == 0){
    nSize_01 <- length(theData01[,1])
    if(resampling == 'subsampling'){
      nSize_01 <- round(nSize_01 * 0.5)
    }
  }

  if(nSize_02 == 0){
    nSize_02 <- length(theData02[,1])
    if(resampling == 'subsampling'){
      nSize_02 <- round(nSize_02 * 0.5)
    }
  }

  ####################################### beta_01 beta_02
  trial_theta <- array(0, dim = c(2,nTrial)) # 1mean 2sd
  trial_beta <- array(0, dim = c(nItem,nTrial))

  colnames(theData01) <- paste0('i',1:nItem)
  colnames(theData02) <- paste0('i',1:nItem)
  temp_temp <- try(mirt(rbind(theData01,theData02),1,itemtype = "Rasch",verbose=F))
  if("try-error" %in% class(temp_temp))
  {
    simErr <- 1
  }else{
    simErr <- 0
    temp_BetaAlpha <- coef(temp_temp,simplify = TRUE)
    trial_beta[,trial] <- temp_BetaAlpha$items[, 2]
    trial_theta[,trial] <- c(0,1)
  }

  temp_temp <- try(mirt(theData01,1,itemtype = "Rasch",verbose=F))
  if("try-error" %in% class(temp_temp))
  {
    simErr1 <- 1
  }else{
    simErr1 <- 0
    temp_BetaAlpha <- coef(temp_temp,simplify = TRUE)
    beta_01 <- temp_BetaAlpha$items[, 2]
  }

  temp_temp <- try(mirt(theData02,1,itemtype = "Rasch",verbose=F))
  if("try-error" %in% class(temp_temp))
  {
    simErr2 <- 1
  }else{
    simErr2 <- 0
    temp_BetaAlpha <- coef(temp_temp,simplify = TRUE)
    beta_02 <- temp_BetaAlpha$items[, 2]
  }

  if(resampling == 'MonteCarlo'){
    theDIF <- runDifMonte('monte',theData01,theData02,nSize_01,nSize_02,nItem,theD,nTime,
                          trial_theta[,trial],trial_beta[,trial],beta_01,beta_02,
                          replace_sample,sort_sample,delta_0_bound,delta_0_Beta,choose_point,
                          delta_bound_method,theSig,theClass,trial,nTrial,dif_Set)

  }else if(resampling == 'jackknife'){
    theDIF <- runDifResampling('jackknife',theData01,theData02,nSize_01,nSize_02,nItem,theD,nTime,
                               beta_01,beta_02,
                               replace_sample,sort_sample,delta_0_bound,delta_0_Beta,choose_point,
                               delta_bound_method,theSig,theClass,trial,nTrial,dif_Set)

  }else if(resampling == 'bootstrap'){
    theDIF <- runDifResampling('bootstrap',theData01,theData02,nSize_01,nSize_02,nItem,theD,nTime,
                               beta_01,beta_02,
                               replace_sample,sort_sample,delta_0_bound,delta_0_Beta,choose_point,
                               delta_bound_method,theSig,theClass,trial,nTrial,dif_Set)

  }else if(resampling == 'subsampling'){
    theDIF <- runDifResampling('bootstrap',theData01,theData02,nSize_01,nSize_02,nItem,theD,nTime,
                               beta_01,beta_02,
                               replace_sample,sort_sample,delta_0_bound,delta_0_Beta,choose_point,
                               delta_bound_method,theSig,theClass,trial,nTrial,dif_Set)

  }

  return(theDIF)
}
