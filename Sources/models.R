library(Rmpfr)
library(DEoptim)
library(TTR)
library(baseModels)


acaMse = function(Hinit2, generated_data, sim, half_index, end_index,movAvg){
  ACA = DEoptim(aca_negLogLik1, lower = 0, upper = 1, H=Hinit2, allpaths = generated_data[1:half_index,], model=1, sim=sim, DEoptim.control(NP = 10,F = 0.8, CR = 0.9, trace = FALSE, itermax = 20))
  alpha_ACA = ACA$optim$bestmem[1]
  
  ACA_probMatrix = baseModels::getProbMatrix(generated_data, alpha_ACA,H=Hinit2,sim,model=1)
  mse_ACA_mat = (pathProbability(generated_data,ACA_probMatrix,sim)-movAvg)^2
  mse_ACA = sum(mse_ACA_mat[(half_index+1):end_index])/(end_index-half_index)
  # ACA_lik_mat <-aca_mle_lik(generated_data,alpha_ACA, H=Hinit2, model=1,sim=2)
  # ACA_lik <- (-1)*sum(ACA_lik_mat[(half_index+1):end_index])
  return(list("mse" = mse_ACA, "alpha" = alpha_ACA))
}

gbMse = function(Hinit2, generated_data, sim, half_index, end_index,movAvg){
  
  GB <- DEoptim(aca_negLogLik1,lower = 0, upper = 1, H = Hinit2, allpaths = generated_data[1:half_index,], model = 2, sim=sim, DEoptim.control(NP=10,F=0.8, CR = 0.9,trace = FALSE, itermax = 20))
  alpha_GB = GB$optim$bestmem[1]
  ## Compute MSE for Model2 using simulated data
  GB_probMatrix <- baseModels::getProbMatrix(generated_data, alpha_GB, H=Hinit2, sim, model=2)
  mse_GB_mat <- (pathProbability(generated_data,GB_probMatrix,sim)-movAvg)^2
  mse_GB <- sum(mse_GB_mat[(half_index+1):end_index])/(end_index-half_index)
  # GB_lik_mat <-aca_mle_lik(generated_data,alpha_GB,H=Hinit2,model=2,sim=2)
  # GB_lik <- (-1)*sum(GB_lik_mat[(half_index+1):end_index])
  return(list("mse" = mse_GB, "alpha" = alpha_GB))
}

gbAcaMse = function(Hinit2, generated_data, sim, half_index, end_index,movAvg){
  ACA_GB <- DEoptim(aca_negLogLik1,lower = 0, upper = 1, H = Hinit2, allpaths = generated_data[1:half_index,], model = 3, sim = sim, DEoptim.control(NP=10,F=0.8, CR = 0.9,trace = FALSE, itermax = 20))
  alpha_ACA_GB = ACA_GB$optim$bestmem[1]
  ## Compute MSE for Model2 using simulated data
  GB_ACA_probMatrix <- baseModels::getProbMatrix(generated_data, alpha_ACA_GB,H=Hinit2, sim, model=3)
  mse_GB_ACA_mat <- (pathProbability(generated_data,GB_ACA_probMatrix,sim) - movAvg)^2
  mse_GB_ACA <- sum(mse_GB_ACA_mat[(half_index+1):end_index])/(end_index-half_index)
  # ACA_GB_lik_mat <-aca_mle_lik(generated_data,alpha_ACA_GB,H=Hinit,model=3,sim=2)
  # ACA_GB_lik <- (-1)*sum(ACA_GB_lik_mat[(half_index+1):end_index])
  return(list("mse" = mse_GB_ACA, "alpha" = alpha_ACA_GB))
}


aca2Mse = function(Hinit2, generated_data, sim, half_index, end_index,movAvg){
  ACA2 <- DEoptim(aca_negLogLik1,lower = 0, upper = 1, H = Hinit2, allpaths = generated_data[1:half_index,], model = 4, sim = sim, DEoptim.control(NP=10,F=0.8, CR = 0.9,trace = FALSE, itermax = 20))
  alpha_ACA2 = ACA2$optim$bestmem[1]

  ACA2_probMatrix = Aca2::getProbMatrix(generated_data, alpha_ACA2,H=Hinit2,sim,model=4)
  mse_ACA2_mat = (pathProbability(generated_data,ACA2_probMatrix,sim) - movAvg)^2
  mse_ACA2 = sum(mse_ACA2_mat[(half_index+1):end_index])/(end_index-half_index)
  return(list("mse" = mse_ACA2, "alpha" = alpha_ACA2))
}

aca3Mse = function(Hinit2, generated_data, sim, half_index, end_index, movAvg){
  ACA3 <- DEoptim(aca_negLogLik1,lower = c(0,0), upper = c(1,1), H = Hinit2, allpaths = generated_data[1:half_index,], model = 5, sim = sim, DEoptim.control(NP=20,F=0.8, CR = 0.9,trace = FALSE, itermax = 50))
  alpha_ACA3 = ACA3$optim$bestmem[1]
  gamma_ACA3 = ACA3$optim$bestmem[2]
  
  ACA3_probMatrix = Aca3::getProbMatrix(generated_data, alpha_ACA3,gamma_ACA3, H=Hinit2, sim, model=5)
  mse_ACA3_mat = (pathProbability(generated_data,ACA3_probMatrix,sim) - movAvg)^2
  mse_ACA3 = sum(mse_ACA3_mat[(half_index+1):end_index])/(end_index-half_index)
  return(list("mse" = mse_ACA3, "alpha" = alpha_ACA3,"gamma" = gamma_ACA3))
}

aca_negLogLik1=function(par,Hinit, allpaths,model,sim) {
  
  alpha = par[1]

  if(model == 1 || model == 2 || model == 3){
    
    lik = baseModels::getPathLikelihood(allpaths, alpha, Hinit, sim, model)
  }else if(model == 4){
    
    lik = Aca2::getPathLikelihood(allpaths, alpha, Hinit, sim, model)
  }else if(model == 5){
    
    gamma = par[2]
    lik = Aca3::getPathLikelihood(allpaths, alpha,gamma, Hinit, sim, model)
  }
  
  negLogLik = (-1) *sum(lik)
  if(is.infinite(negLogLik)){
    return(1000000)
  }else{
    return(negLogLik)
  }
  
}


modelCompare = function(generated_data, sim){
  
  start_index = getStartIndex(generated_data)
  end_index = getEndIndex(generated_data)
  if(start_index >= end_index){
    print(sprintf("start_index >= end_index , quitting"))
    return()
  }
  half_index = start_index
  Hinit1 <-genInitValues(generated_data,sim=sim)
  movAvg1 <- getMovingAverage(generated_data,window=20,sim=sim)
  
  acamse = acaMse(Hinit1, generated_data, sim=sim, half_index, end_index,movAvg1)
  gbmse = gbMse(Hinit1, generated_data, sim=sim, half_index, end_index,movAvg1)
  #gbacamse = gbAcaMse(Hinit1, generated_data, sim=1, half_index, end_index,movAvg1)
  gbacamse=list()
  #aca2mse = aca2Mse(Hinit1, generated_data, sim=sim, half_index, end_index,movAvg1)
  aca2mse = list()
  aca3mse = aca3Mse(Hinit1, generated_data, sim=sim, half_index, end_index,movAvg1)
  
  return(list("acamse"=acamse,"gbmse"=gbmse,"gbacamse"=gbacamse, "aca2mse"=aca2mse, "aca3mse"=aca3mse))
  
}

boxplotMse = function(mat_res, model,rat){
  
  if(model == 1){
    jpeg(paste("boxplot_ACA_",rat,".jpeg",sep=""))
  }else if(model == 2){
    jpeg(paste("boxplot_GB_",rat,".jpeg",sep=""))
  }else if(model == 3){
    jpeg(paste("boxplot_GB_ACA_",rat,".jpeg",sep=""))
  }else if(model == 4){
    jpeg(paste("boxplot_ACA2_",rat,".jpeg",sep=""))
  }else if(model == 5){
    jpeg(paste("boxplot_ACA3_",rat,".jpeg",sep=""))
  }
  
  boxplot(as.numeric(mat_res[,1]),as.numeric(mat_res[,3]),xaxt="n")
  axis(side=1, at=c(1,2), labels = c("ACA","GB"))
  dev.off()
  
  ret = TRUE 
  if(model==1){
    if(length(which(mat_res[,7]=="ACA")) < 70){
      print(sprintf("ACA is selected less than 70 times for %s. Exiting validation",rat))
      ret = FALSE
    }else{
      print(sprintf("ACA is selected more than 70 times for %s.",rat))
    }
  }
  else if(model==2){
    if(length(which(mat_res[,7]=="GB")) < 70){
      print(sprintf("GB is selected less than 70 times for %s. Exiting validation",rat))
      ret = FALSE
    }else{
      print(sprintf("GB is selected more than 70 times for %s.",rat))
    }
  }
  else if(model==3){
    if(length(which(mat_res[,7]=="ACA_GB")) < 70){
      print(sprintf("GB_ACA is selected less than 70 times for %s. Exiting validation",rat))
      ret = FALSE
    }else{
      print(sprintf("GB_ACA is selected more than 70 times for %s.",rat))
    }
  }
  else if(model==4){
    if(length(which(mat_res[,7]=="ACA2")) < 70){
      print(sprintf("ACA2 is selected less than 70 times for %s. Exiting validation",rat))
      ret = FALSE
    }else{
      print(sprintf("ACA2 is selected more than 70 times for %s.",rat))
    }
  }
  else if(model==5){
    if(length(which(mat_res[,7]=="ACA3")) < 70){
      print(sprintf("ACA3 is selected less than 70 times for %s. Exiting validation",rat))
      ret = FALSE
    }else{
      print(sprintf("ACA3 is selected more than 70 times for %s.",rat))
    }
  }
  return(ret)
}
