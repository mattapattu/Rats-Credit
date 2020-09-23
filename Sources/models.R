library(Rmpfr)
library(DEoptim)
library(TTR)



acaMse = function(Hinit2, generated_data, sim, half_index, end_index){
  ACA = DEoptim(aca_negLogLik1, lower = 0, upper = 1, H=Hinit2, allpaths = generated_data[1:half_index,], model=1, sim=sim, DEoptim.control(NP = 10,F = 0.8, CR = 0.9, trace = FALSE, itermax = 20))
  alpha_ACA = ACA$optim$bestmem[1]
  
  ACA_probMatrix = baseModels::getProbMatrix(generated_data, alpha_ACA,H=Hinit2,sim,model=1)
  mse_ACA = mse_activity(generated_data, ACA_probMatrix, half_index, end_index, sim)
  
  return(list("model" = "ACA", "mse" = mse_ACA, "alpha" = alpha_ACA, "probMatrix" = ACA_probMatrix))
}

gbMse = function(Hinit2, generated_data, sim, half_index, end_index){
  
  GB <- DEoptim(aca_negLogLik1,lower = 0, upper = 1, H = Hinit2, allpaths = generated_data[1:half_index,], model = 2, sim=sim, DEoptim.control(NP=10,F=0.8, CR = 0.9,trace = FALSE, itermax = 20))
  alpha_GB = GB$optim$bestmem[1]
  ## Compute MSE for Model2 using simulated data
  GB_probMatrix <- baseModels::getProbMatrix(generated_data, alpha_GB, H=Hinit2, sim, model=2)
  mse_GB = mse_activity(generated_data, GB_probMatrix, half_index, end_index, sim)
  
  return(list("model" = "GB", "mse" = mse_GB, "alpha" = alpha_GB, "probMatrix" = GB_probMatrix))
}

gbAcaMse = function(Hinit2, generated_data, sim, half_index, end_index){
  ACA_GB <- DEoptim(aca_negLogLik1,lower = 0, upper = 1, H = Hinit2, allpaths = generated_data[1:half_index,], model = 3, sim = sim, DEoptim.control(NP=10,F=0.8, CR = 0.9,trace = FALSE, itermax = 20))
  alpha_ACA_GB = ACA_GB$optim$bestmem[1]
  ## Compute MSE for Model2 using simulated data
  GB_ACA_probMatrix <- baseModels::getProbMatrix(generated_data, alpha_ACA_GB,H=Hinit2, sim, model=3)
  mse_GB_ACA = mse_activity(generated_data, GB_ACA_probMatrix, half_index, end_index, sim)
  return(list("model" = "GB-ACA", "mse" = mse_GB_ACA, "alpha" = alpha_ACA_GB, "probMatrix" = GB_ACA_probMatrix))
}


aca2Mse = function(Hinit2, generated_data, sim, half_index, end_index){
  ACA2 <- DEoptim(aca_negLogLik1,lower = 0, upper = 1, H = Hinit2, allpaths = generated_data[1:half_index,], model = 4, sim = sim, DEoptim.control(NP=10,F=0.8, CR = 0.9,trace = FALSE, itermax = 20))
  alpha_ACA2 = ACA2$optim$bestmem[1]

  ACA2_probMatrix = Aca2::getProbMatrix(generated_data, alpha_ACA2,H=Hinit2,sim,model=4)
  mse_ACA2 = mse_activity(generated_data, ACA2_probMatrix, half_index, end_index, sim)
  
  return(list("model" = "ACA2", "mse" = mse_ACA2, "alpha" = alpha_ACA2,"probMatrix" = ACA2_probMatrix))
}

aca3Mse = function(Hinit2, generated_data, sim, half_index, end_index){
  ACA3 <- DEoptim(aca_negLogLik1,lower = c(0,0), upper = c(1,1), H = Hinit2, allpaths = generated_data[1:half_index,], model = 5, sim = sim, DEoptim.control(NP=20,F=0.8, CR = 0.9,trace = FALSE, itermax = 50))
  alpha_ACA3 = ACA3$optim$bestmem[1]
  gamma_ACA3 = ACA3$optim$bestmem[2]
  
  ACA3_probMatrix = Aca3::getProbMatrix(generated_data, alpha_ACA3,gamma_ACA3, H=Hinit2, sim, model=5)
  mse_ACA3 = mse_activity(generated_data, ACA3_probMatrix, half_index, end_index, sim)
  return(list("model" = "ACA3" ,"mse" = mse_ACA3, "alpha" = alpha_ACA3,"gamma" = gamma_ACA3, "probMatrix" = ACA3_probMatrix))
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
 # print(sprintf("negLogLik = %f",negLogLik))
  if(is.infinite(negLogLik)){
    return(1000000)
  }else{
    return(negLogLik)
  }
  
}


modelCompare = function(generated_data, models, window,  sim){
  
  #start_index = getStartIndex(generated_data)
  end_index = getEndIndex(generated_data)
  start_index = round(end_index/2)
  if(start_index >= end_index){
    print(sprintf("start_index >= end_index. Check if rat learns optimal behavior"))
    return()
  }
  
  Hinit1 <-genInitValues(generated_data,sim=sim)
  #empProbMat <- getEmpProbMat(generated_data,window=window,sim=sim)
  
  acamse = list()
  gbmse = list()
  gbacamse = list()
  aca2mse = list()
  aca3mse = list()
  
  if(1 %in% models){
    acamse = acaMse(Hinit1, generated_data, sim=sim, start_index, end_index)
  }
  if(2 %in% models){
    gbmse = gbMse(Hinit1, generated_data, sim=sim, start_index, end_index)
  }
  if(3 %in% models){
    gbacamse = gbAcaMse(Hinit1, generated_data, sim=1, start_index, end_index)
  }
  if(4 %in% models){
    aca2mse = aca2Mse(Hinit1, generated_data, sim=sim, start_index, end_index)
  }
  if(5 %in% models){
    aca3mse = aca3Mse(Hinit1, generated_data, sim=sim, start_index, end_index)
  }
  
  
  return(list("acamse"=acamse,"gbmse"=gbmse,"gbacamse"=gbacamse, "aca2mse"=aca2mse, "aca3mse"=aca3mse))
  
}

validateHoldout=function(models,Hinit,endLearningStage,allpaths_num){
  validated = TRUE
  alpha=0
  gamma=0
  
  for(model in models){
    
    if(model == 5){
      out = DEoptim(aca_negLogLik1, lower = c(0,0), upper = c(1,1), H=Hinit, allpaths = allpaths_num[1:endLearningStage,],  model = model, sim=2, DEoptim.control(NP = 20,F = 0.8, CR = 0.9, trace = FALSE, itermax = 50))
      alpha = out$optim$bestmem[1]
      gamma = out$optim$bestmem[2]
    }else{
      out = DEoptim(aca_negLogLik1, lower = 0, upper = 1, H=Hinit, allpaths = allpaths_num[1:endLearningStage,],  model = model, sim=2, DEoptim.control(NP = 10,F = 0.8, CR = 0.9, trace = FALSE, itermax = 20))
      alpha = out$optim$bestmem[1]
    }
    
    mat_res = matrix(0,nrow=100,ncol=(2*length(models)+1))
    colnames(mat_res) = c("ACA MSE","ACA bestval","GB MSE","GB bestval","ACA2 MSE","ACA2 bestval","Selected Model")
    iter = 1
    start_index = 0
    end_index = 0
    missedOptimalIter = 0
    while(iter <= 100){
      total_trials = length(allpaths_num[,1])
      init_state = as.numeric(allpaths_num[1,2])-1
      
      if(model == 1 || model == 2 || model == 3){
        generated_data = baseModels::simulateTrials(allpaths_num, H=Hinit, alpha, total_trials, init_state, model=model)
      }else if(model == 4){
        generated_data = Aca2::simulateTrials(allpaths_num, H=Hinit, alpha, total_trials,init_state, model=model)
      }else if(model == 5){
        generated_data = Aca3::simulateTrials(allpaths_num, H=Hinit, alpha, gamma, total_trials, init_state, model=model)
      }
      
      
      # if(length(which(SMA(generated_data[,3],10) >= 0.9)) < 500){
      #   missedOptimalIter=missedOptimalIter+1
      #   next
      # }
      
      if(getStartIndex(generated_data) >= getEndIndex(generated_data)){
        missedOptimalIter=missedOptimalIter+1
        next
      }
      
      res = modelCompare(generated_data, models, window = 5, sim=1)
      
      min_index = 0
      min = 100000
      for(m in models){
        mat_res[iter,m]=res[[m]]$mse
        mat_res[iter,(m+1)]=res[[m]]$alpha
        if(res[[m]]$mse < min){
          min = res[[m]]$mse
          min_index = m
        }
      }
      
      mat_res[iter,(2*length(models)+1)]= res[[min_index]]$model
      iter=iter+1
    }
    
    print(sprintf("Nb of iterations where optimal behaviour was not learned=%i", missedOptimalIter))
    
    boxplotMse(mat_res,model,rat)
    
    if(!checkValidation(mat_res,model,rat)){
      validated = FALSE
      break
    }
    
  }
  
  if(validated){
    print(sprintf("All 3  models validated for %s.",rat)) 
  }else{
    print(sprintf("Model %d  failed validation for %s.",model, rat))
  }
  return(mat_res)
}


windowCompare=function(generated_data, models, sim){
  
  end_index = getEndIndex(generated_data)
  start_index = round(end_index/2)
  if(start_index >= end_index){
    print(sprintf("start_index >= end_index. Check if rat learns optimal behavior"))
    return()
  }
  
  Hinit1 = genInitValues(generated_data,sim=sim)
  
  mat_res = matrix(0,nrow=21,ncol=(length(models)+1))
  cols = vector()
  
  if(1 %in% models){
    cols = c(cols,"ACA")
  }
  if(2 %in% models){
    cols = c(cols,"GB")
  }
  if(3 %in% models){
    cols = c(cols,"GB-ACA")
  }
  if(4 %in% models){
    cols = c(cols,"ACA2")
  }
  if(5 %in% models){
    cols = c(cols,"ACA3")
  }
  
  colnames(mat_res) =  c("window",cols)
  
  iter=1
  
  for(window in c(2,seq(5, 100, by = 5))){
    
    mat_res[iter,1]=window
    
    empProbMat <- getEmpProbMat(generated_data,window=window,sim=sim)

    if(1 %in% models){
      acamse = acaMse(Hinit1, generated_data, sim=sim, start_index, end_index,empProbMat)
      mat_res[iter,"ACA"]=acamse$mse
    }
    if(2 %in% models){
      gbmse = gbMse(Hinit1, generated_data, sim=sim, start_index, end_index,empProbMat)
      mat_res[iter,"GB"]=gbmse$mse
    }
    if(3 %in% models){
      gbacamse = gbAcaMse(Hinit1, generated_data, sim=1, start_index, end_index,empProbMat)
      mat_res[iter,"GB-ACA"]=gbacamse$mse
    }
    if(4 %in% models){
      aca2mse = aca2Mse(Hinit1, generated_data, sim=sim, start_index, end_index,empProbMat)
      mat_res[iter,"ACA2"]=aca2mse$mse
    }
    if(5 %in% models){
      aca3mse = aca3Mse(Hinit1, generated_data, sim=sim, start_index, end_index,empProbMat)
      mat_res[iter,"ACA3"]=aca3mse$mse
    }
    
    iter=iter+1
    
  }
  
  plot(mat_res[1:21,3],type='l',xaxt = "n", xlab="Window size", ylab="MSE")
  axis(1, at=1:21, labels=mat_res[1:21,1])
  lines(mat_res[1:21,2],type='l',col='red')
  lines(mat_res[1:21,4],type='l',col='blue')
  lines(mat_res[1:21,4],type='l',col='green')
  lines(mat_res[1:21,4],type='l',col='blue')
  lines(mat_res[1:21,5],type='l',col='green')
  legend("topright", legend=c("MSE GB", "MSE ACA","MSE ACA2","MSE ACA3"),col=c("black","red","blue","green"),cex=0.6,lty = c(1,1,1,1))
  
  return(mat_res)
}