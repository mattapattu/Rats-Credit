library(Rmpfr)
library(DEoptim)
library(TTR)
library(baseModels)
library(Aca2)
library(Aca3)

source("utils.R")
source("models.R")
source("mle_aca.R")


### Use optimal paramters on actual data and compute Mean Squared Error.
compareModels=function(enreg,rat){
  
  allpaths = enregCombine(enreg)
  allpaths = updateACAPathNbmse(allpaths)
  print(sprintf("rat:%s",rat))

  enreg_comb<-matrix(, nrow = 0, ncol = 7)
  for(ses in 1:length(enreg)){
    
    if(is.null(enreg[[ses]])){
      #print(sprintf("skipping %s ses %i as enreg is empty",rat,ses))
      next
    }else if(isempty(enreg[[ses]]$EVENTS)){
      #print(sprintf("skipping %s ses %i as reward data is empty",rat,ses))
      next
    }else if(rat=="rat_106" && ses==3){
      #print(sprintf("skipping %s ses %i as enreg is not good",rat,ses))
      next
      
    }else if(rat=="rat_112" && ses==1){
      #print(sprintf("skipping %s ses %i as enreg is not good",rat,ses))
      next
      
    }else if(rat=="rat_113" && ses==13){
      #print(sprintf("skipping %s ses %i as enreg is not good",rat,ses))
      next
      
    }
    enreg_comb<-rbind(enreg_comb,enreg[[ses]]$POS)
  }
  l<-cbind(as.numeric(enreg_comb[, 1]),as.numeric(enreg_comb[, 6]),as.numeric(enreg_comb[, 7]) )
  
  y<-updateTrialTimes(as.numeric(allpaths[,2]),l)
  allpaths<-cbind(allpaths,y)
  allpaths_num <- matrix(as.numeric(unlist(allpaths[,c(3,5,4,6,2)])),nrow=nrow(allpaths[,c(3,5,4,6,2)]))
  ################Call ACA##########################

  endLearningStage = getEndIndex(allpaths_num)

  Hinit = genInitValues(allpaths_num,sim=2)
  #Hinit <- matrix(0,2,6)

  Models = list("ACA" = 1, "GB" = 2, "GB-ACA" = 3, "ACA2" = 4, "ACA3" = 5)
  validated = TRUE
  alpha=0
  gamma=0
  
  for(model in c(5,1,2)){
    
    if(model == 5){
      out = DEoptim(aca_negLogLik1, lower = c(0,0), upper = c(1,1), H=Hinit, allpaths = allpaths_num[1:endLearningStage,],  model = model, sim=2, DEoptim.control(NP = 20,F = 0.8, CR = 0.9, trace = FALSE, itermax =100))
      alpha = out$optim$bestmem[1]
      gamma = out$optim$bestmem[2]
    }else{
      out = DEoptim(aca_negLogLik1, lower = 0, upper = 1, H=Hinit, allpaths = allpaths_num[1:endLearningStage,],  model = model, sim=2, DEoptim.control(NP = 10,F = 0.8, CR = 0.9, trace = FALSE, itermax = 20))
      alpha = out$optim$bestmem[1]
    }
    
    mat_res = matrix(0,nrow=100,ncol=7)
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
    

      if(length(which(SMA(generated_data[,3],10) >= 0.9)) < 500){
        missedOptimalIter=missedOptimalIter+1
        next
      }

      res = modelCompare(generated_data,sim=1)
      
      mat_res[iter,1]=res$acamse$mse
      mat_res[iter,2]=res$acamse$alpha
      mat_res[iter,3]=res$gbmse$mse
      mat_res[iter,4]=res$gbmse$alpha
      mat_res[iter,5]=res$aca3mse$mse
      mat_res[iter,6]=res$aca3mse$alpha
      

      index_min=which.min(c(mat_res[iter,1],mat_res[iter,3],mat_res[iter,5]))
      if(index_min==1){
        mat_res[iter,7]="ACA"
      }
      else if(index_min==2){
        mat_res[iter,7]="GB"
        break
      }
      else if(index_min==3){
        mat_res[iter,7]="ACA3"
      }

      iter=iter+1
    }
    
    print(sprintf("Nb of iterations where optimal behaviour was not learned=%i",missedOptimalIter))

    if(!boxplotMse(mat_res,model,rat)){
      validated = FALSE
      break
    }

  }
  
  if(validated){
    print(sprintf("All 3  models validated for %s.",rat)) 
  }else{
    print(sprintf("Model %d  failed validation for %s.",model, rat))
    return(mat_res)
  }
  

 
  
  ##### Model Selection GB vd GB_ACA on Acutal Data #########################3
  
  generated_data <- allpaths_num
  
  res = modelCompare(generated_data, sim=2)
  
  print(sprintf("For %s, ACA MSE = %f, GB MSE = %f, ACA2 MSE = %f", rat, res$acamse$mse, res$gbmse$mse, res$aca2mse$mse))

  index_min=which.min(c(res$acamse$mse,res$gbmse$mse,res$aca2mse$mse))

  if(index_min==1){
    print(sprintf("ACA is best fit for %s", rat))
  }else if(index_min==2){
    print(sprintf("GB is best fit for %s", rat))
   }
  else if(index_min==3){
    print(sprintf("ACA2 is best fit for %s", rat))
  }
  # 
  generatePlots(rat,allpaths_num,ACA_probMatrix, GB_probMatrix)
}


