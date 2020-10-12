library(Rmpfr)
library(DEoptim)
library(TTR)
library(baseModels)
library(Aca2)
library(Aca3)

### Use optimal paramters on actual data and compute Mean Squared Error.
comparePathModels=function(enreg,rat, window){
  
  allpaths = enregCombine(enreg, rat)
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
  
  l = cbind(as.numeric(enreg_comb[, 1]),as.numeric(enreg_comb[, 6]),as.numeric(enreg_comb[, 7]) )
  y = baseModels::getTrialTimes(as.numeric(allpaths[,2]),l)
  allpaths = cbind(allpaths,y)
  allpaths_num = matrix(as.numeric(unlist(allpaths[,c(3,5,4,6,2)])),nrow=nrow(allpaths[,c(3,5,4,6,2)]))

  
  #### Holdout Validation ########################################
  endLearningStage = getEndIndex(allpaths_num)
  Hinit = genInitValues(allpaths_num,sim=2)
  #Hinit <- matrix(0,2,6)

  Models = list("ACA" = 1, "GB" = 2, "GB-ACA" = 3, "ACA2" = 4, "ACA3" = 5)
  models = c(1,2,4,5)
  #mat_res = validateHoldout(models,Hinit,endLearningStage,allpaths_num, window = window)

  ##### Model Selection On Acutal Data #########################3
  
  generated_data <- allpaths_num
  #mat_res = windowCompare(generated_data,models, sim=2)
  res = getModelData(generated_data, models, window = window, sim=2)

  # min_index = 0
  # min = 100000
  # for(m in models){
  #   print(sprintf("%s: %s likelihood = %f", rat, res[[m]]@Name, res[[m]]@Metrics$likelihood))
  #   #print(res[[m]]@Params_lik)
  #   
  # 
  #   if(res[[m]]@Metrics$likelihood < min){
  #     min = res[[m]]@Metrics$likelihood
  #     min_index = m
  #   }
  # }
  # print(sprintf("%s is best fit for %s", res[[min_index]]@Name, rat))
  
  min_index = 0
  min = 100000
  for(m in models){
    print(sprintf("%s: %s mse = %f", rat, res[[m]]@Name, res[[m]]@Metrics$activityErr))
    #print(res[[m]]@Params_activity)
    if(res[[m]]@Metrics$activityErr < min){
      min = res[[m]]@Metrics$activityErr
      min_index = m
    }
  }
  print(sprintf("%s is best fit for %s", res[[min_index]]@Name, rat))
  save(res, file = paste0(rat,"_res.Rdata"))
  
  ranges = getTestRange(generated_data)
  #debug(plotData)
  plotData2(res,rat,ranges)
  #empProbMat = baseModels::empiricalProbMat(generated_data, window = window)
  #generatePlots(rat,empProbMat, res$acamse@ProbMatrix, res$gbmse@ProbMatrix, res$aca2mse@ProbMatrix, res$aca3mse@ProbMatrix)
}

getTestRange=function(generated_data){
  
  end_index = getEndIndex(generated_data)
  start_index = round(end_index/2)
  
  idx  = which(generated_data[1:start_index,2]==1)
  start_state1 = length(idx)
  
  idx  = which(generated_data[1:start_index,2]==2)
  start_state2 = length(idx)
  
  
  idx  = which(generated_data[1:end_index,2]==1)
  end_state1 = length(idx)
  
  idx  = which(generated_data[1:end_index,2]==2)
  end_state2 = length(idx)
  
  state1 = list(start_index = start_state1, end_index = end_state1)
  state2 = list(start_index = start_state2, end_index = end_state2)
  
  return(list(state1= state1, state2 = state2))
  
}

