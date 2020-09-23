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
comparePathModels=function(enreg,rat){
  
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
  y = getTrialTimes(as.numeric(allpaths[,2]),l)
  allpaths = cbind(allpaths,y)
  allpaths_num = matrix(as.numeric(unlist(allpaths[,c(3,5,4,6,2)])),nrow=nrow(allpaths[,c(3,5,4,6,2)]))

  #### Holdout Validation ########################################
  endLearningStage = getEndIndex(allpaths_num)
  Hinit = genInitValues(allpaths_num,sim=2)
  #Hinit <- matrix(0,2,6)

  Models = list("ACA" = 1, "GB" = 2, "GB-ACA" = 3, "ACA2" = 4, "ACA3" = 5)
  models = c(1,2,4,5)
  #mat_res = validateHoldout(models,Hinit,endLearningStage,allpaths_num)

  ##### Model Selection On Acutal Data #########################3
  
  generated_data <- allpaths_num
  
  #mat_res = windowCompare(generated_data,models, sim=2)
  
  res = modelCompare(generated_data, models, window = 5, sim=2)

  min_index = 0
  min = 100000
  for(m in models){
    print(sprintf("%s: %s mse = %f", rat, res[[m]]$model, res[[m]]$mse))
    
    if(res[[m]]$mse < min){
      min = res[[m]]$mse
      min_index = m
    }
  }
  print(sprintf("%s is best fit for %s", res[[min_index]]$model, rat))
  
  generatePlots(rat,allpaths_num,res$acamse$probMatrix, res$gbmse$probMatrix, res$aca2mse$probMatrix, res$aca3mse$probMatrix)
}


