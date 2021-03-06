library(Rmpfr)
library(DEoptim)
library(TTR)
library(baseModels)
library(Aca2)
library(Aca3)
library(TurnsModels)

### Use optimal paramters on actual data and compute Mean Squared Error.
comparePathModels=function(enreg,rat, window,path){
  
  enregres = enregCombine(enreg, rat)
  allpaths = enregres$allpaths
  boxTimes = enregres$boxTimes
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
  
  enreg_box_times = cbind(as.numeric(enreg_comb[, 1]),as.numeric(enreg_comb[, 6]),as.numeric(enreg_comb[, 7]) )
  sessionIdVec = as.numeric(allpaths[,2])
  pathTimes = baseModels::getPathTimes(sessionIdVec,enreg_box_times)
  allpaths = cbind(allpaths,pathTimes)
  allpaths = cbind(allpaths,c(1:length(allpaths[,1])))
  allpaths_num = matrix(as.numeric(unlist(allpaths[,c(3,5,4,6,2)])),nrow=nrow(allpaths[,c(3,5,4,6,2)]))
  allpaths_num = cbind(allpaths_num,c(1:length(allpaths_num[,1])))
 
  # empprob2 = baseModels::empiricalProbMat2(allpaths_num,window)
  # endLearningStage = getEndIndex(allpaths_num,sim=2)
  # #debug(generateEmpiricalPlots)
  # generateEmpiricalPlots(rat, empprob2,endLearningStage)
  
  

  # #### Holdout Validation ########################################
  endLearningStage = getEndIndex(allpaths_num,sim=2)
  Hinit = genInitValues(allpaths_num,sim=2)
  #Hinit <- matrix(0,2,6)

  Models = list("ACA" = 1, "GB" = 2, "GB-ACA" = 3, "ACA2" = 4, "ACA3" = 5, "SARSA"=6)
  models = c("aca","gb","aca2","aca3","sarsa","acaTurns","gbTurns","aca2Turns","aca3Turns","sarsaTurns" )
  #models = c("sarsa","acaTurns","gbTurns","aca2Turns","aca3Turns","sarsaTurns")
  turnTimes = TurnsModels::getTurnTimes(allpaths,boxTimes,sim=2)
  debug(validateHoldout)
  mat_res = validateHoldout(models,Hinit=matrix(0,2,6),endLearningStage,allpaths_num,turnTimes, window = window, rat)
  #save(mat_res, file = paste0(rat,"_mat_res.Rdata"))

  # ##### Model Selection On Acutal Data #########################3

  generated_data <- allpaths_num
  #mat_res = windowCompare(generated_data,models, sim=2)
  #debug(getModelData)

  pathmodels=c("aca","gb","aca2","aca3","sarsa")
  turnmodels=c("acaTurns","gbTurns","aca2Turns","aca3Turns","sarsaTurns")
  #pathmodels=c("aca3")
  #turnmodels=c("aca3Turns")
  res1 = getModelData(generated_data, pathmodels, window = window, sim=2)
  debug(getTurnModelData)
  res2 = getTurnModelData(generated_data, turnTimes, turnmodels, window = window, sim=2)

  min_index = 0
  min = 100000
  min_method = "null"


  for(m in models)
  {
    if(m == "aca")
    {
      print(sprintf("%s: %s likelihood = %f", rat, "aca", res1$acamse@Metrics$likelihood))

      if(res1$acamse@Metrics$likelihood < min)
      {
        min = res1$acamse@Metrics$likelihood
        min_method = m
      }

    }
    else if(m == "gb")
    {
      print(sprintf("%s: %s likelihood = %f", rat, "gb", res1$gbmse@Metrics$likelihood))
      if(res1$gbmse@Metrics$likelihood < min)
      {
        min = res1$gbmse@Metrics$likelihood
        min_method = m
      }

    }
    else if(m == "aca2")
    {
      print(sprintf("%s: %s likelihood = %f", rat, "aca2", res1$aca2mse@Metrics$likelihood))
      if(res1$aca2mse@Metrics$likelihood < min)
      {
        min = res1$aca2mse@Metrics$likelihood
        min_method = m
      }

    }
    else if(m == "aca3")
    {
      print(sprintf("%s: %s likelihood = %f", rat, "aca3", res1$aca3@Metrics$likelihood))
      if(res1$aca3mse@Metrics$likelihood < min)
      {
        min = res1$aca3mse@Metrics$likelihood
        min_method = m
      }

    }
    else if(m == "sarsa")
    {
      print(sprintf("%s: %s likelihood = %f", rat, "sarsa", res1$sarsa@Metrics$likelihood))
      if(res1$sarsamse@Metrics$likelihood < min)
      {
        min = res1$sarsamse@Metrics$likelihood
        min_method = m
      }

    }
    else if(m == "acaTurns")
    {
      print(sprintf("%s: %s likelihood = %f", rat, "acaTurns", res2$acaTurnData@Metrics$likelihood))
      if(res2$acaTurnData@Metrics$likelihood < min)
      {
        min = res2$acaTurnData@Metrics$likelihood
        min_method = m
      }

    }
    else if(m == "gbTurns")
    {
      print(sprintf("%s: %s likelihood = %f", rat, "gbTurns", res2$gbTurnData@Metrics$likelihood))
      if(res2$gbTurnData@Metrics$likelihood < min)
      {
        min = res2$gbTurnData@Metrics$likelihood
        min_method = m
      }

    }
    else if(m == "aca2Turns")
    {
      print(sprintf("%s: %s likelihood = %f", rat, "aca2Turns", res2$aca2TurnData@Metrics$likelihood))
      if(res2$aca2TurnData@Metrics$likelihood < min)
      {
        min = res2$aca2TurnData@Metrics$likelihood
        min_method = m
      }

    }
    else if(m == "aca3Turns")
    {
      print(sprintf("%s: %s likelihood = %f", rat, "aca3Turns", res2$aca3TurnData@Metrics$likelihood))
      if(res2$aca3TurnData@Metrics$likelihood < min)
      {
        min = res2$aca3TurnData@Metrics$likelihood
        min_method = m
      }

    }
    else if(m == "sarsaTurns")
    {
      print(sprintf("%s: %s likelihood = %f", rat, "sarsaTurns", res2$sarsaTurnData@Metrics$likelihood))
      if(res2$sarsaTurnData@Metrics$likelihood < min)
      {
        min = res2$sarsaTurnData@Metrics$likelihood
        min_method = m
      }

    }
  }

  print(sprintf("%s is best fit for %s", min_method, rat))

  save(res1,res2, file = paste0(rat,"_results.Rdata"))
  #
  # #ranges = getTestRange(generated_data)
  # #debug(plotData)
  # #plotData2(res,rat,ranges)
  # #empProbMat = baseModels::empiricalProbMat(generated_data, window = window)
  # debug(generatePlots)
  # generatePlots(rat,window, res$acamse@ProbMatrix, res$gbmse@ProbMatrix, res$sarsa@ProbMatrix, res$aca3mse@ProbMatrix, allpaths_num)
  #generatePlots(rat,window, res$acamse@ProbMatrix, models, res1, res2, allpaths_num)
 
  #load(paste0("C:/Rats-Credits/",rat,"_results.RData"))
  #debug(generateModelProbPlots)
  #sgenerateModelProbPlots(rat,window,res1,res2,c("aca3","aca3Turns"),allpaths_num)
  #debug(generateModelProbPlots2)

  #generateModelProbPlots2(rat, window, res1, res2,models, allpaths_num,path)
}



