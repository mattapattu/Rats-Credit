setClass("Action", 
         slots = list(
           action="numeric",
           empActivity = "vector",
           modelActivity = "vector")
          )


setClass("Model", 
         slots = list(
           Name = "character", 
           Params_lik = "list",
           Metrics ="list",
           ProbMatrix = "matrix")
           
)




getTurnModelData = function(generated_data, turnTimes, models, window, sim){
  
  #start_index = getStartIndex(generated_data)
  end_index = getEndIndex(generated_data, sim)
  start_index = round(end_index/2)
  if(start_index >= end_index){
    print(sprintf("start_index >= end_index. Check if rat learns optimal behavior"))
    return()
  }
  
  
  end_index = 0
  #end_index = length(generated_data[,1])
  #Hinit1 <-genInitValues(generated_data,sim=sim)
  #Qinit <-genInitValues(generated_data,sim=sim)
  #empProbMat <- getEmpProbMat(generated_data,window=window,sim=sim)
  
  

  acaTurn = list()
  gbTurn = list()
  gbacamse = list()
  aca2Turn = list()
  aca3Turn = list()
  sarsaTurn = list()
  
  if("acaTurns" %in% models){  
    #debug(acaTurnData)
    acaTurn = acaTurnData(generated_data, turnTimes, turnMethod= 0, sim=sim, start_index, end_index, window)
    # acaTurn1 = acaTurnData(generated_data, turnTimes, turnMethod= 1, sim=sim, start_index, end_index, window)
    # acaTurn2 = acaTurnData(generated_data, turnTimes, turnMethod= 2, sim=sim, start_index, end_index, window)
    # 
    # min.idx = which.min(c(acaTurn0@Metrics[[2]],acaTurn1@Metrics[[2]],acaTurn2@Metrics[[2]]))
    # print(sprintf("ACA turnMethod=%i has max likelihood", min.idx-1))
    #  if(min.idx == 1){
    #   acaTurn = acaTurn0
    # }else if(min.idx == 2){
    #   acaTurn = acaTurn1
    # }else if(min.idx == 3){
    #   acaTurn = acaTurn2
    # }
    
  }
  if("gbTurns" %in% models){    
    gbTurn = gbTurnData(generated_data, turnTimes, turnMethod= 0, sim=sim, start_index, end_index, window)
  }
  if("aca2Turns" %in% models){        
    #debug(aca3TurnData)
    aca2Turn = aca2TurnData(generated_data, turnTimes, turnMethod = 0, sim=sim, start_index, end_index, window)
  }
  if("aca3Turns" %in% models){        
    debug(aca3TurnData)
    aca3Turn = aca3TurnData(generated_data, turnTimes, turnMethod = 0, sim=sim, start_index, end_index, window)
    # aca3Turn1 = aca3TurnData(generated_data, turnTimes, turnMethod = 1, sim=sim, start_index, end_index, window)
    # aca3Turn2 = aca3TurnData(generated_data, turnTimes, turnMethod = 2, sim=sim, start_index, end_index, window)
    # 
    # min.idx = which.min(c(aca3Turn0@Metrics[[2]],aca3Turn1@Metrics[[2]],aca3Turn2@Metrics[[2]]))
    # print(sprintf("ACA3 turnMethod=%i has max likelihood", min.idx-1))
    # if(min.idx == 1){
    #   aca3Turn = aca3Turn0
    # }else if(min.idx == 2){
    #   aca3Turn = aca3Turn1
    # }else if(min.idx == 3){
    #   aca3Turn = aca3Turn2
    # }
  }
  if("sarsaTurns" %in% models){
    #debug(sarsaTurnData)
    sarsaTurn = sarsaTurnData(generated_data,turnTimes, sim=sim, start_index, end_index, window)
  }
  
  
  return(list("acaTurnData"=acaTurn,"gbTurnData"=gbTurn,"gbacamse"=gbacamse, "aca2TurnData"=aca2Turn, "aca3TurnData"=aca3Turn, "sarsaTurnData"=sarsaTurn))
  
}




