library(baseModels)

mse_activity = function(generated_data, model_probMatrix, start_index, end_index, sim){
  
  empProbMat = baseModels::empiricalProbMat(generated_data, window = 5)
  state2_idx = vector()
  actions = c(1:6)
  states = c(1,2)

  mseErr = 0
  activityErr = 0
  for(state in c(1,2)){
    
    if(sim == 1){
      state_idx = which(generated_data[(start_index+1):end_index,2] == (state-1))
    }else{
      state_idx = which(generated_data[(start_index+1):end_index,2] == state)
    }
   
    for(act in c(1:12)){
      probVector =  model_probMatrix[state_idx,act]
      empProbVector = empProbMat[state_idx,act]
      
      mseErr = mseErr + sum(abs(probVector - empProbVector))
      activityErr = activityErr + abs(computeActivityOfCurve(probVector, start_index, end_index) - computeActivityOfCurve(empProbVector, start_index, end_index))
    }
  }
    
 return(mseErr * activityErr)
  
}