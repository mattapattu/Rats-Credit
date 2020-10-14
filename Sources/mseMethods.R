library(baseModels)



getMse1 = function(Actions,half_index, end_index){
  mse = 0
  for(i in c(1:12)){
    action = Actions[[i]]
    mse = mse + sum(action@mse1[(half_index+1):end_index])
  }
  return(mse)
}

getActivityErr = function(Actions, half_index, end_index){
  mse = 0
  for(i in c(4,10)){
    action = Actions[[i]]
    mse = mse + sum(action@activityErr[(half_index+1):end_index])
  }
  mse = abs(mse)
  return(mse)
}

getActionData = function(generated_data, model_probMatrix, start_index, end_index, window,  sim){
  
  empProbMat = baseModels::empiricalProbMat(generated_data, window = window)
  state2_idx = vector()
  actions = c(1:6)
  states = c(1,2)

  
  Actions = list()
  for(state in c(1,2)){
    
    if(sim == 1){
      #state_idx = which(generated_data[(start_index+1):end_index,2] == (state-1))
      state_idx = which(generated_data[,2] == (state-1))
    }else{
      #state_idx = which(generated_data[(start_index+1):end_index,2] == state)
      state_idx = which(generated_data[,2] == state)
    }
    
    for(act in c(1:6)){
      
      activityErr = vector()
      
      probVector =  model_probMatrix[state_idx,(6*(state-1)+act)]
      empProbVector = empProbMat[state_idx,(6*(state-1)+act)]
      
      modelActivity = abs(diff(probVector))
      # if(sum(modelActivity) !=0 ){
      #   modelActivity = modelActivity/mean(modelActivity)
      # }
      
      empActivity = abs(diff(empProbVector))
      # if(sum(empActivity) !=0 ){
      #   empActivity = empActivity/mean(empActivity)
      # }
      
      activityErr = (modelActivity - empActivity )
      
      action <- new("Action", action = (6*(state-1)+act), empActivity = empActivity, modelActivity = modelActivity)
      Actions = append(Actions,action)
      
     
      
    }
    
  }
    
 return(Actions)
  
}