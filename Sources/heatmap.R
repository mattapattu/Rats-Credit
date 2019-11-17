plot.heatmap=function(enreg){
  #plot for all rewarded e/i visit trials
  #plot for all unrewarded e/i visit trials
  #plot for all rewarded e visit trials 
  #plot for all rewarded i visit trials
  
  ###spikes 1s before reward, Reward, 1s after reward
  
  for(ses in c(1)){

    last_trial <- as.numeric(enreg[[ses]]$POS[length(enreg[[ses]]$POS[,1]),"trial"])
    
    mat <-matrix(0, last_trial, 11)
    colnames(mat) <- c("a","b","c","d","e","f","g","h","i","j","k")
    trialIndex =1
    pos <- enreg[[ses]]$POS
    
    for(t in 1:last_trial){
      
      a <- which(enreg[[ses]]$POS[,"trial"] == t & enreg[[ses]]$POS[,"boxname"]== "a")
      time_a_0 = as.numeric(enreg[[ses]]$POS[a[length(a)],1]) - as.numeric(enreg[[ses]]$POS[a[1],1])
      b <- which(enreg[[ses]]$POS[,"trial"] == t & enreg[[ses]]$POS[,"boxname"]== "b")
      time_b_0 = as.numeric(enreg[[ses]]$POS[b[length(b)],1]) - as.numeric(enreg[[ses]]$POS[b[1],1])
      c <- which(enreg[[ses]]$POS[,"trial"] == t & enreg[[ses]]$POS[,"boxname"]== "c")
      time_c_0 = as.numeric(enreg[[ses]]$POS[c[length(c)],1]) - as.numeric(enreg[[ses]]$POS[c[1],1])
      d <- which(enreg[[ses]]$POS[,"trial"] == t & enreg[[ses]]$POS[,"boxname"]== "d")
      time_d_0 = as.numeric(enreg[[ses]]$POS[d[length(d)],1]) - as.numeric(enreg[[ses]]$POS[d[1],1])
      e <- which(enreg[[ses]]$POS[,"trial"] == t & enreg[[ses]]$POS[,"boxname"]== "e")
      time_e_0 = as.numeric(enreg[[ses]]$POS[e[length(e)],1]) - as.numeric(enreg[[ses]]$POS[e[1],1])
      f <- which(enreg[[ses]]$POS[,"trial"] == t & enreg[[ses]]$POS[,"boxname"]== "f")
      time_f_0 = as.numeric(enreg[[ses]]$POS[f[length(f)],1]) - as.numeric(enreg[[ses]]$POS[f[1],1])
      g <- which(enreg[[ses]]$POS[,"trial"] == t & enreg[[ses]]$POS[,"boxname"]== "g")
      time_g_0 = as.numeric(enreg[[ses]]$POS[g[length(g)],1]) - as.numeric(enreg[[ses]]$POS[g[1],1])
      h <- which(enreg[[ses]]$POS[,"trial"] == t & enreg[[ses]]$POS[,"boxname"]== "h")
      time_h_0 = as.numeric(enreg[[ses]]$POS[h[length(h)],1]) - as.numeric(enreg[[ses]]$POS[h[1],1])
      i <- which(enreg[[ses]]$POS[,"trial"] == t & enreg[[ses]]$POS[,"boxname"]== "i")
      time_i_0 = as.numeric(enreg[[ses]]$POS[i[length(i)],1]) - as.numeric(enreg[[ses]]$POS[i[1],1])
      j <- which(enreg[[ses]]$POS[,"trial"] == t & enreg[[ses]]$POS[,"boxname"]== "j")
      time_j_0 = as.numeric(enreg[[ses]]$POS[j[length(j)],1]) - as.numeric(enreg[[ses]]$POS[j[1],1])
      k <- which(enreg[[ses]]$POS[,"trial"] == t & enreg[[ses]]$POS[,"boxname"]== "k")
      time_k_0 = as.numeric(enreg[[ses]]$POS[k[length(k)],1]) - as.numeric(enreg[[ses]]$POS[k[1],1])
      
      if(length(time_a_0) >0) {
        mat[trialIndex,1] =length(enreg[[ses]]$SPIKES[which( enreg[[ses]]$SPIKES[,"trial"]== t & enreg[[ses]]$SPIKES[,"neuron"]== "1" & enreg[[ses]]$SPIKES[,"boxName"]== "a"),1])*1000/time_a_0
      }
      if(length(time_b_0) >0) {
        mat[trialIndex,2] =length(enreg[[ses]]$SPIKES[ enreg[[ses]]$SPIKES[,"trial"]== t & enreg[[ses]]$SPIKES[,"neuron"]== "1" & enreg[[ses]]$SPIKES[,"boxName"]== "b",1])*1000/time_b_0
      }
      if(length(time_c_0) >0) {
        mat[trialIndex,3] =length(enreg[[ses]]$SPIKES[ enreg[[ses]]$SPIKES[,"trial"]== t & enreg[[ses]]$SPIKES[,"neuron"]== "1" & enreg[[ses]]$SPIKES[,"boxName"]== "c",1])*1000/time_c_0
      }
      if(length(time_d_0) >0) {
        mat[trialIndex,4] =length(enreg[[ses]]$SPIKES[ enreg[[ses]]$SPIKES[,"trial"]== t & enreg[[ses]]$SPIKES[,"neuron"]== "1" & enreg[[ses]]$SPIKES[,"boxName"]== "d",1])*1000/time_d_0
      }
      if(length(time_e_0) >0) {
        mat[trialIndex,5] =length(enreg[[ses]]$SPIKES[ enreg[[ses]]$SPIKES[,"trial"]== t & enreg[[ses]]$SPIKES[,"neuron"]== "1" & enreg[[ses]]$SPIKES[,"boxName"]== "e",1])*1000/time_e_0
      }
      if(length(time_f_0) >0) {
        mat[trialIndex,6] =length(enreg[[ses]]$SPIKES[ enreg[[ses]]$SPIKES[,"trial"]== t & enreg[[ses]]$SPIKES[,"neuron"]== "1" & enreg[[ses]]$SPIKES[,"boxName"]== "f",1])*1000/time_f_0
      }
      if(length(time_g_0) >0) {
        mat[trialIndex,7] =length(enreg[[ses]]$SPIKES[ enreg[[ses]]$SPIKES[,"trial"]== t & enreg[[ses]]$SPIKES[,"neuron"]== "1" & enreg[[ses]]$SPIKES[,"boxName"]== "g",1])*1000/time_g_0
      }
      if(length(time_h_0) >0) {
        mat[trialIndex,8] =length(enreg[[ses]]$SPIKES[ enreg[[ses]]$SPIKES[,"trial"]== t & enreg[[ses]]$SPIKES[,"neuron"]== "1" & enreg[[ses]]$SPIKES[,"boxName"]== "h",1])*1000/time_h_0
      }
      if(length(time_i_0) >0) {
        mat[trialIndex,9] =length(enreg[[ses]]$SPIKES[ enreg[[ses]]$SPIKES[,"trial"]== t & enreg[[ses]]$SPIKES[,"neuron"]== "1" & enreg[[ses]]$SPIKES[,"boxName"]== "i",1])*1000/time_i_0
      }
      if(length(time_j_0) >0) {
        mat[trialIndex,10] =length(enreg[[ses]]$SPIKES[ enreg[[ses]]$SPIKES[,"trial"]== t & enreg[[ses]]$SPIKES[,"neuron"]== "1" & enreg[[ses]]$SPIKES[,"boxName"]== "j",1])*1000/time_j_0
      }
      if(length(time_k_0) >0) {
        mat[trialIndex,11] =length(enreg[[ses]]$SPIKES[ enreg[[ses]]$SPIKES[,"trial"]== t & enreg[[ses]]$SPIKES[,"neuron"]== "1" & enreg[[ses]]$SPIKES[,"boxName"]== "k",1])*1000/time_k_0
      }

      trialIndex = trialIndex+1
    }
    
    library(ggplot2)
    
    longData<-melt(mat)
    longData<-longData[longData$value!=0,]
    ggplot(longData, aes(x = Var2, y = Var1)) + 
      geom_raster(aes(fill=value)) + 
      scale_fill_gradient(low="grey90", high="red") +
      labs(x="Boxes", y="Trials", title="Matrix") +
      theme_bw() + theme(axis.text.x=element_text(size=9, angle=0, vjust=0.3),
                         axis.text.y=element_text(size=9),
                         plot.title=element_text(size=11))
    
  } 
  
  print("Returning from plot")
}