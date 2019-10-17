library(rlist)
library(data.table)
library(data.tree)


plot.spikes.by.boxes=function(ses,enreg){
  trial = 0
  spiketrain = list()
  print("HERE1")
  size =length(enreg[[ses]]$SPIKES[,"trial"])
  max_trial = enreg[[ses]]$POS[size,"trial"]
  spiketrain[[ses]]=list("trials"= c(1:max_trial))
  print("HERE2")
  for(idx in 1:max_trial){
    print("HERE3")
     spiketrain[[ses]]$trials[idx] = list(cbind("boxname" = c("a","b","c","d","e","f","g","h","i")))
     sub_enreg <- subset(enreg[[ses]]$SPIKES,enreg[[ses]]$SPIKES[,"trial"] == idx) 
     print("HERE4")
     for(idx in 1:length(sub_enreg[,1])){
       print("HERE5")
       if(sub_enreg[idx,"neuron"] != "0"){
         neuron_nb = sub_enreg[idx,"neuron"]
         bool = neuron_nb %in% colnames(spiketrain[trial])
         neuron = paste("Neuron", neuron_nb)
         print("HERE6")
         if(!bool){
           spiketrain[[ses]]$trials[idx] = list(cbind(spiketrain[[ses]]$trials[[idx]], neuron))
         }
         boxname = enreg[[ses]]$SPIKES[idx,"boxName"]
         spiketrain[[ses]]$trials[[idx]][1,neuron] =  spiketrain[[ses]]$trials[[idx]][1,neuron]
         print("HERE7")
       }
     }
  }
  print("HERE8")
  plot(spiketrain[[1]]$trials[[100]][,"boxname"], spiketrain[[1]]$trials[[100]][,"Neuron1"], pch=19, col="darkgreen", cex=1.5)
  lines(spiketrain[[1]]$trials[[100]][,"boxname"],spiketrain[[1]]$trials[[100]][,"Neuron2"], pch=19, col="blue", cex=1.5)
}