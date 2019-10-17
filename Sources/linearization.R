library(rlist)
library(data.table)
library(data.tree)


plot.spikes.by.boxes=function(ses,enreg){
  trial = 0
  spiketrain = list()
  print("HERE1")
  size =length(enreg[[ses]]$SPIKES[,1])
  max_trial = enreg[[ses]]$POS[size,"trial"]
  spiketrain[[ses]]=list("trials"= c(1:max_trial))
  print("HERE2")
  for(idx in 1:max_trial){
    print("HERE3")
     spiketrain[[ses]]$trials[idx] = list("boxname" = list("a","b","c","d","e","f","g","h","i"))
     sub_enreg <- subset(enreg[[ses]]$SPIKES,enreg[[ses]]$SPIKES[,"trial"] == idx) 
     sub_enreg <- subset(sub_enreg,sub_enreg[,"neuron"] != 0)
     print("HERE4")
     spiketrain[[ses]]$trials[idx] = list(cbind(spiketrain[[ses]]$trials[[idx]], Neuron1="0",Neuron2="0",Neuron3="0",Neuron4="0",Neuron5="0",Neuron6="0",Neuron7="0",Neuron8="0",Neuron9="0",Neuron10="0",Sum="0"))
     for(sp in 1:length(sub_enreg[,1])){
       print("HERE5")
       neuron_nb = sub_enreg[sp,"neuron"]
       neuron = paste("Neuron", neuron_nb,sep = "")
       boxname = sub_enreg[sp,"boxName"]
       if(boxname == "a"){
         spiketrain[[ses]]$trials[[idx]][1,neuron] =  as.numeric(spiketrain[[ses]]$trials[[idx]][1,neuron]) + 1
         spiketrain[[ses]]$trials[[idx]][9,"Sum"] = as.numeric(spiketrain[[ses]]$trials[[idx]][1,"Sum"])+1  
         
       }else if(boxname == "b"){
         spiketrain[[ses]]$trials[[idx]][2,neuron] =  as.numeric(spiketrain[[ses]]$trials[[idx]][2,neuron]) + 1
         spiketrain[[ses]]$trials[[idx]][9,"Sum"] = as.numeric(spiketrain[[ses]]$trials[[idx]][2,"Sum"])+1
         
       }else if(boxname == "c"){
         spiketrain[[ses]]$trials[[idx]][3,neuron] =  as.numeric(spiketrain[[ses]]$trials[[idx]][3,neuron]) + 1
         spiketrain[[ses]]$trials[[idx]][9,"Sum"] = as.numeric(spiketrain[[ses]]$trials[[idx]][3,"Sum"])+1
         
       }else if(boxname == "d"){
         spiketrain[[ses]]$trials[[idx]][4,neuron] =  as.numeric(spiketrain[[ses]]$trials[[idx]][4,neuron]) + 1
         spiketrain[[ses]]$trials[[idx]][9,"Sum"] = as.numeric(spiketrain[[ses]]$trials[[idx]][4,"Sum"])+1 
         
       }else if(boxname == "e"){
         spiketrain[[ses]]$trials[[idx]][5,neuron] =  as.numeric(spiketrain[[ses]]$trials[[idx]][5,neuron]) + 1
         spiketrain[[ses]]$trials[[idx]][9,"Sum"] = as.numeric(spiketrain[[ses]]$trials[[idx]][5,"Sum"])+1
         
       }else if(boxname == "f"){
         spiketrain[[ses]]$trials[[idx]][6,neuron] =  as.numeric(spiketrain[[ses]]$trials[[idx]][6,neuron]) + 1
         spiketrain[[ses]]$trials[[idx]][9,"Sum"] = as.numeric(spiketrain[[ses]]$trials[[idx]][6,"Sum"])+1
         
       }else if(boxname == "g"){
         spiketrain[[ses]]$trials[[idx]][7,neuron] =  as.numeric(spiketrain[[ses]]$trials[[idx]][7,neuron]) + 1
         spiketrain[[ses]]$trials[[idx]][9,"Sum"] = as.numeric(spiketrain[[ses]]$trials[[idx]][7,"Sum"])+1
         
       }else if(boxname == "h"){
         spiketrain[[ses]]$trials[[idx]][8,neuron] =  as.numeric(spiketrain[[ses]]$trials[[idx]][8,neuron]) + 1
         spiketrain[[ses]]$trials[[idx]][9,"Sum"] = as.numeric(spiketrain[[ses]]$trials[[idx]][8,"Sum"])+1
         
       }else if(boxname == "i"){
         spiketrain[[ses]]$trials[[idx]][9,neuron] =  as.numeric(spiketrain[[ses]]$trials[[idx]][9,neuron]) + 1
         spiketrain[[ses]]$trials[[idx]][9,"Sum"] = as.numeric(spiketrain[[ses]]$trials[[idx]][9,"Sum"])+1
         
       }
       print("HERE7")
     }
  }
  print("HERE8")
  m <-as.matrix(spiketrain[[ses]]$trials[[100]]) 
  par(mfrow=c(4,1))
  barplot(as.numeric(m[,"Neuron1"]),col = "red",width = 0.3,xlim=c(0,10))
  barplot(as.numeric(m[,"Neuron2"]),col = "red",width = 0.3,xlim=c(0,10))
  barplot(as.numeric(m[,"Neuron3"]),col = "red",width = 0.3,xlim=c(0,10))
  barplot(as.numeric(m[,"Sum"]),col = "red",width = 0.3,xlim=c(0,10))
}