library(R.matlab)
library(SDMTools)
library(stringr)
library(eegkit)#Librairie EEG pour l'analyse du LFP
library("plot3D")
#library(hashmap)
library(data.tree)
library(pracma)
library(sp) #for spatial polygons

setwd("C:\\Users\\M4NC\\Documents\\Rats-Credit\\Sources")

options(error = recover)


# Load all source files from working directory
for (f in list.files(pattern="*.R$")) {
  if(!strcmp(f,"rats-credit.R"))
    source(f)
}

#Load SDM and SDL folders (SDM113,SDL101, etc.)
setwd("C:\\Users\\M4NC\\Documents\\Rats-Credit\\Data")
# Experiment in Marseille   
#    ¦--SDL                   
#    ¦   °--rat_103           
#    ¦       ¦--103_session_1 
#    ¦       ¦--103_session_2...
#   °--SDM                   
#       °--rat_113           
#           ¦--113_session_1 
#           ¦--113_session_2 
#           ¦--113_session_3...
DATA=buildDataTree()
#print(DATA)

#save(DATA,file="exptree.Rdata")

#DATA$Get("POS")
#DATA$SDM$fieldsAll
#DATA$Climb(position = 1, name = "positions")$path
#plot(DATA)

# Here we define the parameters we want to
# analyze in the data tree
# REGIONS=c("SDM","SDL")
# RATS=c(1,2,3,4,5,6)
# SESSIONS=#ENREG
# #Pour SDM 113
# SESSIONS=list.remove(SESSIONS,13)# Because session 13 is shifted

# ### the boxes to make the plot and to decide where to go
resalex=leo.boxes()
#print(resalex$boxes)
boites=alex.mergeBoxes(resalex$boxes)
#print(boites)

# print one rat/session if node is session, e;g., DATA$SDL$rat_3$session_1
# or all sessions of one rat if node is one rat, eg, DATA$SDL$rat_3
# or all rat/sessions if agr is root node, DATA
# Examples: 
#"Experiment in Marseille" plot all the journeys
# "region_SDM" plot all rat journeys of all rats recorded in SDM region
# "rat_113" plot the rat journey
# "113_session_6" plot one session of one rat
#print.plot.journeys(DATA,FindNode(DATA,"Experiment in Marseille"),boites)

#Add the spatial polygons to the root
DATA$Set(spatialPolygons = boites,filterFun = function(x) x$level == 1)
#print(DATA,"spatialPolygons")


# set spatial polygons of boites
# On obtient tous les indexes par : getSpPPolygonsIDSlots(spol)
# les objets par : spol['a',]
#les coordonnées par : spolygons['a',]@polygons[[1]]@Polygons[[1]]@coords
#spolygons=getSpatialPolygons(boites) 
# #Collect all spiky boxes in a right path
rightPath="efgabchi"
DATA=set.neurons.to.boxes(DATA,rightPath,boites)

#rightPath="ijkabcde"
#DATA=set.neurons.to.boxes(DATA,rightPath,spolygons)

# resalex=leo.boxes()
# ### the boxes to make the plot and to decide where to go
# boites=alex.mergeBoxes(resalex$boxes)
#
# Enreg=recordings() ### Enreg[[session]]$ POS, EVENTS ou SPIKES
#
# ideb=1 # start plotting from the ideb'th event to the ifin'th event
# ifin= 30 #
# #lesmat=list.files(pattern='\\.mat$')
#
# fileNm="SDM113_S1t1.mat"
# data=readMat(fileNm)
# #one=substring.location(fileNm, 'S',restrict=c(3,999))
# #two=substring.location(fileNm, 't',restrict=c(3,13))
# #session=as.integer(substr(fileNm, as.integer(one)+1, as.integer(two)-1))
#
# session=10
#
# #for(session in 1:18){
# ## Print pdf file
# #pdf(paste("journey-session-",session))
#   plot.journey(Enreg,session,ideb,ifin,boites,event=TRUE)
#   #Close to print pdf file
#   #dev.off()
# #}
#
# # position  1ere colonne le temps en ms, puis x puis y
# POS=as.matrix(data[[1]][[1]])[,1:3]
#
# # les evenements 49 et 51 recompense en bas et en haut, 1ere colonne temps
# EVENTS=as.matrix(data[[1]][[3]])
#
# # tu peux garder que les 3 premiere colonnes : temps, tetrode, neurone,
# # attention neurone 0 = poubelle
# SPIKES=as.matrix(data[[1]][[2]])
#
# # temps, amplitude?
# LFP=as.matrix(data[[1]][[4]])
# #plot(round(LFP[1:700,1],1),round(LFP[1:700,2],1),type="l")
#
# myboxes=list()
# for(i in 1:length(boites)){
#   myboxes[[i]]=t(boites[[i]])
#   # text(mean(myboxes[[i]][,1]),mean(boites[[i]][,2]),i,col='orange',cex=2)
# }
#
# spolygons=getSpatialPolygons(boites)
# # On obtient tous les indexes par : getSpPPolygonsIDSlots(spol)
# # les objets par : spol['a',]
# #les coordonnées par : spolygons['a',]@polygons[[1]]@Polygons[[1]]@coords
#
# #Show where the rat is printing the box numbers
# short=shortcut(Enreg, session, myboxes)
# #convert numeric path to string and add it to short
# #short[[3]]=convertToLetters(short[[2]])
#
# ## the central box
# box.ref=myboxes[[10]]
# # extrait pour un chemin et une boîte : le nb de spikes, le temps passé, les passages
# # (temps d'entrée/sortie)
# #get coordinates of the polygon "a"
# coords=spolygons['b',]@polygons[[1]]@Polygons[[1]]@coords
# extrait= extract.spikes(Enreg,session,short,coords,"abc")
#
# # spikes apparus dans la boite centrale
# #1st col spike time in ms temps du spike en ms
# #2nd col tetrod nb
# #3d col neuron nb
# #after 4th col action potential shape for each tetrod
# extrait$spikes
# extrait$duration # duree totale
# # les passages (colonne) dans la boite centrale  avec temps entree/sortie (ligne)
# extrait$pass
#
# #right=extract.spikes(Enreg,session,short,coords,'abc')
# #ncol(right$pass) # le nombre de bons passages
# # left=extract.spikes(Enreg,session,short,coords,'abchijka')
# # ncol(left$pass) # le nombre de bons passages
#
#
# #Collect all spiky boxes in a right path
# rightPath='abcdefg'
# allGoodSessions=seq(1:18)
# ses=8
# short=shortcut(Enreg, ses, myboxes)
# #attention ses, pour une session donc !
#
# allGoodSessions=list.remove(allGoodSessions,13)# Because session 13 is shifted
# #
# spikyBoxes=getBoxesInPath(allGoodSessions,Enreg,spolygons,short,rightPath)
#
# #Compute the neuron activity in each box
# #a list where the indexes are the neuron numbers
# #and for each neuron there is a vector of activity in boxes
# neuronThruBoxes=list()
# #Create an activity vector
# bxs=replicate(length(string_split),0)
# #The indexes of the vector are the right path boxes
# names(bxs)=string_split
# #For each box
# for(nm in string_split){
#   #Get the index of neurons
#   neurons=spikyBoxes[[nm]]$spikes[,3][(spikyBoxes[[nm]]$spikes[,3]!=0)]
#   for(n in neurons){
#     if(is.null(neuronThruBoxes[[toString(n)]])){
#       #names(bxs)=string_split
#       #bxs=list.append(bxs,right)
#       neuronThruBoxes[[toString(n)]]=bxs
#     }
#     a=neuronThruBoxes[[toString(n)]][nm]
#     a=a+1
#     neuronThruBoxes[[toString(n)]][nm]=a
#   }
# }
#
# #compute the activity of boxes per session
#
#
# durationPerPass=list()
# #spikesPerPass=list()
# durationPerSession=list()
# spikesPerSession=list()
# spikesPerPass=list()
# nbNeuronsPerPass=list()
# spikesDensityPerSession=list()
# gammaPerSession=list()
#
#
# for(ses in allGoodSessions){
#   #ses=10
#   short=shortcut(Enreg, ses, myboxes)
#   #plot.new()
#   #plot.journey(Enreg,ses,1,30,boites,event=FALSE)
#
#   coords=spolygons['b',]@polygons[[1]]@Polygons[[1]]@coords
#   right=extract.spikes(Enreg,ses,short,coords,'abcd')#
#
#   #get the neuron indexes
#   neurons=right$spikes[,3][(right$spikes[,3]!=0)]
#   #Get neurons activity for each pass
#   neuronsActivityPerPass= matrix(0,
#                                  nrow=length(unique(neurons)), ncol(right$pass))
#
#   #corrRight=getNeuronsCorrToBox(right)
#   #For each pass
#   for(col in 1:ncol(right$pass)){
#     sortie=right$pass[2,col]
#     entree=right$pass[1,col]
#     durationPerPass[[col]]=sortie-entree
#     #avg nb of spikes in the box at each passage
#     spikesInBox=right$spikes[,3][(right$spikes[,1]>=entree)
#                                  &(right$spikes[,1]<=sortie)
#                                  &(right$spikes[,3]!=0)]
#
#     for(n in spikesInBox){
#       neuronsActivityPerPass[n,col]=neuronsActivityPerPass[n,col]+1
#     }
#
#     # which((right$spikes[,1]>=entree)
#     #                &(right$spikes[,1]<=sortie)
#     #                &(right$spikes[,3]!=0))
#
#     #nb of neurons in the box at each passage
#     nbNeuronsPerPass[[col]]=length(unique(spikesInBox))
#       #length(unique(right$spikes[,2])) nb of tetrodes
#
#       #length(unique(spikesInBox))
#     #length(unique(right$spikes[,3][right$spikes[,3]!=0]))
#
#     if(length(unique(spikesInBox))!=0)
#       spikesPerPass[[col]]=length(spikesInBox)/nbNeuronsPerPass[[col]]
#     else
#       spikesPerPass[[col]]=0
#   }
#
#   plot.new()
#   plot(unlist(durationPerPass),main=paste("session ",ses))
#   list.clean(durationPerPass)
#
#   plot.new()
#   plot(unlist(spikesPerPass),main=paste("session ",ses))
#   #attention ça ne veut pas dire forcemment grand chose si ce ne sont pas les
#   #memes neurones à chaque session, seules les spikes qui ne sont pas du neurone
#   #poubelle 0 sont comptés
#   spikesPerSession[[ses]] = Reduce(`+`, spikesPerPass)/ncol(right$pass)
#   list.clean(spikesPerPass)
#
#   plot.new()
#   plot(unlist(nbNeuronsPerPass),main=paste("session ",ses))
#   list.clean(nbNeuronsPerPass)
#
#
#   plot.new()
#   par(mai=c(0.2,0.2,0.2,0.4))
#   if( length(unique(neurons)) > 1 ){
#   persp3D(seq(1,length(unique(neurons))),seq(1,ncol(right$pass)),
#           neuronsActivityPerPass,theta=70, expand=0.5,phi=40,
#           resfac=5,axes=TRUE,scale=TRUE, box=TRUE, nticks=2,
#           ticktype="detailed",xlab="N", ylab="P", zlab="A",
#           main="Activity of neurons for each pass")
#   }
#   else{
#     par(mai=c(0.8,0.8,0.8,0.8))
#     plot(seq(1,ncol(right$pass)),neuronsActivityPerPass,type='l')
#   }
#   par(mai=c(0.8,0.8,0.8,0.8))
#
#
#
#   #avg time per passage in the box
#   durationPerSession[[ses]]=right$duration/ncol(right$pass)
#
#     #length(which(right$spikes[,3]!=0))/ncol(right$pass)
#
#   spikesDensityPerSession[[ses]]=spikesPerSession[[ses]]
#   #/(durationPerSession[[ses]]/1000)
#
#   #lesmat=list.files(pattern='\\.mat$')
#   fileNm=paste("SDM113_S",ses,"t1.mat",sep='')
#   data=readMat(fileNm)
#   #one=substring.location(fileNm, 'S',restrict=c(3,999))
#   #two=substring.location(fileNm, 't',restrict=c(3,13))
#   #session=as.integer(substr(fileNm, as.integer(one)+1, as.integer(two)-1))
#
#   # temps, amplitude?
#   LFP=as.matrix(data[[1]][[4]])
#   gamma=eegfft(LFP[,2], 1000, 45, 65)
#   accActivity=0
#   max=length(gamma$strength)-1
#   for(i in 1:max){
#     accActivity = accActivity + abs(gamma$strength[i]-gamma$strength[[i+1]])
#   }
#   gammaPerSession[[ses]]=accActivity
#
# }
#
#   plot.new()
#   plot(unlist(durationPerSession))
#   plot.new()
#   plot(unlist(spikesPerSession))
#   plot.new()
#   plot(unlist(spikesDensityPerSession))
#   plot.new()
#   plot(unlist(gammaPerSession))
#
# #credit and proba distributions
#
#
#
#
# #low-gamma (45– 65 Hz) and high-gamma (70 –90 Hz),
# # theta: (7– 14 Hz)
# gamma=eegfft(LFP[,2], 1000, 45, 65)
# #gamma=eegresample(gamma, round(length(gamma[,1])/100))
# #gamma=eegsmooth(gamma$strength, space = NULL, time = NULL, nknots = NULL,
# #          rparm = NULL, lambdas = NULL, skip.iter = TRUE,
# #          se.fit = FALSE, rseed = 1234)
# plot(gamma$frequency,gamma$strength,type="l", xlim=c(40,50))
# eegpsd(LFP[,2], 1000, upper = 90, t = "b")
#
# #### attention a shifter si les 6 rats
#
# shiftx=153.5
# shifty=129.5
# shift=c(shiftx,shifty)
#
# resalex=leo.boxes()
#
# lb=length(resalex$boxes)
# boites=resalex$boxes
#
# for(i in 1:lb)
# {
#   boites[[i]]=rbind(resalex$boxes[[i]][1,]-shiftx,3resalex$boxes[[i]][2,]-shifty)
# }
#
#
# A=resalex$A-shift ; B=resalex$B-shift ;C=resalex$C-shift ;
# D=resalex$D-shift ;E=resalex$E-shift ;G=resalex$G-shift;
# H=resalex$H-shift; I=resalex$I-shift
#
