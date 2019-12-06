#library(data.tree)
#library(stringr)

#Search in working dir and build automatically the data tree
buildDataTree=function(){
  #get all directories, e.g., "./SDM113"
  dirs=list.dirs('.', recursive=TRUE)
  
  DATA <- Node$new("Experiment in Marseille")
  regionSDM = NULL
  regionSDL = NULL
  for(dirNm in dirs){
    if(str_detect(dirNm,"SDM") && !str_detect(dirNm,"rat")){
      if(is.null(regionSDM)){
        regionSDM <- DATA$AddChild("region_SDM")
      }
    }
    else if(str_detect(dirNm,"SDL") && !str_detect(dirNm,"rat")){
      if(is.null(regionSDL) && !str_detect(dirNm,"rat")){
        regionSDL <- DATA$AddChild("region_SDL")
      }
    }
    if(str_detect(dirNm,"rat")){
      if(str_detect(dirNm,"SDM")){
        animalNb=sub("./region_SDM/rat_","",dirNm)
        rat=regionSDM$AddChild(paste("rat_",animalNb,sep=""))
      }
      else if(str_detect(dirNm,"SDL")){
        animalNb=sub("./region_SDL/rat_","",dirNm)
        rat=regionSDL$AddChild(paste("rat_",animalNb,sep=""))
      }
      #Collect all sessions data
      ### Enreg[[session]]$ POS, EVENTS, SPIKES, ou LFP
      print(dirNm)
      enreg=recordingsInFolder(dirNm) 
      #ses=1
      for(ses in 1:length(enreg)){
        if(is.null(enreg[[ses]])){
          print(sprintf("No enreg data for %s , ses %i", rat$name,ses))
          next
        }
        #temps, tetrode, neurone, remove last voltage values
        #spks=rec$SPIKES[,1:3] 
        spks=enreg[[ses]]$SPIKES[,1:3] 
        vb=logical(length(spks[,1])) #add a vector of booleans
        vs=vector(mode="character", length=length(spks[,1])) #add a vector of strings
        spks=cbind(spks,vs,vs,vb)
        colnames(spks) <- c("time","tetrod","neuron","rightPath","boxName","boxSensitivity")
        # add a list of (trial, path, boxNm, activity)
        path=list()
        pth=list("trial"=1, "path"="", "boxNm"="", "activity"="")
          #cbind(rec$POS[,1],vector(mode="character", length=length(rec$POS)), vector(mode="list", length=length(rec$POS)))  
        #colnames(pth) <- c("trial","path","boxActivities")
        #to update
        #session = 
          rat$AddChild(paste(animalNb,"_session_",ses,sep=""),
                               #set fields (capital letters)
                               POS=enreg[[ses]]$POS,#time, x,y
                               # events 49 and 51 are respectively bottom and top rewards
                               EVENTS=enreg[[ses]]$EVENT,#time, evts
                               SPIKES=spks,#time, tetrod, neuron, rightPath,boxName,boxSensitivity
                               PATHS=pth,#trial, path, boxNm, activity
                               LFP=enreg[[ses]]$LFP)#time, amplitude
        #ses=ses+1
      }
    }   
  }
  return(DATA)
}


### to retrieve data
### from directory
### enter the setsession : vector of number of session
### set and restore working directory

recordingsInFolder=function(folderNm){
  #folderNm="./SDL103/"
  folderNm=sub(".","",folderNm)
  rootPath=getwd()
  pt=paste(rootPath,folderNm,sep='')
  setwd(pt)
  
  lesmat=list.files(path=pt,pattern='\\.mat$')
  #get the session nb vector
  allSNbs=c()
  for(fileNm in lesmat){
      #sone=str_locate(fileNm,"_S")
      #two=str_locate(fileNm,"t")
      #sessionNb=as.integer(substr(fileNm, one[2]+1, two[1]-1))
    sessionNb=str_match(fileNm,"S(\\d+)t")[,2]
    if(!is.na(sessionNb)){
      allSNbs=c(allSNbs,sessionNb)
    }
    # print(fileNm)
    # print(sessionNb)
    
  }
  setsession=unique(allSNbs)
  
  lsess=length(setsession)
  Enreg=list()
  
  for(isess in 1:lsess){
    #print(setsession[isess])
    pat=paste('S',setsession[isess],'t',sep='')
    mesmat=lesmat[grep(pat,lesmat)] # here all the files of the given session
    ### les tetrodes
    ntetr= length(mesmat)
    tet=c()
    for(i in 1:ntetr){
      a=strsplit(mesmat[i],pat)[[1]][2]
      tet=c(tet,as.integer(strsplit(a,'.mat')[[1]]))
    }
    ### positions
    data=readMat(mesmat[1])
    POS=as.matrix(data[[1]][[1]])[,1:3]
    
    ### les evenements
    EVENTS=as.matrix(data[[1]][[3]])
    
    ###  les spikes
    SPIKES=as.matrix(data[[1]][[2]])#[,1:3])
    
    # LFP: temps (faux), amplitude (à chaque pas de temps 
    # pour 1kHz, ie, chaque 1ms)
    LFP=as.matrix(data[[1]][[4]]) 
    
    if(ntetr>1)
    {  
      for(i in 2:ntetr)
      {
        data=readMat(mesmat[i])
        SPIKES=rbind(SPIKES,as.matrix(data[[1]][[2]]))#[,1:3])
      }
    }
    print(setsession[isess])
    Enreg[[as.numeric(setsession[isess])]]=list(tet=tet,POS=POS,EVENTS=EVENTS,SPIKES=SPIKES,LFP=LFP)
  }
  setwd(rootPath)
  return(Enreg)
}