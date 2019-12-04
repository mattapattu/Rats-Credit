library(rgeos) #for polygon labels
library(rlist)
library(sp) #for spatial polygons
library(pracma) #for strcmp

####Functions for the striatum project

#### Test whether the numbers of spikes in two conditions (N1,N2) are different
#### This assumes that both spikes processes are Poisson with constant intensity
#### The procedure tests whether they have the same intensity or not.clea
#### If this is the case, given the total number of spikes in both conditions,  N1 obeys a binomila distribution with parameter n=N1+N2 and p = mu1/(mu1+mu2), 
#### where mu1 is the time spent for process 1 and mu2 the time spent for process 2
#### the pvalue of the twosided test is therefore min(2*F(N1),2*(1-F(N1-1))), with F the cdf of B(n,p)
#### Note that we would have found the same pvalue if we exchange N1 and N2, because the q_{1-alpha/2} quantile of one distrib is the q_{alpha/2} quantile of the other distrib
#### binom.test exist in R but does not compute the good upper pvalue


bin.test=function(N1,N2,mu1,mu2)
{
  n=N1+N2
  p=mu1/(mu1+mu2)
  vect=c(2*pbinom(N1,size=n,prob=p),2*(1-pbinom((N1-1),size=n,prob=p)),1)
  pval=min(vect)
  ind=which.min(vect)[1]
  code=0
  if(ind==1){code=-1}else{code=1}
  return(list(pval=pval,code=code))
}

### Multiple version + Benjamini Hochberg search in the pvalues
### Two lines in N and mu, first line is N1, second line is N2
### Use the function BH of the library UnitEvents

mult.bin.test=function(N,mu,level=0.05)
{
  Ntot=ncol(N)
  pval=rep(0,Ntot)
  code=pval
  for(i in 1:Ntot)
  {
    res=bin.test(N[1,i],N[2,i],mu[1,i],mu[2,i])
    pval[i]=res$pval
    code[i]=res$code
  }
  indx=BH(level,pval)
  code[-indx]=0
  return(list(ind=indx,code=code,pval=pval))
  
}

### Predefined list of Boxes (cf Leo Dort's internship )
leo.boxes=function()
{
  bar21 = matrix(c(120,250,120,185),nrow=2)
  bar22 = matrix(c(138,240,138,182),nrow=2)
  bar23 = matrix(c(190,210,190,158),nrow=2)
  bar24 = matrix(c(210,200,210,150),nrow=2)
  bar25 = matrix(c(210,130,240,130),nrow=2)
  bar42 = matrix(c(240,200,240,130),nrow=2)
  
  bar33 = matrix(c(120,75,120,10),nrow=2)
  bar34 = matrix(c(138,77,138,20),nrow=2)
  bar35 = matrix(c(190,101,190,50),nrow=2)
  bar36 = matrix(c(210,111,210,58),nrow=2)
  bar43 = matrix(c(240,130,240,58),nrow=2)
  
  bar26 = matrix(c(210,150,210,110),nrow=2)
  bar27 = matrix(c(105,150,105,110),nrow=2)
  bar28 = matrix(c(60,130,105,130),nrow=2)
  bar29 = matrix(c(60,170,110,170),nrow=2)
  bar30 = matrix(c(60,185,120,185),nrow=2)
  bar31 = matrix(c(60,90,110,90),nrow=2)
  bar32 = matrix(c(60,75,120,75),nrow=2)
  bar37 = matrix(c(105,130,210,130),nrow=2)
  bar38 = matrix(c(105,110,105,90),nrow=2)
  bar39 = matrix(c(105,170,105,150),nrow=2)
  bar40 = matrix(c(60,150,105,150),nrow=2)
  bar41 = matrix(c(60,110,105,110),nrow=2)
  bar44 = matrix(c(110,90,110,75),nrow=2)
  bar45 = matrix(c(110,185,110,170),nrow=2)
  bar46 = matrix(c(190,157,190,103),nrow=2)
  
  barMilieu = matrix(c(158,157,158,103),nrow=2)
  
  listeBar_2 = list(bar21,bar22,bar23,bar24,bar25,bar26,bar27,bar28,bar29,bar30,bar31,bar32,bar33,bar34,bar35,bar36,bar37,bar38,bar39,bar40,bar41,bar42,bar43,bar44,bar45,bar46) # partition du maze total avec disctinction cf fig 2.4
  listeBar=listeBar_2
  
  boite21G = matrix(c(listeBar[[10]][1,1],listeBar[[1]][2,1],listeBar[[10]][1,1],listeBar[[1]][2,2]),nrow=2)
  boite27G = matrix(c(listeBar[[20]][1,1],listeBar[[20]][2,1],listeBar[[21]][1,1],listeBar[[21]][2,1]),nrow=2)
  boite27D = matrix(c(listeBar[[20]][1,2],listeBar[[20]][2,2],listeBar[[21]][1,2],listeBar[[21]][2,2]),nrow=2)
  boite28G = matrix(c(listeBar[[9]][1,1],listeBar[[19]][2,1],listeBar[[9]][1,1],listeBar[[19]][2,2]),nrow=2)
  boite28D = listeBar[[19]]
  boite29G = matrix(c(listeBar[[9]][1,1],listeBar[[25]][2,1],listeBar[[9]][1,1],listeBar[[9]][2,1]),nrow=2)
  boite29D = listeBar[[25]]
  boite30G = matrix(c(listeBar[[11]][1,1],listeBar[[18]][2,1],listeBar[[11]][1,1],listeBar[[18]][2,2]),nrow=2)
  boite30D = listeBar[[18]]
  boite31G = matrix(c(listeBar[[12]][1,1],listeBar[[24]][2,1],listeBar[[12]][1,1],listeBar[[24]][2,2]),nrow=2)
  boite31D = listeBar[[24]]
  boite32G = matrix(c(listeBar[[12]][1,1],listeBar[[12]][2,1],listeBar[[12]][1,1],listeBar[[13]][2,2]),nrow=2)
  boite32D = listeBar[[13]]
  A = c(listeBar[[7]][1,1],listeBar[[26]][2,1]) ; B = c(listeBar[[26]][1,1],listeBar[[26]][2,1]) ; C = c(listeBar[[6]][1,1],listeBar[[6]][2,1])
  D = c(listeBar[[5]][1,2],listeBar[[5]][2,2]) ; E = c(listeBar[[6]][1,2],listeBar[[6]][2,2]) ; G = c(listeBar[[26]][1,2],listeBar[[26]][2,2]) ; H = c(listeBar[[7]][1,2],listeBar[[26]][2,2])
  
  I=(A+H)/2
  
  boites=list()
  
  boites[[1]]=cbind(boite21G,as.matrix(listeBar[[1]][,2]),as.matrix(listeBar[[1]][,1]))
  
  boites[[2]]=cbind(listeBar[[1]],as.matrix(listeBar[[2]][,2]),as.matrix(listeBar[[2]][,1]))
  boites[[3]]=cbind(listeBar[[2]],as.matrix(listeBar[[3]][,2]),as.matrix(listeBar[[3]][,1]))
  boites[[4]]=cbind(listeBar[[3]],as.matrix(listeBar[[4]][,2]),as.matrix(listeBar[[4]][,1]))
  
  boites[[5]]=cbind(listeBar[[4]],as.matrix(listeBar[[22]][,2]),as.matrix(listeBar[[22]][,1]))
  boites[[6]]=cbind(D,C,B,A,I)
  boites[[7]]=cbind(boite27G,as.matrix(boite27D[,2]),as.matrix(boite27D[,1]))
  boites[[8]]=cbind(boite28G,as.matrix(boite28D[,2]),as.matrix(boite28D[,1]))
  boites[[9]]=cbind(boite29G,as.matrix(boite29D[,2]),as.matrix(boite29D[,1]))
  boites[[10]]=cbind(boite30G,as.matrix(boite30D[,2]),as.matrix(boite30D[,1]))
  boites[[11]]=cbind(boite31G,as.matrix(boite31D[,2]),as.matrix(boite31D[,1]))
  boites[[12]]=cbind(boite32G,as.matrix(boite32D[,2]),as.matrix(boite32D[,1]))
  
  boites[[13]]=cbind(listeBar[[14]],as.matrix(listeBar[[13]][,2]),as.matrix(listeBar[[13]][,1]))
  boites[[14]]=cbind(listeBar[[15]],as.matrix(listeBar[[14]][,2]),as.matrix(listeBar[[14]][,1]))
  boites[[15]]=cbind(listeBar[[16]],as.matrix(listeBar[[15]][,2]),as.matrix(listeBar[[15]][,1]))
  
  boites[[16]]=cbind(listeBar[[23]],as.matrix(listeBar[[16]][,2]),as.matrix(listeBar[[16]][,1]))
  boites[[17]]=cbind(D,I,H,G,E)
  
  return(list(boxes=boites,A=A,B=B,C=C,D=D,E=E,F=F,G=G,H=H,I=I))
}

### Predefined list of Boxes (cf alex muzy for aca )
alex.boxes=function()
{
  bar21 = matrix(c(120,250,120,185),nrow=2)
 # bar22 = matrix(c(138,240,138,182),nrow=2)
  #bar23 = matrix(c(190,210,190,158),nrow=2)
  bar24 = matrix(c(210,200,210,150),nrow=2)
  bar25 = matrix(c(210,130,240,130),nrow=2)
  bar42 = matrix(c(240,200,240,130),nrow=2)
  
  bar33 = matrix(c(120,75,120,10),nrow=2)
  #bar34 = matrix(c(138,77,138,20),nrow=2)
  #bar35 = matrix(c(190,101,190,50),nrow=2)
  bar36 = matrix(c(210,111,210,58),nrow=2)
  bar43 = matrix(c(240,130,240,58),nrow=2)
  
  bar26 = matrix(c(210,150,210,110),nrow=2)
  bar27 = matrix(c(105,150,105,110),nrow=2)
  bar28 = matrix(c(60,130,105,130),nrow=2)
  bar29 = matrix(c(60,170,110,170),nrow=2)
  bar30 = matrix(c(60,185,120,185),nrow=2)
  bar31 = matrix(c(60,90,110,90),nrow=2)
  bar32 = matrix(c(60,75,120,75),nrow=2)
  bar37 = matrix(c(105,130,210,130),nrow=2)
  bar38 = matrix(c(105,110,105,90),nrow=2)
  bar39 = matrix(c(105,170,105,150),nrow=2)
  bar40 = matrix(c(60,150,105,150),nrow=2)
  bar41 = matrix(c(60,110,105,110),nrow=2)
  bar44 = matrix(c(110,90,110,75),nrow=2)
  bar45 = matrix(c(110,185,110,170),nrow=2)
  bar46 = matrix(c(190,157,190,103),nrow=2)
  
  barMilieu = matrix(c(158,157,158,103),nrow=2)
  
  listeBar_2 = list(bar21,bar22,bar23,bar24,bar25,bar26,bar27,bar28,
                    bar29,
                    bar30,
                    bar31,bar32,bar33,bar34,bar35,bar36,bar37,bar38,bar39,bar40,
                    bar41,bar42,bar43,bar44,
                    bar45,
                    bar46) # partition du maze total avec disctinction cf fig 2.4
  listeBar=listeBar_2
  
  boite21G = matrix(c(listeBar[[10]][1,1],listeBar[[1]][2,1],
                      listeBar[[10]][1,1],listeBar[[1]][2,2]),nrow=2)
  boite27G = matrix(c(listeBar[[20]][1,1],listeBar[[20]][2,1],listeBar[[21]][1,1],listeBar[[21]][2,1]),nrow=2)
  boite27D = matrix(c(listeBar[[20]][1,2],listeBar[[20]][2,2],listeBar[[21]][1,2],listeBar[[21]][2,2]),nrow=2)
  boite28G = matrix(c(listeBar[[9]][1,1],listeBar[[19]][2,1],listeBar[[9]][1,1],listeBar[[19]][2,2]),nrow=2)
  boite28D = listeBar[[19]]
  boite29G = matrix(c(listeBar[[9]][1,1],listeBar[[25]][2,1],listeBar[[9]][1,1],listeBar[[9]][2,1]),nrow=2)
  boite29D = listeBar[[25]]
  boite30G = matrix(c(listeBar[[11]][1,1],listeBar[[18]][2,1],listeBar[[11]][1,1],listeBar[[18]][2,2]),nrow=2)
  boite30D = listeBar[[18]]
  boite31G = matrix(c(listeBar[[12]][1,1],listeBar[[24]][2,1],listeBar[[12]][1,1],listeBar[[24]][2,2]),nrow=2)
  boite31D = listeBar[[24]]
  boite32G = matrix(c(listeBar[[12]][1,1],listeBar[[12]][2,1],listeBar[[12]][1,1],listeBar[[13]][2,2]),nrow=2)
  boite32D = listeBar[[13]]
  
  A = c(listeBar[[7]][1,1],listeBar[[26]][2,1]) ; 
  B = c(listeBar[[26]][1,1],listeBar[[26]][2,1]) ; 
  C = c(listeBar[[6]][1,1],listeBar[[6]][2,1])
  D = c(listeBar[[5]][1,2],listeBar[[5]][2,2]) ; 
  E = c(listeBar[[6]][1,2],listeBar[[6]][2,2]) ; 
  G = c(listeBar[[26]][1,2],listeBar[[26]][2,2]) ; 
  H = c(listeBar[[7]][1,2],listeBar[[26]][2,2])
  
  I=(A+H)/2
  
  boites=list()
  
  boites[[1]]=cbind(boite21G,as.matrix(listeBar[[1]][,2]),as.matrix(listeBar[[1]][,1]))
  
  boites[[2]]=cbind(listeBar[[1]],as.matrix(listeBar[[2]][,2]),as.matrix(listeBar[[2]][,1]),listeBar[[2]],as.matrix(listeBar[[3]][,2]),as.matrix(listeBar[[3]][,1]), listeBar[[3]],as.matrix(listeBar[[4]][,2]),as.matrix(listeBar[[4]][,1]))
  
  boites[[3]]=cbind(listeBar[[4]],as.matrix(listeBar[[22]][,2]),as.matrix(listeBar[[22]][,1]))
  boites[[4]]=cbind(D,C,B,A,I)
  boites[[5]]=cbind(boite27G,as.matrix(boite27D[,2]),as.matrix(boite27D[,1]))
  boites[[6]]=cbind(boite28G,as.matrix(boite28D[,2]),as.matrix(boite28D[,1]))
  boites[[7]]=cbind(boite29G,as.matrix(boite29D[,2]),as.matrix(boite29D[,1]))
  boites[[8]]=cbind(boite30G,as.matrix(boite30D[,2]),as.matrix(boite30D[,1]))
  boites[[9]]=cbind(boite31G,as.matrix(boite31D[,2]),as.matrix(boite31D[,1]))
  boites[[10]]=cbind(boite32G,as.matrix(boite32D[,2]),as.matrix(boite32D[,1]))
  
  boites[[11]]=cbind(listeBar[[14]],as.matrix(listeBar[[13]][,2]),as.matrix(listeBar[[13]][,1]))
  boites[[12]]=cbind(listeBar[[15]],as.matrix(listeBar[[14]][,2]),as.matrix(listeBar[[14]][,1]))
  boites[[13]]=cbind(listeBar[[16]],as.matrix(listeBar[[15]][,2]),as.matrix(listeBar[[15]][,1]))
  
  boites[[14]]=cbind(listeBar[[23]],as.matrix(listeBar[[16]][,2]),as.matrix(listeBar[[16]][,1]))
  boites[[15]]=cbind(D,I,H,G,E)
  
  return(list(boxes=boites,A=A,B=B,C=C,D=D,E=E,F=F,G=G,H=H,I=I))
}


### to retrieve data
### be in the good directory then
### enter the setsession : vector of number of session

recordings=function(setsession=c(1:18))
{
  lesmat=list.files(pattern='\\.mat$')
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
    
    
    if(ntetr>1)
    {  
      for(i in 2:ntetr)
      {
        data=readMat(mesmat[i])
        SPIKES=rbind(SPIKES,as.matrix(data[[1]][[2]]))#[,1:3])
      }
    }
    print(setsession[isess])
    Enreg[[isess]]=list(tet=tet,POS=POS,EVENTS=EVENTS,SPIKES=SPIKES)
  }
  
  return(Enreg)
}



#### plot of journeys
#### Enreg is the set of data coming from recordings. Session is the number 
#### of in the list of recordings
#### deb = starting event time of the journey
#### fin = ending event time of the journey
#### boites = set of boxes coming from leo.boxes()$boxes for instance
#### if event=true
#### then use deb = index of the starting event
#### use fin = index of the ending event

plot.journey=function(Enreg,session,deb,fin,boites,event=FALSE,aa=50,bb=250,cc=0,dd=250)
{
  if(event==TRUE)
  {
    ideb=deb
    ifin=fin
    deb=Enreg[[session]]$EVENTS[ideb,1]
    fin=Enreg[[session]]$EVENTS[ifin,1]
  }
  indices=which((Enreg[[session]]$POS[,1]>=deb)&(Enreg[[session]]$POS[,1]<=fin))

  Nboites=length(boites)
  mescol=colorRampPalette(c('red','yellow','green','blue'))(Nboites)
  
  ##Plot the polygons and their labels
  lab=list() #the labels
  pol=list() #the polygons
  
  for(p in 1:Nboites)
  {
    #polygon(boites[[p]][1,],boites[[p]][2,],col=mescol[p])
      
    pol[[p]]= Polygons(list(Polygon(cbind(boites[[p]][1,],boites[[p]][2,]))), 
                      ID = p)
#    lab[[p]]=p
    
  } 
  lab[[1]]="e"
  lab[[2]]="f"
  lab[[3]]="g"
  lab[[4]]="c"
  lab[[5]]="d"
  lab[[6]]="h"
  lab[[7]]="i"
  lab[[8]]="j"
  lab[[9]]="a"
  lab[[10]]="b"
  lab[[11]]="k"

  xy.sp = SpatialPolygons(pol)
  #summary(xy.sp)
  #par(new=TRUE)
  plot.new()
  # draw the axes
  plot(c(aa,bb),c(cc,dd),xlab='',ylab='',type="n")
  #par(new=TRUE)
  #plot(axes=TRUE,xlim=c(aa,bb),ylim=c(cc,dd))
  plot(xy.sp, col=mescol, add=TRUE)
  
  #plot(xy.sp)
  
  #par(new=TRUE)
  polygonsLabel(xy.sp, labels=lab, method="centroid",cex=1)
  
  #draw the journey between two events
  #par(new=TRUE)
  lines(Enreg[[session]]$POS[indices,2],Enreg[[session]]$POS[indices,3])

  #Plot the events of the journey
  if(event==TRUE)
  {
    text(aa,Enreg[[session]]$POS[indices[1],3],Enreg[[session]]$EVENTS[ideb,2])
    text(aa,Enreg[[session]]$POS[rev(indices)[1],3],Enreg[[session]]$EVENTS[ifin,2])
  }
}

#### shortcut is a function to summarize where the rat is 
#### to do so one defines up to 9 boxes, with code a... z. 0 
#### is the code for being outside of the boxes
#### it returns first an array with three columns : when it starts being in the box, 
#### when it leaves the box, the index of the box
#### it also gives a string giving the successive states (without 0)
#### then it will be easy to search for typical paths
#### Enreg comes form recordings()
#### session is the index of the session in the list Enreg
#### myboxes is the list of boxes we want to use. They have to be defined as polygons, 
#### they may not have the same number of vertices. 
#### This uses the library SDMTools  for finding if a point is inside a polygon (box) 
#### or not

shortcut=function(Enreg, session, myboxes)
{
  nbox=length(myboxes)
  npos=nrow(Enreg[[session]]$POS)
  reponse= matrix(0,nrow=npos,ncol=nbox)
  
  for(ibox in 1:nbox)
  {
    res=pnt.in.poly(Enreg[[session]]$POS[,2:3],t(myboxes[[ibox]]))
    reponse[,ibox]=res[,3]
  }
  
  rac=matrix(0,nrow=0,ncol=3) ### it is the array 
  stepmoins1=0
  incour=0
  
  for(i in 1:npos)
  {
    if(sum(reponse[i,])==0){stepmoinsun=0}
    if(sum(reponse[i,])>0)
    {
      indice=which(reponse[i,]==1)
      if(length(indice)==1)
      {
        if(indice==stepmoins1)
        {
          rac[incour,2]=Enreg[[session]]$POS[i,1]
        }else{
          incour=incour+1
          newind=convertToLetter(toString(indice))
          #rac=rbind(rac,c(Enreg[[session]]$POS[i,1],0,indice))
          rac=rbind(rac,c(Enreg[[session]]$POS[i,1],0,newind))
          stepmoins1=indice
        }
      }
      # if(length(indice)==2)
      # {
      #   monind=which(indice==stepmoins1)
      #   if(length(monind)>0)
      #   {
      #     #newind=indice[-monind]
      #     newind=convertToLetter(toString(indice[-monind]))
      #     rac[incour,2]=Enreg[[session]]$POS[i,1]
      #     incour=incour+1
      #     rac=rbind(rac,c(Enreg[[session]]$POS[i,1],0,newind))
      #     stepmoins1=newind
      #   }
      #}  
    }
  }
  
  ### it's possible that the rat exited immediately, 
  ### I need to change the 0 that remains in the second column
  
  indices=which(rac[,2]==0)
  rac[indices,2]=rac[indices,1]
  
  ###
  rac_str=paste(rac[,3],collapse='') ## that's the string
  
  return(list(tab=rac,journey=rac_str))
  
}

#### extract.spikes is a function that extracts the $spikes occuring in a certain box 
#### of reference (box.ref) (should be a polygon) along a certain path, 
#### the time spent ($duration), and the entering and exiting times ($pass)
#### there, short is  returned by shortcut
#### path should be a substring of short$journey 
#### needs library(stringr)

extract.spikes=function(Enreg,session,short,box.ref,path)
{ #get indices of path (the pattern to match in reg expression journey)
  indices=gregexpr(path,short$journey)[[1]]
  #print(indices)
  if(length(indices)==1)
  {
    if(indices==-1){
      return='empty'
    }
  }else{
   messpikes=matrix(0,nrow=0,ncol=ncol(Enreg[[session]]$SPIKES))
   lpath=str_length(path)
   #lind=length(indices[[1]])
   lind=length(indices)
   duree=0
   Pass=matrix(0,ncol=lind,nrow=2)
   
    #For each path
    for(bloc in 1:lind){
     #short gets the entering times of each path (bloc) in [,1] 
     #and exiting times in [,2]
     debut=as.integer(short$tab[indices[bloc],1])
     fin=as.integer(short$tab[indices[bloc]+lpath-1,2])
     
     indamoi=which((Enreg[[session]]$POS[,1]>=debut)&(Enreg[[session]]$POS[,1]<=fin))
    
     qui=pnt.in.poly(Enreg[[session]]$POS[indamoi,2:3],box.ref)
    
     ind2=which(qui[,3]==1)
    
     debut_ref=Enreg[[session]]$POS[indamoi[ind2[1]],1]
     Pass[1,bloc]=debut_ref
     fin_ref=Enreg[[session]]$POS[indamoi[tail(ind2,n=1)],1]
     
     Pass[2,bloc]=fin_ref
     duree=duree+(fin_ref-debut_ref)
  
    ### les spikes la 
    mesindloc=which((Enreg[[session]]$SPIKES[,1]>=debut_ref)&(Enreg[[session]]$SPIKES[,1]<=fin_ref))
    if(length(mesindloc)==1){ messpikes=rbind(messpikes,t(as.matrix(Enreg[[session]]$SPIKES[mesindloc,]))) }
    if(length(mesindloc)>1){ messpikes=rbind(messpikes,Enreg[[session]]$SPIKES[mesindloc,]) }
    
   }
  
  return(list(spikes=messpikes, duration=duree, pass=Pass))
  }
}

#Set boxes to run activity-based credit assignment: 
#Test if the rat turns left/right, go straight or backward, etc.
alex.mergeBoxes=function(boxes)
{
  boxes[[3]][1,]= cbind(boxes[[2]][1,1:2],c(190,190))
  boxes[[3]][2,]= cbind(boxes[[2]][2,1:2],c(150,200))
  
  boxes[[5]][1,]= cbind(190,190,240,240)
  boxes[[5]][2,]= cbind(200,150,150,200)
  
  #boxes[[11]][1,]= cbind(240,240,190,190)
  #boxes[[11]][2,]= cbind(111,58,58,111)
  
  #boxes[[5]][2,3]=150
  
  boxes[[8]][1,]= cbind(60,60,105,105)
  boxes[[8]][2,]= cbind(185,150,150,185)
      
  #boxes[[13]][1,]= cbind(boxes[[12]][1,3:4],boxes[[15]][1,1:2])
  #boxes[[13]][2,]= cbind(boxes[[12]][2,3:4],boxes[[15]][2,1:2])
  boxes[[13]][1,]= cbind(120,120,190,190)
  #boxes[[15]][1,1:2])
  boxes[[13]][2,]= cbind(75,10,58,111)
  
  boxes[[10]][1,]= cbind(60,60,105,105)
  boxes[[10]][2,]= cbind(111,75,75,111)
  
  boxes[[14]][1,]= cbind(190,190,240,240)
  boxes[[14]][2,]= cbind(150,111,111,150)
  
  boxes[[15]][1,]= cbind(105,105,190,190)
  boxes[[15]][2,]= cbind(150,111,111,150)
  
  #boxes[[16]][2,1]=111
  boxes[[16]][1,]= cbind(240,240,190,190)
  boxes[[16]][2,]= cbind(111,58,58,111)
  
  boxes[[7]][2,2] = 111
  boxes[[7]][2,3] = 111
  
  list.remove(boxes,c(2,4,6,9,11,17))
}

getSpatialPolygons=function(boites)
{
  Nboites=length(boites)
  mescol=colorRampPalette(c('red','yellow','green','blue'))(Nboites)
  
  ##Plot the polygons and their labels
  lab=list() #the labels
  pol=list() #the polygons
  
  
  for(p in 1:Nboites)
  {
    #polygon(boites[[p]][1,],boites[[p]][2,],col=mescol[p])
    
    if(p==1) l="e"
    else if(p==2) l="f"
    else if(p==3) l="g"
    else if(p==4) l="c"
    else if(p==5) l="d"
    else if(p==6) l="h"
    else if(p==7) l="i"
    else if(p==8) l="j"
    else if(p==9) l="a"
    else if(p==10) l="b"
    else if(p==11) l="k"
    
    
    pol[[p]]= Polygons(list(Polygon(cbind(boites[[p]][1,],boites[[p]][2,]))), 
                       ID = l)
   # lab[[p]]=l
    
  } 
  lab[[1]]="e"
  lab[[2]]="f"
  lab[[3]]="g"
  lab[[4]]="c"
  lab[[5]]="d"
  lab[[6]]="h"
  lab[[7]]="i"
  lab[[8]]="j"
  lab[[9]]="a"
  lab[[10]]="b"
  lab[[11]]="k"
  
  xy.sp = SpatialPolygons(pol)
  polygonsLabel(xy.sp, labels=lab, method="centroid",cex=1)
  
  return(xy.sp)
  
}

convertToLetters=function(shortcutLong){
  path=""
  shortcut = strsplit(shortcutLong, "")[[1]]
  for(i in shortcut){
    if(strcmp(i,"1")) path=paste(path,"e",sep="")
    else if(strcmp(i,"2")) path=paste(path,"f",sep="")
    else if(strcmp(i,"3")) path=paste(path,"g",sep="")
    else if(strcmp(i,"4")) path=paste(path,"c",sep="")
    else if(strcmp(i,"5")) path=paste(path,"d",sep="")
    else if(strcmp(i,"6")) path=paste(path,"h",sep="")
    else if(strcmp(i,"7")) path=paste(path,"i",sep="")
    else if(strcmp(i,"8")) path=paste(path,"j",sep="")
    else if(strcmp(i,"9")) path=paste(path,"a",sep="")
    else if(strcmp(i,"10")) path=paste(path,"b",sep="")
    else if(strcmp(i,"11")) path=paste(path,"k",sep="") 
  }
  
  return(path)
}
convertToLetter=function(i){
    if(strcmp(i,"1")) return("e")
    else if(strcmp(i,"2")) return("f")
    else if(strcmp(i,"3")) return("g")
    else if(strcmp(i,"4")) return("c")
    else if(strcmp(i,"5")) return("d")
    else if(strcmp(i,"6")) return("h")
    else if(strcmp(i,"7")) return("i")
    else if(strcmp(i,"8")) return("j")
    else if(strcmp(i,"9")) return("a")
    else if(strcmp(i,"10")) return("b")
    else if(strcmp(i,"11")) return("k") 
    else {
      print(sprintf("Recevied unknown boxnumber %s, returning -1", i))
      return(-1)
    }
}

convertToIndex=function(i){
  if(strcmp(i,"e")) return(1)
  else if(strcmp(i,"f")) return(2)
  else if(strcmp(i,"g")) return(3)
  else if(strcmp(i,"c")) return(4)
  else if(strcmp(i,"d")) return(5)
  else if(strcmp(i,"h")) return(6)
  else if(strcmp(i,"i")) return(7)
  else if(strcmp(i,"j")) return(8)
  else if(strcmp(i,"a")) return(9)
  else if(strcmp(i,"b")) return(10)
  else if(strcmp(i,"k")) return(11) 
  else return(-1)
}
# 
# getNeuronsCorrToBox=function(right,enreg,session){
#  #For each neuron
#   #For each box
#    #Compute the correlation btwn vector of activity of neurons and binary vector
#    #of positions (OO1OOO where 1 is being in the box)
#  
#   #return
#    
#   
#   for(col in 1:ncol(right$pass)){
#     sortie=right$pass[2,col]
#     entree=right$pass[1,col]
#     durationPerPass[[col]]=sortie-entree
#     #avg nb of spikes in the box at each passage
#     spikesInBox=right$spikes[,3][(right$spikes[,1]>=entree)
#                                  &(right$spikes[,1]<=sortie) 
#                                  &(right$spikes[,3]!=0)]
#     
#     # which((right$spikes[,1]>=entree)
#     #                &(right$spikes[,1]<=sortie)
#     #                &(right$spikes[,3]!=0))
#     
#     #nb of neurons in the box at each passage
#     nbNeuronsPerPass[[col]]=length(unique(spikesInBox))
#     #length(unique(right$spikes[,2])) nb of tetrodes
#     
#     #length(unique(spikesInBox))
#     #length(unique(right$spikes[,3][right$spikes[,3]!=0]))
#     
#     if(length(unique(spikesInBox))!=0)
#       spikesPerPass[[col]]=length(spikesInBox)/nbNeuronsPerPass[[col]]
#     else
#       spikesPerPass[[col]]=0
#   }
#   allNeurons=enreg[[session]]$spikes[,3]
#   neuronsInBox=allNeurons[allNeurons]which(right)
#   
#   return  
# }
#colnames(spikyBoxes)=c("spikes","duration","pass")

#each box of the right path is assigned a spiky box
#it is a list where index is the session nb 
#at each index, there is a vector of boxes
getBoxesInPath=function(allGoodSessions,Enreg,spolygons,short,rightPath){
  #Split right Path boxes names except last
  string_split <- strsplit(str_sub(rightPath,0,-2), "")[[1]]
  #rownames(spikyBoxes)=string_split
  bs=list()
  spikyBoxes=list()
  #b=list('1'=list(),'2'=list(list(),list()))
  for(ses in allGoodSessions){
    for(nm in string_split) {
      coord=spolygons[convertToIndex(nm),]@polygons[[1]]@Polygons[[1]]@coords
      right=extract.spikes(Enreg,ses,short,coord,rightPath)#Get the box in the path
      #bxs=list.append(bxs,right)
      #bs[[nm]]=right
      bs=list.append(bs, right)
    }
    names(bs)=string_split
    spikyBoxes=list.append(spikyBoxes, bs)
    bs=list()
  }
  return(spikyBoxes)
}

boxActivities=function(i){
  #Compute the neuron activity in each box
  #a list where the indexes are the neuron numbers
  #and for each neuron there is a vector of activity in boxes
  actInBoxes=list()
  #Create an activity vector
  bxs=replicate(length(string_split),0)
  #The indexes of the vector are the right path boxes
  names(bxs)=string_split
  #For each box
  for(nm in string_split){
    #Get the index of neurons
    neurons=spikyBoxes[[nm]]$spikes[,3][(spikyBoxes[[nm]]$spikes[,3]!=0)]
    for(n in neurons){
      if(is.null(neuronThruBoxes[[toString(n)]])){
        #names(bxs)=string_split
        #bxs=list.append(bxs,right)
        neuronThruBoxes[[toString(n)]]=bxs
      }
      a=neuronThruBoxes[[toString(n)]][nm]
      a=a+1
      neuronThruBoxes[[toString(n)]][nm]=a
    }
  }
  
  return(neuronThruBoxes)
}
