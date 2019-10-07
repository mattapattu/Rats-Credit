#Make the movie \w comand line
#convert *.jpg -resize 70% j%02d.jpg
#convert -delay 20 -loop 0 journey-step-*.jpg journey.gif

setwd("~/ExpComputabrain/SDM113/")

library(R.matlab)
library(SDMTools)
library(stringr)

source("../func.R")
#source("../aca.R")

resalex=leo.boxes()
### the boxes to make the plot and to decide where to go
boites=alex.mergeBoxes(resalex$boxes) 

#Enreg=recordings() ### Enreg[[session]]$ POS, EVENTS ou SPIKES

lesmat=list.files(pattern='\\.mat$')

fileNm="SDM113_S1t1.mat"
data=readMat(fileNm)
#one=substring.location(fileNm, 'S',restrict=c(3,999))
#two=substring.location(fileNm, 't',restrict=c(3,13))
#session=as.integer(substr(fileNm, as.integer(one)+1, as.integer(two)-1))

POS=as.matrix(data[[1]][[1]])[,1:3] 
position=1
deltaPosition=9
session=10
imageIndex=1

for(position in seq(from=deltaPosition+1,to=round(length(POS[,2])*0.1),by=deltaPosition)){#nextPostition is increment size
  
   jpeg(paste("journey-step-", imageIndex, ".jpg", sep = ""))
   imageIndex = imageIndex+1
  
# position  1ere colonne le temps en ms, puis x puis y

   
#deb=Enreg[[session]]$EVENTS[ideb,1]
#fin=Enreg[[session]]$EVENTS[ifin,1]

#indices=which((Enreg[[session]]$POS[,1]>=deb)&(Enreg[[session]]$POS[,1]<=fin))

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
# draw the axes
plot(c(50,250),c(0,250),xlab='',ylab='',type="n")
#par(new=TRUE)
#plot(axes=TRUE,xlim=c(aa,bb),ylim=c(cc,dd))
plot(xy.sp, col=mescol, add=TRUE)

#plot(xy.sp)

#par(new=TRUE)
polygonsLabel(xy.sp, labels=lab, method="centroid",cex=1)

#draw the journey between two events
#par(new=TRUE)
currentPosition = position - deltaPosition

X=list()
Y=list()

X[1]=POS[position,2]
X[2]=POS[currentPosition,2]

Y[1]=POS[position,3]
Y[2]=POS[currentPosition,3]

arrows(POS[currentPosition,2],POS[currentPosition,3],POS[position,2],POS[position,3])

#lines(X,Y)
#

dev.off()
}