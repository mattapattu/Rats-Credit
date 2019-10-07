#library(data.tree)
library(stringi)

# print one rat/session if node is session
# or all sessions of one rat if node is one rat
# or all rat/sessions if agr is root node
print.plot.journeys=function(tree, noeud, boites){
    #noeud=Node$new("n")
    path=noeud$pathString
    #Drop the root name, which is not part of the path
    if(!noeud$isRoot){
      path=substr(path,str_locate(path,"/")[1],str_length(path)) 
    }
    pdf(paste(getwd(),"/maze",sep=""), onefile=FALSE)
    #Close to print pdf file
    plot.each.journey(NULL,boites,TRUE)
    dev.off()
    
    if(noeud$isRoot){
      #get all regions
      #regions=tree$Get('name', filterFun = function(x) x$level == 2)
      for(q in noeud$children){#get each region
        for(r in q$children){#get each rat
          for(s in r$children){#get each session
            pdf(paste(getwd(),"/",q$name,"/",r$name,
                      "/journey-",s$name,sep=""), onefile=FALSE)
            #Close to print pdf file
            plot.each.journey(s$POS,boites)
            dev.off()
          }
        }
      }
    }
    else if(str_detect(noeud$name,"region")){# detection of a region
      for(r in noeud$children){#get each rat
        for(s in r$children){#get each session
          pdf(paste(getwd(),path,"/",r$name,"/journey-",s$name,sep=""), onefile=FALSE)
          #Close to print pdf file
          plot.each.journey(s$POS,boites)
          dev.off()
        }
      }
    }
    else if(str_detect(noeud$name,"rat")){# not detecting session but rat
      for(n in noeud$children){#get each session
        pdf(paste(getwd(),path,"/journey-",n$name,sep=""), onefile=FALSE)
        #Close to print pdf file
        plot.each.journey(n$POS,boites)
        dev.off()
      }
    }
    else if(str_detect(noeud$name,"session")){
      #drop the session node which is not a directory
      path=substr(path,0,stri_locate_last_fixed(path,"/")[1]) 
      pdf(paste(getwd(),path,noeud$name,sep=""), onefile=FALSE)
      #Close to print pdf file
      plot.each.journey(noeud$POS,boites)
      dev.off()
    }
    else stop("error for printing journey - no node found")
    
  # FindNode(DATA,"session 1")$POS
  # for(session in 1:18){
  # ## Print pdf file
  #   pdf(paste("journey-session-",session))
  #   
  #   plot.journey(Enreg,session,ideb,ifin,boites,event=TRUE)
  #   #Close to print pdf file
  #   dev.off()
  # }
}
getSpatialPolygons=function(boxes){
  Nboites=length(boxes)
  #mescol=colorRampPalette(c('red','yellow','green','blue'))(Nboites)
  
  ##Plot the polygons and their labels
  lab=list() #the labels
  pol=list() #the polygons
  
  for(p in 1:Nboites){
    #polygon(boites[[p]][1,],boites[[p]][2,],col=mescol[p])
    
    pol[[p]]= Polygons(list(Polygon(cbind(boxes[[p]][1,],
                                          boxes[[p]][2,]))), 
                       ID = p)
    #    lab[[p]]=p
  } 

  lab[1]="e"
  lab[2]="f"
  lab[3]="g"
  lab[4]="c"
  lab[5]="d"
  lab[6]="h"
  lab[7]="i"
  lab[8]="j"
  lab[9]="a"
  lab[10]="b"
  lab[11]="k"
  
  xy.sp = SpatialPolygons(pol)
  plot.new()
  polygonsLabel(xy.sp, lab, method="centroid",cex=1)
  
  return(xy.sp)
}

plot.each.journey=function(xy,boites,aa=50,bb=250,cc=0,dd=250, empty=FALSE){
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
  if(!empty){
    lines(xy[,2],xy[,3])
  }
}

