library("plot3D")


##  2D Gaussian Kernal plot

## Generate x and y coordinates as sequences
x = seq(-4,4,0.2)
y = seq(-4,4,0.2)

# An empty matrix z
z = matrix(data=NA, nrow=length(x), ncol=length(x))

### Gaussian kernal generation to fill the z matrix.
sigma = 1.0
mux = 0.0
muy = 0.0
A = 1.0

for(i in 1:length(x))
{
  for(j in 1:length(y))
  {
    
    z[i,j] =  A * (1/(2*pi*sigma^2)) * exp( -((x[i]-mux)^2 + (y[j]-muy)^2)/(2*sigma^2) ) 
  }
}

### Now z is a matrix of dimension length(x) * length(y)

# Plotting surface with persp() function of default "Graphics" library in R.
## (Note: At this point, just give any x,y vector and z matrix of terrain to get your plot)

persp(x,y,z, theta=30, phi=50, r=2, shade=0.4, axes=TRUE,scale=TRUE, box=TRUE, 
      
      nticks=5, ticktype="detailed", zlim = c(0,0.2) , col="cyan", xlab="X-value", 
      
      ylab="Y-value", zlab="Z-value", main="Gaussian Kernal with persp()")

##--------------------------------------------------------


## Create next plot on a separate canvas.
#X11()


# Required for using persp3D() function below.
library(plot3D)


## We call persp3D function for same Gaussian kernal data generated above.
par(mai=c(0.2,0.2,0.2,0.4))
persp3D(x,y,z,theta=30, phi=10, axes=TRUE,scale=1, box=TRUE, nticks=5, 
        
        ticktype="detailed",xlab="X-value", ylab="Y-value", zlab="Z-value", 
        
        main="Gaussian Kernal with persp3D()")
