

#A Problem with csgillespie solution appears, when You have an logarithmic X axis. The you will have a different length of the small bars on the right an the left side (the epsilon follows the x-values).
#You should better use the errbar function from the Hmisc package:
  
  d = data.frame(
    x  = c(1:5)
    , y  = c(1.1, 1.5, 2.9, 3.8, 5.2)
    , sd = c(0.2, 0.3, 0.2, 0.0, 0.4)
  )

##install.packages("Hmisc", dependencies=T)
library("Hmisc")

# add error bars (without adjusting yrange)
plot(d$x, d$y, type="n")
with (
  data = d
  , expr = errbar(x, y, y+sd, y-sd, add=T, pch=16,cex=.7, cap=.1)
)

# new plot (adjusts Yrange automatically)
#plot(d$x, d$y, type="n")
with (
  data = d
  , expr = errbar(x, y, y+sd, y-sd, add=F, pch=1, cap=.015, log="x")
)

