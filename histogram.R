# How to draw a simple histogram in R

# Loading of the dataset

data(iris)
str(iris)

# Graph

hist(iris$Petal.Length[iris$Species=="setosa"], # choose variable and subset
     axes=FALSE,                                # no axes, you can add each separetedly 
     ann=FALSE,                                 # remove graph labels
     freq=FALSE,                                # no frequency graph
     breaks = 10,                               # number of breaks
     col = "grey",                              # colour
     xlim=c(1,1.9),                             # limits x axis
     ylim=c(0,2.6)                              # limits y axis
     )

axis(1) # adds x axis
axis(2) # adds y axis

title(main="Histogram - Petal Length", cex.main=1.5, line=2.2) # main title
title(main="Setosa species", cex.main=1.05, line=0.9)          # sub title 
title(xlab="values", ylab="density")                           # axes titles

# Distribution curve of the data

lines(density(na.omit(iris$Petal.Length[iris$Species=="setosa"])), # draws density plot and removes missing data
      lty=3                                                        # adds fine dashed line
      ) 

# Added: theoretical distribution curve

sequence <- seq(min(iris$Petal.Length[iris$Species=="setosa"], na.rm=TRUE), # minimum
                max(iris$Petal.Length[iris$Species=="setosa"], na.rm=TRUE), # maximum
                abs(max(iris$Petal.Length[iris$Species=="setosa"], na.rm=TRUE) - min(iris$Petal.Length[iris$Species=="setosa"], na.rm=TRUE)) / 1000) # absolute distance

lines(sequence, dnorm(sequence,                                             # draws a normal distribution curve on "sequence"
                      mean=mean(iris$Petal.Length[iris$Species=="setosa"]), # takes the mean
                      sd=sd(iris$Petal.Length[iris$Species=="setosa"])),    # standard deviation
      lty=1,                                                                # draws a line
      lwd=1                                                                 # plotted line width
      )

legend(1.6, 2.5,                # places a legend at the appropriate place
       c("Data","Normal Dist"), # puts text in the legend        
       lty=c(3,1),              # gives the legend appropriate symbols (in this case, lines)
       lwd=c(1,1)               # line width
       )
