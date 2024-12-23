plotMapPoints<-function(area=area, xlim=c(19.5,29.5), ylim=c(34.8,41.8), col="grey60", fill=T,
                        type ="p", col_points="red",cex=0.5,
                        plotclr=c("pink1","red", "red4","black"), symbol.size=1.1){
  library(gstat)
  library(mgcv)
  library(maps)
  library(mapdata)
  library(RColorBrewer)
  library(akima)
  library(maptools)
  library(classInt)
  library(scales)
  
  #area<-area[area$Abundance<5000,]
 # map(database = "worldHires", xlim=xlim, ylim = ylim, resolution = 0, col=col, fill=fill)
  
  #-------Display observations on a map------------
  #let see the data we have
  #pdf(name)
  min(area$Abundance)
  max(area$Abundance)
  min(log(area$Abundance))
  max(log(area$Abundance))
  
  
  #Map the raw data - bubble plots. i want to view where is the most abandant area
  plotvar <- area$Abundance
  #plotvar <- log(area$Abundance) 
  #number of color is 4 (quite good choice)

  #plotclr<-palette(gray(seq(0.8,0,len=4)))
  #choose four different colors. wants red color for high values let's say
  plotclr<-plotclr
  nclr <- length(plotclr)
  #display.brewer.all(n=NULL, type="all", select=NULL, exact.n=TRUE)# display palettes
  #plotclr <- brewer.pal(nclr,"BuPu")
  #plotclr <- brewer.pal(nclr,"Reds")
  #plotclr <- plotclr[nclr:1] # reorder colors if appropriate
  #max.symbol.size=3
  #min.symbol.size=1
  #class <- classIntervals(plotvar, style="equal",n=4)
  
  #very important command. we ask R to get our dataset and difine different classes so that it will give different colors to different classes
  #style is how to spit the data in my classes

  class <- classIntervals(plotvar, n=nclr, style="quantile")
  
  #let's view the classes
  class

  #the next is to put colors in the observations
  colcode <- findColours(class, plotclr)

        
  #size of the bullets
  
  
 
  #symbol.size <- ((plotvar-min(plotvar))/(max(plotvar)-min(plotvar))*(max.symbol.size-min.symbol.size)+min.symbol.size)
  map(database = "world", xlim=xlim, ylim = ylim, resolution = 0, col=col, fill=fill,bg="white",
      xlab="Longitude",ylab="Latitude")
  map.axes()
  points(area$LON, area$LAT, pch=16, type="p",col=colcode,cex=symbol.size,xlab="Longitude",ylab="Latitude")
  #plot(area$LON, area$LAT, pch=16, type="p",col="red",cex=symbol.size,xlab="Longitude",ylab="Latitude",xlim=c(22,29),ylim=c(35,41),axes=F)
  #vazei to parathema
  legend<-names(attr(colcode, "table"))
#  legend2<-c(); i<-1;
#  for(i in 1:length(legend)){
#    legend2[i]<-
#      paste("[",strsplit(legend[i],"[[:punct:]]")[[1]][2],",",
#                      strsplit(legend[i],"[[:punct:]]")[[1]][3],")",sep="")
#    if(i==length(legend)) legend2[i]<-paste("[",strsplit(legend[i],"[[:punct:]]")[[1]][2],",",
#                                            strsplit(legend[i],"[[:punct:]]")[[1]][3],"]",sep="")
#  }

  # legend("topright", legend=legend, fill=attr(colcode, "palette"), cex=1.5, bty="n")
  
  text(34.5,41,"Turkey", cex = 1)
  text(29.7,46.2,"Ukraine", cex = 1)
  text(40.5,45.0,"Russia", cex = 1)
  text(42.4,42.4,"Georgia", cex = 1)
  
  text(27.7,43.6,"Bulgaria", cex = 1)
  text(27.8,44.6,"Romania", cex = 1)  
  
  text(29,45.18,"Danube", cex = 2.0,font=2, col="#ADD8E6")  
  #text(34.3,45.4,"Crimea", cex = 1)  
  # title(input$mzrt[1])

  #abline(44,0)
  #abline(0,32)
    #dev.off()
}
