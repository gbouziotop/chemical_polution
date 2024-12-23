rm(list=ls(all=TRUE))
library(gstat) #model spatial residuals
library(mgcv)
library(maps)
library(mapdata)
library(RColorBrewer)
library(akima)
library(maptools)
library(classInt)
library(scales)
library(plot3D)
load("mapforblacksea.RData")
source("Helper_plotMapPoints.R")
source("Helper_mefilled.contour.R")
ylim=c(44,47)
xlim=c(27,34)
pred.points <- read.table("./prediction_points_black.txt", header = TRUE, sep="\t")

pred.points<-rbind(
  cbind(x=seq(from=29,to=30,length.out=length(seq(from=44.7, to=45.5,by=0.01))),seq(from=44.7, to=45.5,by=0.01)),
  cbind(x=seq(from=29,to=30,length.out=length(rev(seq(from=44.7, to=45.5,by=0.01)))),rev(seq(from=44.7, to=45.5,by=0.01))),
  
  cbind(x=seq(from=30,to=31,length.out=length(seq(from=44, to=44.5,by=0.01))),seq(from=44, to=44.5,by=0.01)),
  cbind(x=seq(from=30,to=31,length.out=length(rev(seq(from=44, to=44.5,by=0.01)))),rev(seq(from=44, to=44.5,by=0.01))),
  
  cbind(x=seq(from=31,to=32,length.out=length(seq(from=44, to=47,by=0.01))),seq(from=44, to=47,by=0.01)),
  cbind(x=seq(from=31,to=32,length.out=length(rev(seq(from=44, to=47,by=0.01)))),rev(seq(from=44, to=47,by=0.01))),
  
  cbind(x=seq(from=32,to=33,length.out=length(seq(from=44, to=45,by=0.01))),seq(from=44, to=45,by=0.01)),
  cbind(x=seq(from=32,to=33,length.out=length(rev(seq(from=44, to=45,by=0.01)))),rev(seq(from=44, to=45,by=0.01))),
  
  cbind(x=seq(from=33,to=34,length.out=length(seq(from=42.5, to=43,by=0.01))),seq(from=42.5, to=43,by=0.01)),
  cbind(x=seq(from=33,to=34,length.out=length(rev(seq(from=42.5, to=43,by=0.01)))),rev(seq(from=42.5, to=43,by=0.01))),
  
  cbind(x=seq(from=34,to=35,length.out=length(seq(from=42.5, to=43,by=0.01))),seq(from=42.5, to=43,by=0.01)),
  cbind(x=seq(from=34,to=35,length.out=length(rev(seq(from=42.5, to=43,by=0.01)))),rev(seq(from=42.5, to=43,by=0.01))),
  
   cbind(x=seq(from=35,to=36,length.out=length(seq(from=42.5, to=43,by=0.01))),seq(from=42.5, to=43,by=0.01)),
   cbind(x=seq(from=35,to=36,length.out=length(rev(seq(from=42.5, to=43,by=0.01)))),rev(seq(from=42.5, to=43,by=0.01)))
)

pred.points<-as.data.frame(pred.points)
names(pred.points)<-c("LON","LAT")

areas <- as.data.frame(seawater_survey2016) #read.table(paste("xcms_for_tserpes_analysis2.csv"), header = TRUE, sep=",", stringsAsFactors=FALSE)
t<-1; to_be_removed<-c()
u<-1
for(t in 6:ncol(areas)){
  tmp<-sum(areas[,t], na.rm=TRUE)
  if(tmp==0){
    print(t)
    to_be_removed[u]<-t
    u<-u+1
  } 
}
areas<-areas[,-to_be_removed]
#Remove QCs
#areas<-areas[-c(55:65),]
#Remove other sampling points
#areas<-areas[-c(1:22,41:54),]




#####1st figure####
i<-1
pdf("point_plots/real.pdf", width = 7, height = 5,
    paper="special", fillOddEven=T)
input<-t(areas)
for(i in 6:ncol(areas)){ #ncol(areas)
  
  area<-data.frame(LAT=as.numeric(areas[,2]),
                   LON=as.numeric(areas[,3]),
                   Abundance=as.numeric(areas[,c(i)]),
                   mzrt=names(areas)[c(i)])
  
  plotMapPoints(area,ylim=c(44,47), xlim=c(27,34), col="white", fill=TRUE,
                type ="p", col_points="red", cex=1.2, 
                plotclr=c("#f2f0f7","#cbc9e2", "#9e9ac8","#756bb1","#54278f"),
                symbol.size=1.8)
  
  print(i)
}
dev.off()


#####2nd plot############
pdf("point_plots/predicted.pdf", width = 7, height = 5,
    paper="special", fillOddEven=T)
i<-35
for(i in 6:ncol(areas)){
  
  input<-data.frame(year=rep(2016,length(areas[,1])),
                    LAT=as.numeric(areas[,2]),
                    LON=as.numeric(areas[,3]),
                    Abundance=areas[,c(i)],
                    mzrt=names(areas)[c(i)],
                    names=areas$Sample)
  input$year<-as.factor(input$year)
  
  area<-input
  area$Abundance[area$Abundance==0]<-1
  m1 <- gam(Abundance~LON+LAT,family=Gamma(link=log),data=area)
  #summary(m1)
  #anova.gam(m1)
  #gam.check(m1,type=c("deviance"))
  
  model.res <- resid(m1, type="deviance")
  res.grid <- interp(area$LON, area$LAT, model.res, duplicate="median")
  mypalette <- palette(gray(seq(.9,.4,len=6)))
  
  mydata2 <- data.frame(model.res, area$LON, area$LAT)
  coordinates(mydata2) <- c("area.LON", "area.LAT")

  pred <- predict(m1, pred.points, type="link")

  min(pred);max(pred)
  pred <- rescale(pred, c(1,100))
  mygrid  <- interp(x=pred.points$LON, y=pred.points$LAT, z=pred, duplicate="mean",linear=FALSE)
  mydataare <- cbind(pred.points$LON,pred.points$LAT,pred)
  # plot(mydataare[,1],mydataare[,2],col=pred)
  

  #windows(width=950,heigh=650)
  mefilled.contour(mygrid, xlim=xlim, ylim=ylim, zlim=c(0,100),
                   col=rev(heat.colors(10)), nlev=10, #numberoflevels
                   xlab="Longitude", ylab="Latitude",
                   main="", # paste(area$mzrt[1]),
                   plot.axes=(map(database="world2", add=T, resolution=0, col="white", fill=T,bg="light blue", map.axes())))
  
  axis(side=1, at = seq(from=27,to=43,by=1),labels = T)
  
  
  text(29.7,46.2,"Ukraine", cex = 1.3)
  text(27.8,44.6,"Romania", cex = 1.3)
  text(28.9,45.18,"Danube", cex = 2.0,font=2, col="#ADD8E6")
  
  points(y=c(area$LAT),x=c(area$LON))
  
  
  
 }
dev.off()
  
  
write.csv(areas,"point_plots/area.csv")