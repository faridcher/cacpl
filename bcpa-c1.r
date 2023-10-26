library(bcpa)

load("movement\\CACP\\animove.rda") #load cheetah 56
c56 <- animove[animove$TrackID=='Cheetah56',]
str(c56)

data <- move::move(x = c56$X,y= c56$Y,time=c56$Time,data=c56,
             proj=CRS("+proj=longlat +ellps=WGS84"), animal="Cheetah")

c56.VT <- GetVT(c56)

#to ArcGIS for Kriging
animove.VT <- GetVT(animove)
animove.VT <- data.frame(lapply(animove.VT, as.character), stringsAsFactors=FALSE)
write.table(animove.VT, "cacp/mydata.txt", sep=',')

head(c56.VT)
c56.ws <- WindowSweep(c56.VT, "V*cos(Theta)", windowsize=30, progress=FALSE, K=2)

#head(c56.ws$ws)

dev.off()
par(mfrow=c(2,1), mar=c(0,4,0,1), oma=c(4,0,1,0), bty="l")

plot(c56.ws, type="smooth")
plot(c56.ws, type="smooth", threshold = 8)
plot.bcpa()

par(mfrow=c(2,1), mar=c(0,4,0,1), oma=c(4,0,1,0), bty="l")
plot(c56.ws, type="flat")
plot(c56.ws, type="flat", clusterwidth=3)

PathPlot(c56, c56.ws, type="flat", clusterwidth = 3, main="Flat BCPA")
PhasePlot(c56.ws, type="smooth", clusterwidth = 3)

#move(c56$Longitude,c56$Latitude,paste (c56$UTC.Date ,c56$UTC.Time,sep = ' '))
