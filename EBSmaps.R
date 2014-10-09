EBSmaps<-function(sex,species,year,length1,length2,allZlim)
{
 #sex<-"male"
 #species<-"RKC"
 #year<-23
 #length1<-1
 #length2<-20
 #allZlim<-0
 #==read in density and locations based on above
  sexUse<-"F"
  colUse<-"#FF000088"
 if(sex=='male')
  {
  sexUse<-"M"
  colUse<-"#0000FF88"
  }
#==this "if then" abomination brought to you by excel's line limit
 if(species=="Opilio" &year<(2000-1974))
 {
 species<-"Snow2000"
 yearUse<-year
 
 urlfile<-paste("https://raw.githubusercontent.com/szuwalski/CrabMaps/master/",species,"_",sexUse,".csv",sep="")
 Density<-(read.csv(urlfile))
 urlfile<-paste("https://raw.githubusercontent.com/szuwalski/CrabMaps/master/",species,"_LOC.csv",sep="")
 AllStnLoc<-matrix(as.numeric(unlist(read.csv(urlfile))),ncol=2)
 urlfile<-paste("https://raw.githubusercontent.com/szuwalski/CrabMaps/master/",species,"_STNall.csv",sep="")
 AllStation<-as.vector(unlist(read.csv(urlfile)))
 urlfile<-paste("https://raw.githubusercontent.com/szuwalski/CrabMaps/master/",species,"_STNyr.csv",sep="")
 StationYr<-as.matrix(read.csv(urlfile))
#  urlfile<-paste("https://raw.githubusercontent.com/szuwalski/CrabMaps/master/",species,"_lengths",sexUse,".RData",sep="")
#  load(url(urlfile))
 
 }
 if(species=="Opilio" &year>=(2000-1974))
 { 
 species<-"Snow2014"
 yearUse<-(year-25)
 urlfile<-paste("https://raw.githubusercontent.com/szuwalski/CrabMaps/master/",species,"_",sexUse,".csv",sep="")
 Density<-(read.csv(urlfile))
 urlfile<-paste("https://raw.githubusercontent.com/szuwalski/CrabMaps/master/",species,"_LOC.csv",sep="")
 AllStnLoc<-matrix(as.numeric(unlist(read.csv(urlfile))),ncol=2)
 urlfile<-paste("https://raw.githubusercontent.com/szuwalski/CrabMaps/master/",species,"_STNall.csv",sep="")
 AllStation<-as.vector(unlist(read.csv(urlfile)))
 urlfile<-paste("https://raw.githubusercontent.com/szuwalski/CrabMaps/master/",species,"_STNyr.csv",sep="")
 StationYr<-as.matrix(read.csv(urlfile))
#  urlfile<-paste("https://raw.githubusercontent.com/szuwalski/CrabMaps/master/",species,"_lengths",sexUse,".RData",sep="")
#  load(url(urlfile))
 }

 if(species!="Snow2014" & species!="Snow2000")
 {
 yearUse<-year
#  urlfile<-paste("https://raw.githubusercontent.com/szuwalski/CrabMaps/master/",species,"_",sexUse,".csv",sep="")
#  Density<-(read.csv(urlfile))
#  urlfile<-paste("https://raw.githubusercontent.com/szuwalski/CrabMaps/master/",species,"_LOC.csv",sep="")
#  AllStnLoc<-matrix(as.numeric(unlist(read.csv(urlfile))),ncol=2)
#  urlfile<-paste("https://raw.githubusercontent.com/szuwalski/CrabMaps/master/",species,"_STNall.csv",sep="")
#  AllStation<-as.vector(unlist(read.csv(urlfile)))
#  urlfile<-paste("https://raw.githubusercontent.com/szuwalski/CrabMaps/master/",species,"_STNyr.csv",sep="")
#  StationYr<-as.matrix(read.csv(urlfile))
# #  urlfile<-paste("https://raw.githubusercontent.com/szuwalski/CrabMaps/master/",species,"_lengths",sexUse,".RData",sep="")
#  load(url(urlfile))
 print("ok1")
 urlfile<-paste("https://raw.githubusercontent.com/szuwalski/CrabMaps/master/",species,"_",sexUse,".csv",sep="")
 Density<-(read.csv(textConnection(urlfile)))
 print("ok2")
 urlfile<-paste("https://raw.githubusercontent.com/szuwalski/CrabMaps/master/",species,"_LOC.csv",sep="")
 AllStnLoc <-matrix(as.numeric(unlist(read.csv(textConnection(urlfile)))),ncol=2)
 print("ok3")
 urlfile<-paste("https://raw.githubusercontent.com/szuwalski/CrabMaps/master/",species,"_STNall.csv",sep="")
 AllStation<-as.vector(unlist(read.csv(textConnection(urlfile))))
 print("ok4")
 urlfile<-paste("https://raw.githubusercontent.com/szuwalski/CrabMaps/master/",species,"_STNyr.csv",sep="")
 StationYr <-as.matrix(read.csv(textConnection(urlfile)))
 print("ok5")
#  urlfile<-paste("https://raw.githubusercontent.com/szuwalski/CrabMaps/master/",species,"_lengths",sexUse,".RData",sep="")
#  download.file(urlfile,"tempCrab")
#  load("tempCrab")
#  print("ok6")
 }

urlfile<-"https://raw.githubusercontent.com/szuwalski/CrabMaps/master/MaxDensities.csv"
maxDen<-read.csv(textConnection(urlfile))

 if(allZlim==1)
  useZlim<-max(log(maxDen[,2:3]))
 if(allZlim==0)
  useZlim<-log(maxDen[which(maxDen[,1]==species),2])
 if(sexUse=="F")
  useZlim<-log(maxDen[which(maxDen[,1]==species),3])

 if(sexUse=="M")
#   useLens<-NatLengthM
  useLens<-NULL
if(sexUse=="F")
  useLens<-NULL
# useLens<-NatLengthF
useDensity<-log(as.numeric(Density[yearUse,]))
for(x in 1:ncol(Density))
{
 tempLen<-useLens[yearUse,x,]
 if(sum(tempLen)>0)
 {
 fracInRng<-sum(tempLen[length1:length2])/sum(tempLen)
 useDensity[x]<-fracInRng*useDensity[x]
 }
}

 #==plot map
state.map <- map('worldHires', xlim=c(-175, -155.9), ylim=c(50, 65.5),
                  plot = FALSE, fill = TRUE,col='grey85')

#state.map<-get_map(location = c(lat=57,lon=-165), source = "google", zoom = 5,maptype="satellite")
# temp<-ggmap(state.map)

tempLon<-AllStnLoc[match(StationYr[yearUse,],AllStation,),2]
tempLat<-AllStnLoc[match(StationYr[yearUse,],AllStation,),1]

state.info <- data.frame(density = useDensity,
 long = tempLon, lat = tempLat)

state.info<-state.info[rowSums(is.na(state.info))!=3, ]
state.info[is.na(state.info),1]<-0
panel.3dmap <- function(..., rot.mat, distance, xlim,
     ylim, zlim, xlim.scaled, ylim.scaled, zlim.scaled) {
     scaled.val <- function(x, original, scaled) {
         scaled[1] + (x - original[1]) * diff(scaled)/diff(original)
     }
     m <- ltransform3dto3d(rbind(scaled.val(state.map$x,
         xlim, xlim.scaled), scaled.val(state.map$y, ylim,
         ylim.scaled), zlim.scaled[1]), rot.mat, distance)
     panel.lines(m[1, ], m[2, ], col = "grey76")
 }

 pl <-cloud(useDensity~ long + lat, state.info, panel.3d.cloud = function(...) {
     panel.3dmap(...)
    panel.3dscatter(...)
 }, type = "h", scales = list(draw = TRUE), zoom = 1.1,
     xlim = state.map$range[1:2], ylim = state.map$range[3:4],
     xlab = NULL, ylab = NULL, zlab = NULL, aspect = c(diff(state.map$range[3:4])/diff(state.map$range[1:2]),
         0.3), panel.aspect = 0.75, lwd = 2, screen = list(z = 30,
         x = -60), par.settings = list(axis.line = list(col = "transparent"),
         box.3d = list(col = "transparent", alpha = 0)),col=colUse,zlim=c(0,useZlim))

 print(pl)
}
EBSmaps("male","RKC",27,5,30,0)
