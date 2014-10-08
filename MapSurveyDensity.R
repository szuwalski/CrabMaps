
library("maps")
library("lattice")
library(PBSmapping)
library(mapdata)    #some additional hires data
library(maptools)   #useful tools such as reading shapefiles
library(mapproj)
library("maps")
library("lattice")
library(ggmap)

for(y in 1:length(SurvYR))
{
 #==try ggmaps to put a google earth picture here? 
state.map <- map('worldHires', xlim=c(-175, -155.9), ylim=c(50, 65.5),
                  plot = FALSE, fill = TRUE,col='grey85')

#==make this work
#state.map<-get_map(location = c(lat=57,lon=-165), source = "google", zoom = 9,maptype="satellite")
 
#==this will change 
state.info <- data.frame(density = DensityM[y,],
 long = AllStnLoc[match(StationYr[y,],AllStation,),2],
 lat = AllStnLoc[match(StationYr[y,],AllStation,),1])

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

 pl <-cloud(density ~ long + lat, state.info, panel.3d.cloud = function(...) {
     panel.3dmap(...)
    panel.3dscatter(...)
 }, type = "h", scales = list(draw = TRUE), zoom = 1.1,
     xlim = state.map$range[1:2], ylim = state.map$range[3:4],
     xlab = NULL, ylab = NULL, zlab = NULL, aspect = c(diff(state.map$range[3:4])/diff(state.map$range[1:2]),
         0.3), panel.aspect = 0.75, lwd = 2, screen = list(z = 30,
         x = -60), par.settings = list(axis.line = list(col = "transparent"),
         box.3d = list(col = "transparent", alpha = 0)))

 print(pl)
}


