library(plotrix)
setwd("C:/Shiny/EBScrab")
RKC<-read.csv("C:/Users/Cody/Desktop/RKC/Red King Crab/PIRKC_survey.csv")
TannerE<-read.csv("C:/Users/Cody/Desktop/CrabSurvey/CRABHAUL_BAIRDI_ALL/CRABHAUL_BAIRDI_EAST166.csv")
TannerW<-read.csv("C:/Users/Cody/Desktop/CrabSurvey/CRABHAUL_BAIRDI_ALL/CRABHAUL_BAIRDI_WEST166.csv")
Snow2000<-read.csv("C:/Users/Cody/Desktop/CrabSurvey/CRABHAUL_ALL_OPILIO/CRABHAUL1975TO1999_OPILIO.csv")
Snow2014<-read.csv("C:/Users/Cody/Desktop/CrabSurvey/CRABHAUL_ALL_OPILIO/CRABHAUL2000TO2014_OPILIO.csv")
Hair<-read.csv("C:/Users/Cody/Desktop/CrabSurvey/HairCrab_CombinedCrabAndHaulData_CurrentDataSet_1980to2014/HairCrab_CombinedCrabAndHaulData_CurrentDataSet_1980to2014.csv")
datfiles<-c("RKC","TannerE","TannerW","Snow2000","Snow2014","Hair")

BinSize<-5
LengthBin<-seq(0,250,BinSize)

for(p in 2:length(datfiles))
{
survDAT<-get(datfiles[p])
drvYear<-as.numeric(substr(survDAT$CRUISE,1,4))
SurvYR<-unique(drvYear)
AllStation<-unique(survDAT$GIS_STATION)

#==plot GIS stations
AllStnLoc<-matrix(ncol=2,nrow=length(AllStation))
for(w in 1:length(AllStation))
 {
  temp<-survDAT[survDAT$GIS_STATION==AllStation[w],]
  AllStnLoc[w,1]<-temp$MID_LATITUDE[1]
  AllStnLoc[w,2]<-temp$MID_LONGITUDE[1]
 }

#==find densities
DensityM2<-matrix(nrow=length(SurvYR),ncol=length(AllStation))
DensityF2<-matrix(nrow=length(SurvYR),ncol=length(AllStation))
StationYr<-matrix(nrow=length(SurvYR),ncol=length(AllStation))
NatLengthM<-array(0,dim=c(length(SurvYR),length(AllStation),length(LengthBin)-1))
NatLengthF<-array(0,dim=c(length(SurvYR),length(AllStation),length(LengthBin)-1))


for(y in 1:length(SurvYR))
{
 yrDAT<-survDAT[drvYear==SurvYR[y],]
 fileyr<-SurvYR[y]
 stationsUNQ<-(unique(yrDAT$GIS_STATION))
#==density at station
 for(j in 1:length(stationsUNQ))
  {
   stationALL<-yrDAT[yrDAT$GIS_STATION==stationsUNQ[j],]
   StationYr[y,j]<-as.character(stationsUNQ[j])
   Hauls<-(unique(stationALL$HAUL))
   Males<-stationALL[stationALL$SEX==1,]
   Females<-stationALL[stationALL$SEX==2,]

#==densities across hauls in crabs per km^2
  tempDensM<-NULL
  tempDensF<-NULL
  for(k in 1:length(Hauls))
   {
    SampFactM<-Males$SAMPLING_FACTOR[which(Males$HAUL==Hauls[k])[1]] 
    AreaSweptM<-Males$AREA_SWEPT_VARIABLE[which(Males$HAUL==Hauls[k])[1]]
    tempDensM<-append(tempDensM,length(Males$HAUL==Hauls[k])*SampFactM/AreaSweptM)

    SampFactF<-Females$SAMPLING_FACTOR[which(Females$HAUL==Hauls[k])[1]] 
    AreaSweptF<-Females$AREA_SWEPT_VARIABLE[which(Females$HAUL==Hauls[k])[1]]
    tempDensF<-append(tempDensF,length(Females$HAUL==Hauls[k])*SampFactF/AreaSweptF)
    }
  DensityM2[y,j]<-mean(tempDensM)
  DensityF2[y,j]<-mean(tempDensF)
   

#==length frequencies at a station
  Males<-Males[!is.na(Males$LENGTH),]
  Females<-Females[!is.na(Females$LENGTH),]
  tempHAULm<-unique(Males$HAUL)
  tempHAULf<-unique(Females$HAUL)

#==if there are no repeated hauls, proceed as normal 
 if(length(tempHAULm)>0)
  if(!is.na(tempHAULm))
   if(length(tempHAULm)==1)
  {
   tempMhist<-weighted.hist(Males$LENGTH,breaks=LengthBin,w = Males$SAMPLING_FACTOR, plot=F)
   NatLengthM[y,j,]<-NatLengthM[y,j,]+tempMhist$counts
  }
 if(length(tempHAULf)>0)
  if(!is.na((tempHAULf)))
   if(length(tempHAULf)==1)
  {
   tempFhist<-weighted.hist(Females$LENGTH,breaks=LengthBin,w = Females$SAMPLING_FACTOR, plot=F)
   NatLengthF[y,j,]<-NatLengthF[y,j,]+tempFhist$counts
  }
 #==males====
 if(length(tempHAULm)>0)
  if(!is.na((tempHAULm)))
   if(length(tempHAULm)>1)
  {
   HaulSampNm<-rep(0,length(tempHAULm))
  #==find the avg number of samples==
  #==the number of observations in a haul * samplingfactor==
   for(q in 1:length(HaulSampNm))
    HaulSampNm[q]<-nrow(Males[Males$HAUL==tempHAULm[q],])*Males[Males$HAUL==tempHAULm[q],]$SAMPLING_FACTOR[1]
  #==find the length frequency ==
    tempMhist<-weighted.hist(Males$LENGTH,breaks=LengthBin,w = Males$SAMPLING_FACTOR, plot=F)
  #==mult avg sampN by length freq, add to total N at len (will give fractions)==
   if(any(!is.na(HaulSampNm)))
    NatLengthM[y,j,]<-NatLengthM[y,j,]+(tempMhist$density * mean(HaulSampNm,na.rm=T))
   }
  #==females==
 if(length(tempHAULf)>0)
  if(!is.na((tempHAULf)))
   if(length(tempHAULf)>1)
  {
   HaulSampNf<-rep(0,length(tempHAULf))
   for(q in 1:length(HaulSampNf))
    HaulSampNf[q]<-nrow(Females[Females$HAUL==tempHAULf[q],])*Females[Females$HAUL==tempHAULf[q],]$SAMPLING_FACTOR[1]
    temphFist<-weighted.hist(Females$LENGTH,breaks=LengthBin,w = Females$SAMPLING_FACTOR, plot=F)
   if(any(!is.na(HaulSampNf)))
    NatLengthF[y,j,]<-NatLengthF[y,j,]+(tempFhist$density * mean(HaulSampNf,na.rm=T))
   }
  }#stations
  }#year
write.csv(DensityM2,paste("C:/Shiny/EBScrab/",datfiles[p],"_M.csv",sep=""),row.names=F)
write.csv(DensityF2,paste("C:/Shiny/EBScrab/",datfiles[p],"_F.csv",sep=""),row.names=F)
write.csv(AllStnLoc,paste("C:/Shiny/EBScrab/",datfiles[p],"_LOC.csv",sep=""),row.names=F)
write.csv(AllStation,paste("C:/Shiny/EBScrab/",datfiles[p],"_STNall.csv",sep=""),row.names=F)
write.csv(StationYr,paste("C:/Shiny/EBScrab/",datfiles[p],"_STNyr.csv",sep=""),row.names=F)

tempNLM<-NULL
for(q in 1:50)
 tempNLM<-rbind(tempNLM,NatLengthM[,,q])
write.csv(tempNLM,paste("C:/Shiny/EBScrab/",datfiles[p],"_lengthM.csv",sep=""),row.names=F)
tempNLM<-NULL
for(q in 1:50)
 tempNLM<-rbind(tempNLM,NatLengthF[,,q])
write.csv(tempNLM,paste("C:/Shiny/EBScrab/",datfiles[p],"_lengthF.csv",sep=""),row.names=F)

}#species


library("maps")
library("lattice")
library(mapdata)    #some additional hires data


for(y in 1:length(SurvYR))
{
 library("maps")
 library("lattice")
 state.map <- map('worldHires', xlim=c(-175, -155.9), ylim=c(50, 65.5),
                  plot = FALSE, fill = TRUE,col='grey85')
#  S of 58.39, W of 168
 #state.map <- map('worldHires', xlim=c(-173, -167.5), ylim=c(52, 58),
  #                plot = FALSE, fill = TRUE,col='grey85')

state.info <- data.frame(density = DensityM2[y,],
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


