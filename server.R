library("maps")
library(mapdata)  
library("lattice")
library(maptools)   #useful tools such as reading shapefiles
library(mapproj)
library(ggmap)
library(RCurl)
library(roxygen2)
library(devtools)

source("EBSmaps.R")

shinyServer(
 function(input,output){
  output$map <- renderPlot({
   sex<-switch(input$sex,"Male" = "male","Female" = "female")
   scalePop<-switch(input$scale,"Species" = 0,"All stocks" = 1)
   species<-switch(input$var,
		"Opilio" = "Opilio",
		"Red king" = "RKC",
		"Tanner E" = "TannerE",
		"Tanner W" = "TannerW",
		"Hair" = "Hair")
   year<- input$slider - 1974
   BinSize<-5
   LengthBin<-seq(0,255,BinSize)
   len1<-which((abs(LengthBin-min(input$slider2)))==min(abs(LengthBin-min(input$slider2))))
   len2<-which((abs(LengthBin-max(input$slider2)))==min(abs(LengthBin-max(input$slider2))))
   
   EBSmaps(sex,species,year,len1,len2,scalePop)
 })
 }
)