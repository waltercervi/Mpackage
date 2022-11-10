
rm(list = ls())
#mydrive="D:/WUR/MagnetGridModel/MagnetGridR"

#@WH Walter:
MagnetGridPath <<-"C:/MagnetGridModel/"  
#@WH Wil:
MagnetGridPath <<- as.character(read.csv("M:/MagnetGridPath.csv")[1,1])
MagnetGridPath <<- as.character(read.csv("C:/MagnetGridModel/MagnetGridPath.csv")[1,1])

setwd(paste(MagnetGridPath,sep=""))

library(raster)
library(maptools)
library(rgdal) #readOGR
library(dplyr)

StringAsFactor=FALSE
options(scipen=999)

source(paste(MagnetGridPath,"ModelConfiguration/SpatialDataAndSpatialCBAmodules/SpatialDataModuleScripts/INI.R", sep = ""))

#load from SUBDIR /Scripts 
#source(paste(MagnetGridPath,"MagnetGridR/Scripts","MagnetGridAux.R",sep="/"))
source(paste(MagnetGridPath,"MagnetGridR/Scripts","ReprojShift.R",sep="/"))
source(paste(MagnetGridPath,"MagnetGridR/Scripts","CreateExoMap.R",sep="/"))
source(paste(MagnetGridPath,"MagnetGridR/Scripts","CreateBaseline.R",sep="/"))
source(paste(MagnetGridPath,"MagnetGridR/Scripts","Allocation.R",sep="/"))
source(paste(MagnetGridPath,"MagnetGridR/Scripts","CreateSuitmap.R",sep="/"))
source(paste(MagnetGridPath,"MagnetGridR/Scripts","RescaleSuitmap.R",sep="/"))
source(paste(MagnetGridPath,"MagnetGridR/Scripts","CorrectLUtotal.R",sep="/"))

ProjectINI <<- read.csv(paste(MagnetGridPath,"ProjectINI.txt",sep=""))
ControlINI <<- read.csv(paste(MagnetGridPath,"ControlINI.txt",sep=""))
Scenarios <- GetINI("Scenarios",1)
Years <- (read.csv(paste(MagnetGridPath,"ScenarioSpecs",Scenarios,"INI_Years.csv",sep="/")) %>% subset(Select==1)) [,1] %>% as.numeric()

#_____________________________________________________________
#
# FUNCTIONS
#_____________________________________________________________
PlotRasterFromTable <- function(tab1) {
  coordinates(tab1) <- ~ X + Y
  gridded(tab1) <- TRUE
  rasterDF <- raster(tab1)
  plot(rasterDF, main=names(tab1))
}
ReadRdaTab<-function(Indicator){
  IndicatorTab = local({load(paste(Indicator,".rda",sep="")); environment()})
  return(IndicatorTab$"Tab")
}
#_____________________________________________________________
#
# SETTINGS & Region
#_____________________________________________________________
CRS.WGS84 <- CRS("+init=epsg:4326") # WGS 84
#MapREG=readOGR(paste("MagnetGridR/SpatialData/InputData_LandUseModel/",Scenarios,sep=""),"RegionArea")
#MapREG=crop(MapREG, extent(-374515.2, 795484.8, 4943932, 5983932)) # adapt this part
#plot(MapREG)

SectorsNm=rev(c("c_b","ctl","gro","ocr","osd","pbf","pdr","v_f","wht"))
#SectorsNm=c("c_b","ctl","gro","ocr","osd","pbf","pdr","v_f")
MaxCover =c( 1, 1, 1, 1, 1, 1, 1, 1, 1)

GridcellSize=10000
HaPerGrid=(GridcellSize/100)^2
Km2PerGrid=HaPerGrid/100
#condition <- c("baseline", "sim")

#_____________________________________________________________
#
################## MAGNETGRID MODELLING ########################-------------------------------------------------------------------------------

for (condition in c("baseline", "sim")){

####################### Baseline -------------------------------------------------------------------------------

if (condition=="baseline"){
  LandDemand=read.csv(paste("MagnetGridR/SpatialData/InputData_LandUseModel/",Scenarios,"/",Years[1],"/LandDemandTab.csv",sep=""))
  SectorsNm=colnames(LandDemand[,-1])[LandDemand[1,-1] > 10]
  doCreateBaseline()
} 

######################## Land simulation -------------------------------------------------------------------------------

if (condition=="sim") {
  
  for (i in 2:length(Years)){
    #i=2
    #_____________________________________________________________
    LandDemand=read.csv(paste("MagnetGridR/SpatialData/InputData_LandUseModel/",Scenarios,"/",Years[i],"/LandDemandTab.csv",sep=""))
    SectorsNm=colnames(LandDemand[,-1])[LandDemand[1,-1] > 10]
    
    InvestmentCosts=read.csv(paste("MagnetGridR/SpatialData/InputData_LandUseModel/",Scenarios,"/",Years[i],"/InvestmentCostsTab.csv",sep=""))
    #
    # Make SuitMap
    #
    print("create suitmaps.....")
    intersectMap=doCreateSuitMap()
    # Find minimum and maximum all crops for purpose of normalizing----------------------------------------------------------------------------
    MinCrops=99999999
    MaxCrops=-99999999
    for (s in 1:length(SectorsNm)){
      SuitMap=raster(paste("MagnetGridR/SpatialData/InputData_LandUseModel/",Scenarios,"/",Years[i],"/SuitMap/",SectorsNm[s],".tif",sep=""))
      if (minValue(SuitMap)<MinCrops) {MinCrops=minValue(SuitMap)}
      if (maxValue(SuitMap)>MaxCrops) {MaxCrops=maxValue(SuitMap)}
    }
    RangeNPV=MaxCrops-MinCrops
    #
    # Rescale SuitMap
    #
    MapsStack=doRescaleSuitMap()
    for (s in 1:length(SectorsNm)){
      eval(parse(text=paste("Suit_",SectorsNm[s],"=MapsStack[[",(s-1)*2+1,"]]",sep="")))
      eval(parse(text=paste("LU_",SectorsNm[s],"=MapsStack[[",(s-1)*2+2,"]]",sep="")))
    }
    LUtotal=MapsStack[[length(SectorsNm)*2+1]]
    #
    # Correct only those grids with sum LU > 100%
    #
    LUtotal=doCorrectLUtotal()
    #
    # Demand from MAGNET
    #
    for (x in 1:length(SectorsNm)){
      eval(parse(text=paste("D_",SectorsNm[x],"=LandDemand$",SectorsNm[x],sep="")))
    }
    #
    # Make Exo-map
    #
    Exo=doCreateExoMap()
    #
    # Allocation procedure
    #
    doAllocation(100,0.25) #Niter;beta
    
  }   #closing sim; end year
}     #closing else
    
} #closing condition    
    
    
#TEST
plot(raster("c:/MagnetGridModel/MagnetGridR/SpatialData/InputData_LandUseModel/GHA/2014/LandUse/v_f.tif"))
#plot(raster("c:/MagnetGridModel/MagnetGridR/SpatialData/InputData_LandUseModel/GHA/2020/SuitMap/v_f.tif"))
plot(raster("c:/MagnetGridModel/MagnetGridR/SpatialData/InputData_LandUseModel/GHA/2020/LandUse/v_f.tif"))
plot(raster("c:/MagnetGridModel/MagnetGridR/SpatialData/InputData_LandUseModel/GHA/2030/LandUse/v_f.tif"))
plot(raster("c:/MagnetGridModel/MagnetGridR/SpatialData/InputData_LandUseModel/GHA/2050/LandUse/v_f.tif"))

