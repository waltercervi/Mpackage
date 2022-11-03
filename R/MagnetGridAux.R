doReprojShift<-function(map,cropping){
  map=projectRaster(map, res=GridcellSize, crs=projection(MapREG), method="bilinear")
  if (cropping==TRUE){
    map=crop(map,MapREG)
    map[is.na(map)]<-0
  }
  #Required to combine rasters
  shiftX=GridcellSize*round(xmin(map)/GridcellSize)-xmin(map)
  shiftY=GridcellSize*round(ymin(map)/GridcellSize)-ymin(map)
  map=shift(map,dx=shiftX,dy=shiftY)
}

doCreateBaseline<-function(){
  for (s in 1:length(SectorsNm)){
    print(paste("Baseline -",SectorsNm[s]))
    #s=2
    fileNm=paste("MagnetGridR/SpatialData/ThematicMaps/SectorLandUseMaps/",Scenarios,"/",SectorsNm[s],"Area",sep="") #!! World
    LUmap=ReadRdaTab(fileNm)
    LUmap=subset(LUmap,LUmap$ISO3==Scenarios)[,c(1,2,4)]
    coordinates(LUmap) <- ~ X + Y
    gridded(LUmap) <- TRUE
    LUmap <- raster(LUmap)
    projection(LUmap)=CRS.WGS84
    #Shift is required to combine rasters
    LUmap=doReprojShift(LUmap,TRUE)
    writeRaster(LUmap,paste("MagnetGridR/SpatialData/InputData_LandUseModel/",Scenarios,"/2014/LandUse/",SectorsNm[s],".tif",sep=""), overwrite=T)
  }
}

#doCreateSuitMap() #???? Not possible, problem with intersectMap
# doCreateSuitMap<-function(){
#   dir.create(paste("MagnetGridR/SpatialData/InputData_LandUseModel/",Scenarios,"/",Years[i],"/SuitMap/",sep=""), showWarnings = FALSE)
#   for (s in 1:length(SectorsNm)){
#     LUmap <- raster(paste("MagnetGridR/SpatialData/InputData_LandUseModel/",Scenarios,"/",Years[i-1],"/LandUse/",SectorsNm[s],".tif",sep=""))
#     if (s==1){intersectMap=LUmap}
#     fileNm=paste("MagnetGridR/SpatialData/InputData_LandUseModel/",Scenarios,"/",Years[i],"/NPV/NPV_",SectorsNm[s],".tif",sep="")
#     SuitMap=crop(raster(fileNm),MapREG)
#     SuitMap=doReprojShift(SuitMap,FALSE)
#     SuitMap=raster::intersect(SuitMap,intersectMap)# fix the dimensions
#     # opportunity costs
#     OppoC <- LUmap/GridcellSize * SuitMap
#     
#     # sunk costs
#     Inv <- eval(parse(text=paste("Inv_",SectorsNm[s],"=InvestmentCosts$",SectorsNm[s],sep="")))
#     InvC <- LUmap/GridcellSize * Inv
#     
#     SuitMap <- SuitMap - OppoC - InvC
#     
#     writeRaster(SuitMap,paste("MagnetGridR/SpatialData/InputData_LandUseModel/",Scenarios,"/",Years[i],"/SuitMap/",SectorsNm[s],".tif",sep=""), overwrite=T)
#     
#   }
# }

doAllocation<-function(Niter){
  SlowFactor=1 #!!! Moet <= 1 zijn!!!
  StopResult=99999
  #
  # !!!! Als eerder stopt: controleren of Suit - Exo voldoende voor demand
  #
  IterResults=data.frame(matrix(,nrow=200,ncol=length(SectorsNm)+1))
  names(IterResults)=c("Iter",SectorsNm)
  Nsame=0
  Niter=50
  for (iter in 1:Niter){#20){
    IterResults[iter,1]=iter
    print(iter)
    for (s in 1:length(SectorsNm)){
      if (iter==1){
        eval(parse(text=paste("Pa",SectorsNm[s],"=1",sep="")))
        #Suit_xx only as placeholder for grid
        eval(parse(text=paste("Pb=Suit_",SectorsNm[s],sep="")))
        Pb[Pb>-999]<-(1/exp(200))
      }
      eval(parse(text=paste("PbExp=Pb*exp(1*Suit_",SectorsNm[s],")",sep="")))
      eval(parse(text=paste("M_",SectorsNm[s],"=Pa",SectorsNm[s],"*PbExp",sep="")))
      eval(parse(text=paste("M_",SectorsNm[s],"[M_",SectorsNm[s],">",Km2PerGrid*MaxCover[s],"]<-",Km2PerGrid*MaxCover[s],sep="")))
      eval(parse(text=paste("Pa",SectorsNm[s],"=D_",SectorsNm[s],"/(sum(na.omit(getValues(PbExp))))",sep="")))
    }
    for (s in 1:length(SectorsNm)){
      eval(parse(text=paste("Msector=Pa",SectorsNm[s],"*exp(1*Suit_",SectorsNm[s],")",sep="")))
      if (s==1){
        M_All=Msector
      }else{
        M_All=M_All+Msector
      }
    }
    SlowFactor=1-min(0.99999,0.99999*iter/round(Niter/2))
    ExoFactor=1#(iter/Niter)^0.75
    Pb=(1-SlowFactor)*Pb + SlowFactor*(Km2PerGrid-ExoFactor*Exo)/M_All
    Pb[is.na(Pb)]<-0
    for (s in 1:length(SectorsNm)){
      eval(parse(text=paste("IterResults[iter,s+1]=round(D_",SectorsNm[s],"-sum(na.omit(getValues(M_",SectorsNm[s],"))),0)",sep="")))
    }
    print(IterResults[iter,])
    SumIter=sum(abs(IterResults[iter,2:(length(SectorsNm)+1)]))
    print(SumIter)
    if (iter > 4 & SumIter>=StopResult){#&max(abs(IterResults[iter,2:7]))<0.25){
      Nsame = Nsame + 1
      print(Nsame)
      if (Nsame >= 2){
        break
      }
    }else{
      StopResult=SumIter
    }
  } #closing iteration

  # CHECK total allocation : if max > 100 then one or more crops too constrained
  #Summation=M_c_b+M_pdr+M_wht+M_gro+M_osd+M_v_f+M_pbf+M_ctl
  #plot(Summation);Summation

  for (x in 1:length(SectorsNm)){
    eval(parse(text=paste("Demand=round(D_",SectorsNm[x],")",sep="")))
    eval(parse(text=paste("SumMap=round(sum(na.omit(getValues(M_",SectorsNm[x],"))))",sep="")))
    MainTxt=paste(SectorsNm[x],";Demand=",Demand,";sumMap=",SumMap,sep="")
    eval(parse(text=paste("plot(M_",SectorsNm[x],",main=\"",MainTxt,"\")",sep="")))
    if (Demand>1.0001*SumMap){
      print (paste("WARNING: crop",SectorsNm[x],"not able to allocate demand (constrained)!!!"))
      print (paste("  ->",round(Demand-SumMap),"ha [",round(100*(Demand-SumMap)/Demand,2),"% ] is not allocated!"))
    }
    if (SumIter<10){
      temp=eval(parse(text=paste("M_",SectorsNm[x],"=M_",SectorsNm[x],"*",Km2PerGrid,sep="")))
      dir.create(paste("MagnetGridR/SpatialData/InputData_LandUseModel/",Scenarios,"/",Years[i],"/LandUse/",sep=""), showWarnings = FALSE)
      writeRaster(temp,paste("MagnetGridR/SpatialData/InputData_LandUseModel/",Scenarios,"/",Years[i],"/LandUse/",SectorsNm[x],".tif",sep=""), overwrite=T)
    }
    
  } #closing last chunk
  
}

doCreateExoMap<-function(){
  # relaxExoNoLU : if demand more than available cropland then exo based on LU relaxed
  sumDemand=0
  for (x in 1:length(SectorsNm)){
    eval(parse(text=paste("sumDemand=sumDemand+D_",SectorsNm[x],sep="")))
  }
  sumLU=cellStats(LUtotal, 'sum')*Km2PerGrid
  if (sumDemand>sumLU){
    relaxExoNoLU=1.05*sumDemand/sumLU
  }else{
    relaxExoNoLU=1.05
  }

  # CREATE EXOGENOUS MAP -------------------------------------------------------------------------------------------------------------
  #
  ExogenousNm=GetINI("ExogenousLandUsesLis",1)
  #ExogenousNm = ExogenousNm[4:5]
  out=stack()
  for (e in 1:length(ExogenousNm)){
    #e=1
    fileNm=paste("MagnetGridR/SpatialData/InputData_LandUseModel/",Scenarios,"/",Years[i],"/Exogenous/",ExogenousNm[e],".tif",sep="")
    ExoMap=crop(raster(fileNm),MapREG)
    out <- stack(out, ExoMap)
  }
  out[is.na(out)]<-0
  ExoMap <- sum(out)
  ExoMap[ExoMap>1]<-1
  plot(ExoMap)
  
  # Water=raster("MagnetGridR/SpatialData/FRA/Water.tif")
  # Urban=raster("MagnetGridR/SpatialData/FRA/Urban.tif")
  # ExoWU=Water+Urban
  # ExoWU[ExoWU>1]<-1
  # ExoWU=crop(ExoWU,MapREG)
  
  ExoWU=doReprojShift(ExoMap,TRUE)
  ExoWU=raster::intersect(ExoWU,intersectMap)
  
  ExoCrop=1-relaxExoNoLU*LUtotal
  
  cellStats(1-ExoCrop, 'sum')*Km2PerGrid;sumDemand
  #ExoCrop=ExoCrop-minValue(ExoCrop)
  #ExoCrop=ExoCrop/maxValue(ExoCrop)
  ExoCrop[ExoCrop<0]<-0
  plot(ExoCrop)
  plot(ExoWU)
  ExoWU[is.na(ExoWU)]<-0
  Exo=ExoWU+ExoCrop
  Exo[Exo>1]<-1
  Exo[is.na(Exo)]<-0
  (sumExo=cellStats(1-Exo, 'sum')*Km2PerGrid);sumDemand
  Exo=Exo*Km2PerGrid
  plot(Exo)
  return(Exo)
}