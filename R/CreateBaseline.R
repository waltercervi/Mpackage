#' Create LandUse baseline
#'
#' @return
#' @export
#'
#' @examples
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
