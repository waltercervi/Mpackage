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
    LUmap <- raster(paste("MagnetGridR/SpatialData/InputData_LandUseModel/",Scenarios,"/SectorLandUse/",SectorsNm[s],".tif",sep=""))
    writeRaster(LUmap,paste("MagnetGridR/SpatialData/InputData_LandUseModel/",Scenarios,"/2014/LandUse/",SectorsNm[s],".tif",sep=""), overwrite=T)
  }
}
