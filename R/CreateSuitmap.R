#' Create Suitability maps
#'
#' @return
#' @export
#'
#' @examples
 doCreateSuitMap<-function(){
    dir.create(paste("MagnetGridR/SpatialData/InputData_LandUseModel/",Scenarios,"/",Years[i],"/SuitMap/",sep=""), showWarnings = FALSE)
    for (s in 1:length(SectorsNm)){
      LUmap <- raster(paste("MagnetGridR/SpatialData/InputData_LandUseModel/",Scenarios,"/",Years[i-1],"/LandUse/",SectorsNm[s],".tif",sep=""))
      if (s==1){intersectMap=LUmap}
      fileNm=paste("MagnetGridR/SpatialData/InputData_LandUseModel/",Scenarios,"/",Years[i],"/NPV/NPV_",SectorsNm[s],".tif",sep="")
      SuitMap=crop(raster(fileNm),MapREG)
      # SuitMap=doReprojShift(SuitMap,FALSE)
      # SuitMap=raster::intersect(SuitMap,intersectMap)# fix the dimensions
      # # # opportunity costs
      #OppoC <- LUmap/GridcellSize * SuitMap
      OppoC <- (1 - LUmap/GridcellSize) * SuitMap

      # sunk costs
      Inv <- eval(parse(text=paste("Inv_",SectorsNm[s],"=InvestmentCosts$",SectorsNm[s],sep="")))
      #InvC <- LUmap/GridcellSize * Inv
      InvC <- (1 - LUmap/GridcellSize) * Inv

      SuitMap <- SuitMap - OppoC - InvC

      writeRaster(SuitMap,paste("MagnetGridR/SpatialData/InputData_LandUseModel/",Scenarios,"/",Years[i],"/SuitMap/",SectorsNm[s],".tif",sep=""), overwrite=T)

    }
   return(intersectMap)
 }

