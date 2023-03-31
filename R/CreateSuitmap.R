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
      SuitMap=raster(fileNm)

      # # # opportunity costs
      OppoC <- (1 - LUmap/HaPerGrid) * SuitMap

      # sunk costs
      Inv <- eval(parse(text=paste("Inv_",SectorsNm[s],"=InvestmentCosts$",SectorsNm[s],sep="")))

      InvC <- (1 - LUmap/HaPerGrid) * Inv

      SuitMap <- SuitMap - OppoC - InvC

      writeRaster(SuitMap,paste("MagnetGridR/SpatialData/InputData_LandUseModel/",Scenarios,"/",Years[i],"/SuitMap/",SectorsNm[s],".tif",sep=""), overwrite=T)

    }
   return(intersectMap)
 }

