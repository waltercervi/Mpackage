#' Rescaling suitability maps
#'
#' @return
#' @export
#'
#' @examples
doRescaleSuitMap<-function(){
  NmLis=c()
    for (s in 1:length(SectorsNm)){
      # s=3
      print(paste(Years[i],SectorsNm[s]))

      # Land use map : read rda file -> select region -> shift operation (for combining) OPEN T-1 LAND USE MAP  TO BE MODELLED --------------
      LUmap <- raster(paste("MagnetGridR/SpatialData/InputData_LandUseModel/",Scenarios,"/",Years[i-1],"/LandUse/",SectorsNm[s],".tif",sep=""))

      SuitMap=raster(paste("MagnetGridR/SpatialData/InputData_LandUseModel/",Scenarios,"/",Years[i],"/SuitMap/",SectorsNm[s],".tif",sep=""))
      SuitMap[SuitMap<MinCrops]<-MinCrops
      SuitMap=SuitMap+(-1*MinCrops)
      SuitMap=SuitMap/RangeNPV
      SuitMap[SuitMap>1]<-1
      SuitMap=Km2PerGrid*SuitMap*MaxCover[s]
      SuitMap=crop(SuitMap,MapREG)

      # create
      eval(parse(text=paste("Suit_",SectorsNm[s],"=SuitMap",sep="")))
      eval(parse(text=paste("LU_",SectorsNm[s],"=LUmap",sep="")))
      if (s==1){
        LUtotal=LUmap
      }else{
        LUtotal=LUtotal+LUmap
      }
      if (s==1){
        RastStack=stack(SuitMap,LUmap)
      }else{
        RastStack=stack(RastStack,SuitMap,LUmap)
      }
      eval(parse(text=paste("NmLis=c(NmLis,\"Suit_",SectorsNm[s],"\",  \"LU_",SectorsNm[s],"\")",sep="")))
    }
  NmLis=c(NmLis,"LUtotal")
  RastStack=stack(RastStack,LUtotal=LUtotal)
  names(RastStack)=NmLis
  return(RastStack)
}


