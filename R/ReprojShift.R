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

