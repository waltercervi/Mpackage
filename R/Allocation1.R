#' Title
#'#'
#' @param Niter
#' @param beta
#'
#' @return
#' @export
#'
#' @examples
doAllocation<-function(Niter, beta){
  #beta is a parameter to adjust the model sensitivity.
  SlowFactor=1 #!!! Moet <= 1 zijn!!!
  StopResult=99999
  #
  # !!!! Als eerder stopt: controleren of Suit - Exo voldoende voor demand
  #
  IterResults=data.frame(matrix(nrow=200,ncol=length(SectorsNm)+1))
  names(IterResults)=c("Iter",SectorsNm)
  Nsame=0
  #Niter=50
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
      eval(parse(text=paste("PbExp=Pb*exp(beta*Suit_",SectorsNm[s],")",sep="")))
      eval(parse(text=paste("M_",SectorsNm[s],"=Pa",SectorsNm[s],"*PbExp",sep="")))
      eval(parse(text=paste("M_",SectorsNm[s],"[M_",SectorsNm[s],">",Km2PerGrid*MaxCover[s],"]<-",Km2PerGrid*MaxCover[s],sep="")))
      eval(parse(text=paste("Pa",SectorsNm[s],"=D_",SectorsNm[s],"/(sum(na.omit(getValues(PbExp))))",sep="")))
    }
    for (s in 1:length(SectorsNm)){
      eval(parse(text=paste("Msector=Pa",SectorsNm[s],"*exp(beta*Suit_",SectorsNm[s],")",sep="")))
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
    #eval(parse(text=paste("plot(M_",SectorsNm[x],",main=\"",MainTxt,"\")",sep="")))
    if (Demand>1.0001*SumMap){
      print (paste("WARNING: crop",SectorsNm[x],"not able to allocate demand (constrained)!!!"))
      print (paste("  ->",round(Demand-SumMap),"ha [",round(100*(Demand-SumMap)/Demand,2),"% ] is not allocated!"))
    }
    #if (SumIter<10){
    temp=eval(parse(text=paste("M_",SectorsNm[x],"=M_",SectorsNm[x],"*",Km2PerGrid,sep="")))
    dir.create(paste("MagnetGridR/SpatialData/InputData_LandUseModel/",Scenarios,"/",Years[i],"/LandUse/",sep=""), showWarnings = FALSE)
    writeRaster(temp,paste("MagnetGridR/SpatialData/InputData_LandUseModel/",Scenarios,"/",Years[i],"/LandUse/",SectorsNm[x],".tif",sep=""), overwrite=T)
    #}

  } #closing last chunk

}


