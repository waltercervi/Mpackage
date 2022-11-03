 doCorrectLUtotal<-function(){
    CorrLU=LUtotal
    CorrLU[CorrLU<=1]<-1
    for (s in 1:length(SectorsNm)){
      #eval(parse(text=paste("LU_",SectorsNm[s],"=LU_",SectorsNm[s],"/CorrLU",sep="")))
      eval(parse(text=paste("LUmap=LU_",SectorsNm[s],"/CorrLU",sep="")))
      if (s==1){
        LUtotal=LUmap
      }else{
        LUtotal=LUtotal+LUmap
      }
    }
    #LUtotal=LU_c_b+LU_ctl+LU_gro+LU_osd+LU_pbf+LU_pdr+LU_v_f#+LU_wht
    LUtotal[LUtotal==0]<-NA
    return(LUtotal)
}