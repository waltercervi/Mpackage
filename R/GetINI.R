#' Get INI file parameters
#'
#' @param itm
#' @param flag
#'
#' @return
#' @export
#'
#' @examples
GetINI = function(itm,flag){
  if (flag==0){
    result=as.character(subset(ProjectINI,as.character(ProjectINI$Item)==itm)$Value[1])
  }
  if (flag==1){
    result=eval(parse(text=paste("",subset(ProjectINI,as.character(ProjectINI$Item)==itm)$Value[1],sep="")))
  }
  if (is.na(result[1])){
    print(paste("ERROR!",itm,"is not a valid item. Check ProjectINI.csv"))
  }else{
    return(result)
  }
}
