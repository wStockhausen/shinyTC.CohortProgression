#'
#' @title Set the model configuration state
#'
#' @description Function to set the model configuration state.
#'
#' @param input - list with elements binZ, minZ, maxZ (see \link{ @details })
#'
#' @return list - list with model configuration info (see \link{ @details })
#'
#' @details Returns a list that reflects the
#' current model configuration state, with elements:<cr>
#' <ul>
#'  <li> moltTime - time of molting/mating (as fraction of year from July 1) </li>
#'  <li> maturityStates - maturity states </li>
#'  <li> shellConditions - shell conditions </li>
#'  <li> binZ - size bin width (mm CW) </li>
#'  <li> minZ - min size cutpoint (mm CW) </li>
#'  <li> maxZ - max size cutpoint (mm CW) </li>
#'  <li> zBs - size bins (mm CW) </li>
#'  <li> Zcs - size bin cutpoints (mm CW) </li>
#' </ul>
#'
#' The input list has elements:<cr>
#' <ul>
#'  <li> moltTime - molt time (as fraction of year from July 1)  </li>
#'  <li> mss - text yielding a character vector of maturity states when parsed/evaluated  </li>
#'  <li> scs - text yielding a character vector of shell conditions when parsed/evaluated </li>
#'  <li> binZ - size bin width (mm CW) </li>
#'  <li> minZ - min size cutpoint (mm CW) </li>
#'  <li> maxZ - max size cutpoint (mm CW) </li>
#' </ul>
#'
setConfigInfo<-function(input,verbose=FALSE){
  #extract dimensions
  cat(file=stderr(),"--setConfigInfo: \n");
  if (verbose) cat(file=stderr(),"----names(configInfo) =",names(input),"\n");
  moltTime<-input$moltTime;
  maturityStates<-eval(parse(text=input$mss));
  shellConditions<-eval(parse(text=input$scs));
  binZ<-input$binZ;
  minZ<-input$minZ;
  maxZ<-input$maxZ;
  if (verbose) cat(file=stderr(),"----molt time:",moltTime,"\n");
  if (verbose) cat(file=stderr(),"----maturity states:",maturityStates,"\n");
  if (verbose) cat(file=stderr(),"----shell conditions:",shellConditions,"\n");
  if (verbose) cat(file=stderr(),"----binZ:",binZ,"\n");
  if (verbose) cat(file=stderr(),"----minZ:",minZ,"\n");
  if (verbose) cat(file=stderr(),"----maxZ:",maxZ,"\n");
  zCs<-seq(from=minZ,to=maxZ,by=binZ);               #size bin cutpoints
  zBs<-seq(from=minZ+binZ/2,to=maxZ-binZ/2,by=binZ); #size bin centers
  if (verbose) cat(file=stderr(),"----zBs:\n",zBs,"\n");
  return(list(moltTime=moltTime,
              maturityStates=maturityStates,
              shellConditions=shellConditions,
              binZ=binZ,minZ=minZ,maxZ=maxZ,
              zBs=zBs,zCs=zCs))
}
