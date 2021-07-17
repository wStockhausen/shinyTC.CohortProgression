#'
#' @title Calculate mean growth and the growth transition matrix
#'
#' @description Function to calculate mean growth and the growth transition matrix.
#'
#' @param configInfo - list created by \code{setConfigInfo} with model configuration elements
#' @param input      - list with growth parameters and other information
#'
#' @return list - list with mean growth and growth transition matrix (see \link{ @details })
#'
#' @details Returns a list with elements:<cr>
#' <ul>
#'  <li> params - a list with input parameter values </li>
#'  <li> mdfrMnGrowth - a dataframe with mean growth by pre-molt size </li>
#'  <li> mdfrPrGrowth - a dataframe wtih probability of growth into a post-molt size by pre-molt size </li>
#'  <li> prGr_zz - the size-transition matrix (rows: pre-molt size bins; cols: post-molt size bins)
#' </ul>
#'
#' The \code{input} list has elements:<cr>
#' <ul>
#'  <li> zA - reference pre-molt size (mm CW) for pA</li>
#'  <li> pA - parameter reflecting mean post-molt size at pre-molt size zA</li>
#'  <li> zB - reference pre-molt size (mm CW) for pB</li>
#'  <li> pB - parameter reflecting mean post-molt size at pre-molt size zB/li>
#'  <li> pBeta - parameter reflecting scale function for gamma distribution zB/li>
#'  <li> maxZBEx - parameter reflecting max extent of potential growth (in size bins) zB/li>
#' </ul>
#'
calcGrowth<-function(configInfo,input,verbose=FALSE){
  #extract dimensions
  cat(file=stderr(),"--calcGrowth\n");
  if (verbose) cat(file=stderr(),"----class, names(configInfo)=",class(configInfo),names(configInfo),"\n");
  if (verbose) cat(file=stderr(),"----binZ:",configInfo$binZ,"\n");
  if (verbose) cat(file=stderr(),"----minZ:",configInfo$minZ,"\n");
  if (verbose) cat(file=stderr(),"----maxZ:",configInfo$maxZ,"\n");
  bnZ <-configInfo$binZ;#bin size
  zCs<-configInfo$zCs;  #size bin cutpoints
  zBs<-configInfo$zBs;  #size bin centers
  #cat("zBs:\n",zBs);

  #extract parameters
  zGrA<-input$zA;
  grA <-input$pA;
  zGrB<-input$zB;
  grB <-input$pB;
  grBeta<-input$pBeta;
  if (verbose) cat("----zGrA, grA = ",zGrA,", ",grA,"\n");
  if (verbose) cat("----zGrA, grA = ",zGrB,", ",grB,"\n");
  if (verbose) cat("----grBeta = ",grBeta,"\n");

  #extract other info
  maxZBEx <- input$maxZBEx;
  if (verbose) cat("----maxZBEx = ",maxZBEx,"\n");
  params<-list(pA=grA,zA=zGrA,pB=grB,zB=zGrB,pBeta=grBeta,maxZBEx=maxZBEx);

  #calculate mean growth for center of each size bin
  mnZs<-grA*exp(log(grB/grA)/log(zGrB/zGrA)*log(zBs/zGrA));

  #calculate growth probabilities
  nZBs<-length(zBs);
  nZCs<-length(zCs);
  prGr_zz<-matrix(data=0.0,nrow=nZBs,ncol=nZBs);
  dimnames(prGr_zz)<-list(z=as.character(zBs),zp=as.character(zBs))
  for (z in 1:(nZBs-1)){ #looping over pre-molt size
    #cat("calculating probabilities for pre-molt size bin",z,": ",zBs[z],"\n");
    #cat("--mean molt increment:",mnZs[z]-zBs[z],"\n")
    #cat("--cutpt increments   :",zCs[(z+1):nZCs]-zBs[z],"\n")
    alIs  <- (mnZs[z]        -zBs[z])/grBeta; #scaled location parameter to evaluate gamma distribution
    sclIs <- (zCs[(z+1):nZCs]-zBs[z])/grBeta; #scaled increments at size bin cut points
    prs   <-0*(z:nZBs); #will be probability of growing in post-molt size bin z (note vector index starts at 1)
    nPrs  <- length(prs);
    cprs.lwr <- 0;                     #cumulative probablility of growth to lower cutpoint of size bin z
    cprs.upr <- pgamma(sclIs[1],alIs); #cumulative probablility of growth to upper cutpoint of size bin z
    prs[1]   <- cprs.upr-cprs.lwr;
    for (zp in 2:(nPrs-1)){
      cprs.lwr <- cprs.upr;               #cumulative probablility of growth to lower cutpoint of size bin zp
      cprs.upr <- pgamma(sclIs[zp],alIs); #cumulative probablility of growth to upper cutpoint of size bin zp
      prs[zp]  <- cprs.upr-cprs.lwr;#cumulative pr from zCs[zp] to zCs[zp+1]
    } #post-molt size
    cprs.lwr  <- cprs.upr;
    prs[nPrs] <- 1.0 - cprs.lwr; #treat final size bin as accumulator
    #cat("--:",paste(prs,collapse=" "),"\n")
    #cat("--sum(prs) = ",sum(prs),"\n");
    #cat("----limiting growth\n")
    if (nPrs > maxZBEx) prs[(maxZBEx+1):nPrs] <- 0.0;#limit growth range
    #cat("----truncated:\n",paste(prs,collapse=" "),"\n")
    #cat("----truncated sum(prs) = ",sum(prs),"\n");
    prs <- prs/sum(prs);#normalize to sum to 1
    #cat("----normalized:\n",paste(prs,collapse=" "),"\n")
    #cat("----normalized sum(prs) = ",sum(prs),"\n");
    prGr_zz[z,z:nZBs] <- prs;
  }#pre-molt size
  prGr_zz[nZBs,nZBs] <- 1.0; #no growth from max size bin
  #cat("GOT HERE 1!\n")

  #convert to melted dataframes
  mdfrMnGrowth<-data.frame(case="",x="",z=zBs,val=mnZs,stringsAsFactors=FALSE);
  #cat("GOT HERE 2!\n")
  #head(prGr_zz);
  mdfrPrGrowth<-reshape2::melt(prGr_zz,value.name="val");
  #cat("GOT HERE 2!\n")
  #head(mdfrPrGrowth);
  mdfrPrGrowth$case<-"";
  #cat("GOT HERE 3!\n")
  mdfrPrGrowth$x   <-"";
  #head(mdfrPrGrowth);
  return(list(params=params,
              mdfrMnGrowth=mdfrMnGrowth,
              mdfrPrGrowth=mdfrPrGrowth,
              prGr_zz=prGr_zz));xs
}
