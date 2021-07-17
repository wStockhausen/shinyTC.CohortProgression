#'
#'@title Calculate the recruitment size distribution
#'
#'@description Function to calculate the recruitment size distribution.
#'
#' @param configInfo - list created by \code{setConfigInfo} with model configuration elements
#' @param input      - list with recruitment size distribution parameters and other information
#'
#' @return list - list with prR as a vector and as a dataframe (see \link{ @details })
#'
#' @details Returns a list with elements:<cr>
#' <ul>
#'  <li> params - a list with input parameter values </li>
#'  <li> prR_z - a vector with the recruitment size distribution </li>
#'  <li> mdfrPrRecAtZ - a dataframe wtih the recruitment size distribution </li>
#' </ul>
#'
#' The \code{input} list has elements:<cr>
#' <ul>
#'  <li> alpha - location parameter for gamma distribution </li>
#'  <li> beta - scale parameter for gamma distribution </li>
#'  <li> mxZ - max size at recruitment </li>
#' </ul>
#'
calcPrRecAtZ<-function(configInfo,input,verbose=FALSE){
  #extract dimensions
  cat(file=stderr(),"--calcRecruitmentSizeDistribution\n");
  if (verbose) cat(file=stderr(),"----class, names(configInfo)=",class(configInfo),names(configInfo),"\n");
  if (verbose) cat(file=stderr(),"----binZ:",configInfo$binZ,"\n");
  if (verbose) cat(file=stderr(),"----minZ:",configInfo$minZ,"\n");
  if (verbose) cat(file=stderr(),"----maxZ:",configInfo$maxZ,"\n");
  bnZ <-configInfo$binZ;#bin size
  zCs<-configInfo$zCs;  #size bin cutpoints
  zBs<-configInfo$zBs;  #size bin centers
  #cat("zBs:\n",zBs);

  #extract parameters
  alpha<-eval(parse(text=input$alpha));
  beta <-eval(parse(text=input$beta));
  mxZ  <- input$mxZ;
  if (verbose) cat("----alpha = ",alpha,"\n");
  if (verbose) cat("----beta  = ",beta,"\n");
  if (verbose) cat("----mxZ   = ",mxZ,"\n");
  params<-list(alpha=alpha,beta=beta,mxZ=mxZ);

  #calc size distribution
  dzbs   <- zBs - zCs[1];#size increments from the lowest size cutpoint
  #print(zbs)
  prs <- dgamma(dzbs,shape=alpha/beta,scale=beta);
  prs[mxZ<zBs] <- 0.0;  #truncate distribution
  prR_z <- prs/sum(prs);#standardize to sum to 1

  #convert to melted dataframe
  mdfrPrR<-data.frame(case="",x="",z=zBs,val=prR_z,stringsAsFactors=FALSE);

  return(list(params=params,
              prR_z=prR_z,
              mdfrPrRecAtZ=mdfrPrR));
}

