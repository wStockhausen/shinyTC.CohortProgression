#'
#' @title Calculate mean growth and the growth transition matrix
#'
#' @description Function to calculate mean growth and the growth transition matrix.
#'
#' @param configInfo - list created by \code{setConfigInfo} with model configuration elements
#' @param input      - list with growth parameters and other information
#'
#' @return list - list with prM2M as a vector and as a dataframe (see \link{ @details })
#'
#' @details Returns a list with elements:<cr>
#' <ul>
#'  <li> params - a list with input parameter values </li>
#'  <li> prM2M_z - a vector with  wtih probability of molt-to-maturity by pre-molt size </li>
#'  <li> mdfrPrM2M - a dataaframe wtih probability of molt-to-maturity by pre-molt size </li>
#' </ul>
#'
#' The \code{input} list has elements:<cr>
#' <ul>
#'  <li> z50 - location parameter for parametric curve (logistic; size at inflection point) </li>
#'  <li> b50 - scale parameter for parametric curve (logistic; width) </li>
#'  <li> z0  - min size at which prM2M becomes non-zero (using parametric description) </li>
#'  <li> pLgtScl - logit-scale parameters reflecting non-parametric curve for prM2M </li>
#'  <li> n0  - number of initial size bins at which prM2M is zero (using non-parametric curve description) </li>
#'  <li> useNonParametric - flag (T/F) to use non-parametric curve for prM2M </li>
#' </ul>
#'
calcPrM2M<-function(configInfo,input,verbose=FALSE){
  #extract dimensions
  cat(file=stderr(),"--calcPrM2M\n");
  if (verbose) cat(file=stderr(),"----class, names(configInfo)=",class(configInfo),names(configInfo),"\n");
  if (verbose) cat(file=stderr(),"----binZ:",configInfo$binZ,"\n");
  if (verbose) cat(file=stderr(),"----minZ:",configInfo$minZ,"\n");
  if (verbose) cat(file=stderr(),"----maxZ:",configInfo$maxZ,"\n");
  bnZ <-configInfo$binZ;#bin size
  zCs<-configInfo$zCs;  #size bin cutpoints
  zBs<-configInfo$zBs;  #size bin centers
  #cat("zBs:\n",zBs);

  #extract parameters
  if (!input$useNonParametric){
    if (verbose) cat(file=stderr(),"Using parametric curve\n");
    z50<-input$z50;
    b50 <-input$b50;
    lgtSclP<-(zBs-z50)/b50;
    prM2M <- 1.0/(1.0+exp(-lgtSclP));
    z0 <- input$z0;    #size at which prM2M becomes non-zero
    n0 <- sum(zBs<z0); #number of initial size bins at which prM2M is identically 0
    if (n0>0) prM2M[1:n0]<-0; #account for initial zeros
    params<-list(parametric=TRUE,z50=z50,b50=b50,z0=z0);
  } else {
    if (verbose) cat(file=stderr(),"Using non-parametric curve\n");
    txtStr <- input$pLgtM2M;
    splt   <- stringi::stri_split_regex(txtStr,"\\s+",omit_empty=TRUE,simplify=TRUE);#returns a matrix with 1 row
    lgtSclP <- as.numeric(splt[1,]);
    prM2M <- 1.0/(1.0+exp(-lgtSclP));
    n0 <- input$n0; #number of initial size bins at which prM2M is identically 0
    if (n0>0) prM2M <- c(rep.int(0.0,n0),prM2M);    #extend for initial 0's
    nP <- length(prM2M);
    nZ <- length(zBs);
    if (nP>nZ) prM2M <- prM2M[1:nZ];                 #truncate to nZ
    if (nP<nZ) prM2M <- c(prM2M,rep.int(1.0,nZ-nP)); #extend for final 1's
    params<-list(parametric=FALSE,n0=n0,n1=nZ-nP,vals=lgtSclP)
  }
  if (verbose) cat(file=stderr(),"lgtSclP:",lgtSclP,"\n")
  if (verbose) cat(file=stderr(),"prM2M  :",prM2M,"\n")

  #convert to melted dataframes
  mdfrM2M<-data.frame(case="",x="",z=zBs,val=prM2M,stringsAsFactors=FALSE);
  if (verbose) cat(file=stderr(),"finished calcPrM2M\n");
  return(list(params=params,
              prM2M_z=prM2M,
              mdfrPrM2M=mdfrM2M));
}
