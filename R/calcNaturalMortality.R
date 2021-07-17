#'
#'@title Calculate natural mortality rates
#'
#'@description Function to calculate natural mortality rates.
#'
#' @param configInfo - list created by \code{setConfigInfo} with model configuration elements
#' @param input      - list with natural mortality rate parameters and other information
#'
#' @return list - list with M_ms as a matrix and as a dataframe (see \link{ @details })
#'
#' @details Returns a list with elements:<cr>
#' <ul>
#'  <li> params - a list with input parameter values </li>
#'  <li> M_ms - a matrix of natural mortality rates indexed by maturity state and shell condition </li>
#'  <li> mdfrM_ms - a dataframe wtih the the same information </li>
#' </ul>
#'
#' The \code{input} list has elements:<cr>
#' <ul>
#'  <li> pM - base natural mortality </li>
#'  <li> pDM1 - multiplier for immature crab </li>
#'  <li> pDM2 - multiplier for mature crab </li>
#' </ul>
#'
calcNaturalMortality<-function(configInfo,input,verbose=FALSE){
  #extract dimensions
  cat(file=stderr(),"--calcNatMort\n");
  mss<-configInfo$maturityStates;
  scs<-configInfo$shellConditions;

  #extract parameter values
  pM   <- eval(parse(text=input$pM));
  pDM1 <- eval(parse(text=input$pDM1));
  pDM2 <- eval(parse(text=input$pDM2));

  nMs <- length(mss);
  nSs <- length(scs);
  M_ms<-array(data=0,dim=c(nMs,nSs),dimnames=list(m=mss,s=scs));
  M_ms["immature",]<-pDM1*pM;
  M_ms[  "mature",]<-pDM2*pM;

  #create melted dataframe
  mdfrM_ms<-reshape2::melt(M_ms,value.name="val");
  mdfrM_ms$case<-"";
  mdfrM_ms$year<-"";
  mdfrM_ms$x   <-"";

  cat(file=stderr(),"--finished calcNatMort\n");
  return (list(params=list(pM=pM,pDM1=pDM1,pDM2=pDM2),
               M_ms=M_ms,mdfrM_ms=mdfrM_ms))
}
