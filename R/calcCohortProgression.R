#'
#'@title Calculate the cohort progression
#'
#'@description Function to calculate the cohort progressionn.
#'
#' @param configInfo - list created by \code{setConfigInfo} with model configuration elements
#' @param nmInfo - list created by \code{natmortServer} with model natural mortality rates elements
#' @param mltInfo - list created by \code{moltServer} with model molt probability elements
#' @param grwInfo - list created by \code{growthServer} with growth transition elements
#' @param m2mInfo - list created by \code{m2mServer} with prM2M elements
#' @param recInfo - list created by \code{recServer} with recruitment distribution elements
#' @param input   - list with cohort progression parameters and other information
#' @param verbose - flag (T/F) to print debugging info
#'
#' @return list - list with n_msz as a vector and as a dataframe (see \link{ @details })
#'
#' @details Returns a list with elements:<cr>
#' <ul>
#'  <li> params - a list with input parameter values </li>
#'  <li> n_ymsz - an array with the cohort progression </li>
#'  <li> mdfrN_ymsz - a dataframe wtih the cohort progression </li>
#'  <li> p_ymsz - an array with the normalized cohort progression </li>
#'  <li> mdfrP_ymsz - a dataframe wtih the normalized cohort progression </li>
#'  <li> n_msz - an array with the equilibrium size distribution </li>
#'  <li> mdfrN_msz - a dataframe wtih the equilibrium size distribution </li>
#'  <li> p_msz - an array with the normalized equilibrium size distribution </li>
#'  <li> mdfrP_msz - a dataframe wtih the normalized equilibrium size distribution </li>
#' </ul>
#'
#' The \code{input} list must have the following elements:<cr>
#' <ul>
#'  <li> maxAge - max age (years) to include in progression </li>
#' </ul>
#'
calcCohortProgression<-function(configInfo,
                                nmInfo,
                                mltInfo,
                                grwInfo,
                                m2mInfo,
                                recInfo,
                                input,
                                verbose=FALSE){
  verbose=TRUE;
  #extract dimensions
  cat(file=stderr(),"----starting calcCohortProgression()\n");
  dtM<-configInfo$moltTime;
  mss<-configInfo$maturityStates;
  scs<-configInfo$shellConditions;
  #if (verbose) cat(file=stderr(),"----class, names(configInfo)=",class(configInfo),names(configInfo),"\n");
  if (verbose) cat(file=stderr(),"----moltTime:",dtM,"\n");
  if (verbose) cat(file=stderr(),"----maturity states:",mss,"\n");
  if (verbose) cat(file=stderr(),"----shell conditions:",scs,"\n");
  if (verbose) cat(file=stderr(),"----binZ:",configInfo$binZ,"\n");
  if (verbose) cat(file=stderr(),"----minZ:",configInfo$minZ,"\n");
  if (verbose) cat(file=stderr(),"----maxZ:",configInfo$maxZ,"\n");
  bnZ <-configInfo$binZ;#bin size
  zCs<-configInfo$zCs;  #size bin cutpoints
  zBs<-configInfo$zBs;  #size bin centers
  #cat("zBs:\n",zBs);

  #extract parameters
  maxAge <- input$maxAge;
  if (verbose) cat(file=stderr(),"----maxAge:",maxAge,"\n");
  params<-list(dtM=dtM,maxAge=maxAge);

  #determine dimensions
  nYs <- maxAge+1;
  nMs <- length(mss);
  nSs <- length(scs);
  nZs <- length(zBs);
  if (verbose) cat(file=stderr(),"----nYs:",nYs,"\n");
  if (verbose) cat(file=stderr(),"----nMs:",nMs,"\n");
  if (verbose) cat(file=stderr(),"----nSs:",nSs,"\n");
  if (verbose) cat(file=stderr(),"----nZs:",nZs,"\n");
  #define abundance arrays
  n_ymsz <- array(data=0,
                  dim=c(nYs,nMs,nSs,nZs),
                  dimnames=list(y=0:maxAge,m=mss,s=scs,z=zBs));#cohort progression
  n_msz <- array(data=0,
                 dim=c(nMs,nSs,nZs),
                 dimnames=list(m=mss,s=scs,z=zBs));#equilibrium size distribution

  #set recruitment
  n_ymsz["0",1,1,] <- recInfo$prR_z; #all recruits to model are immature, new shell
  if (verbose) cat(file=stderr(),"----created n_ymsz.\n");

  #extract population processes
  M_ms   <- nmInfo$M_ms;        #natural mortality rates
  prM_z  <- mltInfo$prMolt_z;   #prob of immature, new shell crab molting (prob of old shell, immature crab molting = 1)
  prG_zz <- t(grwInfo$prGr_zz); #prob of growth, given premolt size and that crab will molt (transposed so pre-molt is columns)
  prT_z  <- m2mInfo$prM2M_z;    #prob of terminal molt, given pre-molt size and that crab will molt
  if (verbose) cat(file=stderr(),"----CP1\n");

  #calculate cohort progression
  for (y in as.character(0:(maxAge-1))){
    #project pop to time of molting
    n_in1 <- exp(-dtM*M_ms[1,1])*n_ymsz[y,1,1,];
    n_io1 <- exp(-dtM*M_ms[1,2])*n_ymsz[y,1,2,];
    n_mn1 <- exp(-dtM*M_ms[2,1])*n_ymsz[y,2,1,];
    n_mo1 <- exp(-dtM*M_ms[2,2])*n_ymsz[y,2,2,];
    #do molting/growth(note that all immature, old shell crab are assumed to molt)
    n_in2 <- prG_zz %*% ((1-prT_z)*((  prM_z)*n_in1+1*n_io1));  #non-terminal molt growth
    n_io2 <-            (          ((1-prM_z)*n_in1+0*n_io1));  #no molt, no growth
    n_mn2 <- prG_zz %*% ((  prT_z)*((  prM_z)*n_in1+1*n_io1));  #terminal molt growth to maturity
    n_mo2 <- n_mo1 + n_mn1;                                     #post terminal molt (no molt, no growth)
    #project to start of new year
    yp1<-as.character(as.numeric(y)+1);
    n_ymsz[yp1,1,1,] <- exp(-(1-dtM)*M_ms[1,1])*n_in2;
    n_ymsz[yp1,1,2,] <- exp(-(1-dtM)*M_ms[1,2])*n_io2;
    n_ymsz[yp1,2,1,] <- exp(-(1-dtM)*M_ms[2,1])*n_mn2;
    n_ymsz[yp1,2,2,] <- exp(-(1-dtM)*M_ms[2,2])*n_mo2;
    if (verbose) cat(file=stderr(),"----CP5\n");
  }

  #calculate equilibriium size distribution
  for (y in as.character(0:maxAge)){
    for (m in 1:nMs){
      for (s in 1:nSs) n_msz[m,s,]<-n_msz[m,s,]+n_ymsz[y,m,s,];
    }
  }

  #calculate normalized size compositions
  p_ymsz <- 0*n_ymsz;
  for (y in as.character(0:maxAge)){
    n <- sum(n_ymsz[y,,,]);#sum over all
    p_ymsz[y,,,]<-n_ymsz[y,,,]/n;
  }
  p_msz <- n_msz/sum(n_msz);

  #convert to melted dataframes
  mdfrN_ymsz<-reshape2::melt(n_ymsz,value.name="val");
  mdfrN_ymsz$case<-"";
  mdfrN_ymsz$x   <-"";
  mdfrP_ymsz<-reshape2::melt(p_ymsz,value.name="val");
  mdfrP_ymsz$case<-"";
  mdfrP_ymsz$x   <-"";
  mdfrN_msz<-reshape2::melt(n_msz,value.name="val");
  mdfrN_msz$case<-"";
  mdfrN_msz$x   <-"";
  mdfrP_msz<-reshape2::melt(p_msz,value.name="val");
  mdfrP_msz$case<-"";
  mdfrP_msz$x   <-"";

  if (verbose) cat(file=stderr(),"----finished calcCohortProgression()\n");
  return(list(params=params,
              prR_z=recInfo$prR_z,
              M_ms=M_ms,prM_z=prM_z,prG_zz=prG_zz,prT_z=prT_z,
              n_ymsz=n_ymsz,mdfrN_ymsz=mdfrN_ymsz,
              p_ymsz=p_ymsz,mdfrP_ymsz=mdfrP_ymsz,
              n_msz=n_msz,mdfrN_msz=mdfrN_msz,
              p_msz=p_msz,mdfrP_msz=mdfrP_msz));
}

