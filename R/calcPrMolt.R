#'
#'@title Calculate probability at size of molting for immature crab
#'
#'@description Function to calculate probability at size of molting for immature crab.
#'
#' @param configInfo - list created by \code{setConfigInfo} with model configuration elements
#' @param input      - list with growth parameters and other information
#'
#' @return list - list with prMolt as a vector and as a dataframe (see \link{ @details })
#'
#' @details Returns a list with elements:<cr>
#' <ul>
#'  <li> params - a list with input parameter values </li>
#'  <li> prMolt_z - a vector with  wtih probability of annual molt by pre-molt size </li>
#'  <li> mdfrPrMolt - a dataframe wtih probability of annual molt by pre-molt size </li>
#' </ul>
#'
#' The \code{input} list has elements:<cr>
#' <ul>
#'  <li> z50 - location parameter for logistic curve (size at inflection point) </li>
#'  <li> b50 - scale parameter for parametric curve </li>
#'  <li> min - mininimum motling probability </li>
#'  <li> z1  - minimum size at which prMolt is less than 1 </li>
#' </ul>
#'
calcPrMolt<-function(configInfo,input,verbose=FALSE){
  #extract dimensions
  cat(file=stderr(),"--calcPrMolt\n");
  if (verbose) cat(file=stderr(),"----class, names(configInfo)=",class(configInfo),names(configInfo),"\n");
  if (verbose) cat(file=stderr(),"----binZ:",configInfo$binZ,"\n");
  if (verbose) cat(file=stderr(),"----minZ:",configInfo$minZ,"\n");
  if (verbose) cat(file=stderr(),"----maxZ:",configInfo$maxZ,"\n");
  bnZ <-configInfo$binZ;#bin size
  zCs<-configInfo$zCs;  #size bin cutpoints
  zBs<-configInfo$zBs;  #size bin centers
  #cat("zBs:\n",zBs);

  #extract parameters
  z50<-input$z50;
  b50 <-input$b50;
  min<-input$min;
  z1 <-input$z1;
  if (verbose) cat("----z50 = ",z50,"\n");
  if (verbose) cat("----b50 = ",b50,"\n");
  if (verbose) cat("----min = ",min,"\n");
  params<-list(z50=z50,b50=b50,min=min,z1=z1);

  #calculate probability of undergoing annual molt (immature crab)
  prMolt <- 1 - (1-min)/(1+exp((zBs-z50)/b50));
  prMolt[zBs<z1]<-1;

  #create melted dataframe
  mdfrMolt<-data.frame(case="",x="",z=zBs,val=prMolt,stringsAsFactors=FALSE);
  #cat("GOT HERE 2!\n")
  return(list(params=params,
              prMolt_z=prMolt,
              mdfrPrMolt=mdfrMolt));
}
