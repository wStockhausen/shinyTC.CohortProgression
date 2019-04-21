#'
#' @title Plot the equilibrium size distribution
#'
#' @description Function to plot the equilibrium size distribution
#'
#' @param mdfr - melted data frame with equilibrium size distribution
#'
#' @return list of ggplot2 objects
#'
#' @details
#'
#' @import ggplot2
#'
#' @export
#'
plotPop.EquilibriumSizeDist<-function(mdfr,
                                      plotType,
                                      scales="free_y",
                                      showPlot=FALSE,
                                      verbose=FALSE){
  if (verbose) cat("Starting plotPop.EquilibriumSizeDist()\n")
  frmla<-"case+";
  frmla<-paste0(frmla,plotType,"~.");
  if (verbose) cat("--frmla =",frmla,"\n");
  if (verbose) print(head(mdfr));
  dfr<-reshape2::dcast(mdfr,frmla,fun.aggregate=wtsUtilities::Sum,value.var="val");
  if (verbose) print(head(dfr));
  nc<-ncol(dfr);
  names(dfr)[nc]<-"val";

  fg <- NULL;
  if (plotType=="m+z")   fg<-"m~.";
  if (plotType=="m+s+z") fg<-"m+s~.";
  if (verbose) cat("fg =",fg,"\n");
  p <- ggplot(dfr,mapping=aes_string(x="z",y="val",colour="case"));
  p <- p + geom_line();
  if (!is.null(fg)) p <- p + facet_grid(rows=as.formula(fg),scales=scales);
  p <- p + labs (x="size (mm CW)",y="equilbrium distribution");
  if (showPlot) print(p);
  if (verbose) cat("Finished plotPop.EquilibriumSizeDist()\n")
  return(list(p=p));
}
