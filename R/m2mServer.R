#'
#' @title Server function for the Tanner crab model molt-to-maturity Shiny module
#'
#' @description Server function for the Tanner crab model molt-to-maturity Shiny module.
#'
#' @params the usual input, output, session variables
#' @param configInfo - reactive variable yielding a list with model configuration info
#'
#' @return reactive object which returns a list with elements
#' <ul>
#'   <li> m2mInfo -  a list with elements inpInfo, prM2M_z, and mdfrPrM2M (see \code{\link{calcPrM2M}})</li>
#'   <li> plotPrM2M - a ggplot2 object
#' </ul>
#'
#' @details Updates the parameters, molt-to-maturity probabilities, and associated ggplot2 objects
#' associated with the Tanner crab molt-to-maturity model.
#'
#' @import shiny
#'
m2mServer<-function(input, output, configInfo, session) {

  observe(
    {
      cat(file=stderr(),"--m2mServer:observer()\n")
      # cat("m2mServer: class(input)      =",class(input),"\n")
      # cat("m2mServer: names(input)      =",names(input),"\n")
      # cat("m2mServer: class(output)     =",class(output),"\n")
      # cat("m2mServer: names(output)     =",names(output),"\n")
      # cat("m2mServer: class(configInfo) =",class(configInfo),"\n")
      # cat("m2mServer: names(configInfo()) =",names(configInfo()),"\n")
#      cat("m2mServer: class(session)    =",class(session),"\n")
    },
    label="m2mObserver"
  );

    #calculate molt-to-maturity info when user clicks "Refresh" button
    m2mInfo<-eventReactive(
      c(input$refresh1,configInfo()),
      {
        cat(file=stderr(),"--starting m2mServer:m2mInfo()\n");
        res<-calcPrM2M(configInfo(),input);
        ret<-list(inpInfo=input,
                  params=res$params,
                  prM2M_z=res$prM2M_z,
                  mdfrPrM2M=res$mdfrPrM2M);
        cat(file=stderr(),"--finished m2mServer:m2mInfo()\n");
        return(ret);
      }
    );

    #reset input values
    observeEvent(
      input$reset1,
      {
        cat(file=stderr(),"--m2mServer:resetting\n");
        shinyjs::reset("inputs");
        cat(file=stderr(),"--m2mServer:reset!\n");
        }
    );

    #create ggplot object of probability of molt-to-maturity
    plotPrM2M<-reactive(
      {
        cat(file=stderr(),"Starting m2mServer:plotPrM2M()\n");
        p<-rCompTCMs::plotPop.PrM2M(
          m2mInfo()$mdfrPrM2M,
          xbnds=c(isolate(input$minX),
                  isolate(input$maxX)),
          showPlot=FALSE
        );
        cat(file=stderr(),"Finished m2mServer:plotPrM2M()\n");
        return(p);
      }
    );

    #plot probability of molt-to-maturity
    output$pltPrM2M<-renderPlot(
      {
        cat(file=stderr(),"Starting m2mServer:renderPlot()\n");
        p<-plotPrM2M();
        cat(file=stderr(),"Finished m2mServer:renderPlot()\n");
        return(p);
      }
    );

    obj<-reactive(list(m2mInfo=m2mInfo(),plotPrM2M=plotPrM2M()))
    return(obj); #here if returning an R object to the enclosing server
  } #function(input,output,session)
