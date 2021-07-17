#'
#' @title Server function for the Tanner crab model reccruitment size distribution Shiny module
#'
#' @description Server function for the Tanner crab model reccruitment size distribution Shiny module.
#'
#' @param input - the usual shiny input variable
#' @param output - the usual shiny output variable
#' @param configInfo - reactive variable yielding a list with model configuration info
#' @param session - the usual shiny session variable
#'
#' @return reactive object which returns a list with elements
#' <ul>
#'   <li> recInfo -  a list with elements inpInfo, prR_z, and mdfrPrRecAtZ (see \code{\link{calcPrRecAtZ}})</li>
#'   <li> plotPrRecAtZ - a ggplot2 object
#' </ul>
#'
#'
#' @details Updates the parameters, recruitment size distribution, and associated ggplot2 objects
#' associated with the Tanner crab annual recruitment-at-size model.
#'
#' @import shiny
#'
recServer<-function(input, output, configInfo, session) {

  observe(
    {
      cat(file=stderr(),"--recServer:observer()\n")
      # cat("recServer: class(input)      =",class(input),"\n")
      # cat("recServer: names(input)      =",names(input),"\n")
      # cat("recServer: class(output)     =",class(output),"\n")
      # cat("recServer: names(output)     =",names(output),"\n")
      # cat("recServer: class(configInfo) =",class(configInfo),"\n")
      # cat("recServer: names(configInfo()) =",names(configInfo()),"\n")
#      cat("recServer: class(session)    =",class(session),"\n")
    },
    label="recObserver"
  );

    #calculate reccruitment-at-size info when user clicks "Refresh" button
    recInfo<-eventReactive(
      c(input$refresh1,configInfo()),
      {
        cat(file=stderr(),"--starting recServer:recInfo()\n");
        res<-calcPrRecAtZ(configInfo(),input);
        ret<-list(inpInfo=input,
                  params=res$params,
                  prR_z=res$prR_z,
                  mdfrPrRecAtZ=res$mdfrPrRecAtZ);
        cat(file=stderr(),"--finished recServer:recInfo()\n");
        return(ret);
      }
    );

    #reset input values
    observeEvent(
      input$reset1,
      {
        cat(file=stderr(),"--recServer:resetting\n");
        shinyjs::reset("inputs");
        cat(file=stderr(),"--recServer:reset!\n");
        }
    );

    #create ggplot of recruitment size distribution
    plotPrRecAtZ<-reactive(
      {
        rCompTCMs::plotPop.RecSizeDistribution(
          recInfo()$mdfrPrRecAtZ,
          xbnds=c(isolate(input$minX),
                  isolate(input$maxX)),
          showPlot=FALSE
        );
      }
    );

    #render recruitment size distribution plot
    output$pltPrRecAtZ<-renderPlot(plotPrRecAtZ());

    obj<-reactive(list(recInfo=recInfo(),plotPrRecAtZ=plotPrRecAtZ()));#can't return elements in output here
    return(obj); #here if returning an R object to the enclosing server
  } #function(input,output,session)
