#'
#' @title Server function for the Tanner crab model growth Shiny module
#'
#' @description Server function for the Tanner crab model growth Shiny module.
#'
#' @params the usual input, output, session variables
#' @param configInfo - reactive variable yielding a list with model configuration info
#'
#' @return a reactive object which returns a list with elements:<cr>
#' <ul>
#'  <li> growthInfo - a list returned from \code{\link{calcGrowth}} </li>
#'  <li> plotMnG - a ggplot2 object with mean growth by pre-molt size </li>
#'  <li> plotPrG - a ggplot2 object wtih probability of growth into a post-molt size by pre-molt size </li>
#' </ul>

#'
#' @details Updates the parameters, mean growth, growth transition matrix, and associated ggplot2 objects
#' associated with the Tanner crab growth model.
#'
#' @import shiny
#'
growthServer<-function(input, output, configInfo, session) {

  observe(
    {
      cat(file=stderr(),"--growthServer:observer()\n")
      # cat("growthServer: class(input)      =",class(input),"\n")
      # cat("growthServer: names(input)      =",names(input),"\n")
      # cat("growthServer: class(output)     =",class(output),"\n")
      # cat("growthServer: names(output)     =",names(output),"\n")
      # cat("growthServer: class(configInfo) =",class(configInfo),"\n")
      # cat("growthServer: names(configInfo()) =",names(configInfo()),"\n")
#      cat("growthServer: class(session)    =",class(session),"\n")
    },
    label="growthObserver"
  );

    #calculate growth info when user clicks "Refresh" button
    growthInfo<-eventReactive(
      c(input$refresh1,configInfo()),
      {
        cat(file=stderr(),"--starting growthServer:growthInfo()\n");
        res<-calcGrowth(configInfo(), input);
        ret<-list(inpInfo=input,
                  params=res$params,
                  mdfrMnGrowth=res$mdfrMnGrowth,
                  mdfrPrGrowth=res$mdfrPrGrowth,
                  prGr_zz=res$prGr_zz);
        cat(file=stderr(),"--finished growthServer:growthInfo()\n");
        return(ret);
      },
      ignoreNULL=TRUE
    );

    #reset input values
    observeEvent(
      input$reset1,
      {
        cat(file=stderr(),"--growthServer:resetting\n");
        shinyjs::reset("inputs");
        cat(file=stderr(),"--growthServer:reset!\n");
        }
    );

    #create ggplot for mean growth curve
    plotMnG<-reactive({
        rCompTCMs::plotPop.MeanGrowth(
          growthInfo()$mdfrMnGrowth,showPlot=FALSE
        )
    });

    #render mean growth curve
    output$pltMnG<-renderPlot(plotMnG());

    #create ggplot for mean growth + probability distributions
    plotPrG<-reactive(
      {
        rCompTCMs::plotPop.MeanGrowthPlusProbs(
          growthInfo()$mdfrMnGrowth,
          growthInfo()$mdfrPrGrowth,
          scale=isolate(input$scale),
          xbnds=c(isolate(input$minX),
                  isolate(input$maxX)),
          ybnds=c(isolate(input$minY),
                  isolate(input$maxY)),
          zbnds=c(isolate(input$minZ),
                  isolate(input$maxZ)),
          showPlot=FALSE
        );
      }
    );

    #render mean growth + probability distributions
    output$pltPrG<-renderPlot(plotPrG());

  obj<-reactive(list(growthInfo=growthInfo(),plotMnG=plotMnG(),plotPrG=plotPrG()));
  return(obj); # if returning an R object to the enclosing server
} #function(input,output)
