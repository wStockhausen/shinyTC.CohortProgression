#'
#' @title Server function for the Tanner crab model annual molt probability Shiny module
#'
#' @description Server function for the Tanner crab model molt probability Shiny module.
#'
#' @param input - the usual shiny input variable
#' @param output - the usual shiny output variable
#' @param configInfo - reactive variable yielding a list with model configuration info
#' @param session - the usual shiny session variable
#'
#' @return reactive object which returns a list with elements
#' <ul>
#'   <li> moltInfo -  a list with elements inpInfo, prMolt_z, and mdfrPrMolt (see \code{\link{calcPrMolt}})</li>
#'   <li> plotPrMolt - a ggplot2 object
#' </ul>
#'
#'
#' @details Updates the parameters, molt probabilities, and associated ggplot2 objects
#' associated with the Tanner crab annual molt model.
#'
#' @import shiny
#'
moltServer<-function(input, output, configInfo, session) {

  observe(
    {
      cat(file=stderr(),"--moltServer:observer()\n")
      # cat("moltServer: class(input)      =",class(input),"\n")
      # cat("moltServer: names(input)      =",names(input),"\n")
      # cat("moltServer: class(output)     =",class(output),"\n")
      # cat("moltServer: names(output)     =",names(output),"\n")
      # cat("moltServer: class(configInfo) =",class(configInfo),"\n")
      # cat("moltServer: names(configInfo()) =",names(configInfo()),"\n")
#      cat("moltServer: class(session)    =",class(session),"\n")
    },
    label="moltObserver"
  );

    #calculate molt-to-maturity info when user clicks "Refresh" button
    moltInfo<-eventReactive(
      c(input$refresh1,configInfo()),
      {
        cat(file=stderr(),"--starting moltServer:moltInfo()\n");
        res<-calcPrMolt(configInfo(),input);
        ret<-list(inpInfo=input,
                  params=res$params,
                  prMolt_z=res$prMolt_z,
                  mdfrPrMolt=res$mdfrPrMolt);
        cat(file=stderr(),"--finished moltServer:moltInfo()\n");
        return(ret);
      },
      ignoreNULL=TRUE
    );

    #reset input values
    observeEvent(
      input$reset1,
      {
        cat(file=stderr(),"--moltServer:resetting\n");
        shinyjs::reset("inputs");
        cat(file=stderr(),"--moltServer:reset!\n");
        }
    );

    #create ggplot object for probability ogive
    plotPrMolt<-reactive(
      {
        p<-rCompTCMs::plotPop.PrMolt(
          moltInfo()$mdfrPrMolt,
          xbnds=c(isolate(input$minX),
                  isolate(input$maxX)),
          showPlot=FALSE
        );
        return(p);
      }
    )

    #render probability ogive plot
    output$pltPrMolt<-renderPlot(plotPrMolt());

    obj<-reactive(list(moltInfo=moltInfo(),plotPrMolt=plotPrMolt()));
    return(obj); #here if returning an R object to the enclosing server
  } #function(input,output,session)
