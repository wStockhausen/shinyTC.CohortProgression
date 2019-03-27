#'
#' @title Server function for the Tanner crab model natural mortality rate Shiny module
#'
#' @description Server function for the Tanner crab model natural mortality rate Shiny module.
#'
#' @params the usual input, output, session variables
#' @param configInfo - reactive variable yielding a list with model configuration info
#'
#' @return reactive object which returns a list with elements
#' <ul>
#'   <li> nmInfo -  a list with elements inpInfo, M_ms, and mdfrM_ms (see \code{\link{calcNaturalMortality}})</li>
#'   <li> plotNM - a ggplot2 object
#' </ul>
#'
#'
#' @details Updates the parameters, natural mortality rates, and associated ggplot2 objects
#' associated with the Tanner crab annual natural mortality rate model.
#'
#' @import shiny
#' @import ggplot2
#'
natmortServer<-function(input, output, configInfo, session) {

  observe(
    {
      cat(file=stderr(),"--natmortServer:observer()\n")
      # cat("recServer: class(input)      =",class(input),"\n")
      # cat("recServer: names(input)      =",names(input),"\n")
      # cat("recServer: class(output)     =",class(output),"\n")
      # cat("recServer: names(output)     =",names(output),"\n")
      # cat("recServer: class(configInfo) =",class(configInfo),"\n")
      # cat("recServer: names(configInfo()) =",names(configInfo()),"\n")
#      cat("recServer: class(session)    =",class(session),"\n")
    },
    label="natmortObserver"
  );

  observeEvent(
    input$reset1,
    {
      cat(file=stderr(),"--resetting\n");
      shinyjs::reset("inputs");
      # shinyjs::reset("pM");
      # shinyjs::reset("pDM1");
      # shinyjs::reset("pDM2");
      cat(file=stderr(),"--reset!\n");
      }
  );

    #calculate reccruitment-at-size info when user clicks "Refresh" button
    nmInfo<-eventReactive(
      c(input$refresh1,configInfo()),
      {
        cat(file=stderr(),"--starting natmortServer:nmInfo()\n");
        res<-calcNaturalMortality(configInfo(),input);
        ret<-list(inpInfo =input,
                  params  =res$params,
                  M_ms    =res$M_ms,
                  mdfrM_ms=res$mdfrM_ms);
        cat(file=stderr(),"--finished natmortServer:nmInfo()\n");
        return(ret);
      },
      ignoreNULL=TRUE
    );

    #create ggplot of recruitment size distribution
    plotNM<-reactive(
      {
        cat(file=stderr(),"--starting natmortServer:plotNM()\n");
        mdfr<-nmInfo()$mdfrM_ms;
        pl <- ggplot2::ggplot(ggplot2::aes(x=case,y=val,fill=m),data=mdfr);
        pl <- pl + ggplot2::geom_bar(stat='identity',position='dodge');
        pl <- pl + ggplot2::labs(x='x',y='Natural Mortality (year^-1)');
        pl <- pl + ggplot2::guides(fill=ggplot2::guide_legend('case'));
        pl <- pl + ggplot2::facet_grid(s~m);
        cat(file=stderr(),"--finished natmortServer:plotNM()\n");
        return(pl);
      }
    );

    #render recruitment size distribution plot
    output$pltNM<-renderPlot(plotNM());

    obj<-reactive(list(nmInfo=nmInfo(),plotNM=plotNM()));#can't return elements in output here
    return(obj); #here if returning an R object to the enclosing server
  } #function(input,output,session)
