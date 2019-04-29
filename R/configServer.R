#'
#' @title Server function for the Tanner crab model configuration Shiny module
#'
#' @description Server function for the Tanner crab model configuration Shiny module.
#'
#' @param input - usual Shiny input variable
#' @param output - usual Shiny output variable
#' @param session - usual Shiny session variable
#'
#' @return configInfo - reactive variable yielding list of model configuration info
#'
#' @details Creates and returns a reactive variable (\code{\link{configInfo}}) using
#' the function \code{\link{setConfigInfo}} that reflects the
#' current model configuration state, which is a list with elements:<cr>
#' <ul>
#'  <li> moltTime - timing of molting/growth as fraction of year </li>
#'  <li> maturityStates - maturity states </li>
#'  <li> shellConditions - shell conditions </li>
#'  <li> binZ - size bin width (mm CW) </li>
#'  <li> minZ - min size cutpoint (mm CW) </li>
#'  <li> maxZ - max size cutpoint (mm CW) </li>
#'  <li> zBs - size bins (mm CW) </li>
#'  <li> Zcs - size bin cutpoints (mm CW) </li>
#' </ul>
#'
#' @import shiny
#'
configServer<-function(input, output, session) {

    observe(
      {
        cat(file=stderr(),"--configServer:observer()\n")
        # cat("configServer: names(input)   =",names(input),"\n")
        # cat("configServer: class(input)   =",class(input),"\n")
        # cat("configServer: names(output)  =",names(output),"\n")
        # cat("configServer: class(output)  =",class(output),"\n")
        # cat("growthServer: names(session) =",names(session),"\n")
        # cat("configServer: class(session) =",class(session),"\n")
      },
      label="configObserver"
    );

  observeEvent(
    input$reset1,
    {
      cat(file=stderr(),"--configServer:resetting\n");
      shinyjs::reset("inputs");
      cat(file=stderr(),"--configServer:reset!\n");
      }
  );

    #set model configuration
    configInfo<-reactive(
      {
        cat(file=stderr(),"--starting configServer:configInfo()\n");
        ret<-setConfigInfo(input);
        cat(file=stderr(),"--finished configServer:configInfo()\n");
        return(ret);
      }
    );

    return(configInfo); #if returning an R object to the enclosing server
  } #configInfo(input,output,session)
