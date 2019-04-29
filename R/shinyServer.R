#'
#'@title Server for the shiny Cohort Progression app
#'
#'@description Server for the shiny Cohort Progression app.
#'
#' @param input - the usual shiny input variable
#' @param output - the usual shiny output variable
#' @param session - the usual shiny session variable
#'
#'@return the html UI for the app
#'
#'@details Creates the UI for the app.
#'
#'@import shiny
#'

#shinyServer(
shinyServer<-function(input, output, session) {
    #model configuration info
    configInfo <- callModule(configServer,"config",session=session);#return reactive variable

    #natural mortality rates
    nmResults <- callModule(natmortServer,"natmort",configInfo=configInfo,session=session);

    #molt probability
    mltResults <- callModule(moltServer,"molt",configInfo=configInfo,session=session);

    #growth
    grwResults<-callModule(growthServer,"growth",configInfo=configInfo,session=session);

    #probability of molt-to-maurity
    m2mResults<-callModule(m2mServer,"m2m",configInfo=configInfo,session=session);

    #reccruitment size distribution
    recResults<-callModule(recServer,"rec",
                           configInfo=configInfo,
                           session=session);

    #cohort progression
    cpResults<-callModule(cohortServer,"cohort",
                          configInfo=configInfo,
                          nmResults =nmResults,
                          mltResults=mltResults,
                          grwResults=grwResults,
                          m2mResults=m2mResults,
                          recResults=recResults,
                          session=session);

    #equilibrium size distribution
    eqzResults<-callModule(eqzServer,"eqz",
                          configInfo=configInfo,
                          nmResults =nmResults,
                          mltResults=mltResults,
                          grwResults=grwResults,
                          m2mResults=m2mResults,
                          recResults=recResults,
                          cpResults=cpResults,
                          session=session);

  } #function(input,output,session)
#) #shinyServer
