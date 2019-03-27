#'
#' @title Launch the shinyTC_CohortProgression app
#'
#' @description Function to launch the shinyTC_CohortProgression app.
#'
#' @return shiny application object
#'
#' @example \dontrun {launchApp()}
#'
#' @import shiny
#'
#' @export
#'

launchApp<-function(){
  shinyApp(ui = shinyUI, server = shinyServer);
  #shinyAppDir(system.file("",package="shinyTC.CohortProgression"))
}
