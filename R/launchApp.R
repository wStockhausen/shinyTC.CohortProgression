#'
#' @title Get the shinyTC_CohortProgression app to run it
#'
#' @description Function to get the shinyTC_CohortProgression app to run it.
#'
#' @return shiny application object (which is run using shiny::runApp(obj,...))
#'
#' @details To run the app from the commandline or in an RStudio session, use
#'shiny::runApp(shinyTC.CohortProgression::launchApp())
#'
#' @import shiny
#'
#' @export
#'

launchApp<-function(){
  shinyApp(ui = shinyUI, server = shinyServer);
  #shinyAppDir(system.file("",package="shinyTC.CohortProgression"))
}
