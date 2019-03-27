#'
#'@title UI for shiny Cohort Progression app
#'
#'@description UI for shiny Cohort Progression app.
#'
#'@params none
#'
#'@return the html UI for the app
#'
#'@details Creates the UI for the app.
#'
shinyUI<-navbarPage(
    "Tanner crab model",
    configUI("config"),
    natmortUI("natmort"),
    moltUI("molt"),
    growthUI("growth"),
    m2mUI("m2m"),
    recUI("rec"),
    cohortUI("cohort")
  )

