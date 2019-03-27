#'
#' @title UI for model configuration ("config") Shiny module
#'
#' @title UI for the Tanner crab model configuration Shiny module
#'
#' @description Function to create the UI for the Tanner crab model configuration ("config") Shiny module
#'
#' @param id - a character string that uniquely identifies the model configuration module in the enclosing Shiny UI.
#'
#' @return A Shiny tabPanel allowing the user to change the model configuration.
#'
#' @details Allows the user to change bin size, min size, and max size for the Tanner crab model.
#'
#' @import shiny
#' @import shinyjs
#'
configUI<-function(id){
  ns<-NS(id); #namespace function for this module
  tabPanel(
    "Configuration",
    div(
      id=ns("inputs"),
      useShinyjs(),
      fluidPage(
        column(6,
          wellPanel(
            wellPanel(actionButton(ns("reset1"),"Reset")),
            wellPanel(
              h5("sizes (mm CW)"),
              fluidRow(
                column(6,numericInput(ns("minZ"),"min",value= 25,min=0)),
                column(6,numericInput(ns("maxZ"),"max",value=185,min=0))
              ), #fluidRow
              sliderInput(ns("binZ"),"bin size",value=5,min=1,max=20,step=1)
            ) #wellPanel
          ) #wellPanel
        ), #column
        column(6,
          wellPanel(
            wellPanel(
              fluidRow(numericInput(ns("moltTime"),"molt timing (fraction of year)", value=0.625,min=0,max=1))
            ), #wellPanel
            wellPanel(
              fluidRow(textInput(ns("mss"),"maturity states", value='c("immature","mature")')),
              fluidRow(textInput(ns("scs"),"shell conditions",value='c("new shell","old shell")'))
            ) #wellPanel
          ) #wellPanel
        ) #column
      ) #fluidPage
    ) #div
  ) #tabPanel
}
