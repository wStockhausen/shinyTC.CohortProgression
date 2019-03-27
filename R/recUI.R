#'
#' @title UI for the Shiny Tanner crab recruitment size distribution module
#'
#' @description Function to create the UI for the Shiny Tanner crab recruitment size distribution module.
#'
#' @param id - a character string that uniquely identifies the recruitment size distribution module in the enclosing Shiny UI.
#'
#' @return A Shiny tabPanel allowing the user to change recruitment size distribution parameters and plot the distribution.
#'
#' @details Allows the user to change recruitment size distribution parameters and plot the distribution for the Tanner crab model.
#'
#' @import shiny
#'
recUI<-function(id){
  require(shiny);
  ns<-NS(id); #namespace function
  tabPanel(
    "Recruitment",
    sidebarLayout(
      sidebarPanel(
        wellPanel(
          fluidRow(
            actionButton(ns("refresh1"),"Refresh"),
            actionButton(ns("reset1"),"Reset")
          ) #fluidRow
        ), #wellPanel
        div(
          id=ns("inputs"),
          useShinyjs(),
          tabsetPanel(
            tabPanel(
            "parameters",
              wellPanel(
                h4("Size (gamma distribution)"),
                fluidRow(textInput(ns("alpha"),"alpha: location parameter",value="exp(2.44234703500)")),
                fluidRow(textInput(ns("beta"),"beta: scale parameter",value="exp(1.38629436100)")),
                fluidRow(numericInput(ns("mxZ"),"max size for recruits",value=50,min=0,max=200))
              ) #wellPanel
            ), #parameters tabPanel
            tabPanel(
              "plot controls",
              wellPanel(
                fluidRow(
                  column(
                    12,
                    h4("size (mm CW)"),
                    fluidRow(
                      column(6,numericInput(ns("minX"),"min",value= 25,min=0)),
                      column(6,numericInput(ns("maxX"),"max",value=185,min=0))
                    ) #fluidRow
                  ) #column
                )
              ) #wellPanel
            ) #controls tabPanel
          ) #tabsetPanel
        ) #div
      ), #sidebarPanel
      mainPanel(
        fluidRow(
          column(
            12,
            h3("Recruitment size distribution"),
            plotOutput(ns("pltPrRecAtZ"))
          ) #column
        ) #fluidRow
      )#mainPanel
    ) #sidebarLayout
  ) #tabPanel
}
