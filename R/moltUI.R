#'
#' @title UI for the Shiny Tanner crab probability of molt module
#'
#' @description Function to create the UI for the Shiny Tanner crab probability of molt module.
#'
#' @param id - a character string that uniquely identifies the probability of molt module in the enclosing Shiny UI.
#'
#' @return A Shiny tabPanel allowing the user to change probability of molt parameters and plot probabilities of molt.
#'
#' @details Allows the user to change probability of molt parameters and plot probabilities of molt for the Tanner crab model.
#'
#' @import shiny
#'
moltUI<-function(id){
  require(shiny);
  ns<-NS(id); #namespace function
  tabPanel(
    "Molt probability",
    sidebarLayout(
      sidebarPanel(
        wellPanel(
          fluidRow(
            actionButton(ns("refresh1"),"Refresh"),
            actionButton(ns("reset1"),"Reset")
          ) #fluidRow
        ),#wellPanel
        div(
          id=ns("inputs"),
          useShinyjs(),
          tabsetPanel(
            tabPanel(
            "parameters",
              wellPanel(
                fluidRow(
                  numericInput(ns("z50"),"z50: size (mm CW) at inflection",value=70,min=0),
                  numericInput(ns("b50"),"b50: scale parameter (mm CW)",value=-1,min=-50),
                  numericInput(ns("min"),"min: min probability.",value=0.5,min=0,max=1)
                ), #fluidRow
                numericInput(ns("z1"),"size at which prMolt becomes less than 1",value=200,min=0)
              ) #wellPanel
            ), #parameters tabPanel
            tabPanel(
              "plot controls",
              wellPanel(
                fluidRow(
                  column(
                    12,
                    h4("pre-molt sizes (mm CW)"),
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
            h3("Annual molt probability"),
            plotOutput(ns("pltPrMolt"))
          ) #column
        ) #fluidRow
      )#mainPanel
    ) #sidebarLayout
  ) #tabPanel
}
