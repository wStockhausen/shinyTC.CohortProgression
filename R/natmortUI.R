#'
#' @title UI for the Shiny Tanner crab natural mortality module
#'
#' @description Function to create the UI for the Shiny Tanner crab natural mortality module.
#'
#' @param id - a character string that uniquely identifies the natural mortality module in the enclosing Shiny UI.
#'
#' @return A Shiny tabPanel allowing the user to change natural mortality parameters and plot the results.
#'
#' @details Allows the user to change natural mortality parameters and plot the reults for the Tanner crab model. Default
#' parameter values are from males from the 2018 assessment model.
#'
#' @import shiny
#'
natmortUI<-function(id){
  require(shiny);
  ns<-NS(id); #namespace function
  tabPanel(
    "M rates",
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
                fluidRow(textInput(ns("pM"),"pM: base rate for M",value="0.23")),
                fluidRow(textInput(ns("pDM1"),"pDM1: immature crab multiplier",value="1.00238960095")),
                fluidRow(textInput(ns("pDM2"),"pDM2:   mature crab multiplier",value="1.15247833771"))
              ) #wellPanel
            ), #parameters tabPanel
            tabPanel(
              "plot controls",
              wellPanel(
                fluidRow(
                  column(
                    12,
                    "size (mm CW)",
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
            h3("Natural mortality rates"),
            plotOutput(ns("pltNM"))
          ) #column
        ) #fluidRow
      )#mainPanel
    ) #sidebarLayout
  ) #tabPanel
}
