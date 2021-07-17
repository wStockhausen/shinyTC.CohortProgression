#'
#' @title UI for the Shiny Tanner crab equilibrium size distribution module
#'
#' @description Function to create the UI for the Shiny Tanner crab equilibrium size distribution module.
#'
#' @param id - a character string that uniquely identifies the equilibrium size distribution module in the enclosing Shiny UI.
#'
#' @return A Shiny tabPanel allowing the user to change the equilibrium size distribution parameters and plot the results.
#'
#' @details Allows the user to change the equilibrium size distribution parameters and plot the results for the Tanner crab model.
#'
#' @import shiny
#'
eqzUI<-function(id){
  require(shiny);
  ns<-NS(id); #namespace function
  tabPanel(
    "Eq. size dist.",
    sidebarLayout(
      sidebarPanel(
        wellPanel(
          fluidRow(
            actionButton(ns("refresh1"),"Refresh"),
            actionButton(ns("reset1"),"Reset")
          ), #fluidRow
          fluidRow(
            downloadButton(ns("save1"), "Export RData"),
            downloadButton(ns("save2"), "Export pdf  ")
          ) #fluidRow
        ), #wellPanel
        div(
          id=ns("inputs"),
          useShinyjs(),
          tabsetPanel(
            tabPanel(
              "plot controls",
              wellPanel(
                selectInput(ns("plotType"),"plot type",
                                     list(`size alone`="z",`by m`="m+z",`by m+s`="m+s+z"),
                                     selected="size alone",multiple=FALSE),
                h4("Use free y-axis scales?"),
                fluidRow(
                  column(6,checkboxInput(ns("yScaleFreeA"),"abundance",value=TRUE)),
                  column(6,checkboxInput(ns("yScaleFreeN"),"size comps",value=FALSE))
                ), # fluidRow
                fluidRow(
                  column(
                    12,
                    h4("Sizes to show (mm CW)"),
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
        h3("Equilibrium size distribution"),
        fluidRow(
          column(
            12,
            h4("Relative abundance"),
            plotOutput(ns("pltEqZA"))
          ) #column
        ), #fluidRow
        fluidRow(
          column(
            12,
            h4("Normalized size compositions"),
            plotOutput(ns("pltEqZN"))
          ) #column
        ) #fluidRow
      )#mainPanel
    ) #sidebarLayout
  ) #tabPanel
}
