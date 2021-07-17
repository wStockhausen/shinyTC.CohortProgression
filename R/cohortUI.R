#'
#' @title UI for the Shiny Tanner crab recruitment cohort progression module
#'
#' @description Function to create the UI for the Shiny Tanner crab cohort progression module.
#'
#' @param id - a character string that uniquely identifies the cohort progression module in the enclosing Shiny UI.
#'
#' @return A Shiny tabPanel allowing the user to change the cohort progression parameters and plot the results.
#'
#' @details Allows the user to change the cohort progression parameters and plot the results for the Tanner crab model.
#'
#' @import shiny
#'
cohortUI<-function(id){
  require(shiny);
  ns<-NS(id); #namespace function
  tabPanel(
    "Cohort progression",
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
            "parameters",
              wellPanel(
                fluidRow(numericInput(ns("maxAge"),"max age to track",value=8,min=0,max=30))
              ) #wellPanel
            ), #parameters tabPanel
            tabPanel(
              "plot controls",
              wellPanel(
                selectInput(ns("plotType"),"plot type",
                                     list(progression="progression",`by year`="byyear"),
                                     selected="progression",multiple=FALSE),
                h4("Use free y-axis scales?"),
                fluidRow(
                  column(6,checkboxInput(ns("yScaleFreeA"),"abundance",value=TRUE)),
                  column(6,checkboxInput(ns("yScaleFreeN"),"size comps",value=FALSE))
                ), # fluidRow
                fluidRow(
                  column(
                    12,
                    h4("Ages to show"),
                    fluidRow(
                      column(6,numericInput(ns("minAgeToShow"),"min",value=0,min=0,max=30)),
                      column(6,numericInput(ns("maxAgeToShow"),"max",value=8,min=0,max=30))
                    ) # fluidRow
                  ) #column
                ), #fluidRow
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
        h3("Cohort progression"),
        fluidRow(
          column(
            12,
            h4("Relative abundance"),
            plotOutput(ns("pltCPA"))
          ) #column
        ), #fluidRow
        fluidRow(
          column(
            12,
            h4("Normalized size compositions"),
            plotOutput(ns("pltCPN"))
          ) #column
        ) #fluidRow
      )#mainPanel
    ) #sidebarLayout
  ) #tabPanel
}
