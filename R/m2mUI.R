#'
#' @title UI for the Shiny Tanner crab molt-to-maturity module
#'
#' @description Function to create the UI for the Shiny Tanner crab molt-to-maturity module.
#'
#' @param id - a character string that uniquely identifies the molt-to-maturity module in the enclosing Shiny UI.
#'
#' @return A Shiny tabPanel allowing the user to change molt-to-maturity parameters and plot molt-to-maturity probabilities.
#'
#' @details Allows the user to change molt-to-maturity parameters and plot molt-to-maturity probabilities for the Tanner crab model.
#'
#' @import shiny
#'
m2mUI<-function(id){
  require(shiny);
  ns<-NS(id); #namespace function
  tabPanel(
    "Terminal molt",
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
              checkboxInput(ns("useNonParametric"),"use non-parametric curve?",value=TRUE),
              wellPanel(
                h4("non-parametric curve"),
                textInput(ns("pLgtM2M"),"logit-scale parameter values",value=
                         "-12.0290295859 -10.8459244198 -9.66281987204 -8.48078523195 -7.31044007393 -6.16369607006 -5.11272376055 -4.48660988289 -4.09981698700 -3.46263118831 -2.92775038662 -2.49750806437 -2.02616478824 -1.43921779828 -0.951890781653 -0.681680218915 -0.532510891830 -0.0623780676816 0.559935830111 1.43517391684 2.81000938072 5.06372359748 7.19725305500 9.01005694604 10.4956932415 11.6879757794 12.6272947367 13.3554316432 13.9145213347 14.3468009094 14.6945362364 14.9999992134"
                         ),
                numericInput(ns("n0"),"no. of initial size bins at which prM2M=0",value=0,min=0,max=100)
              ), #wellPanel
              wellPanel(
                h4("parametric (logistic) curve"),
                fluidRow(
                  numericInput(ns("z50"),"z50: size (mm CW) at 50% prob.",value=70,min=0,max=200),
                  numericInput(ns("b50"),"b50: scale parameter (mm CW).",value=1,min=0,max=50)
                ), #fluidRow
                numericInput(ns("z0"),"size at which prM2M becomes non-zero",value=0,min=0,max=200)
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
            h3("Probability of molt-to-maturity"),
            plotOutput(ns("pltPrM2M"))
          ) #column
        ) #fluidRow
      )#mainPanel
    ) #sidebarLayout
  ) #tabPanel
}
