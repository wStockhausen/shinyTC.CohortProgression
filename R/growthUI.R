#'
#' @title UI for the Shiny Tanner crab growth module
#'
#' @description Function to create the UI for the Shiny Tanner crab growth module.
#'
#' @param id - a character string that uniquely identifies the growth module in the enclosing Shiny UI.
#'
#' @return A Shiny tabPanel allowing the user to change growth parameters and plot mean growth and size transition probabilities.
#'
#' @details Allows the user to change growth parameters and plot mean growth and size transition probabilities for the Tanner crab model.
#'
#' @import shiny
#'
growthUI<-function(id){
  require(shiny);
  ns<-NS(id); #namespace function
  tabPanel(
    "Growth",
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
                fluidRow(numericInput(ns("zA"),h5("zA: reference pre-molt size (mm CW)"),min=0,value=25)),
                fluidRow(numericInput(ns("pA"),h5("pA: mean post-molt size (mm CW) at zA"),min=0,value=33.0888265902)),
                fluidRow(numericInput(ns("zB"),h5("zB: reference pre-molt size (mm CW)"),min=0,value=125)),
                fluidRow(numericInput(ns("pB"),h5("pB: mean post-molt size (mm CW) at zB"),min=0,value=166.95985413)),
                fluidRow(numericInput(ns("pBeta"),h5("pBeta: scale factor"),min=0,value=0.811647719391)),
                sliderInput(ns("maxZBEx"),"max bin range for growth",value=10,min=1,max=50,step=1)
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
                    ), #fluidRow
                    fluidRow(
                      sliderInput(ns("skip"),"number of pre-molt size bins to skip",value=0,min=0,max=10,step=1)
                    )
                  ) #column
                ),
                fluidRow(
                  column(
                    12,
                    h4("post-molt sizes (mm CW)"),
                    fluidRow(
                      column(6,numericInput(ns("minY"),"min",value= 25,min=0)),
                      column(6,numericInput(ns("maxY"),"max",value=185,min=0))
                    ) #fluidRow
                  ) #column
                ), #fluidRow
                sliderInput(ns("scale"),"probability scale",value=10,min=1,max=50,step=1)
              ) #wellPanel
            ) #controls tabPanel
          ) #tabsetPanel
        ) #div
      ), #sidebarPanel
      mainPanel(
        fluidRow(
          column(
            12,
            h3("Size transition probabilities"),
            plotOutput(ns("pltPrG"))
          ) #column
        ) #fluidRow
        # fluidRow(
        #   column(
        #     12,
        #     h3("Mean growth"),
        #     plotOutput(ns("pltMnG"))
        #   ) #column
        # ) #fluidRow
      )#mainPanel
    ) #sidebarLayout
  ) #tabPanel
}
