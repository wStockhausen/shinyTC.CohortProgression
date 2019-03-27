# shinyTC.CohortProgression
An R package encapsulating a Shiny app to explore Tanner crab cohort progressions

##Introduction
This R package encapsulates a Shiny app that allows the user to explore the effects of changes in various life history and population dynamics processes on the progression of a cohort of Tanner crab (*Chionoecetes bairdi*) in the eastern Bering Sea. 

The packaging of this app is based on the article https://www.r-bloggers.com/packaging-shiny-applications-a-deep-dive/.

##Installation
```r
devtools::install_github("wStockhausen/shinyTC.CohortProgression")
```

##Run the app
To launch the app, run 
```r
  shinyTC.CohortProgression::launchApp()
```
  
from the commandline in R (or RStudio).
