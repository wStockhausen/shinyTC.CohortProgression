debugWithReactiveLog<-function(){
  options(shiny.reactlog=TRUE)
  cat("Press Command+F3 (or Ctrl+F3) at some point to launch the reactive log visualization\n");
  cat("If using reactives outside a Shiny app, run 'showReactLog()' at some point to visulaize the reactive log up to that point.\n")
}
