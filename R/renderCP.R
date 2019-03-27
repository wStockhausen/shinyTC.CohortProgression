renderCP<-function(params,clean=TRUE){
  filename<-paste0('./report.CohortProgression-',Sys.Date(),'.pdf');
  cat(file=stderr(),paste0("--Creating pdf '",filename,"'\n"));
  #copy Rmd file to temporary folder
  tempRep<-file.path("./","report.CohortProgression.Rmd"); #substitute tempdir() for "./"
  cat(file=stderr(),paste0("Using temporary file\n\t'",tempRep,"'\n"));
  #file.copy("report.CohortProgression.Rmd",tempRep,overwrite=TRUE);

  #render file
  pdf_styles    <-system.file("rmd/StylesForRmdPDFs.sty",package="wtsUtilities")
  cat(file=stderr(),paste0("--renderCP: pdf-styles = '",pdf_styles,"'\n"));
  output_options<-list(includes=list(in_header=pdf_styles));
  res <- rmarkdown::render(tempRep,
                    output_file=filename,
                    output_options=output_options,
                    params=params,
                    envir=new.env(parent=globalenv()),
                    clean=clean);

  cat(file=stderr(),paste0("--Created pdf '",filename,"'\n"));
  return(res);
}

  # #set up params list to pass to renderCP/Rmd document
  # params<-list(
  #   configInfo = configInfo,
  #   nmResults  = nmResults,
  #   mltResults = mltResults,
  #   grwResults = grwResults,
  #   m2mResults = m2mResults,
  #   recResults = recResults,
  #   cpResults  = cpResults
  # );
  #
  # renderCP(params,clean=TRUE);
