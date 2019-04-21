#'
#' @title Server function for the Tanner crab model cohort progression Shiny module
#'
#' @description Server function for the Tanner crab model cohort progressionn Shiny module.
#'
#' @param input - the usual shiny input variable
#' @param output - the usual shiny output variable
#' @param configInfo - reactive variable yielding a list with model configuration info
#' @param session - the usual shiny session variable
#'
#' @return reactive object which returns a list with elements
#' <ul>
#'   <li> cpInfo -  a list with elements inpInfo, prR_z, and mdfrPrRecAtZ (see \code{\link{calcCohortProgression}})</li>
#'   <li> pltCPA - a ggplot2 object
#'   <li> pltCPN - a ggplot2 object
#' </ul>
#'
#'
#' @details Updates the parameters, results, and associated ggplot2 objects
#' associated with the Tanner crab cohort progression model.
#'
#' @import shiny
#'
cohortServer<-function(input,
                       output,
                       configInfo,
                       nmResults,
                       mltResults,
                       grwResults,
                       m2mResults,
                       recResults,
                       session) {

  observe(
    {
      cat(file=stderr(),"--cohortServer: observer()\n")
    },
    label="cohortObserver"
  );

    #calculate cohort info when user clicks "Refresh" button
    cpInfo<-eventReactive(
      c(input$refresh1,configInfo(),nmResults(),mltResults(),grwResults(),m2mResults(),recResults()),
      {
        cat(file=stderr(),"--starting cohortServer:calcCP()\n")
        res<-calcCohortProgression(configInfo(),
                                   nmResults()$nmInfo,
                                   mltResults()$moltInfo,
                                   grwResults()$growthInfo,
                                   m2mResults()$m2mInfo,
                                   recResults()$recInfo,
                                   input,
                                   verbose=FALSE);
        ret<-list(inpInfo=input,
                  cpParams    =res$params,
                  prR_z     =res$prR_z,
                  M_ms      =res$M_ms,
                  prM_z     =res$prM_z,
                  prG_zz    =res$prG_zz,
                  prT_z     =res$prT_z,
                  n_ymsz    =res$n_ymsz,
                  mdfrN_ymsz=res$mdfrN_ymsz,
                  p_ymsz    =res$p_ymsz,
                  mdfrP_ymsz=res$mdfrP_ymsz,
                  n_msz     =res$n_msz,
                  mdfrN_msz =res$mdfrN_msz,
                  p_msz     =res$p_msz,
                  mdfrP_msz =res$mdfrP_msz);
        cat(file=stderr(),"--finished cohortServer:calcCP()\n")
        return(ret);
      }
    );

    #reset input values
    observeEvent(
      input$reset1,
      {
        cat(file=stderr(),"--cohortServer:resetting\n");
        shinyjs::reset("inputs");
        cat(file=stderr(),"--cohortServer:reset!\n");
        }
    );

    # #save results and plots to RData file when user clicks "Save" button
    # output$save1<-downloadHandler(
    #   filename=function(){paste0('results.CohortProgression-',Sys.Date(),'.RData')},
    #   content=function(file){
    #     cat(file=stderr(),"--Saving results to '",file,"'\n")
    #     configInfo  <-isolate(configInfo());
    #     nmResults   <-isolate(nmResults());
    #     mltResults  <-isolate(mltResults());
    #     grwResults  <-isolate(grwResults());
    #     m2mResults  <-isolate(m2mResults());
    #     recResults  <-isolate(recResults());
    #     cpResults   <-list(cpInfo=isolate(cpInfo()),
    #                       pltCPA=isolate(plotCPA()),
    #                       pltCPN=isolate(plotCPN()));
    #     save(configInfo,nmResults,mltResults,
    #          grwResults,m2mResults,recResults,cpResults,
    #          file=file);
    #     cat(file=stderr(),"--Results saved to '",file,"'\n")
    #   }
    # );
    #
    # #create a pdf report via RMarkdown
    # output$save2<-downloadHandler(
    #   filename=function(){paste0('report.CohortProgression-',Sys.Date(),'.pdf')},
    #   content=function(file){
    #     cat(file=stderr(),"--cohortServer: Creating pdf '",file,"'\n");
    #     #copy Rmd files to temporary folder
    #     tmpd <- tempdir();
    #     tempRep<-file.path(tmpd,"report.CohortProgression.Rmd");
    #     file.copy(system.file("rmd/report.CohortProgression.Rmd",package="shinyTC.CohortProgression"),tempRep,overwrite=TRUE);
    #     tmp<-file.path(tmpd,"report.PrM2M.Nonparametric.Rmd");
    #     file.copy(system.file("rmd/report.PrM2M.Nonparametric.Rmd",package="shinyTC.CohortProgression"),tmp,overwrite=TRUE);
    #     tmp<-file.path(tmpd,"report.PrM2M.Parametric.Rmd");
    #     file.copy(system.file("rmd/report.PrM2M.Parametric.Rmd",package="shinyTC.CohortProgression"),tmp,overwrite=TRUE);
    #     #set up params list to pass to Rmd document
    #     params<-list(
    #       configInfo = isolate(configInfo()),
    #       nmResults  = isolate(nmResults()),
    #       mltResults = isolate(mltResults()),
    #       grwResults = isolate(grwResults()),
    #       m2mResults = isolate(m2mResults()),
    #       recResults = isolate(recResults()),
    #       cpResults  = list(cpInfo=isolate(cpInfo()),
    #                         pltCPA=isolate(plotCPA()),
    #                         pltCPN=isolate(plotCPN())),
    #       eqzResults = isolate(eqzResults())
    #     );
    #
    #     #render file
    #     pdf_styles    <-system.file("rmd/StylesForRmdPDFs.sty",package="wtsUtilities")
    #     cat(file=stderr(),"--cohortServer: pdf-styles = '",pdf_styles,"'\n");
    #     output_options<-list(includes=list(in_header=pdf_styles));
    #     res <- rmarkdown::render(tempRep,
    #                       output_file=file,
    #                       output_options=output_options,
    #                       params=params,
    #                       envir=new.env(parent=globalenv()),
    #                       clean=TRUE);
    #
    #     cat(file=stderr(),"--cohortServer: Created pdf '",file,"'\n")
    #     return(res);
    #   }
    # );

    #create list of ggplot objects for cohort abundance progression
    plotCPA<-reactive(
      {
        cat(file=stderr(),"Starting cohortServer:plotCPA()\n");
        years<-input$minAgeToShow:input$maxAgeToShow;
        mxy<-length(years);
        p<-rCompTCMs::plotPop.CohortProgression(
          cpInfo()$mdfrN_ymsz,
          facet_grid=x~.,
          types=input$plotType,
          years=years,
          mxy=mxy,
          scales=ifelse(input$yScaleFreeA,"free_y","fixed"),
          showPlot=FALSE,
          verbose=TRUE
        );
        for (i in 1:length(p)) {
          names(p)[i]<-"\n  \nFigure &&figno. Absolute scale size comps showing cohort progression.  \n  \n"
        }
        cat(file=stderr(),"Finished cohortServer:plotCPA()\n");
        return(p);
      }
    )

    #create list of ggplot objects for cohort size comps progression
    plotCPN<-reactive(
      {
        cat(file=stderr(),"Starting cohortServer:plotCPN()\n");
        years<-input$minAgeToShow:input$maxAgeToShow;
        mxy<-length(years);
        p<-rCompTCMs::plotPop.CohortProgression(
          cpInfo()$mdfrP_ymsz,
          facet_grid=x~.,
          types=input$plotType,
          years=years,
          mxy=mxy,
          scales=ifelse(input$yScaleFreeN,"free_y","fixed"),
          showPlot=FALSE,
          verbose=TRUE
        );
        cat(file=stderr(),paste0("class(p) = ",class(p),"\n"));
        cat(file=stderr(),paste0("length(p) = ",length(p),"\n"));
        for (i in 1:length(p)) {
          p[[i]] <- p[[i]]+ggplot2::ylab("normalized abundance");
          names(p)[i]<-"\n  \nFigure &&figno. Normalized size comps showing cohort progression.  \n  \n"
        }
        cat(file=stderr(),"Finished cohortServer:plotCPN()\n");
        return(p);
      }
    )

    #render cohort abundance progression plot
    output$pltCPA<-renderPlot({
        cat(file=stderr(),"Starting cohortServer:renderPlot(pltCPA)\n");
        p<-plotCPA();
        cat(file=stderr(),"Finished cohortServer:renderPlot(pltCPA)\n");
        return(p);
    });

    #render cohort size comps progression plot
    output$pltCPN<-renderPlot({
        cat(file=stderr(),"Starting cohortServer:renderPlot(pltCPN)\n");
        p<-plotCPN();
        cat(file=stderr(),"Finished cohortServer:renderPlot(pltCPN)\n");
        return(p);
    });

    obj<-reactive(list(cpInfo=cpInfo(),
                       plotCPA=plotCPA(),
                       plotCPN=plotCPN()))
    return(obj); #here if returning an R object to the enclosing server
  } #function(input,output,session)
