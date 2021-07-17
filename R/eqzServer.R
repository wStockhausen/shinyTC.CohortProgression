#'
#' @title Server function for the Tanner crab model equilibrium size distribution Shiny module
#'
#' @description Server function for the Tanner crab model equilibrium size distribution Shiny module.
#'
#' @param input - the usual shiny input variable
#' @param output - the usual shiny output variable
#' @param configInfo - reactive variable yielding a list with model configuration info
#' @param nmResults - natural mortality results list
#' @param mltResults - probability of molt results list
#' @param grwResults - growth results list
#' @param m2mResults - molt-to-maturity results list
#' @param recResults - recruitment results list
#' @param cpReults - cohort progression results list
#' @param session - the usual shiny session variable
#'
#' @return reactive object which returns a list with elements
#' <ul>
#'   <li> cpInfo -  a list with elements inpInfo, prR_z, and mdfrPrRecAtZ (see \code{\link{calcCohortProgression}})</li>
#'   <li> pltPrRecAtZ - a ggplot2 object
#' </ul>
#'
#'
#' @details Updates the parameters, results, and associated ggplot2 objects
#' associated with the Tanner crab cohort progression model.
#'
#' @import shiny
#'
eqzServer<-function(input,
                       output,
                       configInfo,
                       nmResults,
                       mltResults,
                       grwResults,
                       m2mResults,
                       recResults,
                       cpResults,
                       session) {

  observe(
    {
      cat(file=stderr(),"--eqzServer: observer()\n")
    },
    label="eqzObserver"
  );

    #calculate eqz info when user clicks "Refresh" button
    eqzInfo<-eventReactive(
      c(input$refresh1,configInfo(),nmResults(),mltResults(),grwResults(),m2mResults(),recResults(),cpResults),
      {
        cat(file=stderr(),"--starting eqzServer:getCP()\n")
        ret<-list(inpInfo=input,
                  cpParams  =cpResults()$cpInfo$c,
                  n_msz     =cpResults()$cpInfo$n_msz,
                  mdfrN_msz =cpResults()$cpInfo$mdfrN_msz,
                  p_msz     =cpResults()$cpInfo$p_msz,
                  mdfrP_msz =cpResults()$cpInfo$mdfrP_msz);
        cat(file=stderr(),"--finished eqzServer:getCP()\n")
        return(ret);
      }
    );

    #reset input values
    observeEvent(
      input$reset1,
      {
        cat(file=stderr(),"--eqzServer:resetting\n");
        shinyjs::reset("inputs");
        cat(file=stderr(),"--eqzServer:reset!\n");
        }
    );

    #save results and plots to RData file when user clicks "Save" button
    output$save1<-downloadHandler(
      filename=function(){paste0('results.CohortProgression-',Sys.Date(),'.RData')},
      content=function(file){
        cat(file=stderr(),"--Saving results to '",file,"'\n")
        configInfo  <-isolate(configInfo());
        nmResults   <-isolate(nmResults());
        mltResults  <-isolate(mltResults());
        grwResults  <-isolate(grwResults());
        m2mResults  <-isolate(m2mResults());
        recResults  <-isolate(recResults());
        cpResults   <-isolate(cpResults());
        eqzResults  <-list(eqzInfo=isolate(eqzInfo()),
                          pltEqZA=isolate(plotEqZA()),
                          pltEqZN=isolate(plotEqZN()));
        save(configInfo,nmResults,mltResults,
             grwResults,m2mResults,recResults,
             cpResults,eqzResults,
             file=file);
        cat(file=stderr(),"--Results saved to '",file,"'\n")
      }
    );

    #create a pdf report via RMarkdown
    output$save2<-downloadHandler(
      filename=function(){paste0('report.CohortProgression-',Sys.Date(),'.pdf')},
      content=function(file){
        cat(file=stderr(),"--cohortServer: Creating pdf '",file,"'\n");
        #copy Rmd files to temporary folder
        tmpd <- tempdir();
        tempRep<-file.path(tmpd,"report.CohortProgression.Rmd");
        file.copy(system.file("rmd/report.CohortProgression.Rmd",package="shinyTC.CohortProgression"),tempRep,overwrite=TRUE);
        tmp<-file.path(tmpd,"report.PrM2M.Nonparametric.Rmd");
        file.copy(system.file("rmd/report.PrM2M.Nonparametric.Rmd",package="shinyTC.CohortProgression"),tmp,overwrite=TRUE);
        tmp<-file.path(tmpd,"report.PrM2M.Parametric.Rmd");
        file.copy(system.file("rmd/report.PrM2M.Parametric.Rmd",package="shinyTC.CohortProgression"),tmp,overwrite=TRUE);
        #set up params list to pass to Rmd document
        params<-list(
          configInfo = isolate(configInfo()),
          nmResults  = isolate(nmResults()),
          mltResults = isolate(mltResults()),
          grwResults = isolate(grwResults()),
          m2mResults = isolate(m2mResults()),
          recResults = isolate(recResults()),
          cpResults  = isolate(cpResults()),
          eqzResults = list(eqzInfo=isolate(eqzInfo()),
                            pltEqZA=isolate(plotEqZA()),
                            pltEqZN=isolate(plotEqZN()))
        );

        #render file
        pdf_styles    <-system.file("rmd/StylesForRmdPDFs.sty",package="wtsUtilities")
        cat(file=stderr(),"--eqzServer: pdf-styles = '",pdf_styles,"'\n");
        output_options<-list(includes=list(in_header=pdf_styles));
        res <- rmarkdown::render(tempRep,
                          output_file=file,
                          output_options=output_options,
                          params=params,
                          envir=new.env(parent=globalenv()),
                          clean=TRUE);

        cat(file=stderr(),"--eqzServer: Created pdf '",file,"'\n")
        return(res);
      }
    );

    #create list of ggplot objects for equilibrium size distribution
    plotEqZA<-reactive(
      {
        cat(file=stderr(),"Starting eqzServer:plotEqZA()\n");
        p<-plotPop.EquilibriumSizeDist(
          eqzInfo()$mdfrN_msz,
          plotType=input$plotType,
          scales=ifelse(input$yScaleFreeA,"free_y","fixed"),
          showPlot=FALSE,
          verbose=TRUE
        );
        for (i in 1:length(p)) {
          names(p)[i]<-"\n  \nFigure &&figno. Absolute scale size comps showing equilibrium size distribution.  \n  \n"
        }
        cat(file=stderr(),"Finished eqzServer:plotEqZA()\n");
        return(p);
      }
    )

    #create list of ggplot objects for equilibrium size distribution
    plotEqZN<-reactive(
      {
        cat(file=stderr(),"Starting eqzServer:plotEqZN()\n");
        p<-plotPop.EquilibriumSizeDist(
          eqzInfo()$mdfrP_msz,
          plotType=input$plotType,
          scales=ifelse(input$yScaleFreeN,"free_y","fixed"),
          showPlot=FALSE,
          verbose=TRUE
        );
        for (i in 1:length(p)) {
          p[[i]] <- p[[i]]+ggplot2::ylab("normalized abundance");
          names(p)[i]<-"\n  \nFigure &&figno. Normalized size comps showing equilibrium size distribution.  \n  \n"
        }
        cat(file=stderr(),"Finished eqzServer:plotEqZN()\n");
        return(p);
      }
    )

    #render equlibrium size distribution plot
    output$pltEqZA<-renderPlot({
        cat(file=stderr(),"Starting eqzServer:renderPlot(pltEqZA)\n");
        p<-plotEqZA();
        cat(file=stderr(),"Finished eqzServer:renderPlot(pltEqZA)\n");
        return(p);
    });

    #render normalized equlibrium size distribution plot
    output$pltEqZN<-renderPlot({
        cat(file=stderr(),"Starting eqzServer:renderPlot(pltEqZN)\n");
        p<-plotEqZN();
        cat(file=stderr(),"Finished eqzServer:renderPlot(pltEqZN)\n");
        return(p);
    });

    obj<-reactive(list(eqzInfo=eqzInfo(),
                       plotEqZA=plotEqZA(),
                       plotEqZN=plotEqZN()))
    return(obj); #here if returning an R object to the enclosing server
  } #function(input,output,session)
