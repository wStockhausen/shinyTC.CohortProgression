% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/configServer.R
\name{configServer}
\alias{configServer}
\title{Server function for the Tanner crab model configuration Shiny module}
\usage{
configServer(input, output, session)
}
\arguments{
\item{input}{- usual Shiny input variable}

\item{output}{- usual Shiny output variable}

\item{session}{- usual Shiny session variable}
}
\value{
configInfo - reactive variable yielding list of model configuration info
}
\description{
Server function for the Tanner crab model configuration Shiny module.
}
\details{
Creates and returns a reactive variable (\code{\link{configInfo}}) using
the function \code{\link{setConfigInfo}} that reflects the
current model configuration state, which is a list with elements:<cr>
<ul>
 <li> moltTime - timing of molting/growth as fraction of year </li>
 <li> maturityStates - maturity states </li>
 <li> shellConditions - shell conditions </li>
 <li> binZ - size bin width (mm CW) </li>
 <li> minZ - min size cutpoint (mm CW) </li>
 <li> maxZ - max size cutpoint (mm CW) </li>
 <li> zBs - size bins (mm CW) </li>
 <li> Zcs - size bin cutpoints (mm CW) </li>
</ul>
}
