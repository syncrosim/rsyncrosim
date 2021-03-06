% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/datasheetRaster.R
\name{datasheetRaster}
\alias{datasheetRaster}
\alias{datasheetRaster,character-method}
\alias{datasheetRaster,list-method}
\alias{datasheetRaster,SsimObject-method}
\alias{datasheetRaster,Scenario-method}
\title{Get spatial inputs or outputs from a Scenario(s).}
\usage{
datasheetRaster(
  ssimObject,
  datasheet,
  column = NULL,
  scenario = NULL,
  iteration = NULL,
  timestep = NULL,
  subset = NULL,
  forceElements = FALSE
)

\S4method{datasheetRaster}{character}(
  ssimObject,
  datasheet,
  column = NULL,
  scenario = NULL,
  iteration = NULL,
  timestep = NULL,
  subset = NULL,
  forceElements = FALSE
)

\S4method{datasheetRaster}{list}(
  ssimObject,
  datasheet,
  column = NULL,
  scenario = NULL,
  iteration = NULL,
  timestep = NULL,
  subset = NULL,
  forceElements = FALSE
)

\S4method{datasheetRaster}{SsimObject}(
  ssimObject,
  datasheet,
  column = NULL,
  scenario = NULL,
  iteration = NULL,
  timestep = NULL,
  subset = NULL,
  forceElements = FALSE
)

\S4method{datasheetRaster}{Scenario}(
  ssimObject,
  datasheet,
  column = NULL,
  scenario = NULL,
  iteration = NULL,
  timestep = NULL,
  subset = NULL,
  forceElements = FALSE
)
}
\arguments{
\item{ssimObject}{SsimLibrary/Project/Scenario or list of Scenarios. If SsimLibrary/Project, then scenario argument is required.}

\item{datasheet}{character string. The name of the datasheet containing the raster data.}

\item{column}{character string. The name of the column in the datasheet containing the filenames for raster data. If NULL then use the first column that contains raster filenames.}

\item{scenario}{character string, integer, or vector of these. The scenarios to include. Required if ssimObject is an SsimLibrary/Project, ignored if ssimObject is a list of Scenarios.}

\item{iteration}{integer, character string, or vector of integer/character strings. Iteration(s) to include. If NULL then all iterations are included. If no Iteration column in the datasheet, then ignored.}

\item{timestep}{integer, character string, or vector of integer/character string. Timestep(s) to include. If NULL then all timesteps are included.  If no Timestep column in the datasheet, then ignored.}

\item{subset}{logical expression indicating datasheet rows to return. e.g. expression(grepl("Ts0001",Filename,fixed=T)). See subset() for details.}

\item{forceElements}{logical. If TRUE then returns a single raster as a RasterStack; otherwise returns a single raster as a RasterLayer directly.}
}
\value{
A RasterLayer, RasterStack or RasterBrick object. See raster package documentation for details.
}
\description{
Get spatial inputs or outputs from one or more SyncroSim \code{\link{Scenario}}.
}
\details{
The names of the returned raster stack contain metadata.
For datasheets without Filename this is: paste0(<datasheet name>,".Scn",<scenario id>,".",<tif name>)
For datasheets containing Filename this is: paste0(<datasheet name>,".Scn",<scenario id>,".It",<iteration>,".Ts",<timestep>)
}
\examples{
\dontrun{
## Not run as it would require a result scenario (long runtime)
datasheetRaster(myResult,
  datasheet = "OutputSpatialState",
  subset = expression(grepl("Ts0001", Filename, fixed = TRUE))
)
}

}
