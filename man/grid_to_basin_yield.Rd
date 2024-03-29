% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/grid_to_basin_yield.R
\name{grid_to_basin_yield}
\alias{grid_to_basin_yield}
\title{grid_to_basin_yield}
\usage{
grid_to_basin_yield(
  carbon = NULL,
  weight_floor_ha = 1,
  emulator_dir = NULL,
  input_dir = NULL,
  area_dir = NULL,
  basin_grid = NULL,
  basin_id = NULL,
  region_id = NULL,
  write_dir = "outputs_grid_to_basin_yield",
  wheat_area = NULL,
  crops = c("maize", "rice", "soy", "wheat"),
  esm_name = "WRF",
  cm_name = "LPJmL",
  scn_name = "rcp8p5_hot",
  N = 200
)
}
\arguments{
\item{carbon}{Default = NULL}

\item{weight_floor_ha}{Default = 1}

\item{emulator_dir}{Default = NULL}

\item{input_dir}{Default = NULL}

\item{area_dir}{Default = NULL}

\item{basin_grid}{Default = NULL}

\item{basin_id}{Default = NULL}

\item{region_id}{Default = NULL. Filters to specified GCAM region (1-32), otherwise no filter}

\item{write_dir}{Default = "outputs_grid_to_basin_yield". Output Folder}

\item{wheat_area}{Default = NULL Spring and winter wheat areas}

\item{crops}{Default = c("maize", "rice", "soy", "wheat")}

\item{esm_name}{Default = 'WRF'}

\item{cm_name}{Default = 'LPJmL'}

\item{scn_name}{Default = 'rcp8p5_hot'}

\item{N}{Default = 200. Assuming nothing is nitrogen limited and apply across grids}
}
\value{
number
}
\description{
Function that uses emulators to create gridded yield files then aggregates to
gcam basin level via MIRCA harvested areas.
}
\examples{
\dontrun{
library(osiris)
osiris::grid_to_basin_yield()
}
}
\keyword{test}
