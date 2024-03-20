% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_AgProdChange_xml.R
\name{create_AgProdChange_xml}
\alias{create_AgProdChange_xml}
\title{create_AgProdChange_xml}
\usage{
create_AgProdChange_xml(
  write_dir = "outputs_create_AgProdChange_xml",
  esm_name = "WRF",
  scn_name = "rcp8p5_hot",
  ssp = "ssp3",
  ag_irr_ref = NULL,
  bio_irr_ref = NULL,
  ag_impacts = NULL,
  bio_impacts = NULL,
  GCAM_region_mapping = NULL,
  timestep = 5,
  maxHistYear = 2010,
  minFutYear = 2015,
  appliedto = "full"
)
}
\arguments{
\item{write_dir}{Default = "outputs_create_AgProdChange_xml". Output Folder}

\item{esm_name}{Default = 'WRF'}

\item{scn_name}{Default = 'rcp8p5_hot'}

\item{ssp}{Default = 'ssp3'}

\item{ag_irr_ref}{Default = NULL}

\item{bio_irr_ref}{Default = NULL}

\item{ag_impacts}{Default = NULL}

\item{bio_impacts}{Default = NULL}

\item{GCAM_region_mapping}{Default = NULL}

\item{timestep}{Default = 5}

\item{maxHistYear}{Default = 2010 Historical year}

\item{minFutYear}{Default = 2015 Min future year}

\item{appliedto}{Default = "full". This applies impacts to all regions. Other
options are "domestic" and "international".}
}
\value{
number
}
\description{
Function that takes the defined impacts and reference AgProdChange files then
updates and saves the new AgProdChange csv and xml files.
}
\examples{
\dontrun{
library(osiris)
osiris::create_AgProdChange_xml()
}
}
\keyword{test}