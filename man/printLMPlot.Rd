% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lmPlot.r
\name{printLMPlot}
\alias{printLMPlot}
\title{Print lmplot Object to a File}
\usage{
printLMPlot(plobject, name = NULL, res = 300)
}
\arguments{
\item{plobject}{name of the lmplot object.}

\item{name}{name of the file to be produced. Tiff, png, jpeg, and pdf are
recognized.}

\item{res}{resolution in dpi.}
}
\value{
Returns nothing.
}
\description{
Takes a stored lmplot object and uses the print method for objects of class
mm to print to a file whose type is implied by the file name.
}
\author{
Quinn Payton \email{Payton.Quinn@epa.gov}
}
