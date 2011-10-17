\name{gridToSVG}
\alias{gridToSVG}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Generate SVG output from a grid graphic }
\description{
  Produces an SVG version of the current grid page.
}
\usage{
gridToSVG(name = "Rplots.svg")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{name}{ The name of the SVG file to produce }
}
\details{
  See the files in the directory gridSVG/tests for examples
  of things that can be done.  See the file gridSVG/doc/overview.tex
  for limitations.
}
\value{
  None.  The side effect is to produce an SVG file of the specified
  name, AND a very rudimentary HTML file with the SVG document embedded.
}
\author{ Paul Murrell }
\seealso{ \code{\link{grid.hyperlink}}, \code{\link{grid.animate}} }
\keyword{ dplot }% at least one, from doc/KEYWORDS
