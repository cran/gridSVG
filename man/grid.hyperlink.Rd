\name{grid.hyperlink}
\alias{grid.hyperlink}
\alias{hyperlinkGrob}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Associate a hyperlink with a grid grob }
\description{
  Creates a linked.grob object which is a normal grid grob,
  with a hyperlink attached.  Useful in conjunction with
  \code{gridToSVG}, to produce an SVG document with
  hyperlinked graphical elements.
}
\synopsis{
hyperlinkGrob(x, href)
grid.hyperlink(path, href)
  }
\usage{
grid.hyperlink(path, href)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{path}{ A grob path specifying a drawn grob. }
  \item{href}{ A valid Xlink URI }
}
\value{
  A linked.grob object.
}
\author{ Paul Murrell }
\seealso{ \code{\link{gridToSVG}} }                           
\keyword{ dplot }% at least one, from doc/KEYWORDS

