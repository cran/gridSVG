
hyperlinkGrob <- function(x, href) {
  x$href <- href
  class(x) <- c("linked.grob", class(x))
  x
}

grid.hyperlink <- function(path, href) {
  x <- grid.get(path)
  x <- hyperlinkGrob(x, href)
  grid.set(path, x, redraw=FALSE)
}

# gridToDev method for linked.grob objects
grobToDev.linked.grob <- function(x, dev) {
  svgStartLink(x$href, dev@dev)
  NextMethod()
  svgEndLink(dev@dev)
}
