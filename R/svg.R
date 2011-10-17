
svgOpen <- function(filename="Rplots.svg", width=200, height=200) {
  # For viewing using Adobe SVG Viewer in IE
  # OR in Firefox 3 (native support)
  # create a "wrapper" html file
  htmlfile <- file(paste(filename, ".html", sep=""), "w")
  cat(paste('<object data="', filename, '" type="image/svg+xml"',
            ' width="', ceiling(width), 'px" height="', ceiling(height), 'px"> </object>\n',
            sep=''), file=htmlfile)
  close(htmlfile)
  svgdev <- svgDevice(file(filename, 'w'), width, height)
  svgHeader(width, height, svgdev)
  return(svgdev)
}

svgClose <- function(svgdev) {
  svgFooter(svgdev)
  close(svgDevFile(svgdev))
}

svgClipPath <- function(id, vpx, vpy, vpw,
                        vph, svgdev=svgDevice()) {
  clipPathID <- paste(id, "clipPath", sep=".")
  catsvg('<defs>\n', svgdev)
  incindent(svgdev)
  catsvg(paste('<clipPath id="', clipPathID,
               '">\n', sep=""),
         svgdev)
  incindent(svgdev)
  catsvg(paste('<rect x="', vpx, '" ',
               'y="', vpy, '" ',
               'width="', vpw, '" ',
               'height="', vph, '" ',
               'style="fill: none; stroke: none;"',
               ' />\n', sep=""),
         svgdev)
  decindent(svgdev)
  catsvg('</clipPath>\n', svgdev)
  decindent(svgdev)
  catsvg('</defs>\n', svgdev)
  decindent(svgdev)
}

svgClipAttr <- function(id, clip) {
  if (clip)
    paste('clip-path="url(#', id, '.clipPath)" ', sep="")
  else
    ""
}

svgStartGroup <- function(id=NULL, clip=FALSE,
                          attributes=svgAttrib(),
                          style=svgStyle(), svgdev=svgDevice()) {
  incindent(svgdev)
  catsvg(paste('<g ',
               'id="', getid(id, svgdev), '" ',
               svgAttribTxt(attributes, id), ' ',
               svgClipAttr(id, clip),
               svgStyleAttributes(style), 
               '>\n',
               sep=""), svgdev)
  incID(svgdev)
}

svgEndGroup <- function(svgdev=svgDevice()) {
  catsvg('</g>\n', svgdev)
  decindent(svgdev)
}

svgStartLink <- function(href="", svgdev=svgDevice()) {
  incindent(svgdev)
  catsvg(paste('<a xlink:href="', href, '">\n',
               sep=""), svgdev)  
}

svgEndLink <- function(svgdev=svgDevice()) {
  catsvg('</a>\n', svgdev)
  decindent(svgdev)
}

svgAnimate <- function(attrib, values,
                       begin, interp, duration, rep, revert, id=NULL, 
                       svgdev=svgDevice()) {
  n <- if (is.null(id)) 1 else length(unique(id))
  catsvg(paste('<animate ',
               'xlink:href="#', getid(id, svgdev, n), '" ',
               'attributeName="', attrib, '" ',
               'begin="', begin, 's" ',
               'calcMode="', interp, '" ',
               'dur="', duration, 's" ',
               'values="', values, '" ',
               'repeatCount="',
               if (is.numeric(rep)) rep else if (rep) "indefinite" else 1,
               '" ',
               'fill="',
               if (revert) "remove" else "freeze",
               '" ',               
               '/>\n', sep=""),
         svgdev)
}

# This and svgAnimateY are untested with id != NULL
# and I have a strong suspicion there may be problems
# because tapply returns a list -- see svgAnimatePoints
# for ideas for a possible solution (esp. the lpaste function)
svgAnimateXYWH <- function(attrib, values,
                           begin, interp, duration, rep, revert,
                           id=NULL,
                           svgdev=svgDevice()) {
  svgAnimate(attrib,
             paste(round(values, 2), collapse=";"),
             begin, interp, duration, rep, revert, id, svgdev)  
}

# DON'T call this with a list of length < 2!
old.lpaste <- function(alist, collapse) {
  n <- length(alist)
  if (n == 2)
    result <- paste(alist[[1]], alist[[2]])
  else 
    result <- paste(alist[[n]], lpaste(alist[1:(n-1)], collapse))
  paste(result, collapse=collapse)
}

lpaste <- function(alist, collapse) {
  n <- length(alist)
  result <- alist[[1]]
  for (i in 2:n)
    result <- paste(result, alist[[i]])
  paste(result, collapse=collapse)
}

svgAnimatePoints <- function(xvalues, yvalues, pointsid,
                             begin, interp, duration, rep, revert,
                             id=NULL,
                             svgdev=svgDevice()) {
  if (is.null(id))
    warning("Only one point to animate")
  else
    svgAnimate("points",
                paste(lapply(split(paste(round(xvalues, 2),
                                         round(yvalues, 2), sep=","),
                                   pointsid),
                             paste, collapse=" "),
                      collapse=";"),
               begin, interp, duration, rep, revert, id, svgdev)  
}

svgAnimateTransform <- function(attrib, values,
                                begin, interp, duration, rep, revert,
                                id=NULL,
                                svgdev=svgDevice()) {
  n <- if (is.null(id)) 1 else length(unique(id))
  catsvg(paste('<animateTransform ',
               'xlink:href="#', getid(id, svgdev, n), '" ',
               'attributeName="transform" ',
               'type="', attrib, '" ',
               'begin="', begin, 's" ',
               'calcMode="', interp, '" ',
               'dur="', duration, 's" ',
               'values="', values, '" ',
               'repeatCount="',
               if (is.numeric(rep)) rep else if (rep) "indefinite" else 1,
               '" ',
               'fill="',
               if (revert) "remove" else "freeze",
               '" ',
               '/>\n', sep=""),
         svgdev)
}

svgAnimateTranslation <- function(xvalues, yvalues,
                                  begin, interp, duration, rep, revert,
                                  id=NULL,
                                  svgdev=svgDevice()) {
  svgAnimateTransform("translate",
                      paste(round(xvalues, 2),
                            round(yvalues, 2),
                            sep=",", collapse=';'),
                      begin, interp, duration, rep, revert, id, svgdev)  
}

svgLines <- function(x, y, id=NULL, arrow = NULL,
                     attributes=svgAttrib(),
                     style=svgStyle(), svgdev=svgDevice()) {

  # Grabbing arrow info for marker element references
  if (! is.null(arrow$ends))
      lineMarkerTxt <- markerTxt(arrow$ends, id)
  else
      lineMarkerTxt <- ""

  # Never fill a line
  style$fill <- "none"

  catsvg(paste('<polyline ',
               'id="', id, '" ',
               'points="',
               paste(round(x, 2), ",",
                     round(y, 2), sep="",
                     collapse=" "),
               '" ',
               lineMarkerTxt,
               svgAttribTxt(attributes, id), ' ',
               svgStyleAttributes(style), 
               ' />\n', sep=""),
         svgdev)  
}

svgMarker <- function(x, y, type, ends, name,
                      style=svgStyle(), svgdev=svgDevice()) {
    width <- max(x)
    height <- max(y)
    if (length(x) != length(y))
        stop("x and y must be same length")
    if (is.atomic(x)) {
        if (is.atomic(y)) {
            x <- list(x)
            y <- list(y)
        } else {
            stop("'x' and 'y' must both be lists or both be atomic")
        }
    }

    d <- mapply(
                function(subx, suby) {
                    openPath <- paste(c("M",
                                      rep("L", length(subx) - 1)),
                                      round(subx, 2), round(suby, 2),
                                      collapse=" ")
                    if (type == 2) # Closed arrow
                      paste(openPath, "Z")
                    else
                      openPath
                }, x, y)

    # If the arrow is open, we don't want to fill it
    if (type == 1)
        style$fill <- "none"

    markerDefs <- paste('<marker ',
                        'id="', markerName("both", name), '" ',
                        'refX="', round(c(-width, width), 2),
                        '" refY="', round(c(-height / 2, height / 2), 2), '" ',
                        'overflow="visible" ',
                        'markerUnits="userSpaceOnUse" ',
                        'markerWidth="', round(width, 2), '" ',
                        'markerHeight="', round(height, 2), '" ',
                        'orient="auto">\n',
                        '<path ',
                        'd="', d, '" ',
                        c('transform="rotate(180)" ', ''),
                        svgStyleAttributes(style), 
                        ' />\n',
                        '</marker>\n', sep="", collapse="")
    catsvg(paste('<defs>\n',
                 markerDefs,
                 '</defs>\n', sep=""),
           svgdev)
}

markerTxt <- function(ends, name) {
    mname <- markerName(ends, name)
    if (ends == "first")
        lmt <- paste('marker-start="url(#', mname, ')" ', sep="")
    if (ends == "last")
        lmt <- paste('marker-end="url(#', mname, ')" ', sep="")
    if (ends == "both")
        lmt <- paste(paste('marker-start="url(#', mname[1], ')" ', sep=""),
                     paste('marker-end="url(#', mname[2], ')" ', sep=""),
                     sep="")
    lmt
}

markerName <- function(ends, name) {
    if (ends == "first")
        mname <- paste(name, ".markerStart", sep="")
    if (ends == "last")
        mname <- paste(name, ".markerEnd", sep="")
    if (ends == "both")
        mname <- c(paste(name, ".markerStart", sep=""),
                   paste(name, ".markerEnd", sep=""))
    mname
}

svgPolygon <- function(x, y, id=NULL,
                       attributes=svgAttrib(),
                       style=svgStyle(), svgdev=svgDevice()) {
  if (length(x) != length(y))
    stop("x and y must be same length")

  catsvg(paste('<polygon ',
               'id="', id, '" ',
               'points="', 
               paste(round(x, 2), ",",
                     round(y, 2), sep="",
                     collapse=" "),
               '" ', 
               svgAttribTxt(attributes, id), ' ',
               svgStyleAttributes(style), 
               ' />\n', sep=""),
         svgdev)  
}

# Differs from polygon because it can have sub-paths
svgPath <- function(x, y, rule, id=NULL,
                    attributes=svgAttrib(),
                    style=svgStyle(), svgdev=svgDevice()) {
    if (length(x) != length(y))
        stop("x and y must be same length")
    if (is.atomic(x)) {
        if (is.atomic(y)) {
            x <- list(x)
            y <- list(y)
        } else {
            stop("'x' and 'y' must both be lists or both be atomic")
        }
    }
    n <- length(x)
    d <- mapply(
                function(subx, suby) {
                    paste(paste(c("M",
                                  rep("L", length(subx) - 1)),
                                round(subx, 2), round(suby, 2),
                                collapse=" "),
                          "Z")
                }, x, y)

    catsvg(paste('<path ',
                 'id="', id, '" ',
                 'd="', paste(unlist(d), collapse=" "), '" ', 
                 'fill-rule="',
                 switch(rule, winding="nonzero", "evenodd"), '" ',
                 svgAttribTxt(attributes, id), ' ',
                 svgStyleAttributes(style), 
                 ' />\n', sep=""),
           svgdev)  
}

svgRaster <- function(x, y, width, height, id=NULL,
                      just, vjust, hjust,
                      attributes=svgAttrib(), 
                      style=svgStyle(), svgdev=svgDevice()) {

  # Need to extract the original grob name in order to link to the image
  grobname <- baseGrobName(id)
  fileloc <- paste(grobname, ".png", sep = "")

  rasters <- paste('<image ',
                   'id="', id, '" ',
                   'x="', round(x, 2), '" ',
                   'y="', round(y, 2), '" ',
                   'width="', round(width, 2), '" ',
                   'height="', round(height, 2), '" ',
                   'xlink:href="', fileloc, '" ',
                   # Flipping image vertically to correct orientation
                   'transform="translate(0, ',  round(height + (2 * y), 2), ') scale(1, -1)" ',
                   svgAttribTxt(attributes, id), ' ',
                   svgStyleAttributes(style),
                   ' />\n',
                   sep="")

  catsvg(rasters, svgdev)
}

svgRect <- function(x, y, width, height, id=NULL,
                    attributes=svgAttrib(), 
                    style=svgStyle(), svgdev=svgDevice()) {
  rects <- paste('<rect ',
                 'id="', id, '" ',
                 'x="', round(x, 2), '" ',
                 'y="', round(y, 2), '" ',
                 'width="', round(width, 2), '" ',
                 'height="', round(height, 2), '" ',
                 svgAttribTxt(attributes, id), ' ',
                 svgStyleAttributes(style),
                 ' />\n',
                 sep="")
  catsvg(rects, svgdev)
}

svgText <- function(x, y, text, hjust="left", vjust="bottom", rot=0,
                    lineheight=1, charheight=.8,
                    id=NULL, attributes=svgAttrib(), 
                    style=svgStyle(), svgdev=svgDevice()) {
    # Avoid XML specials in text
    text <-
        gsub("<", "&lt;",
             gsub(">", "&gt;",
                  gsub("'", "&apos;",
                       gsub("\"", "&quot;",
                            # DO & FIRST !!!!
                            gsub("&", "&amp;",
                                      text)))))

    # Flip the y-direction again so that text is drawn "upright"
    # Do the flip in a separate <g> so that can animate the
    # translation easily
    # Use a tspan to do the vertical alignment
    texts <- paste('<g ',
                   'id="', id, '" ',
                   # Attributes applied to group
                   svgAttribTxt(attributes, id), ' ',
                   # Only draw a REALLY thin line for the text outline
                   'stroke-width=".1" ',
                   'transform="translate(',
                   round(x, 2), ', ',
                   round(y, 2), ') ',
                   '">\n',
                   '<g transform="scale(1, -1)">\n',
                   '<text x="0" y="0" ',
                   if (rot != 0) {
                       paste('transform="rotate(',
                             # Rotation in SVG goes clockwise from +ve x=axis
                             round(-rot, 2),
                             ')" ', sep="")
                   } else "",
                   textAnchor(hjust), ' ',
                   svgStyleAttributes(style),
                   ' >\n',
                   svgTextSplitLines(text, lineheight, charheight, vjust),
                   '</text>\n',
                   '</g>\n',
                   '</g>\n',
                   sep="")

    catsvg(texts, svgdev)
}

svgTextSplitLines <- function(text, lineheight, charheight, vjust) {
    # Splitting based on linebreaks
    splitText <- strsplit(text, "\n")

    svgText <- list()
    n <- length(splitText[[1]])

    # Need to adjust positioning based on vertical justification.
    # Horizontal justification is done for us.
    # Only the first line needs to be modified, the rest are all
    # just one line below the previous line
    if (vjust %in% c("centre", "center"))
        firstDelta <- - ((lineheight * (n - 1) - charheight) / 2)
    if (vjust == "bottom")
        firstDelta <- - (n - 1) * lineheight
    if (vjust == "top")
        firstDelta <- charheight
    lineheight <- c(firstDelta, rep(lineheight, n - 1))

    svgText[[1]] <- paste('<tspan dy="',
                          round(lineheight, 2),
                          '" ',
                          'x="0"', # Needs to be pushed to the left
                          '>',
                          splitText[[1]],
                          '</tspan>',
                          sep="", collapse="\n")

    svgText <- paste(unlist(svgText), collapse="\n")

    svgText
}

svgCircle <- function(x, y, r, id=NULL,
                      attributes=svgAttrib(), 
                      style=svgStyle(), svgdev=svgDevice()) {

  circles <- paste('<circle ',
                   'id="', id, '" ',
                   'cx="', round(x, 2), '" ',
                   'cy="', round(y, 2), '" ',
                   'r="', round(r, 2), '" ',
                   svgAttribTxt(attributes, id), ' ',
                   svgStyleAttributes(style),
                   ' />\n',
                   sep="")

  catsvg(circles, svgdev)
}

svgScript <- function(body, href, type="text/ecmascript",
                      id=NULL, svgdev=svgDevice()) {
  script <- paste('<script type="', type, '" ',
                  'id="', getid(id, svgdev, 1), '" ',
                  if (nchar(href) > 0) 
                      paste('xlink:href="', href, '" ' ,sep="")
                  else
                      '',
                  '>\n',
                  if (nchar(body) > 0)
                      paste('<![CDATA[\n',
                            body, '\n',
                            '  ]]>\n',
                            sep="")
                  else
                      '',
                  '</script>\n',sep="");
  catsvg(script, svgdev)
}

#############
# Internal functions
#############

# SVG Devices
# A device is an environment so that we can modify values
# stored within it.
# Store a list of transformation functions for
# x, y, width, and height;  this will allow viewports
# to be defined within user coordinates (see svgPushViewport
# and svgPopViewport)

svgDevice <- function(file="", width=200, height=200) {
  dev <- new.env(FALSE, emptyenv())
  assign("file", file, env=dev)
  assign("width", width, env=dev)
  assign("height", height, env=dev)
  assign("indent", "", env=dev)
  assign("id", 1, env=dev)
  return(dev)
}

svgDevFile <- function(svgdev) {
  get("file", env=svgdev)
}

svgDevWidth <- function(svgdev) {
  get("width", env=svgdev)
}

svgDevHeight <- function(svgdev) {
  get("height", env=svgdev)
}

getid <- function(id, svgdev, n=1) {
  if (is.null(id))
    svgID(svgdev) + (1:n - 1)
  else {
    if (n > 1)
      paste(id, 1:n, sep="")
    else
      id
  }
}

svgID <- function(svgdev) {
  get("id", env=svgdev)
}

# SVG output
catsvg <- function(text, svgdev) {
  cat(paste(get("indent", env=svgdev), text, sep=""),
      file=svgDevFile(svgdev))
}

decindent <- function(svgdev) {
  indent <- get("indent", env=svgdev)
  assign("indent", substr(indent, 1, nchar(indent) - 2),
         env=svgdev)
}

incindent <- function(svgdev) {
  assign("indent", paste(get("indent", env=svgdev), "  ", sep=""),
         env=svgdev)
}

incID <- function(svgdev, n=1) {
  assign("id", get("id", env=svgdev) + n, env=svgdev)
}

svgHeader <- function(width, height, svgdev=svgDevice()) {
    # This header tested on standalone SVG file in Firefox 3
    # FIXME:  add default xmlns for animation and scripts too?
    catsvg(paste(paste('<?xml version="1.0" encoding="',
                       localeToCharset()[1],
                       '"?>', sep=""),
                 '<svg xmlns="http://www.w3.org/2000/svg"',
                 '     xmlns:xlink="http://www.w3.org/1999/xlink"',
                 paste('     width="', width, 'px"', sep=""),
                 paste('     height="', height, 'px"', sep=""),
                 '     version="1.0">',
                 sep="\n"), svgdev)
    # Invert the y-axis so that y and height values measure "up"
    catsvg(paste('<g transform="translate(0, ',
                 svgDevHeight(svgdev), ') ',
                 ' scale(1, -1)">\n',
                 sep=""), svgdev)
}

svgFooter <- function(svgdev=svgDevice()) {
  catsvg('</g>\n', svgdev);
  catsvg('</svg>\n', svgdev);
}

# SVG attributes
svgAttrib <- function(...) {
  temp <- list(...)
  if (length(temp) == 0)
    list()
  else if (is.null(temp[[1]]))
    list()
  else
    temp
}

listToSVGAttrib <- function(alist) {
  alist
}

emptyAttrib <- function(attributes) {
  length(attributes) == 0
}

# Only use the attributes that are for this 'id'
svgAttribTxt <- function(attributes, id) {
    if (emptyAttrib(attributes)) {
        ""
    } else {
        attributes <- lapply(attributes,
                             function(attr, id) {
                                 kept <- attr[names(attr) == id]
                                 if (length(kept) == 0)
                                     NULL
                                 else
                                     kept
                             },
                             id)
        # Drop NULL attributes
        attributes <- attributes[!sapply(attributes, is.null)]
        if (length(attributes) > 0)
            paste(names(attributes), '="', attributes, '"', sep="",
                  collapse=" ")
        else
            ""
    }
}

# SVG styling
svgStyle <- function(...) {
  list(...)
}

listToSVGStyle <- function(alist) {
  alist
}

emptyStyle <- function(svgstyle) {
  length(svgstyle) == 0
}

svgStyleCSS <- function(svgstyle) {
  if (emptyStyle(svgstyle)) {
    ""
  } else {
      paste('style="',
            do.call("paste",
                    c(mapply(function(name, style) {
                        paste(name, ":", style, sep="")
                    }, names(svgstyle), svgstyle),
                      list(sep="; "))),
            '"', sep="")
    # paste('style="', paste(names(svgstyle), ":",
    #                        paste(svgstyle), sep="", collapse="; "),
    #       '"', sep="")
  }
}

# NOTE using SVG presentation attributes
# RATHER THAN CSS style attribute
# BECAUSE ...
# - can modify single presentation attribute without affecting
#   other presentation attributes (if CSS style then have to
#   reset the entire thing) and can do this from JavaScript.
# - presentation attributes have lower priority than CSS style
#   SO this allows overriding by specifying CSS style later.
#   Can also override with general style sheet later.
svgStyleAttributes <- function(svgstyle) {
    if (emptyStyle(svgstyle)) {
        ""
    } else {
        if (any(sapply(svgstyle, length) > 1))
            stop("All SVG style attribute values must have length 1")
        paste(names(svgstyle), "=\"", svgstyle, "\"", sep="", collapse=" ")
    }    
}

# Specifying text justification
textAnchor <- function(hjust) {
  paste("text-anchor=",
        switch(hjust,
               left='"start"',
               center='"middle"',
               centre='"middle"',
               right='"end"',
               '"start"'),
        sep="")
}

dominantBaseline <- function(vjust) {
  paste("dominant-baseline=",
        switch(vjust,
               bottom='"auto"',
               center='"middle"',
               centre='"middle"',
               top='"text-top"',
               '"baseline"'),
        sep="")
}

baselineShift <- function(vjust) {
  paste('baseline-shift=',
        switch(vjust,
               bottom='"0%"',
               center='"-50%"',
               centre='"-50%"',
               top='"-100%"',
               '"0%"'),
        sep="")
}

alignmentBaseline <- function(vjust) {
  paste("alignment-baseline=",
        switch(vjust,
               baseline='"baseline"',
               bottom='"bottom"',
               center='"middle"',
               centre='"middle"',
               top='"top"',
               '"baseline"'),
        sep="")
}

