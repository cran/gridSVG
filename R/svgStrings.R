
## Generate SVG using string tools then parse to XML
## [instead of using newXMLNode(..., parent=)]

## For some svg*() functions in svg.R, there are svg*String() functions here
## Difference between svg*() and svg*String() is that the
## latter is vectorised

## For some *() functions in svg.R, there are *Vec() (vectorised) functions here

# Removes NULL values and flattens our attrib list
# so we can include lists as elements in "alist"
# and arrive at a flattened list
# ALSO removes names from attr values so they don't corrupt attr names
# e.g., avoid list(a=c(b=1)) becoming c(a.b=1)
attrListVec <- function(alist) {
    lapply(alist, unname)
}

## 'rx' and 'ry' because x and y assumed to be already rounded
## Assume that rx, ry, and angle all same length
svgAngleTransformVec <- function(rx, ry, angle) {
    trans <- rep("", length(rx))
    sigAngle <- !(is.null(angle) | angle == 0 | is.na(angle))
    if (any(sigAngle)) {
        trans[sigAngle] <- paste0("rotate(", round(angle[sigAngle], 2),
                                  " ", rx[sigAngle],
                                  " ", ry[sigAngle], ")")
    }
    trans
}

svgStyleAttributesVec <- function(svgstyle, dev) {
    if (emptyStyle(svgstyle)) {
        list()
    } else {
        # Remove non-SVG style attributes (with warning)
        names <- names(svgstyle)
        svgnames <- names %in% get("SVGParList", envir=.gridSVGEnv)
        if (svgStrict(dev) && any(!svgnames)) {
            warning(paste("Removing non-SVG style attribute name(s):",
                          paste(names[!svgnames], collapse=", ")))
            svgstyle <- svgstyle[names[svgnames]]
        }
        svgstyle
    }
}

svgStartLinkString <- function(href="", show="", svgdev=svgDevice()) {
    href <- paste0('xlink:href="', href, '" ')
    show <- paste0('xlink:show="', show, '" ')
    noshow <- is.null(show) | is.na(show) | nchar(show) == 0
    if (any(noshow)) {
        show[noshow] <- ""
    }
    paste0("<a ", href, show, ">")
}

svgEndLinkString <- function(href) {
    rep("</a>", length(href))
}

svgCircleString <- function(x, y, r, id=NULL,
                            attributes=svgAttrib(), links=NULL, show=NULL,
                            style=svgStyle(), svgdev=svgDevice()) {
    # Draw nothing if non-finite location or size
    draw <- is.finite(x) & is.finite(y) & is.finite(r)

    tmpattr <- c(list(id = prefixName(id),
                      cx = round(x, 2),
                      cy = round(y, 2),
                      r = round(r, 2)),
                 svgStyleAttributesVec(style, svgdev),
                 svgAttribTxt(attributes, id, "circle", svgdev))
    attrlist <- attrListVec(tmpattr)
    attrStrings <- mapply(function(a, aname) {
                              paste0(aname, '="', a, '"')
                          },
                          attrlist, names(attrlist), SIMPLIFY=FALSE)
    circleString <- paste0("<circle ",
                           apply(matrix(unlist(attrStrings),
                                        ncol=length(attrlist)),
                                 1, paste, collapse=" "),
                           "/>")
    circleString[!draw] <- ""
    ## Excapsulate within anchor ?
    if (!is.null(links) && any(has.link <- !is.na(links))) {
        circleString[has.link] <- paste0(svgStartLinkString(links[id],
                                                            show[id]),
                                         circleString[has.link],
                                         svgEndLinkString(links[id]))
    }
    svgString <- paste0('<temp xmlns:xlink="http://www.w3.org/1999/xlink">',
                        paste(circleString, collapse=""), "</temp>")
    newNode <- xmlParse(svgString, asText=TRUE)
    addChildren(svgDevParent(svgdev), kids=xmlChildren(xmlRoot(newNode)))
}

svgRectString <- function(x, y, width, height, angle=0, id=NULL,
                          attributes=svgAttrib(), links=NULL, show=NULL,
                          style=svgStyle(), svgdev=svgDevice()) {
    ## Draw nothing if non-finite location or size
    draw <- is.finite(x) & is.finite(y) & is.finite(width) & is.finite(height)
    ## Cope with negative width or height
    negWidth <- width < 0
    negHeight <- height < 0
    x[negWidth] <- x + width # shifts x to the left
    width <- abs(width)
    y[negHeight] <- y + height # shifts y down
    height <- abs(height)
    ## Avoid stupid number of digits in SVG
    rx <- round(x, 2)
    ry <- round(y, 2)
    attrlist <- c(list(id = prefixName(id),
                       x = rx,
                       y = ry,
                       width = round(width, 2),
                       height = round(height, 2),
                       transform = svgAngleTransformVec(rx, ry, angle)), 
                  svgStyleAttributesVec(style, svgdev),
                  svgAttribTxt(attributes, id, "rect", svgdev))
    attrlist <- attrListVec(attrlist)
    attrStrings <- mapply(function(a, aname) {
                              paste0(aname, '="', a, '"')
                          },
                          attrlist, names(attrlist), SIMPLIFY=FALSE)
    rectString <- paste0("<rect ",
                         apply(matrix(unlist(attrStrings),
                                      ncol=length(attrlist)),
                               1, paste, collapse=" "),
                         "/>")
    rectString[!draw] <- ""
    ## Excapsulate within anchor ?
    if (!is.null(links) && any(has.link <- !is.na(links))) {
        rectString[has.link] <- paste0(svgStartLinkString(links[id], show[id]),
                                       rectString[has.link],
                                       svgEndLinkString(links[id]))
    }
    svgString <- paste0('<temp xmlns:xlink="http://www.w3.org/1999/xlink">',
                        paste(rectString, collapse=""), "</temp>")
    newNode <- xmlParse(svgString, asText=TRUE)
    addChildren(svgDevParent(svgdev), kids=xmlChildren(xmlRoot(newNode)))
}

svgUseSymbolString <- function(id, x, y, size, pch, angle=0,
                               attributes=svgAttrib(), links=NULL, show=NULL,
                               style=svgStyle(), svgdev=svgDevice()) {

    ## Draw nothing if non-finite location or size
    draw <- is.finite(x) & is.finite(y) & is.finite(size) &
        (is.character(pch) | is.finite(pch))
    ## Ensure the "dot" is only 1px wide
    size[pch == "."] <- 1
    ## Ensure we refer to the correct <symbol> id
    if (is.character(pch)) {
        numpch <- as.numeric(sapply(pch, charToRaw))
    } else {
        numpch <- pch
    }
    rx <- round(x, 2)
    ry <- round(y, 2)
    
    tmpattr <- list(id = prefixName(id),
                    "xlink:href" =
                        paste0("#", prefixName(paste0("gridSVG.pch", numpch))),
                    x = rx, y = ry,
                    width = round(size, 2),
                    height = round(size, 2))

    ## centering adjustment
    r <- round(-size / 2, 2)
    tmpattr$transform <- paste0("translate(", r, ",", r, ")")
    angleTransform <- svgAngleTransformVec(rx, ry, angle)
    tmpattr$transform <- paste(angleTransform, tmpattr$transform)
  
    ## Preserve order
    tmpattr <- c(tmpattr,
                 svgStyleAttributesVec(style, svgdev),
                 svgAttribTxt(attributes, id, "use", svgdev))
    
    ## Need to scale the stroke width otherwise for large points
    ## we also have large strokes
    sw <- as.numeric(tmpattr$`stroke-width`)
    scalef <- size / 10 # 10 is the point viewBox size
    sw <- sw / scalef
    tmpattr$`stroke-width` <- round(sw, 2)
    
    ## For pch outside 0-25 or character pch
    if (is.character(pch)) {
        isDot <- pch == "."
        isChar <- !isDot
    } else if (is.numeric(pch)) {
        isDot <- pch == 46
        isChar <- pch > 25 & !isDot
    }
    if (!is.null(tmpattr$"font-size")) {
        ## Strip unnecessary attribs
        tmpattr$"font-size"[isDot] <- ""
    }
    if (any(isDot)) {
        ## Because we really want just a dot, use crispEdges
        ## as anti-aliasing isn't really necessary
        tmpattr$"shape-rendering" <- "auto"
        tmpattr$"shape-rendering"[isDot] <- "crispEdges"
    }
    if (any(isChar)) {
        ## Make the s-w small so we see a stroke just barely
        tmpattr$"stroke-width"[isChar] <- "0.1"
        ## Set the font-size, otherwise it's going to mess with our scaling.
        ## 10px so it's the size of the point definition
        tmpattr$"font-size"[isChar] <- "10"
    }
    
    attrlist <- attrListVec(tmpattr)
    attrStrings <- mapply(function(a, aname) {
                              paste0(aname, '="', a, '"')
                          },
                          attrlist, names(attrlist), SIMPLIFY=FALSE)
    useString <- paste0("<use ",
                         apply(matrix(unlist(attrStrings),
                                      ncol=length(attrlist)),
                               1, paste, collapse=" "),
                         "/>")
    useString[!draw] <- ""
    ## Excapsulate within anchor ?
    if (!is.null(links) && any(has.link <- !is.na(links))) {
        useString[has.link] <- paste0(svgStartLinkString(links[id], show[id]),
                                       useString[has.link],
                                       svgEndLinkString(links[id]))
    }
    svgString <- paste0('<temp xmlns:xlink="http://www.w3.org/1999/xlink">',
                        paste(useString, collapse=""), "</temp>")
    newNode <- xmlParse(svgString, asText=TRUE)
    addChildren(svgDevParent(svgdev), kids=xmlChildren(xmlRoot(newNode)))
}

