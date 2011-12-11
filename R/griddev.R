
vpError <- function() {
  stop("vp should only be path")
}

# Functions to take a grid grob and call appropriate
# functions from dev.R to produce output on a device

# Each function has to convert locations and dimensions
# into device coordinates THEN call the dev.R function

# Convert a unit object to a value in "device" units
# The calls to convert*() are just to get 'valueOnly'
# from "inches" units.
cx <- function(x, dev) {
  inchToDevX(convertX(x, "inches", valueOnly=TRUE), dev)
}

cy <- function(x, dev) {
  inchToDevY(convertY(x, "inches", valueOnly=TRUE), dev)
}

cw <- function(x, dev) {
  inchToDevX(convertWidth(x, "inches", valueOnly=TRUE), dev)
}

ch <- function(x, dev) {
  inchToDevY(convertHeight(x, "inches", valueOnly=TRUE), dev)
}

# Convert a "distance" (e.g., a circle radius)
cd <- function(x, dev) {
  pmin(inchToDevX(convertWidth(x, "inches", valueOnly=TRUE), dev),
       inchToDevY(convertHeight(x, "inches", valueOnly=TRUE), dev))
}

# Create a full name for a sub-grob based on the name of a parent grob
subGrobName <- function(baseGrobName, subGrobName, separator = ".") {
    paste(baseGrobName, subGrobName, sep=separator)
}

# Return the base grob name given the full name of a sub-grob
baseGrobName <- function(subGrobName, separator = ".") {
  splitName <- unlist(strsplit(subGrobName, separator, fixed = TRUE))
  grobName <- paste(splitName[-length(splitName)], collapse = separator)

  # Returning the base name
  grobName
}

# Convert a gpar object to an device-neutral graphical parameter list
gparToDevPars <- function(gp) {
    # Split up col into col plus colAlpha
    if (!is.null(gp$col)) {
        rgba <- col2rgb(gp$col, alpha=TRUE)
        gp$colAlpha <- rgba[4]
    }
    # Ditto fill
    if (!is.null(gp$fill)) {
        rgba <- col2rgb(gp$fill, alpha=TRUE)
        gp$fillAlpha <- rgba[4]
    }
    gp
}

# Repeats all elements in a gpar() so that it is fully defined for n values
expandGpar <- function(gp, n) {
    # If there are actually gpar elements defined, repeat them
    if (length(gp) > 0) {
        for (i in 1:length(gp)) {
            gp[[i]] <- rep(gp[[i]], length.out = n)
        }
    }

    # Returning the gp
    gp
}

# Repeats all elements in an arrow() so that it is fully defined for n values
expandArrow <- function(arrow, n) {
    # If there is actually an arrow, repeat its components
    if (! is.null(arrow)) {
        for (i in 1:length(arrow)) {
            arrow[[i]] <- rep(arrow[[i]], length.out = n)
        }
    }

    # Returning the arrow
    arrow
}

# Converting locations and widths
locToInches <- function(x, y, dev) {
  # Convert x and y to inches
  x <- convertX(x, "inches", valueOnly=TRUE)
  y <- convertY(y, "inches", valueOnly=TRUE)
  # Transform to inches on device
  n <- max(length(x), length(y))
  loc <- cbind(rep(x, length=n),
               rep(y, length=n),
               rep(1, length=n)) %*% current.transform()
  x <- unit(loc[,1]/loc[,3], "inches")
  y <- unit(loc[,2]/loc[,3], "inches")
  list(x=x, y=y)
}

dimToInches <- function(w, h, dev) {
  # FIXME:  Doesn't handle rotated viewports!!
  w <- convertWidth(w, "inches")
  h <- convertHeight(h, "inches")
  list(w=w, h=h)
}

dToInches <- function(d, dev) {
  w <- convertWidth(d, "inches", valueOnly=TRUE)
  h <- convertHeight(d, "inches", valueOnly=TRUE)
  d <- unit(pmin(w, h), "inches")
  d
}

# Generate (left, bottom) from (x, y), (width, height), and justification
leftbottom <- function(x, y, width, height, just, dev) {
  left <- switch(just[1],
                 left=x,
                 center=unit(convertX(x, "inches", valueOnly=TRUE) -
                   convertWidth(0.5*width, "inches", valueOnly=TRUE),
                   "inches"),
                 centre=unit(convertX(x, "inches", valueOnly=TRUE) -
                   convertWidth(0.5*width, "inches", valueOnly=TRUE),
                   "inches"),
                 right=unit(convertX(x, "inches", valueOnly=TRUE) -
                   convertWidth(width, "inches", valueOnly=TRUE),
                   "inches"),
                 # If it's "bottom" or "top" just use "centre" for
                 # horizontal align
                 unit(convertX(x, "inches", valueOnly=TRUE) -
                   convertWidth(0.5*width, "inches", valueOnly=TRUE),
                   "inches"))
  bottom <- switch(if (length(just) > 1) just[2] else just[1],
                   bottom=y,
                   center=unit(convertY(y, "inches", valueOnly=TRUE) -
                     convertHeight(0.5*height, "inches", valueOnly=TRUE),
                     "inches"),
                   centre=unit(convertY(y, "inches", valueOnly=TRUE) -
                     convertHeight(0.5*height, "inches", valueOnly=TRUE),
                     "inches"),
                   top=unit(convertY(y, "inches", valueOnly=TRUE) -
                     convertHeight(height, "inches", valueOnly=TRUE),
                     "inches"),
                   # If it's "left" or "right" then use "centre" for
                   # vertical align
                   unit(convertY(y, "inches", valueOnly=TRUE) -
                     convertHeight(0.5*height, "inches", valueOnly=TRUE),
                     "inches"))
  locToInches(left, bottom, dev)
}

# Generate hjust/vjust from just
justTohjust <- function(just) {
  if (length(just) > 1)
    just <- just[1]

  if (is.numeric(just)) {
    # Rounding to nearest of 0, 0.5, 1
    roundedJust <- round(2 * just) / 2
    switch(as.character(roundedJust),
           "0" = "left",
           "0.5" = "centre",
           "1" = "right")
  } else {
    if (is.na(match(just[1], c("left", "right"))))
      "centre"
    else
      just[1]
  }
}

justTovjust <- function(just) {
  if (length(just) > 1)
    just <- just[2]

  if (is.numeric(just)) {
    # Rounding to nearest of 0, 0.5, 1
    roundedJust <- round(2 * just) / 2
    switch(as.character(roundedJust),
           "0" = "bottom",
           "0.5" = "centre",
           "1" = "top")
  } else {
    if (is.na(match(just[1], c("top", "bottom"))))
      "centre"
    else
      just
  }
}

changedGPar <- function(startGP, endGP) {
    diffGP <- mapply(function(x, y) !isTRUE(all.equal(x, y)),
                     endGP, startGP)
    do.call("gpar", unclass(endGP)[diffGP])
}

# Enforce a 'vp' setting
# This could be a viewport (or vpTree or vpList or vpStack) OR a vpPath
# The general idea is to push or down the 'vp' slot
# THEN check how far down we have come
# IF we have come more than one level down then start a group for
# the appropriate number of parent viewports as well as the current viewport
# AND then do the corresponding number of end groups afterwards
startGroup <- function(vp, depth, dev) {
    if (depth > 1)
        startGroup(vp$parent, depth - 1, dev)
    devStartGroup(devGrob(vp, dev), gparToDevPars(vp$gp), dev)
}
enforceVP <- function(vp, dev) {
    depth <- 0
    if (!is.null(vp)) {
        if (!inherits(vp, "vpPath")) {
            pushViewport(vp)
            depth <- grid:::depth(vp)
        } else {
            depth <- downViewport(vp)
        }
        startGroup(grid:::grid.Call("L_currentViewport"), depth, dev)
    }
    depth
}
unwindVP <- function(vp, depth, dev) {
    if (depth > 0) {
        for (i in 1:depth)
            devEndGroup("", dev)
        if (is.null(vp)) { # recorded pops or ups
            upViewport(depth)
        } else if (!inherits(vp, "vpPath")) {
            popViewport(depth)
        } else {
            upViewport(depth)
        }
    }
}

# Grob to SVG
# This mimics grid.draw()
# Push/down any viewports and then call primToDev() to produce SVG
grobToDev <- function(x, dev) {
  UseMethod("grobToDev", x)
}

grobToDev.default <- function(x, dev) {
  stop("We shouldn't be here!")
}

grobToDev.grob <- function(x, dev) {
  depth <- enforceVP(x$vp, dev)
  primToDev(x, dev)
  unwindVP(x$vp, depth, dev)
}

# grob to device grob
# This just converts a grid grob into a generic (bland) device grob
# (which is just a list of values)
devGrob <- function(x, dev) {
  UseMethod("devGrob")
}

devGrob.default <- function(x, dev) {
  list(name=x$name)
}

devGrob.lines <- function(x, dev) {
  loc <- locToInches(x$x, x$y, dev)
  
  # Need to add in attributes to know where arrows
  # go if we have any
  lineArrow <- x$arrow
  if (! is.null(lineArrow)) {
      ends <- switch(as.character(lineArrow$ends),
                     "1" = "first",
                     "2" = "last",
                     "3" = "both")
      list(x=cx(loc$x, dev),
           y=cy(loc$y, dev),
           arrow=list(ends = ends),
           name=x$name)
  } else {
      list(x=cx(loc$x, dev),
           y=cy(loc$y, dev),
           name=x$name)
  }
}

devGrob.polygon <- function(x, dev) {
  loc <- locToInches(x$x, x$y, dev)
  list(x=cx(loc$x, dev),
       y=cy(loc$y, dev),
       name=x$name)
}

devGrob.pathgrob <- function(x, dev) {
    # The complication is converting the 'x', 'y', and 'id's
    # into lists
    if (is.null(x$id) && is.null(x$id.lengths)) {
        loc <- locToInches(x$x, x$y, dev)

        list(x=cx(loc$x, dev),
             y=cy(loc$y, dev),
             rule=x$rule,
             name=x$name)
    } else {
        if (is.null(x$id)) {
            n <- length(x$id.lengths)
            id <- rep(1L:n, x$id.lengths)
        } else {
            n <- length(unique(x$id))
            id <- x$id
        }
        listX <- split(x$x, id)
        listY <- split(x$y, id)
        listLoc <- mapply(locToInches, listX, listY, MoreArgs=list(dev),
                          SIMPLIFY=FALSE)
        list(x=lapply(listLoc,
               function(loc, dev) { cx(loc$x, dev) }, dev),
             y=lapply(listLoc,
               function(loc, dev) { cy(loc$y, dev) }, dev),
             rule=x$rule,
             name=x$name)
    }
}

devGrob.rastergrob <- function(x, dev) {
  lb <- leftbottom(x$x, x$y, x$width, x$height, x$just, dev)
  dim <- dimToInches(x$width, x$height, dev)

  list(x=cx(lb$x, dev),
       y=cy(lb$y, dev),
       width=cw(dim$w, dev),
       height=ch(dim$h, dev),
       name=x$name)
}

devGrob.rect <- function(x, dev) {
  lb <- leftbottom(x$x, x$y, x$width, x$height, x$just, dev)
  dim <- dimToInches(x$width, x$height, dev)
  list(x=cx(lb$x, dev),
       y=cy(lb$y, dev),
       width=cw(dim$w, dev),
       height=ch(dim$h, dev),
       name=x$name)
}

devGrob.text <- function(x, dev) {
  loc <- locToInches(x$x, x$y, dev)
  gp <- get.gpar()
  charHeight <- grobHeight(textGrob("M", gp = x$gp))
  # The R graphics engine does some crazy-ass calculations to
  # determine line height.  This does WAAAAY back so we just
  # have to swallow and follow along.
  # textLineHeight <-  ch(charHeight * gp$lineheight, dev)
  textLineHeight <- ch(unit(gp$lineheight * gp$cex *
                            graphics::par("cin")[2], "inches"), dev)
  charHeight <- ch(charHeight, dev)
  
  # height of current font
  # This corresponds to lineheight in SVG terms,
  # which is defined to be font size
  # see http://www.w3.org/TR/SVG/propidx.html
  #   comment in row for 'baseline-shift' in the 'percentages' column
  # This is needed for positioning plotmath expressions
  # to anything close to the right place
  xcex <- if (is.null(x$gp$cex)) 1 else x$gp$cex
  fontHeight <- ch(unit(gp$fontsize * gp$cex * xcex/ 72, "inches"), dev)

  # Width of the text/expression
  
  # MUST set x$vp to NULL before doing the following calculations
  # because x$vp has already been asserted and the calculation may
  # involve trying to assert it again!
  # (which would mean hidden error because viewport pushed twice OR
  #  visible error because try to "down" to viewport that does not exist)
  x$vp <- NULL
  
  width <- cw(grobWidth(x), dev)
  height <- ch(grobHeight(x), dev)
  ascent <- ch(grobAscent(x), dev)
  descent <- ch(grobDescent(x), dev)
  
  # Checking whether to use just or [h/v]just
  # Will convert numerics to strings in justTo_just function
  just <- rep(x$just, length.out = 2)
  just <- c(justTohjust(just[1]),
            justTovjust(just[2]))
  if (! is.null(x$hjust))
    just[1] <- justTohjust(x$hjust)
  if (! is.null(x$vjust))
    just[2] <-justTovjust(x$vjust)
  hjust <- just[1]
  vjust <- just[2]

  list(x=cx(loc$x, dev),
       y=cy(loc$y, dev),
       text=x$label,
       hjust=hjust,
       vjust=vjust,
       rot=x$rot,
       width=width,
       height=height,
       ascent=ascent,
       descent=descent,
       lineheight=textLineHeight,
       fontheight=fontHeight,
       charheight=charHeight,
       fontfamily=gp$fontfamily,
       fontface=switch(gp$font,
         "plain", "bold", "italic", "bold.italic"),
       name=x$name)  
}

devGrob.circle <- function(x, dev) {
  loc <- locToInches(x$x, x$y, dev)
  list(x=cx(loc$x, dev),
       y=cy(loc$y, dev),
       r=cd(dToInches(x$r), dev),
       name=x$name)
}

vpUsageTable <- data.frame(vpname = character(0),
                           count = integer(0),
                           stringsAsFactors=FALSE)
assign("vpUsageTable", vpUsageTable, envir = .gridSVGEnv)

# Because viewports can be pushed into many times, and each
# time we push we start a group, we need a *unique* id for that
# group, otherwise clipping paths don't work correctly
getvpID <- function(vpname) {
  # Finding out how many times a VP has been pushed to so fara
  vput <- get("vpUsageTable", envir = .gridSVGEnv)
  vpcount <- vput[vput$vpname == vpname, "count"]

  # If the VP name is not in the usage table, add it
  if (length(vpcount) == 0) {
    vpcount <- 0
    assign("vpUsageTable", rbind(vput,
                                 data.frame(vpname = vpname,
                                            count = vpcount,
                                            stringsAsFactors = FALSE)),
           envir = .gridSVGEnv)
    vput <- get("vpUsageTable", envir = .gridSVGEnv)
  }

  # Incrementing the vp appearance counter and storing it
  vpcount <- vpcount + 1
  vput[vput$vpname == vpname, "count"] <- vpcount
  assign("vpUsageTable", vput, envir = .gridSVGEnv)

  vpID <- paste(vpname,
                vpcount,
                sep=".")

  # Returning the vpID
  vpID
}

devGrob.viewport <- function(x, dev) {
  vp <- x
  tm <- current.transform()
  if (is.null(vp$clip)) {
      clip <- FALSE
      list(name=getvpID(vp$name), clip=clip)
  } else if (is.na(vp$clip)) {
      # Clipping has been turned OFF
      # FIXME:  CANNOT do this in SVG (enlarge the clip path)
      clip <- FALSE
      list(name=getvpID(vp$name), clip=clip)
  } else if (! vp$clip) {
      clip <- FALSE
      list(name=getvpID(vp$name), clip=clip)
  } else {
      clip <- TRUE

      transloc <- c(0, 0, 1) %*% tm
      loc <- (transloc / transloc[3])[-3]
      
      list(vpx=cx(unit(loc[1], "inches"), dev),
           vpy=cy(unit(loc[2], "inches"), dev),
           vpw=cw(unit(1, "npc"), dev),
           vph=ch(unit(1, "npc"), dev),
           name=getvpID(vp$name),
           clip=clip)
  }  
}

devGrob.vpPath <- function(x, dev) {
  vp <- current.viewport()
  tm <- current.transform()
  if (is.null(vp$clip)) {
      clip <- FALSE
      list(name=getvpID(vp$name), clip=clip)
  } else if (is.na(vp$clip)) {
      # Clipping has been turned OFF
      # FIXME:  CANNOT do this in SVG (enlarge the clip path)
      clip <- FALSE
      list(name=getvpID(vp$name), clip=clip)
  } else if (! vp$clip) {
      clip <- FALSE
      list(name=getvpID(vp$name), clip=clip)
  } else {
      clip <- TRUE

      transloc <- c(0, 0, 1) %*% tm
      loc <- (transloc / transloc[3])[-3]
      
      list(vpx=cx(unit(loc[1], "inches"), dev),
           vpy=cy(unit(loc[2], "inches"), dev),
           vpw=cw(unit(1, "npc"), dev),
           vph=ch(unit(1, "npc"), dev),
           name=getvpID(vp$name),
           clip=clip)
  }  
}

devGrob.frame <- function(x, dev) {
  fvp <- x
  tm <- current.transform()
  if (is.null(fvp$clip)) {
    clip <- FALSE
    list(name=x$name, clip=clip)
  } else if (is.na(fvp$clip) | ! fvp$clip) {
    clip <- FALSE
    list(name=x$name, clip=clip)
  } else {
    clip <- TRUE

    transloc <- c(0, 0, 1) %*% tm
    loc <- (transloc / transloc[3])[-3]

    list(vpx=cx(unit(loc[1], "inches"), dev),
         vpy=cy(unit(loc[2], "inches"), dev),
         vpw=cw(unit(1, "npc"), dev),
         vph=ch(unit(1, "npc"), dev),
         name=x$name,    
         clip=clip)
  }  
}

devGrob.cellGrob <- function(x, dev) {
  cvp <- x
  tm <- current.transform()
  if (is.null(cvp$clip)) {
    clip <- FALSE
    list(name=x$name, clip=clip)
  } else if (is.na(cvp$clip) | ! cvp$clip) {
    clip <- FALSE
    list(name=x$name, clip=clip)
  } else {
    clip <- TRUE

    transloc <- c(0, 0, 1) %*% tm
    loc <- (transloc / transloc[3])[-3]

    list(vpx=cx(unit(loc[1], "inches"), dev),
         vpy=cy(unit(loc[2], "inches"), dev),
         vpw=cw(unit(1, "npc"), dev),
         vph=ch(unit(1, "npc"), dev),
         name=x$name,    
         clip=clip)
  }  
}

# Prim to Dev
# This generates SVG from the grob to reproduce the grob in SVG code
# General form:
#   startGroup
#   for i=1:n
#     dev&(devGrob(i))
#   endGroup
primToDev <- function(x, dev) {
  UseMethod("primToDev")
}

primToDev.grob <- function(x, dev) {
}

arrowAddName <- function(arrow, name) {
  list(angle = arrow$angle,
       length = arrow$length,
       ends = arrow$ends,
       type = arrow$type,
       name = name)
}

primToDev.lines <- function(x, dev) {
  # Grouping the grob
  devStartGroup(devGrob(x, dev), NULL, dev)

  # This is a bit of a special case where we know there is only one
  # actual graphical object that is being created, so we are simply
  # going to modify it's name in place.
  oldname <- x$name
  x$name <- subGrobName(x$name, 1)

  if (! is.null(x$arrow))
    devArrow(arrowAddName(x$arrow, x$name), gparToDevPars(x$gp), dev)
  devLines(devGrob(x, dev), gparToDevPars(x$gp), dev)

  # Ending the group
  x$name <- oldname
  devEndGroup(x$name, dev)
}

primToDev.polyline <- function(x, dev) {
  # If we only have one line
  if (is.null(x$id) && is.null(x$id.lengths)) {
      x$id <- rep(1L, length(x$x))
  }

  # Multiple lines exist
  if (is.null(x$id)) {
      n <- length(x$id.lengths)
      id <- rep(1L:n, x$id.lengths)
  } else {
      n <- length(unique(x$id))
      id <- x$id
  }

  # Each line has an id, grab corresponding positions
  listX <- split(x$x, id)
  listY <- split(x$y, id)

  # Gp needs to be defined for each sub-grob, as does arrow
  gp <- expandGpar(x$gp, n)
  arrows <- expandArrow(x$arrow, n)

  # Grouping each sub-grob
  devStartGroup(devGrob(x, dev), NULL, dev)

  # Now we want to create a new lineGrob for each line
  # Naming each line with the polyline name suffixed by its id
  for (i in 1:n) {
      lg <- linesGrob(x = listX[[i]],
                      y = listY[[i]],
                      gp = gp[i],
                      arrow = arrows[i],
                      default.units = x$default.units,
                      name = subGrobName(x$name, i))
      if (! is.null(lg$arrow))
          devArrow(arrowAddName(lg$arrow, lg$name), gparToDevPars(lg$gp), dev)
      devLines(devGrob(lg, dev), gparToDevPars(lg$gp), dev) 
  }

  # Ending the group
  devEndGroup(x$name, dev)
}

# Any more efficient way of doing this?
# FIXME:  will lose any extra attributes of segments grob
primToDev.segments <- function(x, dev) {
  nx0 <- length(x$x0)
  nx1 <- length(x$x1)
  ny0 <- length(x$y0)
  ny1 <- length(x$y1)
  n <- max(nx0, nx1, ny0, ny1)

  # Gp needs to be defined for each sub-grob, as does arrow
  gp <- expandGpar(x$gp, n)
  arrows <- expandArrow(x$arrow, n)

  # Grouping each sub-grob
  devStartGroup(devGrob(x, dev), NULL, dev)

  for (i in 1:n) {
    lg <- linesGrob(unit.c(x$x0[(i-1) %% nx0 + 1],
                           x$x1[(i-1) %% nx1 + 1]),
                    unit.c(x$y0[(i-1) %% ny0 + 1],
                           x$y1[(i-1) %% ny1 + 1]),
                    arrow = arrows[i],
                    default.units = x$default.units,
                    gp = gp[i],
                    name = subGrobName(x$name, i))
    if (! is.null(lg$arrow))
      devArrow(arrowAddName(lg$arrow, lg$name), gparToDevPars(lg$gp), dev)
    devLines(devGrob(lg, dev), gparToDevPars(lg$gp), dev)
  }

  # Ending the group
  devEndGroup(x$name, dev)
}

primToDev.polygon <- function(x, dev) {
  # If we have only one polygon
  if (is.null(x$id) && is.null(x$id.lengths)) {
      x$id <- rep(1L, length(x$x))
  }

  # If we have multiple polygons
  if (is.null(x$id)) {
      n <- length(x$id.lengths)
      id <- rep(1L:n, x$id.lengths)
  } else {
      n <- length(unique(x$id))
      id <- x$id
  }

  # Each polygon has an id, grab corresponding positions
  listX <- split(x$x, id)
  listY <- split(x$y, id)

  # Gp needs to be defined for each sub-grob
  gp <- expandGpar(x$gp, n)

  # Grouping each sub-grob
  devStartGroup(devGrob(x, dev), NULL, dev)

  # Now we want to create a new polygonGrob for each polygon
  # Naming each polygon with the polygon name suffixed by its id
  for (i in 1:n) {
      pg <- polygonGrob(x = listX[[i]],
                        y = listY[[i]],
                        gp = gp[i],
                        default.units = x$default.units,
                        name = subGrobName(x$name, i))
      devPolygon(devGrob(pg, dev), gparToDevPars(pg$gp), dev)
  }

  # Ending the group
  devEndGroup(x$name, dev)
}

primToDev.xspline <- function(x, dev) {
  # Setting up function that turns an xspline into a series of points
  # which is then used to define a line or path
  splineToGrob <- function(spline) {
    splinePoints <- xsplinePoints(spline)
    if (spline$open) {
        # Treating as a line because unclosed paths are not filled.
        # svgPath() assumes all paths are closed to allow for filling
        # but we are unable to supply parameters to it to allow for 
        # open paths
        splineGp <- spline$gp
        splineGp$fill <- "transparent"
        linesGrob(x = splinePoints$x,
                  y = splinePoints$y,
                  gp = splineGp,
                  arrow = spline$arrow,
                  default.units = spline$default.units,
                  name = spline$name)
    } else {
        pathGrob(x = splinePoints$x,
                 y = splinePoints$y,
                 gp = spline$gp,
                 default.units = spline$default.units,
                 name = spline$name)
    }
  }

  # If we have only one spline
  if (is.null(x$id) && is.null(x$id.lengths)) {
      x$id <- rep(1L, length(x$x))
  }

  # If we're dealing with more than one spline
  if (is.null(x$id)) {
      n <- length(x$id.lengths)
      id <- rep(1L:n, x$id.lengths)
  } else {
      n <- length(unique(x$id))
      id <- x$id
  }

  # Each xspline has an id, grab corresponding positions
  listX <- split(x$x, id)
  listY <- split(x$y, id)

  # If x$shape is not defined for each point, repeat it for all points
  pointShapes <- rep(x$shape, length.out = length(x$x))
  listShape <- split(pointShapes, id)

  # Like x$shape, if the following attributes not defined for each grob id, repeat it
  splineOpen <- rep(x$open, length.out = n)
  splineEnds <- rep(x$repEnds, length.out = n)

  # Gp needs to be defined for each sub-grob, as does arrow
  gp <- expandGpar(x$gp, n)
  arrows <- expandArrow(x$arrow, n)

  # Grouping each sub-grob
  devStartGroup(devGrob(x, dev), NULL, dev)

  # Now we want to create a new xsplineGrob for each xspline
  # Naming each xspline with the xspline name suffixed by its id
  for (i in 1:n) {
      xsg <- xsplineGrob(x = listX[[i]],
                         y = listY[[i]],
                         open = x$open, # Could use splineOpen[i] but grid.xspline applies this for the entire group of grobs
                         shape = listShape[[i]],
                         default.units = x$default.units,
                         repEnds = splineEnds[i],
                         arrow = arrows[i],
                         gp = gp[i],
                         name = subGrobName(x$name, i))
      sg <- splineToGrob(xsg)
      if (inherits(sg, "pathgrob")) {
          devPath(devGrob(sg, dev), gparToDevPars(sg$gp), dev)
      } else {
          if (! is.null(sg$arrow))
              devArrow(arrowAddName(sg$arrow, sg$name), gparToDevPars(sg$gp), dev)
          devLines(devGrob(sg, dev), gparToDevPars(sg$gp), dev)
      }
  }

  # Ending the group
  devEndGroup(x$name, dev)
}

primToDev.pathgrob <- function(x, dev) {
  # Grouping the grob
  devStartGroup(devGrob(x, dev), NULL, dev)

  # This is a bit of a special case where we know there is only one
  # actual graphical object that is being created, so we are simply
  # going to modify it's name in place.
  oldname <- x$name
  x$name <- subGrobName(x$name, 1)

  devPath(devGrob(x, dev), gparToDevPars(x$gp), dev)

  # Ending the group
  x$name <- oldname
  devEndGroup(x$name, dev)
}

primToDev.rastergrob <- function(x, dev) {
  # Finding out how many rasters we're dealing with
  n <- max(length(x$x), length(x$y), length(x$width), length(x$height))
  # Repeating components as necessary
  xs <- rep(x$x, length.out = n)
  ys <- rep(x$y, length.out = n)

  # Finding the dimensions of the image, c(height, width)
  rasterDims <- dim(x$raster)
  rasterHeight <- rasterDims[1]
  rasterWidth <- rasterDims[2]

  # If we haven't been given any information about the h or w,
  # blow the image up to the full size but respect the aspect ratio
  if (is.null(x$width) && is.null(x$height)) {
      # height > width
      if (rasterHeight > rasterWidth) {
          x$height <- unit(1, "npc")
          x$width <- unit(rasterWidth / rasterHeight, "npc")
      }

      # width > height
      if (rasterWidth > rasterHeight) {
          x$width <- unit(1, "npc")
          x$height <- unit(rasterHeight / rasterWidth, "npc")
      }

      # height == width
      if (rasterHeight == rasterWidth) {
          x$width <- unit(1, "npc")
          x$height <- unit(1, "npc")
      }
  }

  # If we're missing one of width or height
  # we need to assure a correct aspect ratio by setting
  # an appropriate value for the missing dimension
  if (is.null(x$width)) {
      heightNpcs <- convertHeight(x$height, "npc", valueOnly = TRUE)
      widthNpcs <- (rasterWidth / rasterHeight) * heightNpcs

      # If we encounter any widths that exceed the plot boundaries,
      # stretch/shrink to fit.
      if (any(widthNpcs > 1)) {
          widthNpcs[widthNpcs > 1] <- 1
          x$width <- unit(widthNpcs, "npc")
      } else {
          x$width <- (rasterWidth / rasterHeight) * x$height
      }
  }
         
  if (is.null(x$height)) {
      widthNpcs <- convertWidth(x$width, "npc", valueOnly = TRUE)
      heightNpcs <- (rasterHeight / rasterWidth) * widthNpcs

      # If we encounter any heights that exceed the plot boundaries,
      # stretch/shrink to fit.
      if (any(heightNpcs > 1)) {
          heightNpcs[heightNpcs > 1] <- 1
          x$height <- unit(heightNpcs, "npc")
      } else {
          x$height <- (rasterHeight / rasterWidth) * x$width
      }
  }

  widths <- rep(x$width, length.out = n)
  heights <- rep(x$height, length.out = n) 
  
  # Generating the filename of the raster
  fileloc <- paste(x$name, ".png", sep = "")

  # Because of issues regarding interpolation, it's best just to
  # store the raster with as large a dimension as possible.
  rasterDims <- c(ch(max(heights), dev), cw(max(widths), dev))

  png(filename = fileloc, width = rasterDims[2], height = rasterDims[1])
      # The raster stays the same and is only repeated for each appearance.
      # Given that we know the dimensions of the PNG, we can safely say that
      # the raster occupies the entireity of both the x and y dimensions.
      grid.raster(x$raster, width = 1, height = 1, interpolate = x$interpolate)
  dev.off()

  # Expand the gp such that it fully defines all sub-grobs
  gp <- expandGpar(x$gp, n)

  # Grouping each sub-grob
  devStartGroup(devGrob(x, dev), NULL, dev)

  for (i in 1:n) {
      rg <- rasterGrob(x$raster,
                       x = xs[i],
                       y = ys[i],
                       width = widths[i],
                       height = heights[i],
                       just = x$just,
                       hjust = x$hjust,
                       vjust = x$vjust,
                       default.units = x$default.units,
                       gp = gp[i], # Will be ignored, keeping anyway
                       name = subGrobName(x$name, i))
      devRaster(devGrob(rg, dev), gparToDevPars(rg$gp), dev)
  }

  # Ending the group
  devEndGroup(x$name, dev)
}

primToDev.rect <- function(x, dev) {
  # Finding out how many rects we're dealing with
  n <- max(length(x$x), length(x$y), length(x$width), length(x$height))
  # Repeating components as necessary
  xs <- rep(x$x, length.out = n)
  ys <- rep(x$y, length.out = n)
  widths <- rep(x$width, length.out = n)
  heights <- rep(x$height, length.out = n)

  # Expand the gp such that it fully defines all sub-grobs
  gp <- expandGpar(x$gp, n)

  # Grouping each sub-grob
  devStartGroup(devGrob(x, dev), NULL, dev)

  for (i in 1:n) {
      rg <- rectGrob(x = xs[i],
                     y = ys[i],
                     width = widths[i],
                     height = heights[i],
                     just = x$just,
                     hjust = x$hjust,
                     vjust = x$vjust,
                     default.units = x$default.units,
                     gp = gp[i],
                     name = subGrobName(x$name, i))
      devRect(devGrob(rg, dev), gparToDevPars(rg$gp), dev)
  }

  # Ending the group
  devEndGroup(x$name, dev)
}

primToDev.text <- function(x, dev) {
  # Finding out how many pieces of text we're dealing with
  n <- max(length(x$x), length(x$y), length(x$label))
  # Repeating components as necessary
  textX <- rep(x$x, length.out = n)
  textY <- rep(x$y, length.out = n)
  textRot <- rep(x$rot, length.out = n)

  # If any given label is a vector of length 0, we don't want NA to appear
  if (length(x$label) == 0) {
    textLabel <- " "
    textLabel <- rep(textLabel, length.out = n)
  } else {
    # Checking that no element of label vector is empty
    if (!is.language(x$label)) {
        textLabel <- sapply(x$label, function(t) {
            if (nchar(t) == 0 | length(t) == 0)
                " "
            else
                t
        })
    }
    textLabel <- rep(x$label, length.out = n)
  }

  
  # Expand the gp such that it fully defines all sub-grobs
  gp <- expandGpar(x$gp, n)

  # Grouping each sub-grob
  devStartGroup(devGrob(x, dev), NULL, dev)

  for (i in 1:n) {
      tg <- textGrob(x = textX[i],
                     y = textY[i],
                     label = textLabel[i],
                     rot = textRot[i],
                     just = x$just,
                     hjust = x$hjust,
                     vjust = x$vjust,
                     default.units = x$default.units,
                     gp = gp[i],
                     name = subGrobName(x$name, i))
      devText(devGrob(tg, dev), gparToDevPars(tg$gp), dev)
  }

  # Ending the group
  devEndGroup(x$name, dev)
}

primToDev.circle <- function(x, dev) {
  # Finding out how many circles we're dealing with
  n <- max(length(x$x), length(x$y), length(x$r))
  # Repeating components as necessary
  xs <- rep(x$x, length.out = n)
  ys <- rep(x$y, length.out = n)
  rs <- rep(x$r, length.out = n)

  # Expand the gp such that it fully defines all sub-grobs
  gp <- expandGpar(x$gp, n)

  # Grouping each sub-grob
  devStartGroup(devGrob(x, dev), NULL, dev)

  for (i in 1:n) {
      cg <- circleGrob(x = xs[i],
                       y = ys[i],
                       r = rs[i],
                       default.units = x$default.units,
                       gp = gp[i],
                       name = subGrobName(x$name, i))
      devCircle(devGrob(cg, dev), gparToDevPars(cg$gp), dev)
  }

  # Ending the group
  devEndGroup(x$name, dev)
}

# Quick fix for now
# Add device method eventually?
# Could get tricky to do all symbol types here ... (?)
primToDev.points <- function(x, dev) {
    # Finding out how many grobs we're going to be dealing with
    # length of x and y already checked in grid.points
    n <- length(x$x)

    # Expand the gp such that it fully defines all sub-grobs
    gp <- expandGpar(x$gp, n)

    # Grouping each sub-grob
    devStartGroup(devGrob(x, dev), NULL, dev) 

    # ONLY pch = 1 or 3 handled
    if (any(!x$pch %in% c(0:25, 32:127)))
        stop("Unsupported pch value")

    # These can differ for points
    pchs <- rep(x$pch, length.out = n)
    sizes <- rep(x$size, length.out = n)

    for (i in 1:n) {
        pgp <- gp[i]

        # Need to calculate the size of a char, which is affected by
        # cex and fontsize
        # A textGrob with an "M" will be a good approximation for the
        # point size when size is a "char"
        if (attr(sizes[i], "unit") == "char")
            pointSize <- convertHeight(grobHeight(textGrob("M", gp = pgp)),
                                       "inches")
        else
            pointSize <- sizes[i]
        
        if (pchs[i] == 0) {
            # pch = 0 does not have a fill
            pgp$fill <- "transparent"

            devRect(devGrob(rectGrob(x$x[i], x$y[i],
                                     pointSize, pointSize, name = subGrobName(x$name, i),
                                     default.units = x$default.units),
                                     dev),
                      gparToDevPars(pgp), dev)
        }

        if (pchs[i] == 1) {
            radius <- 0.5 * pointSize

            # pch = 1 does not have a fill
            pgp$fill <- "transparent"

            devCircle(devGrob(circleGrob(x$x[i], x$y[i],
                                         radius, name = subGrobName(x$name, i),
                                         default.units = x$default.units),
                                         dev),
                      gparToDevPars(pgp), dev)
        }

        if (pchs[i] == 2) {
            # pch = 2 does not have a fill
            pgp$fill <- "transparent"

            devLines(devGrob(linesGrob(unit.c(x$x[i] - 0.5*pointSize, x$x[i],
                                              x$x[i] + 0.5*pointSize, x$x[i] - 0.5*pointSize),
                                       unit.c(x$y[i] - 0.5*pointSize, x$y[i] + 0.5*pointSize,
                                              x$y[i] - 0.5*pointSize, x$y[i] - 0.5*pointSize),
                                       name = subGrobName(x$name, i),
                                       default.units = x$default.units),
                                       dev),
                     gparToDevPars(pgp), dev)
        }

        if (pchs[i] == 3) { 
            # Because we are dealing with multiple grobs in order to create
            # this point, we add an additional group, and integer suffixes to 
            # identify components of the point
            pointGroupName <- subGrobName(x$name, i)

            # Grouping each sub-grob, here we really do only need a name
            devStartGroup(list(name = pointGroupName), NULL, dev) 

            devLines(devGrob(linesGrob(unit.c(x$x[i] - 0.5*pointSize,
                                              x$x[i] + 0.5*pointSize),
                                       x$y[i],
                                       name = subGrobName(pointGroupName, 1),
                                       default.units = x$default.units),
                                       dev),
                     gparToDevPars(pgp), dev)
            devLines(devGrob(linesGrob(x$x[i],
                                       unit.c(x$y[i] - 0.5*pointSize,
                                              x$y[i] + 0.5*pointSize),
                                       name = subGrobName(pointGroupName, 2),
                                       default.units = x$default.units),
                                       dev),
                     gparToDevPars(pgp), dev)

            # Ending the group
            devEndGroup(x$name, dev) 
        }

        if (pchs[i] == 4) { 
            # Because we are dealing with multiple grobs in order to create
            # this point, we add an additional group, and integer suffixes to 
            # identify components of the point
            pointGroupName <- subGrobName(x$name, i)

            # Grouping each sub-grob, here we really do only need a name
            devStartGroup(list(name = pointGroupName), NULL, dev) 

            devLines(devGrob(linesGrob(unit.c(x$x[i] - 0.5*pointSize, x$x[i] + 0.5*pointSize),
                                       unit.c(x$y[i] + 0.5*pointSize, x$y[i] - 0.5*pointSize),
                                       name = subGrobName(pointGroupName, 1),
                                       default.units = x$default.units),
                                       dev),
                     gparToDevPars(pgp), dev)
            devLines(devGrob(linesGrob(unit.c(x$x[i] - 0.5*pointSize, x$x[i] + 0.5*pointSize),
                                       unit.c(x$y[i] - 0.5*pointSize, x$y[i] + 0.5*pointSize),
                                       name = subGrobName(pointGroupName, 2),
                                       default.units = x$default.units),
                                       dev),
                     gparToDevPars(pgp), dev)

            # Ending the group
            devEndGroup(x$name, dev) 
        }

        if (pchs[i] == 5) { 
            # pch = 5 does not have a fill
            pgp$fill <- "transparent"

            devLines(devGrob(linesGrob(unit.c(x$x[i] - 0.5*pointSize, x$x[i],
                                              x$x[i] + 0.5*pointSize, x$x[i], x$x[i] - 0.5*pointSize),
                                       unit.c(x$y[i], x$y[i] + 0.5*pointSize,
                                              x$y[i], x$y[i] - 0.5*pointSize, x$y[i]),
                                       name = subGrobName(x$name, i),
                                       default.units = x$default.units),
                                       dev),
                     gparToDevPars(pgp), dev)
        }

        if (pchs[i] == 6) {
            # pch = 6 does not have a fill
            pgp$fill <- "transparent"

            devLines(devGrob(linesGrob(unit.c(x$x[i] - 0.5*pointSize, x$x[i] + 0.5*pointSize,
                                       x$x[i], x$x[i] - 0.5*pointSize),
                                       unit.c(x$y[i] + 0.5*pointSize, x$y[i] + 0.5*pointSize,
                                              x$y[i] - 0.5*pointSize, x$y[i] + 0.5*pointSize),
                                       name = subGrobName(x$name, i),
                                       default.units = x$default.units),
                                       dev),
                     gparToDevPars(pgp), dev)
        }

        if (pchs[i] == 7) {
            # pch = 7 does not have a fill
            pgp$fill <- "transparent"

            # Because we are dealing with multiple grobs in order to create
            # this point, we add an additional group, and integer suffixes to 
            # identify components of the point
            pointGroupName <- subGrobName(x$name, i)

            # Grouping each sub-grob, here we really do only need a name
            devStartGroup(list(name = pointGroupName), NULL, dev) 

            devLines(devGrob(linesGrob(unit.c(x$x[i] - 0.5*pointSize, x$x[i] + 0.5*pointSize),
                                       unit.c(x$y[i] + 0.5*pointSize, x$y[i] - 0.5*pointSize),
                                       name = subGrobName(pointGroupName, 1),
                                       default.units = x$default.units),
                                       dev),
                     gparToDevPars(pgp), dev)
            devLines(devGrob(linesGrob(unit.c(x$x[i] - 0.5*pointSize, x$x[i] + 0.5*pointSize),
                                       unit.c(x$y[i] - 0.5*pointSize, x$y[i] + 0.5*pointSize),
                                       name = subGrobName(pointGroupName, 2),
                                       default.units = x$default.units),
                                       dev),
                     gparToDevPars(pgp), dev)
            devRect(devGrob(rectGrob(x$x[i], x$y[i],
                                     pointSize, pointSize, name = subGrobName(pointGroupName, 3),
                                     default.units = x$default.units),
                                     dev),
                      gparToDevPars(pgp), dev)

            # Ending the group
            devEndGroup(x$name, dev) 
        }

        if (pchs[i] == 8) {
            # Because we are dealing with multiple grobs in order to create
            # this point, we add an additional group, and integer suffixes to 
            # identify components of the point
            pointGroupName <- subGrobName(x$name, i)

            # Grouping each sub-grob, here we really do only need a name
            devStartGroup(list(name = pointGroupName), NULL, dev) 

            devLines(devGrob(linesGrob(unit.c(x$x[i] - 0.5*pointSize,
                                              x$x[i] + 0.5*pointSize),
                                       x$y[i],
                                       name = subGrobName(pointGroupName, 1),
                                       default.units = x$default.units),
                                       dev),
                     gparToDevPars(pgp), dev)
            devLines(devGrob(linesGrob(x$x[i],
                                       unit.c(x$y[i] - 0.5*pointSize,
                                              x$y[i] + 0.5*pointSize),
                                       name = subGrobName(pointGroupName, 2),
                                       default.units = x$default.units),
                                       dev),
                     gparToDevPars(pgp), dev)
            devLines(devGrob(linesGrob(unit.c(x$x[i] - 0.5*pointSize, x$x[i] + 0.5*pointSize),
                                       unit.c(x$y[i] + 0.5*pointSize, x$y[i] - 0.5*pointSize),
                                       name = subGrobName(pointGroupName, 3),
                                       default.units = x$default.units),
                                       dev),
                     gparToDevPars(pgp), dev)
            devLines(devGrob(linesGrob(unit.c(x$x[i] - 0.5*pointSize, x$x[i] + 0.5*pointSize),
                                       unit.c(x$y[i] - 0.5*pointSize, x$y[i] + 0.5*pointSize),
                                       name = subGrobName(pointGroupName, 4),
                                       default.units = x$default.units),
                                       dev),
                     gparToDevPars(pgp), dev)

            # Ending the group
            devEndGroup(x$name, dev) 
        }

        if (pchs[i] == 9) {
            # Because we are dealing with multiple grobs in order to create
            # this point, we add an additional group, and integer suffixes to 
            # identify components of the point
            pointGroupName <- subGrobName(x$name, i)

            # Grouping each sub-grob, here we really do only need a name
            devStartGroup(list(name = pointGroupName), NULL, dev) 

            devLines(devGrob(linesGrob(unit.c(x$x[i] - 0.5*pointSize,
                                              x$x[i] + 0.5*pointSize),
                                       x$y[i],
                                       name = subGrobName(pointGroupName, 1),
                                       default.units = x$default.units),
                                       dev),
                     gparToDevPars(pgp), dev)
            devLines(devGrob(linesGrob(x$x[i],
                                       unit.c(x$y[i] - 0.5*pointSize,
                                              x$y[i] + 0.5*pointSize),
                                       name = subGrobName(pointGroupName, 2),
                                       default.units = x$default.units),
                                       dev),
                     gparToDevPars(pgp), dev)
            devLines(devGrob(linesGrob(unit.c(x$x[i] - 0.5*pointSize, x$x[i],
                                              x$x[i] + 0.5*pointSize, x$x[i], x$x[i] - 0.5*pointSize),
                                       unit.c(x$y[i], x$y[i] + 0.5*pointSize,
                                              x$y[i], x$y[i] - 0.5*pointSize, x$y[i]),
                                       name = subGrobName(pointGroupName, 3),
                                       default.units = x$default.units),
                                       dev),
                     gparToDevPars(pgp), dev)

            # Ending the group
            devEndGroup(x$name, dev) 
        }

        if (pchs[i] == 10) {
            # pch = 10 does not have a fill
            pgp$fill <- "transparent"

            radius <- 0.5 * pointSize

            # Because we are dealing with multiple grobs in order to create
            # this point, we add an additional group, and integer suffixes to 
            # identify components of the point
            pointGroupName <- subGrobName(x$name, i)

            # Grouping each sub-grob, here we really do only need a name
            devStartGroup(list(name = pointGroupName), NULL, dev) 

            devLines(devGrob(linesGrob(unit.c(x$x[i] - 0.5*pointSize,
                                              x$x[i] + 0.5*pointSize),
                                       x$y[i],
                                       name = subGrobName(pointGroupName, 1),
                                       default.units = x$default.units),
                                       dev),
                     gparToDevPars(pgp), dev)
            devLines(devGrob(linesGrob(x$x[i],
                                       unit.c(x$y[i] - 0.5*pointSize,
                                              x$y[i] + 0.5*pointSize),
                                       name = subGrobName(pointGroupName, 2),
                                       default.units = x$default.units),
                                       dev),
                     gparToDevPars(pgp), dev)
            devCircle(devGrob(circleGrob(x$x[i], x$y[i],
                                         radius, name = subGrobName(pointGroupName, 3),
                                         default.units = x$default.units),
                                         dev),
                      gparToDevPars(pgp), dev)

            # Ending the group
            devEndGroup(x$name, dev)
        }

        if (pchs[i] == 11) {
            # pch = 11 does not have a fill
            pgp$fill <- "transparent"

            # Because we are dealing with multiple grobs in order to create
            # this point, we add an additional group, and integer suffixes to 
            # identify components of the point
            pointGroupName <- subGrobName(x$name, i)

            # Grouping each sub-grob, here we really do only need a name
            devStartGroup(list(name = pointGroupName), NULL, dev) 

            devLines(devGrob(linesGrob(unit.c(x$x[i] - 0.5*pointSize, x$x[i],
                                              x$x[i] + 0.5*pointSize, x$x[i] - 0.5*pointSize),
                                       unit.c(x$y[i] - 0.5*pointSize, x$y[i] + 0.5*pointSize,
                                              x$y[i] - 0.5*pointSize, x$y[i] - 0.5*pointSize),
                                       name = subGrobName(pointGroupName, 1),
                                       default.units = x$default.units),
                                       dev),
                     gparToDevPars(pgp), dev)
            devLines(devGrob(linesGrob(unit.c(x$x[i] - 0.5*pointSize, x$x[i] + 0.5*pointSize,
                                       x$x[i], x$x[i] - 0.5*pointSize),
                                       unit.c(x$y[i] + 0.5*pointSize, x$y[i] + 0.5*pointSize,
                                              x$y[i] - 0.5*pointSize, x$y[i] + 0.5*pointSize),
                                       name = subGrobName(pointGroupName, 2),
                                       default.units = x$default.units),
                                       dev),
                     gparToDevPars(pgp), dev)

            # Ending the group
            devEndGroup(x$name, dev)
        }

        if (pchs[i] == 12) {
            # pch = 12 does not have a fill
            pgp$fill <- "transparent"

            # Because we are dealing with multiple grobs in order to create
            # this point, we add an additional group, and integer suffixes to 
            # identify components of the point
            pointGroupName <- subGrobName(x$name, i)

            # Grouping each sub-grob, here we really do only need a name
            devStartGroup(list(name = pointGroupName), NULL, dev) 

            devRect(devGrob(rectGrob(x$x[i], x$y[i],
                                     pointSize, pointSize, name = subGrobName(pointGroupName, 1),
                                     default.units = x$default.units),
                                     dev),
                      gparToDevPars(pgp), dev)
            devLines(devGrob(linesGrob(unit.c(x$x[i] - 0.5*pointSize,
                                              x$x[i] + 0.5*pointSize),
                                       x$y[i],
                                       name = subGrobName(pointGroupName, 2),
                                       default.units = x$default.units),
                                       dev),
                     gparToDevPars(pgp), dev)
            devLines(devGrob(linesGrob(x$x[i],
                                       unit.c(x$y[i] - 0.5*pointSize,
                                              x$y[i] + 0.5*pointSize),
                                       name = subGrobName(pointGroupName, 3),
                                       default.units = x$default.units),
                                       dev),
                     gparToDevPars(pgp), dev)

            # Ending the group
            devEndGroup(x$name, dev)
        }

        if (pchs[i] == 13) {
            # pch = 12 does not have a fill
            pgp$fill <- "transparent"

            radius <- 0.5 * pointSize

            # Because we are dealing with multiple grobs in order to create
            # this point, we add an additional group, and integer suffixes to 
            # identify components of the point
            pointGroupName <- subGrobName(x$name, i)

            # Grouping each sub-grob, here we really do only need a name
            devStartGroup(list(name = pointGroupName), NULL, dev) 

            devCircle(devGrob(circleGrob(x$x[i], x$y[i],
                                         radius, name = subGrobName(pointGroupName, 1),
                                         default.units = x$default.units),
                                         dev),
                      gparToDevPars(pgp), dev)
            devLines(devGrob(linesGrob(unit.c(x$x[i] - 0.5*pointSize, x$x[i] + 0.5*pointSize),
                                       unit.c(x$y[i] + 0.5*pointSize, x$y[i] - 0.5*pointSize),
                                       name = subGrobName(pointGroupName, 2),
                                       default.units = x$default.units),
                                       dev),
                     gparToDevPars(pgp), dev)
            devLines(devGrob(linesGrob(unit.c(x$x[i] - 0.5*pointSize, x$x[i] + 0.5*pointSize),
                                       unit.c(x$y[i] - 0.5*pointSize, x$y[i] + 0.5*pointSize),
                                       name = subGrobName(pointGroupName, 3),
                                       default.units = x$default.units),
                                       dev),
                     gparToDevPars(pgp), dev)

            # Ending the group
            devEndGroup(x$name, dev)
        }

        if (pchs[i] == 14) {
            # pch = 12 does not have a fill
            pgp$fill <- "transparent"

            # Because we are dealing with multiple grobs in order to create
            # this point, we add an additional group, and integer suffixes to 
            # identify components of the point
            pointGroupName <- subGrobName(x$name, i)

            # Grouping each sub-grob, here we really do only need a name
            devStartGroup(list(name = pointGroupName), NULL, dev) 

            devRect(devGrob(rectGrob(x$x[i], x$y[i],
                                     pointSize, pointSize, name = subGrobName(pointGroupName, 1),
                                     default.units = x$default.units),
                                     dev),
                      gparToDevPars(pgp), dev)
            devLines(devGrob(linesGrob(unit.c(x$x[i] - 0.5*pointSize, x$x[i],
                                              x$x[i] + 0.5*pointSize),
                                       unit.c(x$y[i] - 0.5*pointSize, x$y[i] + 0.5*pointSize,
                                              x$y[i] - 0.5*pointSize),
                                       name = subGrobName(pointGroupName, 2),
                                       default.units = x$default.units),
                                       dev),
                     gparToDevPars(pgp), dev)

            # Ending the group
            devEndGroup(x$name, dev)
        }

        if (pchs[i] == 15) {
            # pch = 15 does not have a border
            pgp$fill <- pgp$col
            pgp$col <- "transparent"

            devRect(devGrob(rectGrob(x$x[i], x$y[i],
                                     pointSize, pointSize, name = subGrobName(x$name, i),
                                     default.units = x$default.units),
                                     dev),
                      gparToDevPars(pgp), dev)
        }

        if (pchs[i] == 16) {
            radius <- 0.5 * pointSize

            # pch = 16 does not have a border
            pgp$fill <- pgp$col
            pgp$col <- "transparent"

            devCircle(devGrob(circleGrob(x$x[i], x$y[i],
                                         radius, name = subGrobName(x$name, i),
                                         default.units = x$default.units),
                                         dev),
                      gparToDevPars(pgp), dev)
        }

        if (pchs[i] == 17) {
            # pch = 17 does not have a border
            pgp$fill <- pgp$col
            pgp$col <- "transparent"

            devPolygon(devGrob(polygonGrob(unit.c(x$x[i] - 0.5*pointSize, x$x[i],
                                                  x$x[i] + 0.5*pointSize, x$x[i] - 0.5*pointSize),
                                           unit.c(x$y[i] - 0.5*pointSize, x$y[i] + 0.5*pointSize,
                                                  x$y[i] - 0.5*pointSize, x$y[i] - 0.5*pointSize),
                                           name = subGrobName(x$name, i),
                                           default.units = x$default.units),
                               dev),
                     gparToDevPars(pgp), dev)
        }

        if (pchs[i] == 18) {
            # pch = 18 does not have a border
            pgp$fill <- pgp$col
            pgp$col <- "transparent"

            devPolygon(devGrob(polygonGrob(unit.c(x$x[i] - 0.5*pointSize, x$x[i],
                                                  x$x[i] + 0.5*pointSize, x$x[i], x$x[i] - 0.5*pointSize),
                                           unit.c(x$y[i], x$y[i] + 0.5*pointSize,
                                                  x$y[i], x$y[i] - 0.5*pointSize, x$y[i]),
                                           name = subGrobName(x$name, i),
                                           default.units = x$default.units),
                               dev),
                     gparToDevPars(pgp), dev)
        }

        if (pchs[i] == 19) {
            radius <- 0.5 * pointSize

            # col is specified in place of fill
            pgp$fill <- pgp$col

            devCircle(devGrob(circleGrob(x$x[i], x$y[i],
                                         radius, name = subGrobName(x$name, i),
                                         default.units = x$default.units),
                                         dev),
                      gparToDevPars(pgp), dev)
        }

        if (pchs[i] == 20) {
            radius <- 0.5 * 2/3 * pointSize

            # col is specified in place of fill
            pgp$fill <- pgp$col

            devCircle(devGrob(circleGrob(x$x[i], x$y[i],
                                         radius, name = subGrobName(x$name, i),
                                         default.units = x$default.units),
                                         dev),
                      gparToDevPars(pgp), dev)
        }

        if (pchs[i] == 21) {
            radius <- 0.5 * pointSize

            devCircle(devGrob(circleGrob(x$x[i], x$y[i],
                                         radius, name = subGrobName(x$name, i),
                                         default.units = x$default.units),
                                         dev),
                      gparToDevPars(pgp), dev)
        }

        if (pchs[i] == 22) {
            devRect(devGrob(rectGrob(x$x[i], x$y[i],
                                     pointSize, pointSize, name = subGrobName(x$name, i),
                                     default.units = x$default.units),
                                     dev),
                      gparToDevPars(pgp), dev)
        }

        if (pchs[i] == 23) {
            devPolygon(devGrob(polygonGrob(unit.c(x$x[i] - 0.5*pointSize, x$x[i],
                                                  x$x[i] + 0.5*pointSize, x$x[i], x$x[i] - 0.5*pointSize),
                                           unit.c(x$y[i], x$y[i] + 0.5*pointSize,
                                                  x$y[i], x$y[i] - 0.5*pointSize, x$y[i]),
                                           name = subGrobName(x$name, i),
                                           default.units = x$default.units),
                               dev),
                     gparToDevPars(pgp), dev)
        }

        if (pchs[i] == 24) {
            devPolygon(devGrob(polygonGrob(unit.c(x$x[i] - 0.5*pointSize, x$x[i],
                                                  x$x[i] + 0.5*pointSize, x$x[i] - 0.5*pointSize),
                                           unit.c(x$y[i] - 0.5*pointSize, x$y[i] + 0.5*pointSize,
                                                  x$y[i] - 0.5*pointSize, x$y[i] - 0.5*pointSize),
                                           name = subGrobName(x$name, i),
                                           default.units = x$default.units),
                               dev),
                     gparToDevPars(pgp), dev)
        }

        if (pchs[i] == 25) {
            devPolygon(devGrob(polygonGrob(unit.c(x$x[i] - 0.5*pointSize, x$x[i] + 0.5*pointSize,
                                                  x$x[i], x$x[i] - 0.5*pointSize),
                                           unit.c(x$y[i] + 0.5*pointSize, x$y[i] + 0.5*pointSize,
                                                  x$y[i] - 0.5*pointSize, x$y[i] + 0.5*pointSize),
                                           name = subGrobName(x$name, i),
                                           default.units = x$default.units),
                               dev),
                     gparToDevPars(pgp), dev)
        }

        if (pchs[i] %in% 32:127) {
            asciiChar <-  parse(text = paste("\"\\", structure(pchs[i], class = "octmode"), "\"", sep = ""))[[1]]

            devText(devGrob(textGrob(asciiChar, x$x[i], x$y[i]),
                                     dev),
                    gparToDevPars(pgp), dev)
        }
    }

    # Ending the group
    devEndGroup(x$name, dev) 
}
  
primToDev.xaxis <- function(x, dev) {
    devStartGroup(devGrob(x, dev), gparToDevPars(x$gp), dev)
  # If the at is NULL then the axis will have no
  # children;  need to be calculated on-the-fly
  if (is.null(x$at)) {
    at <- grid.pretty(current.viewport()$xscale)
    grobToDev(grid:::make.xaxis.major(at, x$main), dev)
    grobToDev(grid:::make.xaxis.ticks(at, x$main), dev)
    if (x$label)
      grobToDev(grid:::make.xaxis.labels(at, x$label, x$main), dev)
  } 
    devEndGroup(x$name, dev)
}

primToDev.yaxis <- function(x, dev) {
    devStartGroup(devGrob(x, dev), gparToDevPars(x$gp), dev)
  # If the at is NULL then the axis will have no
  # children;  need to be calculated on-the-fly
  if (is.null(x$at)) {
    at <- grid.pretty(current.viewport()$yscale)
    grobToDev(grid:::make.yaxis.major(at, x$main), dev)
    grobToDev(grid:::make.yaxis.ticks(at, x$main), dev)
    if (x$label)
      grobToDev(grid:::make.yaxis.labels(at, x$label, x$main), dev)
  } 
    devEndGroup(x$name, dev)
}

grobToDev.frame <- function(x, dev) {
    depth <- enforceVP(x$vp, dev)

    if (!is.null(x$framevp)) {
        frameDepth <- enforceVP(x$framevp, dev)
    } 
    
    devStartGroup(devGrob(x, dev), gparToDevPars(x$gp), dev)
    lapply(x$children, grobToDev, dev)
    devEndGroup(x$name, dev)
    
    if (!is.null(x$framevp)) {
        unwindVP(x$framevp, frameDepth, dev)
    }
    
    unwindVP(x$vp, depth, dev)
}

grobToDev.cellGrob <- function(x, dev) {
    depth <- enforceVP(x$vp, dev)

    if (!is.null(x$cellvp)) {
        cellDepth <- enforceVP(x$cellvp, dev)
    }

    devStartGroup(devGrob(x, dev), gparToDevPars(x$gp), dev)
    lapply(x$children, grobToDev, dev)
    devEndGroup(x$name, dev)
  
    if (!is.null(x$cellvp)) {
        unwindVP(x$cellvp, cellDepth, dev)
    }

    unwindVP(x$vp, depth, dev)
}

grobToDev.gTree <- function(x, dev) {
  depth <- enforceVP(x$vp, dev)
  if (!is.null(x$childrenvp)) {
    pushViewport(x$childrenvp)
    upViewport(grid:::depth(x$childrenvp))
  }
  primToDev(x, dev)
  unwindVP(x$vp, depth, dev)
}

primToDev.gTree <- function(x, dev) {
    devStartGroup(devGrob(x, dev), gparToDevPars(x$gp), dev)
    lapply(x$children, grobToDev, dev)
    devEndGroup(x$name, dev)
}

# Viewports (and vpPaths and downs and ups)
# on the display list get recorded as wrapped grobs
grobToDev.recordedGrob <- function(x, dev) {
    x <- x$list
    if (!is.null(x$vp)) { # recorded pushViewport
        enforceVP(x$vp, dev)
    } else if (!is.null(x$path)) { # recorded downViewport
        enforceVP(x$path, dev)
    } else if (!is.null(x$n)) { # recorded up or pop
        unwindVP(NULL, x$n, dev)
    }
}

# grid to SVG
# Given a gTree created by grid.grab()
gridToDev <- function(gTree, dev) {
  grobToDev(gTree, dev)
}


