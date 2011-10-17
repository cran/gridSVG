library(grid)
library(gridSVG)

# animValues

animValue(letters[1:4])
animValue(letters[1:4], timeid=rep(1:2, 2))
animValue(letters[1:4], id=rep(1:2, 2))

as.animValue(letters[1:4])
as.animValue(matrix(letters[1:12], ncol=4))
as.animValue(matrix(letters[1:12], ncol=4), multVal=TRUE)
as.animValue(list(letters[1:3], letters[4:6]))
as.animValue(list(letters[1:3], letters[4:6]), multVal=TRUE)

# animUnits

animUnit(unit(1:4, "npc"))
animUnit(unit(1:4, "npc"), timeid=rep(1:2, 2))
animUnit(unit(1:4, "npc"), id=rep(1:2, 2))

as.animUnit(1:4, "npc")
as.animUnit(unit(1:4, "npc"))
as.animUnit(matrix(1:12, ncol=4), "in")
as.animUnit(matrix(1:12, ncol=4), "in", multVal=TRUE)
as.animUnit(list(unit(1:3, "npc"), unit(4:6, "in")))
as.animUnit(list(unit(1:3, "npc"), unit(4:6, "in")), multVal=TRUE)

# Some default settings
pushViewport(viewport(gp=gpar(col="black", fill=NA)))

grid.rect(name="rect",
          x=0,
          y=0,
          just=c("left", "bottom"))
grid.animate("rect", x=unit(0:30, "mm"), duration=5, rep=TRUE)
grid.circle(name="circle",
            x=unit(0.5, "npc") + unit(0, "mm"),
            r=unit(10, "mm"))
grid.animate("circle", x=unit(0.5, "npc") + unit(0:30, "mm"),
             duration=5, rep=TRUE)
grid.text("hello", name="text1",
          x=unit(0.3, "npc") + unit(0, "mm"))
grid.animate("text1",
             x=unit(0.3, "npc") + unit(0:30, "mm"),
             duration=5, rep=TRUE)
grid.text("hello", name="text2",
          x=unit(0.3, "npc") + unit(0, "mm"),
          y=unit(0.3, "npc") + unit(0, "mm"))
grid.animate("text2",
             x=unit(0.3, "npc") + unit(0:30, "mm"),
             y=unit(0.3, "npc") + unit(0:30, "mm"),
             duration=5, rep=TRUE)

popViewport()

gridToSVG("animate.svg")


# Animating rectangles

# There are numerous possibilities to consider:
#   The animation values could be numeric, unit, matrix, or list
#   The original values could spec a single rect or multiple rects
#   We could animate only one of x/y/width/height or several of them at once

# Simple case
# (single rect, anim only x, anim values are just numeric)
grid.newpage()
grid.text("One rectangle moves across",
          y=unit(1, "lines"))
grid.rect()
grid.rect(x=.2, y=.2, width=.1, height=.1, name="rect")
grid.animate("rect", x=c(.2, .8), duration=3)
gridToSVG("anim-rect-simple.svg")

# Complex case
# (multiple rects, anim x/y/width/height, anim values are matrices and lists)
grid.newpage()
grid.text("Three rectangles: one goes up, one goes across, and
one goes diagonal and gets smaller",
          y=unit(1, "lines"))
grid.rect()
grid.rect(x=rep(.2, 3), y=.2, width=.1, height=.1, name="rect")
grid.animate("rect",
             x=cbind(c(.2, .8), c(.2, .8), .2),
             y=cbind(.2, c(.2, .8), c(.2, .8)),
             width=list(unit(.1, "npc"),
               unit(c(.1, 1), c("npc", "cm")),
               unit(.1, "npc")),
             height=list(unit(.1, "npc"),
               unit(c(.1, 1), c("npc", "cm")),
               unit(.1, "npc")),             
             duration=3)
gridToSVG("anim-rect-complex.svg")

# Animating circles

# Complex case
# (multiple circles, anim x/y/width/height, anim values are matrices and lists)
grid.newpage()
grid.text("Three circles: one goes up, one goes across, and
one goes diagonal and gets smaller",
          y=unit(1, "lines"))
grid.rect()
grid.circle(x=rep(.2, 3), y=.2, r=.1, name="circle")
grid.animate("circle",
             x=cbind(c(.2, .8), c(.2, .8), .2),
             y=cbind(.2, c(.2, .8), c(.2, .8)),
             r=list(unit(.1, "npc"),
               unit(c(.1, 1), c("npc", "cm")),
               unit(.1, "npc")),             
             duration=3)
gridToSVG("anim-circle-complex.svg")

# Animating points

# Complex case
# (multiple circles, anim x/y/width/height, anim values are matrices and lists)
grid.newpage()
grid.text("Three points: one goes up, one goes across, and
one goes diagonal and gets larger",
          y=unit(1, "lines"))
grid.rect()
pushViewport(viewport())
grid.points(x=rep(.2, 3), y=rep(.2, 3), size=unit(2, "mm"), name="points")
grid.animate("points",
             x=cbind(c(.2, .8), c(.2, .8), .2),
             y=cbind(.2, c(.2, .8), c(.2, .8)),
             size=list(unit(2, "mm"),
               unit(c(2, .1), c("mm", "npc")),
               unit(2, "mm")),             
             duration=3)
gridToSVG("anim-points-complex.svg")

# Animating text

# Complex case
# (multiple text, anim x/y/width/height, anim values are matrices and lists)
grid.newpage()
grid.text("Three letters:  one goes up, one goes across, and
one goes diagonal",
          y=unit(1, "lines"))
grid.rect()
grid.text(letters[1:3], x=rep(.2, 3), y=.2, name="text")
grid.animate("text",
             x=cbind(c(.2, .8), c(.2, .8), .2),
             y=cbind(.2, c(.2, .8), c(.2, .8)),             
             duration=3)
gridToSVG("anim-text-complex.svg")

# Animating lines

# Simple case
# (line only has two points, animation only has two points, only animate x)
grid.newpage()
grid.text("45 degree line becomes vertical",
          y=unit(1, "lines"))
grid.rect()
grid.lines(c(.1, .9), c(.1, .9), name="lines")
grid.animate("lines",
             x=cbind(c(.1, .9), c(.5, .5)),
             duration=3)
gridToSVG("anim-lines-simple.svg")

# Complex case
# (line has many points, animation has three points, only animate y)
x <- seq(-pi, pi, length.out=100)
y <- sin(x)
grid.newpage()
grid.text("Sine curve becomes flat then inverts (on y)",
          y=unit(1, "lines"))
grid.rect()
pushViewport(dataViewport(x, y))
grid.lines(x, y, default.units="native", name="lines")
grid.animate("lines",
             y=cbind(y, 0, -y),
             duration=3)
gridToSVG("anim-lines-complex.svg")

# Animating polylines

# Simple case
# (line only has two points, animation only has two points, only animate x)
grid.newpage()
grid.text("Two parallel lines slide to the right",
          y=unit(1, "lines"))
grid.rect()
grid.polyline(c(.1, .2, .3, .4),
              c(.1, .9, .1, .9),
              id=rep(1:2, each=2), name="polyline")
grid.animate("polyline",
             x=animUnit(unit(c(.1, .2, .3, .4,
                               .5, .6, .7, .8),
                             unit="npc"),
                           id=rep(rep(1:2, each=2), 2),
                           timeid=rep(1:2, each=4)),
             duration=3)
gridToSVG("anim-polyline-simple.svg")

# Complex case
# (line only has many points, animation only has many points, animate x and y)
grid.newpage()
grid.text("Two random walks",
          y=unit(1, "lines"))
grid.rect()
n <- 50
x <- 1:n
y1 <- runif(n, .6, .8)
y2 <- runif(n, .2, .4)
pushViewport(dataViewport(x, yscale=0:1))
grid.polyline(rep(x[1:2], 2), c(y1[1:2], y2[1:2]),
              default.units="native",
              id=rep(1:2, each=2), name="polyline")
grid.animate("polyline",
             x=animUnit(unit(rep(x[unlist(lapply(2:n, seq))], 2),
                             "native"),
                        id=rep(1:2, each=sum(2:n)),
                        timeid=rep(1:(n - 1), 2:n)),
             y=animUnit(unit(c(y1[unlist(lapply(2:n, seq))],
                               y2[unlist(lapply(2:n, seq))]),
                             "native"),
                        id=rep(1:2, each=sum(2:n)),
                        timeid=rep(1:(n - 1), 2:n)),
             duration=10)
gridToSVG("anim-polyline-complex.svg")

# Animating segments

# Simple case
# (single segment, animation only has two values, only animate x0)
grid.newpage()
grid.text("45 degree line becomes vertical (on right)",
          y=unit(1, "lines"))
grid.rect()
grid.segments(.1, .1, .9, .9, name="segments")
grid.animate("segments",
             x0=c(.1, .9),
             duration=3)
gridToSVG("anim-segments-simple.svg")

# Complex case
# (multiple segments, animation has three values, animate x0 and y0)
grid.newpage()
grid.text("crossed lines swing out to vertical then shorten",
          y=unit(1, "lines"))
grid.rect()
grid.segments(c(.1, .9), .1, c(.9, .1), .9, name="segments")
grid.animate("segments",
             x0=cbind(c(.1, .9, .9), c(.9, .1, .1)),
             y0=c(.1, .1, .5),
             duration=3)
gridToSVG("anim-segments-complex.svg")

############################################

# Multiple animations on same grob
grid.newpage()
grid.rect(x=.1, y=.1, width=.1, height=.1, name="r")
grid.animate("r", x=c(.1, .9))
grid.animate("r", x=c(.9, .1), begin=3)
gridToSVG("anim-rect-multi.svg")
 
# Animate group
grid.newpage()
grid.rect(x=.1, y=.1, width=.1, height=.1, name="r")
grid.animate("r", visibility=c("visible", "hidden"), group=TRUE)
gridToSVG("anim-group.svg")

