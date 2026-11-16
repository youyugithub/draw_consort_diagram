# draw_consort_diagram

```
library(grid)

# Numeric values are interpreted as "lines"
.u <- function(x) if (is.unit(x)) x else unit(x, "lines")

# ============================================================
# Box
# ============================================================

myboxGrob <- function(label, x = 5, y = 5, width = NULL, height = 3,
                      left = NULL, right = NULL, top = NULL, bottom = NULL) {
  
  if (!is.null(left) && !is.null(right)) {
    x <- (.u(left) + .u(right)) / 2
    width <- .u(right) - .u(left)
  }
  
  if (!is.null(top) && !is.null(bottom)) {
    y <- (.u(top) + .u(bottom)) / 2
    height <- .u(bottom) - .u(top)
  }
  
  grob(
    label = label,
    x = .u(x),
    y = unit(1, "npc") - .u(y),
    width = if (is.null(width)) stringWidth(label) + .u(1) else .u(width),
    height = .u(height),
    cl = "mybox"
  )
}

drawDetails.mybox <- function(x, ...) {
  grid.rect(x$x, x$y, x$width, x$height,
            gp = gpar(lwd = 1.5, fill = "white"))
  grid.text(x$label, x$x, x$y)
}

xDetails.mybox <- function(x, theta) {
  grobX(rectGrob(x = x$x, y = x$y,
                 width = x$width, height = x$height), theta)
}

yDetails.mybox <- function(x, theta) {
  grobY(rectGrob(x = x$x, y = x$y,
                 width = x$width, height = x$height), theta)
}

# ============================================================
# Arrow
# ============================================================

myarrowGrob <- function(mybox_from, mybox_to,
                        theta_from = "south", theta_to = "north",
                        curve = "-V-", offset = 1) {
  
  parts <- regmatches(
    curve, regexec("^([udlr]*)-([HVUDLR]+)-([udlr]*)$", curve)
  )[[1]]
  
  if (length(parts) != 4)
    stop("Invalid curve syntax")
  
  # All calculations use numeric coordinates in lines
  offset <- convertHeight(.u(offset), "lines", valueOnly = TRUE)
  pageheight <- convertHeight(unit(1, "npc"), "lines", valueOnly = TRUE)
  
  # ----------------------------------------------------------
  # 1. Boundary and lowercase movements
  # ----------------------------------------------------------
  
  side <- function(box, theta, moves) {
    
    angle <- if (is.character(theta))
      c(east = 0, north = 90, west = 180, south = 270)[tolower(theta)]
    else theta
    
    if (length(angle) != 1 || is.na(angle))
      stop("Invalid theta")
    
    angle <- angle * pi / 180
    
    # Convert grid coordinates into numeric coordinates
    # Origin: top-left; y increases downward
    p <- list(
      x = convertX(grobX(box, theta), "lines", valueOnly = TRUE),
      y = pageheight - convertY(grobY(box, theta), "lines",
                                valueOnly = TRUE)
    )
    
    width <- convertWidth(box$width, "lines", valueOnly = TRUE)
    height <- convertHeight(box$height, "lines", valueOnly = TRUE)
    
    pts <- list(p)
    
    # Outward offset
    p$x <- p$x + offset * cos(angle)
    p$y <- p$y - offset * sin(angle)
    
    pts[[length(pts) + 1]] <- p
    
    # Additional lowercase movements
    for (s in strsplit(moves, "")[[1]]) {
      if (s == "l") p$x <- p$x - width / 2 - offset
      if (s == "r") p$x <- p$x + width / 2 + offset
      if (s == "u") p$y <- p$y - height / 2 - offset
      if (s == "d") p$y <- p$y + height / 2 + offset
      pts[[length(pts) + 1]] <- p
    }
    pts
  }
  
  from <- side(mybox_from, theta_from, parts[2])
  to   <- side(mybox_to, theta_to, parts[4])
  
  # ----------------------------------------------------------
  # 2. Middle route
  # ----------------------------------------------------------
  
  a <- from[[length(from)]]
  b <- to[[length(to)]]
  
  steps <- strsplit(parts[3], "")[[1]]
  
  horizontal <- steps %in% c("H", "L", "R")
  vertical   <- steps %in% c("V", "U", "D")
  
  nH <- sum(horizontal)
  nV <- sum(vertical)
  
  # Prevent diagonal joins caused by missing directions
  if (nH == 0 && abs(a$x - b$x) > 1e-8)
    stop("Middle route needs H: endpoints have different x coordinates")
  
  if (nV == 0 && abs(a$y - b$y) > 1e-8)
    stop("Middle route needs V: endpoints have different y coordinates")
  
  p <- a
  middle <- list(p)
  
  h <- 0
  v <- 0
  
  for (i in seq_along(steps)) {
    if (horizontal[i]) {
      h <- h + 1
      p$x <- a$x + (b$x - a$x) * h / nH
    } else {
      v <- v + 1
      p$y <- a$y + (b$y - a$y) * v / nV
    }
    middle[[length(middle) + 1]] <- p
  }
  
  
  # ----------------------------------------------------------
  # 3. Combine paths
  # ----------------------------------------------------------
  
  pts <- c(from, middle[-1], rev(to)[-1])
  polylineGrob(
    x = do.call(unit.c, lapply(pts, function(p) .u(p$x))),
    y = do.call(unit.c, lapply(pts, function(p) unit(1, "npc") - .u(p$y))),
    arrow = arrow(type = "closed", ends = "last",
                  angle = 15, length = .u(0.6)),
    gp = gpar(lwd = 1.5)
  )
}



grid.newpage()

a <- myboxGrob("Assessed for eligibility",
               x = 12, y = 4, width = 10)

b <- myboxGrob("Excluded",
               x = 30, y = 12, width = 10)

c <- myboxGrob("Randomized",
               x = 12, y = 20, width = 10)

grid.draw(myarrowGrob(a, b,
                      theta_from = "east",
                      theta_to = "west",
                      curve = "d-HVH-u"))

grid.draw(myarrowGrob(a, c,
                      theta_from = "south",
                      theta_to = "north",
                      curve = "-V-"))
```

grid.draw(a)
grid.draw(b)
grid.draw(c)
