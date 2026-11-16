# draw_consort_diagram

```

library(grid)

.u <- function(x) if (is.unit(x)) x else unit(x, "lines")

# ============================================================
# Box
# ============================================================

myboxGrob <- function(label, x = 5, y = 5, width = NULL, height = 2,
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

# ============================================================
# Arrow
# ============================================================

myarrowGrob <- function(mybox_from, mybox_to,
                        side_from = "d", side_to = "u",
                        curve = "-V-", offset = 1) {
  
  parts <- regmatches(
    curve, regexec("^([udlr]*)-([HVUDLR]+)-([udlr]*)$", curve)
  )[[1]]
  
  if (length(parts) != 4)
    stop("Invalid curve syntax. Example: 'dl-HVH-ld'")
  
  offset <- .u(offset)
  
  
  # ----------------------------------------------------------
  # 1. Boundary point -> outward offset -> lowercase moves
  # ----------------------------------------------------------
  
  side <- function(box, direction, moves) {
    
    if (!direction %in% c("u", "d", "l", "r"))
      stop("Side must be one of: u, d, l, r")
    
    # Boundary coordinates
    p <- switch(direction,
                u = list(x = box$x, y = box$y + box$height / 2),
                d = list(x = box$x, y = box$y - box$height / 2),
                l = list(x = box$x - box$width / 2, y = box$y),
                r = list(x = box$x + box$width / 2, y = box$y)
    )
    
    pts <- list(p)
    
    # Stretch outward by offset
    if (direction == "u") p$y <- p$y + offset
    if (direction == "d") p$y <- p$y - offset
    if (direction == "l") p$x <- p$x - offset
    if (direction == "r") p$x <- p$x + offset
    
    pts[[length(pts) + 1]] <- p
    
    # Lowercase movements around the box
    for (s in strsplit(moves, "")[[1]]) {
      
      if (s == "u") p$y <- p$y + box$height / 2 + offset
      if (s == "d") p$y <- p$y - box$height / 2 - offset
      if (s == "l") p$x <- p$x - box$width / 2 - offset
      if (s == "r") p$x <- p$x + box$width / 2 + offset
      
      pts[[length(pts) + 1]] <- p
    }
    
    pts
  }
  
  from <- side(mybox_from, side_from, parts[2])
  to   <- side(mybox_to, side_to, parts[4])
  
  
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
  
  
  # ----------------------------------------------------------
  # 4. Draw polyline
  # ----------------------------------------------------------
  
  polylineGrob(
    x = do.call(unit.c, lapply(pts, `[[`, "x")),
    y = do.call(unit.c, lapply(pts, `[[`, "y")),
    arrow = arrow(type = "closed", ends = "last",
                  angle = 15, length = .u(0.6)),
    gp = gpar(lwd = 1.5)
  )
}


grid.newpage()

a <- myboxGrob("Box A", x = 8, y = 5, width = 8)
b <- myboxGrob("Box B", x = 20, y = 13, width = 8)

grid.draw(myarrowGrob(
  a, b,
  side_from = "r",
  side_to = "l",
  curve = "dl-VHV-ur"
))

grid.draw(a)
grid.draw(b)
```

