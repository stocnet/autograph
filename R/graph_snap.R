# The grid-snapping step behind `graphr(snap = TRUE)`.
#
# Nothing here is a layout: these functions take the coordinates a layout has
# already produced and move them onto a square grid. Two routes lead there.
# `.snap_basis()` looks for a repeating structure in the tie vectors -- the two
# steps a lattice repeats -- and maps those two steps onto the axes, which puts
# every node on its own integer point. `depth_first_recursive_search()` handles
# every other network, by matching each node to the nearest vacant grid point.

#' Layouts for snapping layouts to a grid
#'
#' @description The function uses approximate pattern matching
#'   to redistribute coarse layouts on square grid points, while
#'   preserving the topological relationships among the nodes (see Inoue et al. 2012). 
#' @references
#' Inoue, Kentaro, Shinichi Shimozono, Hideaki Yoshida, and Hiroyuki Kurata. 2012. 
#' “Application of Approximate Pattern Matching in Two Dimensional Spaces to Grid Layout for Biochemical Network Maps” edited by J. Bourdon. 
#' _PLoS ONE_ 7(6):e37739.
#' \doi{https://doi.org/10.1371/journal.pone.0037739}.
#' @keywords internal
depth_first_recursive_search <- function(layout) {
  if("ggraph" %in% class(layout)) layout <- layout$data[,c("x","y")]
  layout <- as.data.frame(layout)
  dims <- ceiling(2 * sqrt(nrow(layout)))
  # evens <- 0:dims[0:dims %% 2 == 0]
  vacant_points <- expand.grid(seq.int(0, dims, 1), seq.int(0, dims, 1)) # create options
  vacant_points <- vacant_points - floor(dims / 2) # centre options
  names(vacant_points) <- c("x", "y")
  gridout <- layout[order(abs(layout[,1]) + abs(layout[,2])), ] # sort centroid distance
  nodes <- seq_len(nrow(gridout))
  for (i in nodes) {
    # Drop the first row (the node's distance to itself, always 0) before
    # picking the nearest vacant point. Comparing against the undropped vector
    # matched row 1 whenever a grid point coincided exactly with the node,
    # giving mindist 0 and a zero-row vacpoint. Two-mode layouts hit this on the
    # very first node, since their coordinates are exactly 0 or 1.
    dists <- as.matrix(stats::dist(rbind(gridout[i, 1:2], vacant_points),
                            method = "manhattan"))[-1, 1]
    mindist <- which.min(dists)
    vacpoint <- vacant_points[mindist, ]
    changes <- vacpoint - gridout[i, 1:2]
    gridout[nodes >= i, 1] <- gridout[nodes >= i, 1] + 
      changes[[1]]
    gridout[nodes >= i, 2] <- gridout[nodes >= i, 2] + 
      changes[[2]]
    vacant_points <- vacant_points[-mindist, ]
  }
  gridout[order(as.integer(row.names(gridout))), ] # reorder from centroid
  # gridout
  # plot(gridout[order(row.names(gridout)),])
}

# Snapping ----

# The one entry point graph_layout() calls. It returns a two column data frame
# of integer coordinates, one row for each node, in the order the layout holds
# them.
.snap_layout <- function(layout, graph) {
  out <- .snap_basis(layout, graph)
  if (is.null(out)) {
    coords <- as.data.frame(layout)[, c("x", "y")]
    out <- depth_first_recursive_search(.snap_rotate(coords, graph))
    names(out) <- c("x", "y")
  }
  out
}

# The tie vectors the layout draws: one row for each tie, holding the step from
# its first node to its second. Loops and ties whose ends coincide say nothing
# about direction, so they are dropped. `names = FALSE` keeps the ends as row
# numbers: a named network otherwise indexes the coordinates by name and gets
# NA for every tie.
.snap_edges <- function(layout, graph) {
  ed <- igraph::as_edgelist(graph, names = FALSE)
  if (is.null(ed) || nrow(ed) == 0L) return(NULL)
  v <- cbind(layout$x[ed[,2]] - layout$x[ed[,1]],
             layout$y[ed[,2]] - layout$y[ed[,1]])
  keep <- ed[,1] != ed[,2] & rowSums(v^2) > 1e-12
  if (sum(keep) < 2L) return(NULL)
  list(ed = ed[keep, , drop = FALSE], v = v[keep, , drop = FALSE])
}

# A tie and the same tie read backwards run in one direction, not two, so every
# vector is folded into the upper half plane before directions are counted.
.snap_fold <- function(v) {
  neg <- v[,1] < 0 | (abs(v[,1]) < 1e-9 & v[,2] < 0)
  v[neg, ] <- -v[neg, , drop = FALSE]
  v
}

# The directions the ties repeat, largest group first. Each group returns one
# vector: its mean direction, at the length of its shorter ties, since a group
# holds one step of the lattice and, where the layout stretched it, some longer
# ones.
.snap_directions <- function(v, tol = pi/18) {
  ang <- atan2(v[,2], v[,1]) %% pi
  o <- order(ang)
  ang <- ang[o]
  v <- v[o, , drop = FALSE]
  cl <- cumsum(c(TRUE, diff(ang) > tol))
  # The first and last groups sit either side of the fold, so they are one
  # group where the gap across it is small enough.
  if (max(cl) > 1L && (ang[1] + pi - ang[length(ang)]) < tol) cl[cl == max(cl)] <- 1L
  ks <- as.integer(names(sort(table(cl), decreasing = TRUE)))
  lapply(ks, function(k) {
    vv <- v[cl == k, , drop = FALSE]
    len <- sqrt(rowSums(vv^2))
    u <- colMeans(vv / len)
    u <- u / sqrt(sum(u^2))
    list(v = u * unname(stats::quantile(len, 0.25)),
         share = nrow(vv) / nrow(v))
  })
}

# The share of ties that the basis maps onto an integer step.
.snap_fit <- function(v, basis, tol = 0.2) {
  tv <- v %*% t(solve(basis))
  mean(apply(abs(tv - round(tv)), 1, max) < tol)
}

# A basis read off one group of ties carries that group's error, so a tie can
# map to 1.5 and round to 2, which opens a gap in the grid. Rounding each
# mapped tie to its integer step and refitting the basis on all of them by
# least squares closes the gap. Two or three rounds settle it.
.snap_refit <- function(basis, v, rounds = 10L) {
  targ <- NULL
  for (i in seq_len(rounds)) {
    tv <- v %*% t(solve(basis))
    new <- round(tv)
    if (!is.null(targ) && identical(new, targ)) break
    targ <- new
    keep <- rowSums(abs(targ)) > 0 & apply(abs(tv - targ), 1, max) < 0.45
    if (sum(keep) < 2L) break
    cf <- stats::lsfit(targ[keep, , drop = FALSE], v[keep, , drop = FALSE],
                       intercept = FALSE)$coefficients
    cand <- t(matrix(cf, nrow = 2L))
    if (any(!is.finite(cand)) || abs(det(cand)) < 1e-8) break
    basis <- cand
  }
  basis
}

# A basis can be read in eight ways: either vector first, each with either
# sign. They draw the same grid mirrored or turned, so the one that agrees most
# with the layout is the one to keep. A reader who runs graphr() and then
# graphr(snap = TRUE) then sees the same drawing, tidied.
.snap_orient <- function(basis, layout) {
  co0 <- as.matrix(layout[, c("x", "y")])
  best <- NULL
  for (swap in c(FALSE, TRUE)) for (s1 in c(1, -1)) for (s2 in c(1, -1)) {
    cand <- basis[, if (swap) c(2L, 1L) else c(1L, 2L), drop = FALSE]
    cand[,1] <- cand[,1] * s1
    cand[,2] <- cand[,2] * s2
    if (det(cand) <= 0) next # a reflection reads as a different drawing
    score <- .snap_agree(co0, co0 %*% t(solve(cand)))
    if (is.null(best) || score > best$score) best <- list(score = score, basis = cand)
  }
  if (is.null(best)) basis else best$basis
}

.snap_agree <- function(a, b) {
  score <- 0
  for (k in 1:2) {
    if (stats::sd(a[,k]) > 0 && stats::sd(b[,k]) > 0) {
      score <- score + stats::cor(a[,k], b[,k])
    }
  }
  score
}

# Rounding a mapped layout bends a row where the layout stretched it, and can
# put two nodes on one point. This sweep repairs both. Each node first takes a
# free point, nearest to where rounding put it. Then, over and over, each node
# moves to the free point, within two units, that best matches its ties to the
# steps they should take. It stops when a sweep moves nothing.
.snap_repair <- function(points, ed, targ, sweeps = 20L, radius = 2L) {
  n <- nrow(points)
  inc_from <- split(seq_len(nrow(ed)), factor(ed[,1], levels = seq_len(n)))
  inc_to   <- split(seq_len(nrow(ed)), factor(ed[,2], levels = seq_len(n)))
  taken <- new.env(hash = TRUE, parent = emptyenv())
  key <- function(p) paste(p[1], p[2], sep = ",")
  holder <- function(p) mget(key(p), envir = taken, ifnotfound = list(NA))[[1]]
  # One node to one point, starting from the middle outwards.
  for (i in order(rowSums(abs(points)))) {
    here <- points[i, ]
    r <- 0L
    while (!is.na(holder(here))) {
      r <- r + 1L
      ring <- .snap_ring(r)
      free <- which(vapply(seq_len(nrow(ring)), function(k)
        is.na(holder(points[i, ] + ring[k, ])), logical(1)))
      if (length(free)) here <- points[i, ] + ring[free[1], ]
    }
    points[i, ] <- here
    assign(key(here), i, envir = taken)
  }
  offsets <- as.matrix(expand.grid(dx = -radius:radius, dy = -radius:radius))
  offsets <- offsets[order(rowSums(abs(offsets))), , drop = FALSE]
  cost <- function(i, p) {
    out <- 0
    for (k in inc_from[[i]]) out <- out + sum((points[ed[k,2], ] - p - targ[k, ])^2)
    for (k in inc_to[[i]])   out <- out + sum((p - points[ed[k,1], ] - targ[k, ])^2)
    out
  }
  for (s in seq_len(sweeps)) {
    moved <- FALSE
    for (i in seq_len(n)) {
      here <- points[i, ]
      best <- here
      bestcost <- cost(i, here)
      for (r in seq_len(nrow(offsets))) {
        cand <- here + offsets[r, ]
        held <- holder(cand)
        if (!is.na(held) && held != i) next
        candcost <- cost(i, cand)
        if (candcost < bestcost - 1e-9) {
          bestcost <- candcost
          best <- cand
        }
      }
      if (any(best != here)) {
        rm(list = key(here), envir = taken)
        assign(key(best), i, envir = taken)
        points[i, ] <- best
        moved <- TRUE
      }
    }
    if (!moved) break
  }
  points
}

# How far a set of coordinates sits, as a whole, from the whole steps of the
# grid. Reading the coordinates as angles and taking their mean direction gives
# the shift that brings them nearest.
.snap_offset <- function(u) {
  a <- mean(exp(complex(imaginary = 2 * pi * u)))
  if (abs(a) < 1e-9) return(0)
  Arg(a) / (2 * pi)
}

# The points exactly r steps away, nearest first.
.snap_ring <- function(r) {
  grid <- as.matrix(expand.grid(dx = -r:r, dy = -r:r))
  ring <- grid[abs(grid[,1]) == r | abs(grid[,2]) == r, , drop = FALSE]
  ring[order(rowSums(ring^2)), , drop = FALSE]
}

# Snap by mapping the two steps the network repeats onto the axes. This draws a
# square lattice as a square grid, and a triangular lattice as a square grid
# with its third family of ties running diagonally. Returns NULL where the
# layout holds no such repeating structure, which leaves the network to
# depth_first_recursive_search().
.snap_basis <- function(layout, graph, threshold = 0.95, share = 0.2) {
  e <- .snap_edges(layout, graph)
  if (is.null(e)) return(NULL)
  # A structure repeats itself only where there are more ties than nodes. A
  # ring, or a tree, has about as many ties as nodes and no repeating steps,
  # however well two directions happen to fit it.
  if (nrow(e$ed) < 1.2 * nrow(layout)) return(NULL)
  folded <- .snap_fold(e$v)
  dirs <- .snap_directions(folded)
  if (length(dirs) < 2L) return(NULL)
  best <- NULL
  for (i in seq_len(length(dirs) - 1L)) for (j in seq(i + 1L, length(dirs))) {
    # A direction that only a few ties take says little about the structure,
    # and two of them can fit any layout by accident.
    if (min(dirs[[i]]$share, dirs[[j]]$share) < share) next
    basis <- cbind(dirs[[i]]$v, dirs[[j]]$v)
    if (abs(det(basis)) < 1e-8) next
    basis <- .snap_refit(basis, folded)
    fit <- .snap_fit(folded, basis)
    if (is.null(best) || fit > best$fit) best <- list(fit = fit, basis = basis)
  }
  if (is.null(best) || best$fit < threshold) return(NULL)
  basis <- .snap_orient(best$basis, layout)
  co <- as.matrix(layout[, c("x", "y")]) %*% t(solve(basis))
  co <- co - matrix(colMeans(co), nrow(co), 2L, byrow = TRUE)
  # Centring can leave a whole column of nodes at half a step, where rounding
  # sends one node up and its neighbour down and breaks the column. Sliding
  # each axis to where the nodes sit nearest to whole steps avoids that.
  for (k in 1:2) co[, k] <- co[, k] - .snap_offset(co[, k])
  targ <- round(co[e$ed[,2], , drop = FALSE] - co[e$ed[,1], , drop = FALSE])
  points <- .snap_repair(round(co), e$ed, targ)
  out <- as.data.frame(points)
  names(out) <- c("x", "y")
  rownames(out) <- NULL
  out
}

# Helper functions ----

.rotate_layout <- function(layout, angle) {
  rot <- matrix(c(cos(angle), -sin(angle),
                  sin(angle),  cos(angle)), ncol = 2)
  coords <- as.matrix(layout[, c("x", "y")])
  newcoords <- coords %*% rot
  layout$x <- newcoords[,1]
  layout$y <- newcoords[,2]
  layout
}

# How far the ties sit, on average, from the nearest cardinal direction. A
# layout whose ties run up and down and across scores 0, and one whose ties all
# run at 45 degrees scores pi/4.
.edge_angle_deviation <- function(layout, graph) {
  e <- .snap_edges(layout, graph)
  if (is.null(e)) return(0)
  ang <- atan2(e$v[,2], e$v[,1]) %% (pi/2)
  mean(pmin(ang, pi/2 - ang))
}

# Turn the layout to the angle at which its ties run most nearly up and down
# and across, which is the angle at which a square grid loses least.
.snap_rotate <- function(layout, graph) {
  angles <- seq(0, pi/2, length.out = 181)
  scores <- vapply(angles, function(a) {
    .edge_angle_deviation(.rotate_layout(layout, a), graph)
  }, numeric(1))
  .rotate_layout(layout, angles[which.min(scores)])
}
