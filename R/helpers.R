utils::globalVariables(c("x", "y", "theta"))

# helpers

#' Makes arrow
#'
#' @param xmin xmin
#' @param xmax xmax
#' @param ymin ymin
#' @param ymax ymax
#' @param edge_length Edge length
#' @param head_length Head length
#' @param angle and
#' @param rotate_at Rotate at
make_arrow <- function(xmin, xmax, ymin, ymax, edge_length = 0.2, head_length = 0.2, angle = 0, rotate_at = "head") {

  if(length(angle) != length(xmin)) angle <- rep(angle[1], length(xmin))

  head_length <- head_length*max(xmax-xmin)

  map_dfr(1:length(xmin), function(k) {

    dx <- xmax[k]-xmin[k]
    dy <- ymax[k]-ymin[k]
    y0 <- ymin[k]+dy/2
    x0 <- switch(
      rotate_at,
      "head" = xmax[k],
      "center" = xmin[k]+dx/2
    )

    tibble(
      x = c(xmin[k], xmin[k], xmin[k]+dx-head_length,
            xmin[k]+dx-head_length, xmax[k], xmin[k]+dx-head_length,
            xmin[k]+dx-head_length, xmin[k])-x0,
      y = c(ymin[k]+edge_length, ymax[k]-edge_length,
            ymax[k]-edge_length, ymax[k], ymin[k]+dy/2, ymin[k],
            ymin[k]+edge_length, ymin[k]+edge_length)-y0
    ) |>
    rotate(angle[k]*pi/180) |>
    mutate(
      x = x+x0,
      y = y+y0,
      ) |>
    mutate(id = k)

  })

}

#' Rotations
#'
#' @param mat Matrix
#' @param angle angle
rotate <- function(mat, angle) {
  mat <- as.matrix(mat)
  cos_angle <- cos(angle)
  sin_angle <- sin(angle)
  mat_rot <- mat %*% t(matrix(c(cos_angle, sin_angle, -sin_angle, cos_angle), 2, 2))
  df <- as.data.frame(mat_rot)
  colnames(df) <- c("x", "y")
  df
}

#' Make the chevron
#'
#' @param xmin xmin
#' @param xmax xmax
#' @param ymin ymin
#' @param ymax ymax
#' @param angle angle
#' @param rotate_at Rotate at
make_chevron <- function(xmin, xmax, ymin, ymax, angle = 0, rotate_at = "center") {

  if(length(angle) != length(xmin)) angle <- rep(angle[1], length(xmin))

  map_dfr(1:length(xmin), function(k) {

    dx <- xmax[k]-xmin[k]
    dy <- ymax[k]-ymin[k]
    y0 <- ymin[k]+dy/2
    x0 <- switch(
      rotate_at,
      "head" = xmax[k],
      "center" = xmin[k]+dx/2
    )

    tibble(
      x = c(xmin[k], xmin[k]+dy/2, xmin[k], xmax[k]-dy/2, xmax[k], xmax[k]-dy/2)-x0,
      y = c(ymin[k], ymin[k]+dy/2, ymax[k], ymax[k], ymin[k]+dy/2, ymin[k])-y0
    ) |>
      rotate(angle[k]*pi/180) |>
      mutate(
        x = x+x0,
        y = y+y0,
      ) |>
      mutate(id = k)

  })

}

#' make callout
#'
#' @param xmin xmin
#' @param xmax xmax
#' @param ymin ymin
#' @param ymax ymax
#' @param pos pos
#' @param length Length
#' @param angle angle
#' @param rotate_at rotate at
make_callout <- function(xmin, xmax, ymin, ymax, pos = 0.3, length = 0.2, angle = 0, rotate_at = "center") {

  if(length(angle) != length(xmin)) angle <- rep(angle[1], length(xmin))

  map_dfr(1:length(xmin), function(k) {

    dx <- xmax[k]-xmin[k]
    dy <- ymax[k]-ymin[k]
    y0 <- ymin[k]+dy/2
    x0 <- switch(
      rotate_at,
      "head" = xmax[k],
      "center" = xmin[k]+dx/2
    )

    tibble(
      x = c(xmin[k], xmin[k], xmax[k], xmax[k], xmin[k]+dx*(pos+0.2), xmin[k]+dx*(pos+0.1), xmin[k]+dx*(pos))-x0,
      y = c(ymin[k], ymax[k], ymax[k], ymin[k], ymin[k], ymin[k]-dy*length, ymin[k])-y0
    ) |>
      rotate(angle[k]*pi/180) |>
      mutate(
        x = x+x0,
        y = y+y0,
      ) |>
      mutate(id = k)

  })

}

#' make triangle
#'
#' @param xmin xmin
#' @param xmax xmax
#' @param ymin ymin
#' @param ymax ymax
#' @param pos pos
#' @param length length
#' @param angle angle
#' @param rotate_at rotate at
make_triangle <- function(xmin, xmax, ymin, ymax, pos = 0.3, length = 0.2, angle = 0, rotate_at = "center") {

  if(length(angle) != length(xmin)) angle <- rep(angle[1], length(xmin))

  map_dfr(1:length(xmin), function(k) {

    dx <- xmax[k]-xmin[k]
    dy <- ymax[k]-ymin[k]
    y0 <- ymin[k]+dy/2
    x0 <- switch(
      rotate_at,
      "head" = xmax[k],
      "center" = xmin[k]+dx/2
    )

    tibble(
      x = c(xmin[k], xmin[k]+dx/2, xmax[k])-x0,
      y = c(ymin[k], ymax[k], ymin[k])-y0
    ) |>
      rotate(angle[k]*pi/180) |>
      mutate(
        x = x+x0,
        y = y+y0,
      ) |>
      mutate(id = k)

  })

}



#' make pentagon
#'
#' @param xmin xmin
#' @param xmax xmax
#' @param ymin ymin
#' @param ymax ymax
#' @param angle angle
#' @param rotate_at rotate at
make_pentagon <- function(xmin, xmax, ymin, ymax, angle = 0, rotate_at = "center") {

  if(length(angle) != length(xmin)) angle <- rep(angle[1], length(xmin))

  map_dfr(1:length(xmin), function(k) {

    dx <- xmax[k]-xmin[k]
    dy <- ymax[k]-ymin[k]
    y0 <- ymin[k]+dy/2
    x0 <- switch(
      rotate_at,
      "head" = xmax[k],
      "center" = xmin[k]+dx/2
    )

    tibble(
      x = c(xmin[k], xmin[k], xmax[k]-dy/2, xmax[k], xmax[k]-dy/2)-x0,
      y = c(ymin[k], ymax[k], ymax[k], ymin[k]+dy/2, ymin[k])-y0
    ) |>
      rotate(angle[k]*pi/180) |>
      mutate(
        x = x+x0,
        y = y+y0,
      ) |>
      mutate(id = k)

  })

}

#' make parallelogram
#'
#' @param xmin xmin
#' @param xmax xmax
#' @param ymin ymin
#' @param ymax ymax
#' @param angle angle
#' @param theta theta
#' @param rotate_at rotate at
make_parallelogram <- function(xmin, xmax, ymin, ymax, angle = 0, theta = 15, rotate_at = "center") {

  if(length(angle) != length(xmin)) angle <- rep(angle[1], length(xmin))

  map_dfr(1:length(xmin), function(k) {

    dx <- xmax[k]-xmin[k]
    dy <- ymax[k]-ymin[k]
    y0 <- ymin[k]+dy/2
    x0 <- switch(
      rotate_at,
      "head" = xmax[k],
      "center" = xmin[k]+dx/2
    )
    # beta <- tan(theta*pi/180)*dy
    beta <- cos((90-theta)*pi/180)*dy

    tibble(
      x = c(xmin[k], xmin[k]+beta, xmax[k], xmax[k]-beta)-x0,
      y = c(ymin[k], ymax[k], ymax[k], ymin[k])-y0
    ) |>
      rotate(angle[k]*pi/180) |>
      mutate(
        x = x+x0,
        y = y+y0,
      ) |>
      mutate(id = k)

  })

}


#' make diamond
#'
#' @param xmin xmin
#' @param xmax xmax
#' @param ymin ymin
#' @param ymax ymax
#' @param angle angle
#' @param rotate_at rotate at
make_diamond <- function(xmin, xmax, ymin, ymax, angle = 0, rotate_at = "center") {

  if(length(angle) != length(xmin)) angle <- rep(angle[1], length(xmin))

  map_dfr(1:length(xmin), function(k) {

    dx <- xmax[k]-xmin[k]
    dy <- ymax[k]-ymin[k]
    y0 <- ymin[k]+dy/2
    x0 <- switch(
      rotate_at,
      "head" = xmax[k],
      "center" = xmin[k]+dx/2
    )

    tibble(
      x = c(xmin[k], xmin[k]+dx/2, xmax[k], xmax[k]-dx/2)-x0,
      y = c(ymin[k]+dy/2, ymax[k], ymax[k]-dy/2, ymin[k])-y0
    ) |>
      rotate(angle[k]*pi/180) |>
      mutate(
        x = x+x0,
        y = y+y0,
      ) |>
      mutate(id = k)

  })

}

#' make elbow
#'
#' @param x x
#' @param y y
#' @param grp group
#' @param pos pos
make_elbow <- function(x, y, grp, pos = 0.5) {

  groups <- unique(grp)
  # if(length(groups) > 1) pos <- seq(0.25, 0.75, length = length(groups))
  pos <- rep(pos, length(groups))

  map_dfr(1:length(groups), function(grp_k) {

    ids <- which(grp == groups[grp_k])
    x0 <- x[ids]
    y0 <- y[ids]
    dx <- x0[2]-x0[1]
    dy <- y0[2]-y0[1]

    tibble(
      x = c(x0[1], x0[1]+pos[grp_k]*dx, x0[1]+pos[grp_k]*dx, x0[2]),
      y = c(y0[1], y0[1], y0[2], y0[2]),
      group = groups[grp_k]
    )

  })

}

#' make cross
#'
#' @param xmin xmin
#' @param xmax xmax
#' @param ymin ymin
#' @param ymax ymax
#' @param wd wd
#' @param angle angle
#' @param rotate_at rotate at
make_cross <- function(xmin, xmax, ymin, ymax, wd = 0.3, angle = 0, rotate_at = "center") {

  if(length(angle) != length(xmin)) angle <- rep(angle[1], length(xmin))

  map_dfr(1:length(xmin), function(k) {

    dx <- xmax[k]-xmin[k]
    dy <- ymax[k]-ymin[k]
    y0 <- ymin[k]+dy/2
    x0 <- switch(
      rotate_at,
      "head" = xmax[k],
      "center" = xmin[k]+dx/2
    )

    tibble(
      x = c(xmin[k], xmin[k], xmin[k]+dx*wd, xmin[k]+dx*wd, xmax[k]-dx*wd, xmax[k]-dx*wd, xmax[k], xmax[k],
            xmax[k]-dx*wd, xmax[k]-dx*wd, xmin[k]+dx*wd, xmin[k]+dx*wd)-x0,
      y = c(ymin[k]+dy*wd, ymax[k]-dy*wd, ymax[k]-dy*wd, ymax[k], ymax[k], ymax[k]-dy*wd, ymax[k]-dy*wd,
            ymin[k]+dy*wd, ymin[k]+dy*wd, ymin[k], ymin[k], ymin[k]+dy*wd)-y0
    ) |>
      rotate(angle[k]*pi/180) |>
      mutate(
        x = x+x0,
        y = y+y0,
      ) |>
      mutate(id = k)

  })

}
