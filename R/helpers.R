# helpers
make_arrow <- function(xmin, xmax, ymin, ymax, edge_length = 0.2, body_length = 0.66, angle = 0, rotate_at = "center") {

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
      x = c(xmin[k], xmin[k], xmin[k]+dx*body_length,
            xmin[k]+dx*body_length, xmax[k], xmin[k]+dx*body_length,
            xmin[k]+dx*body_length, xmin[k])-x0,
      y = c(ymin[k]+dy*edge_length, ymax[k]-dy*edge_length,
            ymax[k]-dy*edge_length, ymax[k], ymin[k]+dy/2, ymin[k],
            ymin[k]+dy*edge_length, ymin[k]+dy*edge_length)-y0
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
make_parallelogram <- function(xmin, xmax, ymin, ymax, angle = 0, theta = 30, rotate_at = "center") {

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
    beta <- tan(theta*pi/180)*dy

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
