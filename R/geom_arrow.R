#' stat_arrow
#'
#' @param geom Geom
#'
#' @rdname arrow
stat_arrow <- function(mapping = NULL, data = NULL,
                       geom = "polygon", position = "identity",
                       na.rm = FALSE, show.legend = NA,
                       inherit.aes = TRUE, edge_length = 0.2,
                       head_length = 0.2, angle = 0, ...) {
  layer(
    stat = StatArrow,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      edge_length = edge_length,
      head_length = head_length,
      angle = angle,
      na.rm = na.rm,
      ...
    )
  )
}

# StatArrow
StatArrow <- ggplot2::ggproto(
  "StatArrow",
  Stat,
  required_aes = c("xmin", "xmax", "ymin", "ymax"),
  setup_params = function(data, params) {
    return(params)
  },
  compute_panel = function(data, scales, edge_length = params$edge_length,
                           head_length = params$head_length, angle = params$angle,
                           rotate_at = params$rotate_at) {

    dat_out <- make_arrow(
        data$xmin, data$xmax, data$ymin, data$ymax,
        edge_length = edge_length, head_length = head_length,
        angle = angle, rotate_at = rotate_at
      ) |>
      left_join(
        data |>
          mutate(id = row_number()) |>
          select(-xmin, -xmax, -ymin, -ymax),
        by = "id"
      ) |>
      mutate(group = id)

    return(dat_out)
  }
)


#' GeomBrick
GeomArrow <- ggplot2::ggproto(
  "GeomArrow",
  GeomPolygon,
  default_aes = aes(
    colour = "black",
    fill = "grey90",
    linewidth = 0.5,
    linetype = 1,
    alpha = NA
  ),
  edge_length = 0.2,
  head_length = 0.2,
  angle = 0,
  rotate_at = "head"
)

#' Polygon arrow
#'
#' Creates a polygon arrow. Default is facing right.
#'
#' @param mapping Set of aesthetic mappings created by [aes()]. If specified and
#'   `inherit.aes = TRUE` (the default), it is combined with the default mapping
#'   at the top level of the plot. You must supply `mapping` if there is no plot
#'   mapping.
#' @param data The data to be displayed in this layer. There are three
#'    options:
#'
#'    If `NULL`, the default, the data is inherited from the plot
#'    data as specified in the call to [ggplot()].
#'
#'    A `data.frame`, or other object, will override the plot
#'    data. All objects will be fortified to produce a data frame. See
#'    [fortify()] for which variables will be created.
#'
#'    A `function` will be called with a single argument,
#'    the plot data. The return value must be a `data.frame`, and
#'    will be used as the layer data. A `function` can be created
#'    from a `formula` (e.g. `~ head(.x, 10)`).
#' @param stat The statistical transformation to use on the data for this
#'    layer, either as a `ggproto` `Geom` subclass or as a string naming the
#'    stat stripped of the `stat_` prefix (e.g. `"count"` rather than
#'    `"stat_count"`)
#' @param position Position adjustment, either as a string naming the adjustment
#'   (e.g. `"jitter"` to use `position_jitter`), or the result of a call to a
#'   position adjustment function. Use the latter if you need to change the
#'   settings of the adjustment.
#' @param show.legend logical. Should this layer be included in the legends?
#'   `NA`, the default, includes if any aesthetics are mapped.
#'   `FALSE` never includes, and `TRUE` always includes.
#'   It can also be a named logical vector to finely select the aesthetics to
#'   display.
#' @param inherit.aes If `FALSE`, overrides the default aesthetics,
#'   rather than combining with them. This is most useful for helper functions
#'   that define both data and aesthetics and shouldn't inherit behaviour from
#'   the default plot specification, e.g. [borders()].
#' @param na.rm If `FALSE` removes `NA`s from the data.
#' @param angle Angle to rotate each pentagon.
#' @param rotate_at The point of rotation, either 'head' or 'center'.
#' @param head_length The length of the head of the arrow as a proportion of the
#' longest arrow e.g. if `head_length = 0.2` and the longest arrow is 10 units long
#' the head of the arrow will be 2 units for all arrows. This is to keep the arrow
#' head consistent.
#' @param edge_length The arrow edge length as a proportion of width. Larger values
#' means skinnier body. Values (0, 0.5).
#' @param ... Dots
#'
#' @import dplyr
#' @import ggplot2
#' @importFrom purrr map_dfr
#'
#' @return Grob
#' @export
#'
#' @name arrow
#'
#' @examples
#' library(ggplot2)
#' library(dplyr)
geom_arrow <- function(mapping = NULL, data = NULL, stat = "arrow",
                       position = "identity", na.rm = FALSE,
                       show.legend = NA, inherit.aes = TRUE,
                       edge_length = 0.2, head_length = 0.2,
                       angle = 0, rotate_at = "head", ...) {

  layer(
    geom = GeomArrow,
    data = data,
    mapping = mapping,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      edge_length = edge_length,
      head_length = head_length,
      angle = angle,
      rotate_at = rotate_at,
      na.rm = na.rm,
      ...)
  )

}

#' Right arrow
#' @rdname arrow
#' @export
geom_arrow_right <- function(mapping = NULL, data = NULL, stat = "arrow",
                       position = "identity", na.rm = FALSE,
                       show.legend = NA, inherit.aes = TRUE,
                       edge_length = 0.2, head_length = 0.2,
                       angle = 0, rotate_at = "head", ...) {

  layer(
    geom = GeomArrow,
    data = data,
    mapping = mapping,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      edge_length = edge_length,
      head_length = head_length,
      angle = angle,
      rotate_at = rotate_at,
      na.rm = na.rm,
      ...)
  )

}

#' Left arrow
#'
#' @rdname arrow
#'
#' @export
geom_arrow_left <- function(mapping = NULL, data = NULL, stat = "arrow",
                             position = "identity", na.rm = FALSE,
                             show.legend = NA, inherit.aes = TRUE,
                             edge_length = 0.2, head_length = 0.2,
                             angle = 0, rotate_at = "head", ...) {

  layer(
    geom = GeomArrow,
    data = data,
    mapping = mapping,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      edge_length = edge_length,
      head_length = head_length,
      angle = angle,
      rotate_at = rotate_at,
      na.rm = na.rm,
      ...)
  )

}

#' Up arrow
#'
#' @rdname arrow
#'
#' @export
geom_arrow_up <- function(mapping = NULL, data = NULL, stat = "arrow",
                            position = "identity", na.rm = FALSE,
                            show.legend = NA, inherit.aes = TRUE,
                            edge_length = 0.2, head_length = 0.2,
                            angle = 0, rotate_at = "head", ...) {

  layer(
    geom = GeomArrow,
    data = data,
    mapping = mapping,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      edge_length = edge_length,
      head_length = head_length,
      angle = angle,
      rotate_at = rotate_at,
      na.rm = na.rm,
      ...)
  )

}

#' Down arrow
#'
#' @rdname arrow
#'
#' @export
geom_arrow_down <- function(mapping = NULL, data = NULL, stat = "arrow",
                          position = "identity", na.rm = FALSE,
                          show.legend = NA, inherit.aes = TRUE,
                          edge_length = 0.2, head_length = 0.2,
                          angle = 0, rotate_at = "head", ...) {

  layer(
    geom = GeomArrow,
    data = data,
    mapping = mapping,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      edge_length = edge_length,
      head_length = head_length,
      angle = angle,
      rotate_at = rotate_at,
      na.rm = na.rm,
      ...)
  )

}
