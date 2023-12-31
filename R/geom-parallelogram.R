#' stat parallelogram
#'
#' @param geom Geom
#'
#' @rdname parallelogram
stat_parallelogram <- function(mapping = NULL, data = NULL,
                         geom = "polygon", position = "identity",
                         na.rm = FALSE, show.legend = NA,
                         inherit.aes = TRUE, angle = 0,
                         rotate_at = "center", ...) {
  layer(
    stat = StatParallelogram,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      angle = angle,
      theta = theta,
      rotate_at = rotate_at,
      na.rm = na.rm,
      ...
    )
  )
}

# StatParallelogram
StatParallelogram <- ggplot2::ggproto(
  "StatParallelogram",
  Stat,
  required_aes = c("xmin", "xmax", "ymin", "ymax"),
  setup_params = function(data, params) {
    return(params)
  },
  compute_panel = function(data, scales, angle = params$angle, rotate_at = params$rotate_at,
                           theta = params$theta) {

    dat_out <- make_parallelogram(
      data$xmin, data$xmax, data$ymin, data$ymax,
      angle = angle, rotate_at = rotate_at,
      theta = theta
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
GeomParallelogram <- ggplot2::ggproto(
  "GeomParallelogram",
  GeomPolygon,
  default_aes = aes(
    colour = "black",
    fill = "grey90",
    linewidth = 0.5,
    linetype = 1,
    alpha = NA
  ),
  angle = 0,
  theta = 15,
  rotate_at = "center"
)

#' Polygon parallelogram
#'
#' Creates a parllelogram like in PowerPoint
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
#' @param angle Angle to rotate each callout box.
#' @param rotate_at The point of rotation, either 'head' or 'center'.
#' @param theta Angle of the slant.
#' @param ... dots
#'
#' @return Grob
#' @export
#'
#' @name parallelogram
#'
#' @examples
#' library(ggplot2)
#' library(dplyr)
geom_parallelogram <- function(mapping = NULL, data = NULL, stat = "parallelogram",
                         position = "identity", na.rm = FALSE,
                         show.legend = NA, inherit.aes = TRUE,
                         angle = 0, rotate_at = "center", theta = 15, ...) {

  layer(
    geom = GeomParallelogram,
    data = data,
    mapping = mapping,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      angle = angle,
      theta = theta,
      rotate_at = rotate_at,
      na.rm = na.rm,
      ...)
  )

}

