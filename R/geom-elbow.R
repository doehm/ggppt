#' stat elbow
#'
#' @param geom Geom
#'
#' @rdname elbow
stat_elbow <- function(mapping = NULL, data = NULL,
                         geom = "line", position = "identity",
                         na.rm = FALSE, show.legend = NA,
                         inherit.aes = TRUE, pos = 0.5, ...) {
  layer(
    stat = StatElbow,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      pos = pos,
      na.rm = na.rm,
      ...
    )
  )
}

# StatElbow
StatElbow <- ggplot2::ggproto(
  "StatElbow",
  Stat,
  required_aes = c("x", "xend", "y", "yend"),
  setup_params = function(data, params) {
    return(params)
  },
  compute_panel = function(data, scales, pos = params$pos) {

    dat <- data |>
      arrange(yend) |>
      mutate(group = 1:n()) |>
      select(-y, -yend) |>
      pivot_longer(c(x, xend), values_to = "x") |>
      select(-name) |>
      bind_cols(
        data |>
          arrange(yend) |>
          select(y, yend) |>
          pivot_longer(c(y, yend), values_to = "y") |>
          select(-name)
      )

    dat_out <- make_elbow(dat$x, dat$y, pos = pos, grp = dat$group) |>
      left_join(
        dat |>
          select(-x, -y) |>
          distinct(),
        by = "group"
      )

    return(dat_out)
  }
)


#' GeomBrick
GeomElbow <- ggplot2::ggproto(
  "GeomElbow",
  GeomLine,
  default_aes = aes(
    colour = "black",
    fill = "grey90",
    linewidth = 0.5,
    linetype = 1,
    alpha = NA
  ),
  pos = 0.5
)

#' Polygon elbow
#'
#' Elow like in PowerPoint
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
#' @param pos Position to make the elbow as a percentage of the length of the
#' segment.
#' @param ... Dots
#'
#' @return Grob
#' @export
#'
#' @name elbow
#'
#' @examples
#' library(ggplot2)
#' library(dplyr)
geom_elbow <- function(mapping = NULL, data = NULL, stat = "elbow",
                       position = "identity", na.rm = FALSE,
                       show.legend = NA, inherit.aes = TRUE,
                       pos = 0.5, ...) {

  layer(
    geom = GeomLine,
    data = data,
    mapping = mapping,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      pos = pos,
      na.rm = na.rm,
      ...)
  )

}
