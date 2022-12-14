#' Basic ggplot2 theme
#'
#' @importFrom ggplot2 %+replace%
#' @export

theme_op <- function(base_size = 12, base_family = "",
                     legend_position = "top",
                     show_axis = c("x", "xy", "y", "none"),
                     show_axis_text = c("xy", "x", "y", "none"),
                     show_grid = c("y", "xy", "x", "none"),
                     show_border = FALSE,
                     x_text_angle = 0,
                     x_text_hjust = NULL,
                     ...) {

  show_axis <- match.arg(show_axis)
  show_grid <- match.arg(show_grid)
  show_axis_text <- match.arg(show_axis_text)

  .theme <- ggplot2::theme_bw(base_size = base_size, base_family = base_family) %+replace%
    ggplot2::theme(
      plot.title.position = "plot",
      plot.caption.position = "plot",
      plot.title = ggplot2::element_text(face = "bold", hjust = 0, size = ggplot2::rel(1.2), margin = ggplot2::margin(b = 10)),
      plot.subtitle = ggplot2::element_text(color = "#7e7e7e", hjust = 0, margin = ggplot2::margin(b = 10)),
      plot.caption = ggplot2::element_text(color = "#7e7e7e", hjust = .1, face = "italic"),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(fill = NA, color = "#d4dddd"),
      axis.line = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      legend.key = ggplot2::element_blank(),
      strip.background = ggplot2::element_rect(fill = "#F2F2F2", color = "#d4dddd", size = 0.7),
      #plot.margin = margin(base_size / 2, base_size / 2, base_size / 2, base_size / 2),
      legend.position = legend_position,
      complete = TRUE
    )

  if (x_text_angle != 0 && grepl("x", show_axis)) {
    if (x_text_angle > 5 && is.null(x_text_hjust)) x_text_hjust <- 1

    .theme <- .theme + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = x_text_angle, hjust = x_text_hjust, color = "#4a4a4a"))
  }

  if (show_axis == "x") {
    .theme <- .theme +
      ggplot2::theme(
        axis.line.x = ggplot2::element_line(),
        axis.ticks.x = ggplot2::element_line(color = "#4a4a4a")
      )
  } else if (show_axis == "y") {
    .theme <- .theme +
      ggplot2::theme(
        axis.line.y = ggplot2::element_line(),
        axis.ticks.y = ggplot2::element_line(color = "#4a4a4a")
      )
  } else if (show_axis == "xy") {
    .theme <- .theme +
      ggplot2::theme(
        axis.line = ggplot2::element_line(),
        axis.ticks = ggplot2::element_line(color = "#4a4a4a")
      )
  }

  if (show_axis_text == "x") {
    .theme <- .theme +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(color = "#4a4a4a")
      )
  } else if (show_axis_text == "y") {
    .theme <- .theme +
      ggplot2::theme(
        axis.text.y = ggplot2::element_text(color = "#4a4a4a")
      )
  } else if (show_axis_text == "xy") {
    .theme <- .theme +
      ggplot2::theme(
        axis.text = ggplot2::element_text(color = "#4a4a4a")
      )
  }

  if (show_grid == "x") {
    .theme <- .theme +
      ggplot2::theme(panel.grid.major.x = ggplot2::element_line())
  } else if (show_grid == "y") {
    .theme <- .theme +
      ggplot2::theme(panel.grid.major.y = ggplot2::element_line())
  } else if (show_grid == "xy") {
    .theme <- .theme +
      ggplot2::theme(panel.grid.major = ggplot2::element_line())
  }

  if (!show_border) .theme <- .theme + ggplot2::theme(panel.border = ggplot2::element_blank())

  .theme <- .theme + ggplot2::theme(...)
  .theme
}

#' output a ggplot chart
#' @export

plot_save <- function(plt, file, width = 9, height = 6) {
  if (basename(file) == file) {
    file_path <- paste0(getwd(), "/fig/")
    if (!dir.exists(file_path)) dir.create(file_path)
    file <- file.path(file_path, file)
  }

  ggplot2::ggsave(file, plot = plt, width = width, height = height)
  (plt)
}

#' A selection of color palettes
#'
#' @export
#' @examples
#' op_palette() |> op_palette_plot()
#' op_palette(c(1, 9)) |> op_palette_plot()

op_palette <- function(x = NULL, name = c("gc", "economist")) {
  name <- match.arg(name)
  paletteList <- list(
    "gc" = c("#002e5c", "#1490ad", "#046f80", "#6dc0c4", "#00977d", "#009e3c", "#76949d", "#8ccbb8", "#768278", "#a6b29f"),
    "economist" = c("#01a2d9", "#014d64", "#6794a7", "#7ad2f6", "#00887d", "#76c0c1", "#7c260b", "#ee8f71", "#a18376", "#ecc265", "#f15a40")
  )

  paletteChosen <- paletteList[[name]]

  if (is.null(x)) x <- seq_along(paletteChosen)
  if (!is.numeric(x)) stop("x must be either NULL or a vector integer.")

  res <- paletteChosen[x]
  res
}

#' @rdname op_palette
#' @export
op_palette_plot <- function(p) {
  pDT <- data.table::data.table(Palette = p)[, xpos := .I]
  p <- pDT |>
    ggplot2::ggplot(ggplot2::aes(xpos, y = 1, fill = Palette)) +
    ggplot2::geom_tile(width = 1, height = .5) +
    ggplot2::geom_label(ggplot2::aes(label = xpos)) +
    ggplot2::scale_fill_identity() +
    ggplot2::labs(x = NULL, y = NULL) +
    theme_op(show_axis = "none", show_grid = "none", show_axis_text = "none")
  p
}

