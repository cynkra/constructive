# theme

    Code
      construct(ggplot2::theme_bw())
    Output
      ggplot2::theme_bw()
    Code
      construct(ggplot2::theme_bw(base_size = 11, base_line_size = 0.5))
    Output
      ggplot2::theme_bw()
    Code
      construct(ggplot2::theme_bw(base_size = 22, base_line_size = 0.5))
    Message
      {constructive} couldn't create code that reproduces perfectly the input
      i Call `construct_issues()` to inspect the last issues
    Output
      ggplot2::theme(
        line = ggplot2::element_line(
          colour = "black",
          linewidth = 0.5,
          linetype = 1,
          lineend = "butt",
          arrow = FALSE,
          inherit.blank = TRUE
        ),
        rect = ggplot2::element_rect(
          fill = "white",
          colour = "black",
          linewidth = 1,
          linetype = 1,
          inherit.blank = TRUE
        ),
        text = ggplot2::element_text(
          family = "",
          face = "plain",
          colour = "black",
          size = 22,
          hjust = 0.5,
          vjust = 0.5,
          angle = 0,
          lineheight = 0.9,
          margin = ggplot2::margin(t = 0, r = 0, b = 0, l = 0, unit = "points") |>
            structure(class = NULL),
          debug = FALSE,
          inherit.blank = TRUE
        ),
        title = NULL,
        aspect.ratio = NULL,
        axis.title = NULL,
        axis.title.x = ggplot2::element_text(
          vjust = 1,
          margin = ggplot2::margin(t = 5.5, r = 0, b = 0, l = 0, unit = "points") |>
            structure(class = NULL),
          inherit.blank = TRUE
        ),
        axis.title.x.top = ggplot2::element_text(
          vjust = 0,
          margin = ggplot2::margin(t = 0, r = 0, b = 5.5, l = 0, unit = "points") |>
            structure(class = NULL),
          inherit.blank = TRUE
        ),
        axis.title.x.bottom = NULL,
        axis.title.y = ggplot2::element_text(
          vjust = 1,
          angle = 90,
          margin = ggplot2::margin(t = 0, r = 5.5, b = 0, l = 0, unit = "points") |>
            structure(class = NULL),
          inherit.blank = TRUE
        ),
        axis.title.y.left = NULL,
        axis.title.y.right = ggplot2::element_text(
          vjust = 1,
          angle = -90,
          margin = ggplot2::margin(t = 0, r = 0, b = 0, l = 5.5, unit = "points") |>
            structure(class = NULL),
          inherit.blank = TRUE
        ),
        axis.text = ggplot2::element_text(colour = "grey30", size = ggplot2::rel(0.8), inherit.blank = TRUE),
        axis.text.x = ggplot2::element_text(
          vjust = 1,
          margin = ggplot2::margin(t = 4.4, r = 0, b = 0, l = 0, unit = "points") |>
            structure(class = NULL),
          inherit.blank = TRUE
        ),
        axis.text.x.top = ggplot2::element_text(
          vjust = 0,
          margin = ggplot2::margin(t = 0, r = 0, b = 4.4, l = 0, unit = "points") |>
            structure(class = NULL),
          inherit.blank = TRUE
        ),
        axis.text.x.bottom = NULL,
        axis.text.y = ggplot2::element_text(
          hjust = 1,
          margin = ggplot2::margin(t = 0, r = 4.4, b = 0, l = 0, unit = "points") |>
            structure(class = NULL),
          inherit.blank = TRUE
        ),
        axis.text.y.left = NULL,
        axis.text.y.right = ggplot2::element_text(
          hjust = 0,
          margin = ggplot2::margin(t = 0, r = 0, b = 0, l = 4.4, unit = "points") |>
            structure(class = NULL),
          inherit.blank = TRUE
        ),
        axis.text.theta = NULL,
        axis.text.r = ggplot2::element_text(
          hjust = 0.5,
          margin = ggplot2::margin(t = 0, r = 4.4, b = 0, l = 4.4, unit = "points") |>
            structure(class = NULL),
          inherit.blank = TRUE
        ),
        axis.ticks = ggplot2::element_line(colour = "grey20", arrow = FALSE, inherit.blank = TRUE),
        axis.ticks.x = NULL,
        axis.ticks.x.top = NULL,
        axis.ticks.x.bottom = NULL,
        axis.ticks.y = NULL,
        axis.ticks.y.left = NULL,
        axis.ticks.y.right = NULL,
        axis.ticks.theta = NULL,
        axis.ticks.r = NULL,
        axis.minor.ticks.x.top = NULL,
        axis.minor.ticks.x.bottom = NULL,
        axis.minor.ticks.y.left = NULL,
        axis.minor.ticks.y.right = NULL,
        axis.minor.ticks.theta = NULL,
        axis.minor.ticks.r = NULL,
        axis.ticks.length = grid::unit(5.5, units = "points") |>
          structure(class = NULL),
        axis.ticks.length.x = NULL,
        axis.ticks.length.x.top = NULL,
        axis.ticks.length.x.bottom = NULL,
        axis.ticks.length.y = NULL,
        axis.ticks.length.y.left = NULL,
        axis.ticks.length.y.right = NULL,
        axis.ticks.length.theta = NULL,
        axis.ticks.length.r = NULL,
        axis.minor.ticks.length = ggplot2::rel(0.75),
        axis.minor.ticks.length.x = NULL,
        axis.minor.ticks.length.x.top = NULL,
        axis.minor.ticks.length.x.bottom = NULL,
        axis.minor.ticks.length.y = NULL,
        axis.minor.ticks.length.y.left = NULL,
        axis.minor.ticks.length.y.right = NULL,
        axis.minor.ticks.length.theta = NULL,
        axis.minor.ticks.length.r = NULL,
        axis.line = ggplot2::element_blank(),
        axis.line.x = NULL,
        axis.line.x.top = NULL,
        axis.line.x.bottom = NULL,
        axis.line.y = NULL,
        axis.line.y.left = NULL,
        axis.line.y.right = NULL,
        axis.line.theta = NULL,
        axis.line.r = NULL,
        legend.background = ggplot2::element_rect(colour = NA, inherit.blank = TRUE),
        legend.margin = ggplot2::margin(t = 11, r = 11, b = 11, l = 11, unit = "points") |>
          structure(class = NULL),
        legend.spacing = grid::unit(22, units = "points") |>
          structure(class = NULL),
        legend.spacing.x = NULL,
        legend.spacing.y = NULL,
        legend.key = NULL,
        legend.key.size = grid::unit(1.2, units = "lines") |>
          structure(class = NULL),
        legend.key.height = NULL,
        legend.key.width = NULL,
        legend.key.spacing = grid::unit(11, units = "points") |>
          structure(class = NULL),
        legend.key.spacing.x = NULL,
        legend.key.spacing.y = NULL,
        legend.frame = NULL,
        legend.ticks = NULL,
        legend.ticks.length = ggplot2::rel(0.2),
        legend.axis.line = NULL,
        legend.text = ggplot2::element_text(size = ggplot2::rel(0.8), inherit.blank = TRUE),
        legend.text.position = NULL,
        legend.title = ggplot2::element_text(hjust = 0, inherit.blank = TRUE),
        legend.title.position = NULL,
        legend.position = "right",
        legend.position.inside = NULL,
        legend.direction = NULL,
        legend.byrow = NULL,
        legend.justification = "center",
        legend.justification.top = NULL,
        legend.justification.bottom = NULL,
        legend.justification.left = NULL,
        legend.justification.right = NULL,
        legend.justification.inside = NULL,
        legend.location = NULL,
        legend.box = NULL,
        legend.box.just = NULL,
        legend.box.margin = ggplot2::margin(t = 0, r = 0, b = 0, l = 0, unit = "cm") |>
          structure(class = NULL),
        legend.box.background = ggplot2::element_blank(),
        legend.box.spacing = grid::unit(22, units = "points") |>
          structure(class = NULL),
        panel.background = ggplot2::element_rect(fill = "white", colour = NA, inherit.blank = TRUE),
        panel.border = ggplot2::element_rect(fill = NA, colour = "grey20", inherit.blank = TRUE),
        panel.spacing = grid::unit(11, units = "points") |>
          structure(class = NULL),
        panel.spacing.x = NULL,
        panel.spacing.y = NULL,
        panel.grid = ggplot2::element_line(colour = "grey92", arrow = FALSE, inherit.blank = TRUE),
        panel.grid.major = NULL,
        panel.grid.minor = ggplot2::element_line(linewidth = ggplot2::rel(0.5), arrow = FALSE, inherit.blank = TRUE),
        panel.grid.major.x = NULL,
        panel.grid.major.y = NULL,
        panel.grid.minor.x = NULL,
        panel.grid.minor.y = NULL,
        panel.ontop = FALSE,
        plot.background = ggplot2::element_rect(colour = "white", inherit.blank = TRUE),
        plot.title = ggplot2::element_text(
          size = ggplot2::rel(1.2),
          hjust = 0,
          vjust = 1,
          margin = ggplot2::margin(t = 0, r = 0, b = 11, l = 0, unit = "points") |>
            structure(class = NULL),
          inherit.blank = TRUE
        ),
        plot.title.position = "panel",
        plot.subtitle = ggplot2::element_text(
          hjust = 0,
          vjust = 1,
          margin = ggplot2::margin(t = 0, r = 0, b = 11, l = 0, unit = "points") |>
            structure(class = NULL),
          inherit.blank = TRUE
        ),
        plot.caption = ggplot2::element_text(
          size = ggplot2::rel(0.8),
          hjust = 1,
          vjust = 1,
          margin = ggplot2::margin(t = 11, r = 0, b = 0, l = 0, unit = "points") |>
            structure(class = NULL),
          inherit.blank = TRUE
        ),
        plot.caption.position = "panel",
        plot.tag = ggplot2::element_text(size = ggplot2::rel(1.2), hjust = 0.5, vjust = 0.5, inherit.blank = TRUE),
        plot.tag.position = "topleft",
        plot.tag.location = NULL,
        plot.margin = ggplot2::margin(t = 11, r = 11, b = 11, l = 11, unit = "points") |>
          structure(class = NULL),
        strip.background = ggplot2::element_rect(fill = "grey85", colour = "grey20", inherit.blank = TRUE),
        strip.background.x = NULL,
        strip.background.y = NULL,
        strip.clip = "inherit",
        strip.placement = "inside",
        strip.text = ggplot2::element_text(
          colour = "grey10",
          size = ggplot2::rel(0.8),
          margin = ggplot2::margin(t = 8.8, r = 8.8, b = 8.8, l = 8.8, unit = "points") |>
            structure(class = NULL),
          inherit.blank = TRUE
        ),
        strip.text.x = NULL,
        strip.text.x.bottom = NULL,
        strip.text.x.top = NULL,
        strip.text.y = ggplot2::element_text(angle = -90, inherit.blank = TRUE),
        strip.text.y.left = ggplot2::element_text(angle = 90, inherit.blank = TRUE),
        strip.text.y.right = NULL,
        strip.switch.pad.grid = grid::unit(5.5, units = "points") |>
          structure(class = NULL),
        strip.switch.pad.wrap = grid::unit(5.5, units = "points") |>
          structure(class = NULL),
        complete = TRUE
      )
    Code
      construct(ggplot2::theme_bw(base_size = 11, base_line_size = 1))
    Output
      ggplot2::theme_bw(base_line_size = 1)
    Code
      construct(ggplot2::theme_bw(base_size = 22, base_line_size = 1))
    Message
      {constructive} couldn't create code that reproduces perfectly the input
      i Call `construct_issues()` to inspect the last issues
    Output
      ggplot2::theme(
        line = ggplot2::element_line(
          colour = "black",
          linewidth = 1,
          linetype = 1,
          lineend = "butt",
          arrow = FALSE,
          inherit.blank = TRUE
        ),
        rect = ggplot2::element_rect(
          fill = "white",
          colour = "black",
          linewidth = 1,
          linetype = 1,
          inherit.blank = TRUE
        ),
        text = ggplot2::element_text(
          family = "",
          face = "plain",
          colour = "black",
          size = 22,
          hjust = 0.5,
          vjust = 0.5,
          angle = 0,
          lineheight = 0.9,
          margin = ggplot2::margin(t = 0, r = 0, b = 0, l = 0, unit = "points") |>
            structure(class = NULL),
          debug = FALSE,
          inherit.blank = TRUE
        ),
        title = NULL,
        aspect.ratio = NULL,
        axis.title = NULL,
        axis.title.x = ggplot2::element_text(
          vjust = 1,
          margin = ggplot2::margin(t = 5.5, r = 0, b = 0, l = 0, unit = "points") |>
            structure(class = NULL),
          inherit.blank = TRUE
        ),
        axis.title.x.top = ggplot2::element_text(
          vjust = 0,
          margin = ggplot2::margin(t = 0, r = 0, b = 5.5, l = 0, unit = "points") |>
            structure(class = NULL),
          inherit.blank = TRUE
        ),
        axis.title.x.bottom = NULL,
        axis.title.y = ggplot2::element_text(
          vjust = 1,
          angle = 90,
          margin = ggplot2::margin(t = 0, r = 5.5, b = 0, l = 0, unit = "points") |>
            structure(class = NULL),
          inherit.blank = TRUE
        ),
        axis.title.y.left = NULL,
        axis.title.y.right = ggplot2::element_text(
          vjust = 1,
          angle = -90,
          margin = ggplot2::margin(t = 0, r = 0, b = 0, l = 5.5, unit = "points") |>
            structure(class = NULL),
          inherit.blank = TRUE
        ),
        axis.text = ggplot2::element_text(colour = "grey30", size = ggplot2::rel(0.8), inherit.blank = TRUE),
        axis.text.x = ggplot2::element_text(
          vjust = 1,
          margin = ggplot2::margin(t = 4.4, r = 0, b = 0, l = 0, unit = "points") |>
            structure(class = NULL),
          inherit.blank = TRUE
        ),
        axis.text.x.top = ggplot2::element_text(
          vjust = 0,
          margin = ggplot2::margin(t = 0, r = 0, b = 4.4, l = 0, unit = "points") |>
            structure(class = NULL),
          inherit.blank = TRUE
        ),
        axis.text.x.bottom = NULL,
        axis.text.y = ggplot2::element_text(
          hjust = 1,
          margin = ggplot2::margin(t = 0, r = 4.4, b = 0, l = 0, unit = "points") |>
            structure(class = NULL),
          inherit.blank = TRUE
        ),
        axis.text.y.left = NULL,
        axis.text.y.right = ggplot2::element_text(
          hjust = 0,
          margin = ggplot2::margin(t = 0, r = 0, b = 0, l = 4.4, unit = "points") |>
            structure(class = NULL),
          inherit.blank = TRUE
        ),
        axis.text.theta = NULL,
        axis.text.r = ggplot2::element_text(
          hjust = 0.5,
          margin = ggplot2::margin(t = 0, r = 4.4, b = 0, l = 4.4, unit = "points") |>
            structure(class = NULL),
          inherit.blank = TRUE
        ),
        axis.ticks = ggplot2::element_line(colour = "grey20", arrow = FALSE, inherit.blank = TRUE),
        axis.ticks.x = NULL,
        axis.ticks.x.top = NULL,
        axis.ticks.x.bottom = NULL,
        axis.ticks.y = NULL,
        axis.ticks.y.left = NULL,
        axis.ticks.y.right = NULL,
        axis.ticks.theta = NULL,
        axis.ticks.r = NULL,
        axis.minor.ticks.x.top = NULL,
        axis.minor.ticks.x.bottom = NULL,
        axis.minor.ticks.y.left = NULL,
        axis.minor.ticks.y.right = NULL,
        axis.minor.ticks.theta = NULL,
        axis.minor.ticks.r = NULL,
        axis.ticks.length = grid::unit(5.5, units = "points") |>
          structure(class = NULL),
        axis.ticks.length.x = NULL,
        axis.ticks.length.x.top = NULL,
        axis.ticks.length.x.bottom = NULL,
        axis.ticks.length.y = NULL,
        axis.ticks.length.y.left = NULL,
        axis.ticks.length.y.right = NULL,
        axis.ticks.length.theta = NULL,
        axis.ticks.length.r = NULL,
        axis.minor.ticks.length = ggplot2::rel(0.75),
        axis.minor.ticks.length.x = NULL,
        axis.minor.ticks.length.x.top = NULL,
        axis.minor.ticks.length.x.bottom = NULL,
        axis.minor.ticks.length.y = NULL,
        axis.minor.ticks.length.y.left = NULL,
        axis.minor.ticks.length.y.right = NULL,
        axis.minor.ticks.length.theta = NULL,
        axis.minor.ticks.length.r = NULL,
        axis.line = ggplot2::element_blank(),
        axis.line.x = NULL,
        axis.line.x.top = NULL,
        axis.line.x.bottom = NULL,
        axis.line.y = NULL,
        axis.line.y.left = NULL,
        axis.line.y.right = NULL,
        axis.line.theta = NULL,
        axis.line.r = NULL,
        legend.background = ggplot2::element_rect(colour = NA, inherit.blank = TRUE),
        legend.margin = ggplot2::margin(t = 11, r = 11, b = 11, l = 11, unit = "points") |>
          structure(class = NULL),
        legend.spacing = grid::unit(22, units = "points") |>
          structure(class = NULL),
        legend.spacing.x = NULL,
        legend.spacing.y = NULL,
        legend.key = NULL,
        legend.key.size = grid::unit(1.2, units = "lines") |>
          structure(class = NULL),
        legend.key.height = NULL,
        legend.key.width = NULL,
        legend.key.spacing = grid::unit(11, units = "points") |>
          structure(class = NULL),
        legend.key.spacing.x = NULL,
        legend.key.spacing.y = NULL,
        legend.frame = NULL,
        legend.ticks = NULL,
        legend.ticks.length = ggplot2::rel(0.2),
        legend.axis.line = NULL,
        legend.text = ggplot2::element_text(size = ggplot2::rel(0.8), inherit.blank = TRUE),
        legend.text.position = NULL,
        legend.title = ggplot2::element_text(hjust = 0, inherit.blank = TRUE),
        legend.title.position = NULL,
        legend.position = "right",
        legend.position.inside = NULL,
        legend.direction = NULL,
        legend.byrow = NULL,
        legend.justification = "center",
        legend.justification.top = NULL,
        legend.justification.bottom = NULL,
        legend.justification.left = NULL,
        legend.justification.right = NULL,
        legend.justification.inside = NULL,
        legend.location = NULL,
        legend.box = NULL,
        legend.box.just = NULL,
        legend.box.margin = ggplot2::margin(t = 0, r = 0, b = 0, l = 0, unit = "cm") |>
          structure(class = NULL),
        legend.box.background = ggplot2::element_blank(),
        legend.box.spacing = grid::unit(22, units = "points") |>
          structure(class = NULL),
        panel.background = ggplot2::element_rect(fill = "white", colour = NA, inherit.blank = TRUE),
        panel.border = ggplot2::element_rect(fill = NA, colour = "grey20", inherit.blank = TRUE),
        panel.spacing = grid::unit(11, units = "points") |>
          structure(class = NULL),
        panel.spacing.x = NULL,
        panel.spacing.y = NULL,
        panel.grid = ggplot2::element_line(colour = "grey92", arrow = FALSE, inherit.blank = TRUE),
        panel.grid.major = NULL,
        panel.grid.minor = ggplot2::element_line(linewidth = ggplot2::rel(0.5), arrow = FALSE, inherit.blank = TRUE),
        panel.grid.major.x = NULL,
        panel.grid.major.y = NULL,
        panel.grid.minor.x = NULL,
        panel.grid.minor.y = NULL,
        panel.ontop = FALSE,
        plot.background = ggplot2::element_rect(colour = "white", inherit.blank = TRUE),
        plot.title = ggplot2::element_text(
          size = ggplot2::rel(1.2),
          hjust = 0,
          vjust = 1,
          margin = ggplot2::margin(t = 0, r = 0, b = 11, l = 0, unit = "points") |>
            structure(class = NULL),
          inherit.blank = TRUE
        ),
        plot.title.position = "panel",
        plot.subtitle = ggplot2::element_text(
          hjust = 0,
          vjust = 1,
          margin = ggplot2::margin(t = 0, r = 0, b = 11, l = 0, unit = "points") |>
            structure(class = NULL),
          inherit.blank = TRUE
        ),
        plot.caption = ggplot2::element_text(
          size = ggplot2::rel(0.8),
          hjust = 1,
          vjust = 1,
          margin = ggplot2::margin(t = 11, r = 0, b = 0, l = 0, unit = "points") |>
            structure(class = NULL),
          inherit.blank = TRUE
        ),
        plot.caption.position = "panel",
        plot.tag = ggplot2::element_text(size = ggplot2::rel(1.2), hjust = 0.5, vjust = 0.5, inherit.blank = TRUE),
        plot.tag.position = "topleft",
        plot.tag.location = NULL,
        plot.margin = ggplot2::margin(t = 11, r = 11, b = 11, l = 11, unit = "points") |>
          structure(class = NULL),
        strip.background = ggplot2::element_rect(fill = "grey85", colour = "grey20", inherit.blank = TRUE),
        strip.background.x = NULL,
        strip.background.y = NULL,
        strip.clip = "inherit",
        strip.placement = "inside",
        strip.text = ggplot2::element_text(
          colour = "grey10",
          size = ggplot2::rel(0.8),
          margin = ggplot2::margin(t = 8.8, r = 8.8, b = 8.8, l = 8.8, unit = "points") |>
            structure(class = NULL),
          inherit.blank = TRUE
        ),
        strip.text.x = NULL,
        strip.text.x.bottom = NULL,
        strip.text.x.top = NULL,
        strip.text.y = ggplot2::element_text(angle = -90, inherit.blank = TRUE),
        strip.text.y.left = ggplot2::element_text(angle = 90, inherit.blank = TRUE),
        strip.text.y.right = NULL,
        strip.switch.pad.grid = grid::unit(5.5, units = "points") |>
          structure(class = NULL),
        strip.switch.pad.wrap = grid::unit(5.5, units = "points") |>
          structure(class = NULL),
        complete = TRUE
      )
    Code
      construct(ggplot2::theme())
    Output
      ggplot2::theme()
    Code
      construct(ggplot2::theme(axis.title = "foo"))
    Output
      ggplot2::theme(axis.title = "foo")

