theme_jkb <- function(base_size = 16, base_family = "sans",
                     base_line_size = base_size / 22,
                     base_rect_size = base_size / 22) {
  # Starts with theme_grey and then modify some parts
  theme_grey(
    base_size = base_size,
    base_family = base_family,
    base_line_size = base_line_size,
    base_rect_size = base_rect_size
  ) %+replace%
    theme(
      # transparent background
      panel.background = element_blank(),
      plot.background = element_blank(),
      # axes/grid: axis lines w/ ticks, gray gridlines
      panel.grid       = element_line(colour = "grey90"),
      panel.grid.major = element_line(size = rel(0.5)),
      panel.grid.minor = element_blank(),
      axis.line          = element_line(colour = "black", size = rel(0.5)),
      axis.ticks         = element_line(colour = "black", size = rel(0.5)),
      # strip titles: no background or border
      strip.background   = element_blank(),
      strip.text         = element_text(colour = "black"),
      # match legend key to background
      legend.key         = element_rect(fill = "white", colour = NA),

      complete = TRUE
    )
}
