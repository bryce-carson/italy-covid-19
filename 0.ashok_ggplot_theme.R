geom_line <- function(..., linewidth = 1.2) ggplot2::geom_line(..., linewidth = linewidth)

ashokTheme <- 
  theme_classic() +
  theme(axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"))