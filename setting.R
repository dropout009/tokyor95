# 利用するライブラリや画像の設定など
library(tidymodels)
library(tidyverse)
library(metR)
library(scales)
library(glue)


# 画像の設定 ------------------------------------------------------------------

cols <- c(
  "#5D9CEC", "#4A89DC",
  "#48CFAD", "#37BC9B",
  "#FC6E51", "#E9573F",
  "#AAB2BD", "#656D78", "#434A54"
)


base_family <- "Noto Sans JP"
bold_family <- "Noto Sans JP Medium"

theme_line <- function() {
  theme_minimal(base_size = 12, base_family = base_family) %+replace%
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray", size = 0.1),
      panel.grid.major.x = element_blank(),
      axis.title = element_text(size = 15, color = cols[8]),
      axis.title.x = element_text(margin = margin(10, 0, 0, 0), hjust = 1),
      axis.title.y = element_text(margin = margin(0, 10, 0, 0), angle = 90, hjust = 1),
      axis.text = element_text(size = 12, color = cols[8]),
      axis.line.x.bottom = element_line(color = cols[8], size = 0.5),
      axis.ticks.x = element_line(color = cols[8], size = 0.5),
      axis.ticks.length.x = unit(5, units = "pt"),
      strip.text = element_text(size = 15, color = cols[8], margin = margin(5, 5, 5, 5)),
      plot.title = element_text(size = 15, color = cols[8]),
      # plot.subtitle = element_text(size = 15, hjust = -0.05, color = cols[8],
      #                              margin = margin(5, 5, 5, 5)),
      plot.title.position = "plot",
      legend.position = "none",
      legend.text = element_text(color = cols[8])
    )
}

theme_scatter <- function() {
  theme_minimal(base_size = 12, base_family = base_family) %+replace%
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray", size = 0.1),
      axis.title = element_text(size = 15, color = cols[8]),
      axis.title.x = element_text(margin = margin(10, 0, 0, 0), hjust = 1),
      axis.title.y = element_text(margin = margin(0, 10, 0, 0), angle = 90, hjust = 1),
      axis.text = element_text(size = 12, color = cols[8]),
      # axis.line.x.bottom = element_line(color = cols[8], size = 0.5),
      # axis.ticks.x = element_line(color = cols[8], size = 0.5),
      # axis.ticks.length.x = unit(5, units = "pt"),
      strip.text = element_text(size = 15, color = cols[8], margin = margin(5, 5, 5, 5)),
      plot.title = element_text(size = 15, color = cols[8], margin = margin(5, 0, 5, 0)),
      plot.subtitle = element_text(
        size = 15, hjust = -0.05, color = cols[8],
        margin = margin(5, 5, 5, 5)
      ),
      legend.position = "top",
      legend.text = element_text(color = cols[8])
    )
}


save_plot <- function(plot, fname, width = 6, height = 4) {
  ggsave(plot = plot, filename = fname, width = width, height = height, dpi = 272, device = "png")
}
