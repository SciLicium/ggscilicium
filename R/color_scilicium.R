scilicium_palettes <- list(
  SciliciumLight  = c("#0DBA13", "#39A0ED", "#466365", "#CAC4CE", "#F7ECE1"), #https://coolors.co/0dba13-466365-39a0ed-cac4ce-f7ece1
  SciliciumDark   = c("#08750C", "#1067AB", "#2D3F40", "#81738A", "#D39558"), #https://coolors.co/08750c-2d3f40-1067ab-81738a-d39558
  SciliciumPaired = c("#0DBA13", "#08750C", "#39A0ED", "#1067AB", "#CAC4CE", "#81738A", "#F7ECE1", "#D39558"),
  KombuGreen      = c("#0DBA13", "#283618", "#93827F", "#E0D2C3", "#B4D4EE"), #https://coolors.co/0dba13-283618-93827f-e0d2c3-b4d4ee
  EarthYellow     = c("#0DBA13", "#283618", "#51BBFE", "#DDA15E", "#FEFAE0")#https://coolors.co/0dba13-283618-fefae0-dda15e-51bbfe
)


#' SciLicium scale color
#'
#' @param theme A theme to use
#' @param pal_key A list of palettes
#' @return A custom ggplot2 color scale
#'
#'
#' @export
#' @title SciLicium scale color for ggplot2
#' @examples
#' scale_color_scilicium(theme="KombuGreen")

scale_color_scilicium <- function(theme="SciliciumLight", pal_key = scilicium_palettes) {
  ggplot2::scale_color_manual(values=pal_key[[theme]])
}


#' SciLicium scale fill
#'
#' @param theme A theme to use
#' @param pal_key A list of palettes
#' @return A custom ggplot2 color scale
#'
#' @export
#' @title SciLicium scale fill for ggplot2
#' @examples
#' scale_fill_scilicium(theme="KombuGreen")


scale_fill_scilicium <- function(theme="SciliciumLight", pal_key = scilicium_palettes) {
  ggplot2::scale_fill_manual(values=pal_key[[theme]])
}


# gradients are defined from darker to lighter shades
scilicium_palettes_gradients <- list(
  green = list(
    light = list(
      c("#0dba13"),
      c("#064406", "#0dba13"), #dark
      c("#064406", "#0dba13", "#c6fbc7"),
      c("#064406", "#0dba13", "#7ff582", "#d4fcd6"),
      c("#010901", "#096209", "#0dba13", "#7ff582", "#d4fcd6"),
      c("#010901", "#096209", "#0db012", "#10ea18", "#aef9b0", "#e2fde3"),
      c("#000000", "#045806", "#08910d", "#0dc913", "#34ef3c", "#b3f9b5", "#e2fde3")
    ),
    dark = list(
      c("#0dba13"),
      c("#0dba13", "#c6fbc7"), #light
      c("#064406", "#0dba13", "#c6fbc7"),
      c("#064406", "#0dba13", "#7ff582", "#d4fcd6"),
      c("#010901", "#096209", "#0dba13", "#7ff582", "#d4fcd6"),
      c("#010901", "#096209", "#0db012", "#10ea18", "#aef9b0", "#e2fde3"),
      c("#000000", "#045806", "#08910d", "#0dc913", "#34ef3c", "#b3f9b5", "#e2fde3")
    )
  ),
  blue = list(
    light = list(
      c("#39a0ed"),
      c("#39a0ed", "#abd7f7"), #light
      c("#093c63", "#39a0ed", "#abd7f7"),
      c("#093c63", "#39a0ed", "#91cbf5", "#deeffc"),
      c("#06263f", "#0b4c7d", "#39a0ed", "#91cbf5", "#deeffc"),
      c("#010509", "#093b62", "#0c62a1", "#44a5ee", "#91cbf5", "#deeffc"),
      c("#010509", "#093b62", "#0c62a1", "#2097ee", "#68b9f3", "#acd8f6", "#deeffc")
    ),
    dark = list(
      c("#39a0ed"),
      c("#093c63", "#39a0ed"), # dark
      c("#093c63", "#39a0ed", "#abd7f7"),
      c("#093c63", "#39a0ed", "#91cbf5", "#deeffc"),
      c("#06263f", "#0b4c7d", "#39a0ed", "#91cbf5", "#deeffc"),
      c("#010509", "#093b62", "#0c62a1", "#44a5ee", "#91cbf5", "#deeffc"),
      c("#010509", "#093b62", "#0c62a1", "#2097ee", "#68b9f3", "#acd8f6", "#deeffc")
    )
  ),
  red = list(
    light = list(
      c("#dc394a"),
      c("#dc394a", "#fac0ba"), #light
      c("#500f1a", "#dc394a", "#fbc1ba"),
      c("#5f121e", "#dc394a", "#f39288", "#fed6d2"),
      c("#1b0408", "#861a2b", "#dc394a", "#f39288", "#fed6d2"),
      c("#1b0408", "#6d1221", "#b5263c", "#e65f60", "#f4978d", "#fed6d2"),
      c("#1b0408", "#60101d", "#a92338", "#dc394a", "#ed7c71", "#f9a89f", "#fed6d2")
    ),
    dark = list(
      c("#dc394a"),
      c("#4f0f1a", "#dc394a"), #dark
      c("#500f1a", "#dc394a", "#fbc1ba"),
      c("#5f121e", "#dc394a", "#f39288", "#fed6d2"),
      c("#1b0408", "#861a2b", "#dc394a", "#f39288", "#fed6d2"),
      c("#1b0408", "#6d1221", "#b5263c", "#e65f60", "#f4978d", "#fed6d2"),
      c("#1b0408", "#60101d", "#a92338", "#dc394a", "#ed7c71", "#f9a89f", "#fed6d2")
    )
  ),
  ocre = list(
    light = list(
      c("#dda15e"),
      c("#583712", "#dda15e"), #dark
      c("#583713", "#dda15e", "#f4ddc3"),
      c("#321f0b", "#9d6321", "#dda15e", "#f8ede2"),
      c("#321f0b", "#9d6321", "#dda15e", "#ebc8a1", "#f9eee3"),
      c("#321f0b", "#7a4d1a", "#bb8446", "#ebb375", "#f4d6b3", "#fbf0e5"),
      c("#321f0b", "#7a4d1a", "#a87438", "#dda15e", "#f9c58b", "#f2dabf", "#faefe4")
    ),
    dark = list(
      c("#dda15e"),
      c("#dda15e", "#f4ddc3"), #light
      c("#583713", "#dda15e", "#f4ddc3"),
      c("#321f0b", "#9d6321", "#dda15e", "#f8ede2"),
      c("#321f0b", "#9d6321", "#dda15e", "#ebc8a1", "#f9eee3"),
      c("#321f0b", "#7a4d1a", "#bb8446", "#ebb375", "#f4d6b3", "#fbf0e5"),
      c("#321f0b", "#7a4d1a", "#a87438", "#dda15e", "#f9c58b", "#f2dabf", "#faefe4")
    )
  )
)




scilicium_gradients <- function(n = 5, color=c("green", "red", "blue", "ocre"), shade = c("light", "dark")) {
  if (missing(n)) {
    stop("Number of colors is missing.")
  }
  if(n <= 0) {
    stop("n must be > 0.")
  }

  shade <- match.arg(shade)
  if (shade != "light" && shade != "dark") {
    stop("shade must be either 'light' or 'dark'")
  }

  color <- match.arg(color)
  if(!(color %in% c("green", "red", "blue", "ocre"))) {
    stop("color must be either 'green'")
  }

  if(n <= 7){
    col_pal <- scilicium_palettes_gradients[[color]][[shade]][[n]]
  } else {
    col_pal <- grDevices::colorRampPalette(scilicium_palettes_gradients[[color]][[shade]][[6]])(n)
  }
  col_pal
}



#' SciLicium gradient colors
#'
#' @param n An integer representing the number of colors in the gradient
#' @param color A color to use (currently only "green", "blue", "red" and "ocre" are supported)
#' @param shade A shade to use ("light" or "dark")
#' @return A custom ggplot2 color scale
#'
#' @export
#' @title SciLicium scale gradient color for ggplot2
#' @examples
#' scale_color_scilicium_gradients(10, "green", "dark")
#' @examples
#' library(ggplot2)
#' n_category <- 5
#' n_replicates <- 5
#' tmp_df <- data.frame(x = runif(n_replicates * n_category),
#'   y=runif(n_replicates * n_category),
#'   category=letters[rep(1:n_category, n_replicates)])
#' gg <- ggplot(tmp_df, aes(x=x, y=y, col=category)) +
#'   geom_point() +
#'   scale_color_scilicium_gradients(5, "red", "dark") +
#'   theme_scilicium()
#'

scale_color_scilicium_gradients <- function(n = 5, color="green", shade = c("light", "dark")) {
  ggplot2::scale_color_manual(values=scilicium_gradients(n, color, shade))
}


#' SciLicium gradient colors
#'
#' @param n An integer representing the number of colors in the gradient
#' @param color A color to use (currently only "green" is supported)
#' @param shade A shade to use ("light" or "dark")
#' @return A custom ggplot2 color scale
#'
#' @export
#' @title SciLicium scale gradient fill for ggplot2
#' @examples
#' scale_fill_scilicium_gradients(10, "green", "dark")
#' @examples
#' library(ggplot2)
#' n_category <- 5
#' n_replicates <- 5
#' tmp_df <- data.frame(x = runif(n_replicates * n_category),
#'   y=runif(n_replicates * n_category),
#'   category=letters[rep(1:n_category, n_replicates)])
#' gg <- ggplot(tmp_df, aes(x=category, y=y, fill=category)) +
#'   geom_boxplot() +
#'   scale_fill_scilicium_gradients(5, "red", "dark") +
#'   theme_scilicium()
#'

scale_fill_scilicium_gradients <- function(n = 5, color="green", shade = c("light", "dark")) {
  ggplot2::scale_fill_manual(values=scilicium_gradients(n, color, shade))
}
