#' ggscilicium: A package for custom SciLicium graphic theme
#'
#' The ggscilicium package provides qualitative and sequential palettes, as well as a theme for ggplot2.
#'
#' @section generic package functions:
#' The most important functions of this package are:
#' * \code{scilicium_pal(name, n)} Access a given palette, and return a vector of colors of length n. For qualitative palettes, if n is larger than the palette length, a warning will be issued and the resulting vector will be the max possible length. On the contrary, sequential palettes will be interpolated if n is larger than the palette length (7).
#' * \code{display_scilicium_pal(name, n)} Generate a plot of the requested palette with n elements.
#' * \code{display_scilicium_all()} Generate a plot of all available palettes!
#'
#' @section ggplot2 specific package functions:
#' The following functions are made to use with ggplot2:
#' * \code{scale_color_scilicium(name, n)} A wrapper for ggplot2 scale_color_manual(values=scilicium_pal(name, n)), to increase ease of use and code readability.
#' * \code{scale_fill_scilicium(name, n)} A wrapper for ggplot2 scale_fill_manual(values=scilicium_pal(name, n)), to increase ease of use and code readability.
#' * \code{theme_scilicium()} A ggplot2 theme
#'
#' @section Note on palettes:
#' * Qualitative palettes are a collection of colors, designed to be hopefully both nice and coherent. They all comprise the notorious 'SciLicium green' (#0dba13).
#' * Sequential palettes are ordered shade of colors , to use e.g. for representing different doses of a same compound. They are available in green, blue, red and ochre.
#'
#' @section package contributions and improvements:
#' To add a new palette, simply add it to the corresponding list at the beginning of color_scilicium.R, rebuild the package, and that's it!
#' To create a new theme, just add it to the theme_scilicium.R file (+rebuild).
#' Future improvements may include diverging palettes as well.
#'
#' @docType package
#' @name ggscilicium
NULL
#> NULL
