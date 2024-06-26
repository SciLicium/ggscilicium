

#' @title Scilicium theme for ggplot2
#'
#' @description
#' Function to use the ggplot2 SciLicium theme.
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#' data("midwest", package = "ggplot2")
#' # midwest <- read.csv("http://goo.gl/G1K41K")  # bkup data source
#'
#' # Scatterplot
#' gg <- ggplot(midwest, aes(x=area, y=poptotal)) +
#'   geom_point(aes(col=state, size=popdensity)) +
#'   geom_smooth(method="loess", se=FALSE) +
#'   xlim(c(0, 0.1)) +
#'   ylim(c(0, 500000)) +
#'   labs(subtitle="Area Vs Population",
#'        y="Population",
#'        x="Area",
#'        title="Scatterplot",
#'        caption = "Source: midwest")
#'
#' gg
#'
#' ggsci <- gg + scale_color_scilicium() + theme_scilicium()
#' ggsci

theme_scilicium <- function() {
  scilicium_params = list(
    title_family="titillium",
    title_colour = "#0DBA13",
    subtitle_family="titillium",
    subtitle_colour = "#0DBA13",
    caption_family="titillium",
    caption_colour = "#cccccc",
    text_family = "sans",
    text_colour = "#1A1A1A")

  ggplot2::theme_minimal() +
    ggplot2::theme(text=ggplot2::element_text(size=18, family=scilicium_params$text_family)) +
    # theme(legend.title=element_blank()) +
    ggplot2::theme(plot.title = ggplot2::element_text(colour = scilicium_params$title_colour,
                                                      family=scilicium_params$title_family)) +
    ggplot2::theme(plot.subtitle = ggplot2::element_text(colour = scilicium_params$subtitle_colour,
                                                         family=scilicium_params$subtitle_family)) +
    ggplot2::theme(axis.text.x=ggplot2::element_text(color=scilicium_params$text_colour)) +
    ggplot2::theme(axis.text.y=ggplot2::element_text(color=scilicium_params$text_colour)) +
    ggplot2::theme(axis.title.x=ggplot2::element_text(color=scilicium_params$text_colour)) +
    ggplot2::theme(axis.title.y=ggplot2::element_text(color=scilicium_params$text_colour)) +
    ggplot2::theme(axis.line.x = ggplot2::element_line(color=scilicium_params$text_colour)) +
    ggplot2::theme(axis.line.y = ggplot2::element_line(color=scilicium_params$text_colour)) +
    ggplot2::theme(axis.ticks.x = ggplot2::element_line(color=scilicium_params$text_colour)) +
    ggplot2::theme(axis.ticks.y = ggplot2::element_line(color=scilicium_params$text_colour)) +
    ggplot2::theme(plot.caption = ggplot2::element_text(colour = scilicium_params$caption_colour,
                                                        family=scilicium_params$caption_family,
                                                        size=ggplot2::rel(0.6)))

}


#' @title Scilicium/genolens theme for ggplot2
#'
#' @description
#' Function to use the ggplot2 SciLicium genolens theme.
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#' data("midwest", package = "ggplot2")
#' # midwest <- read.csv("http://goo.gl/G1K41K")  # bkup data source
#'
#' # Scatterplot
#' gg <- ggplot(midwest, aes(x=area, y=poptotal)) +
#'   geom_point(aes(col=state, size=popdensity)) +
#'   geom_smooth(method="loess", se=FALSE) +
#'   xlim(c(0, 0.1)) +
#'   ylim(c(0, 500000)) +
#'   labs(subtitle="Area Vs Population",
#'        y="Population",
#'        x="Area",
#'        title="Scatterplot",
#'        caption = "Source: midwest")
#'
#' gg
#'
#' ggsci <- gg + scale_color_scilicium("GenolensLong1") + theme_genolens()
#' ggsci

theme_genolens <- function() {
  scilicium_params = list(
    title_family="titillium",
    title_colour = "#0b2a3e",
    subtitle_family="titillium",
    subtitle_colour = "#30415f",
    caption_family="titillium",
    caption_colour = "#41577d",
    text_family = "sans",
    text_colour = "#0b2a3e")

  ggplot2::theme_minimal() +
    ggplot2::theme(text=ggplot2::element_text(size=18, family=scilicium_params$text_family)) +
    # theme(legend.title=element_blank()) +
    ggplot2::theme(plot.title = ggplot2::element_text(colour = scilicium_params$title_colour,
                                                      family=scilicium_params$title_family)) +
    ggplot2::theme(plot.subtitle = ggplot2::element_text(colour = scilicium_params$subtitle_colour,
                                                         family=scilicium_params$subtitle_family)) +
    ggplot2::theme(axis.text.x=ggplot2::element_text(color=scilicium_params$text_colour)) +
    ggplot2::theme(axis.text.y=ggplot2::element_text(color=scilicium_params$text_colour)) +
    ggplot2::theme(axis.title.x=ggplot2::element_text(color=scilicium_params$text_colour)) +
    ggplot2::theme(axis.title.y=ggplot2::element_text(color=scilicium_params$text_colour)) +
    ggplot2::theme(axis.line.x = ggplot2::element_line(color=scilicium_params$text_colour)) +
    ggplot2::theme(axis.line.y = ggplot2::element_line(color=scilicium_params$text_colour)) +
    ggplot2::theme(axis.ticks.x = ggplot2::element_line(color=scilicium_params$text_colour)) +
    ggplot2::theme(axis.ticks.y = ggplot2::element_line(color=scilicium_params$text_colour)) +
    ggplot2::theme(plot.caption = ggplot2::element_text(colour = scilicium_params$caption_colour,
                                                        family=scilicium_params$caption_family,
                                                        size=ggplot2::rel(0.6)))

}
