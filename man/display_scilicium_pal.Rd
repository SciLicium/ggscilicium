% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/color_scilicium.R
\name{display_scilicium_pal}
\alias{display_scilicium_pal}
\title{Display a scilicium palette}
\usage{
display_scilicium_pal(name, n)
}
\arguments{
\item{name}{The name of the palette to use. Currently available palettes are "SciliciumLight" (default, 5 colors), "SciliciumDark" (5 colors), "SciliciumPaired" (8 colors), "KombuGreen" (5 colors) and "EarthYellow" (5 colors)}

\item{n}{An integer representing the number of colors in the palette}
}
\value{
A custom ggplot2 color scale
}
\description{
Display a given SciLicium palette
}
\examples{
display_scilicium_pal("DarkGreen", 5)
display_scilicium_pal("SciliciumPairedLight", 10)

}
\seealso{
Other palette vizualization functions: 
\code{\link{display_scilicium_all}()}
}
\concept{palette vizualization functions}
