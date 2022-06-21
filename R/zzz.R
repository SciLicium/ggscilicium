#' OnLoad
#' @export
#' @import sysfonts
.onLoad <- function(libname, pkgname) {
  packageStartupMessage("A bunch of SciLicium themes and colors!")

  if("titillium" %in% sysfonts::font_families()){
    sysfonts::font_add_google("Titillium Web", "titillium")
  }
  showtext::showtext_auto()
}
