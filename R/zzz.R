#' OnLoad
#' @param libname Library name?
#' @param pkgname Package name
#' @export
#' @import sysfonts
#' @import utils
.onLoad <- function(libname, pkgname) {
  version = packageDescription(pkgname, fields = "Version")
  
  msg = paste0("========================================
", pkgname, " version ", version, "
A bunch of SciLicium themes and colors!

Github page: https://github.com/SciLicium/ggscilicium

This message can be suppressed by:
  suppressPackageStartupMessages(library(ggscilicium))
========================================
")
  
  packageStartupMessage(msg)
  
  if(!("titillium" %in% sysfonts::font_families())){
    sysfonts::font_add_google("Titillium Web", "titillium")
  }
  showtext::showtext_auto()
}
