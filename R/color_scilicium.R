
scilicium_qual_palettes <- list(
  GenolensLong1       = c("#45c2a9", "#5d309c", "#33a8e3", "#ff5f5f", "#fec601", "#30415f", "#c9ada7", 
                      "#beeae1", "#c6b7dc", "#b8e1f5", "#ffc7c7", "#ffeba6", "#ece2e0"),
  GenolensPaired1       = c("#45c2a9", "#beeae1",
                            "#5d309c", "#c6b7dc",
                            "#33a8e3", "#b8e1f5",
                            "#ff5f5f", "#ffc7c7",
                            "#fec601", "#ffeba6",
                            "#30415f", "#ece2e0"),
  SciliciumLight  = c("#0DBA13", "#39A0ED", "#466365", "#CAC4CE", "#F7ECE1"), #https://coolors.co/0dba13-466365-39a0ed-cac4ce-f7ece1
  SciliciumDark   = c("#08750C", "#1067AB", "#2D3F40", "#81738A", "#D39558"), #https://coolors.co/08750c-2d3f40-1067ab-81738a-d39558
  SciliciumPaired2 = c("#0DBA13", "#08750C", "#39A0ED", "#1067AB", "#CAC4CE", "#81738A", "#F7ECE1", "#D39558"),
  SciliciumPairedLight = c("#0DBA13", "#c6fbc7",
                           "#39A0ED", "#abd7f7",
                           "#dc394a", "#fac0ba",
                           "#dda15e", "#f4ddc3",
                           "#a85edd", "#e0c3f4"),
  SciliciumPairedDark = c("#0DBA13", "#086408",
                          "#39A0ED", "#0c4d7c",
                          "#dc394a", "#6c1524",
                          "#dda15e", "#5e3b14",
                          "#a85edd", "#551b7e"),
  SciliciumLong = c("#0DBA13", "#39A0ED", "#dc394a", "#dda15e", "#a85edd",
                    "#c6fbc7","#abd7f7", "#fac0ba", "#f4ddc3", "#e0c3f4",
                    "#086408", "#0c4d7c", "#6c1524",  "#5e3b14","#551b7e"),
  SciliciumTrio = c("#0DBA13", "#c6fbc7", "#086408",
                    "#39A0ED", "#abd7f7", "#0c4d7c",
                    "#dc394a", "#fac0ba", "#6c1524",
                    "#dda15e", "#f4ddc3", "#5e3b14",
                    "#a85edd", "#e0c3f4", "#551b7e"),
  KombuGreen      = c("#0DBA13", "#283618", "#93827F", "#E0D2C3", "#B4D4EE"), #https://coolors.co/0dba13-283618-93827f-e0d2c3-b4d4ee
  Midnight      = c("#0DBA13", "#39A0ED", "#114B5F", "#E4FDE1", "#F45B69"), #https://coolors.co/0dba13-39a0ed-114b5f-e4fde1-f45b69
  Vivid      = c("#0DBA13", "#39A0ED", "#F6511D", "#FFB400", "#CA1551", "#0D2C54", "#9B5DE5", "#F15BB5", "#136F63", "#732C2C", "#B9E28C", "#F0F7F4", "#DDA15E"), #https://coolors.co/0dba13-39a0ed-f6511d-ffb400-0d2c54

  EarthYellow     = c("#0DBA13", "#283618", "#51BBFE", "#DDA15E", "#FEFAE0"), #https://coolors.co/0dba13-283618-fefae0-dda15e-51bbfe
  NewSciliciumPal1 = c("#42E2BA", "#2E294E", "#FFC43D", "#EF476F", "#F8FFE5"), #https://coolors.co/42e2ba-2e294e-ffc43d-ef476f-f8ffe5
  NewSciliciumPal2 = c("#42E2BA", "#0D3B66", "#F4D35E", "#FF9E4A", "#F95738"),
  NewSciliciumPal3 = c("#42E2BA", "#297373", "#39393A", "#CF5C36", "#EFD0A4"), #https://coolors.co/42e2ba-2e294e-ffc43d-ef476f-f8ffe5
  NewSciliciumPal4 = c("#42E2BA", "#297373", "#2DC7FF", "#EAD2AC", "#EABA6B") #https://coolors.co/42e2ba-297373-2dc7ff-ead2ac-eaba6b
)

# sequential palettes (or gradients) are defined from darker to lighter shades
scilicium_sequential_palettes <- list(
  DarkGreen = list(
      c("#0dba13"),
      c("#086408", "#0dba13"), #dark
      c("#086408", "#0dba13", "#c6fbc7"),
      c("#086408", "#0dba13", "#7ff582", "#d4fcd6"),
      c("#010901", "#096209", "#0dba13", "#7ff582", "#d4fcd6"),
      c("#010901", "#096209", "#0db012", "#10ea18", "#aef9b0", "#e2fde3"),
      c("#000000", "#045806", "#08910d", "#0dc913", "#34ef3c", "#b3f9b5", "#e2fde3")
    ),
  LightGreen = list(
      c("#0dba13"),
      c("#0dba13", "#c6fbc7"), #light
      c("#086408", "#0dba13", "#c6fbc7"),
      c("#086408", "#0dba13", "#7ff582", "#d4fcd6"),
      c("#010901", "#096209", "#0dba13", "#7ff582", "#d4fcd6"),
      c("#010901", "#096209", "#0db012", "#10ea18", "#aef9b0", "#e2fde3"),
      c("#000000", "#045806", "#08910d", "#0dc913", "#34ef3c", "#b3f9b5", "#e2fde3")
    ),
  DarkBlue = list(
      c("#39a0ed"),
      c("#0c4d7c", "#39a0ed"), # dark
      c("#0c4d7c", "#39a0ed", "#abd7f7"),
      c("#0c4d7c", "#39a0ed", "#91cbf5", "#deeffc"),
      c("#06263f", "#0b4c7d", "#39a0ed", "#91cbf5", "#deeffc"),
      c("#010509", "#093b62", "#0c62a1", "#44a5ee", "#91cbf5", "#deeffc"),
      c("#010509", "#093b62", "#0c62a1", "#2097ee", "#68b9f3", "#acd8f6", "#deeffc")
  ),
  LightBlue = list(
    c("#39a0ed"),
    c("#39a0ed", "#abd7f7"), #light
    c("#0c4d7c", "#39a0ed", "#abd7f7"),
    c("#0c4d7c", "#39a0ed", "#91cbf5", "#deeffc"),
    c("#06263f", "#0b4c7d", "#39a0ed", "#91cbf5", "#deeffc"),
    c("#010509", "#093b62", "#0c62a1", "#44a5ee", "#91cbf5", "#deeffc"),
    c("#010509", "#093b62", "#0c62a1", "#2097ee", "#68b9f3", "#acd8f6", "#deeffc")
  ),
  DarkRed = list(
    c("#dc394a"),
    c("#781728", "#dc394a"), #dark
    c("#781728", "#dc394a", "#fbc1ba"),
    c("#781728", "#dc394a", "#f39288", "#fed6d2"),
    c("#1b0408", "#861a2b", "#dc394a", "#f39288", "#fed6d2"),
    c("#1b0408", "#6d1221", "#b5263c", "#e65f60", "#f4978d", "#fed6d2"),
    c("#1b0408", "#60101d", "#a92338", "#dc394a", "#ed7c71", "#f9a89f", "#fed6d2")
  ),
  LightRed = list(
      c("#dc394a"),
      c("#dc394a", "#fac0ba"), #light
      c("#781728", "#dc394a", "#fbc1ba"),
      c("#781728", "#dc394a", "#f39288", "#fed6d2"),
      c("#1b0408", "#861a2b", "#dc394a", "#f39288", "#fed6d2"),
      c("#1b0408", "#6d1221", "#b5263c", "#e65f60", "#f4978d", "#fed6d2"),
      c("#1b0408", "#60101d", "#a92338", "#dc394a", "#ed7c71", "#f9a89f", "#fed6d2")
    ),
  DarkOchre = list(
      c("#dda15e"),
      c("#7e501b", "#dda15e"), #dark
      c("#7e501b", "#dda15e", "#f4ddc3"),
      c("#321f0b", "#9d6321", "#dda15e", "#f8ede2"),
      c("#321f0b", "#9d6321", "#dda15e", "#ebc8a1", "#f9eee3"),
      c("#321f0b", "#7a4d1a", "#bb8446", "#ebb375", "#f4d6b3", "#fbf0e5"),
      c("#321f0b", "#7a4d1a", "#a87438", "#dda15e", "#f9c58b", "#f2dabf", "#faefe4")
    ),
  LightOchre = list(
      c("#dda15e"),
      c("#dda15e", "#f4ddc3"), #light
      c("#7e501b", "#dda15e", "#f4ddc3"),
      c("#321f0b", "#9d6321", "#dda15e", "#f8ede2"),
      c("#321f0b", "#9d6321", "#dda15e", "#ebc8a1", "#f9eee3"),
      c("#321f0b", "#7a4d1a", "#bb8446", "#ebb375", "#f4d6b3", "#fbf0e5"),
      c("#321f0b", "#7a4d1a", "#a87438", "#dda15e", "#f9c58b", "#f2dabf", "#faefe4")
    )
  )



#' Helper function that helps generate a sequential palette of arbitrary length
#'
#' @param n The number of colors in the sequential palette
#' @param name The name of the palette to use
#'
#' @return A vector of RGB codes
generate_seq_palette <- function(name="LightGreen", n = 5) {
  if (missing(n)) {
    stop("Number of colors is missing.")
  }
  if(n <= 0) {
    stop("n must be > 0.")
  }

  if(n <= 7){
    col_pal <- scilicium_sequential_palettes[[name]][[n]]
  } else {
    col_pal <- grDevices::colorRampPalette(scilicium_sequential_palettes[[name]][[6]])(n)
  }
  col_pal
}



#' @title Access a scilicium palette
#'
#' @description
#' Access a Scilicium palette in the form of a vector of html color codes.
#'
#' @param name The name of the palette to use. Currently available palettes are SciliciumLight, SciliciumDark, SciliciumPaired2, SciliciumPairedLight, SciliciumPairedDark, KombuGreen and EarthYellow (for qualitative palettes), DarkGreen, LightGreen, DarkBlue, LightBlue, DarkRed, LightRed, DarkOchre, LightOchre (for sequential palettes)
#' @param n An integer representing the number of colors in the palette
#' @return A vector of html color codes
#'
#' @export
#'
#' @examples
#' scilicium_pal("DarkGreen", 5)
#' @examples
#' scilicium_pal("DarkGreen", 3)
#' @examples
#' scilicium_pal("SciliciumPairedLight", 10)

scilicium_pal <- function(name, n){
  if(!(name %in% names(scilicium_qual_palettes)) && !(name %in% names(scilicium_sequential_palettes))){
    err_msg <- paste(name, "is not a valid scilicium palette name.\nAvailable names for qualitative palettes are:\n\t\t")
    err_msg <- paste(err_msg, paste(names(scilicium_qual_palettes), collapse="\n\t\t"))
    err_msg <- paste(err_msg, "\nAvailable names for sequential palettes are:\n\t\t")
    err_msg <- paste(err_msg, paste(names(scilicium_sequential_palettes), collapse="\n\t\t"))
    stop(err_msg)
  }
  ncolors <- n
  if(name %in% names(scilicium_qual_palettes)){
    if(n < 3){
      warning("minimal value for n is 3, displaying requested palette with 3 different levels\n")
      ncolors <- 3
    }
    if(n > length(scilicium_qual_palettes[[name]])){
      warning(paste("n too large, allowed maximum for palette", name, "is", length(scilicium_qual_palettes[[name]])),
              "\nDisplaying the palette you asked for with that many colors\n")
      ncolors <- length(scilicium_qual_palettes[[name]])
    }
    print_colors <- scilicium_qual_palettes[[name]][1:ncolors]
  }
  if(name %in% names(scilicium_sequential_palettes)){
    if(n < 1){
      warning("minimal value for n is 1, displaying requested palette with 3 different levels\n")
      ncolors <- 3
    }
    print_colors <- generate_seq_palette(name, ncolors)
  }

  return(print_colors)
}





#' @title SciLicium scale color for ggplot2
#'
#' @description
#' The `SciLicium` scales provide sequential and qualitative
#' colour schemes in line with SciLicium graphic theme.
#'
#' @param name The name of the palette to use. Currently available palettes are SciliciumLight, SciliciumDark, SciliciumPaired2, SciliciumPairedLight, SciliciumPairedDark, KombuGreen and EarthYellow (for qualitative palettes), DarkGreen, LightGreen, DarkBlue, LightBlue, DarkRed, LightRed, DarkOchre, LightOchre (for sequential palettes)
#' @param n An integer representing the number of colors in the palette
#' @return A custom ggplot2 color scale
#'
#'
#' @export
#' @examples
#' scale_color_scilicium("KombuGreen", 5)
#' @examples
#' library(ggplot2)
#' n_category <- 10
#' n_replicates <- 5
#' tmp_df <- data.frame(x = runif(n_replicates * n_category),
#'   y=runif(n_replicates * n_category),
#'   category=letters[rep(1:n_category, n_replicates)])
#' gg <- ggplot(tmp_df, aes(x=x, y=y, col=category)) +
#'   geom_point() +
#'   scale_color_scilicium("SciliciumPairedLight", 10) +
#'   theme_scilicium()
#' @examples
#' library(ggplot2)
#' n_category <- 10
#' n_replicates <- 5
#' tmp_df <- data.frame(x = runif(n_replicates * n_category),
#'   y=runif(n_replicates * n_category),
#'   category=letters[rep(1:n_category, n_replicates)])
#' gg <- ggplot(tmp_df, aes(x=x, y=y, col=category)) +
#'   geom_point() +
#'   scale_color_scilicium("Vivid", 10) +
#'   theme_scilicium()
#'
#' @family ggplot2 functions

scale_color_scilicium <- function(name="SciliciumLight", n=20) {
  ggplot2::scale_color_manual(values=scilicium_pal(name, n))
}


#' @title SciLicium scale fill for ggplot2
#'
#' @description
#' The `SciLicium` scales provide sequential and qualitative
#' colour schemes in line with SciLicium graphic theme.
#'
#' @param name The name of the palette to use. Currently available palettes are SciliciumLight, SciliciumDark, SciliciumPaired2, SciliciumPairedLight, SciliciumPairedDark, KombuGreen and EarthYellow (for qualitative palettes), DarkGreen, LightGreen, DarkBlue, LightBlue, DarkRed, LightRed, DarkOchre, LightOchre (for sequential palettes)
#' @param n An integer representing the number of colors in the palette
#' @return A custom ggplot2 color fill scale
#'
#' @export
#'
#' @examples
#' scale_fill_scilicium("KombuGreen", 5)
#' @examples
#' library(ggplot2)
#' n_category <- 6
#' n_replicates <- 5
#' tmp_df <- data.frame(x = runif(n_replicates * n_category),
#'   y=runif(n_replicates * n_category),
#'   category=letters[rep(1:n_category, n_replicates)])
#' gg <- ggplot(tmp_df, aes(x=category, y=y, fill=category)) +
#'   geom_boxplot() +
#'   scale_fill_scilicium("DarkRed", n_category) +
#'   theme_scilicium()
#'
#' @family ggplot2 functions

scale_fill_scilicium <- function(name="SciliciumLight", n=20) {
  ggplot2::scale_fill_manual(values=scilicium_pal(name, n))
}



#' @title Display a scilicium palette
#'
#' @description
#' Display a given SciLicium palette
#'
#' @param name The name of the palette to use. Currently available palettes are "SciliciumLight" (default, 5 colors), "SciliciumDark" (5 colors), "SciliciumPaired" (8 colors), "KombuGreen" (5 colors) and "EarthYellow" (5 colors)
#' @param n An integer representing the number of colors in the palette
#' @return A custom ggplot2 color scale
#'
#' @export
#'
#' @examples
#' display_scilicium_pal("DarkGreen", 5)
#' display_scilicium_pal("SciliciumPairedLight", 10)
#'
#' @family palette vizualization functions

display_scilicium_pal <- function(name, n){
  if(!(name %in% names(scilicium_qual_palettes)) && !(name %in% names(scilicium_sequential_palettes))){
    err_msg <- paste(name, "is not a valid scilicium palette name.\nAvailable names for qualitative palettes are:\n\t\t")
    err_msg <- paste(err_msg, paste(names(scilicium_qual_palettes), collapse="\n\t\t"))
    err_msg <- paste(err_msg, "\nAvailable names for sequential palettes are:\n\t\t")
    err_msg <- paste(err_msg, paste(names(scilicium_sequential_palettes), collapse="\n\t\t"))
    stop(err_msg)
  }

  ncolors <- n
  if(name %in% names(scilicium_qual_palettes)){
    if(n < 3){
      warning("minimal value for n is 3, displaying requested palette with 3 different levels\n")
      ncolors <- 3
    }
    if(n > length(scilicium_qual_palettes[[name]])){
      warning(paste("n too large, allowed maximum for palette", name, "is", length(scilicium_qual_palettes[[name]])),
              "\nDisplaying the palette you asked for with that many colors\n")
      ncolors <- length(scilicium_qual_palettes[[name]])
    }
    print_colors <- scilicium_qual_palettes[[name]][1:ncolors]

    #if(length(which(name==quallist))>0) palattr<-"(qualitative)"
    palattr<-"(qualitative)"
  }
  if(name %in% names(scilicium_sequential_palettes)){
    if(n < 1){
      warning("minimal value for n is 1, displaying requested palette with 3 different levels\n")
      ncolors <- 3
    }
    print_colors <- generate_seq_palette(name, ncolors)

    #if(length(which(name==quallist))>0) palattr<-"(qualitative)"
    palattr<-"(sequential)"
  }

  graphics::image(1:ncolors, 1, as.matrix(1:ncolors), col = print_colors,
        xlab=paste(name, palattr),ylab="",xaxt="n",yaxt="n",bty="n")
}



#' @title Display all scilicium palettes
#'
#' @description
#' Display all available Scilicium palettes
#'
#' @return A very nice plot!
#'
#' @export
#'
#' @family palette vizualization functions

display_scilicium_all <- function(){

  qual_palettes <- sort(names(scilicium_qual_palettes))
  seq_palettes <- sort(names(scilicium_sequential_palettes))

  all_palettes <- c(qual_palettes, "", seq_palettes)
  pal_lengths <- c(lengths(scilicium_qual_palettes), 0, lengths(scilicium_sequential_palettes))

  nr <- length(all_palettes)
  nc <- max(max(pal_lengths), max(lengths(scilicium_sequential_palettes))+3)

  oldpar <- graphics::par(mgp=c(2,0.25,0))
  on.exit(graphics::par(oldpar))

  plot(1, 1, xlim=c(-1, nc), ylim=c(0,nr), type="n", axes=FALSE, bty="n",
       xlab="", ylab="")
  for(i in 1:length(qual_palettes)){

    pal <- qual_palettes[i]
    nj <- lengths(scilicium_qual_palettes)[[pal]]

    graphics::rect(xleft=0:(nj-1), ybottom=i-1, xright=1:nj, ytop=i-0.2, col=scilicium_qual_palettes[[pal]],
         border="light grey")
  }
  y_offset <- length(qual_palettes)+1
  for(i in 1:length(seq_palettes)){

    pal <- seq_palettes[i]
    nj <- lengths(scilicium_sequential_palettes)[[pal]]

    nj_offset <- 3

    graphics::rect(xleft=0:(2-1), ybottom=i-1+y_offset, xright=1:2, ytop=i-0.2+y_offset, col=scilicium_sequential_palettes[[pal]][[2]],
         border="light grey")

    graphics::rect(xleft=nj_offset:(nj-1+nj_offset), ybottom=i-1+y_offset, xright=(nj_offset+1):(nj+nj_offset), ytop=i-0.2+y_offset, col=scilicium_sequential_palettes[[pal]][[nj]],
         border="light grey")
  }
  #plot number of colors in seq palettes
  graphics::text(rep(2.05,length(seq_palettes)),(1:length(seq_palettes))-0.6+y_offset, labels=rep("(2)", length(seq_palettes)), xpd=TRUE, adj=0)
  graphics::text(lengths(scilicium_sequential_palettes)+3+.05,(1:length(seq_palettes))-0.6+y_offset, labels=lengths(scilicium_sequential_palettes), xpd=TRUE, adj=0)

  #plot number of colors in qual palettes
  graphics::text(lengths(scilicium_qual_palettes[sort(names(scilicium_qual_palettes))])+.05,(1:length(qual_palettes))-0.6, labels=lengths(scilicium_qual_palettes[sort(names(scilicium_qual_palettes))]), xpd=TRUE, adj=0)

  graphics::text(rep(-0.1,nr),(1:nr)-0.6, labels=all_palettes, xpd=TRUE, adj=1)

}


