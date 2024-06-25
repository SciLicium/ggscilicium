# ggscilicium: A package for custom SciLicium graphic theme

The ggscilicium package provides qualitative and sequential palettes, as well as a theme for ggplot2.

## How to install
You need to have the `devtools` package installed. To install:
```
library(devtools)
install_github("SciLicium/ggscilicium")
```
To use the library:
```
library(ggscilicium)
display_scilicium_all()
?ggscilicium
```
Available palettes:
![image](https://github.com/SciLicium/ggscilicium/assets/101104633/8d25e07a-fa7d-4c90-a1a4-9157d955249d)


## Generic package functions

The most important functions of this package are:
* `scilicium_pal(name, n)` Access a given palette, and return a vector of colors of length n. For qualitative palettes, if n is larger than the palette length, a warning will be issued and the resulting vector will be the max possible length. On the contrary, sequential palettes will be interpolated if n is larger than the palette length (7).
* `display_scilicium_pal(name, n)` Generate a plot of the requested palette with n elements.
* `display_scilicium_all()` Generate a plot of all available palettes!

## ggplot2 specific package functions

The following functions are made to use with ggplot2:
* `scale_color_scilicium(name, n)` A wrapper for ggplot2 scale_color_manual(values=scilicium_pal(name, n)), to increase ease of use and code readability.
* `scale_fill_scilicium(name, n)` A wrapper for ggplot2 scale_fill_manual(values=scilicium_pal(name, n)), to increase ease of use and code readability.
* `theme_scilicium()` A ggplot2 theme

## Note on palettes
* **Qualitative** palettes are a collection of colors, designed to be hopefully both nice and coherent. They all comprise the notorious 'SciLicium green' (#0dba13).
* **Sequential** palettes are ordered shade of colors , to use e.g. for representing different doses of a same compound. They are available in green, blue, red and ochre.

## package contributions and future improvements
To add a new palette, simply add it to the corresponding list at the beginning of color_scilicium.R, rebuild the package, and that's it!
To create a new theme, just add it to the theme_scilicium.R file (+rebuild).
Future improvements may include diverging palettes as well.

## Useful ressources
* https://learnui.design/tools/data-color-picker.html#divergent
* https://projects.susielu.com/viz-palette?colors=[%22#ffd700%22,%22#ffb14e%22,%22#fa8775%22,%22#ea5f94%22,%22#cd34b5%22,%22#9d02d7%22,%22#0000ff%22]&backgroundColor=%22white%22&fontColor=%22black%22&mode=%22normal%22
* https://github.com/thomasp85/scico
