### personalizado 1 colores ----

myColour1 <- "#22546E" # azul
myColour2 <- "#6DB39E" # verdoso pantano
myColour3 <- "#7CA436" # verde
myColour4 <- "#6C5580" # morado
myColour5 <- "#C4761C" # amarillo
myColour6 <- "#A10E3B" # rojo vino

myColourHL1 <- "#C5F6EB" # hipervínculo
myColourHL2 <- "#91E119" # hipervínculo visitado

myColourT1 <- "#222222" # texto fondo obscuro
myColourT2 <- "#FEFEFE" # texto fondo blanco
myColourT3 <- "#4A6670" # texto fondo verdoso
myColourT4 <- "#899F99" # texto fondo hueso

### personalizado P colores ----

myColourP1 <- "#22546E" # azul
myColourP2 <- "#6DB39E" # verdoso pantano
myColourP3 <- "#7CA436" # verde
myColourP4 <- "#5E207B" # morado
myColourP5 <- "#DFBE2F" # amarillo
myColourP6 <- "#A10E3B" # rojo vino

myColourPHL1 <- "#1F4458" # hipervínculo
myColourPHL2 <- "#6021C4" # hipervínculo visitado

myColourPT1 <- "#222222" # texto fondo obscuro
myColourPT2 <- "#FFFFFF" # texto fondo blanco
myColourPT3 <- "#6DB39E" # texto fondo verdoso
myColourPT4 <- "#F9F9F9" # texto fondo hueso


### crear paleta de colores ----

library(ggplot2)
theme_set(theme_minimal())

tresalia_colors <- c(
  `azul` = myColour1,
  `pantano` = myColour2,
  `verde` = myColour3,
  `morado` = myColour4,
  `amarillo` = myColour5,
  `rojo` = myColour6,
  `hiper1` = myColourHL1,
  `hiper2` = myColourHL2,
  `tf_negro` = myColourT1,
  `tf_blanco` = myColourT2,
  `tf_azul` = myColourT3,
  `tf_gris` = myColourT4,
  `azulP` = myColourP1,
  `pantanoP` = myColourP2,
  `verdeP` = myColourP3,
  `moradoP` = myColourP4,
  `amarilloP` = myColourP5,
  `rojoP` = myColourP6,
  `hiper1P` = myColourPHL1,
  `hiper2P` = myColourPHL2,
  `tf_negroP` = myColourPT1,
  `tf_blancoP` = myColourPT2,
  `tf_verdeP` = myColourPT3,
  `tf_huesoP` = myColourPT4
)

tresalia_cols <- function(...) {
  cols <- c(...)
  
  if (is.null(cols))
    return (tresalia_colors)
  
  tresalia_colors[cols]
}

# tresalia_cols()
# tresalia_cols("azul")

tresalia_palettes <- list(
  `main` = tresalia_cols("azul", "pantano", "verde", "morado", "amarillo", "rojo"),
  `hiper` = tresalia_cols("hiper1", "hiper2"),
  `texto_fondo` = tresalia_cols("tf_negro", "tf_blanco", "tf_azul", "tf_gris"),
  `mainP` = tresalia_cols("azulP", "pantanoP", "verdeP", "moradoP", "amarilloP", "rojoP"),
  `hiperP` = tresalia_cols("hiper1P", "hiper2P"),
  `texto_fondoP` = tresalia_cols("tf_negroP", "tf_blancoP", "tf_verdeP", "tf_huesoP")
)

tresalia_pal <- function(palette = "main", reverse = FALSE, ...) {
  pal <- tresalia_palettes[[palette]]
  
  if (reverse) pal <- rev(pal)
  
  colorRampPalette(pal, ...)
}
# tresalia_pal()
# tresalia_pal()(10)
scale_colour_tresalia <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- tresalia_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("colour", paste0("tresalia_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}
scale_fill_tresalia <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- tresalia_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("fill", paste0("tresalia_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}


### crear theme reusable ----

library(rlang)
library(Rcpp)
library(ggplot2)
library(ggthemes)
library(extrafont)
# font_import(prompt = FALSE)
# loadfonts()
theme_tresalia <- theme_bw() + 
  theme(plot.background =  element_rect(fill = myColourPT4, colour = myColourT3, size = 1.0),
        plot.title = element_text(face = "bold", colour = myColourT1, size = 16, family = "Garamond"),
        plot.subtitle = element_text(face = "italic", colour = myColourT1, size = 10, family = "Garamond"),
        panel.border = element_rect(colour = myColourT3, size = 0.25),
        axis.line = element_line(color = myColour1),
        axis.ticks = element_line(color = myColour1),
        strip.text = element_text(color = myColour1),
        axis.title = element_text(face = "italic", hjust = 0.5, colour = myColourT1, size = 14, family = "Garamond"),
        axis.title.y = element_text(margin = ggplot2::margin(t = 0, r = 15, b = 0, l = 5)),
        axis.title.x = element_text(margin = ggplot2::margin(t = 15, r = 0, b = 5, l = 0)),
        legend.title = element_text(color = myColourT1, face = "bold", size = 10),
        legend.margin  = ggplot2::margin(t = 1, r = 5, b = 2.5, l = 5),
        legend.text = element_text(color = myColour1, face = "bold",size = 8, family = "Garamond"),
        legend.background = element_rect(color = myColourT3, size = 0.5, fill = myColourPT4))


