# ----------------------------------------------------
#   Graphics defaults for Bookdown output
# ----------------------------------------------------

# set a default graphics theme
# create other useful parameters (like party colors, palettes)

# type family
font_fam <- "Fira Sans"
update_geom_defaults(geom = "text", list(family = font_fam))


# default theme
theme_set(
  ggthemes::theme_base(base_size = 14, base_family = font_fam) + 
    theme(
      plot.background = element_blank(), 
      axis.ticks = element_line(lineend = "square"), 
      axis.ticks.length = unit(0.25, "lines"), 
      axis.text = element_text(size = 10)
    )
)

# party colors
dblue <- "#0072B2"
rred <- "#D55E00"

party_factor_colors <- c("1" = dblue, "2" = rred)
party_code_colors <- c("D" = dblue, "R" = rred)
party_colors <- c("Democrats" = dblue, "Republicans" = rred)

# other nice colors
yel <- viridis::magma(1, alpha = 1, begin = 0.9, end = 0.9, direction = 1)
purp <-  viridis::magma(1, alpha = 1, begin = 0.5, end = 0.5, direction = 1)


# graphics device, use PDF in LaTeX
options(knitr.graphics.auto_pdf = TRUE)


