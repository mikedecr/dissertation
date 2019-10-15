# ----------------------------------------------------
#   Graphics defaults for Bookdown output
# ----------------------------------------------------

# in this file:
# - font family
# - default ggtheme
# - color definitions



# ---- text family -----------------------

font_fam <- "Fira Sans"

ggplot2::update_geom_defaults(geom = "text", list(family = font_fam))




# ---- theme -----------------------

ggplot2::theme_set(
  ggthemes::theme_base(base_size = 14, base_family = font_fam) + 
    ggplot2::theme(
      plot.background = ggplot2::element_blank(), 
      axis.ticks = ggplot2::element_line(lineend = "square"), 
      axis.ticks.length = ggplot2::unit(0.25, "lines"), 
      axis.text = ggplot2::element_text(size = 10)
    )
)




# ---- colors -----------------------

# the two parties
dblue <- "#179ee0"
rred <- "#ff5d40"

# for scale_color_*
party_factor_colors <- c("1" = dblue, "2" = rred)
party_code_colors <- c("D" = dblue, "R" = rred)
party_colors <- c("Democrats" = dblue, "Republicans" = rred)

# other nice colors
yel <- viridis::magma(1, alpha = 1, begin = 0.9, end = 0.9, direction = 1)
purp <-  viridis::magma(1, alpha = 1, begin = 0.5, end = 0.5, direction = 1)


