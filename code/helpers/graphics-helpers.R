# ----------------------------------------------------
#   Graphics defaults for Bookdown output
# ----------------------------------------------------

# in this file:
# - font family
# - default ggtheme
# - color definitions



# ---- text family -----------------------

font_fam <- "Source Sans Pro"

ggplot2::update_geom_defaults(geom = "text", list(family = font_fam))
ggplot2::update_geom_defaults(geom = "label", list(family = font_fam))




# ---- theme -----------------------
theme_mgd <- function() {
  ggthemes::theme_base(base_size = 14, base_family = font_fam) %+replace% ggplot2::theme(
      plot.background = ggplot2::element_blank(), 
      axis.ticks = ggplot2::element_line(lineend = "square"), 
      axis.ticks.length = ggplot2::unit(0.25, "lines"), 
      axis.text = ggplot2::element_text(size = 10)
    )
}


ggplot2::theme_set(theme_mgd())

theme_mgd_dag <- function() {
  theme_mgd() %+replace%
  ggplot2::theme(
    panel.border = ggplot2::element_blank(),
    panel.background = ggplot2::element_blank(),
    # axis.line = ggplot2::element_blank(),
    axis.title = ggplot2::element_blank(),
    axis.text = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_blank()
  )
}





# ---- colors -----------------------

# the two parties
# dblue <- "#179ee0"
dblue <- "#2297E6"
# rred <- "#ff5d40"
rred <- "#DF536B"



# for scale_color_*
party_factor_colors <- c("1" = dblue, "2" = rred)
party_code_colors <- c("D" = dblue, "R" = rred)
party_colors <- c("Democrats" = dblue, "Republicans" = rred)

# other nice colors
yel <- viridis::magma(1, alpha = 1, begin = 0.9, end = 0.9, direction = 1)
purp <-  viridis::magma(1, alpha = 1, begin = 0.5, end = 0.5, direction = 1)

primary <- 
  viridis::viridis(1, alpha = 1, begin = 0.5, end = 0.5, direction = 1)
secondary <- 
  viridis::viridis(1, alpha = 1, begin = 0.35, end = 0.35, direction = 1)
primary_light <- xaringanthemer::lighten_color(primary, 0.5)
secondary_light <- xaringanthemer::lighten_color(secondary, 0.5)

