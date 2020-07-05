# ----------------------------------------------------
#   Graphics defaults for Bookdown output
# ----------------------------------------------------

# in this file:
# - font family
# - default ggtheme
# - color definitions

library("ggplot2")


# ---- text family -----------------------

font_fam <- "Source Sans Pro"

update_geom_defaults(geom = "text", list(family = font_fam))
update_geom_defaults(geom = "label", list(family = font_fam))




# ---- theme -----------------------

# theme_mgd <- function() {
#   ggthemes::theme_base(base_size = 14, base_family = font_fam) %+replace% theme(
#       plot.background = element_blank(), 
#       axis.ticks = element_line(lineend = "square"), 
#       axis.ticks.length = unit(0.25, "lines"), 
#       axis.text = element_text(size = 10)
#     )
# }

theme_mgd <- function() {
  hrbrthemes::theme_ipsum(base_family = font_fam) %+replace%
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray80"),
    axis.title.x.bottom = element_text(
      margin = margin(t = 0.35, unit = "cm"),
      size = 12
    ),
    axis.title.y.left = element_text(
      margin = margin(r = 0.35, unit = "cm"),
      size = 12
    ),
    plot.margin = margin(t = 30, r = 30, b = 30, l = 30, unit = "pt"),
    plot.subtitle = element_text(
      size = 16,
      hjust = 0,
      vjust = 1,
      margin = margin(t = 0, r = 0, b = 10, l = 0, unit = "pt")
    ),
    plot.caption = element_text(
      size = 11,
      hjust = 1, vjust = 1,
      margin = margin(t = 10)
    ),
    plot.caption.position = "panel",
    plot.background = element_rect(fill = NA, color = NA)
  )
}

theme_set(theme_mgd())
# hrbrthemes::theme_ipsum()

# theme_mgd_dag <- function() {
#   theme_mgd() %+replace%
#   theme(
#     panel.border = element_blank(),
#     panel.background = element_blank(),
#     # axis.line = element_blank(),
#     axis.title = element_blank(),
#     axis.text = element_blank(),
#     axis.ticks = element_blank()
#   )
# }




# ---- colors -----------------------

# the two parties
dblue <- "#2297E6"
rred <- "#DF536B"



# for scale_color_*
party_factor_colors <- c("1" = dblue, "2" = rred)
party_code_colors <- c("D" = dblue, "R" = rred)
party_colors <- c("Democrats" = dblue, "Republicans" = rred)

# other nice colors
yel <- viridisLite::magma(1, alpha = 1, begin = 0.9, end = 0.9, direction = 1)
purp <-  viridisLite::magma(1, alpha = 1, begin = 0.5, end = 0.5, direction = 1)

# green
# viridisLite::viridis(1, alpha = 1, begin = 0.5, end = 0.5, direction = 1)
# blue 
# viridisLite::viridis(1, alpha = 1, begin = 0.35, end = 0.35, direction = 1)

dark1 <- viridisLite::plasma(n = 1, begin = 0, end = 0)
dark2 <- viridisLite::plasma(n = 1, begin = 0.2, end = 0.2)
dark3 <- viridisLite::plasma(n = 1, begin = 0.35, end = 0.35)

med1 <- viridisLite::plasma(n = 1, begin = 0.45, end = 0.45)
med2 <- viridisLite::plasma(n = 1, begin = 0.55, end = 0.55)
med3 <- viridisLite::plasma(n = 1, begin = .65, end = .65)

light1 <- viridisLite::plasma(n = 1, begin = 0.75, end = 0.75)
light2 <- viridisLite::plasma(n = 1, begin = 0.825, end = 0.825)
light3 <- viridisLite::plasma(n = 1, begin = 0.9, end = 0.9)

primary <- med3
secondary <- dark2 
tertiary <- light2

# primary_light <- 
#   viridisLite::plasma(1, alpha = 1, begin = 0.95, end = 0.95, direction = 1)
# # xaringanthemer::lighten_color(primary, 0.5)
# secondary_light <- 
#   viridisLite::plasma(1, alpha = 1, begin = 0.25, end = 0.25, direction = 1)
# # xaringanthemer::lighten_color(secondary, 0.5)

