#----------------------------------------

#   # custom GG theme - minimal version
#   # Starts with theme_minimal() and change some things
#   # Updates a number of geom defaults

#----------------------------------------


dgray <- "black"
mgray <- "gray60"
lgray <- "gray85"
whisp <- "gray98"

scale_colour_discrete <- ggthemes::scale_colour_solarized
scale_fill_discrete <- ggthemes::scale_fill_solarized


# sysfonts::font_add("Myriad Pro", 
#                    regular = "Myriad ProPro-Regular.otf", 
#                    bold = "Minion ProPro-Bold.otf",
#                    italic = "MinionPro-It.otf",
#                    bolditalic = "MinionPro-BoldIt.otf")
# # sysfonts::font_add("minion-bold", "MinionPro-Bold.otf")
# showtext::showtext_auto()

theme_mgd_base <- function(base_size = 12, base_family = "Source Sans Pro") {

  # --- geom defaults -----------------------
  #   FYI not all of these work (e.g. smooth---why?)

  update_geom_defaults("point", list(color = dgray, size = 2))
  update_geom_defaults("text", list(color = dgray, size = 3.5, family = "Source Sans Pro"))
  # update_geom_defaults("line", list(color = dgray, size = 0.15))
  update_geom_defaults("bar", list(fill = mgray, color = dgray))
  update_geom_defaults("density", list(fill = mgray, 
                                       color = dgray, 
                                       alpha = 0.5))
  update_geom_defaults("pointrange", list(color = dgray))
  # update_geom_defaults("vline", list(color = lgray))
  # update_geom_defaults("hline", list(color = lgray))
  # update_geom_defaults("ribbon", list(fill = lgray, color = NULL, alpha = 0.5))


# --- start with bw -----------------------
  
  ggthemes::theme_base(base_size = base_size, base_family = base_family) %+replace%
  theme(
    axis.ticks = element_line(size = rel(0.5), lineend = "square"),
    axis.ticks.length = unit(0.25, "lines"),
    axis.title = element_text(family = "Source Sans Pro"),
    plot.background = element_blank(),
    legend.title = element_text(hjust = 0, family = "Source Sans Pro"),
    strip.text = element_text(vjust = 1, size = rel(0.8), family = "Source Sans Pro")
  )

}

