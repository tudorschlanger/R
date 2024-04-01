black <- "#000000"
white <- "#FFFFFF"
gray_light1 <- "#D9D9D9"


theme_custom <- function() {
  
  theme_minimal() %+replace%
    theme(
      text             = element_text(
        face           = "plain", 
        lineheight     = 1.2
      ),
      panel.background = element_rect(fill = white, color = white, linewidth = 0.5, linetype = "solid"),
      panel.grid.major = element_line(color = gray_light1, linewidth = 0.3, linetype = 2), 
      panel.grid.minor = element_blank(),
      panel.border     = element_rect(fill = NA, color = black, linewidth = 0.5, linetype = "solid"),
      plot.background  = element_rect(fill = white, color = white),
      legend.title     = element_blank(),
      legend.text      = element_text(size = 9.5, color = black),
      legend.key.width = unit(1, "cm"),
      legend.key       = element_rect(fill = "transparent", color = "transparent"),
      axis.title       = element_text(size = 10, color = black),
      axis.text        = element_text(size = 9, color = black),
      plot.title       = element_text(size = 10, face = "bold", color = black),
      
      complete         = TRUE
    )
  
}
