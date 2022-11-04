theme_fer<-function (base_size = 18, base_family = "serif", 
                     style = c("default", "darkunica"), bgcolor = NULL){
  vyellow <- "#FFDA29"
  bluedep <- "#263056"
  coral <- "#FF6F61"
  blackpearl <- "#4D4B50"
  peach <- "#F8A39D"
  beluga <- "#4A4843"
  if (!is.null(bgcolor)) {
    warning("`bgcolor` is deprecated. Use `style` instead.")
    style <- bgcolor
  }
  style <- match.arg(style)
  bgcolor <- switch(style, default = "#eceeed", darkunica = "#2a2a2b")
  ret <- theme(rect = element_rect(fill = bgcolor, linetype = 0, 
                                   colour = NA), text = element_text(size = base_size, 
                                                                     family = base_family),
               title = element_text(hjust = 0.5, colour = "#2A3132"), 
               axis.title.x = element_text(hjust = 0.5, colour = "#636768"), 
               axis.title.y = element_text(hjust = 0.5, colour = "#636768"), 
               panel.grid.major.y = element_line(colour = "#D8D8D8"), 
               panel.grid.minor.y = element_blank(), panel.grid.major.x = element_blank(), 
               panel.grid.minor.x = element_blank(), panel.border = element_blank(), 
               panel.background = element_blank(), legend.position = "bottom", 
               legend.key = element_rect(fill = "#FFFFFF00"))
  if (style == "darkunica") {
    ret <- (ret + theme(rect = element_rect(fill = bgcolor), 
                        text = element_text(colour = "#A0A0A3"),
                        title = element_text(colour = "#FFFFFF"), 
                        axis.title.x = element_text(colour = "#A0A0A3"), 
                        axis.title.y = element_text(colour = "#A0A0A3"), 
                        panel.grid.major.y = element_line(colour = "#707073"), 
                        legend.background = element_text(colour = "#A0A0A3")))
  }
  ret
}
