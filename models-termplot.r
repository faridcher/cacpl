#lor_mode
#c1
x <- models$lor_mode$c1 %>%
  termplot(x, se=T,
           partial.resid = T,
           #ylim = c(-5,3),
           lwd.term = 2, col.res = color_alpha("grey",.2),
           xlabs = c("Elevation (m)","wind speed (knot)"),
           ylabs = rep("Partials for 2nd order poly",2),
           main="cheetah1")

#l
x <- models$lor_mode$l
termplot(x, se=T, partial.resid = T, lwd.term = 2,
           col.res = color_alpha("grey",.2),
           #ylim = c(-5,3),
           xlabs = c("Elevation (m)", "Highway distance (m)", "Humidity (g/m^3)"),
           ylabs = c(rep("Partials for 2nd order poly",2), "Partials for 1st order poly"),
           main="leopard")

# RSF all
layout(matrix(1:4,2,byrow = T))
par(mar = c(3, 3, 1.5, 0), mgp = c(1.5, .5, 0), tck = -0.01, 
    cex.axis = 1.25, cex.lab = 1.25, bty = "l")

x <- models$rsf$all$c1
termplot(x, se=T, partial.resid = T,
           lwd.term = 2, col.res = color_alpha(x$data$used + 1,.2),
           xlabs = c("Elevation (m)", "Water distance (m)", "Slope", "Pathway distance (m)"),
           ylabs = c(rep("Partials for 2st order poly",2), rep("Partials for 1st order poly",2)),
           main="cheetah1")
legend( "topleft", c("used", "not used"), pch = 1, col = 2:1, bty = 'n')

x <- models$rsf$all$l
termplot(x, se=T, partial.resid = T,
           lwd.term = 2, col.res = color_alpha(x$data$used + 1,.2),
           xlabs = c("Elevation (m)", "Water distance (m)", "Slope", "Pathway distance (m)"),
           ylabs = c(rep("Partials for 2st order poly",2), rep("Partials for 1st order poly",2)),
           main="leopard")
legend("topleft", c("used","not used"),pch=1,col=2:1,bty='n')

# ssf moving --------------------------------------------------------------
x <- models$ssf$moving$c1
termplot(x, se=T,partial.resid = T,
           lwd.term = 2,col.res = color_alpha("grey",.2),
           ylim = c(-5,3),
           xlabs = "Elevation (m)",
           ylabs = "Partials for 2nd order poly",
           main="cheetah")

x <- models$ssf$moving$l
termplot(x, se=T,partial.resid = T,
           lwd.term = 2,
           col.res = color_alpha("grey", .2),
           ylim = c(-5,3),
           xlabs = c("Water distance (m)", "Elevation (m)", "Highway distance (m)", "Pathway distance (m)"),
           ylabs = c(rep("Partials for 1st order poly",3), "Partials for 1st order poly"),
           main="leopard")
