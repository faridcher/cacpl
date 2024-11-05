#move_mcp is a list of all home ranges (sfc_polygon)
# move

# overlapping
  subset(move, time %between% overlapping_period) %$%
  split(.,iid) %>%
  lapply(function(xx) {
    xx %>%
      st_combine %>%
      st_convex_hull
  }) ->
  move_mcp

# intersection
move_mcp %<>% {
  y <- combn(., 2, simplify = F, FUN=function(xx){
    xx %>% {
      st_intersection(.[[1]],.[[2]])
   }})
  `names<-`(y, names(.) %>%
              combn(2, simplify = F,
                    function(x)
                      paste0(c(x, "intersect"),
                             collapse = '_')))
  } %>%
  c(move_mcp)

# full
move %$%
  # subset(time %between% overlapping_period) %$%
  split(.,iid) %>%
  `names<-`(paste0(names(.), "_full")) %>%
  lapply(function(xx){
    xx %>%
      st_combine %>%
      st_convex_hull
  }) %>%
  c(move_mcp) ->
  move_mcp

move_mcp %>%
  sapply(function(x)
    x %>%
    st_area
    ) %>%
    set_units("km^2") ->
  move_mcp_area

# overlapping percent
move_mcp_area['c1_l_intersect']/move_mcp_area['l'] * 100

# Visualize
# pdf("plots/homerange.pdf")
png_inch("plots/homerange.png")
# dev.new()
par(mar=c(2.5,2.5,0,0),mgp=c(1.5,.5,0)) #box()

move_mcp %$% {
  c1 %>%
    plot(axes = T, xlab = "X (km)", ylab = "Y (km)")
  l %>%
    plot(add=T,lty=3)
  c1_l_intersect %>%
    plot(add = T,
         col = alpha_col(1, .1),
         border = "transparent")
}

x <- 350
y <- 3510
arrows(x,y,x,y+5)
text(x,y-2,"North")

x <- 380
y <- 3460
segments(x,y,x+10,y,lwd=2)
text(x+5,y+1,"10 KM")

txt <- c("Cheetah1-2 HR:","Leopard HR:","HR intersection:") %>%
  paste0(move_mcp_area[c('c1','l','c1_l_intersect')] %>%
           round(2)) %>%
  paste("km^2")

legend("bottomleft",txt,lty=c(1,3,NA),
       fill = c(NA,NA,alpha_col(1,.1)),
       border=c(NA,NA,'black'),bty='n')
box()
dev.off()

# Hull methods -----------------------------------------------------
library(adehabitatHR)

move %>%
  subset(iid %in% c('c1','l'), 'iid') %>%
  as("Spatial") ->
  x

x %>%
  clusthr(unin = "km",
          unout = "km2") ->
  x_cls

x %>%
  # subset(iid == 'l') %>%
  CharHull(unin = "km",
           unout = "km2") ->
  x_chm2

x %>%
  LoCoH.a(unin = 'km',
          unout = 'km2',
          5) ->
  x_locoh

png_inch("plots/homerange-chm.png")
plot(x_chm)
dev.off()

png_inch("plots/homerange-clusthr.png")
plot(x_cls)
dev.off()

png_inch("plots/homerange-locoh.png")
plot(x_locoh)
dev.off()

# error
x_chm2 %>%
  lapply(st_as_sf) ->
  x_chm

png_inch("chm-errors.png",width = 1100, height = 1100, units="px")
my_par(cex.axis=.8, cex.lab=.8)

plot(
  c(0,100),
  c(0,1200),
  type = 'n',
  cex = .5,
  xlab = "Percent of points (%)",
  ylab = "Area (sq km)"
)

x_chm %>%
  mapply(
    seq_along(.),
    FUN = function(xx, col) {
      xx %$% {
        (((round(percent)) %>%
            mod(5) %>%
            diff %>%
            c(0)) == -4) ->
          idx
        lines(round(percent[idx]),
              area[idx] ,
              type = 'o',
              col = col)
        area[idx]
      }
    }
  )
legend(
  "topleft",
  c("C1", "leopard"),
  col = 1:2,
  pch = 1,
  lwd = 1
)
dev.off()
