library(units)
library(sf)
library(stars)
library(car)

source('Lag.r')
source('climate/funcs.r')
source('interval-intersection.r')
source('read/habitat.r')
source('read/move.r')

## attach spatial covariates
dist <- lapply(shps, st_nearest_dist, sfc1=x)
names(dist) <- paste0(names(dist), "_dist")
extr <- lapply(rstrs, raster::extract, y=as(x, "Spatial"))
weath <- time_interpolate_join(x[, 'ti', drop = T], weather_bafgh$`2007`$ti, weather_bafgh$`2007`[, -1])
move <- cbind(x, extr, dist, weath) # as.data.frame coercion.

# Gaps
with(merged_irreg, {
  plot(ti, sep.c1_l, type='b')
  lines(ti, sep.c1_ch2, col=2)
})

# Gap report
lapply(move_irreg, function(x){table(c(diff(x$ti, units="hour"), NA_integer_)) })
# there is a bit of temporal offset in the time
overlapping_period <- with(move, do.call(interval_intersection, split(ti, f=iid)))

# rasters
with(lapply(rstrs, getValues), cor(elevation, slope, use = "complete.obs"))

png_inch("plots/elev-slope-aspect.png", width=11.3, height = 6.7)
my_par(mar=c(3,4,1,4))
layout(matrix(1:2,1))
mapply(rstrs[1:2], main=names(rstrs[1:2]), FUN = plot, MoreArgs= list(xlab = "X (km)", ylab = "Y (km)"))
dev.off()

# temporal summaries
lapply(split(st_drop_geometry(move), f=move$iid), function(x){
  with(x, list("temporal range"=range(ti),
       duration = diff(range(ti)),
       nobs = nrow(x)))
})

# cheetah_alone
# nearest highway
min(subset(move, move$iid=='c1', select=highway_dist, drop=T))
# bbox : width and height
lapply(split(move, f=move$iid), function(x) list(st_bbox(x), dx=diff(x[c(1,3)])/1000, dy=diff(x[c(2,4)])/1000) )
# average mid-day temperature
summary(subset(weather_bafgh$`2007`, subset=format(x=.$ti, format="%H") %in% c("09","12","15","18"), select='temp'))

# individuals scans
png_inch("plots/scan-tracks%d.png", width=12)
my_par(cex.lab=2)
s <- split(move,f=move$iid)
invisible(mapply(s, names(s), FUN = function(xx, yy) with(xx, scan_track(x = x, y = y, time = ti))))
dev.off()

# move metrics pairs (abs,rel,dist) all animals 9 by 9 at simultaneous points
png_inch("plots/appendix/pairs-move-%02d.png")
x <- st_drop_geometry(subset(move, ti %between% overlapping_period, select=c('abs.angle','rel.angle','dist','iid')))
pairsPlus(cols_numeric_only(do.call(cbind, args = split(x, f=x$iid))))
dev.off()

# c1, l (full track)
pdf("plots/animation/anim_full.pdf")
animate(move, delay = 0)
dev.off()

png_inch("plots/angle-rose.png")
#rose diagram of angular statistics
my_par()
layout(matrix(1:6,3,byrow=T))
mapply(s, names(s), FUN = function(x, y) {
        l <- lapply(subset(x, select = c('abs.angle', 'rel.angle'), drop = T), circular)
        mapply(l, names(l), FUN = function(xx, yy) rose.diag(xx, main = paste(yy, y, sep = '.'), bins = 20, col = "grey", prop = 2))})
#it doesn't work. makes 2 calls to rose.diag
#col=c("grey","lightgrey")[merged$near.c1_l+1], 
dev.off()

# 3D space-time cube
rgl.snapshot("3d2-%02d.png")
mapply(s[1], name = names(s), FUN=function(tr, name){ with(tr,{
            tt <- (time-min(time))/(60*60*24)  # rgl doesn't draw posix properly.
            plot3d(x, y, tt, type = 's', size = .3, col = 3-unclass(mode_null), axes = F, xlab = "", ylab = "", zlab = "", add = F)
            lines3d(x,y,tt)
           })
           #par3d(cex.lab=3)
           axes3d('xyz',tick=F,expand=3)
           decorate3d(xlab = "X (km)", ylab = "Y (km)", #zlab = "time (day)",
                     box = F, axes = TRUE, main = name, sub = NULL, top = TRUE, aspect = FALSE, estylexpand = 1)
           legend3d("topright", c("Moving","Resting") ,col=1:2, pch=19)})
dev.off()
