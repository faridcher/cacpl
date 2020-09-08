library(units)
library(sf)
library(stars)
library(car)

source('func.r')
# Habitat data
# list.files("data/shp", ".*\\.shp$", full.names = TRUE)
# province.shp and layout/rusta.shp are not used. Which road I used? where is highway?
n <- c(paste0("data/doe/iran-bafq/", c('road','city','village'), ".shp"), 
       paste0("data/doe/yazd/bafq/", c('layout/abesh', 'layout/spring','layout/road', 'topo/topo'), ".shp"))
shps <- lapply(n, st_read, quiet = TRUE)
b <- basename(n)
names(shps) <- substring(b,1, nchar(b)-4)
names(shps)[c(1,6)] <- c('highway','path')

# shps[1:3] crs is utm39. 
shps[4:6] <- lapply(shps[4:6], st_set_crs, value=st_crs("epsg:32640"))
# transform units from meter into km
utm40_km <- "+proj=utm +zone=40 +datum=WGS84 +units=km +no_defs"
shps <- invisible(lapply(shps, st_transform, crs=st_crs(utm40_km)))

# Merge abesh (4) and spring (24) as a single layer named water
shps$water <- st_sf(geom=do.call(c, lapply(shps[c('abesh','spring')], st_geometry)))
shps[c('abesh','spring')] <- NULL

# TODO: rasterize "data/doe/yazd/bafq/topo/topo.shp" or download srtm
r <- raster::projectRaster(raster("data/nasa-usgs/srtm/dem.tif"), crs = st_crs(utm40_km)$proj4string)
rstrs <- list(elevation = r,
      slope = raster::terrain(r, opt = 'slope'),
      aspect = raster::terrain(r, opt = 'aspect'))

## Movement data
n <- c('c1','c2','l')
move <- mapply(n, 1, SIMPLIFY=F, FUN=function(n,i) {
                 f <- paste0('data/doe/gps/movement/',n,'.csv')
                 rt <- read.table(file=f, header=TRUE, sep=",")
                 within(rt, {
                          # "N/A" becomes NA_real_
                          x <- as.numeric(Longitude)
                          y <- as.numeric(Latitude)
                          ti <- strptime(paste(UTC.Date, UTC.Time), "%d.%m.%Y %H:%M:%S", tz = "UTC")
                          # round returns POSIXlt
                          ti <- as.POSIXct(round(ti, units="hours"))
                          # iid and tid; iid is Individual iid, tid is Trajectory iid (i.e. burst in other terminologies)
                          iid <- factor(n)
                          tid <- factor(i)
                          rm("No","UTC.Date","UTC.Time","Latitude","Longitude","Height","DOP","Temp")
      })})
# time is not NA in these data. remove all gaps (leading, trailing, intermediate, etc.)
move <- lapply(move, subset, subset=!(is.na(x) | is.na(y)))

# turn list of 3 to a data.frame and make sf. 
move <- st_as_sf(do.call(rbind, move), coords = c("x", "y"), crs = st_crs("epsg:4326"), remove = T)
move <- st_transform(move, st_crs(utm40_km))

## Spatial filter: filter out observations outside a spatial window below.
# plot(st_geometry(move), main="click lower-left and then upper-right spatial range")
# x <- locator(2, 'p', bg=2, pch=21)
x <- list(x = c(315.016, 429.98), y = c(3408.545, 3566.760))
x  <- st_as_sfc(st_bbox(st_linestring(t(do.call(rbind, x)))), crs=st_crs(move))
move <- move[st_intersects(move,x,F)[,1],]

move2 <- st_sf(do.call(rbind, by(move, with(move, iid:tid), function(m){
         # sort by time
         m <- m[order(m$ti),]
         # remove duplicate timestamps (rounded to hour) rows. we don't have dup time here.
         m <- m[c(diff(m$ti)!=0,T),]
         # remove c2 trailing near zero displacement points (e.g. sl less than 10 meters) after its death
         if(m$iid[1]=='c2') m <- head(m, 390)
         m$dt <- c(diff(m$ti, units='hours'), NA_integer_)
         # timeframes before regularization
         print(with(m, iid:tid)[1])
         print(table(m$dt))
         # TODO: split the trajectory at too many intermediate zero displacements (at night) or at large gaps (e.g. timeframes 48h for leopard)
         m <- regularize(m)
         m$sl <- c(st_distance(m[-nrow(m),], m[-1,], by_element=T), as_units(NA, 'km'))
         m$az <- c(st_azimuth(m[-nrow(m),], m[-1,]), as_units(NA,'rad'))
         m$ta <- c(as_units(NA, 'rad'), diff(m$az))
         print(nrow(m))
         m
})))

move <- move2; rm(move2)

## attach spatial covariates
dist <- lapply(shps, st_nearest_dist, sfc1=x) %>% rename("_dist", add=T)
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
par_fav(mar=c(3,4,1,4))
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
par_fav(cex.lab=2)
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
par_fav()
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
