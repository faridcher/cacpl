# list.files('data/shp', '.*\\.shp$', full.names = TRUE)
# province.shp and layout/rusta.shp are not used. Which road I used? where is highway?
n <- c(paste0('data/iran/', c('road','city','village'), '.shp'), 
       paste0('data/layout/', c('abesh', 'spring','road'), '.shp'),
       paste0('data/topo/topo','.shp'))
shps <- lapply(n, st_read, quiet = TRUE)
b <- basename(n)
names(shps) <- substring(b,1, nchar(b)-4)
names(shps)[c(1,6)] <- c('highway','path')

# shps[1:3] crs is utm39. 
shps[4:6] <- lapply(shps[4:6], st_set_crs, value=st_crs('epsg:32640'))
# transform units from meter into km
utm40_km <- '+proj=utm +zone=40 +datum=WGS84 +units=km +no_defs'
shps <- invisible(lapply(shps, st_transform, crs=st_crs(utm40_km)))

# Merge abesh (4) and spring (24) as a single layer named water
shps$water <- st_sf(geom=do.call(c, lapply(shps[c('abesh','spring')], st_geometry)))
shps[c('abesh','spring')] <- NULL

r <- read_stars('data/aster/dem.tif')
# BUG: st_transform to utm40_km sets stars object offset and delta to NA and the plot is too slow
# r <- st_transform(r, crs = st_crs(utm40_km)$proj4string)
rstrs <- list(elevation = r,
      slope = raster::terrain(r, opt = 'slope'),
      aspect = raster::terrain(r, opt = 'aspect'))
