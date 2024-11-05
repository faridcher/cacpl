library(sfextra)
source('regularize.r')

n <- c('c1','c2','l')
n2 <- c('cheetah/56/c1','cheetah/58/c2','leopard/49/l')
move <- mapply(n, n2, 1, SIMPLIFY=F, FUN=function(n,n2,i) {
                 f <- paste0('data/gps/', n2, '.csv')
                 rt <- read.csv(file=f, header=TRUE)
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
# plot(st_geometry(move), main='click lower-left and then upper-right spatial range')
# x <- locator(2, 'p', bg=2, pch=21)
x <- list(x = c(315.016, 429.98), y = c(3408.545, 3566.760))
x  <- st_sfc(st_as_sfc(st_bbox(st_linestring(t(do.call(rbind, x))))), crs=utm40_km)
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
