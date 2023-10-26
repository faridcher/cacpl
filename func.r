# rm global_functions
# n <- names(Filter(is.function, sapply(ls(.GlobalEnv), get, envir=.GlobalEnv)))
# rm(list = n, envir = .GlobalEnv)

# x is mf with dt,iid,tid,sfc.p
regularize <- function(x, max_tf=NA_real_) {
  # assumption: there is no zero dt. x is sorted by time. timeframes (tf) are in hour.
  ct <- table(x$dt)
  # for c1 tf is 8 16 24
  tf <- as.numeric(names(ct))
  if(any(tf==0)) stop("Found zero timeframe. Cannot divide time differences (dt) by zero.")
  # normalize dt (dtn). So it works for any timeframe (tf) not just 8h
  dtn <- x$dt/min(tf)
  # preallocate with estimated total number of points after interpolation. might be faster with data.table
  new <- x[rep(1,sum(tf*ct/min(tf))),]
  j <- 1
  for (i in 1:nrow(x)) {
    # e.g. dtn[i]==1 or the last observation (is.na(dtn[i]))
    new[j,] <- x[i,];
    j <- j+1
    if(dtn[i] > 1 & !is.na(dtn[i])){
      # browser()
      a <- x[rep(i,dtn[i]-1),]
      # p <- st_geometry(x[i:(i+1),])
      # dxy <- diff(p)/as.numeric(dtn[i]) #sfc_p
      # # recycles p[1]. TODO: st_segmentize
      # a$geometry <- p[1] + rep(dxy, dtn[i]-1) * seq_len(dtn[i]-1)
      # st_crs(a$geometry) <- st_crs(x)
      l <- st_cast(st_combine(st_geometry(x[i:(i+1),])), 'LINESTRING')
      a$ti <- seq(x[i,'ti',drop=T], by=as.difftime(min(tf), units=units(x$dt)), len=dtn[i])[-1]
      new[j:(j+dtn[i]-2),] <- a
      j <- j+dtn[i]-2+1
    }
  }
  new$dt <- c(diff(new$ti, units=units(x$dt)), NA_integer_)
  new
}

aggr_track_by_bout <- function(pt_sfc, time, mode_null, units = "hour"){
  #move %>% subset(iid='c1') %$% cumsum(c(1,diff(mode_null %>% unclass) != 0 )) %>% unique
  #become 56. why?
  bout <- cumsum(c(1,diff(mode_null %>% unclass) != 0 ))
  start_time <- aggregate(time, list(bout), min)[,-1] #exclude the grouping. tapply %>% no simplify %>% do.call works
  end_time <- aggregate(time, list(bout), max)[,-1]
  duration <- difftime(end_time, start_time, units = units)  %>% as_units()
  #spatial
  geometry <- tapply(pt_sfc, bout, st_pt_line, simplify = F) %>% do.call(c,.)
  displacement <- tapply(pt_sfc, bout, st_pt_line, simplify = F) %>%
    lapply(st_length) %>%  #sapply removes units
    do.call(c,.) %>%
    set_units('km')
  beeline <- tapply(pt_sfc,
                    bout,
                    st_beeline,
                    simplify = F) %>%
    lapply(st_length) %>%  #sapply removes units
    do.call(c,.) %>%
    set_units('km')
  daily_displacement <- displacement/set_units(duration ,'day')
  #mode
  mode <- tapply(mode_null %>% unclass, list(bout), min) %>% factor(labels=c("Encamped","Moving"))
  data.frame( bout_id=seq_along(start_time), start_time, end_time, duration, displacement, beeline, daily_displacement, mode, geometry) %>%
    st_sf
}

aggr_cov_by_bout <- function(covariates,mode_null){
  move_bout <- cumsum(c(1,diff(mode_null %>% unclass) != 0 ))
  mean_df <- aggregate(covariates, list(move_bout) , mean)[,-1] %>% `names<-`(sprintf("%s_mean",names(.)))
  sd_df <- aggregate(covariates, list(move_bout), sd)[,-1] %>% `names<-`(sprintf("%s_sd",names(.)))
  data.frame(mean_df,sd_df)
}

par_fav <- function(...) par(mar = c(3, 3, 1, 1), mgp = c(2, 1, 0), tck = -.01, cex.axis = 1.25, cex.lab = 1.25, bty='l', ...)

# Lag(1:5,-1)
# Lag(1:5,1)
# Hmsic:Lag(1:5) # same effect but with NA. we want to avoid the NAs
Lag <- function(x, shift=1) 
  if(shift==0) x else c(tail(x,-shift), head(x,shift))

numeric_col <- function(df) df[, sapply(df, mode) %in% c("numeric","logical","integer","double")]

panel_smooth_abc <- function (x, y, col = par("col"), bg = NA, pch = par("pch"), cex = 1, col.smooth = "red", span = 2 / 3, iter = 3, ...) {
    points( x, y, pch = pch, col = col, bg = bg, cex = cex)
    #ok <- is.finite(x) & is.finite(y)
    #if (any(ok))
    #  lines(stats::lowess(x[ok], y[ok], f = span, iter = iter),
    #        col = col.smooth, ...)
    l <- letters[assign('i', get('i', env = .GlobalEnv) + 1, env = .GlobalEnv)]
    mtext(paste(l, ")"), adj = 0)
    grid()
}

#extract rect x range
#x a vector of values for the x axis
#where a logical vector indicating where to extract the x ranges
#width the width of the length one bout i.e. a single T with F on its sides (..FTF...)
rect_xrange <- function(x, where, width=1){
  #mode2bout
  y <- cumsum(c(1, diff(where) != 0))[where]
  x_range <- with(y, data.frame(min_x=tapply(x[where],y, min),
               max_x=tapply(x[where],y, max)))

  equal_idx <- x_range[,1] == x_range[,2]
  x_range[equal_idx, 1] <- x_range[equal_idx, 1] - width
  x_range[equal_idx, 2] <- x_range[equal_idx, 2] + width
  return(x_range)
}

# interval intersection: works with POSIX and numeric vectors
# ... timestamp vectors of type POSIXct and POSIXlt
interval_intersection <- function(...)
{
  times <- list(...)

  if (inherits(times[[1]],"POSIXlt"))
    times <- lapply(times,as.POSIXct)

  rn <- lapply(times, range)
  mat <- do.call(rbind,rn)
  rng <- c(max(mat[,1]),min(mat[,2]))

  if (inherits(times[[1]],"POSIXct")){
    #they may have not this attribute
    tzo <- attr(times[[1]][1], "tzone") # works with both POSIXct and POSIXlt
    rng <- as.POSIXct(rng, origin='1970-01-01 00:00.00 UTC',tz = tzo)
  }
  return(rng)
}

png_inch <- function(..., width = 6.2, height = 6.2, units = 'in', res= 220, bg = 'transparent', antialias = 'cleartype'){
  # the default profile is for A4 paper.
  do.call(png, list(..., width = width, height = height, units = units, res = res, bg = bg, antialias = antialias))
}

color_alpha <- function(col, alpha=1)
  rgb(t(col2rgb(unclass(col))/256), alpha=alpha)
