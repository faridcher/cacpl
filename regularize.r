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

