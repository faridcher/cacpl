# interval intersection: works with POSIX and numeric vectors
# ... timestamp vectors of type POSIXct and POSIXlt
interval_intersection <- function(...) {
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
