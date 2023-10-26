# adds used and stratum fields and calls caterpillar
# x is sfc.P
ssf_ready <- function(x, time = NA_real_, sample_size = 10){
  #time ordering for caterpillar
  used_sf <- st_sf(geometry=x, used=rep(1, length(x)), stratum = 1:length(x), time = time)
  null_sf <- cbind(caterpillar(x,time,sample_size), used = rep(0, (nrow(used_sf)-2) * sample_size))
  return(rbind(null_sf,used_sf))
}

# return a matrix with a dimension of (n-2)*(n-2) of pseudo absences
# (n-2)*n.pseudos
# x <- cumsum(arima.sim(n=3000, model=list(ar=.7)))
# y <- cumsum(arima.sim(n=3000, model=list(ar=.7)))
# z <- x + 1i*y
# op <- par(mfrow=c(1,2))
# znull <- caterpillar(z[1:10],T)
caterpillar <- function(x , time = NA_real_, n.pseudos=10) {
  #if move data aren't regularly sampled must divide by difftime
  # https://www2.clarku.edu/faculty/djoyce/complex/mult.html
  #if(!inherits(used_sf, c("sfc")))
  #  stop("x must be of type sfc")
  xy <- do.call(rbind, x)
  z <- complex(re=xy[,1], im=xy[,2])
  n <- length(z)
  # step-lengths
  sl <- Mod(diff(z))
  # relative to x+ axis ccw
  phi <- Arg(diff(z))
  # turn-angles
  ta <- diff(phi)
  steps <- complex(mod=sl[-1], arg=ta) # 2:9
  phi_unit <- complex(mod=1, arg=phi[-(n-1)]) # [-9] 2:9
  z_inner <- z[-c(1, n)] # 2:9
  # add the rotated steps to the last step
  z_null <- apply(outer(phi_unit, steps), 2, '+', z_inner)
  # n-2 or nrow(z_null)
  z_null <- z_null[,sample(n-2, n.pseudos)]
  # +1 because, stratum starts from 1 in used_df
  stratum <- rep(1:nrow(z_null), ncol(z_null)) + 1
  if (all(!is.na(time)))
    time <- rep(time[-c(1,length(time))],ncol(z_null))
  # as.vector is by col. use t to make it by row
  z_null <- as.vector(z_null)
  cater <- data.frame(stratum=stratum, x=re(z_null), y=im(z_null), time=time)
  return(st_as_sf(cater, coords=c('x','y'), crs=st_crs(x)))
}

# x is sfc.p
plot_caterpillar <- function(x, stratum, used, add = F, ...) {
    plot(subset(x, used == 1), col = color_alpha(1, .5), pch = 19, cex = 1, type = 'o', lwd = 2, add = add, ...)
    xy <- do.call(rbind,args = x)
    df <- data.frame(stratum, used, x=xy[,1],y=xy[,2])
    df <- subset(merge(df, df, by='stratum',suffixes=1:2), used1==1 & used2==0)
    with(df, {
      palette('default')
      s <- st_segment(x1,y1,x2,y2,st_crs(x))
      plot(st_sf(s, stratum), main="", add=T, type='l', col=stratum)
      legend("topright", c("animal traj", paste0("stratum",1:6), "..."),
             lwd=c(2, rep(1,6),1), bg = NA,col=c('black',1:6,1))
})}
