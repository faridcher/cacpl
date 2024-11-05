# adds used and stratum fields and calls caterpillar
# x is sfc.POINT
ssf_ready <- function(x, time = NA_real_, sample_size = 10){
  # time ordering for caterpillar
  used_sf <- st_sf(geometry=x, used=rep(1, length(x)), stratum = 1:length(x), time = time)
  null_sf <- cbind(caterpillar(x,time,sample_size), used = rep(0, (nrow(used_sf)-2) * sample_size))
  return(rbind(null_sf,used_sf))
}

# temporal covariates are meaningless with ssf
move_ssf$all <- by(subset(move, iid %in% c('c1','l')), with(move, iid:tid), function(xx){
    ssf <- ssf_ready(xx, geometry, time = time, sample_size = 10)
    spatial_covariates(ssf, rstrs, shps)
})

move_ssf$moving <- by(subset(move, iid %in% c('c1','l')), with(move, iid:tid), function(xx){
      subset(xx, mode_null=="Moving") %$%
      ssf_ready(geometry, time = time, sample_size = 10) %>%
      spatial_covariates(rstrs,shps)
})
