source('ssf/caterpillar.r')

# temporal covariates are meaningless with ssf
move_ssf$all <- by(subset(move, iid %in% c('c1','l')), with(move, iid:tid), function(xx){
    ssf <- ssf_ready(xx, geometry, time = time, sample_size = 10)
    spatial_covariates(ssf, rstrs, shps)
})

move_ssf$moving <- by(subset(move, iid %in% c('c1','l')), with(move, iid:tid), function(xx){
      x <- subset(xx, mode_null=="Moving")
      ssf <- with(x, ssf_ready(geometry, time = time, sample_size = 10))
      spatial_covariates(ssf, rstrs,shps)
})
