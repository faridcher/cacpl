library(survival)

## SHAPE
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

## Models
# poly doesn't work with na
x <- move_ssf$moving$c1
x <- x[!is.na(x$elevation) & !is.na(x$slope),]
models$ssf$moving$c1 <- survival::clogit(used ~ poly(elevation, 2) + poly(slope,2) + strata(stratum), data = x, na.action = "na.omit")
summary(models$ssf$moving$c1)

(move_ssf$moving$l %>%
  .[!is.na(.$elevation) & !is.na(.$slope),] %>%
  survival::clogit(used ~ poly(water_dist,2) + poly(elevation, 2) + poly(highway_dist,2) + poly(path_dist,1) + poly(slope,2) +
                          strata(stratum),
                   #+ elevation:highway_dist #+ water_dist water is close to zero
                   data = ., na.action = "na.omit") ->
    models$ssf$moving$l) %>%
  summary()

# PLOT
# pdf("plots/ssf_l_vs_c1.pdf",9.5,5)
png_inch("plots/ssf-moving-l-vs-c1.png",
         width = 32,
         height = 32)

layout(matrix(1:9,3,byrow = T))
par_fav(mar=c(3,3,1.5,1))

models$ssf$moving %>%
  mapply(main=names(.),
         FUN = function(xx,main)
           termplot(xx,
                    main=main,
                    se = T,
                    partial.resid = T,
                    lwd.term = 2,
                    col.res = color_alpha(move_ssf$moving[[main]]$used+1, .2)
           ))

legend("bottomright", c("used", "unused"), pch = 1, col = 2:1, bty = 'n')

dev.off()

# SSF sampling caterpilar
png_inch('plots/sampling_ssf.png')

rstrs$elevation %>%
  projectRaster(crs = (st_get_crs("utm40_km")$proj4string),
                xlab = "X (km)",
                ylab = "Y (km)") %>%
image( main = y, xlab = "X (km)", ylab = "Y (km)", col = colorRampPalette(c("white", 'black'))(6))
#mtext(z, adj = 0)

# caterpillar
move_ssf$moving$l %>%
  st_transform(st_get_crs("utm40_km")) %>%
  subset(stratum %in% 500:550) %$%
  plot_caterpillar(geometry, stratum, used, add = T)

dev.off()
