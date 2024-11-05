# RSF - Encamped
# One interesting result is that, distance to nearest water source comes up as a significant covariate and the cheetah tends to stay as close as possible to water while resting. we don't have many data points after the minimum values and the confidence intervals are wide.
move_rsf$encamped <- move[c('c1','l')] %>%
  lapply(function(xx){
  xx %>%
    subset(mode_null=="Resting") %>%
    st_geometry %>%
    rsf_ready(n_sample = length(.) * 2) %>%
    spatial_covariates(rstrs,shps)
})

# Encamped
# c1
(glm(used~poly(elevation,2)+
       poly(slope,2)+
       #poly(city_dist,2)+ #city_dist^2 is not significant and is not removed
       #poly(highway_dist,2)+ #not significant and is removed
       poly(path_dist,1)+ #is automatically removed
       #poly(village_dist,2)+
       poly(water_dist,2),
       data=move_rsf$encamped$c1,
       family=binomial) %>%
       step(trace=0,
         direction='both',
         k=2) ->
    models$rsf$encamped$c1) %>%
  summary

# l
(glm(used~poly(elevation,2)+
       poly(slope,2)+
       #poly(city_dist,2)+ #
       #poly(highway_dist,1)+ #
       poly(path_dist,2)+ #
       #poly(village_dist,2)+
       poly(water_dist,2),
     data=move_rsf$encamped$l,
     family=binomial) %>%
    step(trace=0,
         direction='both',
         k=2) ->
    models$rsf$encamped$l) %>%
  summary

# RSF - Encamped
png_inch("plots/rsf-encamped-%02d.png")
my_par()
layout(matrix(1:8,2,byrow = T))

models$rsf$encamped %>%
  mapply(
    main = names(.) %>%
      paste0("-encamped"),
      FUN = function(xx,main){
      termplot(xx,
               main=main,
               se = T,
               partial.resid = T,
               lwd.term = 2,
               col.res = color_alpha(xx$data$used + 1, .2)
      )
  })

legend(
  "bottomright",
  c("used", "unused"),
  pch = 1,
  col = 2:1,
  bty = 'n'
)

dev.off()
