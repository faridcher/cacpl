# c1_rsf is a data frame of the animal track with all covariates as sf spatial object
# all means no subsetting of the data
move_rsf$all <- move[c('c1','l')] %>%
  lapply(function(xx){
  xx %>%
  st_geometry %>%
  rsf_ready(n_sample = length(.) * 2) %>%
  spatial_covariates(rstrs,shps)
})

# all.names
# c1
# output is the best model and we don't have highway_dist
(glm(used~poly(elevation,2)+
       poly(slope,2)+
       sqrt(water_dist)+
       poly(path_dist,1)
       #poly(city_dist,1)+ #not significant and is not removed
       #poly(highway_dist,2)+ #not significant and is removed
       #poly(village_dist,2)+
     ,
       data=move_rsf$all$c1,
       family=binomial) %>%
       step(trace=0,
         direction='both',
         k=2) -> models$rsf$all$c1) %>%
  summary

#if highway and city are included we have collinearity
(car::vif(models$rsf$all$c1) %>% sqrt) > 2

#l
# if we include all 7 significant covariates we will have collinearity
(glm(used~poly(elevation,2)+
        poly(slope,2)+
        log(water_dist)+
         poly(path_dist,1)
     ,
       #poly(city_dist,2)+
       #poly(highway_dist,1)+ #gets removed
       #poly(path_dist,1), #path also improves the models without collinearity but for simplicity we remove it
      data=move_rsf$all$l,
      family=binomial) %>%
      step(trace=0,
         direction='both',
         k=2) ->
    models$rsf$all$l) %>%
  summary()

(car::vif(models$rsf$all$l) %>% sqrt) > 2

# VISUALIZE
# ALL
png_inch("plots/rsf-all-vis-%02d.png")

layout(matrix(1:8,2,byrow = T))
par_fav(mar = c(3, 3, 1.5, 1))

models$rsf$all %>%
  mapply(
    main = names(.),
    FUN = function(xx,main)
      termplot(xx,
               main=main,
               se = T,
               partial.resid = T,
               lwd.term = 2,
               col.res = color_alpha(xx$data$used + 1, .2)
      )
  )

legend(
  "topleft",
  c("used", "unused"),
  pch = 1,
  col = 2:1,
  bty = 'n'
)

par_reset(bg=NA)
layout(matrix(1:2, ncol = 2))
par_fav(mgp=c(2,1,0))

models$rsf$all %>%
  mapply(names(.),
         FUN = function(x, y) {
           reg_relweights(x)
           mtext(y, adj = 0)
         }
  )

dev.off()
