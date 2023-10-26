#hmm_df[,'cos(wind_direction)'] <- cos(c1[,'wind_direction']*pi/180)
#hmm_df[,'sin(wind_direction)'] <- sin(c1[,'wind_direction']*pi/180)

# only polynomial
# output is the best model and we don't have highway_dist
move %>%
  subset(iid=='c1') ->
  xx

(glm((as.numeric(mode_null)-1)~
         poly(elevation,2)+
         poly(slope,2)+
         poly(city_dist,2)+
         poly(highway_dist,2)+
         poly(path_dist,2)+
         poly(village_dist,2)+
         poly(water_dist,2)+
         poly(wind_speed,2)+
         cos(wind_direction*pi/180)+
         sin(wind_direction*pi/180)+
         poly(temp,2)+
         poly(humidity,2)+
         poly(pressure,2),
       data=xx,
       family=binomial) %>%
       step(trace=0,
            direction='both',
            k=log(nrow(.$model))) -> #BIC
    models$lor_mode$c1) %>%
  summary

#l
move %>%
  subset(iid=='l') ->
  xx
  (glm((as.numeric(mode_null) - 1) ~
       poly(elevation,2)+
       poly(slope,2)+
       poly(city_dist,2)+
       poly(highway_dist,2)+
       poly(path_dist,2)+
       poly(village_dist,2)+
       poly(water_dist,2)+
       poly(wind_speed,2)+
       cos(wind_direction*pi/180)+
       sin(wind_direction*pi/180)+
       poly(temp,2)+
       poly(humidity,2)+
       poly(pressure,2), #is removed,
      family = binomial,
      data = xx) %>%
    step(trace=0,
         direction='both',
         k=log(nrow(.$model))) ->
    models$lor_mode$l) %>%
    summary

models$lor_mode %>%
  lapply(function(x){
    car::vif(x) %>% sqrt > 2
  })

models$lor_mode %>%
  mapply(names(.),
         FUN = function(x, y) {
           reg_relweights(x)
           mtext(y, adj = 0)
         }
  )
# DO A RiidGE AND COMPARE
#pdf("plots/vs/LOR_mode_l_c1.pdf")
png_inch("plots/vs/lor-mode-l-c1-%02d.png")

layout(matrix(1:6,2,byrow =T ))
par_fav()

models$lor_mode[c('l','c1')] %>%
  mapply(
    main = names(.),
    FUN = function(xx,main){
      #layout(matrix(1:6,2,byrow =T ))
      termplot(xx,
               main=main,
               se = T,
               partial.resid = T,
               lwd.term = 2,
               col.res = color_alpha(xx$data$mode_null %>% unclass, .2))
      legend(
        "bottomright",
        c("encamped", "moving"),
        pch = 1,
        col = 1:2,
        bty = 'n')
      }
  )

dev.off()

