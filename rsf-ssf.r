# RSF
png_inch("plots/c1-rsf.png",
    width = 28,
    height = 28)

# output is the best model and we don't have highway_dist
(move_rsf$all$c1 %>%
    glm(used~poly(elevation,2)+
      poly(slope,2)+
      #poly(city_dist,2)+ #city_dist^2 is not significant and is not removed
      #poly(highway_dist,2)+ #not significant and is removed
      poly(path_dist,2)+ #is automatically removed
      poly(village_dist,2)+
      poly(water_dist,2),
    data=.,
    family=binomial) %>%
  step(trace=0,
       direction='both',
       k=2) -> xx) %>%
  summary

#if highway and city are included we have collinearity
(car::vif(xx) %>% sqrt) > 2

layout(matrix(1:4,2,2,T))

par(mar=c(3,3,1.5,1),
    mgp=c(2,.5,0),
    tck=-0.01,
    cex.axis=1.25,
    cex.lab=1.25,
    bty="l")

i <- 0
xx %>%
  termplot(partial.resid = T,
           col.res = color_alpha(.$data$used+1,.2),
           lwd.term = 2,
           smooth = panel_smooth_abc,
           ylab=c('Log-odds',NA_character_) %>% rep(2),
           xlab=expression("elevation"^2,
                           "slope"^2,
                           "village-distance"^2,
                           "water-distance"^2))
legend("topleft",
       c("used","available"),
       pch=1,
       col=2:1,
       bty='n')
dev.off()

#SSF moving
#with direction = 'both', not all models converge. Use forward and manually remove unimportant covariates
png_inch("plots/c1-ssf.png",
         width=28,
         height=28)
(move_ssf$moving$c1 %>%
  .[!is.na(.$elevation) & !is.na(.$slope),] %>%
  clogit(used ~ poly(elevation, 2) +
                poly(slope,2) +
                poly(village_dist,2) +
                poly(water_dist,1) +
                #poly(city_dist,2)+
                #poly(highway_dist,2)+ #doesn't automatically remove non-significant
                #poly(path_dist,2)+ #not significant
                #poly(temp,2)+ #NA
                #poly(pressure,2)+ #NA
                #poly(humidity,2)+ #NA
                strata(stratum),
                   data = ., na.action = "na.omit") %>%
    step(trace=0,
         direction = 'forward',
         steps=100) -> yy) %>%
    summary

(car::vif(yy) %>% sqrt) > 2

layout(matrix(1:4,2,2,T))
par(mar=c(3,3,1.5,1),
    mgp=c(1.5,.5,0),
    tck=-0.01,
    cex.label=1.25,
    cex.axis=1.25,
    bty='l')

i <- 0
move_ssf$moving$c1 %>%
  .[!is.na(.$elevation) & !is.na(.$slope),] %>% {
    termplot(yy,
             partial.resid = T,
             col.res = color_alpha(.$used+1,.2),
             lwd.term = 2,
             smooth = panel_smooth_abc,
             ylab = c('Log-odds',NA_character_) %>% rep(2),
             xlab = expression("elevation"^2,
                               "slope"^2,
                               "village-distance"^2,
                               "water-distance"))
  }

legend("bottomleft",
       c("used","available"),
       col=2:1,
       pch=1,
       bty='n')
dev.off()

