# SSF moving
# with direction = 'both', not all models converge. Use forward and manually remove unimportant covariates
# png_inch("plots/c1-ssf.png", width=28, height=28)
x <- move_ssf$moving$c1
x <- x[!is.na(x$elevation) & !is.na(x$slope),]
x <- clogit(used ~ poly(elevation, 2) + poly(slope,2) + poly(village_dist,2) + poly(water_dist,1) +
                #poly(city_dist,2)+
                #poly(highway_dist,2)+ #doesn't automatically remove non-significant
                #poly(path_dist,2)+ #not significant
                #poly(temp,2)+ #NA
                #poly(pressure,2)+ #NA
                #poly(humidity,2)+ #NA
                strata(stratum), data = x, na.action = "na.omit")

yy <- step(x, trace=0, direction = 'forward', steps=100)
summary(yy)

(car::vif(yy) %>% sqrt) > 2

layout(matrix(1:4,2,2,T))
my_par(mar=c(3,3,1.5,1), mgp=c(1.5,.5,0))

i <- 0
x <- move_ssf$moving$c1
x <- x[!is.na(x$elevation) & !is.na(x$slope),]
termplot(yy, x, partial.resid = T,
             col.res = color_alpha(.$used+1,.2),
             lwd.term = 2, smooth = panel_smooth_abc,
             ylab = rep(c('Log-odds',NA_character_),2),
             xlab = expression("elevation"^2, "slope"^2, "village-distance"^2, "water-distance"))

legend("bottomleft", c("used","available"), col=2:1, pch=1, bty='n')
# dev.off()
