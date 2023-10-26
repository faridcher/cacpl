x),2))
  ) -> precip

png_inch('plots/weather_interpolate_test.png',
         width = 15)
par(xpd=F)
layout(matrix(1:2,ncol=2))

library(plotrix)
par_fav(cex.lab=1.8)
precip %$%
plotCI(rownames(.) %>%
         as.numeric,
       mean,sd,
       xlab="Year",
       ylab="Mean annual precipitation (mm) +/- sd")

m <- mean(precip$mean)
abline(h = m , lty = 2, col = 2)
abline(h = m - .6 * m, lty = 2, col = 3)
abline(h = m + 1.5 * m , lty = 2 , col=4)
legend("topright",
       c("all years' mean", "-60% all years' mean", "+150% all years' mean"),
       lty = 2,
       col=2:4,
       cex = 1.6,
       bty='n')

# extreme wet, extreme dry
(precip$mean - (m - .6 * m)) %>%
  which.min

(precip$mean - (m + 1.5 * m)) %>%
  which.max

#test_time_interpolate_join

weather_bafgh$`2007` %>%
  subset(time %between% range(move$time)) %>%
  head(50) %$%
  plot(
    time,
    wind_direction,
    type = 'o',
    pch = 19,
    xlab = 'time (weekday)',
    ylab = 'wind direction (deg)',
    col = color_alpha('blue', .5)
  )

move %>%
  subset(iid=='c1') %>%
  head(50) %$%
  points(time,
         wind_direction,
         col = color_alpha('red', .5),
         pch = 19)

legend(
  'top',
  c('weather station', 'animal track'),
  col = color_alpha(c('blue', 'red'), .5),
  pch = 19,
  cex = 1.6,
  bg=NA,
  bty='n')

dev.off()
