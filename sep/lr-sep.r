# regression (correlation) of dis_c1_ch2 with habitat features --------
# pdf("plots/c1_ch2_dist.pdf",15,15)
png_inch("plots/c1-ch2-dist-%02d.png",
    width = 7,
    height= 7)

(models$lr$c1_ch2 <- lm(log(c1.ch2.sep) ~ log(c1.l.sep),
                           #poly(c1.ch2.water_dist1,2) +
                           #poly(c1.ch2.elevation1,2) ,
                         move_simult_wide)) %>% summary
# one liner
(models$lr$c1_ch2 <- lm(log(c1.ch2.sep) ~ log(c1.l.sep), move_simult_wide)) %>% summary

1:10 %>%
    plot()

#layout(matrix(1:4,2))
my_par(cex.axis=1.75, cex.lab=1.75)
models$lr$c1_ch2 %>%
  termplot(partial.resid = T,
           se = T,
           pch=c(19,22)[(move_simult_wide$c1.ch2.sep > 3)+1],
           xlabs = "C1-leopard distance (km)",
           ylabs = "C1-C2 distance (km)",
           col.res=color_alpha(1,1)
           )

legend("topright",
       c("C1-C2 distance > 3 km"),
       #col=color_alpha(1,.3),
       cex = 1.75,
       pch = 22)

scatterplotMatrix(~c1.ch2.sep+
                   log10(c1.ch2.sep)+
                   c1.l.sep+
                   log(c1.l.sep)+
                   c(diff(c1.l.sep),NA),
                   #var.labels = c(dist_c1_ch2,log10(dist_c1_ch2),dist_c1_lp,log10(dist_c1_lp),diff(dist_c1_lp)),
                   move_simult_wide)

#c1 covariate and distances
scatterplotMatrix(~c1.ch2.elevation1+
                   c1.ch2.city_dist1+
                   c1.ch2.highway_dist1+
                   c1.ch2.path_dist1+
                   c1.ch2.village_dist1+
                   c1.ch2.water_dist1+
                   c1.ch2.sep+
                   c1.l.sep+
                   c(diff(c1.l.sep),NA),
                        plot.points=T,
                        #var.labels = c(colnames(x)[1:8],"diff(dist_c1_lp)") ,
                  move_simult_wide)
dev.off()


# distance < 5 km----------------------------------------------------------------
move_simult_wide %>%
  subset(c1.l.sep < 5, c(c1.l.sep, c1.ch2.sep)) %>%
  st_drop_geometry() %>%
  plot(type = 'h',
       xlab="c1.lp.distance (km)",
       ylab="c1.ch2.distance (km)",
       main= "fixes when c1.lp.distance < 5 KM")

# cor - ccf ---------------------------------------------------
# c1-ch2 vs c1-l cor and time lag
# pdf("plots/ccf_c1ch2l_dist_ccf.pdf")
png_inch("plots/c1ch2l-dist-ccf.png")

my_par()
layout(matrix(1))
n <- nrow(move_simult_wide)
lags <- -(n-1):(n-1)

move_simult_wide %$% {
  sapply(lags, function(i)
    c(lag=i,
      corr=cor(c1.ch2.sep,
               Lag(c1.l.sep, i)))) %>%
    t() %>% {
      plot(.[,'lag'],
           .[,'corr'],
           type='l',
           xlab="lag",
           ylab="Cross-correlation (c1-ch2 separation vs c1-lp separation)")}

  #azimuth
  # sapply(lags,
  #        function(i)
  #     c(lag=i,
  #     corr=cor(abs.angle_c1,
  #              Lag(abs.angle_l, i)))) %>%
  #   t %$%
  #   plot(lag,
  #        corr,type='l',
  #        xlab="lag",
  #        ylab="Cross-correlation (cheetah1-leopard Azimuth)")
}
dev.off()

# azimuth manual ccf --------------------------------------------------------
#pdf("plots/corr/c1-l_ccf_azi.pdf")
png_inch("plots/corr/c1-l_ccf_azi.pdf")

n <- nrow(move_simult_wide)
lags <- -(n-1):(n-1)
ab <- sapply(lags, function(i)
    c(
      lag = i,
      cor = cor(
        move_simult_wide$c1.ch2.abs.angle1,
        Lag(move_simult_wide$c1.l.abs.angle2, i) ,
        use = "complete.obs"
      )
    )) %>%
  t()

plot(ab[, 'lag'],
     ab[, 'cor'],
     type = 'l',
     xlab = "lag",
     ylab = "")

dev.off()

#smoothing dist diff. not useful
move_simult_wide$c1.l.sep_diff %>%
  zoo::rollapply(20,mean) %>%
  plot(type = 'l',
       ylab = "dist difference c1-l (smoothing bw=20)",
       xlab = "Time step")
abline(h=0,lty=2,col=2)
