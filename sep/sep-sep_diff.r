# separation log vs Separation difference
library(circular)

move_simult_wide %$% {
  (c1.l.sep < 5000) %>%
    data.frame(near = .,
               #far = `!`(.),
               all = rep(T, length(.))) %>%
    lapply(FUN=function(sbst){
      sbst %>% {
        cor(c1.l.sep[.],
            c1.l.sep_diff[.],
            method="kendall") %>%
          round(2) %>%
          cat
        #to use pearson correlation you should use mardia test to check the bivariate normalitly
        cor.test(c1.l.sep[.],
                 c1.l.sep_diff[.],
                 method="kendall")

        cor(c1.l.abs.angle1[.],
            c1.l.abs.angle2[.],
            method="kendall") %>%
          round(2) %>%
          cat

        #circular version
        cor.circular(na.exclude(c1.l.abs.angle1),
                     na.exclude(c1.l.abs.angle2),
                     test=T)
      }
    })
}

#
#pdf("plots/near_far/ch_mmetrics.pdf")
png_inch("plots/near_far/ch_mmetrics.pdf")

c1.l.near <- (move_simult_wide$c1.l.sep < 5)
par_reset()
layout(matrix(1:4,2,byrow = T))
# fill like bef_aft
#boxplot of turn-angle and step-length

move_simult_wide %$% {
  wilcox.test(log(dist_c1)~near.c1_l,data=.)
  t.test(log(dist_c1)~near.c1_l,data=.)
}

#watson rel and abs near far. both ch and l

dev.off()
