#todo: there are some gap in aggr_bout!!! maybe NA!!
#count
aggr_bout %>%
  lapply(subset,
         subset=duration > th) %>%
  lapply(nrow) %>%
  lapply(n2mfrow)

#c1 moving extreme
png_inch("plots/extreme-bout-%02d.png", height=5, width = 11)
#aggr_bout$c1$start_time[1] == c1$time[1]
#par_reset()
my_par()

aggr_bout[1] %>%
  mapply(
    names(.),
    FUN = function(x, y) {
      #layout
      (x$duration > th) %>%
        sum %>%
        n2mfrow() %>%
        rev %>%
        par(mfrow = .)

      x %>%
        subset(duration > th) %$%
        tapply(
          geometry,
          bout_id,
          FUN = function(xx) {
            #plot base
            x %>%
              #st_transform(st_get_crs('utm40_km')) %>%
              st_geometry() %>%
              #st_combine() %>%
              #st_convex_hull() %>% #write a function to convert line to point
              plot(
                col = color_alpha(1, .5),
                cex = .4,
                type = 'o',
                xlab = "X (km)",
                ylab = "Y (km)",
                axes = T)
            #extremes
            xx %>%
              st_transform(st_get_crs('utm40_km')) %>%
              plot(
                col = color_alpha(2, .5),
                cex = .4,
                type = 'o',
                lwd = 2,
                add = T
              )
          }
        )

      legend("bottomleft",
             c("movement track",
               paste0("Extreme bout\n",
                      "bout duration > 200 hours")),
             col=color_alpha(1:2,.5),
             pch=1,
             lwd=1,
             bty='n')
    }
  )

dev.off()

# track metrics --------------------------------------------------------------------
# Only for c1
# displacement duration per mode

png_inch("plots/basic-stats.png")
layout(matrix(1:4,2))

aggr_bout$c1 %>%
{lapply(.[,c('duration',
             'displacement',
             'beeline',
             'daily_displacement')],
        split,
        f=.$mode)} %>%
  mapply(ylab=names(.),
         FUN=function(x,ylab,na.action){
           x %>%
             unclass %>%
             c %>%
             boxplot(ylab,na.action)},
         MoreArgs = list(na.action = na.pass))

dev.off()

# summary of bouts per mode summary(mean, median, sd, quantile)
aggr_bout[c('c1','l')] %>%
  lapply(function(x)
    x %>%
      subset(duration < th) %>%
      aggregate(cbind(displacement,
                      beeline,
                      duration) ~ mode,
                .,
                summary))

# Covariates  --------------------------------------------------------------------
png_inch("plots/bout/bout-cov-%02d.png")

my_par(mar=c(3,3,3,1))
# acf of covariates, original vs aggr_bout_c1 (i.e no correlation)
layout(matrix(1:12,3,byrow = T))

move %>%
  st_drop_geometry %$%
  split(.,iid)[c('c1','l')] %>%
  lapply(function(x)
    x[, 11:22] %>%
    mapply(main=paste(letters_fa,names(.)),
         FUN=acf,
         MoreArgs = list(na.action=na.pass)) %>%
    invisible())

my_par(mar=c(3,3,3,1))
layout(matrix(1:12,3,byrow = T))

aggr_bout[c('c1','l')] %>%
  lapply(function(x){
  #no auto-correlation of means
  #means of covariates
  x[,9:20] %>%
    st_drop_geometry() %>%
    mapply(main=paste(letters_fa,names(.)),
           FUN=acf,
           MoreArgs = list(na.action = na.pass)) %>%
    invisible()
  })

layout(matrix(1:12,3,byrow = T))
aggr_bout[c('c1','l')] %>%
  lapply(function(x){
  # Covariates means per mode (boxplot and wilcox)
  # boxplot, covariates mean per mode
  x %>%
    st_drop_geometry() %$%
    lapply(.[,9:20],
         split,
         f=mode) %>%
      mapply(FUN=boxplot,
             main=letters_fa,
             ylab=names(.),
             MoreArgs = list(names=1:2,na.action = na.pass)) %>%
    invisible()
  #boxplot(lapply(.[[1]],as.numeric), main="wind_direction (circular)")
})

aggr_bout[c('c1','l')] %>%
  lapply(function(x){
  # wilcoxon : env_var~mode
   x %>%
    st_drop_geometry() %$%
    lapply(.[,9:20],
           split,
           f=mode) %>%
      lapply(function(x)
        wilcox.test(x[[1]],
                    x[[2]], paired = F))
  })

dev.off()

# bout covs extremes (CHECK THIS)-------------------------------------------------------
pdf("disp_duration_outlier.pdf",height = 4)
par(mar=c(4,4,1.5,1),mfrow=c(1,2))

# left
aggr_bout_c1 %>%
  subset(duration < 200) %>%
  with(plot(duration,displacement/1000,ylab="Displacement (km)",xlab="Duration (hour)",pch=mode,xlim=c(0,200)))

aggr_bout_c1 %>%
  lm(formula=I(displacement/1000)~duration, subset = (duration < 200) & mode==1) %>%
  .$coefficients %T>%
  abline(lwd=1,lty=2) -> coef1

aggr_bout_c1 %>%
  lm(formula=I(displacement/1000)~duration, subset = (duration < 200) & mode==2) %>%
  .$coefficients %T>%
  abline(lwd=1,lty=1) -> coef2

grid()
legend("topleft",c("Moving","Encamped"),pch=c(2,1),lwd=1,lty=1:2)
mtext("A",3,adj = 0,cex=1.4)

# right
aggr_bout_c1 %>%
  boxplot(daily_displacement~mode_name,
          . ,
          ylab="Daily displacement (km/day)")
grid()
mtext("B",3,adj = 0,cex=1.4)

dev.off()
