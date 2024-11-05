library(moveHMM)

models <- list()
#hmm_models$c1$`~wind_speed` is the cheetah1 hmm model for wind speed
move$mode_null <- NULL
move %$%
  split(., f = iid) %>%
  mapply(
    names(.),
    list(
      c1 = list(c("wind_speed")),
      #,c("sin(wind_direction)","cos(wind_direction)")
      ch2 = list(),
      l = list(c("elevation"))
    ),
    #,c("elevation^2"),c("path_dist"),c("temp")
    SIMPLIFY = F,
    FUN = function(xx, yy, cov) {
      #we scale because fitHMM doesn't converge. read moveHMM vignette page 9
      #scale covariate features only, except lon, lat, time. Don't center to avoid negatives
      xx %>%
        st_drop_geometry %>% {
          data.frame(iid = yy,
                     .[, 1:2], #only x,y. no time
                     scale(.[, 4:ncol(.)] %>%
                             cols_numeric_only(),
                           center = F))
        } %>%
        #compute step-length and turn-angle
        moveHMM::prepData(type = "UTM", coordNames = c("x", "y")) %>%
        #change units to km/h
        within({
          step <- step / 1000 / 8
          #wind_direction <- circular::circular(wind_direction,units="degree",zero=pi/2,rotation="clock")
          #wind_direction <- cos(wind_direction*pi/180)
        }) %>%
        hmm_fit_models(covs = cov,
                   null_model = T) #%>%
    }
  ) ->
  models$hmm

models$hmm %>%
  lapply(hmm_aic_reporting)

# add the mode_null covariate
models$hmm %>%
  lapply(function(xx)
    moveHMM::viterbi(xx$`~1`)) %>%
  unlist %>%
  factor(labels = c("Encamped","Moving")) ->
  move$mode_null

# boxplot of wilcox-signif covariates in behavioral phases.
png_inch("plots/bxplot-wilcox-signif-mode-null-%02d.png")

par(oma=c(0,0,3,0)) #for
move %>%
  st_drop_geometry() %$%
  split(.,f=iid) %>%
  mapply(names(.),
         SIMPLIFY=F,
         FUN=function(xx,yy) {
    xx %>%
    {lapply(.[,4:16],split,f=.$mode_null)} %>%
    lapply(`names<-`,value=c('x','y')) %>%
    lapply(c,paired=F) %>%  #add further arguments of wilcox.test
    lapply(do.call, what=wilcox.test) %>%
    #print %>%
    sapply(function(x) x$p.value < 0.05) %>%
    which %>% names -> signif_cov

    signif_cov %>%
    length() %>%
    n2mfrow() %>%
    par(mfrow=.,mar=c(4,2,2.5,1))

    #batch boxplot
    xx %>%
      {lapply(.[,signif_cov],
              split,
              f=.$mode_null)} %>%
      mapply(FUN=boxplot,
             main=names(.),
             MoreArgs = list(na.action = na.pass))

    title(yy,outer=T)
  })

dev.off()

# mode proportion
par_reset()
layout(matrix(1:2,2))
my_par()

move %>%
  subset((time %between% overlapping_period) &
           (iid %in% c('c1','l'))) %>%
  split(f=.$iid) %>%
  lapply(function(xx){
    xx %$%
    table(mode_null) %T>%
    plot %>%
    {./sum(.) * 100}
  })

# mode proportion and time of day - chi-square test
layout(t(1:2))
move %>%
  subset(iid %in% c('c1','l')) %$%
  split(.,f=iid) %>%
    mapply(names(.),
           SIMPLIFY=F,
           FUN= function(xx,y){
    xx %$%
    table(mode_null,
          format(time,
                 format = '%H')) %T>%
    plot(main=y) %>%
    chisq.test
})

# mode and time-of-day LOR
layout(t(1:2))
move %>%
  subset(iid %in% c('c1','l')) %$%
  split(.,f=iid) %>%
  lapply(function(xx){
  xx %>%
  glm((unclass(mode_null)-1)~time,
      family=binomial,
      data = .) %T>%
  termplot %>%
  summary
})

png_inch("plots/appendix/pairs-move-mode-%02d.png")
#group dist|abs|rel by mode_null per animal
move %>%
  subset(time %between% overlapping_period,
         select=c('abs.angle','rel.angle','dist','mode_null','iid')) %>%
  st_drop_geometry() %$%
  split(.,f=iid) %>%
  lapply(cols_numeric_only) %>%
  mapply(names(.), FUN=function(x,y) x %>%
           scatterplotMatrix(~dist+abs.angle+rel.angle,
                             .,
                             plot.points=F,
                             main=y,
                             groups=.$mode_null))

dev.off()

move %$%
  table(mode_null, iid, exclude = 'ch2') %>%
  #xtable::xtable(caption = "Contingency table of the proportion of the movement behaviors for the cheetah and leopard. Chisquare test","hmmprop")
  fisher.test()

# best hmm model outputs
pdf("plots/hmm/c1_hmm_covs.pdf")
png_inch("plots/hmm/l-hmm-covs-%02d.png")
layout(matrix(1:6,3))
m <- models$hmm$l$`~elevation`
#m <- models$hmm$c1$`~wind_speed`
m %T>%  #wind_speed
  plot(compact=T,animals=1,sepStates=F,ask=F,cex=1.7) %T>%
  # Confidence Interval plot
  # plot states
  plotStates(ask=F) %T>%
  # Plot pseudo residuals
  plotPR()

m %>%
  hmm_plot_CI()

dev.off()

library(tseries)
# jarque bera test for noramlity of step-length and turn-angle pseudo residuals
m %>%
moveHMM::pseudoRes() %$%
  list(
    step = jarque.bera.test(stepRes[complete.cases(stepRes)]),
    angleRes = jarque.bera.test(angleRes[complete.cases(angleRes)])
  )

m %>%
#standard error (SE) of coefficients for 1 > 2
{
  ((moveHMM::CI(.)$beta$upper[,1] -
      moveHMM::CI(.)$beta$lower[,1])/4)[2]
  #standard error (SE) of coefficients for 2 > 1
  ((moveHMM::CI(.)$beta$upper[,2] -
      moveHMM::CI(.)$beta$lower[,2])/4)[2]
}
rm(m)
