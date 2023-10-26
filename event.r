library(circular)
# PROCESSING -----
# when did the cheetah death happen? ans: when the step-length of cheetah was close to 0 and it was at the end of its track.
move %>%
  subset(iid=='c1','dist',drop=T) < .02

move %>%
  subset(iid=='c1','time',drop=T) %>%
  `[`(411) ->
  t_c1_death

# cheetah1 first big increase in separation
move_simult_wide %>%
  subset(c1.ch2.sep > set_units(10, 'km') ,
         'c1.ch2.time1',
         drop = T) %>%
  `[`(1) ->
  t_first_big

# event
move %<>%
  within({
    event <- (((iid %in% c('c1', 'ch2')) &
                 (time > t_first_big)) |
                (iid == 'l' & (time > t_c1_death))) %>%
      factor(labels = c('before', 'after'))
  })

png_inch("plots/bef-aft/event-hmm.png")

par_fav()
layout(matrix(1:2))
# no significant change in l hmm behavior
move %$%
  split(.,iid)[c('c1','l')] %>%
  lapply(function(x)
    x %$%
    table(event, mode_null) %T>%
    plot() %T>%
    chisq.test()
)
dev.off()

png_inch("plots/bef-aft/event-boxplot-%02d.png")
par_fav()
layout(matrix(1:4,2))

move %$%
  split(.,iid)[c('c1','l')] %>%
  lapply(function(x)
    x %>%
    {lapply(.[,c('dist',
                 'R2n',
                 'abs.angle',
                 'rel.angle'),drop=T],
            split,
            f=.$event)} %>%
      mapply(names(.),
             FUN=function(x,y){
               x %>%
                 boxplot(ylab=y, #boxplot works with list
                         col = 'gray',
                         varwidth = T,
                         na.actionf = na.pass)}
             )
)
dev.off()

# percent decrease in step-lengh
move %$%
  split(.,iid)[c('c1','l')] %>%
  lapply(function(x)
    x %$%
  tapply(dist,
         event,
         mean,
         na.rm=T) %>%
  {diff(.)/.[1]*100}
)

# also log(dist), log(R)
move %$%
  split(.,iid)[c('c1','l')] %>%
  lapply(function(x)
    x %$% {
  wilcox.test(dist~event,data=.) #signif
  wilcox.test(R2n~event,data=.)  #not signif
  t.test(dist~event,data=.)      #signif
  t.test(R2n~event,data=.)       #signif
}
)

# watson test
move %$%
  split(.,iid)[c('c1','l')] %>%
  lapply(function(x)
    x %>%
    {
      lapply(.[, c('abs.angle',
                   'rel.angle'), #not signif
               drop = T],
             split,
             f = .$event)
    } %>%
      lapply(
        function(x)
          x %>%
          lapply(circular) %>%
          `names<-`(value = c('x', 'y')) %>%
          c(alpha = 0.05) %>%
          do.call(watson.two.test,
                  args = .)
      )
)

# rose.diagram
png_inch("plots/bef-aft/event-rose.png")

layout(matrix(1:4,2,byrow=T))
move %$%
  split(., iid)[c('c1', 'l')] %>%
  lapply(
    function(x)
      x %>%
      subset(select = c('abs.angle', 'rel.angle'), drop = T) %>%
      mapply(
        names(.),
        FUN = function(x, y)
          x %>%
          circular() %>%
          rose.diag(
            bins = 20,
            col  = "grey",
            #not working
            prop = 2,
            main = y
          )
      )
)

dev.off()

#paper figures
#l - after c1 death (only step-length)
#l - first big is similiar
png_inch("plots/event-vioplot.png",
    width = 7,
    height= 6)

par_fav()
layout(matrix(1:4,ncol=2,byrow = T))

move %$%
  split(.,iid)[c('c1','l')] %>%
  lapply(function(x)
    x %$% {
            jit <- event %>%
            as.numeric %>%
            jitter(amount = .1)

            data.frame("step length (km)"=dist,
                       "turn angle (rad)"=rel.angle) %>%
            mapply(names(.),
                   MoreArgs = list(f = event, jit = jit),
                   function(xx, n, f, jit) {

                xx %>%
                  split(f = f) %>%
                  lapply(na.omit) %>%
                  vioplot(col='transparent')

                mtext(n, side = 2, line = 2)

                jit %>%
                  points(xx,
                         cex = .4,
                         col = color_alpha(1, .5))
              })
        }
)
dev.off()
