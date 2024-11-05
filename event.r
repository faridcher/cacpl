library(circular)
# PROCESSING -----
# when did the cheetah death happen? ans: when the step-length of cheetah was close to 0 and it was at the end of its track.
subset(move, iid=='c1','dist',drop=T) < .02

t_c1_death <- subset(move, iid=='c1','time',drop=T)[411]

# cheetah1 first big increase in separation
t_first_big <- subset(move_simult_wide, c1.ch2.sep > set_units(10, 'km') , 'c1.ch2.time1', drop = T)[1]

# event
move <- within(move, {
    event <- ((iid %in% c('c1', 'ch2')) & 
    	      (time > t_first_big)) |
    	       (iid == 'l' & (time > t_c1_death))
      event <- factor(event, labels = c('before', 'after'))
  })

png_inch("plots/bef-aft/event-hmm.png")

my_par()
layout(matrix(1:2))
# no significant change in l hmm behavior
s <- split(move,move$iid)[c('c1','l')]
lapply(s, function(x)
    tab <- with(x, table(event, mode_null))
    plot(tab)
    chisq.test(tab)
)
dev.off()

png_inch("plots/bef-aft/event-boxplot-%02d.png")
my_par()
layout(matrix(1:4,2))

s <-  split(move,move$iid)[c('c1','l')]
lapply(s, function(x){
    li <- lapply(x[,c('dist', 'R2n', 'abs.angle',
                 'rel.angle'),drop=T], split, f=x$event)
    mapply(li, names(li), FUN=function(x,y){
	     #boxplot works with list
    	     boxplot(x, ylab=y, col = 'gray', varwidth = T, na.actionf = na.pass)
    	      })
})
dev.off()

# percent decrease in step-lengh
s <- split(move,move$iid)[c('c1','l')]
lapply(s, function(x){
  tap <- with(x, tapply(dist, event, mean, na.rm=T))
  diff(tap)/tap[1]*100
})

# also log(dist), log(R)
s <- split(move,move$iid)[c('c1','l')]
lapply(s, function(x){
   with(x, { 
  wilcox.test(dist~event,data=.) #signif
  wilcox.test(R2n~event,data=.)  #not signif
  t.test(dist~event,data=.)      #signif
  t.test(R2n~event,data=.)       #signif
}
)})

s <- split(move,move$iid)[c('c1','l')]
lapply(s, function(x){
      li <- lapply(x[, c('abs.angle', 'rel.angle'), #not signif
               drop = T], split, f = x$event)
      lapply(li, function(x)
          li2 <- lapply(x,circular)
          names(li2) <= c('x', 'y')
          li2 <- c(li2, alpha = 0.05)
          do.call(watson.two.test, args = li2)
      )
   })

# rose.diagram
# png_inch("plots/bef-aft/event-rose.png")

layout(matrix(1:4,2,byrow=T))

s <- split(move,move$iid)[c('c1','l')]
lapply(s, function(x){
      x <- subset(x, select = c('abs.angle', 'rel.angle'), drop = T)
      #rose.diag not working prop = 2, main = y
      mapply(x, names(x), FUN = function(x, y) rose.diag(circular(x), bins = 20, col  = "grey",))
   })

# dev.off()

# paper figures
# l - after c1 death (only step-length)
# l - first big is similiar
# png_inch("plots/event-vioplot.png", width = 7, height= 6)

my_par()
layout(matrix(1:4,ncol=2,byrow = T))

s <- split(move,move$iid)[c('c1','l')]
lapply(s, function(x){
    with(x, {
            jit <- jitter(as.numeric(event), amount = .1)
            df <- data.frame("step length (km)"=dist, "turn angle (rad)"=rel.angle)
            mapply(df, names(df),
                   MoreArgs = list(f = event, jit = jit),
                   function(xx, n, f, jit) {
                  li <- lapply(split(x, f = f), na.omit)
                  vioplot(li, col='transparent')
                  mtext(n, side = 2, line = 2)
                  points(jit, xx, cex = .4, col = color_alpha(1, .5))
              })
        })
# dev.off()
