# TODO
# write a vignette
# subsample to 1h and draw diel activities boxplots
# persp x,y and pythagaros
c('cheetah1', 'leopard') %>%
    mapply(
        n = c('c1', 'l'),
        SIMPLIFY = F,
        FUN = function(x, n){
          x %>%
          paste0('extdata/move/', .,
                 '-activity.txt') %>%
          fread(header = TRUE,
                sep = ' ') %>%
          cbind(iid = n) -> x
     }) -> tb

tb %<>% do.call(rbind,.)

# temp: degree celcius
# activity: no units
# strptime first creates posixlt
tb <- tb[,{
  time <- strptime(paste(UTCDate,UTCTime),
                   format = "%d.%m.%Y %H:%M:%S",
                   tz="UTC") %>%
                    round(units="mins") %>%
                    as.POSIXct

  activity <- sqrt(ActivityX^2 + ActivityY^2)
  doy <- strftime(time,format = "%j",tz="UTC") %>% as.numeric # day of year. tz is mandatory or they are treated as local tz
  ydoy <- paste(strftime(time, format = "%Y", tz="UTC"),doy,sep='-') # year and day of year

  x <- strftime(time,format="%H", tz="UTC") %>% as.numeric
  tod <- ifelse(x >= 0 & x < 8, 0,ifelse(x >= 8 & x < 16, 8, 16)) # time of day

  ydoy_tod <- paste(ydoy,tod,sep='-')
  # rm(x,doy)
 c(.SD, # .SD is a datatable
   .(
     time = time,
     activity=activity,
     ydoy=ydoy,
     tod=tod,
     ydoy_tod=ydoy_tod
   ))
   }
  ]

tb <- tb[(time >= min(move$time))]

yy <- tb[,
         .(act_mean_diel=mean(activity),
               time = min(time)
                ),
         by=.(iid, ydoy)
         ][act_mean_diel > 1]

# scatterplot(act_mean_diel ~ time,
#             yy,
#             group = factor(yy$iid),
#             legend=TRUE,
#             # type='l', # doesn't work. because plot(type='n',...) is called
#             regLine=FALSE,
#             boxplot=FALSE,
#             smooth=list(span=.08) # doesn't work with PoSIXct
# )

# png_inch('activity%03d.png',
         # width = 14,
         # height = 7)
# dev.new(width=14, height=7)

layout(matrix(1))
par_fav(mar=c(4,5,1,0))

yy %$% {
  plot(
    time,
    act_mean_diel,
    type = 'n',
    col = 'red',
    xlab = 'Time of Year [2 months]',
    ylab = expression('Net activity ['*degree*']:'*sqrt(X[activity]^2 + Y[activity]^2)),
    main = paste0('Mean Diel Net Activity')
  )
}

yy %$%
  split(., iid) %>%
  mapply(1:length(.),FUN=function(x, i)
    x %$% {
      points(time,
             act_mean_diel,
             pch = i,
             col = i,
             cex= .5
             )
       lines(time,act_mean_diel,
             col = i)
       lines(loess.smooth(time,
                          act_mean_diel,
                          span = .08),
             lwd=2,
             col = i + 2)

      (a <- lm(act_mean_diel~time, .)) %>%  # l is significant
         summary %>%
         print
        abline(a,
             lwd=2,
             col = i + 4)
})

legend("topright",
       # rep(c("C1","L"), 3),
       paste(
       rep(c("C1","L"),3),
       rep(c("timeseries",
        "local polynomial",
        "linear regression"),
        each=2)),
       lwd=2,
       col=1:6)

# dev.off()

# parse behavioral modes
tb[,
   .(act_mean = mean(activity),
     act_sd = sd(activity),
     time = min(time)
     ),
   by=.(iid, ydoy_tod)][act_mean > 1] -> x

move <- as.data.table(move)
xy <- move[x,on=.(iid,time), nomatch=0]

# png_inch('parse-behavior.png')

par_fav(cex.axis=1)
layout(matrix(1))
boxplot(act_mean~iid*mode_null,
        xy,
        ylab='Net activity')

# dev.off()
