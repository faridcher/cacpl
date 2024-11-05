list.files("fun",full.names = T) %>%
  sapply(source) %>%
  invisible

# synoptic station weather data
weather <- read.csv(
    "extdata/weather-1995-2017.csv",
    header = TRUE,
    sep = ','
 ) %>%
 `[`(-(1:3))

weather %<>% within({
  time <- strptime(date,
                   format = "%m/%d/%Y %H:%M",
                   tz = "GMT")
  rm(date)
  year <- strftime(time,"%Y") %>%
    as.numeric
})

boxplot(precipitation_mm~year,
        weather,
        xlab='year',
        ylab='precipitation')

layout(matrix(1:12, ncol=4, nrow=3, byrow=T))
my_par(mar=c(3,3,1,1))

weather %>% 
    subset(!is.na(precipitation_mm) &
           #precipitation_mm != 0 &
           #(year > 2006) &
           (strftime(time, "%H") == "06")) %$%
          split(.,f=year) %>%
          mapply(names(.),
         FUN = function(x,y){

    # hist(precipitation_mm,
    #      breaks = 20,
    #      col='grey')
    #
    # boxplot(precipitation_mm)
    #
    # plot(time,
    #      precipitation_mm,
    #      cex=.3,
    #      type='o')

    # (precipitation_mm %>%
    #          round(2) %>%
    #          factor()) -> xx
    #
    #  xx %>%
    #  plot(xlab="
