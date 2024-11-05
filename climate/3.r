"extdata/weather/irimo_bafgh_2007.txt" %>%
  readLines() %>%
  gsub(pattern = "\\*+", # replace **** with NA
       replacement = NA,
       perl = T) %>%
# keep . (decimals) - (negative values)

# number of days in the months of the year 2007
paste(c(rep(2007, 12), 2007 + 1), # year 2007
      c(1:12, 1), # months
      1, # first day of month
      sep = '-') %>%
  strptime(format = "%Y-%m-%d") %>%
  diff %>%
  as.numeric -> monthdays

patterns <- c(ddff = "HOURLY DDFF AND MAX WIND AND GUSTY WIND IN KNOT", # ddff is wind_speed and wind_direction
  temperature = "HOURLY DRY BULB AND WET BULB TEMPERATURE",
  hum = "HOURLY RELATIVE HUMiidITY AND DAILY PRECIPITATION",
  pressure = "HOURLY PRESSURE QFE AND QFF IN MILLIBAR"
)

# ddff is repeated twice. remove one of them
ddff_idx <-  which(ddff)

from <- (ddff_idx+6) # the first day of month is 5 rows after found pattern row number
to <- (ddff_idx+6-1+monthdays)
idx <- mapply(seq,from,to,SIMPLIFY = T)

# drop the last 8 values and the first column (day of month)
ddff <- do.call(rbind, r5[idx])[,2:17]
ddff <- matrix(as.numeric(t(ddff)),ncol = 2,byrow = T)
colnames(ddff) <- r5[[ddff_idx[1] + 4]][2:3]
ddff <- data.frame(ddff)

time <- seq(
  as.POSIXct("2007-1-1", tz = "UTC"),
  as.POSIXct("2008-1-1", tz = "UTC") - 1,
  by = as.difftime(3, units = "hours")
)
bafgh <- data.frame(time, ddff, temperature, hum, pressure)

# rename field from meta
meta <-  c(DD = 'wind_direction',
           FF = 'wind_speed',
           DRY = 'temp',
           HUM = 'humidity',
           QFE = 'pressure')
idx <- which(names(bafgh) %in%
               names(meta))
names(bafgh)[idx] <- meta

# wind_direction is circular
bafgh$wind_direction <- circular::circular(
  bafgh$wind_direction,
  units = "degree",
  zero = pi / 2,
  rotation = "clock"
)

# only save target columns plus time
weather_bafgh$`2007` <- bafgh[,c(1,idx)]
