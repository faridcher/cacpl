# interpolates and joins weather data to movement data using weighted averaging.
# returns matrix of covariates at move_time
time_interpolate_join <- function(move_time, weather_time, weather_df) {
  # initialize a new dataframe to fill
  move_df <- matrix(NA,ncol = ncol(weather_df), nrow = length(move_time))
  for (i in 1:length(move_time)) {
    # assumptions: move_time range is in the irimo_df time range
    # before_idx is the index for the time before move_time[i]
    # works fine even if  weather_time == move_time[i]
    before_idx <- diff(weather_time > move_time[i]) == 1
    before_idx <- which(before_idx)
    # averaging. warnings root in the next expression
    # it is not a weighted averaging
    # vectorized. averaging over all cols
    # units are not important here
    w1_2 <- as.numeric(abs(difftime(move_time[i] , weather_time[before_idx:(before_idx+1)])))
    w1_2 <- w1_2/sum(w1_2)
    # w2 applies to the first and w1 to second time
    # weighted averaging of the found records
    # apply removes the circularity
    ave <- apply(weather_df[before_idx:(before_idx+1),], 2, weighted.mean, w=rev(w1_2))
    move_df[i,] <- ave
  }
  colnames(move_df) <- names(weather_df)
  return(move_df)
}

# temperature <- extract_irimo_covariate("HOURLY DRY BULB AND WET BULB TEMPERATURE",5)
extract_irimo_covariate <- function(r5,pattern,row_after_pattern=5) {
  year <- 2007
  y <- c(rep(year,12),year+1)
  d <- 1
  m <- c(1:12,1)
  mdays <- year_month_days(2007)
  ddff <- sapply(r5,function(li) paste0(li,collapse = ' ') == pattern)
  # ddff is repeated twice. remove one of them
  ddff_idx <-  which(ddff)
  from <- (ddff_idx+row_after_pattern)
  to <- (ddff_idx+row_after_pattern-1+mdays)
  idx <- unlist(mapply(seq,from,to,SIMPLIFY = T))
  # drop the last 8 values.
  # drop the first column : the day of month
  ddff <- do.call(rbind,r5[idx])[,2:17]
  ddff <- matrix(as.numeric(t(ddff)),ncol = 2,byrow = T)
  colnames(ddff) <- r5[[ddff_idx[1] + 4]][2:3]
  ddff <- data.frame(ddff)
  return(ddff)
}

read_weather <- function(f) {
  res <- readLines(f)
  r2 <- strsplit(res,"\n")
  r3 <- lapply(r2, recursive_split, splitters=" @$#&?!,;:+/_[](){}><'\"\n\r\t")
  # func is missing
  r4 <- rm_list_nulls(r3)
  r5 <- lapply(r4, gsub, pattern = "\\*+", replacement = NA, perl = T)
  # ddff wind_speed and wind_direction
  ddff <- extract_irimo_covariate(r5,"HOURLY DDFF AND MAX WIND AND GUSTY WIND IN KNOT",5)
  temperature <- extract_irimo_covariate(r5,"HOURLY DRY BULB AND WET BULB TEMPERATURE",5)
  hum <- extract_irimo_covariate(r5,"HOURLY RELATIVE HUMiidITY AND DAILY PRECIPITATION",5)
  pressure <- extract_irimo_covariate(r5,"HOURLY PRESSURE QFE AND QFF IN MILLIBAR",5)
  time <- seq(as.POSIXct("2007-1-1", tz = "UTC"), as.POSIXct("2008-1-1", tz = "UTC") - 1, by = as.difftime(3, units = "hours"))
  bafgh <- data.frame(time, ddff, temperature, hum, pressure)
  # rename field from meta
  meta <-  c(DD = 'wind_direction', FF = 'wind_speed', DRY = 'temp', HUM = 'humidity', QFE = 'pressure')
  idx <- which(names(bafgh) %in% names(meta))
  names(bafgh)[idx] <- meta
  # wind_direction is circular
  bafgh$wind_direction <- circular::circular( bafgh$wind_direction, units = "degree", zero = pi / 2, rotation = "clock")
  # only save target columns plus time
  bafgh <- bafgh[,c(1,idx)]
  return(bafgh)
}
