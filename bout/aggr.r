aggr_track_by_bout <- function(pt_sfc, time, mode_null, units = "hour"){
  #move %>% subset(iid='c1') %$% cumsum(c(1,diff(mode_null %>% unclass) != 0 )) %>% unique
  #become 56. why?
  bout <- cumsum(c(1,diff(mode_null %>% unclass) != 0 ))
  start_time <- aggregate(time, list(bout), min)[,-1] #exclude the grouping. tapply %>% no simplify %>% do.call works
  end_time <- aggregate(time, list(bout), max)[,-1]
  duration <- difftime(end_time, start_time, units = units)  %>% as_units()
  #spatial
  geometry <- tapply(pt_sfc, bout, st_pt_line, simplify = F) %>% do.call(c,.)
  displacement <- tapply(pt_sfc, bout, st_pt_line, simplify = F) %>%
    lapply(st_length) %>%  #sapply removes units
    do.call(c,.) %>%
    set_units('km')
  beeline <- tapply(pt_sfc,
                    bout,
                    st_beeline,
                    simplify = F) %>%
    lapply(st_length) %>%  #sapply removes units
    do.call(c,.) %>%
    set_units('km')
  daily_displacement <- displacement/set_units(duration ,'day')
  #mode
  mode <- tapply(mode_null %>% unclass, list(bout), min) %>% factor(labels=c("Encamped","Moving"))
  data.frame( bout_id=seq_along(start_time), start_time, end_time, duration, displacement, beeline, daily_displacement, mode, geometry) %>%
    st_sf
}

aggr_cov_by_bout <- function(covariates,mode_null){
  move_bout <- cumsum(c(1,diff(mode_null %>% unclass) != 0 ))
  mean_df <- aggregate(covariates, list(move_bout) , mean)[,-1] %>% `names<-`(sprintf("%s_mean",names(.)))
  sd_df <- aggregate(covariates, list(move_bout), sd)[,-1] %>% `names<-`(sprintf("%s_sd",names(.)))
  data.frame(mean_df,sd_df)
}
