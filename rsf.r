#' rsf ready
#'
#' @param move_sfc object of type st_sfc (st_point)
#' @param n_sample number of samples
#' @param time is not used in RSF
#'
#' @description samples and adds the used field
#' @return
#' @export
#'
#' @examples
#' todo: make it work with temporal covariates
rsf_ready <- function(move_sfc, n_sample=length(move_sfc) * 2, time= NA_real_)
{
  used <- st_sf(geometry=move_sfc,used=rep(1,length(move_sfc))) #,time=time)

  samps <- st_sample(st_convex_hull(st_combine(used)),size=n_sample)
  avail <- st_sf(geometry=samps, used = rep(0,length(samps))) #, time=sample(time,length(samps),T))
  return(rbind(avail,used))
}
