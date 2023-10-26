animate <- function(x, last = 10, delay = .2, skip_row = 1){
  df <- st_drop_geometry(x)
  x_sp <- split(df, f = df$iid, drop = T)
  y <- max(sapply(x_sp,nrow))

  for (i in seq(1, y, 1)){
    plot_sf( x, xlab = "X", ylab = "Y", axes = TRUE, bgMap =)

    for (j in seq(1, length(x_sp), 1)) {
      q <- tail(1:i, last)
      lines(x_sp[[j]][q,c('x','y')],
            col = color_alpha(j, .5),
            cex = 0.5,
            lwd = 1.2) #,type='l', asp=1)
      points(x_sp[[j]][i,c('x','y')],
             pch = 19,
             col = alpha_col(j, .5),
             cex = .8)
    }
    title(paste("last", last, "obs.", x$ti[i]))
    legend("topright", names(x_sp), col = seq_along(xx), lwd = 1.2, pch = 19)

    box()
    Sys.sleep(delay)
  }
}
