my_par <- function(...)
  par(mar = c(3, 3, 1, 1), mgp = c(2, 1, 0), tck = -.01,
      cex.axis = 1.25, cex.lab = 1.25, bty='l', ...)

png_inch <- function(..., width = 6.2, height = 6.2, units = 'in', res= 220, bg = 'transparent', antialias = 'cleartype'){
  # the default profile is for A4 paper.
  do.call(png, list(..., width = width, height = height, units = units, res = res, bg = bg, antialias = antialias))
}

color_alpha <- function(col, alpha=1)
  rgb(t(col2rgb(unclass(col))/256), alpha=alpha)

panel_smooth_abc <- function (x, y, col = par("col"), bg = NA, pch = par("pch"), cex = 1, col.smooth = "red", span = 2 / 3, iter = 3, ...) {
    points( x, y, pch = pch, col = col, bg = bg, cex = cex)
    #ok <- is.finite(x) & is.finite(y)
    #if (any(ok))
    #  lines(stats::lowess(x[ok], y[ok], f = span, iter = iter),
    #        col = col.smooth, ...)
    l <- letters[assign('i', get('i', env = .GlobalEnv) + 1, env = .GlobalEnv)]
    mtext(paste(l, ")"), adj = 0)
    grid()
}


