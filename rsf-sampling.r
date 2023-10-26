#  RSF sampling in mcp
png_inch('plots/sampling_rsf.png',
    width = 12)

# dev.new(width=12)
layout(matrix(1:2,nrow = 1))
par_fav()

move_rsf$all %>%
  mapply(
    names(.),
    letters[1:2] %>% paste0(')'),
    FUN = function(x, y, z) {
      rstrs$elevation %>%
        projectRaster(crs = (st_get_crs("utm40_km")$proj4string),
                      xlab = "X (km)",
                      ylab = "Y (km)") %>%
        image(
          main = y,
          xlab = "X (km)",
          ylab = "Y (km)",
          col = colorRampPalette(c("white", 'black'))(6)
        )
      mtext(z, adj = 0)

      x %>%
        st_transform(st_get_crs("utm40_km")) %T>%
        plot(
          col = color_alpha(c('blue', 'red'), .5)[2 - .$used] ,
          pch = 19,
          cex = .5,
          add = T
        ) %>%
        st_combine %>%
        st_convex_hull %>%
        plot(add = T)
    }
  ) %>%
  invisible

legend("bottomright",
       c("Used habitat",
         "Available habitat",
         "MCP (100%)"),
       pch=c(19,19,NA),
       col=color_alpha(c('blue','red','black'),.5),
       fill="transparent",border=c(NA,NA,1),bty='n')

dev.off()
